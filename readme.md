# rhyolite-example devlog

## Setting up the obelisk project (585a2ec8)
Start by running `ob init --branch develop` to set up an obelisk application.

You can now `ob run` to run the skeleton obelisk application.

## Adding the rhyolite dependency (6a86dd14)

Create a directory `dep` at the root of the obelisk project.

In the `dep` directory, run `nix-thunk create git@github.com:obsidiansystems/rhyolite`.

In the obelisk application's `default.nix`, make the following modifications:

```diff
diff --git a/default.nix b/default.nix
index 7afba95..3df728b 100644
--- a/default.nix
+++ b/default.nix
@@ -15,9 +15,14 @@
   }
 }:
 with obelisk;
-project ./. ({ ... }: {
-  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
-  android.displayName = "Obelisk Minimal Example";
-  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
-  ios.bundleName = "Obelisk Minimal Example";
+project ./. ({ pkgs, hackGet, ... }@args: {
+  overrides = pkgs.lib.composeExtensions
+    (pkgs.callPackage (hackGet ./dep/rhyolite) args).haskellOverrides
+      (self: super: with pkgs.haskell.lib; {
+        # Your custom overrides go here.
+      });
+  android.applicationId = "systems.obsidian.obelisk.examples.rhyolite";
+  android.displayName = "Rhyolite Example App";
+  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.rhyolite";
+  ios.bundleName = "Rhyolite Example App";
 })
```

The important part here is importing the rhyolite dependency and extending the haskell package set with the packages provided by rhyolite.

Running `ob run` at this point probably won't do much different, since we're not actually using any of these new packages, but it will help us verify that the `default.nix` file is still valid.

## Generating a schema with groundhog

### Defining the schema (a7b866b0)

Defining the schema is pretty straightforward. We create a couple of Haskell datatypes in `Common.Schema`, and a foreign key constraint between them (messages belong to chatrooms). We need to add `database-id-class` to the common dependencies, since we'll need to be able to refer to database identifiers on both the frontend and backend (hence the `HasId` instances).

On the backend, we use some template haskell (from groundhog via `Rhyolite.Backend.Schema.TH`) to generate the schema migration functionality. With `groundhog` we use some quasiquoted YAML to annotate the datatypes we defined in `Common.Schema` to define the actual database schema.

### Spinning up the database (ae19e331)

Actually starting up a database is pretty straightforward: just add `gargoyle-postgresql-connect` to the backend dependencies and call `withDb` in the backend startup code. In the previous step, we generated the migration function (`migrateSchema`) and now we've just got to run it.

`withDb` gives us a connection pool, and we can use `runDb` from `Rhyolite.Backend.Db` to actually run a transaction using a connection. There's a logging constraint that needs to be satisfied, so we'll need `monad-logger` as well.

To run the schema migration, we first analyze the current state of the database using `getTableAnalysis` and then use groundhog's `runMigration` function to execute the migration we generated previously. Note that `getTableAnalysis` only exists in our fork of groundhog.

After an `ob run`, you should see the following in psql:

```
postgres=# \d
               List of relations
 Schema |      Name       |   Type   |  Owner
--------+-----------------+----------+----------
 public | Chatroom        | table    | postgres
 public | Chatroom_id_seq | sequence | postgres
 public | Message         | table    | postgres
 public | Message_id_seq  | sequence | postgres
(4 rows)


postgres=# \d "Chatroom";
                               Table "public.Chatroom"
 Column |       Type        |                        Modifiers
--------+-------------------+---------------------------------------------------------
 id     | bigint            | not null default nextval('"Chatroom_id_seq"'::regclass)
 title  | character varying | not null
Indexes:
    "Chatroom_pkey" PRIMARY KEY, btree (id)
Referenced by:
    TABLE ""Message"" CONSTRAINT "Message_chatroom_fkey" FOREIGN KEY (chatroom) REFERENCES "Chatroom"(id)

postgres=# \d "Message";
                                      Table "public.Message"
  Column   |            Type             |                       Modifiers
-----------+-----------------------------+--------------------------------------------------------
 id        | bigint                      | not null default nextval('"Message_id_seq"'::regclass)
 chatroom  | bigint                      | not null
 text      | character varying           | not null
 timestamp | timestamp without time zone | not null
Indexes:
    "Message_pkey" PRIMARY KEY, btree (id)
Foreign-key constraints:
    "Message_chatroom_fkey" FOREIGN KEY (chatroom) REFERENCES "Chatroom"(id)
```

### Adding the Rhyolite Account schema (48a87612)

Rhyolite provides a type `Account`, a database schema for that type, and some common account operations (password verification, reset, etc). By importing `Rhyolite.Account` we can add a foreign key constraint on `Account` to one of our tables. On the backend, we need to run the `migrateAccount` function along with our other schema migration to actually create the right tables.

## Communicating with the frontend

Now comes the tricky part.  Our goal here is to be able to call the function `serveDbOverWebsockets` from `Rhyolite.Backend.App`.  This function needs to be told how to respond to API requests, how to respond to notifications from the database indicating that some change has occurred, and how to compute Views of data that connected users are interested in.  It's an important function.  Some of its complexity comes from an attempt to avoid certain performance pitfalls.  For example, part of what this function does under the hood is take the View Selectors provided by connected users and aggregating them to make View computation cheaper.

`serveDbOverWebsockets` takes a bunch of arguments. The first is the database pool, which we already have access to via `withDb`.

### Adding a route for websockets communication (19a55394)

First, we add an endpoint to the list of backend routes in Common.Route and we'll add a stub handler for that route to the backend.  The actual websocket connection handler is going to be produced by `serveDbOverWebsockets`.

### Defining an API (772d6da1)

The second argument of `serveDbOverWebsockets` is a `RequestHandler`, which describes how API requests ought to be handled.

In `Common.Api` we define a couple of GADTs representing our public (unauthenticated) and private (authenticated) request types, and then we define a GADT that includes both types of requests (aptly named `Request`). In `Backend.Request` we define the actual request handler that receives and processes API requests. For our login API, we can use the handler functions defined in `Rhyolite.Backend.Account`.

### Defining View Selector and View types (c11a5578)

The request/response API that we set up will be useful for certain kinds of transactions, especially writes, but we still need a way to read data from the backend and *keep that data up-to-date* without polling.

We do this buy defining a datatype that lets users indicate their interest in some data. The frontend sends this interest set to the backend, which immediately sends the data that the user is interested in, and keeps track of the fact that the user is interested. The backend will look out for changes to the database relevant to each user's interest set and send updates when those changes occur.

The workflow looks like this:

```
User declares interest/sets View Selector
  -> Backend receives View Selector
    -> Backend sends back a View (the data corresponding to the declared interest)
      -> Things that write to the db issue notifications that they're changing certain tables and rows
        -> Backend reads these notifications and checks if they're relevant to any View Selectors
          -> Backend computes a patch of information to get all relevant clients up-to-date and sends its out
```

A few important properties that must be preserved: It must be possible to combine all the View Selectors declared by a client (and ultimately, all the View Selectors declared by all clients) - we don't want to process everything individually for each client, or ask for the same data twice.  It must also be possible for users to declare that they're no longer interested in something, and it must be possible to remove things from the View that are no longer relevant.

We'll use the `vessel` library to define our View Selector and View.

#### The View (c11a5578)

In `Common.View` we define a GADT that represents both our view selector and view types.  We're using the `vessel` library to facilitate this: it contains data structures that can have both the "empty" (view selectors) and "full" (views) versions.

#### Notifications (c11a5578)

In `Backend.Listen` we define another GADT. This one describes the types of database change notifications our application will produce and handle.  These notifications are sent over a postgres [NOTIFY](https://www.postgresql.org/docs/current/sql-notify.html) channel, and every backend connected to the database will receive them.  Notifications are sent over the channel using the `notify` function from `Rhyolite.Backend.Listen` or one of the various specialized insert, delete, or update functions in that module (e.g., `insertAndNotify`).

The third argument to `serveDbOverWebsockets` is a handler for these notifications. We define the `notifyHandler` function to check notifications against the aggregated view selectors and return patches.


