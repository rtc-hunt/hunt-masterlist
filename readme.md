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

