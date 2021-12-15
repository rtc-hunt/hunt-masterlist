# rhyolite-example

## Getting Started

Make sure that [Obelisk](https://github.com/obsidiansystems/obelisk) is installed on your machine.

You should be able to `ob run` from the root directory of the project, which should start a webserver
accessible from `http://localhost:8000/` in Chrome.

## Architecture

This describes the overall structure of the project and what you can expect to find in various files.

### ./common

This directory contains code that both the backend and frontend depend on, and generally contains definitions
of data structures that are used to communicate between them.

#### ./common/src/Common/Request.hs

This file defines the types used to make requests from the frontend to the backend. The requests here will
almost always be triggered by user actions in some way. We make a distinction between public requests,
which don't require a user to be logged in (they don't need an AuthToken) and private requests, which do.

The `PublicRequest` and `PrivateRequest` data types are both GADTs. The index type of the GADTs tells the
backend which type of response it needs to produce, and the frontend which type of response to expect.

You'll notice that we use the Template Haskell functions `deriveArgDict` and `deriveJSONGADT` for both of these
types. `deriveArgDict` gives us the ability to do things like insist that all the index types on a GADT belong
to a particular class (e.g. `ToJSON` and `FromJSON` since we'll need to be able to serialise the responses).
`deriveJSONGADT` gives us `ToJSON` and `FromJSON` instances related to the GADTs themselves.

The requests and responses here are one part of Rhyolite's communication protocol that goes over its websocket,
along with the views and viewselectors.

#### ./common/src/Common/View.hs

This file contains the view/viewselector type(s) for the app, and related types involved in their construction.
Recall that viewselectors identify data that the frontend could be interested in displaying, and views are
the corresponding responses to requests for that data.

In this project, we're using vessel-style functor parametric containers to describe both our viewselector type
and view type simultaneously, distinguished by a choice of functor that the types involved have been
parameterised over. `PrivateChatV` is the main container, and you can see that it's defined as a `Vessel`
keyed by values of type `V`, which is a GADT whose index types describe which type of functor-parametric
container to associate with each possible key. In this case, we have
* `V_Chatrooms` which is a key containing searches for chatrooms (by name, at least for now). The associated
 container here is a `MapV` from the queries to a `SemiMap` from `Id Chatroom` to `Text`. The `MapV`
 lets us only provide keys in the viewselector, and the `SemiMap`s will only exist in the view. Here,
 `SemiMap` is a type that lets us distinguish between complete key/value pairings, that is, complete
 responses for what we requested, and partial ones, used to patch up the mapping as the backend will want to
 notify the frontend about new individual channels that are being added which match the query.
* `V_Chatroom` is a key containing requests for information about individual chat rooms, based on their `Id`,
 (which is simply a number internally). As the example grows, we might put other details about the chatroom
 in the response here, e.g. a topic message, but right now it's just the name of the room.
* `V_Messages` is a key containing requests for messages in a particular chat room. It's a `SubVessel`, which
 is a type that lets us associate a functor-parametric container of the same type with each key of type
 `Id Chatroom` here. So for each `Id Chatroom` we might want to obtain messages for, we have a `MapV`
 from `RequestInterval` to a `SemiMap` that will contain the actual messages being requested (keyed on
 timestamp and the `Id` of the message).

`RequestInterval` is a type that describes which range of messages from a channel we want to obtain.
It's generally important for the sake of performance not to have requests for completely unbounded portions
of the database. So this type lets us request the nth message in the channel, and a specified number of
messages before and after that. We'll eventually use this to implement infinite scrolling: when the user
nears the top of their scrollback, we can add an additional `RequestInterval` back from whatever message is
at the top of their view, and as they scroll around, we'll also be able to delete any that are far from where
they're looking, as a performance improvement.

#### ./common/src/Common/Route.hs

This file contains GADTs that are abstract representations of the HTTP routes that the backend serves, i.e. the
part of the URL from the first `/` and including any `?` parameters, but not including `#` parameters. It also
contains the `Encoder` that converts back and forth between the abstract data type and the actual bunch of path
components and queries (`PageName`).

The index types of these GADTs is typically what type of data is associated with the particular route, if any.
For example, you'll notice that `FrontendRoute_Channel` will also be accompanied with an `Id Chatroom` because
we (at least presently) have URLs like "http://localhost:8000/channel/16" which will be a link that brings up
that particular channel in the frontend.

`FullRoute` is a type from the Obelisk routes package that combines our `BackendRoute` and `FrontendRoute` types
and mixes in a few additional routes that Obelisk needs. The `R p` type is basically like `DSum p Identity` --
it just pairs up the main part of the route with whatever data is meant to be associated with that path, that
often, but not always, will get encoded toward the end of the route.

#### ./common/src/Common/Schema.hs

This file contains datatypes that will be involved in the frontend's view of data, but which also correspond
directly with the contents of tables in our application's database.

### ./backend

This directory contains code that does not ship as part of any frontend. It's the program that actually runs
on the servers and handles HTTP requests, and websocket connections.

#### ./backend/Backend.hs

More or less the main entry point of the Obelisk backend.

We get the key used to encrypt user tokens, do any database migration that's required, set up the websocket
listener, and then start up the webserver, and provide specific handlers for our BackendRoutes
(see Common.Route). We also plug in the route encoder that was specified in Common.Route here.

#### ./backend/Request.hs

This contains the handler for both public and private requests (see Common.Request above).

Essentially all changes to the database will be triggered by these request handlers, aside from migrations.

After each change to the database, we'll also want to notify all running backends (there can be multiple
copies of this program running on many servers if needed) of our change, so that they can respond and adjust
the views that the frontends are seeing. Hence, you'll see some usage of `insertAndNotify`. This uses
the NOTIFY/LISTEN mechanism in PostgreSQL to notify all running backends of the change, which will be picked up
by the code in `Backend.Listen` that will calculate patches to connected users' views.

There can be fancier and more general forms of notification if we needed to notify about updates or deletions
of data, or for larger bulk changes of data, but our example app doesn't do any of that yet.

#### ./backend/View.hs

This file contains handlers that take viewselectors, and produce views, which are run when frontends update
their viewselectors, and need to get their initial view.

In this case, there's just the top level `privateQueryHandler`, and most of the work of
obtaining individual things has been split out into other modules. This top level function iterates over the
keys in the overall view selector, and handles the queries contained under each one (if any), mostly just
doing the combinatorics of unpacking the query a bit and re-packing the results into a view.

#### ./backend/View/

This directory contains modules with functions for actually doing database queries related to
`privateQueryHandler`. Generally the functions have types that are a bit simplified from the full actual
view/selector datatypes.

#### ./backend/Listen.hs

When our backend receives a notification from the PostgreSQL server that data has been changed, we want to
notify any clients of the impact to their view, based on their viewselector. This module defines both the
`Notify` data type that the backend uses to describe these messages to itself, which is again a GADT whose
index describes a payload (usually the database ID that changed, sometimes additional data about what changed).

An important consideration is that postgres limits the length of NOTIFY messages to 4096 bytes, and so if the
notification messages get too long, they might be silently truncated. So, take care not to put anything
of an arbitrary length in a notification. So far, we just have database `Id` values in our notifications in
this app, which are actually just Int64 values.

The `notifyHandler` takes the message received from the database, along with an aggregate viewselector that
accounts for what every connected client is interested in. Its job is then to produce a partial view of just
the new or changed data with respect to those queries, which will then be further divided up into the bits
that individual clients are interested in before sending out messages over the websocket.

It will generally do this by looking up the data whose `Id` it received a notification about, and observing
the viewselector to determine which parts matched.

It is vitally important that this function be reasonably quick, because it runs on a thread which is shared
between all connected clients of the backend, so if this process gets hung up, the app potentially becomes
unresponsive for many users. Anything which takes a substantial amount of time (e.g. has linear or greater
complexity) should not be done here, and moved off into an asynchronous thread, which can perhaps send further
notifications when any heavier work is done. But mostly, this function doesn't need to do all that much work
anyway, so long as the database has relevant indices to quickly determine whether things match.

#### ./backend/Schema.hs

This module contains a specification of the database schema used by the backend, and exports a function
`migrateSchema` that can migrate the database (or at least determine what migration needs to occur in cases
where this may be destructive) should the schema of a running database instance differ from what the backend
expects. It's very important in general that running database schemas always exactly match what the backends
expect, so some degree of automatic checking on backend startup is vital (and automatic migration is obviously
convenient whenever it suffices).

### ./frontend

This directory contains the frontend. Perhaps surprisingly, this code in its entirety will also be part of the
backend, used to pre-render any frontend routes, so that when they're delivered, they look approximately
correct even before all the javascript loads.
