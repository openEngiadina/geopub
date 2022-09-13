# GeoPub Architecture

This document describes the high-level architecture and ideas of GeoPub from the perspective of developers.

GeoPub is an Web application written in the [OCaml](https://ocaml.org/) programming language. The OCaml code is compiled to JavaScript via [js_of_ocaml](https://github.com/ocsigen/js_of_ocaml/).

## Functional Reactive Programming

TODO

### Functional Relational Programming

TODO

## Components

GeoPub is organized into various smaller components. Components handle specific functionaity and may hold state. Components interact with each other over some clearly defined interface and should be as loosely-coupled as possible.

We use the [archi](https://github.com/anmonteiro/archi) library to manage components.

The system (collection of components) is started from the [`main.ml`](../src/geopub/main.ml) module.

### [`Ui`](../src/geopub/ui.ml)

The user-interface of GeoPub. This listens for changes in the router view and loads the appropriate view to the DOM.

### [`Router`](../src/geopub/router.ml)

Listens for changes in the navigator location.

### [`Database`](../src/geopub/database/database.mli)

Store RDF data in an IndexedDB database.

Provides a Dataog interface for querying the database. Results are returned as signals (see section on functional reactive programming above).

### [`User`](../src/geopub/user.ml)

Manages the user session, i.e. an user interface for an XMPP connection.

### [`Geopub_map`](../src/geopub/geopub_map.ml)

Handles the Leaflet map. As Leaflet does not provide a functional interface this component holds state so that the map appears functional to the rest of GeoPub.

Note: this is not called `Map` to avoid clash with the OCaml standard library `Map`.

### [`Xmpp`](../src/geopub/xmpp/xmpp.mli)

XMPP functionality.

#### [`Xmpp.Connection`](../src/geopub/xmpp/connection.ml)

Manages the underlying XMPP connection.

#### [`Xmpp.Entity_capabilities`](../src/geopub/xmpp/entity_capabilities.ml)

Handles entity capability requests. This is necessary to receive XMPP PubSub notifications.

### [`Xmpp_rdf`](../src/geopub/xmpp_rdf.ml)

Reads all incoming XMPP stanzas and attempts to parse RDF data from them. If some RDF data can be parsed it is added to the database.



