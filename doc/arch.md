# GeoPub Architecture

This document describes the high-level architecture and ideas of GeoPub from the perspective of developers.

GeoPub is an Web application written in the [OCaml](https://ocaml.org/) programming language. The OCaml code is compiled to JavaScript via [js_of_ocaml](https://github.com/ocsigen/js_of_ocaml/).

## Functional Reactive Programming

### Functional Relational Programming

## Components

### `Ui`

### `Router`

### `Database`

### `User`

### `Geopub_map`

### `Xmpp`

#### `Xmpp.Connection`

#### `Xmpp.Entity_capabilities`

### `Xmpp_rdf`

Reads all incoming XMPP stanzas and attempts to parse RDF data from them. If some RDF data can be parsed it is added to the database.




