# GeoPub

GeoPub is an XMPP client for geospatial data and RDF developed within the [openEngiadina](https://openengiadina.net) project.

## Open Local Knowledge

GeoPub has been developed to create and manage Open Local Knowledge.

Local knowledge consists of thousand of little pieces of information that describe the social, cultural and natural environment of an area. Examples include geographic information, social, cultural and economic activity or infrastructure status.

The variety of this data is considerable and we need to support them all. To do this GeoPub uses the Resource Description Framework (RDF).

GeoPub is not bound to a fixed set of data types, instead GeoPub works with RDF and any data that can be describes as RDF is supported by GeoPub.

GeoPub contains examples of how to use the [ValueFlows](https://www.valueflo.ws/) vocabulary for simple offers as well, the [OpenHospitalityNetwork](https://github.com/OpenHospitalityNetwork/ohn-solid) vocabulary for hospitality requests and offers as well as the [MusicOntology](http://musicontology.com) for sharing music. Other data types that have been successfully tested include OpenStreetMap data via the [osm2rdf](https://osm2rdf.cs.uni-freiburg.de/) project.

## Semantic Social Network

We acknowledge the fact that data creation and curation is a social activity. Pieces of information are created, commented on, shared and revised collectively.

To model this we use the [ActivityStreams](https://www.w3.org/TR/activitystreams-core/) vocabulary. This is the same vocabulary used by the [ActivityPub](https://www.w3.org/TR/activitypub/) protocol. Previously the openEngiadina project was using the ActivityPub protocol (see [openEngiadina: From ActivityPub to XMPP](https://inqlab.net/2021-11-12-openengiadina-from-activitypub-to-xmpp.html)).

ActivityStreams provides types that describe social interactions on small pieces of data - activities. For example there is an activity that describes the creation of a resource.

GeoPub uses the ActivityStreams vocabulary as a core vocabulary. Whenever a user does something in GeoPub this corresponds to an ActivityStreams activity.

## Technical Overview

GeoPub is a JavaScript web application. It is written in the [OCaml](https://ocaml.org/) programming language and compiled to JavaScript.

GeoPub works with standard XMPP servers (tested with [Prosody](https://prosody.im/) and [ejabberd](https://www.ejabberd.im/)). GeoPub does not require any specialized server software apart from the web server where the JavaScript application code as well as other resources are hosted. GeoPub can be hosted from a static site.

GeoPub receives and publishes content via XMPP over WebSocket (see [RFC 7395](https://www.rfc-editor.org/rfc/rfc7395.html)). The RDF/XML serialization is used and content is by default posted to the `net.openengiadina.xmpp.activitystreams` Personal Eventing Protocol node (see [XEP-0163](https://xmpp.org/extensions/xep-0163.html)). How RDF data is transported over XMPP is described in more detail in the post [ActivityStreams over XMPP](https://inqlab.net/2022-01-17-activitystreams-over-xmpp.html).

Data is persistently stored in a local [IndexedDB](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API) database. GeoPub maintains specialized indices that allows efficient querying for geospatial location, full-text search and usual RDF queries (see also the post [GeoPub: A Multi-Model Database](https://inqlab.net/2022-07-01-geopub-a-multi-model-database.html)).

In order to query data GeoPub uses Datalog, a database query language based on logic programming. See also the post [GeoPub: Datalog, RDF and IndexedDB](https://inqlab.net/2022-04-14-geopub-datalog-rdf-and-indexeddb.html).

## History

GeoPub was started as a very simple JavaScript demonstrator to show how geographic information can be used over the ActivityPub protocol (see branch `initial-js-demonstrator`).

A second version was developed in ClojureScript (see branch `cljs`). The ClojureScript version was abandoned due to licensing and reproducability issues (it is hard to use GPL/AGPL for Clojure(Script) projects and Clojure support in Guix/Nix is not so good).

This (the third version) is implemented in OCaml using `js_of_ocaml`.

## Hacking

A development environment can be created with [Guix](https://guix.gnu.org/):

``` sh
guix shell -D -f guix.scm
```

After this you can run `dune build @install` which will build GeoPub and place all necessary artifacts into `_build/install/default/share/geopub/`.

You may want to start a small webserver for local development (e.g. `python3 -m http.server --directory _build/install/default/share/geopub/`). This works very nicely when running dune in watch mode (`dune build @install --watch`).

Note that this uses dependencies from Guix. You might have to run `guix pull` to get the lastest dependencies from Guix. To use a version of Guix that has been tested use: `guix time-machine -C channels.scm -- shell -D -f guix.scm`.

See also some [notes on the architecture](./doc/arch.md).

## Contact

See the [openEngiadina project site](https://openengiadina.net/).

## Acknowledgments

GeoPub is being developed for the [openEngiadina](https://openengiadina.net) project and has been supported by the [NLnet Foundation](https://nlnet.nl/) through the [NGI0 Discovery Fund](https://nlnet.nl/discovery/).

## License

[AGPL-3.0-or-later](./LICENSES/AGPL-3.0-or-later.txt)
