# Changelog

## [0.7.0] - 2022-09-20

- Use [ERIS](http://purl.org/eris) for content-addressing
- Refactor UI with CSS library
- Componentize application into smaller, state-full parts
- Store XMPP credentials in local storage
- Expose a Datalog interface that transparently does dictionary lookups
- Like any RDF content
- Publish any RDF data as RDF/Turtle
- Publish simple ValueFlows proposals
- Add illustrated Datalog examples
- Allow custom clauses to be defined when querying with Datalog
- Add ValueFlows and OpenHospitalityNetwork examples

## [0.6.0] - 2022-07-01

- Add sample data extracted from the [osm2rdf](https://osm2rdf.cs.uni-freiburg.de/) TTL extracts
- Add index for full-text search
- Add index for geo-spatial search
- Extract leaflet bindings to separate library ([swrup/leaflet](https://git.zapashcanon.fr/swrup/leaflet))

## [0.5.0] - 2022-04-14

- Use IndexedDB for local storage and Datalog for querying

## [0.4.0] - 2022-01-17

- Transfer ActivityStreams content serialized as RDF/XML over an XMPP PEP node

## [0.3.0] - 2021-11-12

Rewrite in OCaml, using XMPP and with a map for posts with geolocation.

## [0.2.0] - 2020-08-12

### Added

- Authentication with OAuth 2.0
- Support [content-addressable RDF](https://openengiadina.net/papers/content-addressable-rdf.html)
- [ERIS encoding](https://openengiadina.net/papers/eris.html) for content-addressing

### Changed

- Refactor to [re-frame framework](https://day8.github.io/re-frame/re-frame/)

## [0.1.0] - 2020-04-03

Initial alpha release.

### Added

- Initial interface for viewing activites and browsing content
- Ability to like content
- Basic ActivityPub Client-to-Server (C2S) Protocol
- Support for ActivityStreams, Schema and RDF ontologies
