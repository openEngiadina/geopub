# Changelog

## [UNRELEASED]

- Add index for full-text search

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
