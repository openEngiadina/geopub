# GeoPub

GeoPub is an ActivityPub client developed within the [openEngiadina](https://openengiadina.net) project.

For more information see also:

- [Initial announcement of GeoPub on the OCaml forum](https://discuss.ocaml.org/t/ann-geopub-a-xmpp-web-client/8819)
- [openEngiadina: From ActivityPub to XMPP](https://inqlab.net/2021-11-12-openengiadina-from-activitypub-to-xmpp.html): Post describing why and how XMPP is used as transport.
- [ActivityStreams over XMPP](https://inqlab.net/2022-01-17-activitystreams-over-xmpp.html): Post describing how the ActivityStreams vocabulary is used over XMPP.

## History

GeoPub was started as a very simple JavaScript demonstrator to show how geographic information can be used over the ActivityPub protocol (see branch `initial-js-demonstrator`).

A second version was developed in ClojureScript (see branch `cljs`). The ClojureScript version was abandoned due to licensing and reproducability issues (it is hard to use GPL/AGPL for Clojure(Script) projects and Clojure support in Guix/Nix is not so good).

This (the third version) is implemented in OCaml using `js_of_ocaml`.

# Hacking

A development environment can be created with [Guix](https://guix.gnu.org/):

``` sh
guix shell -D -f guix.scm
```

After this you can run `dune build @install` which will build GeoPub and place all necessary artifacts into `_build/install/default/share/geopub/`.

You may want to start a small webserver for local development (e.g. `python3 -m http.server --directory _build/install/default/share/geopub/`). This works very nicely when running dune in watch mode (`dune build @install --watch`).

Note that this uses dependencies from Guix. You might have to run `guix pull` to get the lastest dependencies from Guix. To use a version of Guix that has been tested use: `guix time-machine -C channels.scm -- shell -D -f guix.scm`.

# Acknowledgments

GeoPub is being developed for the [openEngiadina](https://openengiadina.net) project and has been supported by the [NLNet Foundation](https://nlnet.nl/) trough the [NGI0 Discovery Fund](https://nlnet.nl/discovery/).

# License

[AGPL-3.0-or-later](./LICENSES/AGPL-3.0-or-later.txt)
