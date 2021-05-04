# GeoPub

GeoPub is an ActivityPub client developed within the [openEngiadina](https://openengiadian.net) project.

# Status

This is an experiment to re-write GeoPub in OCaml using js_of_ocaml.

## History

GeoPub was started as a very simple JavaScript demonstrator to show how geographic information can be used over the ActivityPub protocol (see branch `initial-js-demonstrator`).

A second version was developed in ClojureScript (see branch `cljs`). The ClojureScript version was abandoned due to licensing and reproducability issues (it is hard to use GPL/AGPL for Clojure(Script) projects and Clojure support in Guix/Nix is not so good).

# Hacking

A development environment can be created with [Guix](https://guix.gnu.org/):

``` sh
guix time-machine
    --url=https://gitlab.com/pukkamustard/guix.git \
    --branch=js-of-ocaml \
    --disable-authentication \
    -- environment -l guix.scm
```

After this you can run `dune build` to build GeoPub.

# Acknowledgments

GeoPub is being developed for the [openEngiadina](https://openengiadina.net) project and has been supported by the [NLNet Foundation](https://nlnet.nl/) trough the [NGI0 Discovery Fund](https://nlnet.nl/discovery/).

# License

[AGPL-3.0-or-later](./LICENSES/AGPL-3.0-or-later.txt)
