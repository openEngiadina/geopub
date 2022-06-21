# osm_extractor

This is a small tool to extract interesting nodes from [OpenStreetMap TTL Extracts](https://osm2rdf.cs.uni-freiburg.de/) as provided by the [osm2rdf](https://github.com/ad-freiburg/osm2rdf) project.

## Usage

`osm_extractor` reads N-Triples from standard input and writes N-Triples of interesting features (currently OSM nodes with the key `man_made=surveillance`) to standard output.

As the osm2rdf extracts are provided as Turtle files we need to first convert them to N-Triples (this will no longer be necessary as soon as the OCaml Turtle parser is available):

```
bunzip2 che.osm.ttl.bz2 | serdi -i turtle -o ntriples - | pv -c | dune exec ./osm_extractor.exe | tee che.osm.surveillance.nt
```

