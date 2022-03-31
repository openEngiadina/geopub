all: activitystreams2.xml rdfs.xml owl.xml

%.xml: %.ttl
	rapper -i turtle $< -o rdfxml > $@
