all: activitystreams2.xml rdfs.xml owl.xml sample-data.xml

%.xml: %.ttl
	rapper -i turtle $< -o rdfxml > $@
