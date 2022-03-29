all: activitystreams2.xml

%.xml: %.ttl
	rapper -i turtle $< -o rdfxml > $@
