# Makefile to build and deploy

.PHONY: prod
prod:
	dune build @install -p geopub

.PHONY: deploy
deploy: prod
	rsync -rav -L -e ssh _build/install/default/share/geopub/ pukkamustard@qfwfq.inqlab.net:/srv/http/geopub.openengiadina.net/ --delete
