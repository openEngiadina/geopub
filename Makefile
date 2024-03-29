# Makefile to build and deploy

.PHONY: prod
prod:
	dune build @install --release

.PHONY: deploy
deploy: prod
	rsync -rav -L -e ssh _build/install/default/share/geopub/ qfwfq:/srv/http/geopub.openengiadina.net/ --delete
