# GeoPub

.PHONY: release
release: node_modules
	clj -A:shadow-cljs -m shadow.cljs.devtools.cli release app
	mkdir -p release/js
	cp -r resources/public/* release/
	cp target/js/main.js release/js/main.js

node_modules:
	npm install

.PHONY: watch
watch: node_modules
	clj -A:shadow-cljs -m shadow.cljs.devtools.cli watch app

.PHONY: clean
clean:
	rm -rf node_modules/
	rm -rf .shadow-cljs/
	rm -rf target/
	rm -rf release/
