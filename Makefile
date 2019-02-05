
.PHONY: build
build: build/index.html
	@rm -rf build
	@elm make src/Main.elm --output=build/index.html
	@cp -r src/assets build/assets

build/index.html: $(glob src/*)
	@elm make src/Main.elm --output=build/index.html
	@cp -r src/assets build/assets

.PHONY: publish
publish: build
	scp -r build/* dv@winged.ch:/srv/http/408.ch/htdocs/hexelm
