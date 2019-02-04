
build: $(glob src/*) $(glob src/assets/*)
	@rm -rf build
	@elm make src/Main.elm --output=build/index.html
	@cp -r src/assets build/assets
