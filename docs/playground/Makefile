build: pkg/playground.js

.PHONY: clean
clean:
	rm -r pkg/

pkg/playground.js: src/lib.rs 
	wasm-pack build --target=no-modules