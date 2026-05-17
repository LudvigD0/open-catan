.PHONY: build serve clean

ROOT=..
PROJECT_FILE=cabal.project.wasm
EXE=frontend
PUBLIC=public
STATIC=static

build:
	cd $(ROOT) && wasm32-wasi-cabal build exe:$(EXE) --project-file=$(PROJECT_FILE)
	rm -rf $(PUBLIC)
	mkdir -p $(PUBLIC)
	cp -r $(STATIC) $(PUBLIC)/static
	if ls $(PUBLIC)/static/*.html >/dev/null 2>&1; then mv $(PUBLIC)/static/*.html $(PUBLIC)/; fi
	$(eval WASM_FILE=$(shell cd $(ROOT) && wasm32-wasi-cabal list-bin exe:$(EXE) --project-file=$(PROJECT_FILE) | tail -n 1))
	$$(wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(WASM_FILE) --output $(PUBLIC)/static/ghc_wasm_jsffi.js
	cp -v $(WASM_FILE) $(PUBLIC)/static/app.wasm

serve:
	http-server $(PUBLIC) -p 8081 -c-1

clean:
	rm -rf $(PUBLIC)
	cd $(ROOT) && rm -rf dist-newstyle

refresh: 
	make build
	make serve