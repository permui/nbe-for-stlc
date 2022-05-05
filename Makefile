
.PHONY: debug

debug:
	ocamldebug -I ./_build/install/default/lib/nbe_stlc _build/default/bin/main.bc
