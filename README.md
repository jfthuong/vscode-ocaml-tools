<!-- cspell:word Ocamllex, Ocamlyacc -->
# OCaml Additional Syntax Highlighting

Support additional syntax highlighting for OCaml programming language in Visual Studio Code.

## Highlighting

This extension provides additional syntax highlighting for OCaml and related tools:

* `ocp-build` by OCaml PRO (**.ocp**)
* `Ocamllex` (**.mll**)
* `Ocamlyacc` (**.mly**) [__NOT WORKING YET__]

## Known Issues

* **.ocp** Files:
  * `begin` / `end` block inside a list of `files` is not properly highlighted
* **.mll** Files:
  * Comments not properly highlighted
* **.mly** Files:
  * Syntax Highlighting not working
