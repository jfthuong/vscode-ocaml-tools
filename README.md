<!-- cspell:word Ocamllex, Ocamlyacc -->
# OCaml Additional Syntax Highlighting

Support additional syntax highlighting for few OCaml tools in Visual Studio Code.

Additionally, it provides few snippets for these same file types.

## Getting Started

This extension requires the extension [OCaml and Reason IDE](https://github.com/reasonml-editor/vscode-reasonml) for formatting Ocaml code.

# Features

## Highlighting

This extension provides additional syntax highlighting for OCaml and related tools:

* `ocp-build` by OCaml PRO (**.ocp**)
* `Ocamllex` (**.mll**)
* `Ocamlyacc` (**.mly**)

## Snippets

Snippets for **.ocp**:

* `program`
* `library`
* `test`

# Known Issues

* **.ocp** Files:
  * `begin` / `end` block inside a list of `files` is not properly highlighted
* **.mll** Files:
  * Comments not properly highlighted
  * Top-Level curly brackets cannot be collapsed / expanded
* **.mly** Files:
  * Comments strangely highlighted sometimes in the rules

# FAQ

## Where do the Grammar rules come

The grammar syntaxes for **.mll** and **.mly** came from the deprecated extension
[*vscode-ocaml*](https://github.com/hackwaly/vscode-ocaml/tree/master/syntaxes),
with some modifications for OCamlyacc (.mly).
