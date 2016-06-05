# ocaml-aiger
An Ocaml library to read, write and manipulate AIGER files


## Installation
To install this library, you need Ocaml and ocamlbuild. 
On debian/ubuntu simply run `sudo apt-get install ocamlbuild`.
Then launch the compilation: `make` and optionnaly `sudo make install`.

## Examples
Several examples using the library are included in the examples directory.
In particular, the program rename renames a variables into another one.
compose put two circuits in serie.
minispec parse a minimal language for the description of circuits, an example of a circuit described with this language is given in minispec_example.spec
