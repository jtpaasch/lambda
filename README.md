# `lambda`

A simple lambda calculus interpreter.


## Usage

See the examples in the headers of each library module.


## Build

Download the project, e.g.:

    git clone https://..../lambda.git
    cd lambda

Build it:

    make build

To clean:

    make clean


## Development

Download the project, e.g.:

    git clone https://..../lambda.git
    cd lambda

To build the library:

    cabal new-build

To import a particular library module into GHCi:

    Prelude> :l lib/Lambda.hs

Then experiment with its functions (see the header of `lib/Lambda.hs`
for some examples).

To load all libraries into GHCi:

    cabal repl lambda

To clean:

    cabal new-clean


