# `lambda`

A simple lambda calculus interpreter.


## Usage

See the help:

    lambda --help


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

To import a library module into GHCi:

    Prelude> :l lib/Lambda.hs

To load all libraries into GHCi:

    cabal repl Lambda

To clean:

    cabal new-clean


