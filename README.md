# Readme

The repository is set up to use Docker to manage development tools (prolog, python, 
etc). This means that you don't need to install a bunch of random tools locally, and 
that we should be able to run experiments the same way everywhere.

## Installation

Requirements:

- [Docker](https://docs.docker.com/get-docker/)

## Development environment

To enter the development environment, run `make enter` from the top level of the
"bdhs-bounds-2023" directory. A docker environment should launch, and send you to a
bash shell, where you'll have access to prolog and python.
