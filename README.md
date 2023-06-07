# Readme

The repository is set up to use Docker to manage development tools (prolog, python, 
etc). This means that you don't need to install a bunch of random tools locally, and 
that we should be able to run experiments the same way everywhere.

## Installation

Requirements:

- [Docker](https://docs.docker.com/get-docker/)
- [A DockerHub account](https://hub.docker.com/) (to access the shared docker image)

## Development environment

Download the most recent docker image with `make docker-pull`. 

Create a new docker image with `make docker`, followed by `make docker-push` to push
the new image to the docker registry.

To enter the development environment, run `make enter` from the top level of the
"bdhs-bounds-2023" directory. A docker environment should launch, placing you in a
bash shell, where you'll have access to prolog and python.

To enter a prolog interactive shell directly, in the "bdhs-bounds-2023/prolog"
directory, run `make enter-prolog`.
