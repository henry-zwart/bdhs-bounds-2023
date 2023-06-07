DOCKER_IMAGE := bdhs-dev
MAKEFLAGS += --warn-undefined-variables
SHELL = /bin/bash -o pipefail
.DEFAULT_GOAL := help

help:
	@awk '/^##.*$$/,/^[~\/\.a-zA-Z_-]+:/' $(MAKEFILE_LIST) | awk '!(NR%2){print $$0p}{p=$$0}' | awk 'BEGIN {FS = ":.*?##"}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}' | sort

## Build docker image
docker:
	docker build -t $(DOCKER_IMAGE) -f ./Dockerfile .

## Enter docker image dev container
enter:
	docker run -it -v $$(pwd):/code -w /code $(DOCKER_IMAGE) bash


.PHONY: test
## Run all tests
test:
	poetry run py.test tests -v -rxXs


## Run failed tests
testfailed:
	poetry run py.test tests -v -rxXs --last-failed

## Run precommit tests
pre:
	pre-commit run -a