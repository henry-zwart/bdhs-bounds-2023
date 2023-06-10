GIT_TAG ?= $(shell git log --oneline | head -n1 | awk '{print $$1}')
DOCKER_IMAGE := coprosmo/bdhs-dev
SHELL = /bin/bash -o pipefail
PROLOG ?= swi
RUN_PROLOG ='cd prolog && $(PROLOG_EXECUTABLE)'
.DEFAULT_GOAL := help

.PHONY: test prolog

ifeq ($(PROLOG), swi)
    PROLOG_EXECUTABLE = swipl
else ifeq ($(PROLOG), scryer)
    PROLOG_EXECUTABLE = scryer-prolog
else
    $(error Invalid value for PROLOG. Supported values are 'swi' and 'scryer'.)
endif

help:
	@awk '/^##.*$$/,/^[~\/\.a-zA-Z_-]+:/' $(MAKEFILE_LIST) | awk '!(NR%2){print $$0p}{p=$$0}' | awk 'BEGIN {FS = ":.*?##"}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}' | sort

## Build docker image
docker:
	docker build -t $(DOCKER_IMAGE):$(GIT_TAG) -f ./Dockerfile .
	docker tag $(DOCKER_IMAGE):$(GIT_TAG) $(DOCKER_IMAGE):latest

docker-push:
	docker push $(DOCKER_IMAGE):$(GIT_TAG)
	docker push $(DOCKER_IMAGE):latest

docker-pull:
	docker pull $(DOCKER_IMAGE):$(GIT_TAG)
	docker tag $(DOCKER_IMAGE):$(GIT_TAG) $(DOCKER_IMAGE):latest

## Enter docker image dev container
enter:
	docker run -it -v $$(pwd):/code -w /code $(DOCKER_IMAGE):latest bash

prolog:
	docker run -it -v $$(pwd):/code -w /code $(DOCKER_IMAGE):latest bash -c $(RUN_PROLOG)

enter-problems:
	docker run -it -v $$(pwd):/code -w /code $(DOCKER_IMAGE):latest bash -c "cd prolog/problems; swipl"

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