-include .env
GIT_TAG ?= $(shell git log --oneline | head -n1 | awk '{print $$1}')
DOCKER_IMAGE := coprosmo/bdhs-dev
SHELL = /bin/bash -o pipefail
PROLOG ?= swi
RUN_PROLOG ='cd prolog && $(PROLOG_EXECUTABLE)'
# RUN := docker run --platform linux/amd64 -it -rm -v $$(pwd)/assets:/code/assets -w /code $(DOCKER_IMAGE):latest

RUN := docker run --platform linux/amd64 -it --rm -v $$(pwd):/code -w /code $(DOCKER_IMAGE):latest

PANCAKE_SIZES ?= 3 4 5 6
SLIDING_TILE_SIZES := 8
CYCLIC_TILE_SIZES := 8

PANCAKE_DEGRADATIONS := 0 1 2
TILE_DEGRADATIONS := 0 1 2 3 4 5 6 7 8


PANCAKE_DATA_FILES := $(foreach ps,$(PANCAKE_SIZES),$(foreach d,$(PANCAKE_DEGRADATIONS),.$(ps)_d$(d)_pancake_pl))
SLIDING_TILE_DATA_FILES := $(foreach ss,$(SLIDING_TILE_SIZES),$(foreach d,$(TILE_DEGRADATIONS),.$(ss)_d$(d)_slidingtile_pl))
CYCLIC_TILE_DATA_FILES := $(foreach cs,$(CYCLIC_TILE_SIZES),$(foreach d,$(TILE_DEGRADATIONS),.$(cs)_d$(d)_cyclictile_pl))
SEARCH_DATA_FILES := $(foreach f,$(PANCAKE_DATA_FILES) $(SLIDING_TILE_DATA_FILES) $(CYCLIC_TILE_DATA_FILES),assets/$(f))


.PHONY: test prolog results prolog_inputs print_test
.PRECIOUS: assets/%_data.json


ifeq ($(PROLOG), swi)
    PROLOG_EXECUTABLE = swipl
else ifeq ($(PROLOG), scryer)
    PROLOG_EXECUTABLE = scryer-prolog
else
    $(error Invalid value for PROLOG. Supported values are 'swi' and 'scryer'.)
endif

prolog_inputs: $(SEARCH_DATA_FILES)

assets: 
	mkdir -p assets

assets/.%_pl: \
			assets/%_data.json \
			assets
	$(RUN) bash -c "\
		bdhs json-to-prolog $< assets/$*_prolog_inputs && \
		touch $@"


assets/%_data.json: \
			python/pybound/write_prolog.py \
			assets
	$(RUN) bash -c "\
		bdhs search-results \
			$(word 3,$(subst _, ,$(@F))) \
			--size $(word 1,$(subst _, ,$(@F))) \
			--degradation $(subst d,,$(word 2,$(subst _, ,$(@F)))) \
			--results-path $@ \
	"


help:
	@awk '/^##.*$$/,/^[~\/\.a-zA-Z_-]+:/' $(MAKEFILE_LIST) | awk '!(NR%2){print $$0p}{p=$$0}' | awk 'BEGIN {FS = ":.*?##"}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}' | sort

## Build docker image
docker:
	docker build --platform linux/amd64 -t $(DOCKER_IMAGE):$(GIT_TAG) -f ./Dockerfile .
	docker tag $(DOCKER_IMAGE):$(GIT_TAG) $(DOCKER_IMAGE):latest

docker-push:
	docker push $(DOCKER_IMAGE):$(GIT_TAG)
	docker push $(DOCKER_IMAGE):latest

docker-pull:
	docker pull $(DOCKER_IMAGE):$(GIT_TAG)
	docker tag $(DOCKER_IMAGE):$(GIT_TAG) $(DOCKER_IMAGE):latest

# assets/$(PROBLEM_SIZE)_$(DOMAIN)_search_data.json:


# $(PROBLEM_SIZE)_$(DOMAIN)_prolog_atoms.

## Enter docker image dev container
enter:
	$(RUN) bash

prolog:
	$(RUN) bash -c $(RUN_PROLOG)

## Run all tests
test:
	poetry run py.test tests -v -rxXs


## Run failed tests
testfailed:
	poetry run py.test tests -v -rxXs --last-failed

clean:
	$(RUN) rm -rf assets/*.json assets/*_prolog_inputs assets/.*_pl

## Run precommit tests
pre:
	pre-commit run -a