GIT_TAG ?= $(shell git log --oneline | head -n1 | awk '{print $$1}')
DOCKER_IMAGE := coprosmo/bdhs-dev
SHELL = /bin/bash -o pipefail
PROLOG ?= swi
RUN_PROLOG ='cd prolog && $(PROLOG_EXECUTABLE)'
RUN := docker run --platform linux/amd64 -it -v $$(pwd)/assets:/code/assets -w /code $(DOCKER_IMAGE):latest

PANCAKE_SIZES := 3
SLIDING_TILE_SIZES :=

PANCAKE_DATA_FILES := $(foreach ps,$(PANCAKE_SIZES),$(ps)_pancake_data.json)
SLIDING_TILE_DATA_FILES := $(foreach ss,$(SLIDING_TILE_SIZES),$(ss)_sliding_tile_data.json)
SEARCH_DATA_FILES := $(foreach f,$(PANCAKE_DATA_FILES) $(SLIDING_TILE_DATA_FILES),assets/$(f))

# configurations := \
# 	assets/3_pancake_search_data.json \
# 	assets/4_pancake_search_data.json \
# 	assets/5_pancake_search_data.json

.PHONY: test prolog results search_data

search_data: $(SEARCH_DATA_FILES)

assets/.%_prolog_inputs: assets/%_data.json
	$(RUN) bash -c "\
		[ -d assets/$*_prolog_inputs ] || mkdir assets/$*_prolog_inputs && \
		bdhs json-to-prolog $> assets/$*_prolog_inputs"


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
	docker build --platform linux/amd64 -t $(DOCKER_IMAGE):$(GIT_TAG) -f ./Dockerfile .
	docker tag $(DOCKER_IMAGE):$(GIT_TAG) $(DOCKER_IMAGE):latest

docker-push:
	docker push $(DOCKER_IMAGE):$(GIT_TAG)
	docker push $(DOCKER_IMAGE):latest

docker-pull:
	docker pull $(DOCKER_IMAGE):$(GIT_TAG)
	docker tag $(DOCKER_IMAGE):$(GIT_TAG) $(DOCKER_IMAGE):latest

# assets/$(PROBLEM_SIZE)_$(DOMAIN)_search_data.json:
$(SEARCH_DATA_FILES):
	$(RUN) bash -c "\
		poetry run bdhs search-results \
			$(word 2,$(subst _, ,$(@F))) \
			--size $(word 1,$(subst _, ,$(@F))) \
			--results-path $@"

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
	$(RUN) rm -rf assets/*.json

## Run precommit tests
pre:
	pre-commit run -a