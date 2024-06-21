# -*- Makefile; encoding=utf-8; -*-
##############################################################################
# cl-versiontool Makefile
# Tammy Cravit <tammy@tammymakesthings.com>, 2024-06-21, v1.00
##############################################################################

LISP 			?= $(shell which sbcl | cut -d' ' -f1)
SYSTEM_NAME 	:= cl-versiontool
EXE_NAME		?= $(subst cl-,,$(SYSTEM_NAME))
PROJECT_DIR		:= $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

ASD_FILE		:= $(SYSTEM_NAME).asd
LISP_SOURCES	:= $(shell find src -name '*.lisp' -print)
TEST_SOURCES	:= $(shell find t -name '*.lisp' -print)
OTHER_FILES     := version.lisp-expr $(wildcard *.rst)

default: build test

help:		## Display this help message
	@egrep -h '\s##\s' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

debug:		## Display Makefile debugging output
	$(info =>         LISP is $(LISP))
	$(info =>  PROJECT_DIR is $(PROJECT_DIR))
	$(info =>  SYSTEM_NAME is $(SYSTEM_NAME))
	$(info =>     EXE_NAME is $(EXE_NAME))
	$(info =>     ASD_FILE is $(ASD_FILE))
	$(info => LISP_SOURCES is $(LISP_SOURCES))
	$(info => TEST_SOURCES is $(TEST_SOURCES))
	$(info =>  OTHER_FILES is $(OTHER_FILES))
	@/usr/bin/true

build:		## Build the versiontool executable
	$(MAKE) $(PROJECT_DIR)/bin/$(EXE_NAME)

$(PROJECT_DIR)/bin/$(EXE_NAME): $(ASD_FILE) $(LISP_SOURCES) $(OTHER_FILES)
	$(LISP) \
		--noinform \
		--eval "(load #P\"$(PROJECT_DIR)/$(ASD_FILE)\")" \
		--eval "(asdf:make :$(SYSTEM_NAME))" \
		--quit

test:		## Run the unit tests
	$(MAKE) unittest

clean:		## Clean up build files
	rm -f $(PROJECT_DIR)/bin/$(EXE_NAME)
	find $(HOME)/.cache/common-lisp -name $(SYSTEM_NAME) -print | xargs rm -fr
	rm -fr $(PROJECT_DIR)/.qlot

unittest:	$(ASD_FILE) $(LISP_FILES) $(TEST_SOURCES)
	$(LISP) \
		--noinform \
		--eval "(load #P\"$(PROJECT_DIR)/$(ASD_FILE)\")" \
		--eval "(asdf:test-system :$(SYSTEM_NAME))" \
		--quit

.PHONY: help test build unittest debug clean

