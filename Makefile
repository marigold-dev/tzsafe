.PHONY: build test ligo-version
.PHONY: clean tarball-app
.PHONY: install get-tezos-binary gen_wallet deploy
.PHONY: build-contract build-metadata

# Environment variables

# The version for artifacts releases
PROJECT_NAME:=$(shell basename $(CURDIR))
BUILD_VERSION ?= $(shell git describe --always)

# The path of directory
BUILD_DIRECTORY:=_build
APP_DIRECTORY:=app
TEST_DIRECTORY:=test
BUILT_APP_DIRECTORY:=$(BUILD_DIRECTORY)/$(APP_DIRECTORY)

# Ligo compiler
LIGO_COMPILER_ARGS:=--protocol kathmandu
LIGO_VERSION:=0.60.0
LIGO?=ligo
LIGO_BUILD=$(LIGO) compile contract $(LIGO_COMPILER_ARGS)
LIGO_TEST=$(LIGO) run test
LIGO_CURRENT_VERSION:=$(shell $(LIGO) --version)
LIGO_INSTALL=$(LIGO) install

# Tezos binaries
TEZOS_BINARIES_VERSION:=v15.1-1
TEZOS_BINARIES_REPO:=https://github.com/serokell/tezos-packaging/releases/download/
TEZOS_BINARIES_URL:=$(TEZOS_BINARIES_REPO)$(TEZOS_BINARIES_VERSION)

# Toplevel rules
all: build test tarball-app

install:
	ligo install

build: build-contract build-metadata

build-metadata:
	VERSION=$(BUILD_VERSION) bash ./script/gen_metadata.sh > $(BUILT_APP_DIRECTORY)/metadata.json

build-contract: check-ligo-version
	mkdir -p $(BUILT_APP_DIRECTORY)
	$(LIGO_BUILD) $(APP_DIRECTORY)/main_unit.mligo > $(BUILT_APP_DIRECTORY)/$(PROJECT_NAME)_unit.tez
	$(LIGO_BUILD) $(APP_DIRECTORY)/main_bytes.mligo > $(BUILT_APP_DIRECTORY)/$(PROJECT_NAME)_bytes.tez

test:
	$(LIGO_TEST) $(TEST_DIRECTORY)/test.mligo

clean:
	rm -rf $(BUILD_DIRECTORY)
	rm -rf _esy
	rm -rf .ligo
	rm -rf esy.lock

tarball-app:
	tar czvf $(BUILD_DIRECTORY)/$(PROJECT_NAME)-$(BUILD_VERSION).tar.gz $(BUILT_APP_DIRECTORY)/*

ligo-version:
	@echo $(LIGO_VERSION)

check-ligo-version:
ifneq (${LIGO_VERSION},${LIGO_CURRENT_VERSION})
	$(error Unexpected ligo version (found: ${LIGO_CURRENT_VERSION}, expected: ${LIGO_VERSION}))
endif

gen-wallet:
	$(BUILD_DIRECTORY)/octez-client gen keys wallet_address -f 2> /dev/null
	$(BUILD_DIRECTORY)/octez-client show address wallet_address -S 2> /dev/null
	@echo -e "\e[32m!!! Please go https://faucet.marigold.dev/ to request some XTZ !!!\e[0m"

deploy:
	$(eval SIGNER := $(shell TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=yes ./_build/octez-client --endpoint https://ghostnet.tezos.marigold.dev show address wallet_address | grep Hash | awk '{print $$2}'))
	$(BUILD_DIRECTORY)/octez-client --endpoint https://ghostnet.tezos.marigold.dev originate contract $(PROJECT_NAME)_unit transferring 2 from wallet_address running $(BUILT_APP_DIRECTORY)/$(PROJECT_NAME)_unit.tez --init '(Pair 0 {} {"$(SIGNER)";} 1 604800 {})' --burn-cap 2 -f
	$(BUILD_DIRECTORY)/octez-client --endpoint https://ghostnet.tezos.marigold.dev originate contract $(PROJECT_NAME)_bytes transferring 2 from wallet_address running $(BUILT_APP_DIRECTORY)/$(PROJECT_NAME)_bytes.tez --init '(Pair 0 {} {"$(SIGNER)";} 1 604800 {})' --burn-cap 2 -f

get-tezos-binary:
	wget -O $(BUILD_DIRECTORY)/octez-client $(TEZOS_BINARIES_URL)/octez-client
	chmod +x $(BUILD_DIRECTORY)/octez-client
