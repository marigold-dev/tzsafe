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
APP_DIRECTORY:=src
TEST_DIRECTORY:=test
BUILT_APP_DIRECTORY:=$(BUILD_DIRECTORY)/$(APP_DIRECTORY)

# Ligo compiler
LIGO_COMPILER_ARGS:=--protocol nairobi
LIGO_VERSION:=1.3.0
LIGO?=ligo
LIGO_BUILD=$(LIGO) compile contract $(LIGO_COMPILER_ARGS)
LIGO_TEST=$(LIGO) run test
LIGO_CURRENT_VERSION:=$(shell $(LIGO) --version)
LIGO_INSTALL=$(LIGO) install

# Tezos binaries
TEZOS_BINARIES_VERSION:=v19.0-1
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
	$(LIGO_BUILD) $(APP_DIRECTORY)/wallet.mligo > $(BUILT_APP_DIRECTORY)/$(PROJECT_NAME)_WALLET.tez
	$(LIGO_BUILD) $(APP_DIRECTORY)/fa2.mligo > $(BUILT_APP_DIRECTORY)/$(PROJECT_NAME)_NFT.tez

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

	echo deploy wallet	$(BUILD_DIRECTORY)/octez-client --endpoint https://ghostnet.tezos.marigold.dev originate contract $(PROJECT_NAME)_WALLET transferring 0.000001 from wallet_address running $(BUILT_APP_DIRECTORY)/$(PROJECT_NAME)_WALLET.tez --init '(Pair 0 {} {} {"$(SIGNER)"; "tz1inzFwAjE4oWXMabJFZdPHoDQN5S4XB3wH"} 1 604800 {})' --burn-cap 3 -f

	echo deploy NFT
	$(BUILD_DIRECTORY)/octez-client --endpoint https://ghostnet.tezos.marigold.dev originate contract $(PROJECT_NAME)_NFT transferring 0 from wallet_address running $(BUILT_APP_DIRECTORY)/$(PROJECT_NAME)_NFT.tez --init '(Pair {} {} {} {} "$(SIGNER)" {} {} {})' --burn-cap 3 -f

get-tezos-binary:
	wget -O $(BUILD_DIRECTORY)/octez-client $(TEZOS_BINARIES_URL)/octez-client
	chmod +x $(BUILD_DIRECTORY)/octez-client
