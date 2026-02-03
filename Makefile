SHELL := /usr/bin/env bash

COQ_BUILD_CONTEXT=../_build/default/coq

# Set to true for main, comment out for released versions
VENDORED_SETUP:=true

ifdef VENDORED_SETUP
PKG_SET= \
vendor/coq/rocq-runtime.install \
vendor/coq/rocq-core.install \
vendor/coq/coq-core.install \
vendor/coq-stdlib/rocq-stdlib.install \
vendor/coq-stdlib/coq-stdlib.install \
coq-lsp.install
else
PKG_SET= coq-lsp.install
endif

PKG_SET_WEB=$(PKG_SET) vendor/coq-waterproof/coq-waterproof.install

# Get the ocamlformat version from the .ocamlformat file
OCAMLFORMAT=ocamlformat.$$(awk -F = '$$1 == "version" {print $$2}' .ocamlformat)

DEV_DEPS= \
dune \
$(OCAMLFORMAT) \
ocaml-lsp-server

.PHONY: build
build: coq_boot
	@dune build $(DUNEOPT) $(PKG_SET)

.PHONY: check
check: coq_boot
	dune build $(DUNEOPT) @check

test/server/node_modules: test/server/package.json
	cd test/server && npm i

.PHONY: test
test: build test/server/node_modules
	cd test/server && npm test

.PHONY: test-compiler
test-compiler:
	dune runtest

.PHONY: fmt format
fmt format:
	dune fmt $(DUNEOPT)

.PHONY: watch
watch: coq_boot
	dune build -w $(DUNEOPT) $(PKG_SET)

.PHONY: build-all
build-all: coq_boot
	dune build $(DUNEOPT) @all

vendor/coq:
	$(error Submodules not initialized, please do "make submodules-init")

COQVM=yes

# We set -libdir due to a Coq bug on win32, see
# https://github.com/coq/coq/pull/17289 , this can be removed once we
# drop support for Coq 8.16
vendor/coq/config/coq_config.ml: vendor/coq
	EPATH=$(shell pwd) \
	&& cd vendor/coq \
	&& ./configure -no-ask -prefix "$$EPATH/_build/install/default/" \
	        -libdir "$$EPATH/_build/install/default/lib/coq" \
	        -bytecode-compiler $(COQVM) \
		-native-compiler no \
	&& cp theories/Corelib/dune.disabled theories/Corelib/dune \
	&& cp theories/Ltac2/dune.disabled theories/Ltac2/dune

# We set windows parameters a bit better, note the need to use forward
# slashed (cygpath -m) due to escaping :( , a conversion to `-w` is
# welcomed if someones has time for this
.PHONY: winconfig
winconfig:
	EPATH=$(shell cygpath -am .) \
	&& cd vendor/coq \
	&& ./configure -no-ask -prefix "$$EPATH\\_build\\install\\default\\" \
	        -libdir "$$EPATH\\_build\\install\\default\\lib\\coq\\" \
		-native-compiler no \
	&& cp theories/Corelib/dune.disabled theories/Corelib/dune \
	&& cp theories/Ltac2/dune.disabled theories/Ltac2/dune

.PHONY: js
js: COQVM = no
js: coq_boot
	dune build --profile=release --display=quiet $(PKG_SET_WEB) lsp-server/jsoo/coq_lsp_worker.bc.cjs
	mkdir -p editor/code/out/ && cp -a lsp-server/jsoo/coq_lsp_worker.bc.cjs editor/code/out/coq_lsp_worker.bc.js

.PHONY: coq_boot

ifdef VENDORED_SETUP
coq_boot: vendor/coq/config/coq_config.ml
else
coq_boot:
endif

.PHONY: clean
clean:
	dune clean

# We first pin lablgtk3 as to avoid problems with parallel make
.PHONY: opam
opam:
	opam pin add coq-lsp . --kind=path -y
	opam install coq-lsp

# Create local opam switch
.PHONY: opam-switch
opam-switch:
	opam switch create . --empty

# Install opam deps
.PHONY: opam-deps
opam-deps:
	opam install ./coq-lsp.opam -y --deps-only --with-test

# Install opam deps
.PHONY: opam-dev-deps
opam-dev-deps:
	opam install -y $(DEV_DEPS)

# Initialise submodules
.PHONY: submodules-init
submodules-init:
	git submodule update --init

# Deinitialize submodules
.PHONY: submodules-deinit
submodules-deinit:
	git submodule deinit -f --all

# Update submodules from upstream
.PHONY: submodules-update
submodules-update:
	(cd vendor/coq && git checkout master && git pull upstream master)
	(cd vendor/coq-stdlib && git checkout master && git pull upstream master)
	(cd vendor/coq-waterproof && git checkout coq-master && git pull upstream coq-master)
# For now we update manually
# (cd vendor/coq-waterproof && git checkout coq-master && git pull upstream coq-master)

# Build the vscode extension
WASTUBS=$(addsuffix .wasm,dllcoqrun_stubs dllcoqperf_stubs dllbigstringaf_stubs dlllib_stubs)
WAFILES=$(addprefix lsp-server/wasm/,wacoq_worker.bc $(WASTUBS))
WASM_NODE=lsp-server/wasm/node_modules/
OUTDIR=editor/code/wasm-bin/
OUTDIR_NODE=editor/code/wasm-bin/node_modules

# Use to debug the extensions / wasm build
# ESBUILD_DEBUG=1

ifdef ESBUILD_DEBUG
NPM_TARGET=esbuild
NPM_OPTS=
else
NPM_TARGET=vscode:prepublish
NPM_OPTS=--silent
endif

.PHONY: wasm-bin
wasm-bin:
	@dune build $(WAFILES)
	@mkdir -p $(OUTDIR)
	@cp -af _build/default/lsp-server/wasm/wacoq_worker.bc $(OUTDIR)
	@cp -af _build/default/lsp-server/wasm/*.wasm $(OUTDIR)
	@cd lsp-server/wasm/ && npm i $(NPM_OPTS) && npm run $(NPM_OPTS) $(NPM_TARGET)
	@cp -af lsp-server/wasm/out/wacoq_worker.js $(OUTDIR)
	@mkdir -p $(OUTDIR_NODE)/ocaml-wasm/                        && cp -af $(WASM_NODE)/ocaml-wasm/bin/                        $(OUTDIR_NODE)/ocaml-wasm/
	@mkdir -p $(OUTDIR_NODE)/@ocaml-wasm/4.12--num/             && cp -af $(WASM_NODE)/@ocaml-wasm/4.12--num/bin/             $(OUTDIR_NODE)/@ocaml-wasm/4.12--num/
	@mkdir -p $(OUTDIR_NODE)/@ocaml-wasm/4.12--zarith/          && cp -af $(WASM_NODE)/@ocaml-wasm/4.12--zarith/bin/          $(OUTDIR_NODE)/@ocaml-wasm/4.12--zarith/
	@mkdir -p $(OUTDIR_NODE)/@ocaml-wasm/4.12--janestreet-base/ && cp -af $(WASM_NODE)/@ocaml-wasm/4.12--janestreet-base/bin/ $(OUTDIR_NODE)/@ocaml-wasm/4.12--janestreet-base/

.PHONY: extension
extension: wasm-bin
	@cd editor/code && npm i $(NPM_OPTS) && npm run $(NPM_OPTS) $(NPM_TARGET)

# Without the wasm
.PHONY: extension-code
extension-code:
	@cd editor/code && npm i $(NPM_OPTS) && npm run $(NPM_OPTS) $(NPM_TARGET)

# Run prettier
.PHONY: ts-fmt
ts-fmt:
	cd editor/code && npx prettier -w .

.PHONY: make-fmt
make-fmt: build fmt

# Helper for users that want a global opam install
.PHONY: opam-update-and-reinstall
opam-update-and-reinstall:
	git pull --recurse-submodules
	for pkg in rocq-runtime coq-core rocq-core coqide-server; do opam install -y vendor/coq/$$pkg.opam; done
	for pkg in rocq-stdlib coq-stdlib; do opam install -y vendor/coq-stdlib/$$pkg.opam; done
	opam install .

# Used in git clone
COQ_BRANCH:=v9.2
# Used in opam pin
COQ_CORE_VERSION:=9.2.dev
# Name of COQ_CORE_NAME is rocq-runtime after 8.20
COQ_CORE_NAME:=rocq-runtime
# Waterproof version
WP_VERSION:=none
# WP_VERSION=3.0.0+9.2

.PHONY: wp
wp:
ifdef VENDORED_SETUP
	dune build vendor/coq-waterproof/coq-waterproof.install
else ifeq ($(WP_VERSION),none)
	@echo "no waterproof install for this version"
else
	opam install coq-waterproof.$(WP_VERSION)
endif

ifdef VENDORED_SETUP
COQ_SRC_DIR=vendor/coq
PATCH_DIR=../../etc/
else
COQ_SRC_DIR=../coq_for_jscoq
PATCH_DIR=$(shell pwd)/etc
endif

.PHONY: patch-for-js
patch-for-js:
ifndef VENDORED_SETUP
	git clone --depth=1 https://github.com/coq/coq.git -b $(COQ_BRANCH) $(COQ_SRC_DIR)
endif
	cd $(COQ_SRC_DIR) && git apply $(PATCH_DIR)/0001-jscoq-Interrupt-and-timeout-Rocq-patch.patch
	cd $(COQ_SRC_DIR) && git apply $(PATCH_DIR)/0001-jscoq-lib-system.ml-de-unix-stat.patch
	cd $(COQ_SRC_DIR) && git apply $(PATCH_DIR)/0001-engine-trampoline.patch
ifndef VENDORED_SETUP
	opam pin add $(COQ_CORE_NAME).$(COQ_CORE_VERSION) -k path $(COQ_SRC_DIR) -y
endif

_LIBROOT=$(shell opam var lib)

ifdef VENDORED_SETUP
_CCROOT=_build/install/default/lib/$(COQ_CORE_NAME)
else
# We could use `opam var lib` as well here, as the idea to rely on
# coqc was to avoid having a VENDORED_SETUP variable, which we now
# have anyways.
_CCROOT=$(shell rocq c -where)/../$(COQ_CORE_NAME)
endif

# XXX: Fix the above as rocq c -where suffices
_ROCQLIB=$(shell dune exec -- rocq c -where)

# Super-hack
lsp-server/jsoo/coq-fs-core.js: COQVM = no
lsp-server/jsoo/coq-fs-core.js: coq_boot
	dune build --profile=release --display=quiet $(PKG_SET_WEB) etc/META.threads
	for i in $$(find $(_CCROOT)/plugins -name *.cma); do js_of_ocaml --dynlink $$i; done
	for i in $$(find _build/install/default/lib/coq-lsp/serlib -wholename */*.cma); do js_of_ocaml --dynlink $$i; done
	for i in $$(find $(_CCROOT)/../coq-waterproof -name *.cma); do js_of_ocaml --dynlink $$i; done
	js_of_ocaml build-fs -o lsp-server/jsoo/coq-fs-core.js \
	    $$(find $(_CCROOT)/                          \( -wholename '*/plugins/*/*.js' -or -wholename '*/META' \) -printf "%p:/static/lib/$(COQ_CORE_NAME)/%P ") \
	    $$(find _build/install/default/lib/coq-lsp/  \( -wholename '*/serlib/*/*.js'  -or -wholename '*/META' \) -printf "%p:/static/lib/coq-lsp/%P ") \
	    $$(find $(_CCROOT)/../coq-waterproof/  \( -wholename '*/plugin/*.js'  -or -wholename '*/META' \) -printf "%p:/static/lib/coq-waterproof/%P ") \
	    ./etc/META.threads:/static/lib/threads/META \
	    $$(find $(_LIBROOT) -wholename '*/str/META'                 -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/seq/META'                 -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/uri/META'                 -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/base/META'                -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/unix/META'                -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/zarith/META'              -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/yojson/META'              -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/findlib/META'             -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/dynlink/META'             -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/parsexp/META'             -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/sexplib/META'             -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/sexplib0/META'            -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/bigarray/META'            -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/cmdliner/META'            -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/ppx_hash/META'            -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/angstrom/META'            -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/stringext/META'           -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/ppx_compare/META'         -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/ppx_deriving/META'        -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/ppx_sexp_conv/META'       -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/memprof-limits/META'      -printf "%p:/static/lib/%P ") \
	    $$(find $(_LIBROOT) -wholename '*/ppx_deriving_yojson/META' -printf "%p:/static/lib/%P ")
	    # These libs are actually linked, so no cma is needed.
	    # $$(find $(_LIBROOT) -wholename '*/zarith/*.cma'         -printf "%p:/static/lib/%P " -or -wholename '*/zarith/META'         -printf "%p:/static/lib/%P ")

editor/code/wasm-bin/core-fs.zip:
	$(eval TMP := $(shell mktemp -d ./.wasm-build-fs-XXXXXX))
	dune exec -- ./etc/tools/build-fs/build_fs.exe $(_LIBROOT) $(_CCROOT) $(_ROCQLIB) $(TMP)/fs
	cd $(TMP)/fs &&	zip -rq core-fs.zip static
	cp -a $(TMP)/fs/core-fs.zip $@
	rm -rf $(TMP)

.PHONY: check-js-fs-sanity
check-js-fs-sanity: lsp-server/jsoo/coq-fs-core.js js
	cat _build/default/lsp-server/jsoo/coq-fs.js | grep '/static/'
	cat lsp-server/jsoo/coq-fs-core.js | grep '/static/'

# Serlib plugins require:
#   ppx_compare.runtime-lib
#   ppx_deriving.runtime
#   ppx_deriving_yojson.runtime
#   ppx_hash.runtime-lib
#   ppx_sexp_conv.runtime-lib
