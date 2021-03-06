##############################################################################
# Installation of frontend
##############################################################################

BASE_VERSION     := $(shell sed -n 's/^Version *: *\(.*\)$$/\1/p' curry-base/curry-base.cabal)
FRONTEND_VERSION := $(shell sed -n 's/^Version *: *\(.*\)$$/\1/p' curry-frontend/curry-frontend.cabal)

# install the sources of the front end from its repository

INSTALLED      := $(shell test -n $(CYMAKE) -a -x $(CYMAKE) && echo $$?)
ifeq ($(INSTALLED), 0)
CYMAKE_VERSION := $(shell $(CYMAKE) --numeric-version)
endif

$(FRONTEND):
	mkdir -p $(@D)
ifneq ($(INSTALLED), 0)
	@echo "No cymake found"
	$(MAKE) install
else
ifeq ($(CYMAKE_VERSION), $(FRONTEND_VERSION))
	@echo "Copying cymake"
	cp $(CYMAKE) $(@D)
else
	@echo "cymake versions do not match (found: $(CYMAKE_VERSION), required: $(FRONTEND_VERSION))"
	$(MAKE) install
endif
endif

.PHONY: install
install: unregister
	cd curry-base     && $(CABAL_INSTALL)
	cd curry-frontend && $(CABAL_INSTALL) --bindir=$(shell dirname $(FRONTEND))

# unregister previous version to avoid cabal install failures
.PHONY: unregister
unregister:
	-$(GHC-PKG) unregister curry-frontend-$(FRONTEND_VERSION)
	-$(GHC-PKG) unregister curry-base-$(BASE_VERSION)

.PHONY: update
update:
	cd curry-base     && git pull
	cd curry-frontend && git pull

.PHONY: clean
clean:
	cd curry-base     && $(CABAL) clean
	cd curry-frontend && $(CABAL) clean
