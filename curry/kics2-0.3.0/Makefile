########################################################################
# Makefile for KiCS2 compiler suite
########################################################################

# Some information about this installation
# ----------------------------------------

# Is this a global installation (with restricted flexibility)(yes/no)?
GLOBALINSTALL=yes
# The major version number
MAJORVERSION    = 0
# The minor version number
MINORVERSION    = 3
# The revision version number
REVISIONVERSION = 0
# Complete version
export VERSION  = $(MAJORVERSION).$(MINORVERSION).$(REVISIONVERSION)
# The version date, extracted from the last git commit
COMPILERDATE =2013-07-25
# The installation date, set to the current date
INSTALLDATE    := $(shell date)
# The name of the Curry system, needed for installation of currytools
export CURRYSYSTEM = kics2

# Paths used in this this installation
# ------------------------------------

# root directory of the installation
export ROOT     = $(CURDIR)
# binary directory and executables
export BINDIR   = $(ROOT)/bin
# Directory where the libraries are located
export LIBDIR   = $(ROOT)/lib
# Directory where local executables are stored
export LOCALBIN = $(BINDIR)/.local
# Directory where local package installations are stored
export LOCALPKG = $(ROOT)/pkg
# The path to the package database
export PKGDB    = $(LOCALPKG)/kics2.conf.d

# Special files and binaries used in this this installation
# ---------------------------------------------------------

# The compiler binary
export COMP         = $(LOCALBIN)/kics2c
# The REPL binary, used for building the libraries
export REPL         = $(LOCALBIN)/kics2i
# The default options for the REPL, used for libraries and tools
export REPL_OPTS    = :set v2 :set -ghci
# The frontend binary
export CYMAKE       = $(BINDIR)/cymake
# The Haskell installation info
export INSTALLHS    = $(ROOT)/runtime/Installation.hs
# The Curry installation info
export INSTALLCURRY = $(ROOT)/src/Installation.curry
# The version information for the manual
MANUALVERSION       = $(ROOT)/docs/src/version.tex
# Logfiles for make
MAKELOG             = make.log

# Cabal packages on which this installation depends
# -------------------------------------------------

# Dependencies for the kics2 libraries
export LIBDEPS     = base directory network old-time parallel-tree-search \
                     process time
# Dependencies for the kics2 runtime system
export RUNTIMEDEPS = base containers ghc mtl parallel-tree-search tree-monad


# Fancy GHC and CABAL configuration
# ---------------------------------

# The path to the Glasgow Haskell Compiler and Cabal
export GHC     := "$(shell which ghc)"
export GHC-PKG := "$(shell dirname $(GHC))/ghc-pkg"
export CABAL   =  cabal

# Because of an API change in GHC 7.6, we need to distinguish
# GHC < 7.6 and GHC >= 7.6

# extract GHC version
GHC_MAJOR := $(shell $(GHC) --numeric-version | cut -d. -f1)
GHC_MINOR := $(shell $(GHC) --numeric-version | cut -d. -f2)
# Is the GHC version >= 7.6 ?
GHC_GEQ_76 = $(shell test $(GHC_MAJOR) -gt 7 -o \( $(GHC_MAJOR) -eq 7 \
              -a $(GHC_MINOR) -ge 6 \) ; echo $$?)
# package-db (>= 7.6) or package-conf (< 7.6)?
ifeq ($(GHC_GEQ_76),0)
GHC_PKG_OPT = package-db
else
GHC_PKG_OPT = package-conf
endif

# Libraries shipped with GHC
GHC_LIBS := $(shell $(GHC-PKG) list --global --simple-output --names-only)

# Standard options for compiling target programs with ghc
export GHC_OPTS       = -no-user-$(GHC_PKG_OPT) -$(GHC_PKG_OPT) $(PKGDB)
# Command to unregister a package
export GHC_UNREGISTER = $(GHC-PKG) unregister --$(GHC_PKG_OPT)=$(PKGDB)
#
export CABAL_INSTALL  = $(CABAL) install --with-compiler=$(GHC)       \
                        --with-hc-pkg=$(GHC-PKG) --prefix=$(LOCALPKG) \
                        --global --package-db=$(PKGDB) -O2

########################################################################
# The targets
########################################################################

# main (default) target: starts installation with logging
.PHONY: all
all:
	@rm -f ${MAKELOG}
	@echo "Make started at `date`" > ${MAKELOG}
	$(MAKE) install 2>&1 | tee -a ${MAKELOG}
	@echo "Make finished at `date`" >> ${MAKELOG}
	@echo "Make process logged in file ${MAKELOG}"

# install the complete system if the kics2 compiler is present
.PHONY: install
install: kernel alltools
	cd cpns       && $(MAKE) # Curry Port Name Server demon
	cd www        && $(MAKE) # scripts for dynamic web pages
	$(MAKE) manual
	# make everything accessible:
	chmod -R go+rX .

.PHONY: alltools
alltools:
	cd currytools && $(MAKE) # various tools
	cd tools      && $(MAKE) # various tools

# install the benchmark system
.PHONY: benchmarks
benchmarks:
	cd benchmarks && $(MAKE)

# uninstall globally installed cabal packages
.PHONY: uninstall
uninstall:
	rm -rf $(HOME)/.kics2rc $(HOME)/.kics2rc.bak $(HOME)/.kics2i_history
	@echo "Just remove this directory to finish uninstallation."

# install the kernel system (binaries and libraries)
.PHONY: kernel
kernel: $(PKGDB) $(INSTALLCURRY) frontend scripts
	cd src && $(MAKE)
ifeq ($(GLOBALINSTALL),yes)
	cd lib     && $(MAKE) unregister
	cd runtime && $(MAKE) unregister
	cd runtime && $(MAKE)
	# compile all libraries for a global installation
	cd lib     && $(MAKE) compilelibs
	cd lib     && $(MAKE) installlibs
	cd lib     && $(MAKE) acy
endif

# create package database
$(PKGDB):
	$(GHC-PKG) init $@
	$(CABAL) update
	$(CABAL_INSTALL) $(filter-out $(GHC_LIBS), $(RUNTIMEDEPS) $(LIBDEPS))

.PHONY: scripts
scripts: $(BINDIR)/cleancurry
	cd scripts && $(MAKE) ROOT=$(shell utils/pwd)

.PHONY: frontend
frontend:
	cd frontend && $(MAKE)

.PHONY: clean
clean: $(BINDIR)/cleancurry
	rm -f *.log
	rm -f ${INSTALLHS} ${INSTALLCURRY}
	cd cpns       && ${MAKE} clean
	@if [ -d lib/.curry/kics2 ] ; then \
	  cd lib/.curry/kics2 && rm -f *.hi *.o ; \
	fi
	@if [ -d lib/meta/.curry/kics2 ] ; then \
	  cd lib/meta/.curry/kics2 && rm -f *.hi *.o ; \
	fi
	cd runtime    && ${MAKE} clean
	cd src        && ${MAKE} clean
	cd currytools && ${MAKE} clean
	cd frontend   && ${MAKE} clean
	cd tools      && ${MAKE} clean
	cd utils      && ${MAKE} clean
	cd www        && ${MAKE} clean
	@if [ -d benchmarks ] ; then \
	  cd benchmarks && ${MAKE} clean ; \
	fi

# clean everything (including compiler binaries)
.PHONY: cleanall
cleanall: clean
	cd src   && $(MAKE) cleanall
	cd utils && $(MAKE) cleanall
	$(BINDIR)/cleancurry -r
	rm -rf ${LOCALBIN} $(CYMAKE) $(LOCALPKG)
	cd scripts && $(MAKE) clean
	rm $(BINDIR)/cleancurry

.PHONY: maintainer-clean
maintainer-clean: cleanall
	rm -rf $(BINDIR)

##############################################################################
# Building the compiler itself
##############################################################################

# generate module with basic installation information:
${INSTALLCURRY}: ${INSTALLHS}
	cp $< $@

${INSTALLHS}: Makefile utils/pwd utils/which
ifneq ($(shell test -x $(GHC) ; echo $$?), 0)
	$(error "No executable 'ghc' found. You may use 'make <target> GHC=<path>')
endif
	echo "-- This file is automatically generated, do not change it!" > $@
	echo "module Installation where" >> $@
	echo "" >> $@
	echo 'compilerName :: String' >> $@
	echo 'compilerName = "KiCS2 Curry -> Haskell Compiler"' >> $@
	echo "" >> $@
	echo 'installDir :: String' >> $@
	echo 'installDir = "$(shell utils/pwd)"' >> $@
	echo "" >> $@
	echo 'majorVersion :: Int' >> $@
	echo 'majorVersion = $(MAJORVERSION)' >> $@
	echo "" >> $@
	echo 'minorVersion :: Int' >> $@
	echo 'minorVersion = $(MINORVERSION)' >> $@
	echo "" >> $@
	echo 'revisionVersion :: Int' >> $@
	echo 'revisionVersion = $(REVISIONVERSION)' >> $@
	echo "" >> $@
	echo 'compilerDate :: String' >> $@
	echo 'compilerDate = "$(COMPILERDATE)"' >> $@
	echo "" >> $@
	echo 'installDate :: String' >> $@
	echo 'installDate = "$(INSTALLDATE)"' >> $@
	echo "" >> $@
	echo 'runtime :: String' >> $@
	echo 'runtime = "ghc"' >> $@
	echo "" >> $@
	echo 'runtimeMajor :: Int' >> $@
	echo 'runtimeMajor = $(GHC_MAJOR)' >> $@
	echo "" >> $@
	echo 'runtimeMinor :: Int' >> $@
	echo 'runtimeMinor = $(GHC_MINOR)' >> $@
	echo "" >> $@
	echo 'ghcExec :: String' >> $@
	echo 'ghcExec = "\"$(shell utils/which $(GHC))\""' >> $@
	echo "" >> $@
	echo 'ghcOptions :: String' >> $@
	echo 'ghcOptions = "$(GHC_OPTS)"' >> $@
	echo "" >> $@
	echo 'installGlobal :: Bool' >> $@
ifeq ($(GLOBALINSTALL),yes)
	echo 'installGlobal = True' >> $@
else
	echo 'installGlobal = False' >> $@
endif

$(BINDIR)/cleancurry: utils/cleancurry
	mkdir -p $(@D)
	cp $< $@

utils/%:
	cd utils && $(MAKE) $(@F)

##############################################################################
# Create documentation for system libraries:
##############################################################################

.PHONY: libdoc
libdoc:
	@if [ ! -r $(BINDIR)/currydoc ] ; then \
	  echo "Cannot create library documentation: currydoc not available!" ; exit 1 ; fi
	@rm -f ${MAKELOG}
	@echo "Make libdoc started at `date`" > ${MAKELOG}
	@cd lib && ${MAKE} doc 2>&1 | tee -a ../${MAKELOG}
	@echo "Make libdoc finished at `date`" >> ${MAKELOG}
	@echo "Make libdoc process logged in file ${MAKELOG}"

##############################################################################
# Create the KiCS2 manual
##############################################################################

.PHONY: manual
manual:
	# generate manual, if necessary:
	@if [ -d docs/src ] ; then \
	  ${MAKE} ${MANUALVERSION} && cd docs/src && ${MAKE} install ; \
	fi

${MANUALVERSION}: Makefile
	echo '\\newcommand{\\kicsversiondate}'         >  $@
	echo '{Version $(VERSION) of ${COMPILERDATE}}' >> $@

.PHONY: cleanmanual
cleanmanual:
	if [ -d docs/src ] ; then \
	  cd docs/src && $(MAKE) clean ; \
	fi

