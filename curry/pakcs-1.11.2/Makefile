#****************************************************************************
# PAKCS: The Portland Aachen Kiel Curry System
# ============================================
#
# An Prolog-based implementation of the functional logic language Curry
# developed by
#
# Sergio Antoy, Bernd Brassel, Martin Engelke, Michael Hanus, Klaus Hoeppner,
# Johannes Koj, Philipp Niederau, Ramin Sadre, Frank Steiner
#
# (contact: mh@informatik.uni-kiel.de)
#****************************************************************************

# The major version numbers:
MAJORVERSION=1
# The minor version number:
MINORVERSION=11
# The revision version number:
REVISIONVERSION=2
# The build version number:
BUILDVERSION=1
# Complete version:
VERSION=$(MAJORVERSION).$(MINORVERSION).$(REVISIONVERSION)
# The version date:
COMPILERDATE =2013-03-21

# Logfile for make:
MAKELOG=make.log
# The name of the Curry System
export CURRYSYSTEM=pakcs
# the root directory
export ROOT=$(CURDIR)
# binary directory and executables
export BINDIR=$(ROOT)/bin
# Directory where local executables are stored:
export LOCALBIN=$(BINDIR)/.local
# The version information file for Curry2Prolog:
C2PVERSION=$(ROOT)/curry2prolog/pakcsversion.pl
# The version information file for the manual:
MANUALVERSION=$(ROOT)/docs/src/version.tex

#
# Install all components of PAKCS
#
.PHONY: all
all: config
	@rm -f $(MAKELOG)
	@echo "Make started at `date`" > $(MAKELOG)
	$(MAKE) install 2>&1 | tee -a $(MAKELOG)
	@echo "Make finished at `date`" >> $(MAKELOG)
	@echo "Make process logged in file $(MAKELOG)"

#
# Install all components of PAKCS
#
.PHONY: install
install: installscripts
	$(MAKE) frontend
	# pre-compile all libraries:
	@cd lib && $(MAKE) fcy
	# install the Curry2Prolog compiler as a saved system:
	@if [ -r bin/sicstusprolog -o -r bin/swiprolog ] ; \
	 then $(MAKE) $(C2PVERSION) && cd curry2prolog && $(MAKE) ; \
	 else rm -f bin/pakcs ; fi
	# compile all libraries:
	@cd lib && $(MAKE) acy
	# prepare for separate compilation by compiling all librariers to Prolog code:
	@if [ -r bin/pakcs ] ; then cd lib && $(MAKE) pl ; fi
	# compile the Curry Port Name Server demon:
	@if [ -r bin/pakcs ] ; then cd cpns && $(MAKE) ; fi
	# compile the event handler demon for dynamic web pages:
	@if [ -r bin/pakcs ] ; then cd www && $(MAKE) ; fi
	$(MAKE) tools
	$(MAKE) docs
	chmod -R go+rX .

# Configure installation w.r.t. variables in pakcsinitrc:
.PHONY: config
config: installscripts
	@scripts/configure-pakcs

# install the scripts of PAKCS in the bin directory:
.PHONY: installscripts
installscripts:
	cd scripts && $(MAKE) all

# remove the scripts of PAKCS in the bin directory:
.PHONY: cleanscripts
cleanscripts:
	cd scripts && $(MAKE) clean

# install front end (if sources are present):
# install front end:
.PHONY: frontend
frontend:
	@if [ -d frontend ] ; then cd frontend && $(MAKE) ; fi

# compile the tools:
.PHONY: tools
tools:
	@if [ -r bin/pakcs ] ; then cd currytools && $(MAKE) ; fi
	@if [ -r bin/pakcs ] ; then cd tools && $(MAKE) ; fi

# compile documentation, if necessary:
.PHONY: docs
docs:
	@if [ -d docs/src ] ; \
	 then $(MAKE) $(MANUALVERSION) && cd docs/src && $(MAKE) install ; fi

# install required cabal packages required by the front end
# (only necessary if the front end is installed for the first time)
.PHONY: installhaskell
installhaskell:
	cabal update
	cabal install mtl

# Create file with version information for Curry2Prolog:
$(C2PVERSION): Makefile
	echo ':- module(pakcsversion,[compilerVersion/1, compilerMajorVersion/1, compilerMinorVersion/1, compilerRevisionVersion/1, buildVersion/1, buildDate/1]).' > $@
	echo "compilerVersion('PAKCS$(MAJORVERSION).$(MINORVERSION)')." >> $@
	echo 'compilerMajorVersion($(MAJORVERSION)).' >> $@
	echo 'compilerMinorVersion($(MINORVERSION)).' >> $@
	echo 'compilerRevisionVersion($(REVISIONVERSION)).' >> $@
	echo 'buildVersion($(BUILDVERSION)).' >> $@
	echo "buildDate('$(COMPILERDATE)')." >> $@

# Create file with version information for the manual:
$(MANUALVERSION): Makefile
	echo '\\newcommand{\\pakcsversion}{$(VERSION)}' > $@
	echo '\\newcommand{\\pakcsversiondate}{Version of $(COMPILERDATE)}' >> $@

#
# Create documentation for system libraries:
#
.PHONY: libdoc
libdoc:
	@if [ ! -r bin/currydoc ] ; then \
	  echo "Cannot create library documentation: currydoc not available!" ; exit 1 ; fi
	@rm -f $(MAKELOG)
	@echo "Make libdoc started at `date`" > $(MAKELOG)
	@cd lib && $(MAKE) doc 2>&1 | tee -a ../$(MAKELOG)
	@echo "Make libdoc finished at `date`" >> $(MAKELOG)
	@echo "Make libdoc process logged in file $(MAKELOG)"

# Clean the system files, i.e., remove the installed PAKCS components
.PHONY: clean
clean:
	rm -f $(MAKELOG)
	$(MAKE) cleantools
	cd lib && $(MAKE) clean
	cd examples && ../bin/cleancurry -r
	if [ -d docs/src ] ; then cd docs/src && $(MAKE) clean ; fi
	cd bin && rm -f sicstusprolog swiprolog
	cd scripts && $(MAKE) clean

# Clean the generated PAKCS tools
.PHONY: cleantools
cleantools:
	cd curry2prolog && $(MAKE) clean
	cd currytools && $(MAKE) clean
	cd tools && $(MAKE) clean
	cd cpns && $(MAKE) clean
	cd www && $(MAKE) clean
	cd bin && rm -f pakcs
	rm -rf $(LOCALBIN)

#################################################################################
