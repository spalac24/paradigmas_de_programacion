# generate various tools for handling Curry programs:

# Required:
# - installed curry2prolog system

.PHONY: all
all:    curry2xml
	@cd optimize  ; ${MAKE}
	@cd xmldata   ; ${MAKE}
	@cd Peval     ; ${MAKE}
	@cd coosy     ; ${MAKE}
	@cd erd2curry ; ${MAKE}
	@cd ui        ; ${MAKE}


# generate saved state for curry2xml program:
curry2xml: curry2xml.curry ../lib/meta/FlatXML.curry \
	         ../lib/meta/Flat.curry \
	         ../lib/meta/Flat2Fcy.curry ../lib/XML.curry
	../bin/pakcs -set -error -s curry2xml

.PHONY: clean
clean: 
	../bin/cleancurry
	rm -f curry2xml
	cd Peval     ; ${MAKE} clean
	cd coosy     ; ${MAKE} clean
	cd optimize  ; ${MAKE} clean
	cd xmldata   ; ${MAKE} clean
	cd erd2curry ; ${MAKE} clean
	cd ui        ; ${MAKE} clean
