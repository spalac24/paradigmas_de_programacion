# Makefile for main module "Curry2StrictPl":

PAKCSHOME=/home/pakcs/pakcs
PAKCSLIB=${PAKCSHOME}/lib

.PHONY: all
all: Curry2StrictPl.state SourceProgGUI.state Oracle.fcy

Curry2StrictPl.state:Curry2StrictPl.curry \
	  Prolog.curry \
	  ReadFlat.curry \
	  ${PAKCSLIB}/Integer.curry \
	  ${PAKCSLIB}/ReadShowTerm.curry \
	  ${PAKCSLIB}/meta/CompactFlatCurry.curry \
	  ${PAKCSLIB}/PropertyFile.curry \
	  ${PAKCSLIB}/Distribution.curry \
	  ${PAKCSLIB}/FileGoodies.curry \
	  ${PAKCSLIB}/meta/FlatCurry.curry \
	  ${PAKCSLIB}/SetRBT.curry \
	  ${PAKCSLIB}/Sort.curry \
	  ${PAKCSLIB}/TableRBT.curry \
	  ${PAKCSLIB}/Time.curry \
	  ${PAKCSLIB}/XML.curry \
	  ${PAKCSLIB}/Directory.curry \
	  ${PAKCSLIB}/IOExts.curry \
	  ${PAKCSLIB}/Maybe.curry \
	  ${PAKCSLIB}/RedBlackTree.curry \
	  ${PAKCSLIB}/List.curry \
	  ${PAKCSLIB}/Read.curry \
	  ${PAKCSLIB}/IO.curry \
	  ${PAKCSLIB}/System.curry \
	  ${PAKCSLIB}/Char.curry
	# create saved state for top-level function "main":
	${PAKCSHOME}/bin/pakcs -s Curry2StrictPl
	rm -f debug ; ln -s Curry2StrictPl.state debug

SourceProgGUI.state:SourceProgGUI.curry ${PAKCSLIB}/GUI.curry
	${PAKCSHOME}/bin/pakcs -s SourceProgGUI

Oracle.fcy: Oracle.curry \
	  ${PAKCSLIB}/IOExts.curry \
	  ${PAKCSLIB}/Unsafe.curry \
	  ${PAKCSLIB}/IO.curry \
	  ${PAKCSLIB}/System.curry \
	  ${PAKCSLIB}/Char.curry
	# compile Oracle to create current .fcy file
	${PAKCSHOME}/bin/pakcs -c Oracle

.PHONY: clean
clean:
	${PAKCSHOME}/bin/cleancurry
	rm -f debug
	rm -f ?*Oracle.fcy ?*Oracle.pl ?*Oracle.po *s.pl *.steps


stricths: stricths.curry PrettyStrict.curry 
	kics -executable -o stricths stricths.curry

transform.bin: Transform.curry ReadFlat.curry Wrapper.curry
	kics -executable -o transform.bin Transform.curry
