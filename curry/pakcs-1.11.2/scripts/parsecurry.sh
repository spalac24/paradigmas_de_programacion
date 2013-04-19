#!/bin/sh

# This is the main shell script to compile a Curry program into
# the various intermediate representations.
# see usage messages below for possible parameters

# Define the main directory where PAKCS is installed:
PAKCSHOME=`echo PAKCSHOME must be defined here!`
export PAKCSHOME

# define PAKCSRC
PAKCSRC=$HOME/.pakcsrc
if [ ! -f "$PAKCSRC" ] ; then
  PAKCSRC=$PAKCSHOME/pakcsrc.default
fi

# executable of the parser:
MCCPARSER="$PAKCSHOME/bin/.local/cymake"
if [ ! -f "$MCCPARSER" ] ; then # is the parser locally installed?
  echo "ERROR: Incomplete PAKCS installation: $MCCPARSER missing!" >&2
  exit 1
fi

# Prepare flags for MCC parser:
MCCEXTENDED=
MCCOVERLAPWARN=
MCCOUTFILE=
MCCVERB=
MCCTARGET=
MCCSUFFIX=
# check flag "curryextensions" in pakcsrc:
for i in `sed -n '/^curryextensions=/p' < "$PAKCSRC"`
do 
  if [ xx`expr $i : '.*=\(.*\)'` = xxyes ] ; then
    MCCEXTENDED="--extended"
  fi
done
# check flag "warnoverlapping" in pakcsrc:
for i in `sed -n '/^warnoverlapping=/p' < "$PAKCSRC"`
do 
  if [ xx`expr $i : '.*=\(.*\)'` = xxno ] ; then
    MCCOVERLAPWARN="--no-overlap-warn"
  fi
done
# set additional libraries defined in pakcsrc:
for i in `sed -n '/^libraries=/p' < "$PAKCSRC"`
do 
  RCLIBPATH=`expr $i : '.*=\(.*\)'`
done

TARGET=           # no default case: target language must be specified!
if [ -z "$PAKCSLIBPATH" ] ; then
  PAKCSLIBPATH="$PAKCSHOME/lib:$PAKCSHOME/lib/meta"
fi
if [ -n "$RCLIBPATH" ] ; then
  PAKCSLIBPATH="$RCLIBPATH:$PAKCSLIBPATH"
fi

case $1 in
  --flat ) TARGET=fcy  ; shift ;;
  --acy  ) TARGET=acy  ; shift ;;
  --uacy ) TARGET=uacy ; shift ;;
  --html ) TARGET=html ; shift ;;
  --parse-only ) TARGET=cy ; FORMATMAIN=cy ; shift ;;
esac

if [ -z "$TARGET" ] ; then
  echo "ERROR: Target of the parser not specified!" >&2
  # remove all remaining args:
  set x x x x x x
  shift ; shift ; shift ; shift ; shift ; shift
fi

QUIET=
CONVERSION=no
FULLPATH=
TRANS=
OUTFILE=

while [ $# -gt 1 ] ; do
  case $1 in
    -q | --quiet | -quiet  ) QUIET=quiet ;;
    -o                     ) shift ; OUTFILE=$1 ;;
    --path | -path         ) shift ; CURRYPATH=$1 ; export CURRYPATH ;;
    --fullpath | -fullpath ) shift ; FULLPATH=$1 ;;
    --extended             ) MCCEXTENDED="--extended" ;;
    --no-overlap-warn      ) MCCOVERLAPWARN="--no-overlap-warn" ;;
    --fcypp | -fcypp       ) shift ; FCYPP="$FCYPP $1 " ;;
    --fpopt | --compact | --compactexport | --compactmain=* ) FCYPP="$FCYPP $1 " ;;
    -fpopt | -compact | -compactexport ) FCYPP="$FCYPP -$1 " ;;
    -compactmain:* ) FCYPP="$FCYPP --compactmain=`expr $1 : '-compactmain:\(.*\)'`" ;;
    * ) echo ERROR: Illegal option: "$@" >&2 ; exit 1 ;;
  esac
  shift
done

if [ $# != 1 ] ; then
  echo >&2
  echo "Usage: parsecurry <Target type> [<Options>] <progname>" >&2
  echo >&2
  echo "Parse Curry program stored in <progname>.(l)curry" >&2
  echo "<Target type> is an element of:"  >&2
  echo "--flat       : generate FlatCurry program in <progname>.fcy" >&2
  echo "--acy        : generate AbstractCurry program in <progname>.acy" >&2
  echo "--uacy       : generate untyped AbstractCurry program in <progname>.uacy" >&2
  echo "--html       : generate HTML representation of source program" >&2
  echo "--parse-only : generate source representation of source program" >&2
  echo >&2
  echo "<Options> is a sequence of:"  >&2
  echo "-q|--quiet       : work silently" >&2
  echo "-o <file>        : write output into <file> (for target type --html)" >&2
  echo "--path <p>       : specify additional search path <p> for loading modules" >&2
  echo " (default search path = .:$PAKCSLIBPATH)" >&2
  echo "--fullpath <p>   : specify complete search path <p> for loading modules" >&2
  echo >&2
  echo "--extended       : allow Curry extensions (e.g., functional patterns)">&2
  echo "--no-overlap-warn: show warnings for non-trivial overlapping rules" >&2
  echo "--fpopt          : apply function pattern optimization after parsing" >&2
  echo "--compact        : apply code compactification after parsing" >&2
  echo "--compactexport  : apply code compactification w.r.t. exports after parsing" >&2
  echo "--compactmain=f  : apply code compactification w.r.t. main function 'f' after parsing" >&2
  echo "--fcypp <c>      : apply command <c> to <progname> after parsing" >&2
  exit 1
fi
PROG=$1
PROGDIR=`dirname $PROG`
MODNAME=`basename $PROG`

# set up SEARCHPATH parameter for searching source file:
if [ -n "$FULLPATH" ] ; then
  SEARCHPATH="$PROGDIR:$FULLPATH"
  PAKCSLIBPATH=""
elif [ -z "$CURRYPATH" ] ; then
  SEARCHPATH="$PROGDIR:$PAKCSLIBPATH"
else
  SEARCHPATH="$PROGDIR:$CURRYPATH:$PAKCSLIBPATH"
fi

# compute actual directory of Curry program w.r.t. to SEARCHPATH
MAINDIR=
SP=$SEARCHPATH
while [ -n "$SP" ] ; do
  DIR=`expr "$SP" : '\([^:]*\):.*' \| "$SP"`
  SP=`expr "$SP" : '[^:]*:\(.*\)'`
  if [ -f "$DIR/$MODNAME.lcurry" ] ; then
    MAINDIR=$DIR
    SOURCESUFFIX=.lcurry
    break
  fi
  if [ -f "$DIR/$MODNAME.curry" ] ; then
    MAINDIR=$DIR
    SOURCESUFFIX=.curry
    break
  fi
done

if [ -z "$MAINDIR" ] ; then
   echo "ERROR: Program file '$PROG.curry' not found in path '$SEARCHPATH'!" >&2
   exit 1
fi

# compute import directory parameters for mcc front end:
if [ -n "$FULLPATH" ] ; then
  IMPORTPATH=$FULLPATH
elif [ -z "$CURRYPATH" ] ; then
  IMPORTPATH="$PAKCSLIBPATH"
else
  IMPORTPATH="$CURRYPATH:$PAKCSLIBPATH"
fi
if [ "$MAINDIR" != . ] ; then
  IMPORTPATH="$MAINDIR:$IMPORTPATH"
fi
IMPORTDIRS=
IP=$IMPORTPATH
while [ -n "$IP" ] ; do
  DIR=`expr "$IP" : '\([^:]*\):.*' \| "$IP"`
  IP=`expr "$IP" : '[^:]*:\(.*\)'`
  if [ -n "$IMPORTDIRS" ] ; then
    IMPORTDIRS="$IMPORTDIRS:-i$DIR"
  else
    IMPORTDIRS="-i$DIR"
  fi
done

if [ -z "$OUTFILE" ] ; then
  case $TARGET in
    cy   ) OUTFILE=$MAINDIR/${MODNAME}.cy ;;
    html ) OUTFILE=$MAINDIR/${MODNAME}_curry.html ;;
  esac
fi

# directory to store intermediate representations, like FlatCurry files,
# interfaces, etc:
CURRYDIR=$MAINDIR/.curry
# intermediate representation file in CURRYDIR that should be generated:
CURRYDIRTARGET=
case $TARGET in
  fcy  ) CURRYDIRTARGET=$MODNAME.fcy ;;
  acy  ) CURRYDIRTARGET=$MODNAME.acy ;;
  uacy ) CURRYDIRTARGET=$MODNAME.uacy ;;
esac

# delete old target file to avoid later problems...
case $TARGET in
  ast  ) rm -f $PROG.ast ;;
  cint ) rm -f $PROG.cint ;;
  acy  ) rm -f $PROG.acy ;;
  uacy ) rm -f $PROG.uacy ;;
  html ) rm -f $OUTFILE ;;
esac

case $TARGET in
  fcy  ) MCCTARGET=--flat ;;
  acy  ) MCCTARGET=--acy ;;
  uacy ) MCCTARGET=--uacy ;;
  cy   ) MCCTARGET=--parse-only ; MCCOUTFILE="-o:$OUTFILE" ;;
  html ) MCCTARGET=--html ; MCCOUTFILE="-o:$OUTFILE" ;
                            MCCSUFFIX=$SOURCESUFFIX ;;
esac

if [ -z "$QUIET" ] ; then
  echo "Parsing '$MODNAME$SOURCESUFFIX'..." >&2
else
  MCCVERB="--no-verb"
fi

if [ -n "$MCCTARGET" ] ; then
  # Use mcc front end:
  OLDIFS="$IFS"
  IFS=":" # set separator to ':' for correct passing of IMPORTDIRS as arguments
  "$MCCPARSER" $MCCEXTENDED $MCCOVERLAPWARN $MCCOUTFILE $MCCVERB $MCCTARGET $IMPORTDIRS $MODNAME$MCCSUFFIX
  EXITCODE=$?
  IFS="$OLDIFS"
  if [ $EXITCODE -gt 0 ] ; then
    rm -f $PROG.$TARGET # just to be sure that there is no buggy file
    exit $EXITCODE
  fi
fi

# apply optional transformations on FlatCurry representation:
if [ $TARGET = fcy ] ; then
  # find options in Curry source file:
  OPTIONLINE=`grep '^{-#.*PAKCS_OPTION_FCYPP' "$MAINDIR/$MODNAME$SOURCESUFFIX" | head -1`
  OPTIONS=`expr "$OPTIONLINE" : '{-#.*PAKCS_OPTION_FCYPP*\(.*\)#-}.*'`
  # add source file options to current preprocessing options:
  if [ -n "$OPTIONS" ] ; then
    FCYPP="$FCYPP $OPTIONS"
  fi

  if [ -z "$QUIET" -a -n "$FCYPP" ] ; then
    echo "Executing FlatCurry preprocessing options: $FCYPP"
  fi
  for T in $FCYPP ; do
    case $T in
      --fpopt         ) TCMD=$PAKCSHOME/tools/optimize/NonStrictOpt.state ;;
      --compact       ) TCMD=$PAKCSHOME/tools/optimize/CompactFlat.state ;;
      --compactexport ) TCMD="$PAKCSHOME/tools/optimize/CompactFlat.state -export" ;;
      --compactmain=* ) TCMD="$PAKCSHOME/tools/optimize/CompactFlat.state -main `expr $T : '--compactmain=\(.*\)'`" ;;
      *              ) TCMD=$T ;;
    esac
    $TCMD $PROG
    EXITCODE=$?
    if [ $EXITCODE -gt 0 ] ; then
       # just to be sure that there is no buggy file:
      rm -f "$PROGDIR/$MODNAME.$TARGET" "$PROG.$TARGET"
      exit $EXITCODE
    fi
  done
fi
