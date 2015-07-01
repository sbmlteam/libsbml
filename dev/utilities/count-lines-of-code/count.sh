#!/usr/bin/env bash
# 
# This script counts lines of code.
# 

THIS_DIR=$(cd `dirname $0` && pwd)

CLOC=${CLOC:="${THIS_DIR}/bin/cloc-1.62"}

command -v $CLOC >/dev/null 2>&1 || { CLOC=cloc; }
command -v $CLOC >/dev/null 2>&1 || { echo >&2 "cloc cannot be found, please update the CLOC variable or add it to the path."; }

CLOC_DEFAULT_ARGS=--autoconf
LIBSBML_DIR=$THIS_DIR/../../../..
LIBSBML_DIR=$(cd `dirname $LIBSBML_DIR` && pwd)
LIBSBML_DIR=$LIBSBML_DIR
OUT_DIR=$THIS_DIR

# make sure it also works for CYGWIN
UNAME=$(uname -s)
UNAME=${UNAME:0:6}
if [ "$UNAME" == "CYGWIN" ];then
  LIBSBML_DIR=$(cygpath -w $LIBSBML_DIR)
  OUT_DIR=$(cygpath -w $OUT_DIR)  
fi

EXCLUDE_DIRS=doc-converter

EXCLUDE_EXT=xsl,xsd,html,css

# do not count configure
SET REMOVE_MATCHES=configure$


# record files with no extension as Bourne Shell
FORCE_NO="Bourne Shell"

# additional things to do
# want to group particular things to be counted together
# note have to use the cloc group and will rename later
FORCE_BASH="Bourne Shell",bash
FORCE_BAT="Bourne Shell",bat


# for some reason it is not picking up the R examples
# but does if you tell it they have lowercase r (they dont but it works)
FORCE_R="R",r


OUTPUT_FILE=$OUT_DIR/results_cloc.xml

# run the count
$CLOC $CLOC_DEFAULT_ARGS $LIBSBML_DIR --exclude-dir=$EXCLUDE_DIRS$ --exclude-ext=$EXCLUDE_EXT$ --not-match-f=$REMOVE_MATCHES$ --read-lang-def=$SWIG_DEF$ --force-lang=$FORCE_BASH$ --force-lang=$FORCE_BAT$ --force-lang=$FORCE_R$ --lang-no-ext=$FORCE_NO$ --report-file=$OUTPUT_FILE$ --xml
