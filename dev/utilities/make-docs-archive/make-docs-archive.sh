#!/bin/sh

files="docs/*.txt docs/*.html docs/README* docs/.graphics docs/formatted"

if ! test -d "`/bin/pwd`/docs"; then
    echo "Current dir does not appear to be the root of the libSBML src tree."
    echo "Make sure to run this from the libSBML src root directory."
    echo "Stopping."
    exit
else
    version="`cat VERSION.txt`"
    basename="libsbml-$version-docs"
    if (command -v pv >/dev/null 2>&1) ; then
        size="`du -sk $files | tail -1 | cut -f1`"
        # Not sure why 1000 and not 1024, but the number works out correctly.
        bytes=$[($size * 1000)]
        echo "Creating tar archive ..."
        env COPYFILE_DISABLE=true tar cf - $files | pv -s $bytes | gzip -9 > $basename.tar.gz 2>&1
        echo "Creating zip archive ..."
        zip -rq9 $basename.zip $files
    else
        echo "Creating tar archive ..."
        tar czf $basename.tgz $files
        echo "Creating zip archive ..."
        zip -rq9 $basename.zip $files
    fi
    echo "Done."
fi
