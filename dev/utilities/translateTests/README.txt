-----------------------------------------------------------------------------
Synopsis:      Test code translator
First Author:  Akiya Jouraku (Keio University)
Other Authors: Frank Bergmann (Univ. of Washington), Michael Hucka (Caltech)
-----------------------------------------------------------------------------

(Originally titled "ctest_converter.pl".)

This script translate the test code from different src/*/test directories
into versions for Python, Ruby, Java, and C# (and maybe other languages in
the future).  It is not a full parser, and there are many things it doesn't
handle.  But it gets 80% of the way there.


