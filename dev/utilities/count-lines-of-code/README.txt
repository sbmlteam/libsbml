Invokes the cloc program to count lines of code in the libsbml directory.

WINDOWS
=======

Two batch files:

src-no-bindings.bat: produces no-bindings.csv which counts the loc excluding the bindings

bindings-only.bat: produces bindings.csv which counts loc in the bindings directory only

Mac / Unix / CYGWIN
===================
Analog to the windows scripts, these scripts will produce the same files. The only difference
is that they search for cloc in the environment variable CLOC, or in the path. 

src-no-bindings.sh: produces no-bindings.csv in this folder 

bindings-only.sh  : produces bindings.csv in this folder 
