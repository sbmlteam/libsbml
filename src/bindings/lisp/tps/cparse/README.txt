cparse is a Common Lisp package for parsing the subset of ANSI C that is
likely to be in header files.  It understands variable and function
declarations arrays, structures, enums, and constant expressions.  It
also includes cmu-alien.lisp, which uses cparse to construct foreign
function definitions for CMUCL's alien facility.

CPARSE

cparse goes to some effort to simulate C integer arithmetic for a
given host and compiler implementation; while it implements floating
point arithmetic too, it just uses Lisp floats for that.

cparse:cparse-stream is the main entry point for cparse; it parses a
stream of raw C i.e., C that has already been run through the C
preprocessor.  It doesn't grok any preprocessor directives but it does
understand the line directives output by cpp for error reporting. It
returns a big list of lists of type and variable names, and a scope
structure that contains everything that was defined in the top-level
lexical environment.

cparse-stream also supports another interface: it takes a :stmt-fun
argument, a function that gets called for every statement parsed by
cparse.  cmu-alien uses this interface.


CMU-ALIEN

cmu-alien is an example of using cparse.  Its entry point,
make-alien-defs, produces a form that uses CMUCL's alien facility to
make a definition for everything found in the header files.  This form
could, for example, be returned from a macroexpansion or dumped into a
file for manual editing.


INSTALLATION

I use defsystem from clocc (http://clocc.sourceforge.net).  
(mk:oos "cparse" :compile) does the trick.

It should be easy to build and install by hand if you don't want to
use defsystem.

I've only tested cparse and cmu-alien in CMU Common Lisp, but the
cparse parts (everything other than cmu-alien.lisp) should be portable
to other Lisps.


LIMITATIONS AND FUTURE WORK

Plenty, I'm sure.

Bitfields aren't implemented.

gcc's __attribute__ isn't supported.  I use the
:extra-cpp-lines argument to make-alien-defs to work around the
__attribute__ problem.  Sort of.

The C arithmetic in ctype.lisp has high aspirations, but probably has
many bugs in its handling of overflow.

CMUCL's alien facility doesn't understand stdargs yet, so functions
that declare stdargs are silently ignored.

Interfaces to Objective C and C++ would be nice.  The former is a lot
easier than the latter.


REPORTING BUGS

Send bugs to moore@bricoworks.com.  It's most helpful if you can send
C files that have been preprocessed.
