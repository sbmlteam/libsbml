Author: Martin Ginkel <martin.ginkel@epost.de>
Organization: Max-Planck-Institute Magdeburg Germany
Copyright: Max-Planck-Institute Magdeburg 2004


Lisp Interface to Libsbml
=========================

Preconditions:
--------------

The following external Lisp-packages are used:
- Asdf, Another System Definition Facility, 
  http://www.cliki.net/asdf,
  a build environment and package-manager for CL

  
  If you already have it sitewide, you can use your own.
  However it is distributed with the package.

- Uffi, Universal Foreign Function Interface (for Lisp)
  http://uffi.b9.com/
  Allows to define the interface to the C-library in one
  syntax and translates this to the different FFI-schemes of
  different Lisp implementations. You can also download a newer
  version and use it with libsbml

- Cparse, a C-Syntax parser for Common Lisp
  http://common-lisp.net/project/cparse
  Reads a C-header and allows to translate it into a FFI-description.
  For libsbml I wrote an extension (cparse/uffi.lisp) which
  translates only relevant parts (enums and functions) of
  the libsbml interface to UFFI. This part also cares for the
  appropriate generation of argument and returnvalue-conversion.
  Especially this part is not generally usable but relies on the
  calling conventions of libsbml. Therefore replacement with
  the vanilla version is not possible.

Platforms:
----------

The Lisp part of the package (hopes to) stick to Ansi Common Lisp. 
But it has to use UFFI for the foreign funtions.  It has been
successfully tested on Allegro Common Lisp and Carnegie Mellon 
Common Lisp. It is expected to work also on other platforms 
supported by UFFI. 
It does not work on CLISP yet, but hopefully UFFI will do that soon.

Main concepts of mapping in Lisp:
---------------------------------

The main idea is to provide an direct interface to the C-objects with
necessary argument conversion for primitive types.  The package does not
attempt to build a Lisp-object-world around the C-objects. It also relies on
proper calls to xxx_free, to release memory on the C-heap. This means
practically, after working with some SBMLDocument (read from a File or
created for writing to a file ...), SBMLDocument_free has to be called 
(The latter call can be put into a finalizer function called by your GC).

The SBML objects are handled within lisp as void pointers. This may map
(depending on the implementation) to integer numbers or #<alien objects>.

Strings are freed automatically (when necessary) and converted into
Lisp-strings. There where some problems with memory leaks, depending on the
semantics of the specific FFI-implementation, therefore memtest.lisp provides
some test methods. (For ACL and CMUCL this works cleanly)
Boolean arguments or return-values are also converted from C "0" or ">0" 
to LISP "nil" or "t". 

Names in C with upper and lower case are translated into 
'lower case with dashes for case-change and underbar' in Lisp
(depending on whether the case matters in your Lisp).
This maps SpeciesReference_create -> species-reference-create or
SBMLDocument_create -> sbmldocument-create. The mapping can be found in 
libsbmlc.lisp. All functions and constants of the original C-source reside
in the libsbmlc package.

Enumeration types are integer numbers in Lisp with appropriate constants
defined for all values:

(uffi:def-foreign-type sbmltype-code-t :INT) 
(cl:defconstant +sbml-compartment+ 0) 
(cl:defconstant +sbml-document+ 1) 
(cl:defconstant +sbml-event+ 2) 
...

Since the model for programming with libsbml is object-oriented,
no direct access to the struct-types is provided nor necessary.
Everything can and should be done with the accessors.


Files:
------

tps/asdf/*	Asdf resides there
tps/uffi/*	Uffi package for foreign functions
tps/cparse/*	Cparse package for translating C-headers into 
		FFI Definitions
tps/cparse/uffi.lisp
		Translator implementation that is used for libsbml
		This inserts argument and return value conversion and
		Memory management for strings.
./*		Wrapping of libsbml:
libsbml.asd
		System definition: names all files and packages with 
		dependencies. also controls the translation from 
		headers to Lisp.
libsbml-config.lisp
		Logical pathname translations necessary for the
		build-process.
libsbml-config-run.lisp
		Logical pathname translations used in the installed version
		of the wrapper. Contains the installation paths of
		the different lisp packages and of libsbml.so
build.lisp
		Loader for the system. This contains all the commands to
		start compilation of the package in lisp. This is used
		in Makefile.
sbmllisp.h
                C-Header which includes all interesting SBML-headers
                and makes some defines to avoid descending into 
		system definitions.
sbmllisp-pre.h
		C-Header preprocessed: cparse has no preprocessor, so includes
		and defines have to be resolved by gcc -E or something 
		similar.
package.lisp
		definition of the package for the lisp code around the
		foreign functions.
boolean-functions.lisp
		Declaration, which functions or function arguments must be
		boolean-converted. This is an input to the translation 
		process.
libsbmlc.lisp 
		File automatically generated from C. It defines a separate
		package with all the functions and enums defined in the
		header-files.
utilities.lisp
		Utility functions connected to the lisp implementation
sbml.lisp
		The class hierarchy of SBML resembled in Lisp, objects
		of these classes are generated in the sbml-reader
sbml-reader.lisp
		Example implementation of a reader for SBML files
		based on libsbmlc. This reads all element types and
		calls methods to define result types. All math is 
		translated to Lisp prefix notation. The expample 
		implementation results in a list of 
		
Maintainance and Development:
-----------------------------

The interesting point in the mapping is the maintainance of libsbmlc.lisp.
This has been tested on Linux only. Since the lisp file as such is platform
independent, it can be transferred from one platform to others.

The translation is based on sbmllisp.h, this header file includes the main 
headers of libsbml. This file is preprocessed in Makefile with $(CPP) to
produce a expanded header sbmllisp-pre.h. This file is in turn
processed by cparse, extracting all the defined functions and enumeration
types for Lisp. The algorithm for this mapping is contained in 
cparse/uffi.lisp.

Problematic points are, that the #include of system headers leads to
confusion, since there architecture- and compiler-specific primitive C-types
may occur, which are not specified in Standard-C. To avoid this,
some defines in the top of the file pretend, that these system files have
already been included. Cparse will otherwise barf on the unknown primitive 
types.

Only if the interface of libsbml changes, it is necessary do this 
recompilation. Therefore the libsbmlc.lisp should be distributed with the 
package.

Usage:
------

The installed package is loaded by asdf. If you normally use asdf, it is
a good Idea to create a symbolic link from ${libdir}/lisp/libsbml/libsbml.asd 
into your central-repository. (Replace ${libdir} with your platform-dependent
library directory).
Then just run:

(asdf:operate 'asdf:load-op :libsbml)

You can use the symbols in the libsbml and libsbmlc packages then.

If you are not using asdf, you should run:

(load  "${libdir}/lisp/libsbml/libsbml.asd")
(asdf:operate 'asdf:load-op :libsbml)


Example-calls:
--------------

;; Use of the package
(defpackage :mypack (:use :libsbml :libsbmlc :cl))
(in-package :mypack)
;; Using the example reader
;; Create the reader, this runs (libsbmlc:read-sbml <filename>)
(setf r (make-instance 'sbml-reader :filename
"/home/ginkel/Projects/Promot/promot/src/sbml/models/Metabolism2000Teusink.xml"))
;; translation into Lisp
(setf model (read-model r))
;; Looking into the model
(inspect model)


;; Using the plain C functions
(setf d (read-sbml
"/home/ginkel/Projects/Promot/promot/src/sbml/models/Metabolism2000Teusink.xml"))
;=> #<Alien (* T) at #x08E89CF8>
(setf m (sbmldocument-get-model d))
;=> #<Alien (* T) at #x08ECA7A8>
(model-get-id m)
;=> nil
(model-get-name m)
;=> "Teusink"










