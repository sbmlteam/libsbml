
                            l i b S B M L

                            Ben Bornstein

              The Systems Biology Workbench Development Group
                JST ERATO Kitano Symbiotic Systems Project
                 Control and Dynamical Systems, MC 107-81
                    California Institute of Technology
                         Pasadena, CA, 91125, USA

                         http://www.sbw-sbml.org/
                      mailto:sysbio-team@caltech.edu


--------------
0. Quick Start
--------------

At the Unix or Cygwin (under Windows) command prompt, untar the
distribution, cd into it (e.g., libsbml-1.0/), and type:


  % ./configure
  % make
  % make install


To compile programs that use libsbml (e.g., see Section 4.1) with GCC:

  % gcc -o myapp.c myapp.c -lsbml




---------------
1. Introduction
---------------

This README describes libsbml, a C API for reading, writing and
manipulating the Systems Biology Markup Language (SBML).  Currently,
the library supports SBML level 1, versions 1 and 2.  Support for SBML
level 2 is under active development.

Since the library provides a C API, familiarity with the C programming
language is assumed.  Some parts of the library were written in C++,
however, experience with C++ is not required; its use is "hidden"
behind C functions.

Libsbml is entirely open-source and all specifications and source code
are freely and publicly available.  For more information about SBML,
please see the references section or visit http://www.sbw-sbml.org/.




---------------
2. Installation
---------------

Libsbml depends on Apache's Xerces-C++ XML library for low-level XML
tokenizing and Unicode support.  Xerces is supported on both Unix
(Linux) and Windows (MacOS X?).  Many popular Linux systems provide
the Xerces library either as part of their standard distribution or as
an optional RPM or Debian package.  For more information, see:


  http://xml.apache.org/xerces-c/


A good way to determine whether or not Xerces-C is installed is to run
the build script (see below); it will halt if it cannot find the
Xerces-C library.

Libsbml is designed to be extremely portable.  It is written in 100%
pure ANSI-C and the build system uses the GNU Autotools (Autoconf,
Automake and Libtool).  In most cases, building should be as easy
unpacking the sources and running:


  % ./configure
  % make
  % make check    (optional)
  % make install


However, if Xerces-C is installed in a non-standard place (e.g., your
home directory), configure will not be able to detect it.  In this
case, configure needs to be told explicitly where to find the library.
Set the CPPFLAGS and LDFLAGS environment variables to include the
custom directories containing Xerces-C header and library files.  For
example:


  % export CPPFLAGS=-I/home/bornstei/software/xerces-c/2.2.0/include
  % export LDFLAGS=-L/home/bornstei/software/xerces-c/2.2.0/lib
  % ./configure
  % # etc.


'make install' copies header files to /usr/local/include/sbml and
(shared and static) library files to /usr/local/lib.  To specify a
different install location use:


  % ./configure --prefix=/my/favorite/path


'make check' is optional and will build and run an extensive suite of
unit tests to verify all facets of the library.  These tests are meant
primarily for developers of libsbml and running them is not required
for the library to function properly.

To run the unit tests a second library is required, libcheck.  Check
is a very lightweight C unit test framework based on the xUnit
framework popularized by Kent Beck and eXtrememe Programming (all of
libsbml was written using the test first approach).  Check is quite
small, once installed, it's only two files: libcheck.a and check.h.
To download Check, visit:


  http://check.sf.net/


Debian users can find Check as a standard add-on package (.deb).

All tests should pass with no failures or errors.  If for some reason
this is not the case on your system, please let me know!


Developer's Note:
-----------------

In addition to the unit tests, a custom memory tracing facility is
available:


  % ./configure --enable-memory-tracing
  % make
  % make check


With memory tracing turned on, every piece of memory in both the
library and all test suites is tracked.  At the end of the run
statistics are printed on total memory allocations, deallocations and
leaks.

The memory statistics should report zero leaks.  Again, please let me
know if you experience otherwise.

For performance reasons, memory tracing should be turned off in
production environments:


  % ./configure --disable-memory-tracing




--------------------
3. SBML Objects in C
--------------------

The SBML specification, with its UML diagrams, is suggestive of an
object-oriented (OO) design.  An API which attempts to represent SBML
would do well to use an object-oriented programming (OOP) style to
lower the inevitable impedance mismatch between specification and
implementation.  Unfortunately, the C programming language was not
designed with OOP in mind and therefore does not support many of its
features.  It is possible, however, to construct a minimal object-like
system in C with few, if any, drawbacks.  For these reasons, the
libsbml API mimics an object-oriented programming style.

The particular OOP-like style used by libsbml is not revolutionary.
In fact, it is quite common and comprised of only a few simple
stylistic conventions:


  - Every SBML class defined in the specification has a corresponding
    C "class".  UnitKinds and RuleTypes are represented by C enums.

  - The name of the C class is the derived by appending "_t" to the
    name of the SBML object, e.g. Model -> Model_t.

  - C classes are actually typedef'd structs.

  - C objects are nothing more than pointers to specific structs in
    memory.  These pointers, instead of the structs themselves, are
    passed to and returned from functions.

  - Functions meant to represent methods (or messages) on a object are
    named beginning with SBML class, followed by an underscore (_) and
    ending in the method name.  The functions take the object (pointer
    to struct) as their first argument.  For example the
    addCompartment() method of a Model:


      void Model_addCompartment(Model_t *m, Compartment_t *c);


  - Constructor names are similar to methods but end in "_create()".

  - Destructors end in "_free()".


The following table makes explicit each SBML class and its
corresponding C class:


    SBML Class                  C Class (typedef struct)
    -----------------------     ------------------------
  * SBase                       SBase_t
    Model                       Model_t    
    UnitDefinition              UnitDefinition_t
    Unit                        Unit_t
    Compartment                 Compartment_t
    Parameter                   Parameter_t
    Species                     Species_t      
    Reaction                    Reaction_t
    SpeciesReference            SpeciesReference_t
    KineticLaw                  KineticLaw_t
  * Rule                        Rule_t
  * AssignmentRule              AssignmentRule_t
    AlebraicRule                AlgebraicRule_t
    CompartmentVolumeRule       CompartmentVolumeRule_t
    ParameterRule               ParameterRule_t
    SpeciesConcentrationRule    SpeciesConcentrationRule_t


Classes marked with an asterisk (*) are abstract, which sets them
apart slightly from the others (more later).

There are two enumeration types:

     SBML Enumeration    C typedef enum
     ----------------    --------------
     UnitKind            UnitKind_t
     RuleType            RuleType_t



and one special class that exists in C, but not strictly in SBML, the
SBMLDocument_t (described in Section 4).


----------------------------
3.1. Mapping SBML Types to C
----------------------------

   Note this section describes something entirely separate from the
                       SBW <-> C type mapping.


The mapping between SBML types and C structs is straightforward as an
example will help illustrate. The SBML specification (level 1, version
2) defines Species in UML syntax as follows:


  +-------------------------------------------------------------+
  |                        Species                              |
  +-------------------------------------------------------------+
  |                                                             |
  |              name: SName                                    |
  |       compartment: SName                                    |
  |     initialAmount: double                                   |
  |             units: SName   {use="optional"}                 |
  | boundaryCondition: boolean {use="optional" default="false"} |
  |            charge: integer {use="optional"}                 |
  |                                                             |
  +-------------------------------------------------------------+


The corresponding C API struct is defined as:


  typedef struct
  {
    SBASE_FIELDS;
    char   *name;
    char   *compartment;
    double  initialAmount;
    char   *units;
    int     boundaryCondition;
    int     charge;
  } Species_t;

  (Ignore SBASE_FIELDS for now.)


The similarity between the SBML Species class and the C Species_t
struct demonstrates several key mapping rules.  In all cases,
attribute names, including capitalization, are preserved (e.g.,
initialAmount).  For the standard primitive types:


  - SName maps to a standard "char *" NULL terminated C string.
    Note that SName syntax is not yet enforced in the API.

  - double and integer map to C double and int respectively.

  - boolean maps to C int, where zero is false and non-zero is true.


-------------------
3.2 Object Creation
-------------------

Every type has a set of methods (functions) for creating, destroying
and manipulating them.  These methods are declared in header files
that correspond to the type (class) name (e.g., Model.h).  To include
all methods for all classes in one fell swoop, include SBMLTypes.h.

To instantiate a class (create an object) use either the XXX_create()
or XXX_createWith() method.  To destroy (free) an object use
XXX_free().  For example, the methods (function prototypes) for
Species (taken from Species.h) are:


  /**
   * Creates a new Species and returns a pointer to it.
   */
  Species_t *
  Species_create (void);

  /**
   * Creates a new Species with the given name, compartment, initialAmount,
   * units, boundaryCondition and charge and returns a pointer to it.  This
   * convenience function is functionally equivalent to:
   *
   *   Species_t *s = Species_create();
   *   Species_setName(s, name); Species_setCompartment(s, compartment); ...;
   */
  Species_t *
  Species_createWith( const char *name,
                      const char *compartment,
                      double     initialAmount,
                      const char *units,
                      int        boundaryCondition,
                      int        charge );

  /**
   * Frees the given Species.
   */
  void
  Species_free (Species_t *s);


The XXX_createWith() functions are a convenient way both to create
SBML objects and initialize their attributes in a single function
call.  If XXX_create() is used instead, only attributes with default
values (as defined by the specification) will be set.  All other
attributes will default to zero (in the case of integers, doubles or
boolean) or a NULL pointer (in the case of SNames strings).

When an SBML object is destroyed with XXX_free(), all of its Strings
are freed (see Section 3.3 Accessing Fields for more information) and
all of its contained objects are freed (see Section 3.4 Lists for more
information).


--------------------
3.3 Accessing Fields
--------------------

Accessing fields within a struct is almost always direct.  For
example, here is a function (which is not part of the API) to print
the fields of a Species:


  /**
   * Prints Species to stream (good for debugging).
   */
  void
  myPrintSpecies (Species_t *s, FILE *stream)
  {
    char none[]       = "(none)";
    char *compartment = (s->compartment       == NULL) ? none : s->compartment;
    char *units       = (s->units             == NULL) ? none : s->units;
    char *boundary    = (s->boundaryCondition == 0)    ? "false" : "true";


    fprintf(stream, "Species %s:\n", s->name);
    fprintf(stream, "         Compartment: %s\n", compartment   );
    fprintf(stream, "      Initial Amount: %f\n", initialAmount );
    fprintf(stream, "               Units: %s\n", units         );
    fprintf(stream, "  Boundary Condition: %s\n", boundary      );
    fprintf(stream, "              Charge: %d\n", charge        );
  }


Setting fields is direct for integers, doubles, and booleans:


  s->initialAmount     = 2.3;
  s->boundaryCondition = 1;
  s->charge            = -2;


The same is not true for SName strings.  Setting strings should always
done through accessor functions of the form XXX_setYYY().  Continuing
with Species:


  /**
   * Sets the name field of this Species to a copy of sname.
   */
  void
  Species_setName (Species_t *s, const char *sname);

  /**
   * Sets the compartment field of this Species to a copy of sname.
   */
  void
  Species_setCompartment (Species_t *s, const char *sname);

  /**
   * Sets the units field of this Species to a copy of sname.
   */
  void
  Species_setUnits (Species_t *s, const char *sname);


So to set the compartment of Species s to "cell":


  Species_setCompartment(s, "cell");


Requiring such setter functions enables clean and simple memory
semantics.  The rule is: every SBML object is responsible for its own
memory, including SName strings.  Whenever a set method is called, the
passed-in string is copied and stored.  If a previous string existed,
it is freed.  When XXX_free() is called, all strings are freed.  To
clear a string attribute, use NULL:


  Species_setUnits(s, NULL);


Note:
----

There is nothing in C language specification or the compiler that
prevents string fields from being set directly, as in:


  s->name = "s2";


But to do so would likely cause a memory leak if s->name was assigned
to another string before the above assignment was made.  Using the
setters if much safer than setting the memory directly.


---------
3.4 Lists
---------

Species contains only SNames and primitive types, but many SBML
classes also contain lists of other objects.  For example, a
UnitDefinition contains a list of Units:


  +------------------+     +---------------------------------------------+
  |  UnitDefinition  |     |                     Unit                    |
  +------------------+     +---------------------------------------------+
  |                  |     |                                             |
  | name: SName      |     |     kind: UnitKind                          |
  | unit: Unit[0..*] |     | exponent: integer {use="default" value="1"} |
  |                  |     |    scale: integer {use="default" value="0"} |
  +------------------+     |                                             |
                           +---------------------------------------------+


To help manage this containment relationship, three standard functions
are provided XXX_addYYY(), XXX_getYYY() and XXX_getNumYYY().  For
example, the methods for UnitDefinition (taken from UnitDefinition.h)
are:


  /**
   * Adds the given Unit to this UnitDefinition.
   */
  void
  UnitDefinition_addUnit(UnitDefinition_t *ud, Unit_t *u);

  /**
   * @return the nth Unit of this UnitDefinition.
   */
  Unit_t *
  UnitDefinition_getUnit(const UnitDefinition_t *ud, unsigned int n);

  /**
   * @return the number of Units in this UnitDefinition.
   */
  unsigned int
  UnitDefinition_getNumUnits(const UnitDefinition_t *ud);


Furthering the example, creating the UnitDefinition mmol l^-1 s^1 named
"mmls", corresponding to the SBML:


  <listOfUnitDefinitions>
    <unitDefinition name="mmls">
      <listOfUnits>
        <unit kind="mole"   scale="-3"/>
        <unit kind="litre"  exponent="-1"/>
        <unit kind="second" exponent="-1"/>
      </listOfUnits>
    </unitDefinition>
  </listOfUnitDefinitions>


could be accomplished with the following C:


  UnitDefinition_t *ud = UnitDefinition_createWith("mmls");


  UnitDefinition_addUnit(ud, Unit_createWith(UNIT_KIND_MOLE  ,  1, -3) );
  UnitDefinition_addUnit(ud, Unit_createWith(UNIT_KIND_LITRE , -1,  0) );
  UnitDefinition_addUnit(ud, Unit_createWith(UNIT_KIND_SECOND, -1,  0) );

  (UNIT_KIND_* enumerations are discussed later.)


In this case, UnitDefinition_getNumUnits(ud) returns 3 and
UnitDefinition_getUnit(ud, 1) returns the *second* Unit.  Notice that
items are numbered starting at zero.

Related to lists is a set of convenience methods for creating and
adding SBML objects to a Model in a single operation.  The rationale
is that since a Model is the top-level container for all other SBML
objects, programmers are likely to have handles to them.  Another way
to construct the above UnitDefinition, but this time inside a Model,
is:


  Model_t          *m  = Model_createWith("MyModel");
  UnitDefinition_t *ud = Model_createUnitDefinition(m);

  UnitDefinition_setName(ud, "mmls");

  Model_createUnit(m, Unit_createWith(UNIT_KIND_MOLE  ,  1, -3) );
  Model_createUnit(m, Unit_createWith(UNIT_KIND_LITRE , -1,  0) );
  Model_createUnit(m, Unit_createWith(UNIT_KIND_SECOND, -1,  0) );


Model_createUnit() creates a new Unit inside the Model m and returns a
pointer to it (in this case the result is discarded).  The Unit is
added to the last UnitDefinition created.  One caveat to be aware of
with these methods is the case where no intermediate container exists;
e.g., if no UnitDefinition were created above.  In that case, the call
to Model_createUnit() does nothing.  More specifically, no Unit is
created and (obviously) nothing is added to the model.

For more detailed information on Lists, see Appendix A.


----------------
3.5 Enumerations
----------------

SBML has two enumeration types, UnitKind and RuleType.  These
translate directly to C enums with a few support functions for
equality testing and converting to and from strings.  From UnitKind.h:


  typedef enum
  {
      UNIT_KIND_AMPERE
    , UNIT_KIND_BECQUEREL
    , UNIT_KIND_CANDELA

    /* Omitted for space */


    , UNIT_KIND_WATT
    , UNIT_KIND_WEBER
    , UNIT_KIND_INVALID
  } UnitKind_t;


  /**
   * Tests for logical equality between two UnitKinds.  This function behaves
   * exactly like C's == operator, except for the following two cases:
   *
   *   - UNIT_KIND_LITER == UNIT_KIND_LITRE
   *   - UNIT_KIND_METER == UNIT_KIND_METRE
   *
   * where C would yield false (since each of the above is a distinct
   * enumeration value), UnitKind_equals(...) yields true.
   *
   * @return true (!0) if uk1 is logically equivalent to uk2,
   * false (0) otherwise.
   */
  int
  UnitKind_equals (UnitKind_t uk1, UnitKind_t uk2);

  /**
   * Returns the UnitKind with the given name (case-insensitive).
   */
  UnitKind_t
  UnitKind_forName (const char *name);

  /**
   * Returns the name of the given UnitKind.  The caller does not own the
   * returned string and is therefore not allowed to modify it.
   */
  const char *
  UnitKind_toString (UnitKind_t uk);


The last item in the enumeration, UNIT_KIND_INVALID, is used whenever,
as the name implies, the UnitKind is invalid or unknown.  The
corresponding string representation is "(Invalid UnitKind)".  When a
Unit is created, its kind field is initialized to UNIT_KIND_INVALID.
Also, UnitKind_forName() will return UNIT_KIND_INVALID if the
passed-in name does not match any known UnitKind.

The same ideas apply to RuleType, except there is no need for
RuleType_equals().  See RuleType.h for more information.


Developers Note:
----------------

The internal UNIT_KIND_STRINGS table is sorted alphabetically and
UnitKind_t matches this sort order.  Because of this,
UnitKind_forName() is able to perform a binary search to find a
matching name, making its complexity O(log(n)).  That is,
UnitKind_forName() is implemented efficiently.


--------------------
3.6 Abstract Classes
--------------------

The SBML specification defines three classes that have no
representation apart from subclasses that specialize (inherit from)
them.  In OOP parlance, these types are termed abstract.  The abstract
SBML classes are:


    SBML Class                 C Class (typedef struct)
    -----------------------    ------------------------
  * SBase                      SBase_t
  * Rule                       Rule_t
  * AssignmentRule             AssignmentRule_t


The conventions for abstract classes in the libsbml API are similar to
that of other classes with a few modifications and additions.

Since abstract classes cannot be created or destroyed directly, they
have no XXX_create() or XXX_free() methods.  Instead they have
XXX_init() and XXX_clear() methods which subclasses use to initialize
and free their memory, respectively.  Users of the API do not need to
worry about these functions.

Fields of the abstract class are #defined to a symbol in the class
header file.  This symbol is used in subclasses *in the order of
inheritance*.  For example, Species inherits from SBase:


  +-------------------------------------+
  |                 SBase               |
  +-------------------------------------+
  |                                     |
  |      notes: (XHTML) {minOccurs="0"} |
  | annotation: (any)   {minOccurs="0"} |
  |                                     |
  +-------------------+-----------------+
                     / \
                    /   \
                   +-----+
                      |
                      |
  +-------------------------------------------------------------+
  |                        Species                              |
  +-------------------------------------------------------------+
  |                                                             |
  |              name: SName                                    |
  |       compartment: SName                                    |
  |     initialAmount: double                                   |
  |             units: SName   {use="optional"}                 |
  | boundaryCondition: boolean {use="optional" default="false"} |
  |            charge: integer {use="optional"}                 |
  |                                                             |
  +-------------------------------------------------------------+



SBase.h defines:


  /**
   * As shown below, put SBASE_FIELDS as the *first* item of any struct
   * which "is a(n)" SBML object.
   */
  #define SBASE_FIELDS       \
    SBMLTypeCode_t typecode; \
    char           *notes;   \
    char           *annotation


and recall Species_t from earlier:


  typedef struct
  {
    SBASE_FIELDS;
    char   *name;
    char   *compartment;
    double  initialAmount;
    char   *units;
    int     boundaryCondition;
    int     charge;
  } Species_t;


The effect is that when the library source is compiled, the first
three fields of Species are typecode, notes, and annotation.  In fact,
every class that inherits from SBase, i.e. all SBML classes, have
these same first three fields.  Accessing the notes or annotation
field of a Species_t *s, or any other SBML object, is the same as for
other fields.  For example:


  if (s->notes != NULL)
  {
    printf("Notes for Species %s:\n", (s->name == NULL) ? "(null)" : s->name);
    printf("%s", s->notes);
  }


Setting string fields requires special care to guard against memory
leaks.  The XXX_setYYY() methods must be used.  But, Species does not
define either the notes or annotation fields and as such there are
*no* Species_setNotes() or Species_setAnnotation() methods.  Instead,
SBase defines them:


  /**
   * Sets the notes field of the given SBML object to a copy of notes.  If
   * object already has notes, the existing string is freed before the new
   * one is copied.
   */
  void
  SBase_setNotes (SBase_t *sb, const char *notes);

  /**
   * Sets the annotation field of the given SBML object to a copy of
   * annotations.  If object already has an annotation, the existing string
   * is freed before the new one is copied.
   */
  void
  SBase_setAnnotation (SBase_t *sb, const char *annotation);


The first argument to these functions is, of course, an object of type
SBase.  Since Species inherits from SBase, i.e. Species is an SBase,
it can be used as the first argument to these functions.  A slight
caveat is a cast is required.  To set the notes field of Species s
then:


  SBase_setNotes( (SBase_t *) s, "My Favorite Species" );


The same applies to all other SBML objects.


Finally, each SBML class has a typecode which is initialized when an
object is instantiated.  The typecode is a simple C enumeration,
defined in SBMLTypeCodes.h:


  /**
   * An enumeration of SBML types to help identify SBML objects at runtime.
   * Abstract types do not have a typecode since they cannot be instantiated.
   */
  typedef enum
  {
    SBML_COMPARTMENT,
    SBML_KINETIC_LAW,
    SBML_MODEL,
    SBML_PARAMETER,
    SBML_REACTION,
    SBML_SPECIES,
    SBML_SPECIES_REFERENCE,
    SBML_UNIT_DEFINITION,
    SBML_UNIT,
    SBML_ALGEBRAIC_RULE,
    SBML_SPECIES_CONCENTRATION_RULE,
    SBML_COMPARTMENT_VOLUME_RULE,
    SBML_PARAMETER_RULE
  } SBMLTypeCode_t;


The primary reason for the typecode is distinguish specific types of
rules in a Model.  A Model contains a list of rules, but a Rule in
SBML may be of one of four specific types: AlgebraicRule,
SpeciesConcentrationRule, CompartmentVolumeRule and ParameterRule.




---------------------
4. Reading SBML Files
---------------------

SBML may be read from a file or an in memory string into an
SBMLDocument.  SBMLReader.h defines two read functions:


  /**
   * Reads the SBML document from the given file and returns a pointer to it.
   */
  SBMLDocument_t *
  readSBML (const char *filename);

  /**
   * Reads the SBML document from the given XML string and returns a pointer
   * to it.
   *
   * The XML string must be complete and legal XML document.  Among other
   * things, it must start with an XML processing instruction.  For e.g.,:
   *
   *   <?xml version='1.0' encoding='UTF-8'?>
   */
  SBMLDocument_t *
  readSBMLFromString (const char *xml);


These functions return a pointer to an SBMLDocument:


  /**
   * The SBMLDocument
   */
  typedef struct
  {
    unsigned int level;
    unsigned int version;

    List_t *error;
    List_t *fatal;
    List_t *warning;

    Model_t *model;
  } SBMLDocument_t;


The level and version of the SBML document are stored in the first two
fields.  The last field is the SBML model itself.  The three lists
record warnings and errors encountered during the XML parse.  Each
warning or error is a ParseMessage (again from SBMLDocument.h):


  /**
   * SBMLDocuments contain three Lists of ParseMessages, one for each class
   * of messages that could be triggered during an XML parse: Warnings,
   * Errors and Fatal Errors.
   *
   * Each ParseMessage contains the message itself and the line and column
   * numbers of the XML entity that triggered the message.  If line or column
   * information is unavailable, -1 is used.
   */
  typedef struct
  {
    char *message;
    int  line;
    int  column;
  } ParseMessage_t;


While its possible to access these lists directly, convenience
functions are provided:


  /**
   * @return the nth warning encountered during the parse of this
   * SBMLDocument or NULL if n > getNumWarnings() - 1.
   */
  ParseMessage_t *
  SBMLDocument_getWarning (SBMLDocument_t *d, unsigned int n);

  /**
   * @return the nth error encountered during the parse of this
   * SBMLDocument or NULL if n > getNumErrors() - 1.
   */
  ParseMessage_t *
  SBMLDocument_getError (SBMLDocument_t *d, unsigned int n);

  /**
   * @return the nth fatal error encountered during the parse of this
   * SBMLDocument or NULL if n > getNumErrors() - 1.
   */
  ParseMessage_t *
  SBMLDocument_getFatal (SBMLDocument_t *d, unsigned int n);

  /**
   * @return the number of warnings encountered during the parse of this
   * SBMLDocument.
   */
  unsigned int
  SBMLDocument_getNumWarnings (SBMLDocument_t *d);

  /**
   * @return the number of errors encountered during the parse of this
   * SBMLDocument.
   */
  unsigned int
  SBMLDocument_getNumErrors (SBMLDocument_t *d);

  /**
   * @return the number of fatal errors encountered during the parse of this
   * SBMLDocument.
   */
  unsigned int
  SBMLDocument_getNumFatals (SBMLDocument_t *d);


------------
4.1 Examples
------------

The following example is included in your distribution as readSBML.c.
It is compiled as part of the build process, but is not installed.
The program takes a single command-line argument, the name of an SBML
file, reads it into memory and reports some basic information about
the file.  Warnings or errors, if any are reported, as well as the
total read time (in milliseconds).

To run the example, go to the top-level directory where libsbml was
unpacked and:

  % cd src
  % ./readSBML test-data/l1v1-branch.xml
  % ./readSBML test-data/l1v1-minimal.xml
  % ./readSBML test-data/l1v1-rules.xml
  % ./readSBML test-data/l1v1-units.xml
  % # etc...


The exact procedure for compile the example (and linking-in libsbml in
general) varies from one platform and compiler to another.  On Linux
or Solaris with GCC the following should work:


  % gcc -o readSBML readSBML.c -lsbml


The entire example follows:


#include <stdio.h>
#include <sys/timeb.h>

#include "sbml/SBMLReader.h"
#include "sbml/SBMLTypes.h"


/**
 * Function Prototypes
 */
unsigned long getCurrentMillis (void);
void          reportErrors     (SBMLDocument_t *d);


int
main (int argc, char *argv[])
{
  SBMLDocument_t *d;
  Model_t *m;

  unsigned long start, stop;


  if (argc != 2)
  {
    printf("usage: readSBML <filename>\n");
    return 1;
  }

  start = getCurrentMillis();
  d     = readSBML(argv[1]);
  stop  = getCurrentMillis();

  m = d->model;

  printf( "File: %s\n", argv[1]);
  printf( "       model name: %s\n",  m->name );
  printf( "  unitDefinitions: %d\n",  Model_getNumUnitDefinitions(m) );
  printf( "     compartments: %d\n",  Model_getNumCompartments(m)    );
  printf( "          species: %d\n",  Model_getNumSpecies(m)         );
  printf( "       parameters: %d\n",  Model_getNumParameters(m)      );
  printf( "        reactions: %d\n",  Model_getNumReactions(m)       );
  printf( "            rules: %d\n",  Model_getNumRules(m)           );
  printf( "\n");

  reportErrors(d);

  printf( "Total Read Time (ms): %lu\n", stop - start);


  SBMLDocument_free(d);
  return 0;
}


unsigned long
getCurrentMillis (void)
{
  struct timeb t;


  ftime(&t);
  return (unsigned long) (t.time * 1000 + t.millitm);
}


void
reportErrors (SBMLDocument_t *d)
{
  unsigned int   n, size;
  ParseMessage_t *pm;


  /* Fatal Errors */
  if ((size = SBMLDocument_getNumFatals(d)) > 0)
  {
    printf("%d Fatal Error(s):\n", size);

    for (n = 0; n < size; n++)
    {
      pm = SBMLDocument_getFatal(d, n);
      printf("  Line %d, Col %d: %s\n", pm->line, pm->column, pm->message);
    }

    printf("\n");
  }

  /* Errors */
  if ((size = SBMLDocument_getNumErrors(d)) > 0)
  {
    printf("%d Error(s):\n", size);

    for (n = 0; n < size; n++)
    {
      pm = SBMLDocument_getError(d, n);
      printf("  Line %d, Col %d: %s\n", pm->line, pm->column, pm->message);
    }

    printf("\n");
  }

  /* Warnings */
  if ((size = SBMLDocument_getNumWarnings(d)) > 0)
  {
    printf("%d Warning(s):\n", size);

    for (n = 0; n < size; n++)
    {
      pm = SBMLDocument_getWarning(d, n);
      printf(" Line %d, Col %d: %s\n", pm->line, pm->column, pm->message);
    }

    printf("\n");
  }
}




---------------------
5. Writing SBML Files
---------------------


For now look at SBMLFormatter.cpp and SBMLWriter.cpp.  Documentation
is coming soon.




------------------
A. Appendix: Lists
------------------


While List convenience methods (e.g., XXX_getNumYYY()) are provided
for every object, it is possible to access and manipulate each list
directly.  All lists are themselves objects of type List_t.  The full
set of list methods are:


  /**
   * Adds item to this List.
   */
  void
  List_add (List_t *list, void *item);

  /**
   * Returns the nth item in this List.  If n > List_size(list) returns NULL.
   */
  void *
  List_get (List_t *list, unsigned int n);

  /**
   * Removes the nth item from this List and returns a pointer to it.  If n >
   * List_size(list) returns NULL.
   */
  void *
  List_remove (List_t *list, unsigned int n);

  /**
   * Returns the number of elements in this List.
   */
  unsigned int
  List_size (List_t *list);


Developer's Note:
-----------------

The list is singly-linked and maintains pointers to its head (first),
tail (last), and current (last accessed) nodes.  Maintaining a pointer
to these allows for all of the following common operations to occur in
constant time, i.e. O(1):


  int n;
  int size = List_size(list);

  List_get(list, 0);
  List_get(list, size - 1);

  for (n = 0; n < size; n++)
  {
    List_get(list, n);
  }


Since UnitDefinitions maintains a List of Units, the UnitDefinition
example in section 3.4 could also be written as:


  UnitDefinition_t *ud = UnitDefinition_createWith("mmls");


  List_add(ud->unit, Unit_createWith(UNIT_KIND_MOLE  ,  1, -3) );
  List_add(ud->unit, Unit_createWith(UNIT_KIND_LITRE , -1,  0) );
  List_add(ud->unit, Unit_createWith(UNIT_KIND_SECOND, -1,  0) );


However, this approach is not preferred.  The best reason to use
specific XXX_getYYY() methods over the List API is that the former are
typed to specific items, whereas List_get() returns a void pointer
that must be cast to a specific type.  For example, compare:


  Unit_t *u = UnitDefinition_getUnit(u, 1);


to:


  Unit_t *u = (Unit_t *) List_get(ud->unit, 1);


Further, the first is more readable.  The List API is mentioned i) for
the sake of completeness and ii) currently the only way to remove an
item from a list is to use the API directly:


  List_remove(ud->unit, 2);


has no analog in UnitDefinition.  "Remove" convenience methods can be
added to the API.  This feature was skipped because list item removal
seemed like an uncommon operation.




-------------------------------------------
File author: B. Bornstein
Last Modified: $Date$
-------------------------------------------
