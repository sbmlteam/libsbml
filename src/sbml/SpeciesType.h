/**
 * @file    SpeciesType.h
 * @brief   Definitions of SpeciesType and ListOfSpeciesType.
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class SpeciesType
 * @brief LibSBML implementation of %SBML's %SpeciesType construct.
 *
 * The term @em species @em type refers to reacting entities independent of
 * location.  These include simple ions (e.g., protons, calcium), simple
 * molecules (e.g., glucose, ATP), large molecules (e.g., RNA,
 * polysaccharides, and proteins), and others.
 * 
 * SpeciesType structures are included in %SBML to enable Species of the
 * same type to be related together.  It is a conceptual construct; the
 * existence of SpeciesType objects in a model has no effect on the
 * model's numerical interpretation.  Except for the requirement for
 * uniqueness of species/species type combinations located in compartments,
 * simulators and other numerical analysis software may ignore SpeciesType
 * definitions and references to them in a model.
 * 
 * There is no mechanism in %SBML for representing hierarchies of species
 * types.  One SpeciesType object cannot be the subtype of another
 * SpeciesType object; %SBML provides no means of defining such
 * relationships.
 * 
 * As with other major structures in %SBML, SpeciesType has a mandatory
 * attribute, "id", used to give the species type an identifier.  The
 * identifier must be a text string conforming to the identifer syntax
 * permitted in %SBML.  SpeciesType also has an optional "name" attribute,
 * of type @c string.  The "id" and "name" must be used according to the
 * guidelines described in the %SBML specification (e.g., Section 3.3 in
 * the Level 2 Version 3 specification).
 *
 * SpeciesType was introduced in SBML Level 2 Version 2.  It is not
 * available in earlier versions of Level 2 nor in any version of Level 1.
 *
 * <!---------------------------------------------------------------------- -->
 *
 * @class ListOfSpeciesTypes.
 * @brief Container class for lists of SpeciesType objects in a Model.
 * 
 * The various ListOf___ classes in %SBML are merely containers used for
 * organizing the main components of an %SBML model.  All are derived from
 * the abstract class SBase, and inherit the various attributes and
 * subelements of SBase, such as "metaid" as and "annotation".  The
 * ListOf___ classes do not add any attributes of their own.
 *
 * The relationship between the lists and the rest of an %SBML model is
 * illustrated by the following (for %SBML Level 2 Version 3):
 *
 * @image html listof-illustration.jpg "ListOf___ elements in an SBML Model"
 * @image latex listof-illustration.jpg "ListOf___ elements in an SBML Model"
 *
 * Readers may wonder about the motivations for using the ListOf___
 * containers.  A simpler approach in XML might be to place the components
 * all directly at the top level of the model definition.  We chose instead
 * to group them within XML elements named after ListOf<em>Classname</em>,
 * in part because we believe this helps organize the components and makes
 * visual reading of models in XML easier.  More importantly, the fact that
 * the container classes are derived from SBase means that software tools
 * can add information about the lists themselves into each list
 * container's "annotation".
 *
 * @see ListOfFunctionDefinitions, ListOfUnitDefinitions,
 * ListOfCompartmentTypes, ListOfSpeciesTypes, ListOfCompartments,
 * ListOfSpecies, ListOfParameters, ListOfInitialAssignments, ListOfRules,
 * ListOfConstraints, ListOfReactions, and ListOfEvents.
 */

#ifndef SpeciesType_h
#define SpeciesType_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>


class SBMLVisitor;


class LIBSBML_EXTERN SpeciesType : public SBase
{
public:

  /**
   * Creates a new SpeciesType, optionally with the given @p id and @p
   * name attribute values.
   *
   * In %SBML, identifiers are required for SpeciesType objects;
   * however, the identifier does not have to be set at the time of
   * creation of the object, and instead can be set using the setId()
   * method on the SBase parent class.
   *
   * @param id a string, the identifier of this SpeciesType instance
   * @param name a string, the optional name of this SpeciesType instance
   *
   * @docnote The native C++ implementation of this method defines a
   * default argument value.  In the documentation generated for different
   * libSBML language bindings, you may or may not see corresponding
   * arguments in the method declarations.  For example, in Java, a default
   * argument is handled by declaring two separate methods, with one of
   * them having the argument and the other one lacking the argument.
   * However, the libSBML documentation will be @em identical for both
   * methods.  Consequently, if you are reading this and do not see an
   * argument even though one is described, please look for descriptions of
   * other variants of this method near where this one appears in the
   * documentation.
   */
  SpeciesType (const std::string& id = "", const std::string& name = "");


  /**
   * Destroys this SpeciesType.
   */
  virtual ~SpeciesType ();


  /**
  * Copy constructor; creates a copy of this SpeciesType.
  */
  SpeciesType(const SpeciesType& orig);


  /**
   * Assignment operator for SpeciesType.
   */
  SpeciesType& operator=(const SpeciesType& orig);


  /**
   * Accepts the given SBMLVisitor for this instance of SpeciesType.
   *
   * @param v the SBMLVisitor instance to be used.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether the Visitor would like to visit the next SpeciesType in
   * the list of compartment types.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /**
   * Creates and returns a deep copy of this SpeciesType.
   * 
   * @return a (deep) copy of this SpeciesType.
   */
  virtual SBase* clone () const;


  /**
   * Returns the libSBML type code for this %SBML object.
   * 
   * @return the SBMLTypeCode_t of this object or SBML_UNKNOWN (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;


  /**
   * Returns the XML element name of this object, which for
   * SpeciesType, is always @c "compartmentType".
   * 
   * @return the name of this element, i.e., @c "compartmentType".
   */
  virtual const std::string& getElementName () const;


protected:
  /** @cond doxygen-libsbml-internal */

  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   *
   * @param attributes the XMLAttributes to use.
   */
  virtual void readAttributes (const XMLAttributes& attributes);


  /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.
   *
   * @param stream the XMLOutputStream to use.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;

  /** @endcond doxygen-libsbml-internal */
};



class LIBSBML_EXTERN ListOfSpeciesTypes : public ListOf
{
public:

  /**
   * Creates and returns a deep copy of this ListOfSpeciesTypes instance.
   *
   * @return a (deep) copy of this ListOfSpeciesTypes.
   */
  virtual SBase* clone () const;

  /**
   * Returns the libSBML type code for this %SBML object.
   * 
   * @return the SBMLTypeCode_t of this object or SBML_UNKNOWN (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const { return SBML_LIST_OF; };

  /**
   * Returns the libSBML type code for the objects contained in this ListOf
   * (i.e., SpeciesType objects, if the list is non-empty).
   * 
   * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
   * SBML_UNKNOWN (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getItemTypeCode () const;


  /**
   * Returns the XML element name of this object.
   *
   * For ListOfSpeciesTypes, the XML element name is @c
   * "listOfSpeciesTypes".
   * 
   * @return the name of this element, i.e., @c "listOfSpeciesTypes".
   */
  virtual const std::string& getElementName () const;


  /** @cond doxygen-libsbml-internal */

  /**
   * Get the ordinal position of this element in the containing object
   * (which in this case is the Model object).
   *
   * The ordering of elements in the XML form of %SBML is generally fixed
   * for most components in %SBML.  For example, the
   * ListOfSpeciesTypes in a model (in %SBML Level 2 Version 3) is the
   * third ListOf___.  (However, it differs for different Levels and
   * Versions of SBML, so calling code should not hardwire this number.)
   *
   * @return the ordinal position of the element with respect to its
   * siblings, or @c -1 (default) to indicate the position is not significant.
   */
  virtual int getElementPosition () const;

  /** @endcond doxygen-libsbml-internal */


protected:
  /** @cond doxygen-libsbml-internal */

  /**
   * Create a ListOfSpeciesTypes object corresponding to the next token
   * in the XML input stream.
   * 
   * @return the %SBML object corresponding to next XMLToken in the
   * XMLInputStream, or @c NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);

  /** @endcond doxygen-libsbml-internal */
};


#endif  /* __cplusplus */


#ifndef SWIG

BEGIN_C_DECLS

/*-----------------------------------------------------------------------------
 * See the .cpp file for the documentation of the following functions.
 *---------------------------------------------------------------------------*/


LIBSBML_EXTERN
SpeciesType_t *
SpeciesType_create (void);


LIBSBML_EXTERN
SpeciesType_t *
SpeciesType_createWith (const char *sid, const char *name);


LIBSBML_EXTERN
void
SpeciesType_free (SpeciesType_t *st);


LIBSBML_EXTERN
SpeciesType_t *
SpeciesType_clone (const SpeciesType_t *st);


LIBSBML_EXTERN
const char *
SpeciesType_getId (const SpeciesType_t *st);


LIBSBML_EXTERN
const char *
SpeciesType_getName (const SpeciesType_t *st);


LIBSBML_EXTERN
int
SpeciesType_isSetId (const SpeciesType_t *st);


LIBSBML_EXTERN
int
SpeciesType_isSetName (const SpeciesType_t *st);


LIBSBML_EXTERN
void
SpeciesType_setId (SpeciesType_t *st, const char *sid);


LIBSBML_EXTERN
void
SpeciesType_setName (SpeciesType_t *st, const char *name);


LIBSBML_EXTERN
void
SpeciesType_unsetName (SpeciesType_t *st);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* SpeciesType_h */
