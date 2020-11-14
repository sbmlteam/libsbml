/**
 * @file:   SubListOfSpeciesFeatures.h
 * @brief:  Implementation of the SubListOfSpeciesFeatures class
 * @author: Fengkai Zhang
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2009-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class SubListOfSpeciesFeatures
 * @sbmlbrief{multi} Defines a set of SpeciesFeature objects.
 *
 * The SubListOfSpeciesFeatures object is an optional child of the
 * ListOfSpeciesFeatures list child of the extended Species (via the
 * MultiSpeciesPlugin object).  Listed alongside its sibling SpeciesFeature
 * objects, it allows the user to define a set of two or more SpeciesFeature
 * elements that have a logical relationship with each other.  This
 * relationship is defined by the "relation" attribute, which is an
 * enumeration of values representing "and", "or", "not".  (An "unknown"
 * option is provided here for incomplete models, but cannot be used in a
 * valid SBML document.)  The following constants represent the values:
 * @sbmlconstant{MULTI_RELATION_AND, Relation_t},
 * @sbmlconstant{MULTI_RELATION_OR, Relation_t},
 * @sbmlconstant{MULTI_RELATION_NOT, Relation_t}, and
 * @sbmlconstant{MULTI_RELATION_UNKNOWN, Relation_t}.
 * If any SpeciesFeature involved in a SubListOfSpeciesFeatures references a
 * SpeciesFeatureType with an "occur" attribute greater than 1, the
 * SubListOfSpeciesFeatures can only have the value "and" for its relation
 * attribute.
 */

 /**
 * <!-- ~ ~ ~ ~ ~ Start of common documentation strings ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
 * The following text is used as common documentation blocks copied multiple
 * times elsewhere in this file. The use of @class is a hack needed because
 * Doxygen's @copydetails command has limited functionality. Symbols
 * beginning with "doc_" are marked as ignored in our Doxygen configuration.
 * ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ -->
 *
 *
 * @class doc_sublistofspeciesfeatures_relation
 *
 * @par
 * The attribute "relation" on a SubListOfSpeciesFeatures object is used to
 * define the logic relationship among its children.  If any SpeciesFeature
 * involved in a SubListOfSpeciesFeatures references a SpeciesFeatureType with 
 * occur > 1, the SubListOfSpeciesFeatures can only have the value 'and' for its
 * relation attribute.
 *
 * In the SBML
 * Level&nbsp;3 Version&nbsp;1 Multi specification, the following are the
 * allowable values for "relation":
 * <ul>
 * <li> @c "and", means that the species features all apply.
 *
 * <li> @c "or", means that at least one of the species features apply.
 *
 * <li> @c "not", means that the species feature must not apply.
 *
 * </ul>
 */
#ifndef SubListOfSpeciesFeatures_H__
#define SubListOfSpeciesFeatures_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/multi/common/multifwd.h>

/**
 * @enum  Relation_t
 * @brief Enumeration of possible relations between the children of a
 * SubListOfSpeciesFeatures in the libSBML "multi" package implementation.
 *
 * @copydetails doc_what_are_typecodes
 *
 * @copydetails doc_additional_typecode_details
 */
typedef enum
{
    MULTI_RELATION_AND     /** The SpeciesFeature children of the SubListOfSpeciesFeatures share an 'and' relationship. */
  , MULTI_RELATION_OR      /** The SpeciesFeature children of the SubListOfSpeciesFeatures share an 'or' relationship. */
  , MULTI_RELATION_NOT     /** The SpeciesFeature children of the SubListOfSpeciesFeatures share a 'not' relationship. */
  , MULTI_RELATION_UNKNOWN /** The SpeciesFeature children of the SubListOfSpeciesFeatures share an unknown relationship.  This value is not permitted for valid SBML models. */
} Relation_t;



#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>

#include <sbml/packages/multi/extension/MultiExtension.h>
#include <sbml/packages/multi/sbml/SpeciesFeature.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN SubListOfSpeciesFeatures : public ListOf
{

protected:

  /** @cond doxygenLibsbmlInternal */

  ////  std::string   mId;
  Relation_t mRelation;
  std::string   mComponent;

  /** @endcond */


public:

  /**
   * Creates a new SubListOfSpeciesFeatures object.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SubListOfSpeciesFeatures(unsigned int level      = MultiExtension::getDefaultLevel(),
                        unsigned int version    = MultiExtension::getDefaultVersion(),
                        unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new SubListOfSpeciesFeatures with the given MultiPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param multins the MultiPkgNamespaces object
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SubListOfSpeciesFeatures(MultiPkgNamespaces* multins);


  /**
   * Creates and return a copy of SubListOfSpeciesFeatures.
   *
   * @param orig this SubListOfSpeciesFeatures object
   */
  SubListOfSpeciesFeatures(const SubListOfSpeciesFeatures &orig);


  /**
   * Creates and returns a deep copy of this SubListOfSpeciesFeatures object.
   *
   * @return a (deep) copy of this SubListOfSpeciesFeatures object.
   */
  virtual SubListOfSpeciesFeatures* clone () const;


  /**
   * Destroys this SubListOfSpeciesFeatures object.
   */
  virtual ~SubListOfSpeciesFeatures();

  /**
   * Returns the value of the "id" attribute of this SubListOfSpeciesFeatures.
   *
   * @return the value of the "id" attribute of this SubListOfSpeciesFeatures as a string.
   */
  virtual const std::string& getId() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SubListOfSpeciesFeatures's "id" attribute has been set.
   *
   * @return @c true if this SubListOfSpeciesFeatures's "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Sets the value of the "id" attribute of this SubListOfSpeciesFeatures.
   *
   * @param id const std::string& value of the "id" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setId(const std::string& id);


  /**
   * Unsets the value of the "id" attribute of this SubListOfSpeciesFeatures.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Returns the value of the "name" attribute of this
   * SubListOfSpeciesFeatures.
   *
   * @return the value of the "name" attribute of this
   * SubListOfSpeciesFeatures as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns @c true if this SubListOfSpeciesFeatures's "name" attribute has
   * been set.
   *
   * @return @c true if this SubListOfSpeciesFeatures' "name" attribute has
   * been set, otherwise @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "name" attribute of this SubListOfSpeciesFeatures.
   *
   * @param name the new value for the attribute.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setName(const std::string& name);


  /**
   * Unsets the value of the "name" attribute of this
   * SubListOfSpeciesFeatures.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the value of the "component" attribute of this
   * SubListOfSpeciesFeatures.
   *
   * @return the value of the "component" attribute of this
   * SubListOfSpeciesFeatures as a string.
   */
  virtual const std::string& getComponent() const;


  /**
   * Returns @c true if this SubListOfSpeciesFeatures's "component" attribute
   * has been set.
   *
   * @return @c true if this SubListOfSpeciesFeatures's "component" attribute
   * has been set; otherwise, @c false is returned.
   */
  virtual bool isSetComponent() const;


  /**
   * Sets the value of the "component" attribute of this
   * SubListOfSpeciesFeatures.
   *
   * @param component the new value for the attribute.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setComponent(const std::string& component);


  /**
   * Unsets the value of the "component" attribute of this
   * SubListOfSpeciesFeatures.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetComponent();


  /**
   * Creates a new SpeciesFeature object and adds it to this
   * SubListOfSpeciesFeatures object.
   *
   * @return the newly created SpeciesFeature object.
   */
  SpeciesFeature* createSpeciesFeature ();


  /**
   * Get the nth SpeciesFeature object from the SubListOfSpeciesFeatures.
   *
   * @param n the index number of the SpeciesFeature to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see size()
   */
  virtual SpeciesFeature* get(unsigned int n);


  /**
   * Get the nth SpeciesFeature object from the SubListOfSpeciesFeatures.
   *
   * @param n the index number of the SpeciesFeature to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see size()
   */
  virtual const SpeciesFeature* get(unsigned int n) const;


  /**
   * Get the SpeciesFeature object with the given identifier @p sid.
   *
   * @param sid a string representing the identifier
   * of the SpeciesFeature to get.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual SpeciesFeature* get(const std::string& sid);


  /**
   * Get the SpeciesFeature object with the given identifier @p sid.
   *
   * @param sid a string representing the identifier
   * of the SpeciesFeature to get.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const SpeciesFeature* get(const std::string& sid) const;


  /**
   * Removes the nth SpeciesFeature object from this
   * SubListOfSpeciesFeatures.
   *
   * @param n the index of the SpeciesFeature to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   *
   * @see size()
   */
  virtual SpeciesFeature* remove(unsigned int n);


  /**
   * Removes the SpeciesFeature object with the given identifier @p sid.
   *
   * @param sid the identifier of the SpeciesFeature to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   */
  virtual SpeciesFeature* remove(const std::string& sid);


  /**
   * Returns the value of the "relation" attribute of this
   * SubListOfSpeciesFeatures.
   *
   * @return the value of the "relation" attribute of this
   * SubListOfSpeciesFeatures as a FIX ME.
   */
  Relation_t getRelation() const;


  /**
   * Returns @c true if this SubListOfSpeciesFeatures's "relation" attribute
   * has been set.
   *
   * @return @c true if this SubListOfSpeciesFeatures's "relation" attribute
   * has been set; otherwise, @c false is returned.
   */
  virtual bool isSetRelation() const;


  /**
   * Sets the value of the "relation" attribute of this SubListOfSpeciesFeatures.
   *
   * @param relation FIX ME value of the "relation" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setRelation(Relation_t relation);


  /**
   * Sets the value of the "relation" attribute of this
   * SubListOfSpeciesFeatures.
   *
   * @param relation std::string& of the "relation" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_sublistofspeciesfeatures_relation
   */
  int setRelation(const std::string& relation);


  /**
   * Unsets the value of the "relation" attribute of this SubListOfSpeciesFeatures.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetRelation();


  /**
   * Returns the XML element name of this object.
   *
   * @return the name of this element, i.e. @c "subListOfSpeciesFeatures".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for this SBML object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_MULTI_BINDING_SITE_SPECIES_TYPE, SBMLMultiTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode () const;


  /**
   * Returns the libSBML type code for the objects contained in this ListOf
   * (i.e., Compartment objects, if the list is non-empty).
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for the objects contained in this ListOf
   * instance: @sbmlconstant{SBML_COMPARTMENT, SBMLTypeCode_t} (default).
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode () const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * Connects to child elements.
   */
  virtual void connectToChild ();
  /** @endcond */


  /**
   * Returns the number of SpeciesFeature objects contained in this
   * SubListOfSpeciesFeatures.
   *
   * A ListOfSpeciesFeature can contain either speciesFeature elements or a
   * SubListOfSpeciesFeatures (which is derived from ListOf), which itself
   * contains SpeciesFeature elements. The sublist also has a couple of
   * attributes which describe the relationship of the members of the sublist
   * to each other and their parent ListOfSpeciesFeatures.  Here is a sample
   * of the XML:
   * @verbatim
<multi:listOfSpeciesFeatures>
  <multi:speciesFeature multi:speciesFeatureType="sftP" multi:occur="1">
    <snip/>
  </multi:speciesFeature>
  <multi:subListOfSpeciesFeatures multi:component="stY1" multi:relation="not">
    <multi:speciesFeature multi:speciesFeatureType="sftYP1" multi:occur="1">
      <snip/>
    </multi:speciesFeature>
    <multi:speciesFeature multi:speciesFeatureType="sftYP2" multi:occur="1">
      <multi:listOfSpeciesFeatureValues>
        <multi:speciesFeatureValue multi:value="yp1v1b"/>
      </multi:listOfSpeciesFeatureValues>
    </multi:speciesFeature>
  </multi:subListOfSpeciesFeatures>
</multi:listOfSpeciesFeatures>
@endverbatim
   *
   * @return a count of the SpeciesFeature objects.
   */
  unsigned int getNumSpeciesFeatures() const;


  /** @cond doxygenLibsbmlInternal */
  /**
   */
  /**
   * Accepts the given SBMLVisitor.
   */
  virtual bool accept (SBMLVisitor& v) const;

  /** @endcond */


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new SpeciesFeature in this ListOfSpeciesFeatures
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write the namespace for the Multi package.
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;


  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to get the list of
   * expected attributes.
   * This function is invoked from corresponding readAttributes()
   * function.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Reads the attributes of corresponding package in SBMLDocument element.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Writes the attributes of corresponding package in SBMLDocument element.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;

  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   */
  virtual void writeElements (XMLOutputStream& stream) const;

  /** @endcond */

};

LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Returns the string version of the provided Relation_t enumeration.
 *
 * @param r the Relation_t enumeration value to convert.
 *
 * @return A string corresponding to the given type:
 * "and",
 * "or",
 * "not",
 * or @c NULL if the value is @sbmlconstant{MULTI_RELATION_UNKNOWN, Relation_t} or
 * another invalid enumeration value.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @if conly
 * @memberof SubListOfSpeciesFeatures_t
 * @endif
 */
LIBSBML_EXTERN
const char*
Relation_toString(Relation_t r);

/**
 * Returns the Relation_t enumeration corresponding to the given string or
 * @sbmlconstant{MULTI_RELATION_UNKNOWN, Relation_t} if there is no such match.
 *
 * @param code the string to convert to a Relation_t.
 *
 * @return the corresponding Relation_t or @sbmlconstant{MULTI_RELATION_UNKNOWN,
 * Relation_t} if no match is found.
 *
 * @note The matching is case-sensitive: "and" will return
 * @sbmlconstant{MULTI_RELATION_AND, Relation_t}, but "And" will return
 * @sbmlconstant{MULTI_RELATION_UNKNOWN, Relation_t}.
 *
 * @if conly
 * @memberof SubListOfSpeciesFeatures_t
 * @endif
 */
LIBSBML_EXTERN
Relation_t
Relation_fromString(const char* code);

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END



#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given Relation_t is valid.
 *
 * @param relation the Relation_t enumeration to query.
 *
 * @return @c 1 (true) if the Relation_t is
 * @sbmlconstant{MULTI_RELATION_AND, Relation_t},
 * @sbmlconstant{MULTI_RELATION_OR, Relation_t}, or
 * @sbmlconstant{MULTI_RELATION_NOT, Relation_t};
 * @c 0 (false) otherwise (including @sbmlconstant{MULTI_RELATION_UNKNOWN,
 * Relation_t}).
 *
 * @if conly
 * @memberof SubListOfSpeciesFeatures_t
 * @endif
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_isValidRelation(Relation_t relation);


/**
 * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
 * given string is a valid Relation_t.
 *
 * @param code the string to query.
 *
 * @return @c 1 (true) if the string is
 * "and",
 * "or", or
 * "not";
 * @c 0 (false) otherwise.
 *
 * @note The matching is case-sensitive: "and" will return @c 1 (true), but
 * "And" will return @c 0 (false).
 *
 * @if conly
 * @memberof SubListOfSpeciesFeatures_t
 * @endif
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_isValidRelationString(const char* code);

/**
 * Creates a new SubListOfSpeciesFeatures_t using the given SBML Level, Version
 * and &ldquo;multi&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * SubListOfSpeciesFeatures_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * SubListOfSpeciesFeatures_t.
 *
 * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this
 * SubListOfSpeciesFeatures_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
SubListOfSpeciesFeatures_t *
SubListOfSpeciesFeatures_create(unsigned int level,
  unsigned int version,
  unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this SubListOfSpeciesFeatures_t object.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure.
 *
 * @return a (deep) copy of this SubListOfSpeciesFeatures_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
SubListOfSpeciesFeatures_t*
SubListOfSpeciesFeatures_clone(const SubListOfSpeciesFeatures_t* slosf);


/**
 * Frees this SubListOfSpeciesFeatures_t object.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure.
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
void
SubListOfSpeciesFeatures_free(SubListOfSpeciesFeatures_t* slosf);


/**
 * Returns the value of the "id" attribute of this SubListOfSpeciesFeatures_t.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this SubListOfSpeciesFeatures_t
 * as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
char *
SubListOfSpeciesFeatures_getId(const SubListOfSpeciesFeatures_t * slosf);


/**
 * Returns the value of the "name" attribute of this
 * SubListOfSpeciesFeatures_t.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this SubListOfSpeciesFeatures_t
 * as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
char *
SubListOfSpeciesFeatures_getName(const SubListOfSpeciesFeatures_t * slosf);


/**
 * Returns the value of the "relation" attribute of this
 * SubListOfSpeciesFeatures_t.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure whose relation is
 * sought.
 *
 * @return the value of the "relation" attribute of this
 * SubListOfSpeciesFeatures_t as a Relation_t.
 *
 * @copydetails doc_sublistofspeciesfeatures_relation
 * @if clike The value is drawn from the enumeration @ref Relation_t @endif
 * The possible values returned by this method are:
 * @li @sbmlconstant{MULTI_RELATION_AND, Relation_t}
 * @li @sbmlconstant{MULTI_RELATION_OR, Relation_t}
 * @li @sbmlconstant{MULTI_RELATION_NOT, Relation_t}
 * @li @sbmlconstant{MULTI_RELATION_UNKNOWN, Relation_t}
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
Relation_t
SubListOfSpeciesFeatures_getRelation(const SubListOfSpeciesFeatures_t * slosf);


/**
 * Returns the value of the "relation" attribute of this
 * SubListOfSpeciesFeatures_t.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure whose relation is
 * sought.
 *
 * @return the value of the "relation" attribute of this
 * SubListOfSpeciesFeatures_t as a const char *.
 *
 * @copydetails doc_returned_unowned_char
 *
 * @copydetails doc_sublistofspeciesfeatures_relation
 * The possible values returned by this method are:
 * @li @c "and"
 * @li @c "or"
 * @li @c "not"
 * @li @c NULL
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
const char *
SubListOfSpeciesFeatures_getRelationAsString(const SubListOfSpeciesFeatures_t *
  slosf);


/**
 * Returns the value of the "component" attribute of this
 * SubListOfSpeciesFeatures_t.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure whose component is
 * sought.
 *
 * @return the value of the "component" attribute of this
 * SubListOfSpeciesFeatures_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
char *
SubListOfSpeciesFeatures_getComponent(const SubListOfSpeciesFeatures_t *
  slosf);


/**
 * Predicate returning @c 1 (true) if this SubListOfSpeciesFeatures_t's "id"
 * attribute is set.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure.
 *
 * @return @c 1 (true) if this SubListOfSpeciesFeatures_t's "id" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_isSetId(const SubListOfSpeciesFeatures_t * slosf);


/**
 * Predicate returning @c 1 (true) if this SubListOfSpeciesFeatures_t's "name"
 * attribute is set.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure.
 *
 * @return @c 1 (true) if this SubListOfSpeciesFeatures_t's "name" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_isSetName(const SubListOfSpeciesFeatures_t * slosf);


/**
 * Predicate returning @c 1 (true) if this SubListOfSpeciesFeatures_t's
 * "relation" attribute is set.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure.
 *
 * @return @c 1 (true) if this SubListOfSpeciesFeatures_t's "relation"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @copydetails doc_sublistofspeciesfeatures_relation
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_isSetRelation(const SubListOfSpeciesFeatures_t *
  slosf);


/**
 * Predicate returning @c 1 (true) if this SubListOfSpeciesFeatures_t's
 * "component" attribute is set.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure.
 *
 * @return @c 1 (true) if this SubListOfSpeciesFeatures_t's "component"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_isSetComponent(const SubListOfSpeciesFeatures_t *
  slosf);


/**
 * Sets the value of the "id" attribute of this SubListOfSpeciesFeatures_t.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling SubListOfSpeciesFeatures_unsetId().
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_setId(SubListOfSpeciesFeatures_t * slosf,
  const char * id);


/**
 * Sets the value of the "name" attribute of this SubListOfSpeciesFeatures_t.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling SubListOfSpeciesFeatures_unsetName().
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_setName(SubListOfSpeciesFeatures_t * slosf,
  const char * name);


/**
 * Sets the value of the "relation" attribute of this
 * SubListOfSpeciesFeatures_t.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure.
 *
 * @param relation Relation_t value of the "relation" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_sublistofspeciesfeatures_relation
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_setRelation(SubListOfSpeciesFeatures_t * slosf,
  Relation_t relation);


/**
 * Sets the value of the "relation" attribute of this
 * SubListOfSpeciesFeatures_t.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure.
 *
 * @param relation const char * of the "relation" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_sublistofspeciesfeatures_relation
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_setRelationAsString(
  SubListOfSpeciesFeatures_t *
  slosf,
  const char * relation);


/**
 * Sets the value of the "component" attribute of this
 * SubListOfSpeciesFeatures_t.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure.
 *
 * @param component const char * value of the "component" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_setComponent(SubListOfSpeciesFeatures_t * slosf,
  const char * component);


/**
 * Unsets the value of the "id" attribute of this SubListOfSpeciesFeatures_t.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_unsetId(SubListOfSpeciesFeatures_t * slosf);


/**
 * Unsets the value of the "name" attribute of this SubListOfSpeciesFeatures_t.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_unsetName(SubListOfSpeciesFeatures_t * slosf);


/**
 * Unsets the value of the "relation" attribute of this
 * SubListOfSpeciesFeatures_t.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @copydetails doc_sublistofspeciesfeatures_relation
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_unsetRelation(SubListOfSpeciesFeatures_t * slosf);


/**
 * Unsets the value of the "component" attribute of this
 * SubListOfSpeciesFeatures_t.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_unsetComponent(SubListOfSpeciesFeatures_t * slosf);


///*
// * Returns a ListOf_t * containing SpeciesFeature_t objects from this
// * SubListOfSpeciesFeatures_t.
// *
// * @param slosf the SubListOfSpeciesFeatures_t structure whose
// * ListOfSpeciesFeatures is sought.
// *
// * @return the ListOfSpeciesFeatures from this SubListOfSpeciesFeatures_t as a
// * ListOf_t *.
// *
// * @copydetails doc_returned_unowned_pointer
// *
// * @see SubListOfSpeciesFeatures_addSpeciesFeature()
// * @see SubListOfSpeciesFeatures_createSpeciesFeature()
// * @see SubListOfSpeciesFeatures_getSpeciesFeatureById()
// * @see SubListOfSpeciesFeatures_getSpeciesFeature()
// * @see SubListOfSpeciesFeatures_getNumSpeciesFeatures()
// * @see SubListOfSpeciesFeatures_removeSpeciesFeatureById()
// * @see SubListOfSpeciesFeatures_removeSpeciesFeature()
// *
// * @memberof SubListOfSpeciesFeatures_t
// */
//LIBSBML_EXTERN
//ListOf_t*
//SubListOfSpeciesFeatures_getListOfSpeciesFeatures(SubListOfSpeciesFeatures_t*
//  slosf);


/**
 * Get the number of SpeciesFeature_t objects in this
 * SubListOfSpeciesFeatures_t.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure to query.
 *
 * @return the number of SpeciesFeature_t objects in this
 * SubListOfSpeciesFeatures_t.
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
unsigned int
SubListOfSpeciesFeatures_getNumSpeciesFeatures(SubListOfSpeciesFeatures_t*
  slosf);


/**
 * Creates a new SpeciesFeature_t object, adds it to this
 * SubListOfSpeciesFeatures_t object and returns the SpeciesFeature_t object
 * created.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure to which the
 * SpeciesFeature_t should be added.
 *
 * @return a new SpeciesFeature_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
SpeciesFeature_t*
SubListOfSpeciesFeatures_createSpeciesFeature(SubListOfSpeciesFeatures_t*
  slosf);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * SubListOfSpeciesFeatures_t object have been set.
 *
 * @param slosf the SubListOfSpeciesFeatures_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * SubListOfSpeciesFeatures_t have been set, otherwise @c 0 (false) is
 * returned.
 *
 *
 * @note The required attributes for the SubListOfSpeciesFeatures_t object are:
 * @li "relation"
 *
 * @memberof SubListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_hasRequiredAttributes(const SubListOfSpeciesFeatures_t
  * slosf);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /* SubListOfSpeciesFeatures_H__ */
