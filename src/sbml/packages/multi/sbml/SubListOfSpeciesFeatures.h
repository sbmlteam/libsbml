/**
 * @file:   SubListOfSpeciesFeatures.h
 * @brief:  Implementation of the SubListOfSpeciesFeatures class
 * @author: Fengkai Zhang
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2017 jointly by the following organizations:
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

#ifndef SWIG

#endif  /*  !SWIG  */

/**
* @memberof SubListOfSpeciesFeatures_t
*/
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_isValidRelation(Relation_t relation);


/**
* @memberof SubListOfSpeciesFeatures_t
*/
LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_isValidRelationString(const char* s);

/**
* @memberof SubListOfSpeciesFeatures_t
*/
LIBSBML_EXTERN
const char*
Relation_toString(Relation_t relation);


/**
* @memberof SubListOfSpeciesFeatures_t
*/
LIBSBML_EXTERN
Relation_t
Relation_fromString(const char* s);

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END




#endif /* SubListOfSpeciesFeatures_H__ */
