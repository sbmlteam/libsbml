/**
 * @file:   SpeciesFeature.h
 * @brief:  Implementation of the SpeciesFeature class
 * @author: SBMLTeam
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
 * Copyright (C) 2009-2013 jointly by the following organizations:
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
 * @class SpeciesFeature
 * @sbmlbrief{multi} Defines a feature of a multi Species.
 *
 * Each SpeciesFeature object is a child of the MultiSpeciesPlugin, which
 * extends the Species.  Each defines one feature of the parent Species.  It
 * has three optional attributes, "id", "name" and "component", and two
 * required attributes, "speciesFeatureType" and "occur", and a required
 * child ListOfSpeciesFeatureValues. SpeciesFeature serves to define the
 * state of a component in a species by selecting values from the
 * ListOfPossibleSpeciesFeatureValues of the referenced SpeciesFeatureType.
 * Its "speciesFeatureType" attribue references the particular
 * SpeciesFeatureType of which this Species is an example.  The "occur"
 * attribute defines the number of instances of the referenced
 * SpeciesFeatureType.  The optional "component" attribute, of type SIdRef,
 * can be used to indicate which component of a Species the SpeciesFeature
 * belongs to, and is required when the component cannot be identified only
 * based on the speciesFeatureType attribute.  The ListOfSpeciesFeatureValues
 * contain one or more SpeciesFeatureValue objects&mdash;if more than one, the
 * relationship between them is "or", defining a list of mutually exclusive
 * possibilities.  Each SpeciesFeatureValue serves to specify a value for a
 * SpeciesFeature to select from the ListOfPossibleSpeciesFeatureValues
 * defined in the referenced SpeciesFeatureType.
 *
 * @class ListOfSpeciesFeatures
 * @sbmlbrief{multi} A list of SpeciesFeature objects.
 *
 * The ListOfSpeciesFeatures is a container for SpeciesFeature objects.
 *
 * @copydetails doc_what_is_listof
 *
 * @see SpeciesFeature
 */


#ifndef SpeciesFeature_H__
#define SpeciesFeature_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/multi/common/multifwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/multi/extension/MultiExtension.h>

#include <sbml/packages/multi/sbml/SpeciesFeatureValue.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class SubListOfSpeciesFeatures;

class LIBSBML_EXTERN SpeciesFeature : public SBase
{

protected:

  /** @cond doxygenLibsbmlInternal */

  ////  std::string   mId;
  std::string   mSpeciesFeatureType;
  unsigned int  mOccur;
  bool          mIsSetOccur;
  std::string   mComponent;
  ListOfSpeciesFeatureValues   mSpeciesFeatureValues;

  /** @endcond */


public:

  /**
   * Creates a new SpeciesFeature object.
   *
   * @param level the SBML Level.
   * @param version the Version within the SBML Level.
   * @param pkgVersion the version of the package.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SpeciesFeature(unsigned int level      = MultiExtension::getDefaultLevel(),
                 unsigned int version    = MultiExtension::getDefaultVersion(),
                 unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new SpeciesFeature with the given MultiPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param multins the MultiPkgNamespaces object
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  SpeciesFeature(MultiPkgNamespaces* multins);


   /**
   * Copy constructor for SpeciesFeature.
   *
   * @param orig the SpeciesFeature instance to copy.
   */
  SpeciesFeature(const SpeciesFeature& orig);


   /**
   * Assignment operator for SpeciesFeature.
   *
   * @param rhs the object whose values are used as the basis
   * of the assignment
   */
  SpeciesFeature& operator=(const SpeciesFeature& rhs);


   /**
   * Creates and returns a deep copy of this SpeciesFeature object.
   *
   * @return a (deep) copy of this SpeciesFeature object.
   */
  virtual SpeciesFeature* clone () const;


   /**
   * Destructor for SpeciesFeature.
   */
  virtual ~SpeciesFeature();


   /**
   * Returns the value of the "id" attribute of this SpeciesFeature.
   *
   * @return the value of the "id" attribute of this SpeciesFeature as a
   * string.
   */
  virtual const std::string& getId() const;


  /**
   * Returns @c true if this SpeciesFeature's "id" attribute has been set.
   *
   * @return @c true if this SpeciesFeature's "id" attribute has been set;
   * otherwise, @c false is returned.
   */
  virtual bool isSetId() const;


  /**
   * Sets the value of the "id" attribute of this SpeciesFeature.
   *
   * @param id const std::string& value of the "id" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setId(const std::string& id);


  /**
   * Unsets the value of the "id" attribute of this SpeciesFeature.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetId();


  /**
   * Returns the value of the "name" attribute of this SpeciesFeature.
   *
   * @return the value of the "name" attribute of this SpeciesFeature as a string.
   */
  virtual const std::string& getName() const;


  /**
   * Returns @c true if this SpeciesFeature's "name" attribute has been set.
   *
   * @return @c true if this SpeciesFeature's "name" attribute has been set;
   * otherwise, @c false is returned.
   */
  virtual bool isSetName() const;


  /**
   * Sets the value of the "name" attribute of this SpeciesFeature.
   *
   * @param name const std::string& value of the "name" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setName(const std::string& name);


  /**
   * Unsets the value of the "name" attribute of this SpeciesFeature.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetName();


  /**
   * Returns the value of the "speciesFeatureType" attribute of this
   * SpeciesFeature.
   *
   * @return the value of the "speciesFeatureType" attribute of this
   * SpeciesFeature as a string.
   */
  virtual const std::string& getSpeciesFeatureType() const;


  /**
   * Returns @c true if this SpeciesFeature's "speciesFeatureType" attribute
   * has been set.
   *
   * @return @c true if this SpeciesFeature's "speciesFeatureType" attribute
   * has been set; otherwise, @c false is returned.
   */
  virtual bool isSetSpeciesFeatureType() const;


  /**
   * Sets the value of the "speciesFeatureType" attribute of this SpeciesFeature.
   *
   * @param speciesFeatureType the new value for the "speciesFeatureType"
   * attribute.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setSpeciesFeatureType(const std::string& speciesFeatureType);


  /**
   * Unsets the value of the "speciesFeatureType" attribute of this SpeciesFeature.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetSpeciesFeatureType();


  /**
   * Returns the value of the "occur" attribute of this SpeciesFeature.
   *
   * @return the value of the "occur" attribute of this SpeciesFeature as a
   * unsigned integer.
   */
  virtual unsigned int getOccur() const;


  /**
   * Returns @c true if this SpeciesFeature's "occur" attribute has been set.
   *
   * @return @c true if this SpeciesFeature's "occur" attribute has been set;
   * otherwise, @c false is returned.
   */
  virtual bool isSetOccur() const;


  /**
   * Sets the value of the "occur" attribute of this SpeciesFeature.
   *
   * @param occur unsigned int value of the "occur" attribute to be set
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setOccur(unsigned int occur);


  /**
   * Unsets the value of the "occur" attribute of this SpeciesFeature.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetOccur();


  /**
   * Returns the value of the "component" attribute of this SpeciesFeature.
   *
   * @return the value of the "component" attribute of this SpeciesFeature as
   * a string.
   */
  virtual const std::string& getComponent() const;


  /**
   * Returns @c true if this SpeciesFeature's "component" attribute has been
   * set.
   *
   * @return @c true if this SpeciesFeature's "component" attribute has been
   * set; otherwise, @c false is returned.
   */
  virtual bool isSetComponent() const;


  /**
   * Sets the value of the "component" attribute of this SpeciesFeature.
   *
   * @param component the new value of the attribute.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setComponent(const std::string& component);


  /**
   * Unsets the value of the "component" attribute of this SpeciesFeature.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetComponent();


  /**
   * Returns the ListOfSpeciesFeatureValues in this SpeciesFeature object.
   *
   * @return the ListOfSpeciesFeatureValues child of this SpeciesFeature.
   */
  const ListOfSpeciesFeatureValues* getListOfSpeciesFeatureValues() const;


  /**
   * Returns the ListOfSpeciesFeatureValues in this SpeciesFeature object.
   *
   * @return the ListOfSpeciesFeatureValues child of this SpeciesFeature.
   */
  ListOfSpeciesFeatureValues* getListOfSpeciesFeatureValues();


  /**
   * Get the nth SpeciesFeatureValue object from the
   * ListOfSpeciesFeatureValues.
   *
   * @param n the index number of the SpeciesFeatureValue to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see getNumSpeciesFeatureValues()
   */
  SpeciesFeatureValue* getSpeciesFeatureValue(unsigned int n);


  /**
   * Get the nth SpeciesFeatureValue object from the
   * ListOfSpeciesFeatureValues.
   *
   * @param n the index number of the SpeciesFeatureValue to get.
   *
   * @return the nth object, or @c NULL if the index @p is out of range.
   *
   * @see getNumSpeciesFeatureValues()
   */
  const SpeciesFeatureValue* getSpeciesFeatureValue(unsigned int n) const;


  /**
   * Get the SpeciesFeatureValue object with the given identifier @p sid.
   *
   * @param sid a string representing the identifier of the
   * SpeciesFeatureValue to get.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see getSpeciesFeatureValue(unsigned int n)
   * @see getNumSpeciesFeatureValues()
   */
  SpeciesFeatureValue* getSpeciesFeatureValue(const std::string& sid);


  /**
   * Get the SpeciesFeatureValue object with the given identifier @p sid.
   *
   * @param sid a string representing the identifier of the
   * SpeciesFeatureValue to get.
   *
   * @return the object with the given id, or @c NULL if no such object exists.
   *
   * @see getSpeciesFeatureValue(unsigned int n)
   * @see getNumSpeciesFeatureValues()
   */
  const SpeciesFeatureValue* getSpeciesFeatureValue(const std::string& sid) const;


  /**
   * Adds a copy the given "SpeciesFeatureValue" to this SpeciesFeature.
   *
   * @param sfv the SpeciesFeatureValue object to add
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  int addSpeciesFeatureValue(const SpeciesFeatureValue* sfv);


  /**
   * Get the number of SpeciesFeatureValue objects in this SpeciesFeature.
   *
   * @return the number of SpeciesFeatureValue objects in this SpeciesFeature
   */
  unsigned int getNumSpeciesFeatureValues() const;


  /**
   * Creates a new SpeciesFeatureValue object and adds it to this
   * SpeciesFeatures ListOfSpeciesFeatureValues.
   *
   * @return a new SpeciesFeatureValue object instance
   *
   * @see addSpeciesFeatureValue(const SpeciesFeatureValue* sfv)
   */
  SpeciesFeatureValue* createSpeciesFeatureValue();


  /**
   * Removes the nth SpeciesFeatureValue from the ListOfSpeciesFeatureValues.
   *
   * @param n the index of the SpeciesFeatureValue to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   *
   * @see getNumSpeciesFeatureValues()
   */
  SpeciesFeatureValue* removeSpeciesFeatureValue(unsigned int n);


  /**
   * Removes the SpeciesFeatureValue object with the given identifier @p sid.
   *
   * @param sid the identifier of the SpeciesFeatureValue to remove.
   *
   * @return the object removed, or @c NULL if no such object exists.  Note that
   * the caller owns the returned object and is responsible for deleting it.
   */
  SpeciesFeatureValue* removeSpeciesFeatureValue(const std::string& sid);


  /**
   * Renames all the @c SIdRef attributes on this element, including any
   * found in MathML content (if such exists).
   *
   * This method works by looking at all attributes and (if appropriate)
   * mathematical formulas, comparing the identifiers to the value of @p
   * oldid.  If any matches are found, the matching identifiers are replaced
   * with @p newid.  The method does @em not descend into child elements.
   *
   * @param oldid the old identifier
   * @param newid the new identifier
   */
   virtual void renameSIdRefs(const std::string& oldid, const std::string& newid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitary depth.
   *
   * @param filter a pointer to an ElementFilter, which causes the function
   * to return only elements that match a particular set of constraints.
   * If NULL (the default), the function will return all child objects.
   *
   * @return a List of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the XML element name of this object.
   *
   * @return the name of this element, i.e. @c "speciesFeature".
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
   * Returns @c true if this SpeciesFeature object has all the required
   * attributes.
   *
   * @note The required attributes for a SpeciesFeature object are:
   * @li "speciesFeatureType"
   * @li "occur"
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const;


  /**
   * Returns @c true if SpeciesFeature has all the required subelements.
   *
   * @note A SpeciesFeature object has no required subelements.
   *
   * @return a boolean value indicating whether all the required
   * elements for this object have been defined.
   */
  virtual bool hasRequiredElements() const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Accepts the given SBMLVisitor.
   */
  virtual bool accept (SBMLVisitor& v) const;
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Sets the parent SBMLDocument.
   */
  virtual void setSBMLDocument (SBMLDocument* d);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Connects to child elements.
   */
  virtual void connectToChild ();
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Enables/Disables the given package with this element.
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
               const std::string& pkgPrefix, bool flag);
  /** @endcond */


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * return the SBML object corresponding to next XMLToken.
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Get the list of expected attributes for this element.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Read values from the given XMLAttributes set into their specific fields.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write values of XMLAttributes to the output stream.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;


  /** @endcond */



};

class LIBSBML_EXTERN ListOfSpeciesFeatures : public ListOf
{

public:

    /**
     * Creates a new ListOfSpeciesFeatures with the given level, version, and package version.
     *
     * @param level an unsigned int, the SBML Level to assign to this ListOfSpeciesFeatures
     *
     * @param version an unsigned int, the SBML Version to assign to this ListOfSpeciesFeatures
     *
     * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this ListOfSpeciesFeatures
     */
    ListOfSpeciesFeatures(unsigned int level      = MultiExtension::getDefaultLevel(),
			  unsigned int version    = MultiExtension::getDefaultVersion(),
			  unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


    /**
     * Creates a new ListOfSpeciesFeatures with the given MultiPkgNamespaces object.
     *
     * @param multins the MultiPkgNamespaces object
     */
    ListOfSpeciesFeatures(MultiPkgNamespaces* multins);


    /**
     * Creates and returns a deep copy of this ListOfSpeciesFeatures object
     *
     * @param orig the MultiPkgNamespaces object
     */
    ListOfSpeciesFeatures(const ListOfSpeciesFeatures& orig);


     /**
     * Creates and returns a deep copy of this ListOfSpeciesFeatures object.
     *
     * @return a (deep) copy of this ListOfSpeciesFeatures object.
     */
    virtual ListOfSpeciesFeatures* clone () const;


     /**
     * Destroys this ListOfSpeciesFeatures object.
     */
    virtual ~ListOfSpeciesFeatures();


    /**
    * Get a SpeciesFeature from the ListOfSpeciesFeatures.
    *
    * @param n the index number of the SpeciesFeature to get.
    *
    * @return the nth SpeciesFeature in this ListOfSpeciesFeatures.
    * If the index @p n is invalid, @c NULL is returned.
    *
    * @see size()
    */
   virtual SpeciesFeature* get(unsigned int n);


   /**
    * Get a SpeciesFeature from the ListOfSpeciesFeatures.
    *
    * @param n the index number of the SpeciesFeature to get.
    *
    * @return the nth SpeciesFeature in this ListOfSpeciesFeatures.
    * If the index @p n is invalid, @c NULL is returned.
    *
    * @see size()
    */
   virtual const SpeciesFeature* get(unsigned int n) const;


   /**
    * Get a SpeciesFeature from the ListOfSpeciesFeatures
    * based on its identifier.
    *
    * @param sid a string representing the identifier
    * of the SpeciesFeature to get.
    *
    * @return SpeciesFeature in this ListOfSpeciesFeatures
    * with the given id or @c NULL if no such
    * SpeciesFeature exists.
    *
    * @see get(unsigned int n)   *
    * @see size()
    */
   virtual SpeciesFeature* get(const std::string& sid);


   /**
    * Get a SpeciesFeature from the ListOfSpeciesFeatures
    * based on its identifier.
    *
    * @param sid a string representing the identifier
    * of the SpeciesFeature to get.
    *
    * @return SpeciesFeature in this ListOfSpeciesFeatures
    * with the given id or @c NULL if no such
    * SpeciesFeature exists.
    *
    * @see get(unsigned int n)   *
    * @see size()
    */
   virtual const SpeciesFeature* get(const std::string& sid) const;


   /**
    * Removes the nth SpeciesFeature from this ListOfSpeciesFeatures
    * and returns a pointer to it.
    *
    * The caller owns the returned item and is responsible for deleting it.
    *
    * @param n the index of the SpeciesFeature to remove.
    *
    * @see size()
    */
   virtual SpeciesFeature* remove(unsigned int n);


   /**
    * Removes the SpeciesFeature from this ListOfSpeciesFeatures with the given identifier
    * and returns a pointer to it.
    *
    * The caller owns the returned item and is responsible for deleting it.
    * If none of the items in this list have the identifier @p sid, then
    * @c NULL is returned.
    *
    * @param sid the identifier of the SpeciesFeature to remove.
    *
    * @return the SpeciesFeature removed. As mentioned above, the caller owns the
    * returned item.
    */
   virtual SpeciesFeature* remove(const std::string& sid);

   unsigned int getNumSpeciesFeatures() const;

   unsigned int size() const;

   /**
   * Get a SubListOfSpeciesFeatures from the ListOfSpeciesFeatures.
   *
   * @param n the index number of the SubListOfSpeciesFeatures to get.
   *
   * @return the nth SubListOfSpeciesFeatures in this ListOfSpeciesFeatures.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @see size()
   */
  virtual SubListOfSpeciesFeatures* getSubListOfSpeciesFeatures(unsigned int n);


  /**
   * Get a SubListOfSpeciesFeatures from the ListOfSpeciesFeatures.
   *
   * @param n the index number of the SubListOfSpeciesFeatures to get.
   *
   * @return the nth SubListOfSpeciesFeatures in this ListOfSpeciesFeatures.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @see size()
   */
  virtual const SubListOfSpeciesFeatures* getSubListOfSpeciesFeatures(unsigned int n) const;


  /**
   * Get a SubListOfSpeciesFeatures from the ListOfSpeciesFeatures
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SubListOfSpeciesFeatures to get.
   *
   * @return SubListOfSpeciesFeatures in this ListOfSpeciesFeatures
   * with the given id or @c NULL if no such
   * SubListOfSpeciesFeatures exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual SubListOfSpeciesFeatures* getSubListOfSpeciesFeatures(const std::string& sid);


  /**
   * Get a SubListOfSpeciesFeatures from the ListOfSpeciesFeatures
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SubListOfSpeciesFeatures to get.
   *
   * @return SubListOfSpeciesFeatures in this ListOfSpeciesFeatures
   * with the given id or @c NULL if no such
   * SubListOfSpeciesFeatures exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const SubListOfSpeciesFeatures* getSubListOfSpeciesFeatures(const std::string& sid) const;


  /**
   * Removes the nth SubListOfSpeciesFeatures from this ListOfSpeciesFeatures
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the SubListOfSpeciesFeatures to remove.
   *
   * @see size()
   */
  virtual SubListOfSpeciesFeatures* removeSubListOfSpeciesFeatures(unsigned int n);


  /**
   * Removes the SubListOfSpeciesFeatures from this ListOfSpeciesFeatures with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the SubListOfSpeciesFeatures to remove.
   *
   * @return the SubListOfSpeciesFeatures removed. As mentioned above, the caller owns the
   * returned item.
   */
  virtual SubListOfSpeciesFeatures* removeSubListOfSpeciesFeatures(const std::string& sid);


  int addSubListOfSpeciesFeatures(SubListOfSpeciesFeatures* losf);

  unsigned int getNumSubListOfSpeciesFeatures() const;


  /**
   * Returns the XML element name of this object, which for ListOfSpeciesFeatures, is
   * always @c "listOfSpeciesFeatures".
   *
   * @return the name of this element, i.e. @c "listOfSpeciesFeatures".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for this SBML object.
   * 
   * @if clike LibSBML attaches an identifying code to every kind of SBML
   * object.  These are known as <em>SBML type codes</em>.  The set of
   * possible type codes is defined in the enumeration #SBMLTypeCode_t.
   * The names of the type codes all begin with the characters
   * @c SBML_. @endif@if java LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.  In
   * other languages, the set of type codes is stored in an enumeration; in
   * the Java language interface for libSBML, the type codes are defined as
   * static integer constants in the interface class {@link
   * libsbmlConstants}.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if python LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the Python language interface for libSBML, the type
   * codes are defined as static integer constants in the interface class
   * @link libsbml@endlink.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if csharp LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the C# language interface for libSBML, the type codes
   * are defined as static integer constants in the interface class @link
   * libsbmlcs.libsbml@endlink.  The names of the type codes all begin with
   * the characters @c SBML_. @endif
   *
   * @return the SBML type code for this object, or
   * @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual int getTypeCode () const;


  /**
   * Returns the libSBML type code for the SBML objects
   * contained in this ListOf object
   * 
   * @if clike LibSBML attaches an identifying code to every kind of SBML
   * object.  These are known as <em>SBML type codes</em>.  The set of
   * possible type codes is defined in the enumeration #SBMLTypeCode_t.
   * The names of the type codes all begin with the characters
   * @c SBML_. @endif@if java LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.  In
   * other languages, the set of type codes is stored in an enumeration; in
   * the Java language interface for libSBML, the type codes are defined as
   * static integer constants in the interface class {@link
   * libsbmlConstants}.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if python LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the Python language interface for libSBML, the type
   * codes are defined as static integer constants in the interface class
   * @link libsbml@endlink.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if csharp LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the C# language interface for libSBML, the type codes
   * are defined as static integer constants in the interface class @link
   * libsbmlcs.libsbml@endlink.  The names of the type codes all begin with
   * the characters @c SBML_. @endif
   *
   * @return the SBML type code for the objects in this ListOf instance, or
   * @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual int getItemTypeCode () const;

  /** @cond doxygenLibsbmlInternal */

  /**
   * Connects to child elements.
   */
  virtual void connectToChild ();


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


  /** @cond doxygenLibsbmlInternal */

  List * mSubListOfSpeciesFeatures;

  /** @endcond */

};



LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS



/**
 * Creates a new SpeciesFeature_t using the given SBML Level, Version and
 * &ldquo;multi&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * SpeciesFeature_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * SpeciesFeature_t.
 *
 * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this
 * SpeciesFeature_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
SpeciesFeature_t *
SpeciesFeature_create(unsigned int level, unsigned int version,
                      unsigned int pkgVersion);


/**
 * Frees this SpeciesFeature_t object.
 *
 * @param sf the SpeciesFeature_t structure.
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
void
SpeciesFeature_free(SpeciesFeature_t* sf);


/**
 * Creates and returns a deep copy of this SpeciesFeature_t object.
 *
 * @param sf the SpeciesFeature_t structure.
 *
 * @return a (deep) copy of this SpeciesFeature_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
SpeciesFeature_t*
SpeciesFeature_clone(const SpeciesFeature_t* sf);


/**
 * Returns the value of the "id" attribute of this SpeciesFeature_t.
 *
 * @param sf the SpeciesFeature_t structure whose id is sought.
 *
 * @return the value of the "id" attribute of this SpeciesFeature_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
char *
SpeciesFeature_getId(const SpeciesFeature_t * sf);


/**
 * Returns the value of the "name" attribute of this SpeciesFeature_t.
 *
 * @param sf the SpeciesFeature_t structure whose name is sought.
 *
 * @return the value of the "name" attribute of this SpeciesFeature_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
char *
SpeciesFeature_getName(const SpeciesFeature_t * sf);


/**
 * Returns the value of the "speciesFeatureType" attribute of this
 * SpeciesFeature_t.
 *
 * @param sf the SpeciesFeature_t structure whose speciesFeatureType is sought.
 *
 * @return the value of the "speciesFeatureType" attribute of this
 * SpeciesFeature_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
char *
SpeciesFeature_getSpeciesFeatureType(const SpeciesFeature_t * sf);


/**
 * Returns the value of the "occur" attribute of this SpeciesFeature_t.
 *
 * @param sf the SpeciesFeature_t structure whose occur is sought.
 *
 * @return the value of the "occur" attribute of this SpeciesFeature_t as a
 * unsigned integer.
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
unsigned int
SpeciesFeature_getOccur(const SpeciesFeature_t * sf);


/**
 * Returns the value of the "component" attribute of this SpeciesFeature_t.
 *
 * @param sf the SpeciesFeature_t structure whose component is sought.
 *
 * @return the value of the "component" attribute of this SpeciesFeature_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
char *
SpeciesFeature_getComponent(const SpeciesFeature_t * sf);


/**
 * Predicate returning @c 1 (true) if this SpeciesFeature_t's "id" attribute is
 * set.
 *
 * @param sf the SpeciesFeature_t structure.
 *
 * @return @c 1 (true) if this SpeciesFeature_t's "id" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
int
SpeciesFeature_isSetId(const SpeciesFeature_t * sf);


/**
 * Predicate returning @c 1 (true) if this SpeciesFeature_t's "name" attribute
 * is set.
 *
 * @param sf the SpeciesFeature_t structure.
 *
 * @return @c 1 (true) if this SpeciesFeature_t's "name" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
int
SpeciesFeature_isSetName(const SpeciesFeature_t * sf);


/**
 * Predicate returning @c 1 (true) if this SpeciesFeature_t's
 * "speciesFeatureType" attribute is set.
 *
 * @param sf the SpeciesFeature_t structure.
 *
 * @return @c 1 (true) if this SpeciesFeature_t's "speciesFeatureType"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
int
SpeciesFeature_isSetSpeciesFeatureType(const SpeciesFeature_t * sf);


/**
 * Predicate returning @c 1 (true) if this SpeciesFeature_t's "occur" attribute
 * is set.
 *
 * @param sf the SpeciesFeature_t structure.
 *
 * @return @c 1 (true) if this SpeciesFeature_t's "occur" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
int
SpeciesFeature_isSetOccur(const SpeciesFeature_t * sf);


/**
 * Predicate returning @c 1 (true) if this SpeciesFeature_t's "component"
 * attribute is set.
 *
 * @param sf the SpeciesFeature_t structure.
 *
 * @return @c 1 (true) if this SpeciesFeature_t's "component" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
int
SpeciesFeature_isSetComponent(const SpeciesFeature_t * sf);


/**
 * Sets the value of the "id" attribute of this SpeciesFeature_t.
 *
 * @param sf the SpeciesFeature_t structure.
 *
 * @param id const char * value of the "id" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p id = @c NULL or an empty string is equivalent
 * to calling SpeciesFeature_unsetId().
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
int
SpeciesFeature_setId(SpeciesFeature_t * sf, const char * id);


/**
 * Sets the value of the "name" attribute of this SpeciesFeature_t.
 *
 * @param sf the SpeciesFeature_t structure.
 *
 * @param name const char * value of the "name" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * Calling this function with @p name = @c NULL or an empty string is
 * equivalent to calling SpeciesFeature_unsetName().
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
int
SpeciesFeature_setName(SpeciesFeature_t * sf, const char * name);


/**
 * Sets the value of the "speciesFeatureType" attribute of this
 * SpeciesFeature_t.
 *
 * @param sf the SpeciesFeature_t structure.
 *
 * @param speciesFeatureType const char * value of the "speciesFeatureType"
 * attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
int
SpeciesFeature_setSpeciesFeatureType(SpeciesFeature_t * sf, const char * speciesFeatureType);


/**
 * Sets the value of the "occur" attribute of this SpeciesFeature_t.
 *
 * @param sf the SpeciesFeature_t structure.
 *
 * @param occur unsigned int value of the "occur" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
int
SpeciesFeature_setOccur(SpeciesFeature_t * sf, unsigned int occur);


/**
 * Sets the value of the "component" attribute of this SpeciesFeature_t.
 *
 * @param sf the SpeciesFeature_t structure.
 *
 * @param component const char * value of the "component" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
int
SpeciesFeature_setComponent(SpeciesFeature_t * sf, const char * component);


/**
 * Unsets the value of the "id" attribute of this SpeciesFeature_t.
 *
 * @param sf the SpeciesFeature_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
int
SpeciesFeature_unsetId(SpeciesFeature_t * sf);


/**
 * Unsets the value of the "name" attribute of this SpeciesFeature_t.
 *
 * @param sf the SpeciesFeature_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
int
SpeciesFeature_unsetName(SpeciesFeature_t * sf);


/**
 * Unsets the value of the "speciesFeatureType" attribute of this
 * SpeciesFeature_t.
 *
 * @param sf the SpeciesFeature_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
int
SpeciesFeature_unsetSpeciesFeatureType(SpeciesFeature_t * sf);


/**
 * Unsets the value of the "occur" attribute of this SpeciesFeature_t.
 *
 * @param sf the SpeciesFeature_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
int
SpeciesFeature_unsetOccur(SpeciesFeature_t * sf);


/**
 * Unsets the value of the "component" attribute of this SpeciesFeature_t.
 *
 * @param sf the SpeciesFeature_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
int
SpeciesFeature_unsetComponent(SpeciesFeature_t * sf);


/**
 * Adds a copy of the given SpeciesFeatureValue_t to this SpeciesFeature_t.
 *
 * @param sf the SpeciesFeature_t structure to which the SpeciesFeatureValue_t
 * should be added.
 *
 * @param sfv the SpeciesFeatureValue_t object to add.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_LEVEL_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_PKG_VERSION_MISMATCH, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_DUPLICATE_OBJECT_ID, OperationReturnValues_t}
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
int
SpeciesFeature_addSpeciesFeatureValue(SpeciesFeature_t* sf, const SpeciesFeatureValue_t* sfv);


/**
 * Creates a new SpeciesFeatureValue_t object, adds it to this SpeciesFeature_t
 * object and returns the SpeciesFeatureValue_t object created.
 *
 * @param sf the SpeciesFeature_t structure to which the SpeciesFeatureValue_t
 * should be added.
 *
 * @return a new SpeciesFeatureValue_t object instance.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
SpeciesFeatureValue_t*
SpeciesFeature_createSpeciesFeatureValue(SpeciesFeature_t* sf);


/**
 * Returns a ListOf_t * containing SpeciesFeatureValue_t objects from this
 * SpeciesFeature_t.
 *
 * @param sf the SpeciesFeature_t structure whose ListOfSpeciesFeatureValues is
 * sought.
 *
 * @return the ListOfSpeciesFeatureValues from this SpeciesFeature_t as a
 * ListOf_t *.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @see SpeciesFeature_addSpeciesFeatureValue()
 * @see SpeciesFeature_createSpeciesFeatureValue()
 * @see SpeciesFeature_getSpeciesFeatureValue()
 * @see SpeciesFeature_getNumSpeciesFeatureValues()
 * @see SpeciesFeature_removeSpeciesFeatureValue()
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
ListOf_t*
SpeciesFeature_getListOfSpeciesFeatureValues(SpeciesFeature_t* sf);


/**
 * Get a SpeciesFeatureValue_t from the SpeciesFeature_t.
 *
 * @param sf the SpeciesFeature_t structure to search.
 *
 * @param n an unsigned int representing the index of the SpeciesFeatureValue_t
 * to retrieve.
 *
 * @return the nth SpeciesFeatureValue_t in the ListOfSpeciesFeatureValues
 * within this SpeciesFeature.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
SpeciesFeatureValue_t*
SpeciesFeature_getSpeciesFeatureValue(SpeciesFeature_t* sf, unsigned int n);


/**
 * Get the number of SpeciesFeatureValue_t objects in this SpeciesFeature_t.
 *
 * @param sf the SpeciesFeature_t structure to query.
 *
 * @return the number of SpeciesFeatureValue_t objects in this
 * SpeciesFeature_t.
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
unsigned int
SpeciesFeature_getNumSpeciesFeatureValues(SpeciesFeature_t* sf);


/**
 * Removes the nth SpeciesFeatureValue_t from this SpeciesFeature_t and returns
 * a pointer to it.
 *
 * @param sf the SpeciesFeature_t structure to search.
 *
 * @param n an unsigned int representing the index of the SpeciesFeatureValue_t
 * to remove.
 *
 * @return a pointer to the nth SpeciesFeatureValue_t in this SpeciesFeature_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
SpeciesFeatureValue_t*
SpeciesFeature_removeSpeciesFeatureValue(SpeciesFeature_t* sf, unsigned int n);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * SpeciesFeature_t object have been set.
 *
 * @param sf the SpeciesFeature_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * SpeciesFeature_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the SpeciesFeature_t object are:
 * @li "speciesFeatureType"
 * @li "occur"
 *
 * @memberof SpeciesFeature_t
 */
LIBSBML_EXTERN
int
SpeciesFeature_hasRequiredAttributes(const SpeciesFeature_t * sf);




/**
 * Return the structure indicated by the given @p sid.
 *
 * @param lo the ListOf_t structure to use.
 *
 * @param sid a string matching the "id" attribute of the element sought.
 *
 * @return the structure for the given variable, or @c NULL if no such
 * object exists in the list.
 *
 * @memberof ListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
SpeciesFeature_t *
ListOfSpeciesFeatures_getById(ListOf_t * lo, const char * sid);


/**
 * Removes the structure with the given @p sid
 * from the given list and returns a pointer to it.
 *
 * The caller owns the returned structure and is responsible for deleting it.
 *
 * @param lo the ListOf_t structure.
 * @param sid a string matching the "id" attribute of the element sought.
 *
 * @return the structure removed.  As mentioned above, the
 * caller owns the returned structure. @c NULL is returned if no
 * structure with the "id" attribute exists in the given list.
 *
 * @memberof ListOfSpeciesFeatures_t
 */
LIBSBML_EXTERN
SpeciesFeature_t *
ListOfSpeciesFeatures_removeById(ListOf_t * lo, const char * sid);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /*  !SWIG  */

#endif /*  SpeciesFeature_H__  */

