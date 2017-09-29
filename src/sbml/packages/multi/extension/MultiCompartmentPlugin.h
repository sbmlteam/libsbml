/**
 * @file:   MultiCompartmentPlugin.h
 * @brief:  Implementation of the MultiCompartmentPlugin class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 * @class MultiCompartmentPlugin
 * @sbmlbrief{multi} Extension of Compartment for the "multi" package.
 *
 * The MultiCompartmentPlugin object is used to extend the standard SBML
 * Compartment base object with an optional "compartmentType" attribute, a
 * required "isType" Boolean attribute, and a child
 * ListOfCompartmentReferences.  The "isType" attribute flags whether this
 * Compartment should be treated as a standard SBML Compartment (if @c false),
 * or as a more generic rule-based "type" of Compartment (if @c true).  A
 * compartment "type" is a template (in the sense of prototype) for all
 * Compartment objects referencing it (via "compartmentType" attributes). A
 * Species object directly referencing a compartment type is not a "fully
 * defined" species, but rather a "template" species.  If the value of the
 * "isType" attribute is @c false, the Compartment object is a "not-a-type"
 * compartment, and it is similar to a SBML core Compartment except it can
 * reference a compartment type and can have a ListOfCompartmentReferences
 * child.  Each child CompartmentReference in the ListOfCompartmentReferences
 * defines a subcompartment of the parent Compartment.  The "compartmentType"
 * attribute identifies this Compartment as the CompartmentType defined
 * elsewhere in the Model.  If the "compartmentType" attribute is set, the
 * "isType" attribute must be @c true.
 */

#ifndef MultiCompartmentPlugin_H__
#define MultiCompartmentPlugin_H__


#include <sbml/common/extern.h>


#ifdef __cplusplus


#include <string>

#include <sbml/extension/SBasePlugin.h>
#include <sbml/packages/multi/sbml/CompartmentReference.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN MultiCompartmentPlugin : public SBasePlugin
{
public:

  /**
   * Creates a new MultiCompartmentPlugin object.
   *
   * @copydetails doc_what_are_xmlnamespaces
   *
   * @copydetails doc_what_are_sbmlnamespaces
   *
   * @param uri the URI of the SBML Level&nbsp;3 package implemented by
   * this libSBML package extension.
   *
   * @param prefix the XML namespace prefix being used for the package.
   *
   * @param multins the namespaces object for the package.
   */
  MultiCompartmentPlugin(const std::string& uri, const std::string& prefix,
                         MultiPkgNamespaces* multins);


  /**
   * Copy constructor for MultiCompartmentPlugin.
   *
   * @param orig the MultiCompartmentPlugin instance to copy.
   */
  MultiCompartmentPlugin(const MultiCompartmentPlugin& orig);


  /**
   * Assignment operator for MultiCompartmentPlugin.
   *
   * @param rhs the object whose values are used as the basis
   * of the assignment
   */
  MultiCompartmentPlugin& operator=(const MultiCompartmentPlugin& rhs);


   /**
   * Creates and returns a deep copy of this MultiCompartmentPlugin object.
   *
   * @return a (deep) copy of this MultiCompartmentPlugin object.
   */
  virtual MultiCompartmentPlugin* clone () const;


   /**
   * Destructor for MultiCompartmentPlugin.
   */
  virtual ~MultiCompartmentPlugin();


  //---------------------------------------------------------------
  //
  // overridden virtual functions for read/write/check
  //
  //---------------------------------------------------------------

  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses must override this method to create, store, and then
   * return an SBML object corresponding to the next XMLToken in the
   * XMLInputStream if they have their specific elements.
   *
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or @c NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses must override this method to write out their contained
   * SBML objects as XML elements if they have their specific elements.
   */
  virtual void writeElements (XMLOutputStream& stream) const;
  /** @endcond */


  /**
   * Returns @c true if this object has all the required elements.
   *
   * @return @c true if this object has all the elements required by the
   * package specification; otherwise, @c false will be returned.
   */
  virtual bool hasRequiredElements () const;


  //---------------------------------------------------------------


  //---------------------------------------------------------------
  //
  // Functions for interacting with the members of the plugin
  //
  //---------------------------------------------------------------


  /**
   * Returns the value of the "compartmentType" attribute of this compartment.
   *
   * @return the "compartmentType" attribute value.
   */
  virtual const std::string& getCompartmentType() const;


  /**
   * Returns @c true if this compartment's "compartmentType" attribute has
   * been set.
   *
   * @return @c true if the "compartmentType" attribute has been set;
   * otherwise, @c false is returned.
   */
  virtual bool isSetCompartmentType() const;


  /**
   * Sets the value of the "compartmentType" attribute on this compartment.
   *
   * @param compartmentType the new value for the attribute.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setCompartmentType(const std::string& compartmentType);


  /**
   * Unsets the value of the "compartmentType" attribute.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetCompartmentType();


  /**
   * Returns the value of the "isType" attribute.
   *
   * @return the value of the "isType" attribute.
   */
  virtual bool getIsType() const;


  /**
   * Returns @c true if this compartment's "isType" attribute has been set.
   *
   * @return @c true if the "isType" attribute has been set; otherwise,
   * @c false is returned.
   */
  virtual bool isSetIsType() const;


  /**
   * Sets the value of the "isType" attribute.
   *
   * @param isType the new value of the attribute.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setIsType(bool isType);


  /**
   * Unsets the value of the "isType" attribute.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetIsType();


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitary depth.
   *
   * @param filter a pointer to an ElementFilter, which causes the function
   * to return only elements that match a particular set of constraints.
   * If NULL (the default), the function will return all child objects.
   *
   * @return a List* of pointers to all child objects.
   */
   virtual List* getAllElements(ElementFilter * filter = NULL);


  /**
   * Returns the ListOfCompartmentReferences object.
   *
   * @return the ListOfCompartmentReferences object within this
   * "multi" Compartment object.
   */
  const ListOfCompartmentReferences* getListOfCompartmentReferences () const;


  /**
   * Returns the ListOfCompartmentReferences object.
   *
   * @return the ListOfCompartmentReferences object within this "multi"
   * Compartment.
   */
  ListOfCompartmentReferences* getListOfCompartmentReferences ();


  /**
   * Returns the nth CompartmentReference object from the
   * ListOfCompartmentReferences.
   *
   * @param n the index number of the CompartmentReference to get.
   *
   * @return the nth CompartmentReference, or @c NULL if the index @p n is out
   * of range.
   */
  const CompartmentReference* getCompartmentReference(unsigned int n) const;


  /**
   * Returns the nth CompartmentReference object from the
   * ListOfCompartmentReferences.
   *
   * @param n the index number of the CompartmentReference to get.
   *
   * @return the nth CompartmentReference, or @c NULL if the index @p n is out
   * of range.
   */
  CompartmentReference* getCompartmentReference(unsigned int n);


  /**
   * Returns the CompartmentReference with the given identifier @p sid.
   *
   * @param sid the identifier of the CompartmentReference object to get from
   * the ListOfCompartmentReferences.
   *
   * @return the CompartmentReference object with the given identifier in the
   * ListOfCompartmentReferences, or @c NULL if no such CompartmentReference
   * exists.
   */
  const CompartmentReference* getCompartmentReference(const std::string& sid) const;


  /**
   * Returns the CompartmentReference with the given identifier @p sid.
   *
   * @param sid the identifier of the CompartmentReference object to get from
   * the ListOfCompartmentReferences.
   *
   * @return the CompartmentReference object with the given identifier in the
   * ListOfCompartmentReferences, or @c NULL if no such CompartmentReference
   * exists.
   */
  CompartmentReference* getCompartmentReference(const std::string& sid);


  /**
   * Adds a copy of the given CompartmentReference object to the
   * ListOfCompartmentReferences.
   *
   * @param compartmentReference the CompartmentReference object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int addCompartmentReference (const CompartmentReference* compartmentReference);


  /**
   * Creates a new CompartmentReference object and adds it to the
   * ListOfCompartmentReferences.
   *
   * @return the newly created CompartmentReference object.
   */
  CompartmentReference* createCompartmentReference ();


  /**
   * Removes the nth CompartmentReference object from the
   * ListOfCompartmentReferences.
   *
   * @param n the index of the CompartmentReference to remove.
   *
   * @return the CompartmentReference object removed, or @c NULL if the given
   * index @p n is out of range.  Note that the caller owns the returned
   * object and is responsible for deleting it.
   */
  CompartmentReference* removeCompartmentReference(unsigned int n);


  /**
   * Removes the CompartmentReference object with the given identifier from
   * the ListOfCompartmentReferences.
   *
   * @param sid the id of the CompartmentReference to remove.
   *
   * @return the CompartmentReference object removed, or @c NULL if no such
   * CompartmentReference exists.  Note that the caller owns the returned
   * object and is responsible for deleting it.
   */
  CompartmentReference* removeCompartmentReference(const std::string& sid);


  /**
   * Returns the number of CompartmentReference objects in the
   * ListOfCompartmentReferences.
   *
   * @return the number of CompartmentReference objects.
   */
  unsigned int getNumCompartmentReferences () const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * Sets the parent SBMLDocument.
   */
  virtual void setSBMLDocument (SBMLDocument* d);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  virtual void connectToParent (SBase* sbase);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix, bool flag);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  virtual bool accept (SBMLVisitor& v) const;
  /** @endcond */


protected:

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

  /** @cond doxygenLibsbmlInternal */


  ListOfCompartmentReferences mListOfCompartmentReferences;

  std::string   mCompartmentType;
  bool          mIsType;
  bool          mIsSetIsType;

  /** @endcond */


};




LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */
#endif /* MultiCompartmentPlugin_H__ */


