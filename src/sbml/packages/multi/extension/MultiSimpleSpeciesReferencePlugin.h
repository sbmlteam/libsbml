/**
 * @file:   MultiSimpleSpeciesReferencePlugin.h
 * @brief:  Implementation of the MultiSimpleSpeciesReferencePlugin class
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
 * @class MultiSimpleSpeciesReferencePlugin
 * @sbmlbrief{multi} Extension of SimpleSpeciesReference for the "multi" package.
 *
 * The MultiSpeciesPlugin class extends the SimpleSpeciesReference class with
 * a new optional attribute "compartmentReference", of type SIdRef, that
 * points to a CompartmentReference.  The compartmentReference attribute can
 * serve to indicate in which subcompartment the SpeciesReference or
 * ModifierSpeciesReference (which inherit from SimpleSpeciesReference) is
 * located.
 */

#ifndef MultiSimpleSpeciesReferencePlugin_H__
#define MultiSimpleSpeciesReferencePlugin_H__


#include <sbml/common/extern.h>
#include <sbml/packages/multi/common/multifwd.h>


#ifdef __cplusplus


#include <sbml/extension/SBasePlugin.h>
#include <sbml/packages/multi/extension/MultiExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN MultiSimpleSpeciesReferencePlugin : public SBasePlugin
{
public:

  /**
   * Creates a new MultiSimpleSpeciesReferencePlugin object.
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
  MultiSimpleSpeciesReferencePlugin(const std::string& uri, const std::string& prefix,
                                    MultiPkgNamespaces* multins);


  /**
   * Copy constructor for MultiSimpleSpeciesReferencePlugin.
   *
   * @param orig the MultiSimpleSpeciesReferencePlugin instance to copy.
   */
  MultiSimpleSpeciesReferencePlugin(const MultiSimpleSpeciesReferencePlugin& orig);


   /**
   * Assignment operator for MultiSimpleSpeciesReferencePlugin.
   *
   * @param rhs the object whose values are used as the basis
   * of the assignment
   */
  MultiSimpleSpeciesReferencePlugin& operator=(const MultiSimpleSpeciesReferencePlugin& rhs);


   /**
   * Creates and returns a deep copy of this MultiSimpleSpeciesReferencePlugin object.
   *
   * @return a (deep) copy of this MultiSimpleSpeciesReferencePlugin object.
   */
  virtual MultiSimpleSpeciesReferencePlugin* clone () const;


   /**
   * Destructor for MultiSimpleSpeciesReferencePlugin.
   */
  virtual ~MultiSimpleSpeciesReferencePlugin();


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
   * Returns the value of the "compartmentReference" attribute of this
   * SimpleSpeciesReference.
   *
   * @return the value of the "compartmentReference" attribute of this
   * SimpleSpeciesReference, as a string.
   */
  virtual const std::string& getCompartmentReference() const;


  /**
   * Returns @c true if this SimpleSpeciesReference's "compartmentReference"
   * attribute has been set.
   *
   * @return @c true if this SimpleSpeciesReference's "compartmentReference"
   * attribute has been set; otherwise, @c false is returned.
   */
  virtual bool isSetCompartmentReference() const;


  /**
   * Sets the value of the "compartmentReference" attribute of this
   * SimpleSpeciesReference.
   *
   * @param compartmentReference the new value of the "compartmentReference"
   * attribute.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
   */
  virtual int setCompartmentReference(const std::string& compartmentReference);


  /**
   * Unsets the value of the "compartmentReference" attribute of this
   * SimpleSpeciesReference.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetCompartmentReference();


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

  std::string   mCompartmentReference;

  /** @endcond */


};




LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Returns the value of the "compartmentReference" attribute of this
 * MultiSimpleSpeciesReferencePlugin_t.
 *
 * @param mssrp the MultiSimpleSpeciesReferencePlugin_t structure whose
 * compartmentReference is sought.
 *
 * @return the value of the "compartmentReference" attribute of this
 * MultiSimpleSpeciesReferencePlugin_t as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof MultiSimpleSpeciesReferencePlugin_t
 */
LIBSBML_EXTERN
char *
MultiSimpleSpeciesReferencePlugin_getCompartmentReference(const
  MultiSimpleSpeciesReferencePlugin_t * mssrp);


/**
 * Predicate returning @c 1 (true) if this
 * MultiSimpleSpeciesReferencePlugin_t's "compartmentReference" attribute is
 * set.
 *
 * @param mssrp the MultiSimpleSpeciesReferencePlugin_t structure.
 *
 * @return @c 1 (true) if this MultiSimpleSpeciesReferencePlugin_t's
 * "compartmentReference" attribute has been set, otherwise @c 0 (false) is
 * returned.
 *
 * @memberof MultiSimpleSpeciesReferencePlugin_t
 */
LIBSBML_EXTERN
int
MultiSimpleSpeciesReferencePlugin_isSetCompartmentReference(const
  MultiSimpleSpeciesReferencePlugin_t * mssrp);


/**
 * Sets the value of the "compartmentReference" attribute of this
 * MultiSimpleSpeciesReferencePlugin_t.
 *
 * @param mssrp the MultiSimpleSpeciesReferencePlugin_t structure.
 *
 * @param compartmentReference const char * value of the "compartmentReference"
 * attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof MultiSimpleSpeciesReferencePlugin_t
 */
LIBSBML_EXTERN
int
MultiSimpleSpeciesReferencePlugin_setCompartmentReference(
  MultiSimpleSpeciesReferencePlugin_t * mssrp,
  const char *
  compartmentReference);


/**
 * Unsets the value of the "compartmentReference" attribute of this
 * MultiSimpleSpeciesReferencePlugin_t.
 *
 * @param mssrp the MultiSimpleSpeciesReferencePlugin_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof MultiSimpleSpeciesReferencePlugin_t
 */
LIBSBML_EXTERN
int
MultiSimpleSpeciesReferencePlugin_unsetCompartmentReference(MultiSimpleSpeciesReferencePlugin_t
  * mssrp);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !MultiSimpleSpeciesReferencePlugin_H__ */
