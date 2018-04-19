/**
 * @file DistribFunctionDefinitionPlugin.h
 * @brief Definition of the DistribFunctionDefinitionPlugin class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class DistribFunctionDefinitionPlugin
 * @sbmlbrief{distrib} Extension of FunctionDefinition.
 */


#ifndef DistribFunctionDefinitionPlugin_H__
#define DistribFunctionDefinitionPlugin_H__


#include <sbml/common/extern.h>


#ifdef __cplusplus


#include <sbml/extension/SBasePlugin.h>
#include <sbml/packages/distrib/sbml/DistribDrawFromDistribution.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribFunctionDefinitionPlugin : public SBasePlugin
{
protected:

  /** @cond doxygenLibsbmlInternal */

  DistribDrawFromDistribution* mDistribDrawFromDistribution;

  /** @endcond */

public:

  /**
   * Creates a new DistribFunctionDefinitionPlugin using the given URI, prefix
   * and package namespace.
   *
   * @param uri a string, representing the URI of the SBML Level&nbsp;3 package
   * implemented by this libSBML package extension.
   *
   * @param prefix a string, the XML namespace prefix being used for this
   * package.
   *
   * @param distribns a pointer to the namesspaces object
   * (DistribPkgNamespaces) for this package.
   *
   * @copydetails doc_what_are_xmlnamespaces
   *
   * @copydetails doc_what_are_sbmlnamespaces
   */
  DistribFunctionDefinitionPlugin(const std::string& uri,
                                  const std::string& prefix,
                                  DistribPkgNamespaces* distribns);


  /**
   * Copy constructor for DistribFunctionDefinitionPlugin.
   *
   * @param orig the DistribFunctionDefinitionPlugin instance to copy.
   */
  DistribFunctionDefinitionPlugin(const DistribFunctionDefinitionPlugin& orig);


  /**
   * Assignment operator for DistribFunctionDefinitionPlugin.
   *
   * @param rhs the DistribFunctionDefinitionPlugin object whose values are to
   * be used as the basis of the assignment.
   */
  DistribFunctionDefinitionPlugin& operator=(const
    DistribFunctionDefinitionPlugin& rhs);


  /**
   * Creates and returns a deep copy of this DistribFunctionDefinitionPlugin
   * object.
   *
   * @return a (deep) copy of this DistribFunctionDefinitionPlugin object.
   */
  virtual DistribFunctionDefinitionPlugin* clone() const;


  /**
   * Destructor for DistribFunctionDefinitionPlugin.
   */
  virtual ~DistribFunctionDefinitionPlugin();


  /**
   * Returns the value of the "distribDrawFromDistribution" element of this
   * DistribFunctionDefinitionPlugin.
   *
   * @return the value of the "distribDrawFromDistribution" element of this
   * DistribFunctionDefinitionPlugin as a DistribDrawFromDistribution*.
   */
  const DistribDrawFromDistribution* getDistribDrawFromDistribution() const;


  /**
   * Returns the value of the "distribDrawFromDistribution" element of this
   * DistribFunctionDefinitionPlugin.
   *
   * @return the value of the "distribDrawFromDistribution" element of this
   * DistribFunctionDefinitionPlugin as a DistribDrawFromDistribution*.
   */
  DistribDrawFromDistribution* getDistribDrawFromDistribution();


  /**
   * Predicate returning @c true if this DistribFunctionDefinitionPlugin's
   * "distribDrawFromDistribution" element is set.
   *
   * @return @c true if this DistribFunctionDefinitionPlugin's
   * "distribDrawFromDistribution" element has been set, otherwise @c false is
   * returned.
   */
  bool isSetDistribDrawFromDistribution() const;


  /**
   * Sets the value of the "distribDrawFromDistribution" element of this
   * DistribFunctionDefinitionPlugin.
   *
   * @param distribDrawFromDistribution DistribDrawFromDistribution* value of
   * the "distribDrawFromDistribution" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setDistribDrawFromDistribution(const DistribDrawFromDistribution*
    distribDrawFromDistribution);


  /**
   * Creates a new DistribDrawFromDistribution object, adds it to this
   * DistribFunctionDefinitionPlugin object and returns the
   * DistribDrawFromDistribution object created.
   *
   * @return a new DistribDrawFromDistribution object instance.
   */
  DistribDrawFromDistribution* createDistribDrawFromDistribution();


  /**
   * Unsets the value of the "distribDrawFromDistribution" element of this
   * DistribFunctionDefinitionPlugin.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDistribDrawFromDistribution();



  /** @cond doxygenLibsbmlInternal */

  /**
   * Write any contained elements
   */
  virtual void writeElements(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor
   */
  virtual bool accept(SBMLVisitor& v) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument
   */
  virtual void setSBMLDocument(SBMLDocument* d);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Connects to child elements
   */
  virtual void connectToChild();

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Connects to parent element
   */
  virtual void connectToParent(SBase* base);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Enables/disables the given package with this element
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix,
                                     bool flag);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Updates the namespaces when setLevelVersion is used
   */
  virtual void updateSBMLNamespace(const std::string& package,
                                   unsigned int level,
                                   unsigned int version);

  /** @endcond */




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this
   * DistribFunctionDefinitionPlugin.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName, bool& value)
    const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this
   * DistribFunctionDefinitionPlugin.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName, int& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this
   * DistribFunctionDefinitionPlugin.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           double& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this
   * DistribFunctionDefinitionPlugin.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           unsigned int& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this
   * DistribFunctionDefinitionPlugin.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           std::string& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Predicate returning @c true if this DistribFunctionDefinitionPlugin's
   * attribute "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DistribFunctionDefinitionPlugin's attribute
   * "attributeName" has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribFunctionDefinitionPlugin.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, bool value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribFunctionDefinitionPlugin.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, int value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribFunctionDefinitionPlugin.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, double value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribFunctionDefinitionPlugin.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName,
                           unsigned int value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * DistribFunctionDefinitionPlugin.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName,
                           const std::string& value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Unsets the value of the "attributeName" attribute of this
   * DistribFunctionDefinitionPlugin.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetAttribute(const std::string& attributeName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates and returns an new "elementName" object in this
   * DistribFunctionDefinitionPlugin.
   *
   * @param elementName, the name of the element to create.
   *
   * @return pointer to the element created.
   */
  virtual SBase* createChildObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds a new "elementName" object to this DistribFunctionDefinitionPlugin.
   *
   * @param elementName, the name of the element to create.
   *
   * @param element, pointer to the element to be added.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int addChildObject(const std::string& elementName,
                             const SBase* element);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Removes and returns the new "elementName" object with the given id in this
   * DistribFunctionDefinitionPlugin.
   *
   * @param elementName, the name of the element to remove.
   *
   * @param id, the id of the element to remove.
   *
   * @return pointer to the element removed.
   */
  virtual SBase* removeChildObject(const std::string& elementName,
                                   const std::string& id);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the number of "elementName" in this
   * DistribFunctionDefinitionPlugin.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @return unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this
   * DistribFunctionDefinitionPlugin.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @param index, unsigned int the index of the object to retrieve.
   *
   * @return pointer to the object.
   */
  virtual SBase* getObject(const std::string& elementName, unsigned int index);

  /** @endcond */




  #endif /* !SWIG */


  /**
   * Returns the first child element that has the given @p id in the model-wide
   * SId namespace, or @c NULL if no such object is found.
   *
   * @param id a string representing the id attribute of the object to
   * retrieve.
   *
   * @return a pointer to the SBase element with the given @p id. If no such
   * object is found, this method returns @c NULL.
   */
  virtual SBase* getElementBySId(const std::string& id);


  /**
   * Returns the first child element that has the given @p metaid, or @c NULL
   * if no such object is found.
   *
   * @param metaid a string representing the metaid attribute of the object to
   * retrieve.
   *
   * @return a pointer to the SBase element with the given @p metaid. If no
   * such object is found this method returns @c NULL.
   */
  virtual SBase* getElementByMetaId(const std::string& metaid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * @param filter an ElementFilter that may impose restrictions on the objects
   * to be retrieved.
   *
   * @return a List* pointer of pointers to all SBase child objects with any
   * restriction imposed.
   */
  virtual List* getAllElements(ElementFilter * filter = NULL);



  /** @cond doxygenLibsbmlInternal */

  /**
   * Append items from model (used in comp flattening)
   *
   * @param model a pointer to a model object.
   *
   */
  int appendFrom(const Model* model);

  /** @endcond */


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new object from the next XMLToken on the XMLInputStream
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Returns the value of the "distribDrawFromDistribution" element of this
 * DistribFunctionDefinitionPlugin_t.
 *
 * @param dfdp the DistribFunctionDefinitionPlugin_t structure whose
 * distribDrawFromDistribution is sought.
 *
 * @return the value of the "distribDrawFromDistribution" element of this
 * DistribFunctionDefinitionPlugin_t as a DistribDrawFromDistribution*.
 *
 * @memberof DistribFunctionDefinitionPlugin_t
 */
LIBSBML_EXTERN
const DistribDrawFromDistribution_t*
DistribFunctionDefinitionPlugin_getDistribDrawFromDistribution(const
  DistribFunctionDefinitionPlugin_t * dfdp);


/**
 * Predicate returning @c 1 (true) if this DistribFunctionDefinitionPlugin_t's
 * "distribDrawFromDistribution" element is set.
 *
 * @param dfdp the DistribFunctionDefinitionPlugin_t structure.
 *
 * @return @c 1 (true) if this DistribFunctionDefinitionPlugin_t's
 * "distribDrawFromDistribution" element has been set, otherwise @c 0 (false)
 * is returned.
 *
 * @memberof DistribFunctionDefinitionPlugin_t
 */
LIBSBML_EXTERN
int
DistribFunctionDefinitionPlugin_isSetDistribDrawFromDistribution(const
  DistribFunctionDefinitionPlugin_t * dfdp);


/**
 * Sets the value of the "distribDrawFromDistribution" element of this
 * DistribFunctionDefinitionPlugin_t.
 *
 * @param dfdp the DistribFunctionDefinitionPlugin_t structure.
 *
 * @param distribDrawFromDistribution DistribDrawFromDistribution_t* value of
 * the "distribDrawFromDistribution" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribFunctionDefinitionPlugin_t
 */
LIBSBML_EXTERN
int
DistribFunctionDefinitionPlugin_setDistribDrawFromDistribution(
                                                               DistribFunctionDefinitionPlugin_t
                                                                 * dfdp,
                                                               const
                                                                 DistribDrawFromDistribution_t*
                                                                   distribDrawFromDistribution);


/**
 * Creates a new DistribDrawFromDistribution_t object, adds it to this
 * DistribFunctionDefinitionPlugin_t object and returns the
 * DistribDrawFromDistribution_t object created.
 *
 * @param dfdp the DistribFunctionDefinitionPlugin_t structure to which the
 * DistribDrawFromDistribution_t should be added.
 *
 * @return a new DistribDrawFromDistribution_t object instance.
 *
 * @memberof DistribFunctionDefinitionPlugin_t
 */
LIBSBML_EXTERN
DistribDrawFromDistribution_t*
DistribFunctionDefinitionPlugin_createDistribDrawFromDistribution(DistribFunctionDefinitionPlugin_t*
  dfdp);


/**
 * Unsets the value of the "distribDrawFromDistribution" element of this
 * DistribFunctionDefinitionPlugin_t.
 *
 * @param dfdp the DistribFunctionDefinitionPlugin_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DistribFunctionDefinitionPlugin_t
 */
LIBSBML_EXTERN
int
DistribFunctionDefinitionPlugin_unsetDistribDrawFromDistribution(DistribFunctionDefinitionPlugin_t
  * dfdp);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DistribFunctionDefinitionPlugin_H__ */


