/**
 * @file SpatialParameterPlugin.h
 * @brief Definition of the SpatialParameterPlugin class.
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
 * @class SpatialParameterPlugin
 * @sbmlbrief{spatial} Extension of Parameter.
 */


#ifndef SpatialParameterPlugin_H__
#define SpatialParameterPlugin_H__


#include <sbml/common/extern.h>


#ifdef __cplusplus


#include <sbml/extension/SBasePlugin.h>
#include <sbml/packages/spatial/sbml/SpatialSymbolReference.h>
#include <sbml/packages/spatial/sbml/AdvectionCoefficient.h>
#include <sbml/packages/spatial/sbml/BoundaryCondition.h>
#include <sbml/packages/spatial/sbml/DiffusionCoefficient.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SpatialParameterPlugin : public SBasePlugin
{
protected:

  /** @cond doxygenLibsbmlInternal */

  SpatialSymbolReference* mSpatialSymbolReference;
  AdvectionCoefficient* mAdvectionCoefficient;
  BoundaryCondition* mBoundaryCondition;
  DiffusionCoefficient* mDiffusionCoefficient;

  /** @endcond */

public:

  /**
   * Creates a new SpatialParameterPlugin using the given uri, prefix and
   * package namespace.
   *
   * @param uri a string, representing the uri of the package.
   *
   * @param prefix a string, the prefix to be used.
   *
   * @param spatialns a pointer to the SpatialPkgNamespaces object to be used.
   */
  SpatialParameterPlugin(const std::string& uri,
                         const std::string& prefix,
                         SpatialPkgNamespaces* spatialns);


  /**
   * Copy constructor for SpatialParameterPlugin.
   *
   * @param orig the SpatialParameterPlugin instance to copy.
   */
  SpatialParameterPlugin(const SpatialParameterPlugin& orig);


  /**
   * Assignment operator for SpatialParameterPlugin.
   *
   * @param rhs the SpatialParameterPlugin object whose values are to be used
   * as the basis of the assignment.
   */
  SpatialParameterPlugin& operator=(const SpatialParameterPlugin& rhs);


  /**
   * Creates and returns a deep copy of this SpatialParameterPlugin object.
   *
   * @return a (deep) copy of this SpatialParameterPlugin object.
   */
  virtual SpatialParameterPlugin* clone() const;


  /**
   * Destructor for SpatialParameterPlugin.
   */
  virtual ~SpatialParameterPlugin();


  /**
   * Returns the value of the "spatialSymbolReference" element of this
   * SpatialParameterPlugin.
   *
   * @return the value of the "spatialSymbolReference" element of this
   * SpatialParameterPlugin as a SpatialSymbolReference*.
   */
  const SpatialSymbolReference* getSpatialSymbolReference() const;


  /**
   * Returns the value of the "spatialSymbolReference" element of this
   * SpatialParameterPlugin.
   *
   * @return the value of the "spatialSymbolReference" element of this
   * SpatialParameterPlugin as a SpatialSymbolReference*.
   */
  SpatialSymbolReference* getSpatialSymbolReference();


  /**
   * Returns the value of the "advectionCoefficient" element of this
   * SpatialParameterPlugin.
   *
   * @return the value of the "advectionCoefficient" element of this
   * SpatialParameterPlugin as a AdvectionCoefficient*.
   */
  const AdvectionCoefficient* getAdvectionCoefficient() const;


  /**
   * Returns the value of the "advectionCoefficient" element of this
   * SpatialParameterPlugin.
   *
   * @return the value of the "advectionCoefficient" element of this
   * SpatialParameterPlugin as a AdvectionCoefficient*.
   */
  AdvectionCoefficient* getAdvectionCoefficient();


  /**
   * Returns the value of the "boundaryCondition" element of this
   * SpatialParameterPlugin.
   *
   * @return the value of the "boundaryCondition" element of this
   * SpatialParameterPlugin as a BoundaryCondition*.
   */
  const BoundaryCondition* getBoundaryCondition() const;


  /**
   * Returns the value of the "boundaryCondition" element of this
   * SpatialParameterPlugin.
   *
   * @return the value of the "boundaryCondition" element of this
   * SpatialParameterPlugin as a BoundaryCondition*.
   */
  BoundaryCondition* getBoundaryCondition();


  /**
   * Returns the value of the "diffusionCoefficient" element of this
   * SpatialParameterPlugin.
   *
   * @return the value of the "diffusionCoefficient" element of this
   * SpatialParameterPlugin as a DiffusionCoefficient*.
   */
  const DiffusionCoefficient* getDiffusionCoefficient() const;


  /**
   * Returns the value of the "diffusionCoefficient" element of this
   * SpatialParameterPlugin.
   *
   * @return the value of the "diffusionCoefficient" element of this
   * SpatialParameterPlugin as a DiffusionCoefficient*.
   */
  DiffusionCoefficient* getDiffusionCoefficient();


  /**
   * Predicate returning @c true if this SpatialParameterPlugin's
   * "spatialSymbolReference" element is set.
   *
   * @return @c true if this SpatialParameterPlugin's "spatialSymbolReference"
   * element has been set, otherwise @c false is returned.
   */
  bool isSetSpatialSymbolReference() const;


  /**
   * Predicate returning @c true if this SpatialParameterPlugin's
   * "advectionCoefficient" element is set.
   *
   * @return @c true if this SpatialParameterPlugin's "advectionCoefficient"
   * element has been set, otherwise @c false is returned.
   */
  bool isSetAdvectionCoefficient() const;


  /**
   * Predicate returning @c true if this SpatialParameterPlugin's
   * "boundaryCondition" element is set.
   *
   * @return @c true if this SpatialParameterPlugin's "boundaryCondition"
   * element has been set, otherwise @c false is returned.
   */
  bool isSetBoundaryCondition() const;


  /**
   * Predicate returning @c true if this SpatialParameterPlugin's
   * "diffusionCoefficient" element is set.
   *
   * @return @c true if this SpatialParameterPlugin's "diffusionCoefficient"
   * element has been set, otherwise @c false is returned.
   */
  bool isSetDiffusionCoefficient() const;


  /**
   * Sets the value of the "spatialSymbolReference" element of this
   * SpatialParameterPlugin.
   *
   * @param spatialSymbolReference SpatialSymbolReference* value of the
   * "spatialSymbolReference" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setSpatialSymbolReference(const SpatialSymbolReference*
    spatialSymbolReference);


  /**
   * Sets the value of the "advectionCoefficient" element of this
   * SpatialParameterPlugin.
   *
   * @param advectionCoefficient AdvectionCoefficient* value of the
   * "advectionCoefficient" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setAdvectionCoefficient(const AdvectionCoefficient*
    advectionCoefficient);


  /**
   * Sets the value of the "boundaryCondition" element of this
   * SpatialParameterPlugin.
   *
   * @param boundaryCondition BoundaryCondition* value of the
   * "boundaryCondition" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setBoundaryCondition(const BoundaryCondition* boundaryCondition);


  /**
   * Sets the value of the "diffusionCoefficient" element of this
   * SpatialParameterPlugin.
   *
   * @param diffusionCoefficient DiffusionCoefficient* value of the
   * "diffusionCoefficient" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setDiffusionCoefficient(const DiffusionCoefficient*
    diffusionCoefficient);


  /**
   * Creates a new SpatialSymbolReference object, adds it to this
   * SpatialParameterPlugin object and returns the SpatialSymbolReference
   * object created.
   *
   * @return a new SpatialSymbolReference object instance.
   */
  SpatialSymbolReference* createSpatialSymbolReference();


  /**
   * Creates a new AdvectionCoefficient object, adds it to this
   * SpatialParameterPlugin object and returns the AdvectionCoefficient object
   * created.
   *
   * @return a new AdvectionCoefficient object instance.
   */
  AdvectionCoefficient* createAdvectionCoefficient();


  /**
   * Creates a new BoundaryCondition object, adds it to this
   * SpatialParameterPlugin object and returns the BoundaryCondition object
   * created.
   *
   * @return a new BoundaryCondition object instance.
   */
  BoundaryCondition* createBoundaryCondition();


  /**
   * Creates a new DiffusionCoefficient object, adds it to this
   * SpatialParameterPlugin object and returns the DiffusionCoefficient object
   * created.
   *
   * @return a new DiffusionCoefficient object instance.
   */
  DiffusionCoefficient* createDiffusionCoefficient();


  /**
   * Unsets the value of the "spatialSymbolReference" element of this
   * SpatialParameterPlugin.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetSpatialSymbolReference();


  /**
   * Unsets the value of the "advectionCoefficient" element of this
   * SpatialParameterPlugin.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetAdvectionCoefficient();


  /**
   * Unsets the value of the "boundaryCondition" element of this
   * SpatialParameterPlugin.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetBoundaryCondition();


  /**
   * Unsets the value of the "diffusionCoefficient" element of this
   * SpatialParameterPlugin.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDiffusionCoefficient();


  /**
   * Predicate returning @c true if all the required elements for this
   * SpatialParameterPlugin object have been set.
   *
   * @return @c true to indicate that all the required elements of this
   * SpatialParameterPlugin have been set, otherwise @c false is returned.
   *
   *
   * @note The required elements for the SpatialParameterPlugin object are:
   */
  virtual bool hasRequiredElements() const;



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




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Gets the value of the "attributeName" attribute of this
   * SpatialParameterPlugin.
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
   * SpatialParameterPlugin.
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
   * SpatialParameterPlugin.
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
   * SpatialParameterPlugin.
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
   * SpatialParameterPlugin.
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
   * Predicate returning @c true if this SpatialParameterPlugin's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this SpatialParameterPlugin's attribute "attributeName"
   * has been set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this
   * SpatialParameterPlugin.
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
   * SpatialParameterPlugin.
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
   * SpatialParameterPlugin.
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
   * SpatialParameterPlugin.
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
   * SpatialParameterPlugin.
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
   * SpatialParameterPlugin.
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
   * SpatialParameterPlugin.
   *
   * @param elementName, the name of the element to create.
   *
   * pointer to the element created.
   */
  virtual SBase* createObject(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the number of "elementName" in this SpatialParameterPlugin.
   *
   * @param elementName, the name of the element to get number of.
   *
   * unsigned int number of elements.
   */
  virtual unsigned int getNumObjects(const std::string& elementName);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the nth object of "objectName" in this SpatialParameterPlugin.
   *
   * @param elementName, the name of the element to get number of.
   *
   * @param index, unsigned int teh index of teh object to retrieve.
   *
   * pointer to the object.
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
   * @return a pointer to the SBase element with the given @p id.
   */
  virtual SBase* getElementBySId(const std::string& id);


  /**
   * Returns the first child element that has the given @p metaid, or @c NULL
   * if no such object is found.
   *
   * @param metaid a string representing the metaid attribute of the object to
   * retrieve.
   *
   * @return a pointer to the SBase element with the given @p metaid.
   */
  virtual SBase* getElementByMetaId(const std::string& metaid);


  /**
   * Returns a List of all child SBase objects, including those nested to an
   * arbitrary depth.
   *
   * filter, an ElementFilter that may impose restrictions on the objects to be
   * retrieved.
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


public:
   /** 
   * @return true, if either the spatial symbol reference, diffusion coefficient, 
   *   advection coefficient or boundary is set. Otherwise the return value is false.
   */ 
  bool isSpatialParameter() const;

  /** 
   * Determines the type of the spatial parameter, that is one of: 
   * 
   * SBML_SPATIAL_SPATIALSYMBOLREFERENCE
   * SBML_SPATIAL_DIFFUSIONCOEFFICIENT
   * SBML_SPATIAL_ADVECTIONCOEFFICIENT
   * SBML_SPATIAL_BOUNDARYCONDITION
   * 
   * or -1 in case no other is defined.
   */
  int getType() const;

};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#endif /* !SpatialParameterPlugin_H__ */


