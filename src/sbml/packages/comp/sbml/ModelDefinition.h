/**
 * @file    ModelDefinition.h
 * @brief   Definition of ModelDefinition, the SBase derived class of the comp package.
 * @author  Lucian Smith 
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2011 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class ModelDefinition
 * @ingroup comp
 * @brief @htmlinclude pkg-marker-comp.html
 * Implementation of the %ModelDefinition construct from the &ldquo;comp&rdquo; package.
 *
 * The @ref comp "Hierarchical Model Composition" package (&ldquo;comp&rdquo;)
 * allows multiple Model objects
 * to be defined in a single SBMLDocument.  While these new Model objects are
 * not new SBML classes, they are given a new name,
 * <code>&lt;modelDefinition&gt;</code>, and reside in ListOfModelDefinition
 * objects.  In libSBML, this class inherits from the Model class, changing
 * only the expected parent of the object, and the XML name.
 *
 * An additional restriction is placed on the "id" attribute of ModelDefinition
 * objects:  not only must it be unique across all such attributes of type SId 
 * within the ModelDefintion, it must also be unique across all Model, 
 * ModelDefinition, and ExternalModelDefinition objects in the same SBMLDocument.
 */

#ifndef ModelDefinition_H__
#define ModelDefinition_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/comp/common/compfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/Model.h>
#include <sbml/packages/comp/extension/CompExtension.h>
#include <sbml/packages/comp/sbml/CompBase.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN ModelDefinition : public Model
{
public:

  /**
   * Creates a new ModelDefinition with the given level, version, and package
   * version.
   *
   * @param level the SBML Level
   * @param version the Version within the SBML Level
   * @param pkgVersion the version of the package
   */
  ModelDefinition(unsigned int level      = CompExtension::getDefaultLevel(),
                  unsigned int version    = CompExtension::getDefaultVersion(),
                  unsigned int pkgVersion = CompExtension::getDefaultPackageVersion());


  /**
   * Creates a new ModelDefinition with the given CompPkgNamespaces object.
   *
   * @param compns the namespace to use
   */
  ModelDefinition(CompPkgNamespaces* compns);


  /**
   * Copy constructor from base Model object.
   */
  ModelDefinition(const Model& source);


  /**
   * Assignment operator.
   */
  ModelDefinition& operator=(const Model& source);


  /**
   * Creates and returns a deep copy of this ModelDefinition object.
   * 
   * @return a (deep) copy of this ModelDefinition object
   */
  virtual ModelDefinition* clone () const;


  /**
   * Destructor.
   */ 
  virtual ~ModelDefinition ();


  /**
   * The only difference between a Model and a ModelDefinition is the
   * element name ('modelDefinition')
   *
   * @return the string of the name of this element ("modelDefintion").
   * @see getTypeCode()
   */
  virtual const std::string& getElementName () const ;


  /**
   * Returns the libSBML type code of this object instance.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @link SBMLCompTypeCode_t#SBML_COMP_MODELDEFINITION SBML_COMP_MODELDEFINITION@endlink
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode () const;


  /**
   * Finds this Model's parent ListOfModelDefinitions and removes itself from
   * it and deletes itself.
   *
   * This method actually just calls the SBase function, since the Model
   * class overrides it, but that's actually what we want to happen here.
   *
   * @return integer value indicating success/failure of the
   * operation. The possible return values are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int removeFromParentAndDelete();


  /** @cond doxygenLibsbmlInternal */
  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the SBML object's next
   * sibling object (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;
  /** @endcond */

protected:
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
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual void readAttributes (const XMLAttributes& attributes, 
                               const ExpectedAttributes& expectedAttributes);
  /** @endcond */



};


LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

//
// C API will be added here.
//

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* ModelDefinition_H__ */
