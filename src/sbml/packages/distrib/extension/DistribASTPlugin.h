/**
 * @file DistribASTPlugin.h
 * @brief Definition of the DistribASTPlugin class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
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
 * @class DistribASTPlugin
 * @sbmlbrief{Distrib} Extension of AST.
 */


#ifndef DistribASTPlugin_h
#define DistribASTPlugin_h

#include <sbml/extension/ASTBasePlugin.h>


#ifdef __cplusplus

#include <vector>
#include <string>
#include <map>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN DistribASTPlugin : public ASTBasePlugin
{
private:
  /** @cond doxygenLibsbmlInternal */
  void populateNodeTypes();
  /** @endcond */

public:

  /**
   * Creates a new DistribASTPlugin object.
   *
   */
  DistribASTPlugin();

  DistribASTPlugin(const   DistribASTPlugin& orig);

  
  /**
  * Assignment operator for DistribASTPlugin.
  *
  * @param orig the object whose values are used as the basis of the
  * assignment.
  */
  DistribASTPlugin& operator=(const DistribASTPlugin& orig);


  /**
  * Creates and returns a deep copy of this DistribASTPlugin object.
  *
  * @return the (deep) copy of this DistribASTPlugin object.
  */
  virtual DistribASTPlugin* clone() const;



  /**
  * Destructor for DistribASTPlugin.
  */
  virtual ~DistribASTPlugin();




  virtual bool hasCorrectNamespace(SBMLNamespaces* namespaces) const;
  virtual double evaluateASTNode(const ASTNode * node, const Model * m = NULL) const;
  virtual UnitDefinition * getUnitDefinitionFromPackage(UnitFormulaFormatter* uff, const ASTNode * node, bool inKL, int reactNo) const;

  /**
   * Check if the node type is known to be allowed inside function definitions.
   *
   * Function definitions must be able to be evaluated without resort to outside information.
   * Therefore, some ASTNodes (like AST_TIME and AST_FUNCTION_RATE_OF) are disallowed
   * from appearing there.  This function checks whether this is true for a given type:
   * a return value of '-1' means the plugin has no knowledge of that type; a return
   * value of '1' means the plugin knows that the type is indeed allowed, and a
   * return value of '0' means that the plugin knows that the type is not allowed.
   */
  virtual int allowedInFunctionDefinition(ASTNodeType_t type) const;

  DistribASTPlugin(const std::string& uri);

};


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif

#endif /* DistribASTPlugin_h */
