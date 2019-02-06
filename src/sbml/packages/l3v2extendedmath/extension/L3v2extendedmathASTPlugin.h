/**
 * @file L3v2extendedmathASTPlugin.h
 * @brief Definition of the L3v2extendedmathASTPlugin class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 * @class L3v2extendedmathASTPlugin
 * @sbmlbrief{l3v2extendedmath} Extension of AST.
 */


#ifndef L3v2ExtendedMath_h
#define L3v2ExtendedMath_h

#include <sbml/extension/ASTBasePlugin.h>


#ifdef __cplusplus

#include <vector>
#include <string>
#include <map>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN L3v2extendedmathASTPlugin : public ASTBasePlugin
{
private:
  /** @cond doxygenLibsbmlInternal */
  void populateNodeTypes();
  /** @endcond */


public:

  /**
   * Creates a new L3v2extendedmathASTPlugin object.
   *
   */
  L3v2extendedmathASTPlugin();

  L3v2extendedmathASTPlugin(const   L3v2extendedmathASTPlugin& orig);

  
  /**
  * Assignment operator for L3v2extendedmathASTPlugin.
  *
  * @param orig the object whose values are used as the basis of the
  * assignment.
  */
  L3v2extendedmathASTPlugin& operator=(const L3v2extendedmathASTPlugin& orig);


  /**
  * Creates and returns a deep copy of this L3v2extendedmathASTPlugin object.
  *
  * @return the (deep) copy of this L3v2extendedmathASTPlugin object.
  */
  virtual L3v2extendedmathASTPlugin* clone() const;



  /**
  * Destructor for L3v2extendedmathASTPlugin.
  */
  virtual ~L3v2extendedmathASTPlugin();




  virtual bool hasCorrectNamespace(SBMLNamespaces* namespaces) const;

  L3v2extendedmathASTPlugin(const std::string &uri);

  virtual int checkNumArguments(const ASTNode* function, std::stringstream& error) const;
  virtual double evaluateASTNode(const ASTNode * node, const Model * m = NULL) const;
  /** 
   * returns the unitDefinition for the ASTNode from a rem function
   */
  UnitDefinition * getUnitDefinitionFromRem(UnitFormulaFormatter* uff, const ASTNode *node, 
    bool inKL, int reactNo) const;

  /** 
   * returns the unitDefinition for the ASTNode from a rateOf function
   */
  UnitDefinition * getUnitDefinitionFromRateOf(UnitFormulaFormatter* uff, const ASTNode *node, 
    bool inKL, int reactNo) const;

  virtual UnitDefinition * getUnitDefinitionFromPackage(UnitFormulaFormatter* uff, const ASTNode * node, bool inKL, int reactNo) const;
  //virtual void checkUnits(ArgumentsUnitsCheck* auc, const Model& m, const ASTNode& node, const SBase & sb, bool inKL, int reactNo) const;

  virtual bool isLogical(ASTNodeType_t type) const;
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

};


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */

#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif

#endif /* L3v2ExtendedMath_h */
