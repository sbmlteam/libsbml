/**
* @file    SBMLLevel1Version1Converter.cpp
* @brief   Implementation of SBMLLevel1Version1Converter.
* @author  Frank Bergmann
* 
* <!--------------------------------------------------------------------------
* This file is part of libSBML.  Please visit http://sbml.org for more
* information about SBML, and the latest version of libSBML.
*
* Copyright (C) 2013-2015 jointly by the following organizations:
*     1. California Institute of Technology, Pasadena, CA, USA
*     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
*     3. University of Heidelberg, Heidelberg, Germany
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
*/

#include <sbml/conversion/SBMLLevel1Version1Converter.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/SBMLWriter.h>
#include <sbml/SBMLReader.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>

#ifdef __cplusplus

#include <algorithm>
#include <string>

using namespace std;
LIBSBML_CPP_NAMESPACE_BEGIN


  /** @cond doxygenLibsbmlInternal */
void SBMLLevel1Version1Converter::init()
{
  SBMLLevel1Version1Converter converter;
  SBMLConverterRegistry::getInstance().addConverter(&converter);
}
/** @endcond */


SBMLLevel1Version1Converter::SBMLLevel1Version1Converter () 
  : SBMLConverter("SBML Level 1 Version 1 Converter")
{
}


/*
* Copy constructor.
*/
SBMLLevel1Version1Converter::SBMLLevel1Version1Converter(const SBMLLevel1Version1Converter& orig) :
  SBMLConverter(orig)
{
}


/*
* Destroy this object.
*/
SBMLLevel1Version1Converter::~SBMLLevel1Version1Converter ()
{
}


/*
* Assignment operator for SBMLLevel1Version1Converter.
*/
SBMLLevel1Version1Converter& 
  SBMLLevel1Version1Converter::operator=(const SBMLLevel1Version1Converter& rhs)
{  
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment operator");
  }
  else if(&rhs!=this)
  {
    this->SBMLConverter::operator =(rhs);
  }

  return *this;
}


SBMLLevel1Version1Converter*
  SBMLLevel1Version1Converter::clone () const
{
  return new SBMLLevel1Version1Converter(*this);
}


ConversionProperties
  SBMLLevel1Version1Converter::getDefaultProperties() const
{
  static ConversionProperties prop;
  static bool init = false;

  if (init) 
  {
    return prop;
  }
  else
  {
    SBMLNamespaces * sbmlns = new SBMLNamespaces(1,1); // default namespaces
    prop.setTargetNamespaces(sbmlns); // this gets cloned
    prop.addOption("convertToL1V1", true,
      "convert the document to SBML Level 1 Version 1");
    prop.addOption("changePow", false, 
      "change pow expressions to the (^) hat notation");
    prop.addOption("inlineCompartmentSizes", false, 
      "if true, occurrances of compartment ids in expressions will be replaced with their initial size");



    delete sbmlns;
    init = true;
    return prop;
  }
}


bool 
  SBMLLevel1Version1Converter::matchesProperties(const ConversionProperties &props) const
{
  if (&props == NULL || !props.hasOption("convertToL1V1"))
    return false;
  return true;
}


bool 
  SBMLLevel1Version1Converter::inlineCompartmentSizes()
{
  if (getProperties() == NULL)
  {
    return true;
  }
  else if (getProperties()->hasOption("inlineCompartmentSize") == false)
  {
    return true;
  }
  else
  {
    return getProperties()->getBoolValue("inlineCompartmentSize");
  }
}


bool 
  SBMLLevel1Version1Converter::shouldChangePow()
{
  if (getProperties() == NULL)
  {
    return true;
  }
  else if (getProperties()->hasOption("changePow") == false)
  {
    return true;
  }
  else
  {
    return getProperties()->getBoolValue("changePow");
  }
}

void changePow (ASTNode* node, const std::map<string, double>& compartmentValueMap, bool shouldChangePow)
{
  unsigned int c;
  std::map<string, double>::const_iterator it;

  if (node->getType() == AST_NAME)
  {
    it = compartmentValueMap.find(node->getName());
    if (it != compartmentValueMap.end())
    { 
      node->setType(AST_REAL);
      node->setValue(it->second);
    }
  }

  if (shouldChangePow)
  {
    if (ASTNode_getType(node) == AST_FUNCTION_POWER)
    {
      ASTNode_setType(node, AST_POWER);
    }
  }

    for (c = 0; c < ASTNode_getNumChildren(node); c++)
    {
      changePow( ASTNode_getChild(node, c), compartmentValueMap, shouldChangePow );
    }
}


void convertPow(SBMLDocument_t* doc, bool shouldChangePow, bool inlineCompartmentSizes)
{

  ASTNode_t*		ast_Node;
  Reaction_t*		r;
  KineticLaw_t*	kl;
  unsigned int i;
  Model_t*		model;
  const char*			strKineticFormula;

  model = SBMLDocument_getModel(doc);
  if (model == NULL)
  {
    return;
  }

  std::map<string, double> compartmentValueMap;
  if (inlineCompartmentSizes)
    for(i = 0; i < Model_getNumCompartments(model); i++)
    {
      Compartment* c = model->getCompartment(i);
      compartmentValueMap[c->getId()] = c->getSize();
    }


    for(i = 0; i < Model_getNumReactions(model); i++)
    {
      r  = Model_getReaction(model, i);
      kl = Reaction_getKineticLaw(r);

      if (kl == NULL)
      {
        strKineticFormula = "";
      }
      else
      {
        strKineticFormula = KineticLaw_getFormula(kl);
        if (strKineticFormula == NULL)
        {
          return;
        }
      }
      ast_Node	= (ASTNode_t *) SBML_parseFormula (strKineticFormula);
      if (ast_Node != NULL)
      {
        changePow(ast_Node, compartmentValueMap, shouldChangePow);
        KineticLaw_setMath (kl, ast_Node);
      }
    }

}


int
  SBMLLevel1Version1Converter::convert()
{

  if (mDocument == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }

  unsigned int currentLevel = mDocument->getLevel();
  unsigned int currentVersion = mDocument->getVersion();

  if (currentLevel == 1 && currentVersion == 1)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }

  /* since this function will write to the error log we should
  * clear anything in the log first
  */
  mDocument->getErrorLog()->clearLog();

  bool success;

  success = mDocument->setLevelAndVersion(1, 2, false);
  if (!success) return LIBSBML_OPERATION_FAILED;

  mDocument->updateSBMLNamespace("core", 1, 1);

  convertPow(mDocument, shouldChangePow(), inlineCompartmentSizes());


  return LIBSBML_OPERATION_SUCCESS;
}
/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


