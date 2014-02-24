/**
 * @file    ASTArraysVectorFunctionNode.cpp
 * @brief   Base Abstract Syntax Tree (AST) class.
 * @author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2012 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
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
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */

#include <sbml/packages/arrays/math/ASTArraysVectorFunctionNode.h>
#include <sbml/math/ASTNaryFunctionNode.h>
#include <sbml/math/ASTNumber.h>
#include <sbml/math/ASTFunction.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

ASTArraysVectorFunctionNode::ASTArraysVectorFunctionNode (int type) :
  ASTNaryFunctionNode(type)
    //, mName ("")
    //, mDefinitionURL ( "" )
{
}
  

  /**
   * Copy constructor
   */
ASTArraysVectorFunctionNode::ASTArraysVectorFunctionNode (const ASTArraysVectorFunctionNode& orig):
  ASTNaryFunctionNode(orig)
    //, mName (orig.mName)
    //, mDefinitionURL (orig.mDefinitionURL)
{
}
  /**
   * Assignment operator for ASTNode.
   */
ASTArraysVectorFunctionNode&
ASTArraysVectorFunctionNode::operator=(const ASTArraysVectorFunctionNode& rhs)
{
  if(&rhs!=this)
  {
    this->ASTNaryFunctionNode::operator =(rhs);
    //mName = rhs.mName;
    //this->mDefinitionURL = rhs.mDefinitionURL;
  }
  return *this;
}
  /**
   * Destroys this ASTNode, including any child nodes.
   */
ASTArraysVectorFunctionNode::~ASTArraysVectorFunctionNode ()
{
}

  /**
   * Creates a copy (clone).
   */
ASTArraysVectorFunctionNode*
ASTArraysVectorFunctionNode::deepCopy () const
{
  return new ASTArraysVectorFunctionNode(*this);
}

//int
//ASTArraysVectorFunctionNode::swapChildren(ASTFunction* that)
//{
//  if (that->getUnaryFunction() != NULL)
//  {
//    return ASTFunctionBase::swapChildren(that->getUnaryFunction());
//  }
//  else if (that->getBinaryFunction() != NULL)
//  {
//    return ASTFunctionBase::swapChildren(that->getBinaryFunction());
//  }
//  else if (that->getNaryFunction() != NULL)
//  {
//    return ASTFunctionBase::swapChildren(that->getNaryFunction());
//  }
//  else if (that->getUserFunction() != NULL)
//  {
//    return ASTFunctionBase::swapChildren(that->getUserFunction());
//  }
//  else if (that->getLambda() != NULL)
//  {
//    return ASTFunctionBase::swapChildren(that->getLambda());
//  }
//  else if (that->getPiecewise() != NULL)
//  {
//    return ASTFunctionBase::swapChildren(that->getPiecewise());
//  }
//  else if (that->getCSymbol() != NULL)
//  {
//    return ASTFunctionBase::swapChildren(that->getCSymbol()->getDelay());;
//  }
//  else if (that->getQualifier() != NULL)
//  {
//    return ASTFunctionBase::swapChildren(that->getQualifier());
//  }
//  else if (that->getSemantics() != NULL)
//  {
//    return ASTFunctionBase::swapChildren(that->getSemantics());
//  }
//  else
//  {
//    return LIBSBML_OPERATION_FAILED;
//  }
//}



//const std::string& 
//ASTArraysVectorFunctionNode::getName() const
//{
//  return mName;
//}
//
//  
//bool 
//ASTArraysVectorFunctionNode::isSetName() const
//{
//  return (mName.empty() != true);
//}
//
//  
//int 
//ASTArraysVectorFunctionNode::setName(const std::string& name)
//{
//  mName = name;
//  return LIBSBML_OPERATION_SUCCESS;
//
//}
//
//
//int 
//ASTArraysVectorFunctionNode::unsetName()
//{
//  mName = "";
//  return LIBSBML_OPERATION_SUCCESS;
//}
//
//
//
//const std::string& 
//ASTArraysVectorFunctionNode::getDefinitionURL() const
//{
//  return mDefinitionURL;
//}
//
//  
//bool 
//ASTArraysVectorFunctionNode::isSetDefinitionURL() const
//{
//  return (mDefinitionURL.empty() != true);
//}
//
//  
//int 
//ASTArraysVectorFunctionNode::setDefinitionURL(const std::string& url)
//{
//  mDefinitionURL = url;
//  return LIBSBML_OPERATION_SUCCESS;
//
//}
//
//
//int 
//ASTArraysVectorFunctionNode::unsetDefinitionURL()
//{
//  mDefinitionURL = "";
//  return LIBSBML_OPERATION_SUCCESS;
//}
//
//
void
ASTArraysVectorFunctionNode::write(XMLOutputStream& stream) const
{
  if (&stream == NULL) return;

  stream.startElement("vector");
  
  ASTBase::writeAttributes(stream);

  //if (isSetDefinitionURL() == true)
  //{
  //  stream.writeAttribute("definitionURL", getDefinitionURL());
  //}

  //stream << " " << getName() << " ";
  //  
  //stream.endElement("ci");
  
  /* write children */

  for (unsigned int i = 0; i < ASTFunctionBase::getNumChildren(); i++)
  {
    ASTFunctionBase::getChild(i)->write(stream);
  }
    
  stream.endElement("vector");

}

bool
ASTArraysVectorFunctionNode::read(XMLInputStream& stream, const std::string& reqd_prefix)
{
  bool read = false;
  ASTBase * child = NULL;
  const XMLToken element = stream.peek ();

  ASTBase::checkPrefix(stream, reqd_prefix, element);

  const char*      name;

  unsigned int numChildrenAdded = 0;
  while (stream.isGood() && numChildrenAdded < getExpectedNumChildren())// && stream.peek().isEndFor(element) == false)
  {
    stream.skipText();

    name = stream.peek().getName().c_str();

    if (representsNumber(ASTBase::getTypeFromName(name)) == true)
    {
      child = new ASTNumber();
    }
    else 
    {
      child = new ASTFunction();
    }
    
    read = child->read(stream, reqd_prefix);

    stream.skipText();

    if (read == true && addChild(child) == LIBSBML_OPERATION_SUCCESS)
    {
      numChildrenAdded++;
    }
    else
    {
      read = false;
      break;
    }
  }

  if (getExpectedNumChildren() == 0 && numChildrenAdded == 0)
  {
    read = true;
  }

  return read;
}



LIBSBML_CPP_NAMESPACE_END