/**
 * @file    ASTArraysVectorFunctionNode.cpp
 * @brief   Base Abstract Syntax Tree (AST) class.
 * @author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
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
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */

#include <sbml/packages/arrays/math/ASTArraysVectorFunctionNode.h>
#include <sbml/packages/arrays/extension/ArraysASTPlugin.h>
#include <sbml/math/ASTNaryFunctionNode.h>
#include <sbml/math/ASTNumber.h>
#include <sbml/math/ASTFunction.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

ASTArraysVectorFunctionNode::ASTArraysVectorFunctionNode (int type) :
  ASTNaryFunctionNode(type)
{
}
  

  /**
   * Copy constructor
   */
ASTArraysVectorFunctionNode::ASTArraysVectorFunctionNode (const ASTArraysVectorFunctionNode& orig):
  ASTNaryFunctionNode(orig)
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

void
ASTArraysVectorFunctionNode::write(XMLOutputStream& stream) const
{
  if (&stream == NULL) return;

  std::string name = getNameFromType(this->getExtendedType());
  stream.startElement(name);
  
  ASTBase::writeAttributes(stream);

  /* write children */

  for (unsigned int i = 0; i < ASTFunctionBase::getNumChildren(); i++)
  {
    ASTFunctionBase::getChild(i)->write(stream);
  }
    
  stream.endElement(name);

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


int
ASTArraysVectorFunctionNode::getTypeCode () const
{
  return AST_TYPECODE_VECTOR_CONSTRUCTOR;
}


// need to explicitly put these as otherwise the plugins

bool 
ASTArraysVectorFunctionNode::hasCorrectNumberArguments() const
{
  // can have any number of arguments
  return true;
}


LIBSBML_CPP_NAMESPACE_END
