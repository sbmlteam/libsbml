/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    ASTPiecewiseFunctionNode.cpp
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

#include <sbml/math/ASTPiecewiseFunctionNode.h>
#include <sbml/math/ASTNaryFunctionNode.h>
#include <sbml/math/ASTNumber.h>
#include <sbml/math/ASTFunction.h>
#include <sbml/math/ASTNode.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

ASTPiecewiseFunctionNode::ASTPiecewiseFunctionNode (int type) :
  ASTNaryFunctionNode(type)
    , mNumPiece (0)
    , mHasOtherwise (false)
{
}
  
  
  /**
   * Copy constructor
   */
ASTPiecewiseFunctionNode::ASTPiecewiseFunctionNode (const ASTPiecewiseFunctionNode& orig):
  ASTNaryFunctionNode(orig)
    , mNumPiece (orig.mNumPiece)
    , mHasOtherwise (orig.mHasOtherwise)
{
}
  /**
   * Assignment operator for ASTNode.
   */
ASTPiecewiseFunctionNode&
ASTPiecewiseFunctionNode::operator=(const ASTPiecewiseFunctionNode& rhs)
{
  if(&rhs!=this)
  {
    this->ASTNaryFunctionNode::operator =(rhs);
    mNumPiece = rhs.mNumPiece;
    mHasOtherwise = rhs.mHasOtherwise;
  }
  return *this;
}
  /**
   * Destroys this ASTNode, including any child nodes.
   */
ASTPiecewiseFunctionNode::~ASTPiecewiseFunctionNode ()
{
}

int
ASTPiecewiseFunctionNode::getTypeCode () const
{
  return AST_TYPECODE_FUNCTION_PIECEWISE;
}


  /**
   * Creates a copy (clone).
   */
ASTPiecewiseFunctionNode*
ASTPiecewiseFunctionNode::deepCopy () const
{
  return new ASTPiecewiseFunctionNode(*this);
}

int
ASTPiecewiseFunctionNode::swapChildren(ASTFunction* that)
{
  if (that->getUnaryFunction() != NULL)
  {
    return ASTFunctionBase::swapChildren(that->getUnaryFunction());
  }
  else if (that->getBinaryFunction() != NULL)
  {
    return ASTFunctionBase::swapChildren(that->getBinaryFunction());
  }
  else if (that->getNaryFunction() != NULL)
  {
    return ASTFunctionBase::swapChildren(that->getNaryFunction());
  }
  else if (that->getUserFunction() != NULL)
  {
    return ASTFunctionBase::swapChildren(that->getUserFunction());
  }
  else if (that->getLambda() != NULL)
  {
    return ASTFunctionBase::swapChildren(that->getLambda());
  }
  else if (that->getPiecewise() != NULL)
  {
    return ASTFunctionBase::swapChildren(that->getPiecewise());
  }
  else if (that->getCSymbol() != NULL)
  {
    return ASTFunctionBase::swapChildren(that->getCSymbol()->getDelay());;
  }
  else if (that->getQualifier() != NULL)
  {
    return ASTFunctionBase::swapChildren(that->getQualifier());
  }
  else if (that->getSemantics() != NULL)
  {
    return ASTFunctionBase::swapChildren(that->getSemantics());
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


int
ASTPiecewiseFunctionNode::addChild(ASTBase* child, bool inRead)
{
  // now here what I want to do is just keep track of the number
  // of children being added so the mNumPiece and mHasOtherwise
  // variables can be given appropriate values

  // but not if we are reading a stream because then we already know

  if (inRead == false)
  {
    if (child->getType() != AST_CONSTRUCTOR_PIECE && 
        child->getType() != AST_CONSTRUCTOR_OTHERWISE)
    {
      // if this is an odd number then it could be otherwise
      unsigned int currentNum = ASTFunctionBase::getNumChildren();

      if ((currentNum+1)%2 == 0)
      {
        setNumPiece(getNumPiece()+1);
        setHasOtherwise(false);
      }
      else
      {
        setHasOtherwise(true);
      }
    }
    else
    {
      if (child->getType() == AST_CONSTRUCTOR_PIECE)
      {
        setNumPiece(getNumPiece()+1);
      }
      else
      {
        setHasOtherwise(true);
      }
    }
  }

  return ASTFunctionBase::addChild(child);
}

ASTBase* 
ASTPiecewiseFunctionNode::getChild (unsigned int n) const
{
  /* HACK TO REPLICATE OLD AST */
  /* do not return a node with teh piece or otherwise type
   * return the correct child of the piece type
   * or the child of the otherwise
   */

  unsigned int numChildren = ASTFunctionBase::getNumChildren();

  // determine index that we actually want
  unsigned int childNo = (unsigned int)(n/2);
  unsigned int pieceIndex = (unsigned int)(n%2);

  if (getHasOtherwise() == true && childNo == numChildren - 1)
  {
    if (ASTFunctionBase::getChild(childNo)->getType() 
                                            == AST_CONSTRUCTOR_OTHERWISE)
    {
      ASTBase * base = ASTFunctionBase::getChild(childNo);
      ASTNode * otherwise = dynamic_cast<ASTNode*>(base);
      //ASTFunction * otherwise;
      //if (base->getFunction() != NULL)
      //{
      //  otherwise = static_cast<ASTFunction*>(base->getFunction());
      //}
      //else
      //{
      //  otherwise = static_cast<ASTFunction*>(base);
      //}

      if (otherwise != NULL)
      {
        if (otherwise->getNumChildren() > 0)
        {
          return otherwise->getChild(0);
        }
        else
        {
          return NULL;
        }
      }
      else
      {
        return NULL;
      }
    }
    else
    {
      return ASTFunctionBase::getChild(childNo);
    }
  }
  else if (ASTFunctionBase::getChild(childNo)->getType() 
                                               == AST_CONSTRUCTOR_PIECE)
  {
    ASTBase * base = ASTFunctionBase::getChild(childNo);
    ASTNode * piece = dynamic_cast<ASTNode*>(base);
    //if (base->getFunction() != NULL)
    //{
    //  piece = static_cast<ASTFunction*>(base->getFunction());
    //}
    //else
    //{
    //  piece = static_cast<ASTFunction*>(base);
    //}

    if (piece != NULL)
    {
      if (piece->getNumChildren() > pieceIndex)
      {
        return piece->getChild(pieceIndex);
      }
      else
      {
        return NULL;
      }
    }
    else
    {
      return NULL;
    }
  }
  else if (n < numChildren)
  {
    return ASTFunctionBase::getChild(n);
  }
  else
  {
    return NULL;
  }
}


unsigned int
ASTPiecewiseFunctionNode::getNumChildren() const
{
  /* HACK TO REPLICATE OLD AST */
  unsigned int numChildren = getNumPiece() * 2;
  if (getHasOtherwise() == true)
  {
    numChildren++;
  }

  return numChildren;
}




unsigned int 
ASTPiecewiseFunctionNode::getNumPiece() const
{
  return mNumPiece;
}
  
  
int 
ASTPiecewiseFunctionNode::setNumPiece(unsigned int numPiece)
{
  mNumPiece = numPiece;
  return LIBSBML_OPERATION_SUCCESS;

}


bool
ASTPiecewiseFunctionNode::getHasOtherwise() const
{
  return mHasOtherwise;
}
  
  
int 
ASTPiecewiseFunctionNode::setHasOtherwise(bool otherwise)
{
  mHasOtherwise = otherwise;
  return LIBSBML_OPERATION_SUCCESS;

}

void
ASTPiecewiseFunctionNode::write(XMLOutputStream& stream) const
{
  if (&stream == NULL) return;

  ASTBase::writeStartElement(stream);

  unsigned int i;
  unsigned int numChild = 0;
  unsigned int numChildren = ASTFunctionBase::getNumChildren();
  for (i = 0; i < getNumPiece(); i++)
  {
  /* HACK TO REPLICATE OLD AST */
  /* old ast behaviour would take each child in turn as elements of piece
   * and then otherwise
   */
    if (ASTFunctionBase::getChild(i)->getType() == AST_CONSTRUCTOR_PIECE)
    {
      ASTFunctionBase::getChild(i)->write(stream);
    }
    else
    {
      stream.startElement("piece");
      if (numChild < numChildren)
      {
        ASTFunctionBase::getChild(numChild)->write(stream);
        numChild++;
      }
      if (numChild < numChildren)
      {
        ASTFunctionBase::getChild(numChild)->write(stream);
        numChild++;
      }
      stream.endElement("piece");
    }
  }

  if (getHasOtherwise() == true)
  {
    if (ASTFunctionBase::getChild(numChildren-1)->getType() 
                                             == AST_CONSTRUCTOR_OTHERWISE)
    {
      ASTFunctionBase::getChild(numChildren-1)->write(stream);
    }
    else
    {
      stream.startElement("otherwise");
      ASTFunctionBase::getChild(numChildren-1)->write(stream);
      stream.endElement("otherwise");
    }
  }

  
    
  stream.endElement("piecewise");
}

bool
ASTPiecewiseFunctionNode::read(XMLInputStream& stream, const std::string& reqd_prefix)
{
  bool read = false;
  ASTBase * child = NULL;

  unsigned int numPiece = getNumPiece();
  unsigned int numChildrenAdded = 0;
  
  // read in piece
  // these are functions as they will be created as ASTQualifierNodes

  while(numChildrenAdded < numPiece)
  {
    child = new ASTFunction();
    read = child->read(stream, reqd_prefix);

    if (read == true && addChild(child, true) == LIBSBML_OPERATION_SUCCESS)
    {
      numChildrenAdded++;
    }
    else
    {
      read = false;
      break;
    }
  }

  // if there were no piece statements mark read true so we can continue
  if (numPiece == 0)
  {
    read = true;
  }


  if (read == true && getHasOtherwise() == true)
  {
    child = new ASTFunction();
    read = child->read(stream, reqd_prefix);
    
    if (read == true && addChild(child, true) == LIBSBML_OPERATION_SUCCESS)
    {
      numChildrenAdded++;
    }
    else
    {
      read = false;
    }
  }
  return read;
}



bool 
ASTPiecewiseFunctionNode::hasCorrectNumberArguments() const
{
  bool correctNumArgs = true;

  if (getNumChildren() < 1)
  {
    correctNumArgs = false;
  }

  return correctNumArgs;
}



LIBSBML_CPP_NAMESPACE_END


/** @endcond */

