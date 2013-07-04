/**
 * @file    ArraysASTPlugin.cpp
 * @brief   Implementation of ArraysASTPlugin, the plugin class of
 *          arrays package for the AST element.
 * @author  Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2011 jointly by the following organizations: 
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
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */

#include <sbml/packages/arrays/extension/ArraysASTPlugin.h>
#include <sbml/math/NewASTNode.h>
//#include <sbml/packages/arrays/sbml/ASTCnLogicalNode.h>

#include <iostream>
using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN

static const char* QUAL_MATHML_ELEMENTS[] =
{
    "selector"
  , "sum"
};

static const int QUAL_MATHML_TYPES[] =
{
    AST_ARRAYS_FUNCTION_SELECTOR
  , AST_FUNCTION_SUM
};

static const char* QUAL_MATHML_FUNCTIONS[] =
{
    "selector"
  , "sum"
};


/*
 * Constructor
 */
ArraysASTPlugin::ArraysASTPlugin (const std::string &uri)
  : ASTBasePlugin(uri)
  , mMath (NULL)
{
}


/*
 * Constructor
 */
ArraysASTPlugin::ArraysASTPlugin ()
  : ASTBasePlugin()
  , mMath (NULL)
{
}


/*
 * Copy constructor. Creates a copy of this SBase object.
 */
ArraysASTPlugin::ArraysASTPlugin(const ArraysASTPlugin& orig)
  : ASTBasePlugin(orig)
  , mMath (orig.mMath)
{
}


/*
 * Destroy this object.
 */
ArraysASTPlugin::~ArraysASTPlugin () 
{
  delete mMath;
}

/*
 * Assignment operator for ArraysASTPlugin.
 */
ArraysASTPlugin& 
ArraysASTPlugin::operator=(const ArraysASTPlugin& orig)
{
  if(&orig!=this)
  {
    this->ASTBasePlugin::operator =(orig);
    mMath = orig.mMath;
  }    
  return *this;
}


/*
 * Creates and returns a deep copy of this ArraysASTPlugin object.
 * 
 * @return a (deep) copy of this SBase object
 */
ArraysASTPlugin* 
ArraysASTPlugin::clone () const
{
  return new ArraysASTPlugin(*this);  
}


/*
 *
 */
NewASTNode*
ArraysASTPlugin::createObject(XMLInputStream& stream)
{
  NewASTNode*        object = 0;

  const std::string&   name   = stream.peek().getName();
  const XMLNamespaces& xmlns  = stream.peek().getNamespaces();
  const std::string&   prefix = stream.peek().getPrefix();

  //const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;
  //
  //if (prefix == targetPrefix)
  //{
    //if ( name == "math" ) 
    //{
    //  object = mMath;
    //
    //}          
  //}    

  return object;
}

bool
ArraysASTPlugin::read(XMLInputStream& stream)
{
  bool read = false;
  const XMLToken element = stream.peek();
  const string&  name = element.getName();
  
    
  return read;
}


/*
 *
 */
void
ArraysASTPlugin::writeElements (XMLOutputStream& stream) const
{
  if (isSetMath() == true)
  {
    mMath->write(stream);
  }
}


/*
 * Sets the parent SBMLDocument of this SBML object.
 *
 * @param d the SBMLDocument object to use
 */
void 
ArraysASTPlugin::setSBMLDocument (SBMLDocument* d)
{
  ASTBasePlugin::setSBMLDocument(d);

}


/*
 * Sets the parent SBML object of this plugin object to
 * this object and child elements (if any).
 * (Creates a child-parent relationship by this plugin object)
 */
void
ArraysASTPlugin::connectToParent (ASTBase* astbase)
{
  ASTBasePlugin::connectToParent(astbase);

}


/*
 * Enables/Disables the given package with child elements in this plugin
 * object (if any).
 */
void
ArraysASTPlugin::enablePackageInternal(const std::string& pkgURI,
                                        const std::string& pkgPrefix, bool flag)
{
}

  
NewASTNode* 
ArraysASTPlugin::getMath() const
{
  return mMath;
}

bool 
ArraysASTPlugin::isSetMath() const
{
  return (mMath != NULL);
}

int 
ArraysASTPlugin::setMath(const NewASTNode* math)
{
  if (mMath == math) 
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (math == NULL)
  {
    delete mMath;
    mMath = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  //else if (!(math->isWellFormedASTNode()))
  //{
  //  return LIBSBML_INVALID_OBJECT;
  //}
  else
  {
    delete mMath;
//    mMath = (math != NULL) ? math->deepCopy() : NULL;
    //if (mMath != NULL) mMath->setParentSBMLObject(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
}

  
int 
ArraysASTPlugin::unsetMath()
{
  delete mMath;
  mMath = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}

bool 
ArraysASTPlugin::representsNumberNode(int type) const
{
  bool valid = false;

  return valid;
}


bool 
ArraysASTPlugin::isFunction(int type) const
{
  bool valid = false;

  switch (type)
  {
    case AST_ARRAYS_FUNCTION_SELECTOR:
      valid = true;
      break;
    default:
      break;

  }
  return valid;
}

bool 
ArraysASTPlugin::representsUnaryFunction(int type) const
{
  bool valid = false;

  return valid;
}


bool 
ArraysASTPlugin::representsBinaryFunction(int type) const
{
  bool valid = false;

  return valid;
}


bool 
ArraysASTPlugin::representsNaryFunction(int type) const
{
  bool valid = false;

  switch (type)
  {
    case AST_ARRAYS_FUNCTION_SELECTOR:
      valid = true;
      break;
    default:
      break;

  }
  return valid;
}


int 
ArraysASTPlugin::getTypeFromName(const std::string& name) const
{
  int type = AST_UNKNOWN;

  static const int size = sizeof(QUAL_MATHML_ELEMENTS) / sizeof(QUAL_MATHML_ELEMENTS[0]);

  int  index = util_bsearchStringsI(QUAL_MATHML_ELEMENTS, name.c_str(), 0, size - 1);
  bool found = (index < size);

  if (found) 
  {
    type = QUAL_MATHML_TYPES[index];
  }

  return type;
}

const char * 
ArraysASTPlugin::getNameFromType(int type) const
{
  const char* name = QUAL_MATHML_FUNCTIONS[type - AST_ARRAYS_FUNCTION_SELECTOR];
  return name;
}

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
