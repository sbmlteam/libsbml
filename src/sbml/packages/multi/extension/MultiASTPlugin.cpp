/**
 * @file    MultiASTPlugin.cpp
 * @brief   Implementation of MultiASTPlugin, the plugin class of
 *          multi package for the AST element.
 * @author  Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2009-2011 jointly by the following organizations: 
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

#include <sbml/packages/multi/extension/MultiASTPlugin.h>

#include <sbml/math/ASTNode.h>

#include <iostream>
using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Constructor
 */
MultiASTPlugin::MultiASTPlugin (const std::string& uri)
  : ASTBasePlugin(uri)
    , mSpeciesReference       ( "" )
    , mRepresentationType ( "" )
{
}



/*
 * Copy constructor. Creates a copy of this SBase object.
 */
MultiASTPlugin::MultiASTPlugin(const MultiASTPlugin& orig)
  : ASTBasePlugin(orig)
    , mSpeciesReference      (orig.mSpeciesReference)
    , mRepresentationType (orig.mRepresentationType)
{
}


/*
 * Destroy this object.
 */
MultiASTPlugin::~MultiASTPlugin () 
{
}

/*
 * Assignment operator for MultiASTPlugin.
 */
MultiASTPlugin& 
MultiASTPlugin::operator=(const MultiASTPlugin& orig)
{
  if(&orig!=this)
  {
    this->ASTBasePlugin::operator =(orig);
    this->mSpeciesReference = orig.mSpeciesReference;
    this->mRepresentationType = orig.mRepresentationType;
  }    
  return *this;
}


/*
 * Creates and returns a deep copy of this MultiASTPlugin object.
 * 
 * @return a (deep) copy of this SBase object
 */
MultiASTPlugin* 
MultiASTPlugin::clone () const
{
  return new MultiASTPlugin(*this);  
}


/** @cond doxygenLibsbmlInternal */
MultiASTPlugin*
MultiASTPlugin::createObject(XMLInputStream& stream)
{
  MultiASTPlugin*        object = 0;


  return object;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
bool
MultiASTPlugin::read(XMLInputStream& stream, const std::string& reqd_prefix,
                                            const XMLToken& currentElement)
{
  bool read = false;
    
  return read;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
void
MultiASTPlugin::writeElements (XMLOutputStream& stream) const
{
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the parent SBML object of this plugin object to
 * this object and child elements (if any).
 * (Creates a child-parent relationship by this plugin object)
 */

void
MultiASTPlugin::connectToParent(ASTNode* astbase)
{
  ASTBasePlugin::connectToParent(astbase);

}
/** @endcond */


/** @cond doxygenLibsbmlInternal *//*
 * Enables/Disables the given package with child elements in this plugin
 * object (if any).
 */
void
MultiASTPlugin::enablePackageInternal(const std::string& pkgURI,
                                        const std::string& pkgPrefix, bool flag)
{
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
const std::string& 
MultiASTPlugin::getSpeciesReference() const
{
  return mSpeciesReference;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
bool
MultiASTPlugin::isSetSpeciesReference() const
{
  return (mSpeciesReference.empty() != true);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
int
MultiASTPlugin::setSpeciesReference(const std::string& speciesReference)
{
  mSpeciesReference = speciesReference;
  return LIBSBML_OPERATION_SUCCESS;

}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
int
MultiASTPlugin::unsetSpeciesReference()
{
  mSpeciesReference = "";
  return LIBSBML_OPERATION_SUCCESS;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
const std::string&
MultiASTPlugin::getRepresentationType() const
{
  return mRepresentationType;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
bool
MultiASTPlugin::isSetRepresentationType() const
{
  return (mRepresentationType.empty() != true);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
int
MultiASTPlugin::setRepresentationType(const std::string& representationType)
{
  mRepresentationType = representationType;
  return LIBSBML_OPERATION_SUCCESS;

}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
int 
MultiASTPlugin::unsetRepresentationType()
{
  mRepresentationType = "";
  return LIBSBML_OPERATION_SUCCESS;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
void
MultiASTPlugin::addExpectedAttributes(ExpectedAttributes& attributes, 
                                     XMLInputStream& stream, int type)
{
  // these are only added to a ci name type
  if (type == AST_NAME)
  {
    attributes.add("speciesReference");
    attributes.add("representationType");
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
bool
MultiASTPlugin::readAttributes(const XMLAttributes& attributes,
                       const ExpectedAttributes& expectedAttributes,
                               XMLInputStream& stream, const XMLToken& element,
                               int type)
{
  bool read = true;

  if (type != AST_NAME)
  {
    return read;
  }
  else
  {

    string speciesReference; 

    attributes.readInto( "speciesReference", speciesReference        );

    if (speciesReference.empty() == false)
    {
      if (setSpeciesReference(speciesReference) != LIBSBML_OPERATION_SUCCESS)
      {
        read = false;
      }
    }

    string representationType; 

    attributes.readInto( "representationType", representationType        );

    if (representationType.empty() == false)
    {
      if (setRepresentationType(representationType) != LIBSBML_OPERATION_SUCCESS)
      {
        read = false;
      }
    }

    return read;
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
void
MultiASTPlugin::writeAttributes(XMLOutputStream& stream, int type) const
{
  if (type != AST_NAME)
  {
    return;
  }
  else
  {
    if (isSetSpeciesReference())
      stream.writeAttribute("speciesReference", getPrefix(), getSpeciesReference());

    if (isSetRepresentationType())
      stream.writeAttribute("representationType", getPrefix(), getRepresentationType());
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the prefix bound to this element.
 */
const std::string& 
MultiASTPlugin::getPrefix() const
{
  if (mPrefix.empty() == true)
  {
    static std::string prefix("multi");
    return prefix;
  }
  else
  {
    return mPrefix;
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
void
MultiASTPlugin::writeXMLNS(XMLOutputStream& stream) const
{
  //bool hasAttributes = false;

  if (hasAttributesSet() == true)
    stream.writeAttribute(getPrefix(), "xmlns", getURI());
}
/** @endcond */


#define GET_NUM_CHILDREN(result,node) \
{\
  ASTFunction* tmp = dynamic_cast<ASTFunction*>(node);\
  if (tmp != NULL) result= tmp->getNumChildren(); \
  else\
  {\
    ASTNode* tmp2 = dynamic_cast<ASTNode*>(node);\
    if (tmp2 != NULL)\
      result= tmp2->getNumChildren(); \
    else result = 0;\
  }\
}

#define GET_NTH_CHILD(result,n,node) \
{\
  ASTFunction* tmp = dynamic_cast<ASTFunction*>(node); \
  if (tmp != NULL) result = tmp->getChild(n); \
  else\
  {\
    ASTNode* tmp2 = dynamic_cast<ASTNode*>(node);\
    if (tmp2 != NULL)\
      result= tmp2->getChild(n); \
    else result = NULL;\
  }\
}


/** @cond doxygenLibsbmlInternal */
void 
MultiASTPlugin::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  ASTBasePlugin::renameSIdRefs(oldid, newid);
  if (!mSpeciesReference.empty() && mSpeciesReference == oldid) {
    mSpeciesReference = newid;
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
bool
MultiASTPlugin::hasAttributesSet() const
{
  bool hasAttributes = false;

  if (isSetSpeciesReference() == true)
  {
    return true;
  }
  else if (isSetRepresentationType() == true)
  { 
    return true;
  }
  else if (mParentASTNode != NULL)
  {
    unsigned int i = 0;

    //ASTNode* node = dynamic_cast<ASTNode*>(mParentASTNode);
    size_t numChildren = mParentASTNode->getNumChildren();
    while (hasAttributes == false && i < numChildren)
    {
      ASTNode* ast = mParentASTNode->getChild(i);
      if (ast != NULL)
      {
        //MultiASTPlugin* mp =
        //  dynamic_cast<MultiASTPlugin*>(ast->getPlugin("multi"));
        //if (mp != NULL)
        //{
        //  hasAttributes = mp->hasAttributesSet();
        //}
      }
      i++;
    }

  }

  return hasAttributes;
}
/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
