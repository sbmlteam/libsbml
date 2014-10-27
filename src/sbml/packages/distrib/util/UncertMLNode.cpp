/**
 * @file:   UncertMLNode.cpp
 * @brief:  Implementation of the UncertMLNode class
 * @author: Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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


#include <sbml/packages/distrib/util/UncertMLNode.h>
#include <sbml/util/IdList.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new UncertMLNode
 */
UncertMLNode::UncertMLNode ()
  : mElementName ("")
  , mText ("")
  , mAttributes()
  , mChildren()
{
}

/*
 * Creates a new UncertMLNode from XMLNode object.
 */
UncertMLNode::UncertMLNode (XMLNode* xml)
  : mElementName ("")  
  , mText ("")
  , mAttributes()
  , mChildren()
{
 
  this->parseXMLNode(xml);
}


/*
 * Copy constructor for UncertMLNode.
 */
UncertMLNode::UncertMLNode (const UncertMLNode& orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mElementName = orig.mElementName;
    mText        = orig.mText;
    mAttributes  = orig.mAttributes;

    for (unsigned int c = 0; c < orig.getNumChildren(); ++c)
    {
      addChild( orig.getChild(c)->clone() );
    }
  }
}


/*
 * Assignment for UncertMLNode.
 */
UncertMLNode&
UncertMLNode::operator=(const UncertMLNode& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    mElementName = rhs.mElementName;
    mText        = rhs.mText;
    mAttributes  = rhs.mAttributes;
    
    int size = (int)mChildren.size();
    for (int i = size -1; i >=0;--i)
    {
      delete mChildren[i];
    }

    mChildren.clear();

    for (unsigned int c = 0; c < rhs.getNumChildren(); ++c)
    {
      addChild( rhs.getChild(c)->clone() );
    }

  }
  return *this;
}


/*
 * Clone for UncertMLNode.
 */
UncertMLNode*
UncertMLNode::clone () const
{
  return new UncertMLNode(*this);
}


/*
 * Destructor for UncertMLNode.
 */
UncertMLNode::~UncertMLNode ()
{  
  int size = (int)mChildren.size();
  for (int i = size -1; i >=0;--i)
  {
    delete mChildren[i];
  }

  mChildren.clear();
}


/*
 * Returns the value of the "elementName" attribute of this UncertMLNode.
 */
const std::string&
UncertMLNode::getElementName() const
{
  return mElementName;
}


/*
 * Returns the value of the "text" element of this UncertMLNode.
 */
const std::string&
UncertMLNode::getText() const
{
  return mText;
}


/*
 * Returns the value of the "attributes" attribute of this UncertMLNode.
 */
const XMLAttributes&
UncertMLNode::getAttributes() const
{
  return mAttributes;
}


/*
 * Returns true/false if elementName is set.
 */
bool
UncertMLNode::isSetElementName() const
{
  return (mElementName.empty() == false);
}


/*
 * Returns true/false if text is set.
 */
bool
UncertMLNode::isSetText() const
{
  return (mText.empty() == false);
}


/*
 * Returns true/false if attributes is set.
 */
unsigned int
UncertMLNode::getNumAttributes() const
{
  return mAttributes.getLength();
}


/*
 * Sets elementName and returns value indicating success.
 */
int
UncertMLNode::setElementName(const std::string& elementName)
{
  if (&(elementName) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mElementName = elementName;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets text and returns value indicating success.
 */
int
UncertMLNode::setText(const std::string& text)
{
  if (&(text) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mText = text;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets attributes and returns value indicating success.
 */
int
UncertMLNode::setAttributes(const XMLAttributes & attr)
{
  if (&(attr) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mAttributes = XMLAttributes(attr);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets elementName and returns value indicating success.
 */
int
UncertMLNode::unsetElementName()
{
  mElementName.erase();

  if (mElementName.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets text and returns value indicating success.
 */
int
UncertMLNode::unsetText()
{
  mText.erase();

  if (mText.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets attributes and returns value indicating success.
 */
int
UncertMLNode::unsetAttributes()
{
  mAttributes.clear();
  return LIBSBML_OPERATION_SUCCESS;
}

/* deal with children */
unsigned int
UncertMLNode::getNumChildren() const
{
  return (unsigned int)mChildren.size();
}


UncertMLNode*
UncertMLNode::getChild (unsigned int index) const
{
  if ( index >= mChildren.size()) return NULL;
  return static_cast<UncertMLNode*>( mChildren[index]);
}


int
UncertMLNode::addChild(UncertMLNode * child)
{
  if (child == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else
  {
    unsigned int numBefore = getNumChildren();
    
    mChildren.push_back(child);

    if (getNumChildren() == numBefore + 1)
    {
      return LIBSBML_OPERATION_SUCCESS;
    }
    else
    {
      return LIBSBML_OPERATION_FAILED;
    }
  }
}



/*
 * check if all the required attributes are set
 */
bool
UncertMLNode::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetElementName() == false)
    allPresent = false;

  return allPresent;
}


bool
UncertMLNode::parseXMLNode(const XMLNode* xml)
{
  // if we have an UncertML element then we want the
  // child
  if (xml->getName() == "UncertML")
  {
    xml = &(xml->getChild(0));
  }

  bool success = true;

  std::string name = xml->getName();

  if (name.empty() == false)
  {
    // set the element name
    if (setElementName(xml->getName()) != LIBSBML_OPERATION_SUCCESS)
    {
      success = false;
    }

    // set any attributes from teh element
    if (xml->getAttributesLength() > 0 && success != false)
    {
      if (setAttributes(xml->getAttributes()) != LIBSBML_OPERATION_SUCCESS)
      {
        success = false;
      }
    }

    // loop thru the children of the XMLNode
    // parse each and add as a child
    for (unsigned int i = 0; success != false && i < xml->getNumChildren(); i++)
    {
      UncertMLNode * child = new UncertMLNode();
      success = child->parseXMLNode(&(xml->getChild(i)));
      if (success == true)
      {
        if (addChild(child) != LIBSBML_OPERATION_SUCCESS)
        {
          success = false;
        }
      }
    }
  }
  else
  {
    // here we have a text element
    if (setText(xml->getCharacters()) != LIBSBML_OPERATION_SUCCESS)
    {
      success = false;
    }
  }

  return success;
}


XMLNode*
UncertMLNode::constructXMLNode() const
{
  /* create the top level UncertML element */
  /* create Namespaces*/
  XMLNamespaces xmlns = XMLNamespaces();
  xmlns.add("http://www.uncertml.org/3.0");

  XMLTriple top_triple = XMLTriple("UncertML", 
    "http://www.uncertml.org/3.0", "");
  
  XMLAttributes blank_att = XMLAttributes();
 
  XMLNode * xml = new XMLNode(top_triple, blank_att, xmlns);

  XMLNode * child = reconstructXML();
  xml->addChild(*(child));
  delete child;

  return xml;
}

XMLNode *
UncertMLNode::reconstructXML() const
{
  XMLNode * xml = NULL;
  if (isSetElementName() == true)
  {
    XMLTriple triple = XMLTriple(getElementName(),"", "");
    XMLAttributes att = XMLAttributes(getAttributes());

    xml = new XMLNode(triple, att);

    for (unsigned int n = 0; n < getNumChildren(); n++)
    {
      XMLNode * child = getChild(n)->reconstructXML();
      xml->addChild(*(child));
      delete child;
    }
  }
  else
  {
    xml = new XMLNode(getText());
  }

  return xml;
}

void
UncertMLNode::write(XMLOutputStream & stream) const
{
  XMLNode * tempNode = constructXMLNode();

  tempNode->write(stream);

  delete tempNode;
}


std::string
UncertMLNode::toXMLString() const
{
  XMLNode * tempNode = constructXMLNode();

  std::string xml = tempNode->toXMLString();

  delete tempNode;

  return xml;
}


UncertMLNode * 
UncertMLNode::createStatisticsNode(std::string arguments,  
                                   std::string argumentIds)
{
  UncertMLNode *node = new UncertMLNode();
  node->setElementName("StatisticsCollection");

  XMLAttributes attr = XMLAttributes();
  /* really the url should be specific to the distribtuion
  * but whilst the attribue is required in uncertML it does not require
  * it to be an exact match
  */
  attr.add("definition", "http://www.uncertml.org/statistics");
  node->setAttributes(attr);

  /* create an idlist from the arguments 
   * and check we have the same number of args and ids
   */
  IdList args = IdList(arguments);
  IdList argIds = IdList(argumentIds);

  unsigned int numArgs = args.size();
  unsigned int numIds = argIds.size();

  if (numArgs != numIds)
  {
    return NULL;
  }


  for (unsigned int i = 0; i < numArgs; i++)
  {
    UncertMLNode * varChild = new UncertMLNode();
    varChild->setElementName("var");
    
    XMLAttributes attributes = XMLAttributes();
    attributes.add("varId", argIds.at(i));
    varChild->setAttributes(attributes);

    UncertMLNode * valueChild = new UncertMLNode();
    valueChild->setElementName("value");

    valueChild->addChild(varChild);

    UncertMLNode * child = new UncertMLNode();
    child->setElementName(args.at(i));
    XMLAttributes attr1 = XMLAttributes();
    attr1.add("definition", "http://www.uncertml.org/statistics");
    child->setAttributes(attr1);

    child->addChild(valueChild);

    node->addChild(child);
  }



  return node;
}


UncertMLNode * 
UncertMLNode::createDistributionNode(std::string name, 
                     std::string arguments, std::string argumentIds)
{
  UncertMLNode *node = new UncertMLNode();
  node->setElementName(name);

  XMLAttributes attr = XMLAttributes();
  /* really the url should be specific to the distribtuion
  * but whilst the attribue is required in uncertML it does not require
  * it to be an exact match
  */
  attr.add("definition", "http://www.uncertml.org/distributions");
  node->setAttributes(attr);

  /* create an idlist from the arguments 
   * and check we have the same number of args and ids
   */
  IdList args = IdList(arguments);
  IdList argIds = IdList(argumentIds);

  unsigned int numArgs = args.size();
  unsigned int numIds = argIds.size();

  if (numArgs != numIds)
  {
    return NULL;
  }


  for (unsigned int i = 0; i < numArgs; i++)
  {
    UncertMLNode * varChild = new UncertMLNode();
    varChild->setElementName("var");
    
    XMLAttributes attributes = XMLAttributes();
    attributes.add("varId", argIds.at(i));
    varChild->setAttributes(attributes);

    UncertMLNode * child = new UncertMLNode();
    child->setElementName(args.at(i));

    child->addChild(varChild);

    node->addChild(child);
  }


  return node;
}



/**
 *
 */
LIBSBML_EXTERN
UncertMLNode_t *
UncertMLNode_create()
{
  return new UncertMLNode();
}


/**
 *
 */
LIBSBML_EXTERN
void
UncertMLNode_free(UncertMLNode_t * umln)
{
  if (umln != NULL)
    delete umln;
}


/**
 *
 */
LIBSBML_EXTERN
UncertMLNode_t *
UncertMLNode_clone(UncertMLNode_t * umln)
{
  if (umln != NULL)
  {
    return static_cast<UncertMLNode_t*>(umln->clone());
  }
  else
  {
    return NULL;
  }
}


/**
 *
 */
LIBSBML_EXTERN
char *
UncertMLNode_getElementName(UncertMLNode_t * umln)
{
  if (umln == NULL)
    return NULL;

  return umln->getElementName().empty() ? NULL : 
                       safe_strdup(umln->getElementName().c_str());
}


/**
 *
 */
LIBSBML_EXTERN
const XMLAttributes_t *
UncertMLNode_getAttributes(UncertMLNode_t * umln)
{
  //if (umln == NULL)
  //  return NULL;

  //return umln->getAttributes();
  return NULL;
}


/**
 *
 */
LIBSBML_EXTERN
int
UncertMLNode_isSetElementName(UncertMLNode_t * umln)
{
  return (umln != NULL) ? static_cast<int>(umln->isSetElementName()) : 0;
}


/**
 *
 */
LIBSBML_EXTERN
unsigned int
UncertMLNode_getNumAttributes(UncertMLNode_t * umln)
{
  return (umln != NULL) ? umln->getNumAttributes() : 0;
}


/**
 *
 */
LIBSBML_EXTERN
int
UncertMLNode_setElementName(UncertMLNode_t * umln, const char * elementName)
{
  return (umln != NULL) ? 
    umln->setElementName(elementName) : LIBSBML_INVALID_OBJECT;
}


/**
 *
 */
LIBSBML_EXTERN
int
UncertMLNode_setAttributes(UncertMLNode_t * umln, XMLAttributes_t * attributes)
{
  return 0;
  //(umln != NULL) ? 
  //   umln->setAttributes(attributes) : LIBSBML_INVALID_OBJECT;
}


/**
 *
 */
LIBSBML_EXTERN
int
UncertMLNode_unsetElementName(UncertMLNode_t * umln)
{
  return (umln != NULL) ? umln->unsetElementName() : LIBSBML_INVALID_OBJECT;
}


/**
 *
 */
LIBSBML_EXTERN
int
UncertMLNode_unsetAttributes(UncertMLNode_t * umln)
{
  return (umln != NULL) ? umln->unsetAttributes() : LIBSBML_INVALID_OBJECT;
}


/**
 *
 */
LIBSBML_EXTERN
int
UncertMLNode_hasRequiredAttributes(UncertMLNode_t * umln)
{
  return (umln != NULL) ? static_cast<int>(umln->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


