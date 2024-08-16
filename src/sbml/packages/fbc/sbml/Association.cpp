/**
 * @file    Association.cpp
 * @brief   Implementation of Association, the contents of a GeneAssociation.
 * @author  Frank T. Bergmann
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
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
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 * 
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */

#include <iostream>
#include <limits>

#include <sbml/SBMLVisitor.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/util/util.h>

#include <sbml/packages/fbc/sbml/Association.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>
#include <sbml/packages/fbc/sbml/FbcAnd.h>
#include <sbml/packages/fbc/sbml/FbcOr.h>
#include <sbml/packages/fbc/sbml/GeneProductRef.h>
#include <sbml/math/FormulaParser.h>
#include <sbml/util/util.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus

/*
 * Creates a new Member with the given level, version, and package version.
 */
Association::Association (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : SBase (level,version)
  , mType(UNKNOWN_ASSOCIATION)
  , mReference()
  , mAssociations()
{
  // set an SBMLNamespaces derived object (FbcPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new FbcPkgNamespaces(level,version,pkgVersion));  
}


/*
 * Creates a new Member with the given FbcPkgNamespaces object.
 */
Association::Association(FbcPkgNamespaces* fbcns)
 : SBase(fbcns)
 , mType(UNKNOWN_ASSOCIATION)
 , mReference()
 , mAssociations()
{
  //
  // set the element namespace of this object
  //
  setElementNamespace(fbcns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(fbcns);
}

/*
 * Creates a new Member with the given FbcPkgNamespaces object.
 */
Association::Association(const XMLNode& node, FbcPkgNamespaces* fbcns)
  : SBase(fbcns)
  , mType(UNKNOWN_ASSOCIATION)
  , mReference()
  , mAssociations()
{
  //
  // set the element namespace of this object
  //
  setElementNamespace(fbcns->getURI());
  
  // load package extensions bound with this object (if any)
  loadPlugins(fbcns);
  
  
  if (node.getName() == "gene")
  {
    setType(GENE_ASSOCIATION);    
    ExpectedAttributes ea;
    addExpectedAttributes(ea);
    readAttributes(node.getAttributes(),ea);
  }
  else if (node.getName() == "and")
  {
    setType(AND_ASSOCIATION);
  }
  else if (node.getName() == "or")
  {
    setType(OR_ASSOCIATION);
  }
  
  if (mType == AND_ASSOCIATION || mType == OR_ASSOCIATION)
  {
    unsigned int n=0,nMax = node.getNumChildren();
    while(n<nMax)
    {
      const XMLNode *child=&node.getChild(n);
      const std::string& childName=child->getName();
      
      if(childName=="gene" || childName=="or" || childName == "and")
      {
        mAssociations.push_back(new Association(*child, new FbcPkgNamespaces(*fbcns)));
      }
      ++n;
    }

  }
  
}

/*
 * Copy constructor.
 */
Association::Association(const Association& source) : SBase(source)
{
  this->mType=source.mType;
  this->mReference=source.mReference;

  std::vector<Association*>::const_iterator it;
  for (it = source.mAssociations.begin(); it != source.mAssociations.end(); ++it)
  {
    mAssociations.push_back((*it)->clone());
  }
}

/*
 * Assignment operator.
 */
Association& Association::operator=(const Association& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
    this->mType=source.mType;
    this->mReference=source.mReference;

    std::vector<Association*>::iterator it;
    for (it = mAssociations.begin(); it != mAssociations.end(); ++it)
    {
      delete (*it);
    }
    mAssociations.clear();

    std::vector<Association*>::const_iterator cit;
    for (cit = source.mAssociations.begin(); cit != source.mAssociations.end(); ++cit)
    {
      mAssociations.push_back((*cit)->clone());
    }
  }
  
  return *this;
}


/*
 * Destructor.
 */ 
Association::~Association ()
{
  std::vector<Association*>::iterator it;
  for (it = mAssociations.begin(); it != mAssociations.end(); ++it)
  {
    delete (*it);
  }
  mAssociations.clear();
}

/*
 * Predicate returning @c true if this abstract Association is of type FbcAnd
 */
bool
Association::isFbcAnd() const
{
  return dynamic_cast<const FbcAnd*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract Association is of type FbcOr
 */
bool
Association::isFbcOr() const
{
  return dynamic_cast<const FbcOr*>(this) != NULL;
}


/*
 * Predicate returning @c true if this abstract Association is of type
 * GeneProductRef
 */
bool
Association::isGeneProductRef() const
{
  return dynamic_cast<const GeneProductRef*>(this) != NULL;
}


AssociationTypeCode_t 
Association::getType () const
{
  return mType;
}

bool 
Association::isSetType () const
{
  return mType != UNKNOWN_ASSOCIATION;
}

int 
Association::setType (const AssociationTypeCode_t type)
{
  mType=type;
  return LIBSBML_OPERATION_SUCCESS;
}

int 
Association::unsetType ()
{
  mType=UNKNOWN_ASSOCIATION;
  return LIBSBML_OPERATION_SUCCESS;
}

const std::string& 
Association::getReference () const
{
  return mReference;
}

bool 
Association::isSetReference () const
{
  return !mReference.empty();
}

int 
Association::setReference (const std::string& reference)
{
  if (mType == GENE_ASSOCIATION && mAssociations.size() == 0)
  {
    mReference = reference;
    return LIBSBML_OPERATION_SUCCESS;
  }
  return LIBSBML_OPERATION_FAILED;
}

int 
Association::unsetReference ()
{
  mReference.erase();
  if (mReference.empty())
    return LIBSBML_OPERATION_SUCCESS;
  return LIBSBML_OPERATION_FAILED;

}

Association* toAssociation(const ASTNode* node);

void addChildren(Association* association, const ASTNode* node, const ASTNode *current)
{

  if (node->getType() == AST_TIMES || node->getType() == AST_PLUS)
  {
  for (unsigned int i = 0; i < node->getNumChildren(); ++i)
  {
    ASTNode* astChild = node->getChild(i);
    if (astChild->getType() == current->getType())
    {
      addChildren(association, astChild, node);
      continue;
    }
    
    Association* child = toAssociation(astChild);
    if (child == NULL)
      continue;
    association->addAssociation(*child);
    delete child;
  }
  }
  else{
    Association* child = toAssociation(node);
    if (child == NULL)
      return;
    association->addAssociation(*child);
    
  }

  
}

Association* toAssociation(const ASTNode* node)
{
  if (node == NULL)
    return NULL;

  if (node->getType() == AST_NAME)
  {
    Association* a = new Association();
    a->setType(GENE_ASSOCIATION);
    std::string name = node->getName();
    replaceAllSubStrings(name, "__MINUS__", "-");
    replaceAllSubStrings(name, "__COLON__", ":");
    replaceAllSubStrings(name, "__DOT__", ".");
    replaceAllSubStrings(name, "__ONE__"  ,"1" );
    replaceAllSubStrings(name, "__TWO__"  ,"2" );
    replaceAllSubStrings(name, "__THREE__","3"  );
    replaceAllSubStrings(name, "__FOUR__" ,"4" );
    replaceAllSubStrings(name, "__FIVE__" ,"5" );
    replaceAllSubStrings(name, "__SIX__"  ,"6"  );
    replaceAllSubStrings(name, "__SEVEN__","7" );
    replaceAllSubStrings(name, "__EIGHT__","8" );
    replaceAllSubStrings(name, "__NINE__" ,"9" );
    replaceAllSubStrings(name, "__ZERO__" ,"0" );
    a->setReference(name);
    return a;
  }
  else if (node->getType() == AST_PLUS)
  {
    Association* a = new Association();
    a->setType(OR_ASSOCIATION);
    addChildren(a, node, node);
    return a;
  }
  else if (node->getType() == AST_TIMES)
  {
    Association* a = new Association();
    a->setType(AND_ASSOCIATION);
    addChildren(a, node, node);
    return a;
  }
  return NULL;
}

Association*
Association::parseInfixAssociation(const std::string& association)
{
  std::string tweaked(association);
  replaceAllSubStrings(tweaked, " and ", " * ");
  replaceAllSubStrings(tweaked, " AND ", " * ");
  replaceAllSubStrings(tweaked, " or ", " + ");
  replaceAllSubStrings(tweaked, " OR ", " + ");
  replaceAllSubStrings(tweaked, "-", "__MINUS__");
  replaceAllSubStrings(tweaked, ":", "__COLON__");
  replaceAllSubStrings(tweaked, ".", "__DOT__");
  replaceAllSubStrings(tweaked, "1", "__ONE__");
  replaceAllSubStrings(tweaked, "2", "__TWO__");
  replaceAllSubStrings(tweaked, "3", "__THREE__");
  replaceAllSubStrings(tweaked, "4", "__FOUR__");
  replaceAllSubStrings(tweaked, "5", "__FIVE__");
  replaceAllSubStrings(tweaked, "6", "__SIX__");
  replaceAllSubStrings(tweaked, "7", "__SEVEN__");
  replaceAllSubStrings(tweaked, "8", "__EIGHT__");
  replaceAllSubStrings(tweaked, "9", "__NINE__");
  replaceAllSubStrings(tweaked, "0", "__ZERO__");
  
  ASTNode* node = SBML_parseFormula(tweaked.c_str());

  if (node == NULL)
    return NULL;
  
  Association* result = toAssociation(node);
  
  delete node;
  
  return result;
}



std::string
Association::toInfix() const
{
  if (mType == GENE_ASSOCIATION) return mReference;
  
  if (mType == OR_ASSOCIATION && mAssociations.size() > 0)
  {
    stringstream str;
    str << "(";
    str << mAssociations[0]->toInfix();
    for (size_t pos = 1; pos < mAssociations.size(); ++pos)
    {
      str << " or ";
      str << mAssociations[pos]->toInfix();
    }
    str << ")";
    return str.str();
  }
  
  if (mType == AND_ASSOCIATION  && mAssociations.size() > 0)
  {
    stringstream str;
    str << "(";
    str << mAssociations[0]->toInfix();
    for (size_t pos = 1; pos < mAssociations.size(); ++pos)
    {
      str << " and ";
      str << mAssociations[pos]->toInfix();
    }
    str << ")";
    return str.str();
    
  }
  return "";
}


int
Association::addGene(const std::string& id)
{
  if (mType == OR_ASSOCIATION || mType == AND_ASSOCIATION)
  {
    Association *geneAssociation = new Association();
    geneAssociation->setType(GENE_ASSOCIATION);
    geneAssociation->setReference(id);
    mAssociations.push_back(geneAssociation);
    return LIBSBML_OPERATION_SUCCESS;
  }
  return LIBSBML_OPERATION_FAILED;
}

unsigned int 
Association::getNumAssociations()
{
  return (unsigned int)(mAssociations.size());
}

int 
Association::addAssociation(Association &association)
{
  if (mType == OR_ASSOCIATION || mType == AND_ASSOCIATION)
  {
    mAssociations.push_back(association.clone());
    return LIBSBML_OPERATION_SUCCESS;
  }
  return LIBSBML_OPERATION_FAILED;
}

int 
Association::removeAssociation(int index)
{  
  if (index >= 0 && (unsigned int)index < mAssociations.size())
  {
    mAssociations.erase(mAssociations.begin() + index);
    return LIBSBML_OPERATION_SUCCESS;
  }
  return LIBSBML_OPERATION_FAILED;
}

int 
Association::clearAssociations()
{
  mAssociations.clear();
  if (mAssociations.empty())
    return LIBSBML_OPERATION_SUCCESS;
  return LIBSBML_OPERATION_FAILED;
}

Association* 
Association::createAnd()
{
  Association* anAnd = new Association();
  anAnd->setType(AND_ASSOCIATION);
  return anAnd;
}
  
Association* 
Association::createOr()
{
  Association* anOr = new Association();
  anOr->setType(OR_ASSOCIATION);
  return anOr;
}

Association* 
Association::createGene(const std::string reference /*= "" */)
{
  Association* gene = new Association();
  gene ->setType(AND_ASSOCIATION);
  gene->setReference(reference);
  return gene ;
}

/*
 * Returns the XML element name of
 * this SBML object.
 */
const std::string&
Association::getElementName () const
{
  static const std::string gene = "gene";
  static const std::string sAnd = "and";
  static const std::string sOr = "or";
  static const std::string name = "association";
  if (mType == GENE_ASSOCIATION)
    return gene;
  if (mType == AND_ASSOCIATION)
    return sAnd;
  if (mType == OR_ASSOCIATION)
    return sOr;
  return name;
}

/*
 * Creates an XMLNode object from this.
 */
XMLNode Association::toXML() const
{
  XMLNamespaces xmlns = XMLNamespaces();
  XMLTriple triple = XMLTriple(getElementName(), "", "");
  XMLAttributes att = XMLAttributes();
  if(mType == GENE_ASSOCIATION)
  {
    att.add("reference",this->getReference());
  }
  XMLToken token = XMLToken(triple, att, xmlns);
  XMLNode node(token);
  // add the notes and annotations
  if(this->mNotes) node.addChild(*this->mNotes);
  if(this->mAnnotation) node.addChild(*this->mAnnotation);

  std::vector<Association*>::const_iterator it;
  for (it = mAssociations.begin(); it != mAssociations.end(); ++it)
  {
    node.addChild((*it)->toXML());
  }
  return node;
}


/** @cond doxygenLibsbmlInternal */
SBase*
Association::createObject (XMLInputStream& stream)
{
  SBase* object = NULL;


  if (mType == GENE_ASSOCIATION) return NULL;

  const string& name = stream.peek().getName();
  if (name == "gene" || name == "or" || name == "and")
  {
    Association* association = new Association(getLevel(), getVersion());
    if (name == "gene")
      association->setType(GENE_ASSOCIATION);
    else if (name == "and")
      association->setType(AND_ASSOCIATION);
    else if (name == "or")
      association->setType(OR_ASSOCIATION);

    mAssociations.push_back(association);
    object = association;
  }
  return object;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
void
Association::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  if (mType == GENE_ASSOCIATION)
  attributes.add("reference");
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
Association::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  if (mType != GENE_ASSOCIATION) return;

  bool assigned = attributes.readInto("reference", mReference, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mReference.empty())
  {
    logEmptyString(mReference, sbmlLevel, sbmlVersion, "<gene>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mReference)) 
    logError(InvalidIdSyntax, sbmlLevel, sbmlVersion,
    "The syntax of the attribute reference='" + mReference + "' does not conform.");

}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
Association::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (mType == GENE_ASSOCIATION)
  stream.writeAttribute("reference",   getPrefix(), mReference);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionAttributes(stream);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
Association::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  
  std::vector<Association*>::const_iterator it;
  for (it = mAssociations.begin(); it!= mAssociations.end(); ++it)
  {
    (*it)->write(stream);
  }
  
  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}
/** @endcond */


/*
 * @return the typecode (int) of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
int
Association::getTypeCode () const
{
  return SBML_FBC_ASSOCIATION;
}

Association*
Association::clone() const
{
    return new Association(*this);
}


/** @cond doxygenLibsbmlInternal */
bool
Association::accept (SBMLVisitor&) const
{
  return false;
}
/** @endcond */
/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Association.
 */
int
Association::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Association.
 */
int
Association::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Association.
 */
int
Association::getAttribute(const std::string& attributeName,
                          double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Association.
 */
int
Association::getAttribute(const std::string& attributeName,
                          unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Association.
 */
int
Association::getAttribute(const std::string& attributeName,
                          std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this Association's attribute "attributeName"
 * is set.
 */
bool
Association::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Association.
 */
int
Association::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Association.
 */
int
Association::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Association.
 */
int
Association::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Association.
 */
int
Association::setAttribute(const std::string& attributeName,
                          unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Association.
 */
int
Association::setAttribute(const std::string& attributeName,
                          const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this Association.
 */
int
Association::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  return value;
}

/** @endcond */

#endif /* __cplusplus */


/*
 * Creates a new FbcAnd (Association_t) using the given SBML Level, Version and
 * &ldquo;fbc&rdquo; package version.
 */
LIBSBML_EXTERN
FbcAnd_t *
Association_createFbcAnd(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion)
{
  return new FbcAnd(level, version, pkgVersion);
}


/*
 * Creates a new FbcOr (Association_t) using the given SBML Level, Version and
 * &ldquo;fbc&rdquo; package version.
 */
LIBSBML_EXTERN
FbcOr_t *
Association_createFbcOr(unsigned int level,
                        unsigned int version,
                        unsigned int pkgVersion)
{
  return new FbcOr(level, version, pkgVersion);
}


/*
 * Creates a new GeneProductRef (Association_t) using the given SBML Level,
 * Version and &ldquo;fbc&rdquo; package version.
 */
LIBSBML_EXTERN
GeneProductRef_t *
Association_createGeneProductRef(unsigned int level,
                                 unsigned int version,
                                 unsigned int pkgVersion)
{
  return new GeneProductRef(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this Association_t object.
 */
LIBSBML_EXTERN
Association_t*
Association_clone(const Association_t* a)
{
  if (a != NULL)
  {
    return static_cast<Association_t*>(a->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this Association_t object.
 */
LIBSBML_EXTERN
void
Association_free(Association_t* a)
{
  if (a != NULL)
  {
    delete a;
  }
}


/*
 * Predicate returning @c 1 if this Association_t is of type FbcAnd_t
 */
LIBSBML_EXTERN
int
Association_isFbcAnd(const Association_t * a)
{
  return (a != NULL) ? static_cast<int>(a->isFbcAnd()) : 0;
}


/*
 * Predicate returning @c 1 if this Association_t is of type FbcOr_t
 */
LIBSBML_EXTERN
int
Association_isFbcOr(const Association_t * a)
{
  return (a != NULL) ? static_cast<int>(a->isFbcOr()) : 0;
}


/*
 * Predicate returning @c 1 if this Association_t is of type GeneProductRef_t
 */
LIBSBML_EXTERN
int
Association_isGeneProductRef(const Association_t * a)
{
  return (a != NULL) ? static_cast<int>(a->isGeneProductRef()) : 0;
}


LIBSBML_CPP_NAMESPACE_END
