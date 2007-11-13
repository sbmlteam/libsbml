/**
 * @file    CVTerm.cpp
 * @brief   CVTerm I/O
 * @author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLErrorLog.h>

#include <sbml/SBase.h>

#include <sbml/SBMLErrorLog.h>

#include <sbml/util/util.h>
#include <sbml/util/List.h>

#include <sbml/annotation/CVTerm.h>


/** @cond doxygen-ignore */

using namespace std;

/** @endcond doxygen-ignore */


/**
 * create a new CVTerm
 */
CVTerm::CVTerm(QualifierType_t type)
{
  mResources = new XMLAttributes();

  mQualifier = UNKNOWN_QUALIFIER;
  mModelQualifier = BQM_UNKNOWN;
  mBiolQualifier = BQB_UNKNOWN;

  setQualifierType(type);

}


/**
 * create a new CVTerm from an XMLNode
 * this assumes that the XMLNode has a prefix 
 * that represents a CV term
 */
CVTerm::CVTerm(const XMLNode node)
{
  const string& name = node.getName();
  const string& prefix = node.getPrefix();
  XMLNode Bag = node.getChild(0);

  mResources = new XMLAttributes();

  mQualifier = UNKNOWN_QUALIFIER;
  mModelQualifier = BQM_UNKNOWN;
  mBiolQualifier = BQB_UNKNOWN;

  if (prefix == "bqbiol")
  {
    setQualifierType(BIOLOGICAL_QUALIFIER);

    if      (name == "is")      
      setBiologicalQualifierType(BQB_IS);
    else if (name == "hasPart") 
      setBiologicalQualifierType(BQB_HAS_PART);
    else if (name == "isPartOf") 
      setBiologicalQualifierType(BQB_IS_PART_OF);
    else if (name == "isVersionOf") 
      setBiologicalQualifierType(BQB_IS_VERSION_OF);
    else if (name == "hasVersion") 
      setBiologicalQualifierType(BQB_HAS_VERSION);
    else if (name == "isHomologTo") 
      setBiologicalQualifierType(BQB_IS_HOMOLOG_TO);
    else if (name == "isDescribedBy") 
      setBiologicalQualifierType(BQB_IS_DESCRIBED_BY);
    else if (name == "isEncodedBy") 
      setBiologicalQualifierType(BQB_IS_ENCODED_BY);
    else if (name == "encodes") 
      setBiologicalQualifierType(BQB_ENCODES);


  }
  else if (prefix == "bqmodel")
  {
    setQualifierType(MODEL_QUALIFIER);
    
    if      (name == "is")      
      setModelQualifierType(BQM_IS);
    else if (name == "isDescribedBy") 
      setModelQualifierType(BQM_IS_DESCRIBED_BY);
  }


  for (unsigned int n = 0; n < Bag.getNumChildren(); n++)
  {
    for (int b = 0; b < Bag.getChild(n).getAttributes().getLength(); b++)
    {
      addResource(Bag.getChild(n).getAttributes().getValue(b));
    }
  }

}


/**
 * destructor
 */
CVTerm::~CVTerm()
{
  delete mResources;
}

/**
 * Copy constructor; creates a copy of a CVTerm.
 * 
 * @param orig the CVTerm instance to copy.
 */
CVTerm::CVTerm(const CVTerm& orig):
          mQualifier       (orig.mQualifier)
        , mModelQualifier  (orig.mModelQualifier)
        , mBiolQualifier   (orig.mBiolQualifier)
{
  //if (orig.mResources.isEmpty())
  //  mResources = new XMLAttributes();
  //else
  //  mResources = new XMLAttributes(orig.getResources());
  mResources=new XMLAttributes(*orig.mResources);
}

/**
 * Assignment operator for CVTerm.
 */
CVTerm& 
CVTerm::operator=(const CVTerm& rhs)
{
  mQualifier       = rhs.mQualifier;
  mModelQualifier  = rhs.mModelQualifier;
  mBiolQualifier   = rhs.mBiolQualifier;

  mResources=new XMLAttributes(*rhs.mResources);
  return *this;
}





/**
  * clones the CVTerm
  */  
CVTerm* CVTerm::clone() const
{
    CVTerm* term=new CVTerm(*this);
    term->mResources=new XMLAttributes(*this->mResources);
    return term;
}


/**
 * set the qualifier type
 */
void 
CVTerm::setQualifierType(QualifierType_t type)
{
  mQualifier = type;
}


/**
 * set the model qualifier type
 * this should be consistent with the mQualifier == MODEL_QUALIFIER
 */
void 
CVTerm::setModelQualifierType(ModelQualifierType_t type)
{
  if (mQualifier == MODEL_QUALIFIER)
  {
    mModelQualifier = type;
  }
  else
  {
    mModelQualifier = BQM_UNKNOWN;
  }
}


/**
 * set the biological qualifier type
 * this should be consistent with the mQualifier == BIOLOGICAL_QUALIFIER
 */
void 
CVTerm::setBiologicalQualifierType(BiolQualifierType_t type)
{
  if (mQualifier == BIOLOGICAL_QUALIFIER)
  {
    mBiolQualifier = type;
  }
  else
  {
    mBiolQualifier = BQB_UNKNOWN;
  }
}

/**
 * gets the Qualifier type
 */
QualifierType_t 
CVTerm::getQualifierType()
{
  return mQualifier;
}


/**
 * gets the Model Qualifier type
 */
ModelQualifierType_t 
CVTerm::getModelQualifierType()
{
  return mModelQualifier;
}


/**
 * gets the biological Qualifier type
 */
BiolQualifierType_t 
CVTerm::getBiologicalQualifierType()
{
  return mBiolQualifier;
}


/**
 * gets the resources
 */
XMLAttributes * 
CVTerm::getResources()
{
  return mResources;
}

/**
 * gets the resources
 */
const XMLAttributes * 
CVTerm::getResources() const
{
  return mResources;
}


/**
  * adds a resource to the term
  */
void 
CVTerm::addResource(std::string resource)
{
  mResources->addResource("rdf:resource", resource);
}


/**
  * removes a resource to the term
  */
void 
CVTerm::removeResource(std::string resource)
{
  for (int n = 0; n < mResources->getLength(); n++)
  {
    if (resource == mResources->getValue(n))
    {
      mResources->removeResource(n);
    }
  }

  if (mResources->getLength() == 0)
  {
    if (getQualifierType() == MODEL_QUALIFIER)
    {
      setModelQualifierType(BQM_UNKNOWN);
      setQualifierType(UNKNOWN_QUALIFIER);
    }
    else
    {
      setBiologicalQualifierType(BQB_UNKNOWN);
      setQualifierType(UNKNOWN_QUALIFIER);
    }
  }
}





/** @cond doxygen-c-only */


/**
 * Creates a new CVTerm_t with the given @p QualifierType and returns a 
 * pointer to it.
 *
 * The possible QualifierTypes are MODEL_QUALIFIER and BIOLOGICAL_QUALIFIER.  
 *
 * @param type a QualifierType_t
 *
 * @return a pointer to the newly created CVTerm_t structure.
 */
LIBSBML_EXTERN
CVTerm_t*
CVTerm_createWithQualifierType(QualifierType_t type)
{
  return new(nothrow) CVTerm(type);
}

/**
 * Create a new CVTerm_t from the given XMLNode_t and returns a 
 * pointer to it.
 *
 * RDFAnnotations within a model are stored as a List of CVTerms.  This allows
 * the user to interact with the %CVTerms directly.  When LibSBML reads in a 
 * model containing RDFAnnotations it parses them into a %List of CVTerms and
 * when writing a model it parses the CVTerms into the appropriate annotation
 * structure.  This function creates a %CVTerm from the %XMLNode supplied.
 *
 * @param node an %XMLNode_t representing a %CVTerm_t.
 *
 * @return a pointer to the newly created CVTerm_t structure.
 *
 * @note this method assumes that the %XMLNode_t is of the correct form
 */
LIBSBML_EXTERN
CVTerm_t*
CVTerm_createFromNode(const XMLNode_t *node)
{
  return new(nothrow) CVTerm(*node);
}


/**
 * Frees the given CVTerm_t structure.
 *
 * @param term the CVTerm_t structure to be freed.
 */
LIBSBML_EXTERN
void
CVTerm_free(CVTerm_t * term)
{
  delete static_cast<CVTerm*>(term);
}

/**
 * Creates a deep copy of the given CVTerm_t structure
 * 
 * @param p the CVTerm_t structure to be copied
 * 
 * @return a (deep) copy of the given CVTerm_t structure.
 */
LIBSBML_EXTERN
CVTerm_t *
CVTerm_clone (const CVTerm_t* c)
{
  return static_cast<CVTerm*>( c->clone() );
}


/**
 * Takes a CVTerm_t structure and returns the QualifierType.
 *
 * @param term the CVTerm_t structure whose QualifierType is sought
 *
 * @return the QualifierType_t of this CVTerm_t or UNKNOWN_QUALIFIER (default).
 */
LIBSBML_EXTERN
QualifierType_t 
CVTerm_getQualifierType(CVTerm_t * term)
{
  return term->getQualifierType();
}

/**
 * Takes a CVTerm_t structure and returns the ModelQualifierType.
 *
 * @param term the CVTerm_t structure whose ModelQualifierType is sought.
 *
 * @return the ModelQualifierType_t of this CVTerm_t or BQM_UNKNOWN (default).
 */
LIBSBML_EXTERN
ModelQualifierType_t 
CVTerm_getModelQualifierType(CVTerm_t * term)
{
  return term->getModelQualifierType();
}

/**
 * Takes a CVTerm_t structure and returns the BiolQualifierType.
 *
 * @param term the CVTerm_t structure whose BiolQualifierType is sought.
 *
 * @return the BiolQualifierType_t of this CVTerm_t or BQB_UNKNOWN (default).
 */
LIBSBML_EXTERN
BiolQualifierType_t 
CVTerm_getBiologicalQualifierType(CVTerm_t * term)
{
  return term->getBiologicalQualifierType();
}

/**
 * Takes a CVTerm_t structure and returns the resources.
 * 
 * @param term the CVTerm_t structure whose reources are sought.
 *
 * @return the XMLAttributes_t that store the resources of this CVTerm_t.
 */
LIBSBML_EXTERN
XMLAttributes_t * 
CVTerm_getResources(CVTerm_t * term)
{
  return term->getResources();
}

/**
 * Sets the "QualifierType_t" of this %CVTerm_t.
 *
 * @param term the CVTerm_t structure to set.
 * @param type the QualifierType_t 
 */
LIBSBML_EXTERN
void 
CVTerm_setQualifierType(CVTerm_t * term, QualifierType_t type)
{
  term->setQualifierType(type);
}


/**
 * Sets the "ModelQualifierType_t" of this %CVTerm.
 *
 * @param term the CVTerm_t structure to set.
 * @param type the ModelQualifierType_t
 *
 * @note if the QualifierType for this object is not MODEL_QUALIFIER
 * then the ModelQualifierType will default to BQM_UNKNOWN.
 */
LIBSBML_EXTERN
void 
CVTerm_setModelQualifierType(CVTerm_t * term, ModelQualifierType_t type)
{
  term->setModelQualifierType(type);
}


/**
 * Sets the "BiolQualifierType_t" of this %CVTerm_t.
 *
 * @param term the CVTerm_t structure to set.
 * @param type the BiolQualifierType_t
 *
 * @note if the QualifierType for this object is not BIOLOGICAL_QUALIFIER
 * then the BiolQualifierType_t will default to BQB_UNKNOWN.
 */
LIBSBML_EXTERN
void 
CVTerm_setBiologicalQualifierType(CVTerm_t * term, BiolQualifierType_t type)
{
  term->setBiologicalQualifierType(type);
}


/**
 * Adds a resource to the CVTerm_t.
 *
 * @param term the CVTerm_t structure to set.
 * @param resource string representing the resource 
 * e.g. http://www.geneontology.org/#GO:0005892
 *
 * @note this method adds the name "rdf:resource" to the attribute prior
 * to adding it to the resources in this CVTerm.
 */
LIBSBML_EXTERN
void 
CVTerm_addResource(CVTerm_t * term, const char * resource)
{
  term->addResource(resource);
}

/**
 * Removes a resource from the CVTerm_t.
 *
 * @param term the CVTerm_t structure.
 * @param resource string representing the resource 
 * e.g. http://www.geneontology.org/#GO:0005892
 */
LIBSBML_EXTERN
void 
CVTerm_removeResource(CVTerm_t * term, const char * resource)
{
  term->removeResource(resource);
}


/** @endcond doxygen-c-only */
