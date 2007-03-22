/**
 * \file    CVTerm.cpp
 * \brief   CVTerm I/O
 * \author  Sarah Keating
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


using namespace std;

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


  }
  else if (prefix == "bqmodel")
  {
    setQualifierType(MODEL_QUALIFIER);
    
    if      (name == "is")      
      setModelQualifierType(BQM_IS);
    else if (name == "hasPart") 
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
  * adds a resource to the term
  */
void 
CVTerm::addResource(std::string resource)
{
  mResources->addResource("rdf:resource", resource);
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
 *
 */
LIBSBML_EXTERN
CVTerm_t*
CVTerm_createWithQualifierType(QualifierType_t type)
{
  return new(nothrow) CVTerm(type);
}

/**
 *
 */
LIBSBML_EXTERN
CVTerm_t*
CVTerm_createFromNode(const XMLNode_t *node)
{
  return new(nothrow) CVTerm(*node);
}

/**
 *
 */
LIBSBML_EXTERN
void
CVTerm_free(CVTerm_t * term)
{
  delete static_cast<CVTerm*>(term);
}

/**
 * gets the Qualifier type
 */
LIBSBML_EXTERN
QualifierType_t 
CVTerm_getQualifierType(CVTerm_t * term)
{
  return term->getQualifierType();
}

/**
  * gets the Model Qualifier type
  */
LIBSBML_EXTERN
ModelQualifierType_t 
CVTerm_getModelQualifierType(CVTerm_t * term)
{
  return term->getModelQualifierType();
}

/**
  * gets the biological Qualifier type
  */
LIBSBML_EXTERN
BiolQualifierType_t 
CVTerm_getBiologicalQualifierType(CVTerm_t * term)
{
  return term->getBiologicalQualifierType();
}

/**
* gets the resources
*/
LIBSBML_EXTERN
XMLAttributes_t * 
CVTerm_getResources(CVTerm_t * term)
{
  return term->getResources();
}

/**
  * set the qualifier type
  */
LIBSBML_EXTERN
void 
CVTerm_setQualifierType(CVTerm_t * term, QualifierType_t type)
{
  term->setQualifierType(type);
}


/**
  * set the model qualifier type
  * this should be consistent with the mQualifier == MODEL_QUALIFIER
  */
LIBSBML_EXTERN
void 
CVTerm_setModelQualifierType(CVTerm_t * term, ModelQualifierType_t type)
{
  term->setModelQualifierType(type);
}


/**
  * set the biological qualifier type
  * this should be consistent with the mQualifier == BIOLOGICAL_QUALIFIER
  */
LIBSBML_EXTERN
void 
CVTerm_setBiologicalQualifierType(CVTerm_t * term, BiolQualifierType_t type)
{
  term->setBiologicalQualifierType(type);
}


/**
  * adds a resource to the term
  */
LIBSBML_EXTERN
void 
CVTerm_addResource(CVTerm_t * term, const char * resource)
{
  term->addResource(resource);
}

