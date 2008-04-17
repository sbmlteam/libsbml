/**
 * @file    SpeciesReference.cpp
 * @brief   Implementation of SimpleSpeciesReference, SpeciesReference,
 *          ModifierSpeciesReference, and ListOfSpeciesReferences. 
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/


#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/math/FormulaParser.h>
#include <sbml/math/MathML.h>
#include <sbml/math/ASTNode.h>

#ifdef USE_LAYOUT
#include <sbml/annotation/LayoutAnnotation.h>
#endif // USE_LAYOUT

#include <sbml/SBO.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLError.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/SpeciesReference.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/*
 * Creates a new SimpleSpeciesReference, optionally with its species
 * attribute set.
 */
SimpleSpeciesReference::SimpleSpeciesReference (const std::string& species) :
   SBase (-1)
 , mSpecies( species )
{
}


/*
 * Destroys this SimpleSpeciesReference.
 */
SimpleSpeciesReference::~SimpleSpeciesReference ()
{
}


/*
 * Copy constructor. Creates a copy of this SimpleSpeciesReference.
 */
SimpleSpeciesReference::SimpleSpeciesReference(const SimpleSpeciesReference& orig) :
  SBase     (orig)
, mSpecies  (orig.mSpecies)
{
}


/*
 * Assignment operator.
 */
SimpleSpeciesReference& SimpleSpeciesReference::operator=(const SimpleSpeciesReference& rhs)
{
  this->SBase::operator =(rhs);
  mSpecies = rhs.mSpecies;
  return *this;
}


/*
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Reaction's next
 * SimpleSpeciesReference (if available).
 */
bool
SimpleSpeciesReference::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/*
 * @return the species for this SimpleSpeciesReference.
 */
const string&
SimpleSpeciesReference::getSpecies () const
{
  return mSpecies;
}


/*
 * @return true if the species for this SimpleSpeciesReference has been
 * set, false otherwise.
 */
bool
SimpleSpeciesReference::isSetSpecies () const
{
  return (mSpecies.empty() == false);
}


/*
 * Sets the species of this SimpleSpeciesReference to a copy of sid.
 */
void
SimpleSpeciesReference::setSpecies (const std::string& sid)
{
  mSpecies = sid;
}


/*
 * @return true if this SpeciesReference is a ModiferSpeciesReference,
 * false otherwise.
 */
bool
SimpleSpeciesReference::isModifier () const
{
  return (getTypeCode() == SBML_MODIFIER_SPECIES_REFERENCE);
}


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
SimpleSpeciesReference::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();
  const string s = (level == 1 && version == 1) ? "specie" : "species";
  expectedAttributes.push_back(s);
  expectedAttributes.push_back("stoichiometry");

  if (level == 1)
  {
    expectedAttributes.push_back("denominator");
  }
  else
  {
    expectedAttributes.push_back("metaid");
  }

  if (level == 2 && version > 1)
  {
    expectedAttributes.push_back("id");
    expectedAttributes.push_back("name");
    expectedAttributes.push_back("sboTerm");
  }

  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    if (std::find(begin, end, name) == end)
    {
      logUnknownAttribute(name, level, version, "<speciesReference>");
    }
  }


  if (level == 2 && (version == 2 || version == 3))
  {
    //
    // id: SId  { use="optional" }  (L2v2)
    //
    attributes.readInto("id" , mId, getErrorLog());
    SBase::checkIdSyntax();

    //
    // name: string  { use="optional" }  (L2v2)
    //
    attributes.readInto("name" , mName);

    //
    // sboTerm: SBOTerm { use="optional" }  (L2v2)
    //
    mSBOTerm = SBO::readTerm(attributes, this->getErrorLog());
  }

  //
  // specie : SName   { use="required" }  (L1v1)
  // species: SName   { use="required" }  (L1v2, L2v1, L2v2)
  //
  //const string s = (level == 1 && version == 1) ? "specie" : "species";
  attributes.readInto(s , mSpecies, getErrorLog(), true);
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
SimpleSpeciesReference::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();


  if (level == 2 && (version == 2 || version == 3))
  {
    //
    // id: SId  { use="optional" }  (L2v2)
    //
    stream.writeAttribute("id" , mId);

    //
    // name: string  { use="optional" }  (L2v2)
    //
    stream.writeAttribute("name" , mName);

    //
    // sboTerm: SBOTerm { use="optional" }  (L2v2)
    //
    SBO::writeTerm(stream, mSBOTerm);
  }

  //
  // specie : SName   { use="required" }  (L1v1)
  // species: SName   { use="required" }  (L1v2, L2v1, L2v2)
  //
  const string s = (level == 1 && version == 1) ? "specie" : "species";
  stream.writeAttribute(s , mSpecies);
}
/** @endcond doxygen-libsbml-internal */



/*
 * Creates a new SpeciesReference, optionally with its species,
 * stoichiometry, and denominator attributes set.
 */
SpeciesReference::SpeciesReference (  const std::string& species
                                    , double        stoichiometry
                                    , int           denominator ) :
   SimpleSpeciesReference( species       )
 , mStoichiometry        ( stoichiometry )
 , mDenominator          ( denominator   )
 , mStoichiometryMath    ( 0             )
{
}


/*
 * Destroys this SpeciesReference.
 */
SpeciesReference::~SpeciesReference ()
{
  delete mStoichiometryMath;
}


/*
 * Copy constructor. Creates a copy of this SpeciesReference.
 */
SpeciesReference::SpeciesReference (const SpeciesReference& orig) :
   SimpleSpeciesReference( orig                )
 , mStoichiometry        ( orig.mStoichiometry )
 , mDenominator          ( orig.mDenominator   )
 , mStoichiometryMath    ( 0                   )
{
  if (orig.mStoichiometryMath)
  {
    mStoichiometryMath = new StoichiometryMath(*orig.getStoichiometryMath());
  }
}


/*
 * Assignment operator
 */
SpeciesReference& SpeciesReference::operator=(const SpeciesReference& rhs)
{
  this->SBase::operator =(rhs);
  this->SimpleSpeciesReference::operator = ( rhs );
  mStoichiometry = rhs.mStoichiometry ;
  mDenominator = rhs.mDenominator   ;
  if (rhs.mStoichiometryMath)
  {
    mStoichiometryMath = new StoichiometryMath(*rhs.getStoichiometryMath());
  }
  return *this;
}


/*
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Reaction's next
 * SpeciesReference (if available).
 */
bool
SpeciesReference::accept (SBMLVisitor& v) const
{
  bool result = v.visit(*this);
  
  if (mStoichiometryMath) mStoichiometryMath->accept(v);
  
  return result;
}


/*
 * @return a (deep) copy of this SpeciesReference.
 */
SBase*
SpeciesReference::clone () const
{
  return new SpeciesReference(*this);
}


/*
 * Initializes the fields of this SpeciesReference to their defaults:
 *
 *   - stoichiometry = 1
 *   - denominator   = 1
 */
void
SpeciesReference::initDefaults ()
{
  mStoichiometry = 1.0;
  mDenominator   = 1;
}


/*
 * @return the stoichiometry of this SpeciesReference.
 */
double
SpeciesReference::getStoichiometry () const
{
  return mStoichiometry;
}


/*
 * @return the stoichiometryMath of this SpeciesReference.
 */
const StoichiometryMath*
SpeciesReference::getStoichiometryMath () const
{
  return mStoichiometryMath;
}


/*
 * @return the stoichiometryMath of this SpeciesReference.
 */
StoichiometryMath*
SpeciesReference::getStoichiometryMath ()
{
  return mStoichiometryMath;
}


/*
 * @return the denominator of this SpeciesReference.
 */
int
SpeciesReference::getDenominator () const
{
  return mDenominator;
}


/*
 * @return true if the stoichiometryMath of this SpeciesReference has been
 * set, false otherwise.
 */
bool
SpeciesReference::isSetStoichiometryMath () const
{
  return (mStoichiometryMath != NULL);
}


/*
 * Sets the stoichiometry of this SpeciesReference to value.
 */
void
SpeciesReference::setStoichiometry (double value)
{
   unsetStoichiometryMath();

   mStoichiometry = value;
}


/*
 * Sets the stoichiometryMath of this SpeciesReference to a copy of the
 * given ASTNode.
 */
void
SpeciesReference::setStoichiometryMath (const StoichiometryMath* math)
{
  mStoichiometry = 1;
  if (mStoichiometryMath == math) return;


  delete mStoichiometryMath;
  mStoichiometryMath = (math != 0) ? static_cast<StoichiometryMath*>(math->clone()) : 0;
}


/*
 * Sets the denominator of this SpeciesReference to value.
 */
void
SpeciesReference::setDenominator (int value)
{
  mDenominator = value;
}


/*
 * Unsets the "stoichiometryMath" subelement of this SpeciesReference.
 */
void 
SpeciesReference::unsetStoichiometryMath ()
{
  delete mStoichiometryMath;
  mStoichiometryMath = 0;
}





/*
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
SpeciesReference::getTypeCode () const
{
  return SBML_SPECIES_REFERENCE;
}


/*
 * Sets the annotation of this SBML object to a copy of annotation.
 */
void
SpeciesReference::setAnnotation (const XMLNode* annotation)
{
  SBase::setAnnotation(annotation);

#ifdef USE_LAYOUT
  if(this->getLevel()==1 || (this->getLevel()==2 && this->getVersion()==1))
  {
    // clear existing SBase::mID 
    setId("");

    if(mAnnotation)
    {
      // parse mAnnotation (if any) and set mId 
      parseSpeciesReferenceAnnotation(mAnnotation,*this);
    }
  }
#endif // USE_LAYOUT
}


/*
 * Sets the annotation (by string) of this SBML object to a copy of annotation.
 */
void
SpeciesReference::setAnnotation (const std::string& annotation)
{
  if(annotation.empty())
  {
    unsetAnnotation();
    return;
  }

  XMLNode* annt_xmln;
  if (getSBMLDocument())
  {
    XMLNamespaces* xmlns = getSBMLDocument()->getNamespaces();
    annt_xmln = XMLNode::convertStringToXMLNode(annotation,xmlns);
  }
  else
  {
    annt_xmln = XMLNode::convertStringToXMLNode(annotation);
  }

  if(annt_xmln)
  {
    setAnnotation(annt_xmln);
    delete annt_xmln;
  }
}


/*
 * Appends annotation to the existing annotations.
 * This allows other annotations to be preserved whilst
 * adding additional information.
 */
void
SpeciesReference::appendAnnotation (const XMLNode* annotation)
{
  if(!annotation) return;

  XMLNode* new_annotation = NULL;

#ifdef USE_LAYOUT
  if(this->getLevel()==1 || (this->getLevel()==2 && this->getVersion()==1))
  {
    XMLNode* new_annotation = NULL;
    const string&  name = annotation->getName();

    // check for annotation tags and add if necessary 
    if (name != "annotation")
    {
      XMLToken ann_t = XMLToken(XMLTriple("annotation", "", ""), XMLAttributes());
      new_annotation = new XMLNode(ann_t);
      new_annotation->addChild(*annotation);
    }
    else
    {
      new_annotation = annotation->clone();
    }

    // parse new_annotation and reset SBase::mId 
    parseSpeciesReferenceAnnotation(new_annotation,*this);

    // delete mId from new_annotation 
//    XMLNode* tmp_annotation=deleteLayoutIdAnnotation(new_annotation);
//    delete new_annotation;
//    new_annotation = tmp_annotation;
  }
#endif // USE_LAYOUT
  SBase::appendAnnotation(new_annotation);

  delete new_annotation;
}

/*
 * Appends annotation (by string) to the existing annotations.
 * This allows other annotations to be preserved whilst
 * adding additional information.
 */
void
SpeciesReference::appendAnnotation (const std::string& annotation)
{
  XMLNode* annt_xmln;
  if (getSBMLDocument())
  {
    XMLNamespaces* xmlns = getSBMLDocument()->getNamespaces();
    annt_xmln = XMLNode::convertStringToXMLNode(annotation,xmlns);
  }
  else
  {
    annt_xmln = XMLNode::convertStringToXMLNode(annotation);
  }

  if(annt_xmln)
  {
    appendAnnotation(annt_xmln);
    delete annt_xmln;
  }
}


/*
 * @return the name of this element ie "speciesReference".
 
 */
const string&
SpeciesReference::getElementName () const
{
  static const string specie  = "specieReference";
  static const string species = "speciesReference";

  return (getLevel() == 1 && getVersion() == 1) ? specie : species;
}


/** @cond doxygen-libsbml-internal */
void
SpeciesReference::sortMath()
{
  if (mStoichiometryMath && mStoichiometryMath->getMath()->isRational())
  {
    mStoichiometry = mStoichiometryMath->getMath()->getNumerator();
    mDenominator   = mStoichiometryMath->getMath()->getDenominator();

    delete mStoichiometryMath;
    mStoichiometryMath = 0;
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
SpeciesReference::createObject (XMLInputStream& stream)
{
  const string& name = stream.peek().getName();
  
  if (name == "stoichiometryMath")
  {
    if (getLevel() == 1)
    {
      return NULL;
    }
    delete mStoichiometryMath;

    mStoichiometryMath = new StoichiometryMath();
    return mStoichiometryMath;
  }
  else
  {
    return 0;
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
SpeciesReference::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();

 // if (name == "stoichiometryMath")
 // {
 //   const XMLToken wrapperElement = stream.next();
 //   stream.skipText();
 //   const XMLToken element = stream.peek();

 //   bool found = false;

 //   /* The first element must always be 'math'. */

 //   if (element.getName() != "math")
 //   {
 //     found = true;
 //   }

 //   /* Check this declares the MathML namespace.  This may be explicitly
 //    * declared here or implicitly declared on the whole document
 //    */

 //   if (!found && element.getNamespaces().getLength() != 0)
 //   {
 //     for (int n = 0; n < element.getNamespaces().getLength(); n++)
 //     {
 //       if (!strcmp(element.getNamespaces().getURI(n).c_str(),
	//	    "http://www.w3.org/1998/Math/MathML"))
 //       {
	//  found = true;
 //         break;
 //       }
 //     }
 //   }
 //   if (!found && mSBML->getNamespaces() != 0)
 //   {
 //     /* check for implicit declaration */
 //     for (int n = 0; n < mSBML->getNamespaces()->getLength(); n++)
 //     {
	//if (!strcmp(mSBML->getNamespaces()->getURI(n).c_str(),
	//	    "http://www.w3.org/1998/Math/MathML"))
	//{
	//  found = true;
	//  break;
	//}
 //     }
 //   }

 //   if (! found)
 //   {
 //     static_cast <SBMLErrorLog*> (stream.getErrorLog())->logError(10201);
 //   }

 //   delete mStoichiometryMath;
 //   mStoichiometryMath = readMathML(stream);
 //   read               = true;

 //   stream.skipPastEnd(wrapperElement);

 //   if (mStoichiometryMath && mStoichiometryMath->isRational())
 //   {
 //     mStoichiometry = mStoichiometryMath->getNumerator();
 //     mDenominator   = mStoichiometryMath->getDenominator();

 //     delete mStoichiometryMath;
 //     mStoichiometryMath = 0;
 //   }
 // }
  //else 

  // This has to do additional work for reading annotations, so the code
  // here is copied and expanded from SBase::readNotes().

  if (name == "annotation")
  {
//    XMLNode* new_annotation = NULL;
    /* if annotation already exists then it is an error 
     */
    if (mAnnotation)
    {
      logError(NotSchemaConformant, getLevel(), getVersion(),
	       "Only one <annotation> element is permitted inside a "
	       "particular containing element.");
    }
    delete mAnnotation;
    mAnnotation = new XMLNode(stream);
    checkAnnotation();
    if (mCVTerms)
    {
      unsigned int size = mCVTerms->getSize();
      while (size--) delete static_cast<CVTerm*>( mCVTerms->remove(0) );
      delete mCVTerms;
    }
    mCVTerms = new List();
    RDFAnnotationParser::parseRDFAnnotation(mAnnotation, mCVTerms);
//    new_annotation = RDFAnnotationParser::deleteRDFAnnotation(mAnnotation);
//    delete mAnnotation;
//    mAnnotation = new_annotation;

#ifdef USE_LAYOUT
    // only parse the id annotation if it is Level 1 or Level 2 Version 1
    // everything after Level 2 Version 1 has ids.
    if(this->getLevel()==1 || (this->getLevel()==2 && this->getVersion()==1))
    {
      parseSpeciesReferenceAnnotation(mAnnotation,*this);
      checkAnnotation();

//      new_annotation=deleteLayoutIdAnnotation(mAnnotation);
//      delete mAnnotation;
//      mAnnotation = new_annotation;
    }
#endif // USE_LAYOUT
    read = true;
  }

  return read;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
SpeciesReference::readAttributes (const XMLAttributes& attributes)
{
  SimpleSpeciesReference::readAttributes(attributes);

  //
  // stoichiometry: integer  { use="optional" default="1" }  (L1v1, L1v2)
  // stoichiometry: double   { use="optional" default="1" }  (L2v1, L2v2)
  //
  attributes.readInto("stoichiometry", mStoichiometry);

  //
  // denominator: integer  { use="optional" default="1" }  (L1v1, L1v2)
  //
  if (getLevel() == 1) attributes.readInto("denominator", mDenominator);
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
SpeciesReference::writeAttributes (XMLOutputStream& stream) const
{
  SimpleSpeciesReference::writeAttributes(stream);

  if (getLevel() == 1)
  {
    //
    // stoichiometry: integer  { use="optional" default="1" }  (L1v1, L1v2)
    //
    int s = static_cast<int>( mStoichiometry );
    if (s != 1) stream.writeAttribute("stoichiometry", s);

    //
    // denominator  { use="optional" default="1" }  (L1v1, L1v2)
    //
    if (mDenominator != 1) stream.writeAttribute("denominator", mDenominator);
  }
  else
  {
    //
    // stoichiometry: double   { use="optional" default="1" }  (L2v1, L2v2)
    //
    if (mStoichiometry != 1 && mDenominator == 1)
    {
      stream.writeAttribute("stoichiometry", mStoichiometry);
    }
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
SpeciesReference::writeElements (XMLOutputStream& stream) const
{
  if ( mNotes ) stream << *mNotes;
  SpeciesReference * sr = const_cast <SpeciesReference *> (this);
  sr->syncAnnotation();
  if ( mAnnotation ) stream << *mAnnotation;

  if (getLevel() == 2)
  {
    if (mStoichiometryMath || mDenominator != 1)
    {
      if (mStoichiometryMath) 
      {
        mStoichiometryMath->write(stream);
      }
      else
      {
        ASTNode node;
        node.setValue(static_cast<long>(mStoichiometry), mDenominator);

        stream.startElement("stoichiometryMath");
        writeMathML(&node, stream);
        stream.endElement("stoichiometryMath");
      }
    }
  }

}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Synchronizes the annotation of this SBML object.
 */
void
SpeciesReference::syncAnnotation ()
{
  SBase::syncAnnotation();

#ifdef USE_LAYOUT
  if (getLevel() == 2)
  {
    if(this->getLevel()==1 || (this->getLevel()==2 && this->getVersion()==1))
    {
      if(mAnnotation)
      {
        XMLNode* new_annotation = deleteLayoutIdAnnotation(mAnnotation);
        delete mAnnotation;
        mAnnotation = new_annotation;
      }

      if (this->isSetId())
      {
        XMLNode * idAnnotation = parseLayoutId(this);
        if (idAnnotation)
        {
          if (!mAnnotation)
          {
            mAnnotation = idAnnotation;
          }
          else
          {
            if (mAnnotation->isEnd())
            {
              mAnnotation->unsetEnd();
            }
            mAnnotation->addChild(idAnnotation->getChild(0));
          }
        }

      }
    }
  }
#endif // USE_LAYOUT
}
/** @endcond doxygen-libsbml-internal */


/*
 * Creates a new ModifierSpeciesReference, optionally with its species
 * attribute set.
 */
ModifierSpeciesReference::ModifierSpeciesReference (const std::string& species) :
  SimpleSpeciesReference(species)
{
}


/*
 * Destroys this ModifierSpeciesReference.
 */
ModifierSpeciesReference::~ModifierSpeciesReference ()
{
}


/*
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Reaction's next
 * ModifierSpeciesReference (if available).
 */
bool
ModifierSpeciesReference::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/*
 * @return a (deep) copy of this ModifierSpeciesReference.
 */
SBase*
ModifierSpeciesReference::clone () const
{
  return new ModifierSpeciesReference(*this);
}


/*
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
ModifierSpeciesReference::getTypeCode () const
{
  return SBML_MODIFIER_SPECIES_REFERENCE;
}


/*
 * @return the name of this element ie "modifierSpeciesReference".
 
 */
const string&
ModifierSpeciesReference::getElementName () const
{
  static const string name = "modifierSpeciesReference";
  return name;
}


#ifdef USE_LAYOUT

/*
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
ModifierSpeciesReference::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();

  // This has to do additional work for reading annotations, so the code
  // here is copied and expanded from SBase::readNotes().

  if (name == "annotation")
  {
//    XMLNode* new_annotation = NULL;
    /* if annotation already exists then it is an error 
     */
    if (mAnnotation)
    {
      logError(NotSchemaConformant, getLevel(), getVersion(),
	       "Only one <annotation> element is permitted inside a "
	       "particular containing element.");
    }
    delete mAnnotation;
    mAnnotation = new XMLNode(stream);
    checkAnnotation();
    if (mCVTerms)
    {
      unsigned int size = mCVTerms->getSize();
      while (size--) delete static_cast<CVTerm*>( mCVTerms->remove(0) );
      delete mCVTerms;
    }
    mCVTerms = new List();
    RDFAnnotationParser::parseRDFAnnotation(mAnnotation, mCVTerms);
//    new_annotation = RDFAnnotationParser::deleteRDFAnnotation(mAnnotation);
//    delete mAnnotation;
//    mAnnotation = new_annotation;
    
    if(this->getLevel()==1 || (this->getLevel()==2 && this->getVersion()==1))
    {
      parseSpeciesReferenceAnnotation(mAnnotation,*this);
//      new_annotation=deleteLayoutIdAnnotation(mAnnotation);
//      delete mAnnotation;
//      mAnnotation = new_annotation;
    }
    read=true;
  }

  return read;
}

/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
ModifierSpeciesReference::writeElements (XMLOutputStream& stream) const
{
  if(this->getLevel()==1 || (this->getLevel()==2 && this->getVersion()==1))
  {
    if (this->isSetId())
    {
      ModifierSpeciesReference * sr = const_cast <ModifierSpeciesReference *> (this);
      XMLNode * idAnnotation = parseLayoutId(this);
      if(!mAnnotation)
      {
        if (idAnnotation) static_cast <SBase *> (sr)->setAnnotation(idAnnotation);
      }
      else
      {
        if (idAnnotation) static_cast <SBase *> (sr)->appendAnnotation(idAnnotation);
      }
    }
  }
  SBase::writeElements(stream);    
  
}

#endif // USE_LAYOUT


/*
 * Creates a new ListOfSpeciesReferences.
 */
ListOfSpeciesReferences::ListOfSpeciesReferences () : mType(Unknown)
{
}


/*
 * @return a (deep) copy of this ListOfUnits.
 */
SBase*
ListOfSpeciesReferences::clone () const
{
  return new ListOfSpeciesReferences(*this);
}


/*
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOfSpeciesReferences::getItemTypeCode () const
{
  if (mType == Reactant || mType == Product)
  {
    return SBML_SPECIES_REFERENCE;
  }
  else if (mType == Modifier)
  {
    return SBML_MODIFIER_SPECIES_REFERENCE;
  }
  else
  {
    return SBML_UNKNOWN;
  }
}


/*
 * @return the name of this element ie "listOfReactants" or "listOfProducts" etc.
 */
const string&
ListOfSpeciesReferences::getElementName () const
{
  static const string unknown   = "listOfUnknowns";
  static const string reactants = "listOfReactants";
  static const string products  = "listOfProducts";
  static const string modifiers = "listOfModifiers";

       if (mType == Reactant) return reactants;
  else if (mType == Product ) return products;
  else if (mType == Modifier) return modifiers;
  else return unknown;
}


/** @cond doxygen-libsbml-internal */
/*
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
ListOfSpeciesReferences::getElementPosition () const
{
  int position;

  switch (mType)
  {
    case Reactant: position =  1; break;
    case Product:  position =  2; break;
    case Modifier: position =  3; break;
    default:       position = -1; break;
  }

  return position;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * Sets type of this ListOfSpeciesReferences.
 */
void
ListOfSpeciesReferences::setType (SpeciesType type)
{
  mType = type;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfSpeciesReferences::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (mType == Reactant || mType == Product)
  {
    if (name == "speciesReference" || name == "specieReference")
    {
      object = new SpeciesReference();
    }
    else
    {
      /* create the object anyway - or will also get unrecognized element message 
       * which is confusion if user has merely reversed modifierSpeciesReference
       * and speciesReference */
      object = new SpeciesReference();
      logError(InvalidReactantsProductsList);
    }
  }
  else if (mType == Modifier)
  {
    if (name == "modifierSpeciesReference")
    {
      object = new ModifierSpeciesReference();
    }
    else
    {
      object = new ModifierSpeciesReference();
      logError(InvalidModifiersList);
    }
  }

  if (object) mItems.push_back(object);

  return object;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-c-only */



/**
 * Creates a new, empty SpeciesReference_t structure and returns a pointer
 * to it.
 *
 * Note that the "species" attribute on SpeciesReference and
 * ModifierSpeciesReference is required to have a value in SBML.  Although
 * the attribute is optional in this constructor, callers should provide a
 * value or use SpeciesReference_setSpecies() shortly after creating the
 * structure.
 *
 * @return the SpeciesReference_t structure created.
 */
LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_create (void)
{
  return new(nothrow) SpeciesReference;
}


/**
 * Creates a new, empty ModifierSpeciesReference_t structure and returns a
 * pointer to it.
 *
 * Note that the "species" attribute on ModifierSpeciesReference and
 * ModifierModifierSpeciesReference is required to have a value in SBML.
 * Although the attribute is optional in this constructor, callers should
 * provide a value or use ModifierSpeciesReference_setModifierSpecies()
 * shortly after creating the structure.
 *
 * @return the ModifierSpeciesReference_t structure created.
 */
LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_createModifier (void)
{
  return new(nothrow) ModifierSpeciesReference;
}


/**
 * Creates a new, empty SpeciesReference_t structure with values for the
 * "species", "stoichiometry" and "denominator" attributes.
 *
 * The "denominator" attribute is only actually written out in the case of
 * an SBML Level 1 model.  In SBML Level 2, rational-number stoichiometries
 * are written as MathML elements in the "stoichiometryMath" subelement.
 * However, as a convenience to users, libSBML allows the creation and
 * manipulation of rational-number stoichiometries by supplying the
 * numerator and denominator directly rather than having to manually create
 * an ASTNode structure.  LibSBML will write out the appropriate constructs
 * (either a combination of "stoichiometry" and "denominator" in the case
 * of SBML Level 1, or a "stoichiometryMath" subelement in the case of SBML
 * Level 2).
 *
 * @param species the identifier of a Species_t structure defined in the
 * enclosing Model_t structure
 *
 * @param stoichiometry a floating-point number for the stoichiometry, or
 * in the case of SBML Level 1, for the numerator of the stoichiometry
 *
 * @param denominator the denominator of a rational-numbered stoichiometry.
 *
 * @return the SpeciesReference_t structure created.
 */
LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_createWithSpeciesAndStoichiometry ( const char *species,
                              double      stoichiometry,
                              int         denominator )
{
  const char *s = species ? species : "";
  return new(nothrow) SpeciesReference(s, stoichiometry, denominator);
}


/**
 * Frees the given SpeciesReference_t structure.
 *
 * @param sr The SpeciesReference_t structure.
 */
LIBSBML_EXTERN
void
SpeciesReference_free (SpeciesReference_t *sr)
{
  delete sr;
}


/**
 * Creates and returns a deep copy of the given SpeciesReference_t
 * structure.
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return a (deep) copy of this SpeciesReference_t.
 */
LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_clone (const SpeciesReference_t *sr)
{
  return static_cast<SpeciesReference_t*>( sr->clone() );
}


/**
 * Initializes the attributes of the given SpeciesReference_t structure to
 * their defaults:
 *
 * @li stoichiometry is set to @c 1
 * @li denominator is set to @c 1
 *
 * This function has no effect if the SpeciesReference_t structure is a
 * modifer (see SpeciesReference_isModifier()).
 *
 * @param sr The SpeciesReference_t structure to use.
 */
LIBSBML_EXTERN
void
SpeciesReference_initDefaults (SpeciesReference_t *sr)
{
  if (sr->isModifier()) return;
  static_cast<SpeciesReference*>(sr)->initDefaults();
}


/**
 * Predicate returning @c true or @c false depending on whether the
 * given SpeciesReference_t structure is a modifier.
 * 
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return nonzero if this SpeciesReference_t represents a modifier
 * species, zero (0)if it is a plain SpeciesReference.
 */
LIBSBML_EXTERN
int
SpeciesReference_isModifier (const SpeciesReference_t *sr)
{
  return static_cast<int>( sr->isModifier() );
}


/**
 * Get the value of the "id" attribute of the given SpeciesReference_t
 * structure.
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return the identifier of the SpeciesReference_t instance.
 */
LIBSBML_EXTERN
const char *
SpeciesReference_getId (const SpeciesReference_t *sr)
{
  return sr->isSetId() ? sr->getId().c_str() : NULL;
}


/**
 * Get the value of the "name" attribute of the given SpeciesReference_t
 * structure.
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return the name of the SpeciesReference_t instance.
 */
LIBSBML_EXTERN
const char *
SpeciesReference_getName (const SpeciesReference_t *sr)
{
  return sr->isSetName() ? sr->getName().c_str() : NULL;
}


/**
 * Get the value of the "species" attribute of the given SpeciesReference_t
 * structure.
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return the "species" attribute value
 */
LIBSBML_EXTERN
const char *
SpeciesReference_getSpecies (const SpeciesReference_t *sr)
{
  return sr->isSetSpecies() ? sr->getSpecies().c_str() : NULL;
}


/**
 * Get the value of the "stoichiometry" attribute of the given
 * SpeciesReference_t structure.
 *
 * This function returns zero if the SpeciesReference_t structure is a
 * Modifer (see SpeciesReference_isModifier()).
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return the "stoichiometry" attribute value
 */
LIBSBML_EXTERN
double
SpeciesReference_getStoichiometry (const SpeciesReference_t *sr)
{
  if (sr->isModifier()) return 0.0;
  return static_cast<const SpeciesReference*>(sr)->getStoichiometry();
}


/**
 * Get the content of the "stoichiometryMath" subelement of the given
 * SpeciesReference_t structure.
 *
 * This function returns NULL if the SpeciesReference_t structure is a
 * Modifer (see SpeciesReference_isModifier()).
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return the stoichiometryMath of this SpeciesReference.
 */
LIBSBML_EXTERN
StoichiometryMath_t *
SpeciesReference_getStoichiometryMath (SpeciesReference_t *sr)
{
  if (sr->isModifier()) return NULL;
  return static_cast<SpeciesReference*>(sr)->getStoichiometryMath();
}


/**
 * Get the value of the "denominator" attribute, for the case of a
 * rational-numbered stoichiometry or a model in SBML Level 1.
 *
 * The "denominator" attribute is only actually written out in the case of
 * an SBML Level 1 model.  In SBML Level 2, rational-number stoichiometries
 * are written as MathML elements in the "stoichiometryMath" subelement.
 * However, as a convenience to users, libSBML allows the creation and
 * manipulation of rational-number stoichiometries by supplying the
 * numerator and denominator directly rather than having to manually create
 * an ASTNode structure.  LibSBML will write out the appropriate constructs
 * (either a combination of "stoichiometry" and "denominator" in the case
 * of SBML Level 1, or a "stoichiometryMath" subelement in the case of SBML
 * Level 2).
 *
 * This function returns 0 if the SpeciesReference_t structure is a Modifer (see
 * SpeciesReference_isModifier()).
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return the denominator of this SpeciesReference.
 */
LIBSBML_EXTERN
int
SpeciesReference_getDenominator (const SpeciesReference_t *sr)
{
  if (sr->isModifier()) return 0;
  return static_cast<const SpeciesReference*>(sr)->getDenominator();
}


/**
 * Predicate returning nonzero (for true) or zero (for false) depending on
 * whether the "id" attribute of the given SpeciesReference_t structure is
 * set.
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return nonzero if the "id" attribute of given SpeciesReference_t
 * structure has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReference_isSetId (const SpeciesReference_t *sr)
{
  return static_cast<int>( sr->isSetId() );
}


/**
 * Predicate returning nonzero (for true) or zero (for false) depending on
 * whether the "name" attribute of the given SpeciesReference_t
 * structure is set.
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return nonzero if the "name" attribute of given SpeciesReference_t
 * structure has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReference_isSetName (const SpeciesReference_t *sr)
{
  return static_cast<int>( sr->isSetName() );
}


/**
 * Predicate returning nonzero (for true) or zero (for false) depending on
 * whether the "species" attribute of the given SpeciesReference_t
 * structure is set.
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return nonzero if the "species" attribute of given SpeciesReference_t
 * structure has been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReference_isSetSpecies (const SpeciesReference_t *sr)
{
  return static_cast<int>( sr->isSetSpecies() );
}


/**
 * Predicate returning nonzero (for true) or zero (for false) depending on
 * whether the "stoichiometryMath" subelement of the given
 * SpeciesReference_t structure is non-empty.
 *
 * This function returns false if the SpeciesReference_t structure is a
 * Modifer (see SpeciesReference_isModifier()).
 *
 * @param sr The SpeciesReference_t structure to use.
 * 
 * @return nonzero if the "stoichiometryMath" subelement has content, zero
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReference_isSetStoichiometryMath (const SpeciesReference_t *sr)
{
  if (sr->isModifier()) return 0;

  return static_cast<int>
  (
    static_cast<const SpeciesReference*>(sr)->isSetStoichiometryMath()
  );
}


/**
 * Sets the value of the "id" attribute of the given SpeciesReference_t
 * structure.
 *
 * The string in @p sid will be copied.
 *
 * @param sr The SpeciesReference_t structure to use.
 *
 * @param sid The identifier string that will be copied and assigned as the
 * "id" attribute value.
 */
LIBSBML_EXTERN
void
SpeciesReference_setId (SpeciesReference_t *sr, const char *sid)
{
  (sid == NULL) ? sr->unsetId() : sr->setId(sid);
}


/**
 * Sets the value of the "name" attribute of the given SpeciesReference_t
 * structure.
 *
 * The string in @p sid will be copied.
 *
 * @param sr The SpeciesReference_t structure to use.
 *
 * @param sid The identifier string that will be copied and assigned as the
 * "name" attribute value.
 */
LIBSBML_EXTERN
void
SpeciesReference_setName (SpeciesReference_t *sr, const char *name)
{
  (name == NULL) ? sr->unsetName() : sr->setName(name);
}


/**
 * Sets the value of the "species" attribute of the given SpeciesReference_t
 * structure.
 *
 * The string in @p sid will be copied.
 *
 * @param sr The SpeciesReference_t structure to use.
 *
 * @param sid The identifier string that will be copied and assigned as the
 * "species" attribute value.
 */
LIBSBML_EXTERN
void
SpeciesReference_setSpecies (SpeciesReference_t *sr, const char *sid)
{
  sr->setSpecies(sid ? sid : "");
}


/**
 * Sets the value of the "stoichiometry" attribute of the given
 * SpeciesReference_t structure.
 *
 * This function has no effect if the SpeciesReference_t structure is a
 * Modifer (see SpeciesReference_isModifier()).
 *
 * @param sr The SpeciesReference_t structure to use.
 *
 * @param value The value to assign to the "stoichiometry" attribute.
 */
LIBSBML_EXTERN
void
SpeciesReference_setStoichiometry (SpeciesReference_t *sr, double value)
{
  if (sr->isModifier()) return;
  static_cast<SpeciesReference*>(sr)->setStoichiometry(value);
}


/**
 * Sets the content of the "stoichiometryMath" subelement of the given
 * SpeciesReference_t structure.
 *
 * This function has no effect if the SpeciesReference_t structure is a
 * Modifer (see SpeciesReference_isModifier()).
 *
 * @param sr The SpeciesReference_t structure to use.
 *
 * @param math An ASTNode expression tree to use as the content of the
 * "stoichiometryMath" subelement.
 */
LIBSBML_EXTERN
void
SpeciesReference_setStoichiometryMath (  SpeciesReference_t *sr
                                       , const StoichiometryMath_t    *math )
{
  if (sr->isModifier()) return;
  static_cast<SpeciesReference*>(sr)->setStoichiometryMath(math);
}


/**
 * Sets the value of the "denominator" attribute of the given
 * SpeciesReference_t structure.
 *
 * The "denominator" attribute is only actually written out in the case of
 * an SBML Level 1 model.  In SBML Level 2, rational-number stoichiometries
 * are written as MathML elements in the "stoichiometryMath" subelement.
 * However, as a convenience to users, libSBML allows the creation and
 * manipulation of rational-number stoichiometries by supplying the
 * numerator and denominator directly rather than having to manually create
 * an ASTNode structure.  LibSBML will write out the appropriate constructs
 * (either a combination of "stoichiometry" and "denominator" in the case
 * of SBML Level 1, or a "stoichiometryMath" subelement in the case of SBML
 * Level 2).
 *
 * This function has no effect if the SpeciesReference_t structure is a
 * Modifer (see SpeciesReference_isModifier()).
 *
 * @param sr The SpeciesReference_t structure to use.
 *
 * @param value The value to assign to the "denominator" attribute.
 */
LIBSBML_EXTERN
void
SpeciesReference_setDenominator (SpeciesReference_t *sr, int value)
{
  if (sr->isModifier()) return;
  static_cast<SpeciesReference*>(sr)->setDenominator(value);
}



/**
 * Unsets the value of the "id" attribute of the given SpeciesReference_t
 * structure.
 *
 * @param sr The SpeciesReference_t structure to use.
 */
LIBSBML_EXTERN
void
SpeciesReference_unsetId (SpeciesReference_t *sr)
{
  sr->unsetId();
}


/**
 * Unsets the value of the "name" attribute of the given SpeciesReference_t
 * structure.
 *
 * @param sr The SpeciesReference_t structure to use.
 */
LIBSBML_EXTERN
void
SpeciesReference_unsetName (SpeciesReference_t *sr)
{
  sr->unsetName();
}

/** @endcond doxygen-c-only */
