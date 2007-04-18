/**
 * @file    SpeciesReference.cpp
 * @brief   SBML SpeciesReference
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and Japan Science and
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
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/SpeciesReference.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


/**
 * Creates a new SimpleSpeciesReference, optionally with its species
 * attribute set.
 */
SimpleSpeciesReference::SimpleSpeciesReference (const string& species) :
   SBase (-1)
 , mSpecies( species )
{
}


/**
 * Destroys this SimpleSpeciesReference.
 */
SimpleSpeciesReference::~SimpleSpeciesReference ()
{
}


/**
 * Copy constructor. Creates a copy of this SimpleSpeciesReference.
 */
SimpleSpeciesReference::SimpleSpeciesReference(const SimpleSpeciesReference& rhs) :
          SBase     (rhs)
        , mSpecies  (rhs.mSpecies)
{
}


/**
 * Assignment operator.
 */
SimpleSpeciesReference& SimpleSpeciesReference::operator=(const SimpleSpeciesReference& rhs)
{
  this->SBase::operator =(rhs);
  mSpecies = rhs.mSpecies;
  return *this;
}


/**
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


/**
 * @return the species for this SimpleSpeciesReference.
 */
const string&
SimpleSpeciesReference::getSpecies () const
{
  return mSpecies;
}


/**
 * @return true if the species for this SimpleSpeciesReference has been
 * set, false otherwise.
 */
bool
SimpleSpeciesReference::isSetSpecies () const
{
  return (mSpecies.empty() == false);
}


/**
 * Sets the species of this SimpleSpeciesReference to a copy of sid.
 */
void
SimpleSpeciesReference::setSpecies (const string& sid)
{
  mSpecies = sid;
}


/**
 * @return true if this SpeciesReference is a ModiferSpeciesReference,
 * false otherwise.
 */
bool
SimpleSpeciesReference::isModifier () const
{
  return (getTypeCode() == SBML_MODIFIER_SPECIES_REFERENCE);
}


/**
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


  if (level == 2 && (version == 2 || version == 3))
  {
    //
    // id: SId  { use="optional" }  (L2v2)
    //
    attributes.readInto("id" , mId);
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
  const string s = (level == 1 && version == 1) ? "specie" : "species";
  attributes.readInto(s , mSpecies);
}


/**
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




/**
 * Creates a new SpeciesReference, optionally with its species,
 * stoichiometry, and denominator attributes set.
 */
SpeciesReference::SpeciesReference (  const string& species
                                    , double        stoichiometry
                                    , int           denominator ) :
   SimpleSpeciesReference( species       )
 , mStoichiometry        ( stoichiometry )
 , mDenominator          ( denominator   )
 , mStoichiometryMath    ( 0             )
{
}


/**
 * Destroys this SpeciesReference.
 */
SpeciesReference::~SpeciesReference ()
{
  delete mStoichiometryMath;
}


/**
 * Copy constructor. Creates a copy of this SpeciesReference.
 */
SpeciesReference::SpeciesReference (const SpeciesReference& rhs) :
   SimpleSpeciesReference( rhs                )
 , mStoichiometry        ( rhs.mStoichiometry )
 , mDenominator          ( rhs.mDenominator   )
 , mStoichiometryMath    ( 0                  )
{
  if (rhs.mStoichiometryMath)
  {
    mStoichiometryMath = rhs.mStoichiometryMath->deepCopy();
  }
}


/**
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
    mStoichiometryMath = rhs.mStoichiometryMath->deepCopy();
  }
  return *this;
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the Reaction's next
 * SpeciesReference (if available).
 */
bool
SpeciesReference::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/**
 * @return a (deep) copy of this SpeciesReference.
 */
SBase*
SpeciesReference::clone () const
{
  return new SpeciesReference(*this);
}


/**
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


/**
 * @return the stoichiometry of this SpeciesReference.
 */
double
SpeciesReference::getStoichiometry () const
{
  return mStoichiometry;
}


/**
 * @return the stoichiometryMath of this SpeciesReference.
 */
const ASTNode*
SpeciesReference::getStoichiometryMath () const
{
  return mStoichiometryMath;
}


/**
 * @return the denominator of this SpeciesReference.
 */
int
SpeciesReference::getDenominator () const
{
  return mDenominator;
}


/**
 * @return true if the stoichiometryMath of this SpeciesReference has been
 * set, false otherwise.
 */
bool
SpeciesReference::isSetStoichiometryMath () const
{
  return (mStoichiometryMath != NULL);
}


/**
 * Sets the stoichiometry of this SpeciesReference to value.
 */
void
SpeciesReference::setStoichiometry (double value)
{
  mStoichiometry = value;
}


/**
 * Sets the stoichiometryMath of this SpeciesReference to a copy of the
 * given ASTNode.
 */
void
SpeciesReference::setStoichiometryMath (const ASTNode* math)
{
  if (mStoichiometryMath == math) return;


  delete mStoichiometryMath;
  mStoichiometryMath = (math != 0) ? math->deepCopy() : 0;
}


/**
 * Sets the stoichiometryMath of this SpeciesReference to the given
 * formula string.
 */
void
SpeciesReference::setStoichiometryMath (const string& formula)
{
  if ( !formula.empty() )
  {
    delete mStoichiometryMath; 
    mStoichiometryMath = SBML_parseFormula( formula.c_str() );
  }
}


/**
 * Sets the denominator of this SpeciesReference to value.
 */
void
SpeciesReference::setDenominator (int value)
{
  mDenominator = value;
}


/**
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


/**
 * @return the name of this element ie "speciesReference".
 
 */
const string&
SpeciesReference::getElementName () const
{
  static const string specie  = "specieReference";
  static const string species = "speciesReference";

  return (getLevel() == 1 && getVersion() == 1) ? specie : species;
}


/**
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


  if (name == "stoichiometryMath")
  {
    const XMLToken elem = stream.next();
    stream.skipText();
   
    /* check for MathML namespace 
     * this may be explicitly declared here
     * or implicitly declared on the whole document
     */
    const XMLToken element = stream.peek();
    unsigned int match = 0;
    unsigned int math = 0;

    if (element.getName() == "math")
    {
      math = 1;
    }

    int n;
    if (math == 1 && element.getNamespaces().getLength() != 0)
    {
      for (n = 0; n < element.getNamespaces().getLength(); n++)
      {
        if (!strcmp(element.getNamespaces().getURI(n).c_str(), "http://www.w3.org/1998/Math/MathML"))
        {
          match = 1;
          break;
        }
      }
    }
    if (math == 1 && match == 0)
    {
      if( mSBML->getNamespaces() != NULL)
      /* check for implicit declaration */
      {
        for (n = 0; n < mSBML->getNamespaces()->getLength(); n++)
        {
          if (!strcmp(mSBML->getNamespaces()->getURI(n).c_str(), 
                                                     "http://www.w3.org/1998/Math/MathML"))
          {
            match = 1;
            break;
          }
        }
      }
    }
    if (math == 0 || match == 0)
    {
      static_cast <SBMLErrorLog*> (stream.getErrorLog())->logError(10201);
    }

    delete mStoichiometryMath;
    mStoichiometryMath = readMathML(stream);
    read               = true;

    stream.skipPastEnd(elem);

    if (mStoichiometryMath && mStoichiometryMath->isRational())
    {
      mStoichiometry = mStoichiometryMath->getNumerator();
      mDenominator   = mStoichiometryMath->getDenominator();

      delete mStoichiometryMath;
      mStoichiometryMath = 0;
    }
  }
  else if (name == "annotation")
  {
    /* if annotation already exists then it is an error 
     */
    if (mAnnotation)
    {
      mSBML->getErrorLog()->logError(10103);
    }
    delete mAnnotation;
    mAnnotation = new XMLNode(stream);
    checkAnnotation();
    mCVTerms = new List();
    parseRDFAnnotation(mAnnotation, mCVTerms);
    mAnnotation = deleteRDFAnnotation(mAnnotation);
#ifdef USE_LAYOUT
    // only parse the id annotation if it is Level 1 or Level 2 Version 1
    // everything after Level 2 Version 1 has ids.
    if(this->getLevel()==1 || (this->getLevel()==2 && this->getVersion()==1))
    {
      parseSpeciesReferenceAnnotation(mAnnotation,*this);
      checkAnnotation();
      mAnnotation=deleteLayoutIdAnnotation(mAnnotation);
    }
#endif // USE_LAYOUT
    read = true;
  }
  else if (name == "notes")
  {
    /* if notes already exists then it is an error 
     * if annotation already exists then ordering is wrong
     */
    if (mNotes || mAnnotation)
    {
      mSBML->getErrorLog()->logError(10103);
    }

    delete mNotes;
    mNotes = new XMLNode(stream);
    checkXHTML(mNotes);
    read = true;
  }

  return read;
}


/**
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


/**
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


/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
SpeciesReference::writeElements (XMLOutputStream& stream) const
{
  if (getLevel() == 2)
  {
    if (mStoichiometryMath || mDenominator != 1)
    {
      stream.startElement("stoichiometryMath");

      if (mStoichiometryMath) 
      {
        writeMathML(mStoichiometryMath, stream);
      }
      else
      {
        ASTNode node;
        node.setValue(static_cast<long>(mStoichiometry), mDenominator);

        writeMathML(&node, stream);
      }

      stream.endElement("stoichiometryMath");
#ifdef USE_LAYOUT
    if(this->getLevel()==1 || (this->getLevel()==2 && this->getVersion()==1))
    {
      if (this->isSetId())
      {
        SpeciesReference * sr = const_cast <SpeciesReference *> (this);
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
#endif // USE_LAYOUT    
    }
  }
}




/**
 * Creates a new ModifierSpeciesReference, optionally with its species
 * attribute set.
 */
ModifierSpeciesReference::ModifierSpeciesReference (const string& species) :
  SimpleSpeciesReference(species)
{
}


/**
 * Destroys this ModifierSpeciesReference.
 */
ModifierSpeciesReference::~ModifierSpeciesReference ()
{
}


/**
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


/**
 * @return a (deep) copy of this ModifierSpeciesReference.
 */
SBase*
ModifierSpeciesReference::clone () const
{
  return new ModifierSpeciesReference(*this);
}


/**
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


/**
 * @return the name of this element ie "modifierSpeciesReference".
 
 */
const string&
ModifierSpeciesReference::getElementName () const
{
  static const string name = "modifierSpeciesReference";
  return name;
}


#ifdef USE_LAYOUT

/**
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
  if (name == "annotation")
  {
    /* if annotation already exists then it is an error 
     */
    if (mAnnotation)
    {
      mSBML->getErrorLog()->logError(10103);
    }
    delete mAnnotation;
    mAnnotation = new XMLNode(stream);
    checkAnnotation();
    mCVTerms = new List();
    parseRDFAnnotation(mAnnotation, mCVTerms);
    mAnnotation = deleteRDFAnnotation(mAnnotation);
    
    if(this->getLevel()==1 || (this->getLevel()==2 && this->getVersion()==1))
    {
      parseSpeciesReferenceAnnotation(mAnnotation,*this);
      mAnnotation=deleteLayoutIdAnnotation(mAnnotation);
    }
    read=true;
  }
  else if (name == "notes")
  {
    /* if notes already exists then it is an error 
     * if annotation already exists then ordering is wrong
     */
    if (mNotes || mAnnotation)
    {
      mSBML->getErrorLog()->logError(10103);
    }

    delete mNotes;
    mNotes = new XMLNode(stream);
    checkXHTML(mNotes);
    read = true;
  }

  return read;
}

/**
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


/**
 * Creates a new ListOfSpeciesReferences.
 */
ListOfSpeciesReferences::ListOfSpeciesReferences () : mType(Unknown)
{
}


/**
 * @return a (deep) copy of this ListOfUnits.
 */
SBase*
ListOfSpeciesReferences::clone () const
{
  return new ListOfSpeciesReferences(*this);
}


/**
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


/**
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


/**
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


/**
 * Sets type of this ListOfSpeciesReferences.
 */
void
ListOfSpeciesReferences::setType (SpeciesType type)
{
  mType = type;
}


/**
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
      mSBML->getErrorLog()->logError(21104);
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
      mSBML->getErrorLog()->logError(21105);
    }
  }

  if (object) mItems.push_back(object);

  return object;
}




/**
 * Creates a new SpeciesReference and returns a pointer to it.
 */
LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_create (void)
{
  return new(nothrow) SpeciesReference;
}


/**
 * Creates a new ModifierSpeciesReference and returns a pointer to it.
 */
LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_createModifier (void)
{
  return new(nothrow) ModifierSpeciesReference;
}


/**
 * Creates a new SpeciesReference with the given species, stoichiometry and
 * denominator and returns a pointer to it.
 */
LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_createWith ( const char *species,
                              double      stoichiometry,
                              int         denominator )
{
  const char *s = species ? species : "";
  return new(nothrow) SpeciesReference(s, stoichiometry, denominator);
}


/**
 * Frees the given SpeciesReference.
 */
LIBSBML_EXTERN
void
SpeciesReference_free (SpeciesReference_t *sr)
{
  delete sr;
}


/**
 * @return a (deep) copy of this SpeciesReference
 */
LIBSBML_EXTERN
SpeciesReference_t *
SpeciesReference_clone (const SpeciesReference_t *sr)
{
  return static_cast<SpeciesReference_t*>( sr->clone() );
}


/**
 * Initializes the fields of this SpeciesReference to their defaults:
 *
 *   - stoichiometry = 1
 *   - denominator   = 1
 *
 * This function has no effect if the SpeciesReference is a Modifer (see
 * SpeciesReference_isModifier()).
 */
LIBSBML_EXTERN
void
SpeciesReference_initDefaults (SpeciesReference_t *sr)
{
  if (sr->isModifier()) return;
  static_cast<SpeciesReference*>(sr)->initDefaults();
}


/**
 * @return true (non-zero) if the SpeciesReference is a
 * ModiferSpeciesReference, false otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReference_isModifier (const SpeciesReference_t *sr)
{
  return static_cast<int>( sr->isModifier() );
}



/**
 * @return the id of this SpeciesReference.
 */
LIBSBML_EXTERN
const char *
SpeciesReference_getId (const SpeciesReference_t *sr)
{
  return sr->isSetId() ? sr->getId().c_str() : NULL;
}


/**
 * @return the name of this SpeciesReference.
 */
LIBSBML_EXTERN
const char *
SpeciesReference_getName (const SpeciesReference_t *sr)
{
  return sr->isSetName() ? sr->getName().c_str() : NULL;
}


/**
 * @return the species of this SpeciesReference.
 */
LIBSBML_EXTERN
const char *
SpeciesReference_getSpecies (const SpeciesReference_t *sr)
{
  return sr->isSetSpecies() ? sr->getSpecies().c_str() : NULL;
}


/**
 * @return the stoichiometry of this SpeciesReference.
 *
 * This function returns zero if the SpeciesReference is a Modifer (see
 * SpeciesReference_isModifier()).
 */
LIBSBML_EXTERN
double
SpeciesReference_getStoichiometry (const SpeciesReference_t *sr)
{
  if (sr->isModifier()) return 0.0;
  return static_cast<const SpeciesReference*>(sr)->getStoichiometry();
}


/**
 * @return the stoichiometryMath of this SpeciesReference.
 *
 * This function returns NULL if the SpeciesReference is a Modifer (see
 * SpeciesReference_isModifier()).
 */
LIBSBML_EXTERN
const ASTNode_t *
SpeciesReference_getStoichiometryMath (const SpeciesReference_t *sr)
{
  if (sr->isModifier()) return NULL;
  return static_cast<const SpeciesReference*>(sr)->getStoichiometryMath();
}


/**
 * @return the denominator of this SpeciesReference.
 *
 * This function returns 0 if the SpeciesReference is a Modifer (see
 * SpeciesReference_isModifier()).
 */
LIBSBML_EXTERN
int
SpeciesReference_getDenominator (const SpeciesReference_t *sr)
{
  if (sr->isModifier()) return 0;
  return static_cast<const SpeciesReference*>(sr)->getDenominator();
}


/**
 * @return true (non-zero) if the id for this SpeciesReference has been
 * set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReference_isSetId (const SpeciesReference_t *sr)
{
  return static_cast<int>( sr->isSetId() );
}


/**
 * @return true (non-zero) if the name for this SpeciesReference has been
 * set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReference_isSetName (const SpeciesReference_t *sr)
{
  return static_cast<int>( sr->isSetName() );
}


/**
 * @return true (non-zero) if the species for this SpeciesReference
 * has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
SpeciesReference_isSetSpecies (const SpeciesReference_t *sr)
{
  return static_cast<int>( sr->isSetSpecies() );
}


/**
 * @return true (non-zero) if the stoichiometryMath of this
 * SpeciesReference has been set, false (0) otherwise.
 *
 * This function returns false if the SpeciesReference is a Modifer (see
 * SpeciesReference_isModifier()).
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
 * Sets the id of this SpeciesReference to a copy of sid.
 */
LIBSBML_EXTERN
void
SpeciesReference_setId (SpeciesReference_t *sr, const char *sid)
{
  (sid == NULL) ? sr->unsetId() : sr->setId(sid);
}


/**
 * Sets the name of this SpeciesReference to a copy of name.
 */
LIBSBML_EXTERN
void
SpeciesReference_setName (SpeciesReference_t *sr, const char *name)
{
  (name == NULL) ? sr->unsetName() : sr->setName(name);
}


/**
 * Sets the species of this SpeciesReference to a copy of sid.
 */
LIBSBML_EXTERN
void
SpeciesReference_setSpecies (SpeciesReference_t *sr, const char *sid)
{
  sr->setSpecies(sid ? sid : "");
}


/**
 * Sets the stoichiometry of this SpeciesReference to value.
 *
 * This function has no effect if the SpeciesReference is a Modifer (see
 * SpeciesReference_isModifier()).
 */
LIBSBML_EXTERN
void
SpeciesReference_setStoichiometry (SpeciesReference_t *sr, double value)
{
  if (sr->isModifier()) return;
  static_cast<SpeciesReference*>(sr)->setStoichiometry(value);
}


/**
 * Sets the stoichiometryMath of this SpeciesReference to a copy of the
 * given ASTNode.
 *
 * This function has no effect if the SpeciesReference is a Modifer (see
 * SpeciesReference_isModifier()).
 */
LIBSBML_EXTERN
void
SpeciesReference_setStoichiometryMath (  SpeciesReference_t *sr
                                       , const ASTNode_t    *math )
{
  if (sr->isModifier()) return;
  static_cast<SpeciesReference*>(sr)->setStoichiometryMath(math);
}


/**
 * Sets the denominator of this SpeciesReference to value.
 *
 * This function has no effect if the SpeciesReference is a Modifer (see
 * SpeciesReference_isModifier()).
 */
LIBSBML_EXTERN
void
SpeciesReference_setDenominator (SpeciesReference_t *sr, int value)
{
  if (sr->isModifier()) return;
  static_cast<SpeciesReference*>(sr)->setDenominator(value);
}



/**
 * Unsets the id of this Species.
 */
LIBSBML_EXTERN
void
SpeciesReference_unsetId (SpeciesReference_t *sr)
{
  sr->unsetId();
}


/**
 * Unsets the name of this Species.
 */
LIBSBML_EXTERN
void
SpeciesReference_unsetName (SpeciesReference_t *sr)
{
  sr->unsetName();
}
