/**
 * @file    ListOf.cpp
 * @brief   Wraps List and inherits from SBase
 * @author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2010 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <algorithm>
#include <functional>

#include <sbml/SBMLVisitor.h>
#include <sbml/ListOf.h>
#include <sbml/SBO.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new ListOf items.
 */
ListOf::ListOf ()
{
}


/**
 * Used by the Destructor to delete each item in mItems.
 */
struct Delete : public unary_function<SBase*, void>
{
  void operator() (SBase* sb) { delete sb; }
};


/*
 * Destroys the given ListOf and its constituent items.
 */
ListOf::~ListOf ()
{
  for_each( mItems.begin(), mItems.end(), Delete() );
}


/**
 * Used by the Copy Constructor to clone each item in mItems.
 */
struct Clone : public unary_function<SBase*, SBase*>
{
  SBase* operator() (SBase* sb) { return sb->clone(); }
};


/*
 * Copy constructor. Creates a copy of this ListOf items.
 */
ListOf::ListOf (const ListOf& orig) : SBase(orig)
{
  mItems.resize( orig.size() );
  transform( orig.mItems.begin(), orig.mItems.end(), mItems.begin(), Clone() );
}


/*
 * Assignment operator
 */
ListOf& ListOf::operator=(const ListOf& rhs)
{
  if(&rhs!=this)
  {
    this->SBase::operator =(rhs);
    // Deletes existing items
    for_each( mItems.begin(), mItems.end(), Delete() );
    mItems.resize( rhs.size() );
    transform( rhs.mItems.begin(), rhs.mItems.end(), mItems.begin(), Clone() );
  }

  return *this;
}

/*
 * Accepts the given SBMLVisitor.
 */
bool
ListOf::accept (SBMLVisitor& v) const
{
  v.visit(*this, getItemTypeCode() );
  for (unsigned int n = 0 ; n < mItems.size() && mItems[n]->accept(v); ++n) ;
  v.leave(*this, getItemTypeCode() );

  return true;
}


/*
 * @return a (deep) copy of this ListOf items.
 */
SBase*
ListOf::clone () const
{
  return new ListOf(*this);
}


/*
 * Adds item to the end of this ListOf items.  This ListOf items assumes
 * ownership of item and will delete it.
 */
int
ListOf::append (const SBase* item)
{
  return appendAndOwn( item->clone() );
}


/*
 * Adds item to the end of this ListOf items.  This ListOf items assumes
 * ownership of item and will delete it.
 */
int
ListOf::appendAndOwn (SBase* item)
{
  bool okayToAdd = false;
  /* okay to add the object if
   * 1) first object to be added to listOf doesnt have type
   * 2) itemTypeCode of list matches typecode of object
   *    - need to consider sub classed objects
   * 3) where someone is dealing directly with a listOf object
   *    the itemTypeCode is unknown but once one object has
   *    been added only similar objects should be added
   */
  if (this->getItemTypeCode() == SBML_UNKNOWN)
  {
    if (mItems.size() == 0)
    {
      okayToAdd = true;
    }
    else
    {
      if (mItems.at(0)->getTypeCode() == item->getTypeCode())
      {
        okayToAdd = true;
      }
    }
  }
  else if (this->getItemTypeCode() == item->getTypeCode())
  {
    okayToAdd = true;
  }
  else
  {
    /* deal with objects that are sub classed */
    SBMLTypeCode_t type = item->getTypeCode();
    switch (type)
    {
      case SBML_ALGEBRAIC_RULE:
      case SBML_ASSIGNMENT_RULE:
      case SBML_RATE_RULE:
      case SBML_SPECIES_CONCENTRATION_RULE:
      case SBML_COMPARTMENT_VOLUME_RULE:
      case SBML_PARAMETER_RULE:
        if (this->getItemTypeCode() == SBML_RULE)
          okayToAdd = true;
        break;
#ifdef USE_LAYOUT
      case SBML_LAYOUT_COMPARTMENTGLYPH:
      case SBML_LAYOUT_REACTIONGLYPH:
      case SBML_LAYOUT_SPECIESGLYPH:
      case SBML_LAYOUT_SPECIESREFERENCEGLYPH:
      case SBML_LAYOUT_TEXTGLYPH:
        if (this->getItemTypeCode() == SBML_LAYOUT_GRAPHICALOBJECT)
          okayToAdd = true;
        break;
      case SBML_LAYOUT_CUBICBEZIER:
        if (this->getItemTypeCode() == SBML_LAYOUT_LINESEGMENT)
          okayToAdd = true;
        break;
#endif
    }
  }
  if (okayToAdd)
  {
    mItems.push_back( item );
    item->setSBMLDocument(mSBML);
    item->setParentSBMLObject(this);
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_INVALID_OBJECT;
  }
}


/*
 * @return the nth item in this ListOf items.
 */
const SBase*
ListOf::get (unsigned int n) const
{
  return (n < mItems.size()) ? mItems[n] : 0;
}


/*
 * @return the nth item in this ListOf items.
 */
SBase*
ListOf::get (unsigned int n)
{
  return const_cast<SBase*>( static_cast<const ListOf&>(*this).get(n) );
}


/**
 * Used by ListOf::get() to lookup an SBase based by its id.
 */
//struct IdEq : public unary_function<SBase*, bool>
//{
//  const string& id;
//
//  IdEq (const string& id) : id(id) { }
//  bool operator() (SBase* sb) { return sb->getId() == id; }
//};


/*
 * @return item in this ListOf items with the given id or NULL if no such
 * item exists.
 */
//const SBase*
//ListOf::get (const std::string& sid) const
//{
//  vector<SBase*>::const_iterator result;
//
//  result = find_if( mItems.begin(), mItems.end(), IdEq(sid) );
//  return (result == mItems.end()) ? 0 : *result;
//}


/*
 * @return item in this ListOf items with the given id or NULL if no such
 * item exists.
 */
//SBase*
//ListOf::get (const std::string& sid)
//{
//  return const_cast<SBase*>( static_cast<const ListOf&>(*this).get(sid) );
//}


/*
 * Removes all items in this ListOf object.
 *
 * If doDelete is true (default), all items in this ListOf object are deleted
 * and cleared, and thus the caller doesn't have to delete those items.
 * Otherwise, all items are just cleared from this ListOf object and the caller
 * is responsible for deleting all items (In this case, pointers to all items
 * should be stored elsewhere before calling this function by the caller).
 */
void
ListOf::clear (bool doDelete)
{
  if (doDelete)
    for_each( mItems.begin(), mItems.end(), Delete() );
  mItems.clear();
}


/*
 * Removes the nth item from this ListOf items and returns a pointer to
 * it.  The caller owns the returned item and is responsible for deleting
 * it.
 */
SBase*
ListOf::remove (unsigned int n)
{
  SBase* item = get(n);
  if (item) mItems.erase( mItems.begin() + n );
  return item;
}


/*
 * Removes item in this ListOf items with the given id or NULL if no such
 * item exists.  The caller owns the returned item and is repsonsible for
 * deleting it.
 */
//SBase*
//ListOf::remove (const std::string& sid)
//{
//  SBase* item = 0;
//  vector<SBase*>::iterator result;
//
//  result = find_if( mItems.begin(), mItems.end(), IdEq(sid) );
//
//  if (result != mItems.end())
//  {
//    item = *result;
//    mItems.erase(result);
//  }
//
//  return item;
//}
//

/*
 * @return the number of items in this ListOf items.
 */
unsigned int
ListOf::size () const
{
  return mItems.size();
}


/**
 * Used by ListOf::setSBMLDocument().
 */
struct SetSBMLDocument : public unary_function<SBase*, void>
{
  SBMLDocument* d;

  SetSBMLDocument (SBMLDocument* d) : d(d) { }
  void operator() (SBase* sbase) { sbase->setSBMLDocument(d); }
};


/**
 * Used by ListOf::setParentSBMLObject().
 */
struct SetParentSBMLObject : public unary_function<SBase*, void>
{
  SBase* sb;

  SetParentSBMLObject (SBase *sb) : sb(sb) { }
  void operator() (SBase* sbase) { sbase->setParentSBMLObject(sb); }
};

/** @cond doxygen-libsbml-internal */

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
ListOf::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;
  for_each( mItems.begin(), mItems.end(), SetSBMLDocument(d) );
}


/*
 * Sets the parent SBML object of this SBML object.
 */
void
ListOf::setParentSBMLObject (SBase* sb)
{
  mParentSBMLObject = sb;
  for_each( mItems.begin(), mItems.end(), SetParentSBMLObject(this) );
}
/** @endcond */


/*
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 */
SBMLTypeCode_t
ListOf::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
SBMLTypeCode_t
ListOf::getItemTypeCode () const
{
  return SBML_UNKNOWN;
}


/*
 * @return the name of this element ie "listOf".
 
 */
const string&
ListOf::getElementName () const
{
  static const string name = "listOf";
  return name;
}


/**
 * Used by ListOf::writeElements().
 */
struct Write : public unary_function<SBase*, void>
{
  XMLOutputStream& stream;

  Write (XMLOutputStream& s) : stream(s) { }
  void operator() (SBase* sbase) { sbase->write(stream); }
};


/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
ListOf::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  for_each( mItems.begin(), mItems.end(), Write(stream) );
}
/** @endcond */

/** @cond doxygen-libsbml-internal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
ListOf::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  std::vector<std::string> expectedAttributes;
  expectedAttributes.clear();

  if (level > 1)
  {
    expectedAttributes.push_back("metaid");

    if (level > 2 || (level == 2 && version > 2))
      expectedAttributes.push_back("sboTerm");
  }


  // check that all attributes are expected
  for (int i = 0; i < attributes.getLength(); i++)
  {
    std::vector<std::string>::const_iterator end = expectedAttributes.end();
    std::vector<std::string>::const_iterator begin = expectedAttributes.begin();
    std::string name = attributes.getName(i);
    std::string prefix = attributes.getPrefix(i);
    // only check attributes in the sbml namespace   
    if (prefix.empty() || prefix == "sbml")
    {
      if (std::find(begin, end, name) == end)
      {
        std::string component = "<" + getElementName() + ">";
        logUnknownAttribute(name, level, version, component);
      }
    }
  }

  if (level > 1)
  {
    //
    // sboTerm: SBOTerm { use="optional" }  (L2v3 ->)
    //
    if (!(level == 2 && version < 3)) 
    {
      mSBOTerm = SBO::readTerm(attributes, this->getErrorLog(), level, version);
    }
  }
}

void 
ListOf::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  if (level > 1)
  {
    //
    // sboTerm: SBOTerm { use="optional" }  (L2v3 ->)
    //
    if (!(level == 2 && version < 3)) 
    {
      SBO::writeTerm(stream, mSBOTerm);
    }
  }
}

/** @endcond */



/** @cond doxygen-c-only */


/**
 * Creates a new ListOf.
 *
 * @return a pointer to created ListOf.
 */
LIBSBML_EXTERN
ListOf_t *
ListOf_create ()
{
  return new(nothrow) ListOf;
}


/**
 * Frees the given ListOf and its constituent items.
 *
 * This function assumes each item in the list is derived from SBase.
 */
LIBSBML_EXTERN
void
ListOf_free (ListOf_t *lo)
{
  delete lo;
}


/**
 * @return a (deep) copy of this ListOf items.
 */
LIBSBML_EXTERN
ListOf_t *
ListOf_clone (const ListOf_t *lo)
{
  return static_cast<ListOf_t*>( lo->clone() );
}


/**
 * Adds a copy of item to the end of this ListOf items.
 */
LIBSBML_EXTERN
int
ListOf_append (ListOf_t *lo, const SBase *item)
{
  return lo->append(item);
}


/**
 * Adds the given item to the end of this ListOf items.
 */
LIBSBML_EXTERN
int
ListOf_appendAndOwn (ListOf_t *lo, SBase_t *item)
{
  return lo->appendAndOwn(item);
}


/**
 * Returns the nth item in this ListOf items.
 */
LIBSBML_EXTERN
SBase *
ListOf_get (ListOf_t *lo, unsigned int n)
{
  return lo->get(n);
}


/*
 * @return item in this ListOf items with the given id or NULL if no such
 * item exists.
 */
//LIBSBML_EXTERN
//SBase *
//ListOf_getById (ListOf_t *lo, const char *sid)
//{
//  return (sid != NULL) ? lo->get(sid) : NULL;
//}
//

/**
 * Removes all items in this ListOf object.
 *
 * If doDelete is true (non-zero), all items in this ListOf object are deleted
 * and cleared, and thus the caller doesn't have to delete those items.
 * Otherwise (zero), all items are just cleared from this ListOf object and the 
 * caller is responsible for deleting all items (In this case, pointers to all 
 * items should be stored elsewhere before calling this function by the caller).
 */
LIBSBML_EXTERN
void
ListOf_clear (ListOf_t *lo, int doDelete)
{
  lo->clear(doDelete);
}


/**
 * Removes the nth item from this ListOf items and returns a pointer to
 * it.  The caller owns the returned item and is responsible for deleting
 * it.
 */
LIBSBML_EXTERN
SBase *
ListOf_remove (ListOf_t *lo, unsigned int n)
{
  return lo->remove(n);
}


/*
 * Removes item in this ListOf items with the given id or NULL if no such
 * item exists.  The caller owns the returned item and is repsonsible for
 * deleting it.
 */
//LIBSBML_EXTERN
//SBase *
//ListOf_removeById (ListOf_t *lo, const char *sid)
//{
//  return (sid != NULL) ? lo->remove(sid) : NULL;
//}


/**
 * Returns the number of items in this ListOf items.
 */
LIBSBML_EXTERN
unsigned int
ListOf_size (const ListOf_t *lo)
{
  return lo->size();
}


/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
LIBSBML_EXTERN
SBMLTypeCode_t
ListOf_getItemTypeCode (const ListOf_t *lo)
{
  return lo->getItemTypeCode();
}

/** @endcond */

LIBSBML_CPP_NAMESPACE_END
