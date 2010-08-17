/**
 * @file    ListOf.h
 * @author  Wraps List and inherits from SBase
 * @author  SBML Team <sbml-team@caltech.edu>
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
 *----------------------------------------------------------------------- -->
 *
 * @class ListOf
 * @brief Parent class for the various SBML "ListOfXYZ" classes.
 *
 * @htmlinclude not-sbml-warning.html
 *
 */


#ifndef ListOf_h
#define ListOf_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/SBMLTypeCodes.h>


#ifdef __cplusplus


#include <vector>

#include <sbml/SBase.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class SBMLVisitor;


class LIBSBML_EXTERN ListOf : public SBase
{
public:

  /**
   * Creates a new ListOf.
   */
  ListOf ();


  /**
   * Destroys the given ListOf and the items inside it.
   */
  virtual ~ListOf ();


  /**
   * Copy constructor.  Creates a copy of this ListOf.
   */
  ListOf (const ListOf& orig);


  /**
   * Assignment operator for ListOf.
   */
  ListOf& operator=(const ListOf& rhs);


  /**
   * Accepts the given SBMLVisitor.
   *
   * @param v the SBMLVisitor instance to be used.
   * 
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether the Visitor would like to visit the next item in the
   * list.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /**
   * Creates and returns a deep copy of this ListOf.
   * 
   * @return a (deep) copy of this ListOf.
   */
  virtual SBase* clone () const;


  /**
   * Adds item to the end of this ListOf.
   *
   * This variant of the method makes a clone of the @p item handed to it.
   * This means that when the ListOf is destroyed, the original items will
   * not be destroyed.
   *
   * @param item the item to be added to the list.
   *
   * @see appendAndOwn(SBase* item)
   */
  void append (const SBase* item);


  /**
   * Adds item to the end of this ListOf.
   *
   * This variant of the method does not clone the @p item handed to it;
   * instead, it assumes ownership of it.  This means that when the ListOf
   * is destroyed, the item will be destroyed along with it.
   *
   * @param item the item to be added to the list.
   *
   * @see append(const SBase* item)
   */
  void appendAndOwn (SBase* item);


  /**
   * Get an item from the list.
   *
   * @param n the index number of the item to get.
   * 
   * @return the nth item in this ListOf items.
   *
   * @see size()
   */
  virtual const SBase* get (unsigned int n) const;


  /**
   * Get an item from the list.
   *
   * @param n the index number of the item to get.
   * 
   * @return the nth item in this ListOf items.
   *
   * @see size()
   */
  virtual SBase* get (unsigned int n);


#if 0
  /**
   * Get an item from the list based on its identifier.
   *
   * @param sid a string representing the the identifier of the item to get.
   * 
   * @return item in this ListOf items with the given id or NULL if no such
   * item exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const SBase* get (const std::string& sid) const;
#endif


#if 0
  /**
   * Get an item from the list based on its identifier.
   *
   * @param sid a string representing the the identifier of the item to get.
   * 
   * @return item in this ListOf items with the given id or NULL if no such
   * item exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual SBase* get (const std::string& sid);
#endif


  /**
   * Removes all items in this ListOf object.
   *
   * If doDelete is true (default), all items in this ListOf object are deleted
   * and cleared, and thus the caller doesn't have to delete those items.
   * Otherwise, all items are just cleared from this ListOf object and the caller 
   * is responsible for deleting all items (In this case, pointers to all items 
   * should be stored elsewhere before calling this function by the caller).
   *
   * @param doDelete if true (default), all items are deleted and cleared.
   * Otherwise, all items are just cleared and not deleted. 
   */ 
  void clear (bool doDelete = true);


  /**
   * Removes the nth item from this ListOf items and returns a pointer to
   * it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the item to remove
   *
   * @see size()
   */
  virtual SBase* remove (unsigned int n);


#if 0
  /**
   * Removes item in this ListOf items with the given identifier.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then @c
   * NULL is returned.
   *
   * @param sid the identifier of the item to remove
   *
   * @return the item removed.  As mentioned above, the caller owns the
   * returned item.
   */
  virtual SBase* remove (const std::string& sid);
#endif


  /**
   * Get the size of this ListOf.
   * 
   * @return the number of items in this ListOf items.
   */
  unsigned int size () const;

  /** @cond doxygen-libsbml-internal */

  /**
   * Sets the parent SBMLDocument of this SBML object.
   *
   * @param d the SBMLDocument that should become the parent of this
   * ListOf.
   */
  virtual void setSBMLDocument (SBMLDocument* d);


  /**
   * Sets the parent SBML object of this SBML object.
   *
   * @param sb the SBML object to use
   */
  virtual void setParentSBMLObject (SBase* sb);

  /** @endcond */

  /**
   * Returns the libSBML type code for this object, namely, @c
   * SBML_LIST_OF.
   * 
   * @if clike LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.
   * The set of possible type codes is defined in the enumeration
   * #SBMLTypeCode_t.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if java LibSBML attaches an
   * identifying code to every kind of SBML object.  These are known as
   * <em>SBML type codes</em>.  In other languages, the set of type codes
   * is stored in an enumeration; in the Java language interface for
   * libSBML, the type codes are defined as static integer constants in
   * interface class {@link libsbmlConstants}.  The names of the type codes
   * all begin with the characters @c SBML_. @endif
   *
   * @return the SBML type code for this object, or @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;


  /**
   * Get the type code of the objects contained in this ListOf.
   * 
   * @if clike LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.
   * The set of possible type codes is defined in the enumeration
   * #SBMLTypeCode_t.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if java LibSBML attaches an
   * identifying code to every kind of SBML object.  These are known as
   * <em>SBML type codes</em>.  In other languages, the set of type codes
   * is stored in an enumeration; in the Java language interface for
   * libSBML, the type codes are defined as static integer constants in
   * interface class {@link libsbmlConstants}.  The names of the type codes
   * all begin with the characters @c SBML_. @endif
   * 
   * @return the SBML type code for the objects contained in this ListOf
   * instance, or @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   */
  virtual SBMLTypeCode_t getItemTypeCode () const;


  /**
   * Returns the XML element name of this object, which for ListOf, is
   * always @c "listOf".
   * 
   * @return the XML name of this element.
   */
  virtual const std::string& getElementName () const;


  /** @cond doxygen-libsbml-internal */
  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;
  /** @endcond */

protected:
  /** @cond doxygen-libsbml-internal */
  
  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual void readAttributes (const XMLAttributes& attributes);

  /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.  For example:
   *
   *   SBase::writeAttributes(stream);
   *   stream.writeAttribute( "id"  , mId   );
   *   stream.writeAttribute( "name", mName );
   *   ...
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;



  std::vector<SBase*> mItems;

  /** @endcond */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


/**
 * Creates a new ListOf.
 *
 * @return a pointer to created ListOf.
 */
LIBSBML_EXTERN
ListOf_t *
ListOf_create (void);

/**
 * Frees the given ListOf and its constituent items.
 *
 * This function assumes each item in the list is derived from SBase.
 */
LIBSBML_EXTERN
void
ListOf_free (ListOf_t *lo);

/**
 * @return a (deep) copy of this ListOf items.
 */
LIBSBML_EXTERN
ListOf_t *
ListOf_clone (const ListOf_t *lo);


/**
 * Adds a copy of item to the end of this ListOf items.
 */
LIBSBML_EXTERN
void
ListOf_append (ListOf_t *lo, const SBase_t *item);

/**
 * Adds the given item to the end of this ListOf items.
 */
LIBSBML_EXTERN
void
ListOf_appendAndOwn (ListOf_t *lo, SBase_t *item);

/**
 * Returns the nth item in this ListOf items.
 */
LIBSBML_EXTERN
SBase_t *
ListOf_get (ListOf_t *lo, unsigned int n);

#if (0)
/**
 * @return item in this ListOf items with the given id or NULL if no such
 * item exists.
 */
LIBSBML_EXTERN
SBase_t *
ListOf_getById (ListOf_t *lo, const char *sid);
#endif

/**
 * Removes all items in this ListOf object.
 */
LIBSBML_EXTERN
void
ListOf_clear (ListOf_t *lo, int doDelete);

/**
 * Removes the nth item from this ListOf items and returns a pointer to
 * it.  The caller owns the returned item and is responsible for deleting
 * it.
 */
LIBSBML_EXTERN
SBase_t *
ListOf_remove (ListOf_t *lo, unsigned int n);

#if (0)
/**
 * Removes item in this ListOf items with the given id or NULL if no such
 * item exists.  The caller owns the returned item and is repsonsible for
 * deleting it.
 */
LIBSBML_EXTERN
SBase_t *
ListOf_removeById (ListOf_t *lo, const char *sid);
#endif

/**
 * Returns the number of items in this ListOf items.
 */
LIBSBML_EXTERN
unsigned int
ListOf_size (const ListOf_t *lo);

/**
 * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
LIBSBML_EXTERN
SBMLTypeCode_t
ListOf_getItemTypeCode (const ListOf_t *lo);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /* ListOf_h */
