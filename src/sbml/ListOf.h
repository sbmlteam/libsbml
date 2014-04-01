/**
 * @file    ListOf.h
 * @author  Wraps List and inherits from SBase
 * @author  SBML Team <sbml-team@caltech.edu>
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
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
 * ---------------------------------------------------------------------- -->
 *
 * @class ListOf
 * @sbmlbrief{core} Parent class for libSBML's "ListOfXYZ" classes.
 *
 * @htmlinclude not-sbml-warning.html
 *
 * The ListOf class in libSBML is a utility class that serves as the parent
 * class for implementing the ListOf__ classes.  It provides methods for
 * working generically with the various SBML lists of objects in a program.
 * LibSBML uses this separate list class rather than ordinary
 * @if conly C@endif@if cpp C++; @endif@if java Java@endif@if python Python@endif@~ lists,
 * so that it can provide the methods and features associated with SBase.
 *
 * @copydetails doc_what_is_listof 
 */


#ifndef ListOf_h
#define ListOf_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/SBMLTypeCodes.h>


#ifdef __cplusplus


#include <vector>
#include <algorithm>
#include <functional>

#include <sbml/SBase.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class SBMLVisitor;


/** @cond doxygenLibsbmlInternal */
/**
 * Used by ListOf::get() to lookup an SBase based by its id.
 */
#ifndef SWIG
template<class CNAME>
struct IdEq : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEq (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <CNAME*> (sb)->getId() == id; }
};
#endif /* SWIG */
/** @endcond */


class LIBSBML_EXTERN ListOf : public SBase
{
public:

  /**
   * Creates a new ListOf object.
   *
   * @param level the SBML Level; if not assigned, defaults to the
   * value of SBMLDocument::getDefaultLevel().
   * 
   * @param version the Version within the SBML Level; if not assigned,
   * defaults to the value of SBMLDocument::getDefaultVersion().
   * 
   * @if notcpp @htmlinclude warn-default-args-in-docs.html @endif@~
   */
  ListOf (unsigned int level   = SBML_DEFAULT_LEVEL, 
          unsigned int version = SBML_DEFAULT_VERSION);


  /**
   * Creates a new ListOf with SBMLNamespaces object.
   *
   * @param sbmlns the set of SBML namespaces that this ListOf should
   * contain.
   */
  ListOf (SBMLNamespaces* sbmlns);


  /**
   * Destroys the given ListOf and the items inside it.
   */
  virtual ~ListOf ();


  /**
   * Copy constructor; creates a copy of this ListOf.
   *
   * @param orig the ListOf instance to copy.
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
  virtual ListOf* clone () const;


  /**
   * Adds an item to the end of this ListOf's list of items.
   *
   * This method makes a clone of the @p item handed to it.  This means that
   * when the ListOf object is destroyed, the original items will not be
   * destroyed.  For a method with an alternative ownership behavior, see the
   * ListOf::appendAndOwn(SBase* item) method.
   *
   * @param item the item to be added to the list.
   *
   * @see appendAndOwn(SBase* item)
   * @see appendFrom(const ListOf* list)
   */
  int append (const SBase* item);


  /**
   * Adds an item to the end of this ListOf's list of items.
   *
   * This method does not clone the @p item handed to it; instead, it assumes
   * ownership of it.  This means that when the ListOf is destroyed, the item
   * will be destroyed along with it.  For a method with an alternative
   * ownership behavior, see the ListOf::append(SBase* item) method.
   *
   * @param item the item to be added to the list.
   *
   * @see append(const SBase* item)
   * @see appendFrom(const ListOf* list)
   */
  int appendAndOwn (SBase* item);


  /**
   * Adds a clone of a list of items to this ListOf's list.
   *
   * Note that because this clones the objects handed to it, the original
   * items will not be destroyed when this ListOf object is destroyed.
   *
   * @param list a list of items to be added.
   *
   * @see append(const SBase* item)
   * @see appendAndOwn(SBase* item)
   */
  virtual int appendFrom (const ListOf* list);


  /**
   * Inserts an item at a given position in this ListOf's list of items.
   *
   * This variant of the method makes a clone of the @p item handed to it.
   * This means that when the ListOf is destroyed, the original items will
   * not be destroyed.  For an alternative method with different ownership
   * behavior, see insertAndOwn(int location, SBase* item).
   *
   * @param location the location in the list where to insert the item.
   * @param item the item to be inserted to the list.
   *
   * @see insertAndOwn(int location, SBase* item)
   */
  int insert (int location, const SBase* item);


  /**
   * Inserts an item at a given position in this ListOf's list of items.
   *
   * This variant of the method makes a clone of the @p item handet to it.
   * This means that when the ListOf is destroyed, the original items will
   * not be destroyed.
   *
   * @param location the location where to insert the item
   * @param item the item to be inserted to the list
   *
   * @see insert(int location, const SBase* item)
   */
  int insertAndOwn(int location, SBase* item);


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


  /**
   * Returns the first child element found that has the given identifier.
   *
   * This method searches this ListOf's list of items for SBML objects based
   * on their "id" attribute value in the model-wide SId namespace.
   *
   * @param id string representing the id of objects to find.
   *
   * @return the first element found with the given @p id, or @c NULL if no
   * such object is found.
   */
  virtual SBase* getElementBySId(const std::string& id);


  /**
   * Returns the first child element found with the given @p metaid.
   *
   * @param metaid string representing the metaid of objects to find
   *
   * @return the first element found with the given @p metaid, or @c NULL if
   * no such object is found.
   */
  virtual SBase* getElementByMetaId(const std::string& metaid);


  /**
   * Returns a List of all child SBase objects.
   *
   * The values returned include all children of the objects in this ListOf
   * list, nested to an arbitrary depth.
   *
   * @return a List of pointers to all child objects.
   */
  virtual List* getAllElements(ElementFilter* filter=NULL);


#if 0
  /**
   * Get an item from the list based on its identifier.
   *
   * @param sid a string representing the the identifier of the item to get.
   * 
   * @return item in this ListOf items with the given @p sid or @c NULL if no such
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
   * @return item in this ListOf items with the given @p sid or @c NULL if no such
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
   * If parameter @p doDelete is @c true (default), all items in this ListOf
   * object are deleted and cleared, and thus the caller doesn't have to
   * delete those items.  Otherwise, all items are cleared only from this
   * ListOf object; the caller is still responsible for deleting all items.
   * (In the latter case, callers are advised to store pointers to all items
   * elsewhere before calling this function.)
   *
   * @param doDelete if @c true (default), all items are deleted and cleared.
   * Otherwise, all items are just cleared and not deleted.
   *
   * @if notcpp @htmlinclude warn-default-args-in-docs.html @endif@~
   */
  void clear (bool doDelete = true);


  /**
   * Because ListOf objects typically live as object children of their parent
   * object and not as pointer children, this function clears itself, but
   * does not attempt to do anything else.
   *
   * If a particular ListOf subclass does indeed exist as a pointer only,
   * this function will need to be overridden.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif@~ The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   */
  virtual int removeFromParentAndDelete();


  /**
   * Removes the <em>n</em>th item from this ListOf items and returns it.
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
   * Returns number of items in this ListOf list.
   *
   * @return the number of items in this ListOf items.
   */
  unsigned int size () const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * Sets the parent SBMLDocument of this SBML object.
   *
   * @param d the SBMLDocument that should become the parent of this
   * ListOf.
   */
  virtual void setSBMLDocument (SBMLDocument* d);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Sets this SBML object to child SBML objects (if any).
   * (Creates a child-parent relationship by the parent)
   *
   * Subclasses must override this function if they define
   * one ore more child elements.
   * Basically, this function needs to be called in
   * constructor, copy constructor and assignment operator.
   *
   * @if cpp 
   * @see setSBMLDocument()
   * @see enablePackageInternal()
   * @endif
   */
  virtual void connectToChild ();
  /** @endcond */


  /**
   * Returns the libSBML type code for this object, namely,
   * @link SBMLTypeCode_t#SBML_LIST_OF SBML_LIST_OF@endlink.
   * 
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @link SBMLTypeCode_t#SBML_LIST_OF SBML_LIST_OF@endlink (default).
   *
   * @note The various ListOf classes mostly differ from each other in what they
   * contain.  Hence, one must call getItemTypeCode() to fully determine the
   * class of this SBML object.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getItemTypeCode()
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode () const;


  /**
   * Get the type code of the objects contained in this ListOf.
   * 
   * @copydetails doc_what_are_typecodes
   *
   * Classes that inherit from the ListOf class should 
   * override this function to return the SBML type code for 
   * the objects contained in this ListOf.  If they do not, 
   * @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink is returned.
   * 
   * @return The ListOf class itself contains no SBML objects, and 
   * therefore returns @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink.
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode () const;


  /**
   * Returns the XML element name of this object, which for ListOf, is
   * always @c "listOf".
   * 
   * @return the XML name of this element.
   */
  virtual const std::string& getElementName () const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Enables/Disables the given package with this element and child
   * elements (if any).
   * (This is an internal implementation for enablePackage function)
   *
   * @note Subclasses of the SBML Core package in which one or more child
   * elements are defined must override this function.
   */
  virtual void enablePackageInternal(const std::string& pkgURI, const std::string& pkgPrefix, bool flag);
  /** @endcond */

protected:
  /** @cond doxygenLibsbmlInternal */

  typedef std::vector<SBase*>           ListItem;
  typedef std::vector<SBase*>::iterator ListItemIter;

  /**
   * Subclasses should override this method to get the list of
   * expected attributes.
   * This function is invoked from corresponding readAttributes()
   * function.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);

  
  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);

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

  virtual bool isValidTypeForList(SBase * item) {return item->getTypeCode() == getItemTypeCode();}

  ListItem mItems;

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
 *
 * @memberof ListOf_t
 */
LIBSBML_EXTERN
ListOf_t *
ListOf_create (unsigned int level, unsigned int version);


/**
 * Frees the given ListOf_t and its constituent items.
 *
 * This function assumes each item in the list is derived from SBase_t.
 *
 * @memberof ListOf_t
 */
LIBSBML_EXTERN
void
ListOf_free (ListOf_t *lo);


/**
 * @return a (deep) copy of this ListOf_t items.
 *
 * @memberof ListOf_t
 */
LIBSBML_EXTERN
ListOf_t *
ListOf_clone (const ListOf_t *lo);


/**
 * Adds a copy of item to the end of this ListOf_t items.
 *
 * @memberof ListOf_t
 */
LIBSBML_EXTERN
int
ListOf_append (ListOf_t *lo, const SBase_t *item);


/**
 * Adds the given item to the end of this ListOf_t items.
 *
 * @memberof ListOf_t
 */
LIBSBML_EXTERN
int
ListOf_appendAndOwn (ListOf_t *lo, SBase_t *item);


/**
 * Adds clones of the given items from the second list to the end of this ListOf_t items.
 *
 * @memberof ListOf_t
 */
LIBSBML_EXTERN
int
ListOf_appendFrom (ListOf_t *lo, ListOf_t *list);


/**
 * inserts a copy of item to this ListOf_t items at the given position.
 *
 * @memberof ListOf_t
 */
LIBSBML_EXTERN
int
ListOf_insert (ListOf_t *lo, int location, const SBase_t *item);


/**
 * inserts the item to this ListOf_t items at the given position.
 *
 * @memberof ListOf_t
 */
LIBSBML_EXTERN
int
ListOf_insertAndOwn (ListOf_t *lo, int location, SBase_t *item);


/**
 * Returns the nth item in this ListOf_t items.
 *
 * @memberof ListOf_t
 */
LIBSBML_EXTERN
SBase_t *
ListOf_get (ListOf_t *lo, unsigned int n);


#if (0)
/**
 * @return item in this ListOf_t items with the given @p sid or @c NULL if no such
 * item exists.
 *
 * @memberof ListOf_t
 */
LIBSBML_EXTERN
SBase_t *
ListOf_getById (ListOf_t *lo, const char *sid);
#endif


/**
 * Removes all items in this ListOf_t structure.
 *
 * If doDelete is true (non-zero), all items in this ListOf_t structure are deleted
 * and cleared, and thus the caller doesn't have to delete those items.
 * Otherwise (zero), all items are just cleared from this ListOf_t structure and the 
 * caller is responsible for deleting all items (In this case, pointers to all 
 * items should be stored elsewhere before calling this function by the caller).
 *
 * @memberof ListOf_t
 */
LIBSBML_EXTERN
void
ListOf_clear (ListOf_t *lo, int doDelete);


/**
 * Removes the nth item from this ListOf_t items and returns a pointer to
 * it.  The caller owns the returned item and is responsible for deleting
 * it.
 *
 * @memberof ListOf_t
 */
LIBSBML_EXTERN
SBase_t *
ListOf_remove (ListOf_t *lo, unsigned int n);


#if (0)
/**
 * Removes item in this ListOf_t items with the given @p sid or @c NULL if no such
 * item exists.  The caller owns the returned item and is repsonsible for
 * deleting it.
 *
 * @memberof ListOf_t
 */
LIBSBML_EXTERN
SBase_t *
ListOf_removeById (ListOf_t *lo, const char *sid);
#endif


/**
 * Returns the number of items in this ListOf_t items.
 *
 * @memberof ListOf_t
 */
LIBSBML_EXTERN
unsigned int
ListOf_size (const ListOf_t *lo);


/**
 * @return the int of SBML structures contained in this ListOf_t or
 * SBML_UNKNOWN (default).
 *
 * @memberof ListOf_t
 */
LIBSBML_EXTERN
int
ListOf_getItemTypeCode (const ListOf_t *lo);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /* ListOf_h */

