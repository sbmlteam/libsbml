/**
 * @file    GeneAssociation.h
 * @brief   Definition of GeneAssociation, the SBase derived class of the fbc package.
 * @author  Frank T. Bergmann
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2009-2013 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class GeneAssociation
 * @ingroup FBC
 * @brief @htmlinclude pkg-marker-fbc.html
 * Implementation of the 'fbc' package %GeneAssociation construct.
 *
 * <!-- leave this next break as-is to work around some doxygen bug -->
 */ 
/**
 * @class ListOfGeneAssociations
 * @ingroup FBC
 * @brief @htmlinclude pkg-marker-fbc.html
 * Implementation of the 'fbc' package %ListOfGeneAssociations construct.
 */


#ifndef GeneAssociation_H__
#define GeneAssociation_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/fbc/common/fbcfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/ListOf.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>
#include <sbml/packages/fbc/sbml/Association.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN GeneAssociation : public SBase
{
protected:
  /** @cond doxygenLibsbmlInternal */
  std::string mId;
  std::string mReaction;
  Association* mAssociation;
  /** @endcond */

public:

  /**
   * Creates a new GeneAssociation with the given level, version, and package version.
   */
   GeneAssociation(unsigned int level      = FbcExtension::getDefaultLevel(),
          unsigned int version    = FbcExtension::getDefaultVersion(),
          unsigned int pkgVersion = FbcExtension::getDefaultPackageVersion());

  GeneAssociation(const XMLNode& node, FbcPkgNamespaces* fbcns);

  /**
   * Creates a new GeneAssociation with the given FbcPkgNamespaces object.
   */
   GeneAssociation(FbcPkgNamespaces* fbcns);


  /**
   * Copy constructor.
   */
   GeneAssociation(const GeneAssociation& source);


  /**
   * Assignment operator.
   */
   GeneAssociation& operator=(const GeneAssociation& source);


  /**
   * Destructor.
   */ 
  virtual ~GeneAssociation ();

  
  /**
   * Returns the string of the "id" attribute of this GeneAssociation.
   *
   * @return the string of the "id" attribute of this GeneAssociation.
   */
  virtual const std::string& getId () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * GeneAssociation's "id" attribute has been set.
   *
   * @return @c true if this GeneAssociation's "id" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetId () const;

  
  /**
   * Sets the SIdRef string of the "id" attribute of this GeneAssociation.
   *
   * @param id a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setId (const std::string& id);


  /**
   * Unsets the value of the "id" attribute of this GeneAssociation.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetId ();

  
  /**
   * Returns the string of the "reaction" attribute of this GeneAssociation.
   *
   * @return the string of the "reaction" attribute of this GeneAssociation.
   */
  virtual const std::string& getReaction () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * GeneAssociation's "reaction" attribute has been set.
   *
   * @return @c true if this GeneAssociation's "reaction" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetReaction () const;

  
  /**
   * Sets the SIdRef string of the "reaction" attribute of this GeneAssociation.
   *
   * @param reaction a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setReaction (const std::string& reaction);


  /**
   * Unsets the value of the "id" attribute of this GeneAssociation.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetReaction ();

  
  /** 
   * Creates a new association, sets it to this element and returns it. 
   */
  Association* createAssociation();

  /**
   * Returns Association object of this GeneAssociation.
   *
   * @return Association object of this GeneAssociation.
   */
  virtual const Association* getAssociation () const;

    /**
   * Returns Association object of this GeneAssociation.
   *
   * @return Association object of this GeneAssociation.
   */
  virtual Association* getAssociation ();


  /**
   * Predicate returning @c true or @c false depending on whether this
   * GeneAssociation's "association" element has been set.
   *
   * @return @c true if this GeneAssociation's "association" element has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetAssociation () const;

  
  /**
   * Sets the Association object of this GeneAssociation.
   *
   * @param association a Association object to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setAssociation (const Association* association);


  /**
   * Unsets the Association object of this GeneAssociation.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetAssociation ();


  /**
   * Returns the XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * @return a (deep) copy of this GeneAssociation.
   */
  virtual GeneAssociation* clone () const;


  /**
   * @return the typecode (int) of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  int getTypeCode () const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.  For example:
   *
   *   SBase::writeElements(stream);
   *   mReactants.write(stream);
   *   mProducts.write(stream);
   *   ...
   */
  virtual void writeElements (XMLOutputStream& stream) const;
  /** @endcond */


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the SBML object's next
   * sibling object (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;
  
  
  XMLNode toXML() const;

  
protected:
  /** @cond doxygenLibsbmlInternal */
  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase*
  createObject (XMLInputStream& stream);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to get the list of
   * expected attributes.
   * This function is invoked from corresponding readAttributes()
   * function.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual void readAttributes (const XMLAttributes& attributes, 
                               const ExpectedAttributes& expectedAttributes);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
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
  /** @endcond */
};

class LIBSBML_EXTERN ListOfGeneAssociations : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfGeneAssociations.
   */
  virtual ListOfGeneAssociations* clone () const;


  /**
   * Creates a new ListOfGeneAssociations with the given level, version, and package version.
   */
   ListOfGeneAssociations(unsigned int level      = FbcExtension::getDefaultLevel(), 
                 unsigned int version    = FbcExtension::getDefaultVersion(), 
                 unsigned int pkgVersion = FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfGeneAssociations with the given FbcPkgNamespaces object.
   */
   ListOfGeneAssociations(FbcPkgNamespaces* fbcns);


  /**
   * Get a GeneAssociation from the ListOfGeneAssociations.
   *
   * @param n the index number of the GeneAssociation to get.
   * 
   * @return the nth GeneAssociation in this ListOfGeneAssociations.
   *
   * @see size()
   */
  virtual GeneAssociation * get(unsigned int n); 


  /**
   * Get a GeneAssociation from the ListOfGeneAssociations.
   *
   * @param n the index number of the GeneAssociation to get.
   * 
   * @return the nth GeneAssociation in this ListOfGeneAssociations.
   *
   * @see size()
   */
  virtual const GeneAssociation * get(unsigned int n) const; 

  /**
   * Get a GeneAssociation from the ListOfGeneAssociations
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the GeneAssociation to get.
   * 
   * @return GeneAssociation in this ListOfGeneAssociations
   * with the given @p sid or @c NULL if no such
   * GeneAssociation exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual GeneAssociation* get (const std::string& sid);


  /**
   * Get a GeneAssociation from the ListOfGeneAssociations
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the GeneAssociation to get.
   * 
   * @return GeneAssociation in this ListOfGeneAssociations
   * with the given @p sid or @c NULL if no such
   * GeneAssociation exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const GeneAssociation* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfGeneAssociations items and returns a pointer to
   * it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the item to remove
   * @return the item removed.  As mentioned above, the caller owns the
   * returned item.
   *
   * @see size()
   */
  virtual GeneAssociation* remove (unsigned int n);


  /**
   * Removes item in this ListOfGeneAssociations items with the given identifier.
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
  virtual GeneAssociation* remove (const std::string& sid);


  /**
   * @return the typecode (int) of SBML objects contained in this ListOf or
   * SBML_UNKNOWN (default).
   */
  virtual int getItemTypeCode () const;

  /**
   * Returns the XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const;


protected:

  /** @cond doxygenLibsbmlInternal */
  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);
  /** @endcond */
};

/** @cond doxygenLibsbmlInternal */
/**
 * Used by ListOfGeneAssociations::get() to lookup an SBase based by its 
 * symbol
 */
#ifndef SWIG
template<>
struct IdEq<GeneAssociation> : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEq (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <GeneAssociation*> (sb)->getId() == id; }
};
#endif
/** @endcond */

LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/*
 * C API will be added here.
 */

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* GeneAssociation_H__ */
