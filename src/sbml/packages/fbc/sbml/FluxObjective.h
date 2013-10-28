/**
 * @file    FluxObjective.h
 * @brief   Definition of FluxObjective, the SBase derived class of the fbc package.
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
 * @class FluxObjective
 * @ingroup fbc
 * @brief @htmlinclude pkg-marker-fbc.html
 * Implementation of the 'fbc' package %FluxObjective construct.
 *
 * An integral component in a complete description of a steady-state model is the so-called 'objective function' which generally consists of a linear combination of model variables (fluxes) and a sense (direction). In the FBC package this concept is succinctly captured in the Objective class.
 * <!-- leave this next break as-is to work around some doxygen bug -->
 */ 


#ifndef FluxObjective_H__
#define FluxObjective_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/fbc/common/fbcfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN FluxObjective : public SBase
{
protected:
  /** @cond doxygenLibsbmlInternal */
  std::string mId;
  std::string mName;
  std::string mReaction;
  double      mCoefficient;
  /** @endcond */

public:

  /**
   * Creates a new FluxObjective with the given level, version, and package version.
   */
   FluxObjective(unsigned int level      = FbcExtension::getDefaultLevel(),
          unsigned int version    = FbcExtension::getDefaultVersion(),
          unsigned int pkgVersion = FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new FluxObjective with the given FbcPkgNamespaces object.
   */
   FluxObjective(FbcPkgNamespaces* fbcns);


  /**
   * Copy constructor.
   */
   FluxObjective(const FluxObjective& source);


  /**
   * Assignment operator.
   */
   FluxObjective& operator=(const FluxObjective& source);


  /**
   * Destructor.
   */ 
  virtual ~FluxObjective ();

  
  /**
   * Returns the value of the "id" attribute of this FluxObjective.
   *
   * @return the value of the "id" attribute of this FluxObjective.
   */
  virtual const std::string& getId () const;
  
  
  /**
   * Predicate returning @c true or @c false depending on whether this
   * FluxObjective "id" attribute has been set.
   *
   * @return @c true if this FluxObjective "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetId () const;
  
  
  /**
   * Sets the value of the "id" attribute of this FluxObjective.
   *
   * @return integer value indicating success/failure of the
   * operation. The possible return values are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  virtual int setId (const std::string& id);
  
  
  /**
   * Unsets the value of the "id" attribute of this FluxObjective.
   *
   * @return integer value indicating success/failure of the
   * operation. The possible return values are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetId ();
  
  
  /**
   * Returns the value of the "name" attribute of this FluxObjective.
   *
   * @return the value of the "name" attribute of this FluxObjective.
   */
  virtual const std::string& getName () const;
  
  
  /**
   * Predicate returning @c true or @c false depending on whether this
   * FluxObjective "name" attribute has been set.
   *
   * @return @c true if this FluxObjective "id" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetName () const;
  
  
  /**
   * Sets the value of the "name" attribute of this FluxObjective.
   *
   * @return integer value indicating success/failure of the
   * operation. The possible return values are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  virtual int setName (const std::string& name);
  
  
  /**
   * Unsets the value of the "name" attribute of this FluxObjective.
   *
   * @return integer value indicating success/failure of the
   * operation. The possible return values are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetName ();

  /**
   * Returns the string of the "reaction" attribute of this FluxObjective.
   *
   * @return the string of the "reaction" attribute of this FluxObjective.
   */
  virtual const std::string& getReaction () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * FluxObjective's "reaction" attribute has been set.
   *
   * @return @c true if this FluxObjective's "reaction" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetReaction () const;

  
  /**
   * Sets the SIdRef string of the "reaction" attribute of this FluxObjective.
   *
   * @param reaction a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * operation. The possible return values are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  virtual int setReaction (const std::string& reaction);


  /**
   * Unsets the value of the "id" attribute of this FluxObjective.
   *
   * @return integer value indicating success/failure of the
   * operation. The possible return values are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetReaction ();

  
  /**
   * Returns the value of the "coefficient" attribute of this FluxObjective.
   *
   * @return the value of the "coefficient" attribute of this FluxObjective.
   */
  virtual const double getCoefficient () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * FluxObjective's "coefficient" attribute has been set.
   *
   * @return @c true if this FluxObjective's "coefficient" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetCoefficient () const;

  
  /**
   * Sets the value of the "coefficient" attribute of this FluxObjective.
   *
   * @param coefficient a double coefficient to be set.
   *
   * @return integer value indicating success/failure of the
   * operation. The possible return values are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   */
  virtual int setCoefficient (const double coefficient);


  /**
   * Unsets the value of the "id" attribute of this FluxObjective.
   *
   * @return integer value indicating success/failure of the
   * operation. The possible return values are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int unsetCoefficient ();


  /**
   * Renames all the @c SIdRef attributes on this element, including any
   * found in MathML content (if such exists).
   *
   * This method works by looking at all attributes and (if appropriate)
   * mathematical formulas, comparing the identifiers to the value of @p
   * oldid.  If any matches are found, the matching identifiers are replaced
   * with @p newid.  The method does @em not descend into child elements.
   *
   * @param oldid the old identifier
   * @param newid the new identifier
   */
   virtual void renameSIdRefs(const std::string& oldid, const std::string& newid);


  /**
   * Returns the XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * Creates and returns a deep copy of this FluxObjective.
   * 
   * @return a (deep) copy of this FluxObjective.
   */
  virtual FluxObjective* clone () const;


  /**
   * Returns the libSBML type code of this object instance.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @link SBMLFbcTypeCode_t#SBML_FBC_FLUXOBJECTIVE SBML_FBC_FLUXOBJECTIVE@endlink
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode () const;


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



protected:
  /** @cond doxygenLibsbmlInternal */
  /**
   * Create and return an SBML object of this class, if present.
   *
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

/**
 * @class ListOfFluxObjectives
 * @ingroup fbc
 * @brief @htmlinclude pkg-marker-fbc.html
 * Implementation of the 'fbc' package %ListOfFluxObjectives construct.
 * 
 * The ListOfFluxObjectives is a container for the FluxObjective elements of a Model annotation.
 * 
 * @copydetails doc_what_is_listof
 *
 * @see FluxObjective
 */
class LIBSBML_EXTERN ListOfFluxObjectives : public ListOf
{
public:

  /**
   * Creates and returns a deep copy of this ListOfFluxObjectives.
   * 
   * @return a (deep) copy of this ListOfFluxObjectives.
   */
  virtual ListOfFluxObjectives* clone () const;


  /**
   * Creates a new ListOfFluxObjectives with the given level, version, and package version.
   */
   ListOfFluxObjectives(unsigned int level      = FbcExtension::getDefaultLevel(), 
                 unsigned int version    = FbcExtension::getDefaultVersion(), 
                 unsigned int pkgVersion = FbcExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfFluxObjectives with the given FbcPkgNamespaces object.
   */
   ListOfFluxObjectives(FbcPkgNamespaces* fbcns);


  /**
   * Get a FluxObjective from the ListOfFluxObjectives.
   *
   * @param n the index number of the FluxObjective to get.
   * 
   * @return the nth FluxObjective in this ListOfFluxObjectives.
   *
   * @see size()
   */
  virtual FluxObjective * get(unsigned int n); 


  /**
   * Get a FluxObjective from the ListOfFluxObjectives.
   *
   * @param n the index number of the FluxObjective to get.
   * 
   * @return the nth FluxObjective in this ListOfFluxObjectives.
   *
   * @see size()
   */
  virtual const FluxObjective * get(unsigned int n) const; 

  /**
   * Get a FluxObjective from the ListOfFluxObjectives
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the FluxObjective to get.
   * 
   * @return FluxObjective in this ListOfFluxObjectives
   * with the given @p sid or @c NULL if no such
   * FluxObjective exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual FluxObjective* get (const std::string& sid);


  /**
   * Get a FluxObjective from the ListOfFluxObjectives
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the FluxObjective to get.
   * 
   * @return FluxObjective in this ListOfFluxObjectives
   * with the given @p sid or @c NULL if no such
   * FluxObjective exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const FluxObjective* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfFluxObjectives items and returns a pointer to
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
  virtual FluxObjective* remove (unsigned int n);


  /**
   * Removes item in this ListOfFluxObjectives items with the given identifier.
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
  virtual FluxObjective* remove (const std::string& sid);


  /**
   * Returns the libSBML type code for the SBML objects
   * contained in this ListOf object.
   * 
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for objects contained in this list:
   * @link SBMLTypeCode_t#SBML_FBC_FLUXOBJECTIVE SBML_FBC_FLUXOBJECTIVE@endlink (default).
   *
   * @see getElementName()
   * @see getPackageName()
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
   * Create and return an SBML object of this class, if present.
   *
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);
  /** @endcond */
};

/** @cond doxygenLibsbmlInternal */
/**
 * Used by ListOfFluxObjectives::get() to lookup an SBase based by its 
 * symbol
 */
#ifndef SWIG
template<>
struct IdEq<FluxObjective> : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEq (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <FluxObjective*> (sb)->getReaction() == id; }
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
LIBSBML_EXTERN
FluxObjective_t *
FluxObjective_create(unsigned int level, unsigned int version, unsigned int pkgversion);



LIBSBML_EXTERN
const char *
FluxObjective_getId(FluxObjective_t * fb);


LIBSBML_EXTERN
int
FluxObjective_isSetId(FluxObjective_t * fb);


LIBSBML_EXTERN
int
FluxObjective_setId(FluxObjective_t * fb, const char * id);


LIBSBML_EXTERN
int
FluxObjective_unsetId(FluxObjective_t * fb);

LIBSBML_EXTERN
const char *
FluxObjective_getName(FluxObjective_t * fb);


LIBSBML_EXTERN
int
FluxObjective_isSetName(FluxObjective_t * fb);


LIBSBML_EXTERN
int
FluxObjective_setName(FluxObjective_t * fb, const char * name);


LIBSBML_EXTERN
int
FluxObjective_unsetName(FluxObjective_t * fb);



LIBSBML_EXTERN
const char *
FluxObjective_getReaction(FluxObjective_t * flux);


LIBSBML_EXTERN
int
FluxObjective_isSetReaction(FluxObjective_t * flux);


LIBSBML_EXTERN
int
FluxObjective_setReaction(FluxObjective_t * flux, const char * reaction);


LIBSBML_EXTERN
int
FluxObjective_unsetReaction(FluxObjective_t * flux);


LIBSBML_EXTERN
double
FluxObjective_getCoefficient(FluxObjective_t * flux);


LIBSBML_EXTERN
int
FluxObjective_isSetCoefficient(FluxObjective_t * flux);


LIBSBML_EXTERN
int
FluxObjective_setCoefficient(FluxObjective_t * flux, double coeff);


LIBSBML_EXTERN
int
FluxObjective_unsetCoefficient(FluxObjective_t * flux);



END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* FluxObjective_H__ */
