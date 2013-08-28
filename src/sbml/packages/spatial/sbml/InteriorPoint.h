/*
 * @file    InteriorPoint.h
 * @brief   Definition of InteriorPoint, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: InteriorPoint.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/InteriorPoint.h $
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2009 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */


#ifndef InteriorPoint_H__
#define InteriorPoint_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN InteriorPoint : public SBase
{
protected:
  
  double mCoord1;
  double mCoord2;
  double mCoord3;

  bool  mIsSetCoord1;
  bool  mIsSetCoord2;
  bool  mIsSetCoord3;

public:

  /**
   * Creates a new InteriorPoint with the given level, version, and package version.
   */
   InteriorPoint(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new InteriorPoint with the given SpatialPkgNamespaces object.
   */
   InteriorPoint(SpatialPkgNamespaces* spatialns);


  /**
   * Copy constructor.
   */
   InteriorPoint(const InteriorPoint& source);


  /**
   * Assignment operator.
   */
   InteriorPoint& operator=(const InteriorPoint& source);


  /**
   * Destructor.
   */ 
  virtual ~InteriorPoint ();


  /**
   * Returns the double of the "coord1" attribute of this InteriorPoint.
   *
   * @return the double of the "coord1" attribute of this InteriorPoint.
   */
  virtual double getCoord1 () const;

  /**
   * Returns the double of the "coord2" attribute of this InteriorPoint.
   *
   * @return the double of the "coord2" attribute of this InteriorPoint.
   */
  virtual double getCoord2 () const;

  /**
   * Returns the double of the "coord3" attribute of this InteriorPoint.
   *
   * @return the double of the "coord3" attribute of this InteriorPoint.
   */
  virtual double getCoord3 () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * InteriorPoint's "coord1" attribute has been set.
   *
   * @return @c true if this InteriorPoint's "coord1" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetCoord1 () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * InteriorPoint's "coord2" attribute has been set.
   *
   * @return @c true if this InteriorPoint's "coord2" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetCoord2 () const;

    /**
   * Predicate returning @c true or @c false depending on whether this
   * InteriorPoint's "coord2" attribute has been set.
   *
   * @return @c true if this InteriorPoint's "coord2" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetCoord3 () const;

  /**
   * Sets the double value of the "coord1" attribute of this InteriorPoint.
   *
   * @param coord1 a double to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoord1 (double coord1);

  /**
   * Sets the double value of the "coord2" attribute of this InteriorPoint.
   *
   * @param coord2 a double to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoord2 (double coord2);

  /**
   * Sets the double value of the "coord3" attribute of this InteriorPoint.
   *
   * @param coord3 a double to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setCoord3 (double coord3);

  /**
   * Unsets the value of the "coord1" attribute of this InteriorPoint.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCoord1 ();

  /**
   * Unsets the value of the "coord2" attribute of this InteriorPoint.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCoord2 ();

  /**
   * Unsets the value of the "coord3" attribute of this InteriorPoint.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetCoord3 ();


  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * @return a (deep) copy of this InteriorPoint.
   */
  virtual InteriorPoint* clone () const;


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
   *   mReactans.write(stream);
   *   mProducts.write(stream);
   *   ...
   */
  virtual void writeElements (XMLOutputStream& stream) const;


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the SBML object's next
   * sibling object (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;
  /** @endcond doxygenLibsbmlInternal */
    
protected:
  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase*
  createObject (XMLInputStream& stream);


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

 /* the validator classes need to be friends to access the 
  * protected constructor that takes no arguments
  */
//  friend class MathMLConsistencyValidator;
  friend class Validator;
  friend class ConsistencyValidator;
  friend class IdentifierConsistencyValidator;
  friend class InternalConsistencyValidator;
  
/*  
  friend class L1CompatibilityValidator;
  friend class L2v1CompatibilityValidator;
  friend class L2v2CompatibilityValidator;
  friend class L2v3CompatibilityValidator;
  friend class L2v4CompatibilityValidator;
  friend class SBOConsistencyValidator;
  friend class UnitConsistencyValidator;
*/
  friend class ModelingPracticeValidator;
  friend class OverdeterminedValidator;

  /** @endcond doxygenLibsbmlInternal */

};

class LIBSBML_EXTERN ListOfInteriorPoints : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfInteriorPoints.
   */
  virtual ListOfInteriorPoints* clone () const;


  /**
   * Creates a new ListOfInteriorPoints with the given level, version, and package version.
   */
   ListOfInteriorPoints(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfInteriorPoints with the given SpatialPkgNamespaces object.
   */
   ListOfInteriorPoints(SpatialPkgNamespaces* spatialns);


  /**
   * Get a InteriorPoint from the ListOfInteriorPoints.
   *
   * @param n the index number of the InteriorPoint to get.
   * 
   * @return the nth InteriorPoint in this ListOfInteriorPoints.
   *
   * @see size()
   */
  virtual InteriorPoint * get(unsigned int n); 


  /**
   * Get a InteriorPoint from the ListOfInteriorPoints.
   *
   * @param n the index number of the InteriorPoint to get.
   * 
   * @return the nth InteriorPoint in this ListOfInteriorPoints.
   *
   * @see size()
   */
  virtual const InteriorPoint * get(unsigned int n) const; 

  /**
   * Removes the nth item from this ListOfInteriorPoints items and returns a pointer to
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
  virtual InteriorPoint* remove (unsigned int n);

  /**
   * @return the typecode (int) of SBML objects contained in this ListOf or
   * SBML_UNKNOWN (default).
   */
  virtual int getItemTypeCode () const;

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const;


protected:

  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);
};

LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

//
// C API will be added here.
//

LIBSBML_EXTERN
double
InteriorPoint_getCoord1 (const InteriorPoint_t *p);


LIBSBML_EXTERN
double
InteriorPoint_getCoord2 (const InteriorPoint_t *p);


LIBSBML_EXTERN
double
InteriorPoint_getCoord3 (const InteriorPoint_t *p);


LIBSBML_EXTERN
int
InteriorPoint_isSetCoord1 (const InteriorPoint_t *p);


LIBSBML_EXTERN
int
InteriorPoint_isSetCoord2 (const InteriorPoint_t *p);


LIBSBML_EXTERN
int
InteriorPoint_isSetCoord3 (const InteriorPoint_t *p);


LIBSBML_EXTERN
int
InteriorPoint_setCoord1 (InteriorPoint_t *p, double coord1);


LIBSBML_EXTERN
int
InteriorPoint_setCoord2 (InteriorPoint_t *p, double coord2);


LIBSBML_EXTERN
int
InteriorPoint_setCoord3 (InteriorPoint_t *p, double coord3);


LIBSBML_EXTERN
int
InteriorPoint_unsetCoord1 (InteriorPoint_t *p);


LIBSBML_EXTERN
int
InteriorPoint_unsetCoord2 (InteriorPoint_t *p);


LIBSBML_EXTERN
int
InteriorPoint_unsetCoord3 (InteriorPoint_t *p);


LIBSBML_EXTERN
InteriorPoint_t *
InteriorPoint_clone (const InteriorPoint_t *p);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* InteriorPoint_H__ */
