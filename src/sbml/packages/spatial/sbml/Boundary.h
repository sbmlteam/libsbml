/*
 * @file    Boundary.h
 * @brief   Definition of Boundary, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: Boundary.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/Boundary.h $
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


#ifndef Boundary_H__
#define Boundary_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

BEGIN_C_DECLS

typedef enum
{
    BOUNDARY_TYPE_MIN
  , BOUNDARY_TYPE_MAX
  , BOUNDARY_TYPE_INVALID
} BoundaryType_t;

END_C_DECLS


#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN Boundary : public SBase
{
protected:

  std::string mSpatialId;
  SBMLSpatialTypeCode_t mType;
  double mValue;
  bool  mIsSetValue;


public:

  /**
   * Destructor.
   */ 
  virtual ~Boundary ();

  /**
   * Copy constructor.
   */
   Boundary(const Boundary& source);


  /**
   * Assignment operator.
   */
   Boundary& operator=(const Boundary& source);


  /**
   * Returns the string of the "spatialId" attribute of this Boundary.
   *
   * @return the string of the "spatialId" attribute of this Boundary.
   */
  virtual const std::string& getSpatialId () const;

  /**
   * Get the value of the "value" attribute.
   * 
   * @return the 'value' of this Boundary, as a float-point number.
   */
  double getValue () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * Boundary's "spatialId" attribute has been set.
   *
   * @return @c true if this Boundary's "spatialId" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetSpatialId () const;

 /**
   * Predicate returning @c true or @c false depending on whether this
   * Boundary's "value" attribute has been set.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   * 
   * @return @c true if the "value" attribute of this Boundary has
   * been set, @c false otherwise.
   *
   * @note In SBML Level&nbsp;1, Boundary' "value" is required and
   * therefore <em>should always be set</em>.  (However, in Level&nbsp;1, the
   * attribute has no default value either, so this method will not return
   * @c true until a value has been assigned.)  In SBML Level&nbsp;2,
   * "value" is optional and as such may or may not be set.
   */
  bool isSetValue () const;


  /**
   * Sets the SIdRef string of the "spatialId" attribute of this Boundary.
   *
   * @param spatialId a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setSpatialId (const std::string& spatialId);

   /**
   * Sets the "value" attribute of this Boundary and marks the field
   * as set.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @param value the value to which the "value" attribute should
   * be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  int setValue (double value);


  /**
   * Unsets the value of the "id" attribute of this Boundary.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetSpatialId ();

  /**
   * Unsets the "value" attribute value of this Boundary.
   *
   * @htmlinclude libsbml-comment-set-methods.html
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   */
  int unsetValue ();

 /**
   * (SBML Level&nbsp;1) Get the type of Boundary this is (min or max).
   * 
   * @return the Boundary type (a value drawn from the enumeration <a
   * class="el" href="#Boundary_t">Boundary_t</a>) of this Boundary.
   * The value will be either @c BOUNDARY_TYPE_MIN or 
   * @c BOUNDARY_TYPE_MAX.
   */
  BoundaryType_t getType () const;

  /**
   * Predicate returning @c true or @c false depending on whether this
   * Boundary is min.
   * 
   * @return @c true if this Boundary is min, @c false otherwise.
   */
  bool isBoundaryMin () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Boundary is max.
   * 
   * @return @c true if this Boundary is max, @c false otherwise.
   */
  bool isBoundaryMax () const;

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;

  /**
   * @return the typecode (int) of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  virtual int getTypeCode () const;

  /**
   * Predicate returning @c true or @c false depending on whether all the
   * required attributes for this Boundary object have been set.
   *
   * @note The required attributes for a Boundary object are: spatialId, value
   *
   * @return a boolean value indicating whether all the required
   * attributes for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const ;

  /**
   * @return a (deep) copy of this Boundary.
   */
  virtual Boundary* clone () const;

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the SBML object's next
   * sibling object (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;
  /** @endcond doxygen-libsbml-internal */
    
protected:

  /**
   * Only subclasses may create Boundary.
   */
   Boundary( SBMLSpatialTypeCode_t	type
		     , unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new Boundary with the given SpatialPkgNamespaces object.
   */
   Boundary(  SBMLSpatialTypeCode_t	type
			, SpatialPkgNamespaces* spatialns);


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
  friend class Validator;
  friend class ConsistencyValidator;
  friend class IdentifierConsistencyValidator;
  friend class InternalConsistencyValidator;
  friend class ModelingPracticeValidator;
  friend class OverdeterminedValidator;

 /** @endcond doxygen-libsbml-internal */

};

class LIBSBML_EXTERN BoundaryMin : public Boundary
{
public:

  /**
   * Creates a new BoundaryMin using the given SBML @p level and @p version
   * and @p packageVersion values.
   *
   * @param level an unsigned int, the SBML Level to assign to this BoundaryMin
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * BoundaryMin
   * 
   * @param pkgVersion an unsigned int, the SBML Package Version to assign to this
   * BoundaryMin

   * @note Once a BoundaryMin has been added to an SBMLDocument, the @p level,
   * @p version for the document @em override those used
   * to create the BoundaryMin.  Despite this, the ability to supply the values
   * at creation time is an important aid to creating valid SBML.  Knowledge of
   * the intented SBML Level and Version determine whether it is valid to
   * assign a particular value to an attribute, or whether it is valid to add
   * an object to an existing SBMLDocument.
   */
  BoundaryMin (unsigned int level, unsigned int version, unsigned int pkgVersion);


  /**
   * Creates a new BoundaryMin using the given SpatialPkgNamespaces object
   * @p sbmlns.
   *
   * The SpatialPkgNamespaces object encapsulates SBML Level/Version/PkgVersion/namespaces
   * information.  It is used to communicate the SBML Level, Version, and
   * (in Level&nbsp;3) packages used in addition to SBML Level&nbsp; Core.
   * A common approach to using this class constructor is to create an
   * SBMLNamespaces object somewhere in a program, once, then pass it to
   * object constructors such as this one when needed.
   *
   * @param sbmlns an SpatialPkgNamespaces object.
   *
   * @note Once a BoundaryMin has been added to an SBMLDocument, the @p level,
   * @p version and @p xmlns namespaces for the document @em override those used
   * to create the BoundaryMin.  Despite this, the ability to supply the values
   * at creation time is an important aid to creating valid SBML.  Knowledge of
   * the intented SBML Level and Version determine whether it is valid to
   * assign a particular value to an attribute, or whether it is valid to add
   * an object to an existing SBMLDocument.
   */
  BoundaryMin (SpatialPkgNamespaces* sbmlns);

  /**
   * Destroys this BoundaryMin.
   */
  virtual ~BoundaryMin ();

  /**
   * Creates and returns a deep copy of this Boundary.
   * 
   * @return a (deep) copy of this Boundary.
   */
  virtual BoundaryMin* clone () const;


  /**
   * Accepts the given SBMLVisitor for this instance of BoundaryMin.
   *
   * @param v the SBMLVisitor instance to be used.
   *
   * @return the result of calling <code>v.visit()</code>.
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * Predicate returning @c true or @c false depending on whether
   * all the required attributes for this BoundaryMin object
   * have been set.
   *
   * @note The required attributes for a BoundaryMin object are:
   * spatialId, value
   */
  virtual bool hasRequiredAttributes() const ;

  /* the validator classes need to be friends to access the 
   * protected constructor that takes no arguments
   */
  friend class Validator;
  friend class ConsistencyValidator;
  friend class IdentifierConsistencyValidator;
  friend class InternalConsistencyValidator;
  friend class ModelingPracticeValidator;
  friend class OverdeterminedValidator;

  /** @endcond doxygen-libsbml-internal */

};

class LIBSBML_EXTERN BoundaryMax : public Boundary
{
public:

  /**
   * Creates a new BoundaryMax using the given SBML @p level and @p version
   * and @p packageVersion values.
   *
   * @param level an unsigned int, the SBML Level to assign to this BoundaryMax
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * BoundaryMax
   * 
   * @param pkgVersion an unsigned int, the SBML Package Version to assign to this
   * BoundaryMax

   * @note Once a BoundaryMax has been added to an SBMLDocument, the @p level,
   * @p version for the document @em override those used
   * to create the BoundaryMax.  Despite this, the ability to supply the values
   * at creation time is an important aid to creating valid SBML.  Knowledge of
   * the intented SBML Level and Version determine whether it is valid to
   * assign a particular value to an attribute, or whether it is valid to add
   * an object to an existing SBMLDocument.
   */
  BoundaryMax (unsigned int level, unsigned int version, unsigned int pkgVersion);


  /**
   * Creates a new BoundaryMax using the given SpatialPkgNamespaces object
   * @p sbmlns.
   *
   * The SpatialPkgNamespaces object encapsulates SBML Level/Version/PkgVersion/namespaces
   * information.  It is used to communicate the SBML Level, Version, and
   * (in Level&nbsp;3) packages used in addition to SBML Level&nbsp; Core.
   * A common approach to using this class constructor is to create an
   * SBMLNamespaces object somewhere in a program, once, then pass it to
   * object constructors such as this one when needed.
   *
   * @param sbmlns an SpatialPkgNamespaces object.
   *
   * @note Once a BoundaryMax has been added to an SBMLDocument, the @p level,
   * @p version and @p xmlns namespaces for the document @em override those used
   * to create the BoundaryMin.  Despite this, the ability to supply the values
   * at creation time is an important aid to creating valid SBML.  Knowledge of
   * the intented SBML Level and Version determine whether it is valid to
   * assign a particular value to an attribute, or whether it is valid to add
   * an object to an existing SBMLDocument.
   */
  BoundaryMax (SpatialPkgNamespaces* sbmlns);

  /**
   * Destroys this BoundaryMax.
   */
  virtual ~BoundaryMax ();

  /**
   * Creates and returns a deep copy of this BoundaryMax.
   * 
   * @return a (deep) copy of this BoundaryMax.
   */
  virtual BoundaryMax* clone () const;


  /**
   * Accepts the given SBMLVisitor for this instance of BoundaryMax.
   *
   * @param v the SBMLVisitor instance to be used.
   *
   * @return the result of calling <code>v.visit()</code>.
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * Predicate returning @c true or @c false depending on whether
   * all the required attributes for this BoundaryMax object
   * have been set.
   *
   * @note The required attributes for a BoundaryMax object are:
   * spatialId, value
   */
  virtual bool hasRequiredAttributes() const ;

  /* the validator classes need to be friends to access the 
   * protected constructor that takes no arguments
   */
  friend class Validator;
  friend class ConsistencyValidator;
  friend class IdentifierConsistencyValidator;
  friend class InternalConsistencyValidator;
  friend class ModelingPracticeValidator;
  friend class OverdeterminedValidator;

  /** @endcond doxygen-libsbml-internal */

};


LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

//
// C API will be added here.
//


/*LIBSBML_EXTERN
Boundary_t *
Boundary_createBoundaryMin (unsigned int level, unsigned int version,  unsigned int pkgVersion);


LIBSBML_EXTERN
Boundary_t *
Boundary_createBoundaryMinWithNS (SpatialPkgNamespaces_t *sbmlns);


LIBSBML_EXTERN
Boundary_t *
Boundary_createBoundaryMax (unsigned int level, unsigned int version,  unsigned int pkgVersion);


LIBSBML_EXTERN
Boundary_t *
Boundary_createBoundaryMaxWithNS (SpatialPkgNamespaces_t *sbmlns);
*/

LIBSBML_EXTERN
const char *
Boundary_getSpatialId (const Boundary_t *c);


LIBSBML_EXTERN
double
Boundary_getValue (const Boundary_t *c);


LIBSBML_EXTERN
Boundary_t *
Boundary_clone (const Boundary_t* c);


LIBSBML_EXTERN
int
Boundary_isSetSpatialId (const Boundary_t *c);


LIBSBML_EXTERN
int
Boundary_isSetValue(const Boundary_t *c);


LIBSBML_EXTERN
int
Boundary_setSpatialId (Boundary_t *c, const char *sid);


LIBSBML_EXTERN
int
Boundary_setValue (Boundary_t *c, double value);


LIBSBML_EXTERN
int
Boundary_unsetSpatialId (Boundary_t *c);


LIBSBML_EXTERN
int
Boundary_unsetValue (Boundary_t *c);


LIBSBML_EXTERN
BoundaryType_t
Boundary_getType (const Boundary_t *g);


LIBSBML_EXTERN
int
Boundary_isBoundaryMin (const Boundary_t *g);


LIBSBML_EXTERN
int
Boundary_isBoundaryMax (const Boundary_t *g);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* Boundary_H__ */
