/*
 * @file    GeometryDefinition.h
 * @brief   Definition of GeometryDefinition, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: GeometryDefinition.h 10673 2010-01-17 07:18:20Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/GeometryDefinition.h $
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


#ifndef GeometryDefinition_H__
#define GeometryDefinition_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/spatial/common/spatialfwd.h>

BEGIN_C_DECLS

typedef enum
{
    GEOMETRICDEFN_TYPE_SAMPLEDFIELDGEOMETRY
  , GEOMETRICDEFN_TYPE_PARAMETRICGEOMETRY
  , GEOMETRICDEFN_TYPE_ANALYTICGEOMETRY
  , GEOMETRICDEFN_TYPE_CSGEOMETRY
  , GEOMETRICDEFN_TYPE_INVALID
} GeometryDefinitionType_t;

END_C_DECLS

#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class SBMLVisitor;

class LIBSBML_EXTERN GeometryDefinition : public SBase
{
protected:

  std::string mSpatialId;
  SBMLSpatialTypeCode_t mType;

public:
  //
  //  Only subclasses may create GeometryDefinitions.
  // 
  GeometryDefinition (  SBMLSpatialTypeCode_t	type
					 , unsigned int				level
					 , unsigned int				version );

  GeometryDefinition (  SBMLSpatialTypeCode_t	type
					 , SpatialPkgNamespaces*			sbmlns );


  /**
   * Assignment operator.
   */
   GeometryDefinition& operator=(const GeometryDefinition& source);


  /**
   * Destructor.
   */ 
  virtual ~GeometryDefinition ();


  /**
   * Returns the string of the "spatialId" attribute of this GeometryDefinition.
   *
   * @return the string of the "spatialId" attribute of this GeometryDefinition.
   */
  virtual const std::string& getSpatialId () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * GeometryDefinition's "spatialId" attribute has been set.
   *
   * @return @c true if this GeometryDefinition's "spatialId" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetSpatialId () const;

  
  /**
   * Sets the SIdRef string of the "spatialId" attribute of this GeometryDefinition.
   *
   * @param SpatialId a SIdRef string to be set.
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
   * Unsets the value of the "id" attribute of this GeometryDefinition.
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
   * (SBML Level&nbsp;1) Get the type of geometryDefinition this is.
   * 
   * @return the geometryDefinition type (a value drawn from the enumeration <a
   * class="el" href="#GeometryDefinition_t">GeometryDefinition_t</a>) of this GeometryDefinition.
   * The value will be either @c GEOMETRICDEFN_TYPE_SAMPLEDFIELDGEOMETRY or 
   * @c GEOMETRICDEFN_TYPE_PARAMETRICGEOMETRY or @c GEOMETRICDEFN_TYPE_ANALYTICGEOMETRY or 
   * @c GEOMETRICDEFN_TYPE_CSGEOMETRY.
   */
  GeometryDefinitionType_t getType () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * GeometryDefinition is an SampleFieldGeometry.
   * 
   * @return @c true if this GeometryDefinition is an SampleFieldGeometry, @c false otherwise.
   */
  bool isSampledFieldGeometry () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * GeometryDefinition is an ParametricGeometry.
   * 
   * @return @c true if this GeometryDefinition is an ParametricGeometry, @c false otherwise.
   */
  bool isParametricGeometry () const;


  /**
   * Predicate returning @c true or @c false depending on whether this 
   * GeometryDefinition is an AnalyticGeometry.
   *
   * @return @c true if this GeometryDefinition is a AnalyticGeometry, @c false
   * otherwise.
   */
  bool isAnalyticGeometry () const;


  /**
   * Predicate returning @c true or @c false depending on whether this 
   * GeometryDefinition is an CSGeometry.
   *
   * @return @c true if this GeometryDefinition is a CSGeometry, @c false
   * otherwise.
   */
  bool isCSGeometry () const;

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const;

  /**
   * @return a (deep) copy of this GeometryDefinition.
   */
  virtual GeometryDefinition* clone () const;

  /**
   * @return the typecode (int) of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  virtual int getTypeCode () const;

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
  virtual void writeElements (XMLOutputStream& stream) const {
  };

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the SBML object's next
   * sibling object (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;
  /** @endcond doxygenLibsbmlInternal */
    

   /** @cond doxygenLibsbmlInternal */
  /**
   * Sets the parent SBMLDocument of this SBML object.
   *
   * @param d the SBMLDocument object to use
   */
  virtual void setSBMLDocument (SBMLDocument* d);

  /**
   * Sets this SBML object to child SBML objects (if any).
   * (Creates a child-parent relationship by the parent)
   *
   * Subclasses must override this function if they define
   * one ore more child elements.
   * Basically, this function needs to be called in
   * constructor, copy constructor, assignment operator.
   *
   * @see setSBMLDocument
   * @see enablePackageInternal
   */
  virtual void connectToChild (){ 
  };

  /**
   * Enables/Disables the given package with this element and child
   * elements (if any).
   * (This is an internal implementation for enablePakcage function)
   *
   * @note Subclasses in which one or more child elements are defined
   * must override this function.
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix, bool flag);
  /** @endcond doxygenLibsbmlInternal */



protected:

  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase*
	  createObject (XMLInputStream& stream) {
		  return NULL;
  };

  /**
   * Subclasses should override this method to read (and store) XHTML,
   * MathML, etc. directly from the XMLInputStream.
   *
   * @return true if the subclass read from the stream, false otherwise.
   */
  virtual bool readOtherXML (XMLInputStream& stream) {
	  return false;
  };


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
/*  
  friend class L1CompatibilityValidator;
  friend class L2v1CompatibilityValidator;
  friend class L2v2CompatibilityValidator;
  friend class L2v3CompatibilityValidator;
  friend class L2v4CompatibilityValidator;
  friend class MathMLConsistencyValidator;
  friend class SBOConsistencyValidator;
  friend class UnitConsistencyValidator;
*/
  friend class ModelingPracticeValidator;
  friend class OverdeterminedValidator;

  /** @endcond doxygenLibsbmlInternal */


};

class LIBSBML_EXTERN ListOfGeometryDefinitions : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfGeometryDefinitions.
   */
  virtual ListOfGeometryDefinitions* clone () const;


  /**
   * Creates a new ListOfGeometryDefinitions with the given level, version, and package version.
   */
   ListOfGeometryDefinitions(unsigned int level      = SpatialExtension::getDefaultLevel(),
          unsigned int version    = SpatialExtension::getDefaultVersion(),
          unsigned int pkgVersion = SpatialExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfGeometryDefinitions with the given spatialPkgNamespaces object.
   */
   ListOfGeometryDefinitions(SpatialPkgNamespaces* spatialsns);

  /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions.
   *
   * @param n the index number of the GeometryDefinition to get.
   * 
   * @return the nth GeometryDefinition in this ListOfGeometryDefinitions.
   *
   * @see size()
   */
  virtual GeometryDefinition * get(unsigned int n); 

  /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions.
   *
   * @param n the index number of the GeometryDefinition to get.
   * 
   * @return the nth GeometryDefinition in this ListOfGeometryDefinitions.
   *
   * @see size()
   */
  virtual const GeometryDefinition * get(unsigned int n) const; 

  /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the GeometryDefinition to get.
   * 
   * @return GeometryDefinition in this ListOfGeometryDefinitions
   * with the given id or NULL if no such
   * GeometryDefinition exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual GeometryDefinition* get (const std::string& sid);


  /**
   * Get a GeometryDefinition from the ListOfGeometryDefinitions
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the GeometryDefinition to get.
   * 
   * @return GeometryDefinition in this ListOfGeometryDefinitions
   * with the given id or NULL if no such
   * GeometryDefinition exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const GeometryDefinition* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfGeometryDefinitions items and returns a pointer to
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
  virtual GeometryDefinition* remove (unsigned int n);


  /**
   * Removes item in this ListOfGeometryDefinitions items with the given identifier.
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
  virtual GeometryDefinition* remove (const std::string& sid);


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

  virtual bool isValidTypeForList(SBase * item);
};

/** @cond doxygenLibsbmlInternal */
/**
 * Used by ListOfGeometryDefinitions::get() to lookup an SBase based by its 
 * spatialId
 */
#ifndef SWIG
template<>
struct IdEq<GeometryDefinition> : public std::unary_function<SBase*, bool>
{
  const std::string& id;

  IdEq (const std::string& id) : id(id) { }
  bool operator() (SBase* sb) 
       { return static_cast <GeometryDefinition*> (sb)->getSpatialId() == id; }
};

#endif
/** @endcond doxygenLibsbmlInternal */

LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

//
// C API will be added here.
//


LIBSBML_EXTERN
const char *
GeometryDefinition_getSpatialId (const GeometryDefinition_t *g);


LIBSBML_EXTERN
int
GeometryDefinition_isSetSpatialId (const GeometryDefinition_t *g);


LIBSBML_EXTERN
int
GeometryDefinition_setSpatialId (GeometryDefinition_t *g, const char *coordSys);


LIBSBML_EXTERN
int
GeometryDefinition_unsetSpatialId (GeometryDefinition_t *g);


LIBSBML_EXTERN
GeometryDefinitionType_t
GeometryDefinition_getType (const GeometryDefinition_t *g);


LIBSBML_EXTERN
int
GeometryDefinition_isAnalyticGeometry (const GeometryDefinition_t *g);


LIBSBML_EXTERN
int
GeometryDefinition_isSampledFieldGeometry (const GeometryDefinition_t *g);


LIBSBML_EXTERN
int
GeometryDefinition_isParametricGeometry (const GeometryDefinition_t *g);


LIBSBML_EXTERN
int
GeometryDefinition_isCSGeometry (const GeometryDefinition_t *g);


LIBSBML_EXTERN
GeometryDefinition_t *
GeometryDefinition_clone (const GeometryDefinition_t *g);


LIBSBML_EXTERN
GeometryDefinition_t *
ListOfGeometryDefinitions_getById (ListOf_t *lo, const char *sid);


LIBSBML_EXTERN
GeometryDefinition_t *
ListOfGeometryDefinitions_removeById (ListOf_t *lo, const char *sid);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* GeometryDefinition_H__ */
