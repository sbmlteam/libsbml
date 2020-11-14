/**
 * @file    RenderPoint.h
 * @brief Definition of the RenderPoint class.
 * @author  Ralph Gauges
 * @author  Frank T. Bergmann
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2011-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright 2010 Ralph Gauges
 *     Group for the modeling of biological processes 
 *     University of Heidelberg
 *     Im Neuenheimer Feld 267
 *     69120 Heidelberg
 *     Germany
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class RenderPoint
 * @sbmlbrief{render} A point with both absolute and relative values
 *
 * Render objects are often specified relative to the current viewport,
 * i.e. we need a way to specify relative coordinate values in curves. For
 * this we introduced the RenderPoint and the RenderCubicBezier class in the
 * render extension.  Those two classes are used to specify curve and polygon
 * elements.
 *
 * @see RenderCurve
 * @see Polygon
 */

#ifndef RenderPoint_H__
#define RenderPoint_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/RelAbsVector.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class RenderPoint;
class RenderCubicBezier;

class LIBSBML_EXTERN RenderPoint : public SBase
{
protected:
  /** @cond doxygenLibsbmlInternal */
  RelAbsVector mXOffset;
  RelAbsVector mYOffset;
  RelAbsVector mZOffset;
  std::string mElementName;
  /** @endcond */

public:

  /**
   * Creates a new RenderPoint using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * RenderPoint.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * RenderPoint.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this RenderPoint.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  RenderPoint(unsigned int level = RenderExtension::getDefaultLevel(),
              unsigned int version = RenderExtension::getDefaultVersion(),
              unsigned int pkgVersion =
                RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new RenderPoint using the given RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  RenderPoint(RenderPkgNamespaces *renderns);


  /**
   * Creates a new point with the given coordinates.
   *
   * @param renderns the RenderPkgNamespaces object.
   * @param x x coordinate of the RenderPoint object
   * @param y y coordinate of the RenderPoint object
   * @param z z coordinate of the RenderPoint object
   * If the z value is omitted, it is set to 0.
   */ 
  RenderPoint (RenderPkgNamespaces* renderns, const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z = RelAbsVector(0.0,0.0));
        

  /**
   * Creates a new RenderPoint object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * RenderPoint object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the RenderPoint
   * object to be instantiated.
   * @param l2version the version of SBML Level&nbsp;2 to target.
   */
   RenderPoint(const XMLNode& node, unsigned int l2version=4);

  /**
   * Copy constructor for RenderPoint.
   *
   * @param orig the RenderPoint instance to copy.
   */
  RenderPoint(const RenderPoint& orig);


  /**
   * Assignment operator for RenderPoint.
   *
   * @param rhs the RenderPoint object whose values are to be used as the basis
   * of the assignment.
   */
  RenderPoint& operator=(const RenderPoint& rhs);


  /**
   * Comparison operator for RenderPoint objects.
   */
  bool operator==(const RenderPoint& left) const;


  /**
   * Creates and returns a deep copy of this RenderPoint object.
   *
   * @return a (deep) copy of this RenderPoint object.
   */
  virtual RenderPoint* clone() const;


  /**
   * Destructor for RenderPoint.
   */
  virtual ~RenderPoint();


  /**
   * Sets the Z offset to 0.0.
   */ 
  void initDefaults ();

  /**
   * Returns the value of the "x" element of this RenderPoint.
   *
   * @return the value of the "x" element of this RenderPoint as a
   * RelAbsVector&.
   */
  const RelAbsVector& getX() const;


  /**
   * Returns the value of the "x" element of this RenderPoint.
   *
   * @return the value of the "x" element of this RenderPoint as a
   * RelAbsVector&.
   */
  RelAbsVector& getX();


  /**
   * Returns the x coordinate of the RenderPoint as a const reference.
   *
   * @return const reference to x coordinate.
   */ 
  const RelAbsVector& x() const;
        
  /**
   * Returns the x coordinate of the RenderPoint as a reference.
   *
   * @return reference to x coordinate.
   */ 
  RelAbsVector& x();


  /**
   * Returns the value of the "y" element of this RenderPoint.
   *
   * @return the value of the "y" element of this RenderPoint as a
   * RelAbsVector&.
   */
  const RelAbsVector& getY() const;


  /**
   * Returns the value of the "y" element of this RenderPoint.
   *
   * @return the value of the "y" element of this RenderPoint as a
   * RelAbsVector&.
   */
  RelAbsVector& getY();


  /**
   * Returns the y coordinate of the RenderPoint as a const reference.
   *
   * @return const reference to y coordinate.
   */ 
  const RelAbsVector& y() const;
        
  /**
   * Returns the y coordinate of the RenderPoint as a reference.
   *
   * @return reference to y coordinate.
   */ 
  RelAbsVector& y();
        

  /**
   * Returns the value of the "z" element of this RenderPoint.
   *
   * @return the value of the "z" element of this RenderPoint as a
   * RelAbsVector&.
   */
  const RelAbsVector& getZ() const;


  /**
   * Returns the value of the "z" element of this RenderPoint.
   *
   * @return the value of the "z" element of this RenderPoint as a
   * RelAbsVector&.
   */
  RelAbsVector& getZ();


  /**
   * Returns the z coordinate of the RenderPoint as a const reference.
   *
   * @return const reference to z coordinate.
   */ 
  const RelAbsVector& z() const;
  
        
  /**
   * Returns the z coordinate of the RenderPoint as a reference.
   *
   * @return reference to z coordinate.
   */ 
  RelAbsVector& z();
  
  /**
   * Predicate returning @c true if this RenderPoint's "x" element is set.
   *
   * @return @c true if this RenderPoint's "x" element has been set, otherwise
   * @c false is returned.
   */
  bool isSetX() const;


  /**
   * Predicate returning @c true if this RenderPoint's "y" element is set.
   *
   * @return @c true if this RenderPoint's "y" element has been set, otherwise
   * @c false is returned.
   */
  bool isSetY() const;


  /**
   * Predicate returning @c true if this RenderPoint's "z" element is set.
   *
   * @return @c true if this RenderPoint's "z" element has been set, otherwise
   * @c false is returned.
   */
  bool isSetZ() const;


  /**
   * Sets the value of the "x" element of this RenderPoint.
   *
   * @param x RelAbsVector value of the "x" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setX (const RelAbsVector& x);


  /**
   * Sets the value of the "y" element of this RenderPoint.
   *
   * @param y RelAbsVector value of the "y" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setY (const RelAbsVector& y);


  /**
   * Sets the value of the "z" element of this RenderPoint.
   *
   * @param z RelAbsVector value of the "z" element to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setZ (const RelAbsVector& z);


  /**
   * Sets the coordinates of the RenderPoint to the given values.
   *
   * @param x x coordinate to be set.
   * @param y y coordinate to be set.
   * @param z z coordinate to be set. If the z coordinate is omitted, it is set to 0.
   */ 
  void setCoordinates (const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z = RelAbsVector(0.0,0.0));

#ifndef OMIT_DEPRECATED
  /**
   * Sets the coordinates of the RenderPoint to the given values.
   * This method is deprecated, please use setCoordinates. 
   *
   * @param x x coordinate to be set.
   * @param y y coordinate to be set.
   * @param z z coordinate to be set. If the z coordinate is omitted, it is set to 0.
   */ 
  void setOffsets (const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z = RelAbsVector(0.0,0.0));
#endif // OMIT_DEPRECATED
         
  /**
   * Unsets the value of the "x" element of this RenderPoint.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetX();


  /**
   * Unsets the value of the "y" element of this RenderPoint.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetY();


  /**
   * Unsets the value of the "z" element of this RenderPoint.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetZ();


  /**
   * Predicate returning @c true if this abstract RenderPoint is of type
   * RenderPoint
   *
   * @return @c true if this abstract RenderPoint is of type RenderPoint,
   * @c false otherwise
   */
  virtual bool isRenderPoint() const;


  /**
   * Predicate returning @c true if this abstract RenderPoint is of type
   * RenderCubicBezier
   *
   * @return @c true if this abstract RenderPoint is of type
   * RenderCubicBezier, @c false otherwise
   */
  virtual bool isRenderCubicBezier() const;


  /**
   * Returns the XML element name of this RenderPoint object.
   *
   * For RenderPoint, the XML element name is always @c "element".
   *
   * @return the name of this element, i.e. @c "element".
   */
  virtual const std::string& getElementName() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the XML name of this RenderPoint object.
   */
  virtual void setElementName(const std::string& name);

  /** @endcond */


  /**
   * Returns the libSBML type code for this RenderPoint object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_POINT, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * RenderPoint object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * RenderPoint have been set, otherwise @c false is returned.
   *
   *
   * @note The required attributes for the RenderPoint object are:
   * @li "x"
   * @li "y"
   */
  virtual bool hasRequiredAttributes() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Write any contained elements
   */
  virtual void writeElements(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor
   */
  virtual bool accept(SBMLVisitor& v) const;

  /** @endcond */



   

  /**
   * Creates an XMLNode object from this ColorDefinition object.
   *
   * @return the XMLNode with the XML representation for the 
   * ColorDefinition object.
   */
  virtual XMLNode toXML(const std::string& name) const;
    

protected:


  /** @cond doxygenLibsbmlInternal */

  /**
  * Adds the expected attributes for this element
  */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
  * Reads the expected attributes into the member data variables
  */
  virtual void readAttributes(const XMLAttributes& attributes,
    const ExpectedAttributes& expectedAttributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
  * Writes the attributes to the stream
  */
  virtual void writeAttributes(XMLOutputStream& stream) const;

  /** @endcond */


  /**
   *
   * Subclasses should override this method to write their xmlns attriubutes
   * (if any) to the XMLOutputStream. 
   *
   */
  virtual void writeXMLNS (XMLOutputStream& stream) const;
};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new RenderPoint (RenderPoint_t) using the given SBML Level,
 * Version and &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * RenderPoint_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * RenderPoint_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * RenderPoint_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RenderPoint_t
 */
LIBSBML_EXTERN
RenderPoint_t *
RenderPoint_createRenderPoint(unsigned int level,
                              unsigned int version,
                              unsigned int pkgVersion);


/**
 * Creates a new RenderCubicBezier (RenderPoint_t) using the given SBML Level,
 * Version and &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * RenderPoint_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * RenderPoint_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * RenderPoint_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RenderPoint_t
 */
LIBSBML_EXTERN
RenderPoint_t *
RenderPoint_createRenderCubicBezier(unsigned int level,
                                    unsigned int version,
                                    unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this RenderPoint_t object.
 *
 * @param rp the RenderPoint_t structure.
 *
 * @return a (deep) copy of this RenderPoint_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RenderPoint_t
 */
LIBSBML_EXTERN
RenderPoint_t*
RenderPoint_clone(const RenderPoint_t* rp);


/**
 * Frees this RenderPoint_t object.
 *
 * @param rp the RenderPoint_t structure.
 *
 * @memberof RenderPoint_t
 */
LIBSBML_EXTERN
void
RenderPoint_free(RenderPoint_t* rp);


/**
 * Returns the value of the "x" element of this RenderPoint_t.
 *
 * @param rp the RenderPoint_t structure whose x is sought.
 *
 * @return the value of the "x" element of this RenderPoint_t as a
 * RelAbsVector_t.
 *
 * @memberof RenderPoint_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RenderPoint_getX(const RenderPoint_t * rp);


/**
 * Returns the value of the "y" element of this RenderPoint_t.
 *
 * @param rp the RenderPoint_t structure whose y is sought.
 *
 * @return the value of the "y" element of this RenderPoint_t as a
 * RelAbsVector_t.
 *
 * @memberof RenderPoint_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RenderPoint_getY(const RenderPoint_t * rp);


/**
 * Returns the value of the "z" element of this RenderPoint_t.
 *
 * @param rp the RenderPoint_t structure whose z is sought.
 *
 * @return the value of the "z" element of this RenderPoint_t as a
 * RelAbsVector_t.
 *
 * @memberof RenderPoint_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RenderPoint_getZ(const RenderPoint_t * rp);


/**
 * Predicate returning @c 1 (true) if this RenderPoint_t's "x" element is set.
 *
 * @param rp the RenderPoint_t structure.
 *
 * @return @c 1 (true) if this RenderPoint_t's "x" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof RenderPoint_t
 */
LIBSBML_EXTERN
int
RenderPoint_isSetX(const RenderPoint_t * rp);


/**
 * Predicate returning @c 1 (true) if this RenderPoint_t's "y" element is set.
 *
 * @param rp the RenderPoint_t structure.
 *
 * @return @c 1 (true) if this RenderPoint_t's "y" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof RenderPoint_t
 */
LIBSBML_EXTERN
int
RenderPoint_isSetY(const RenderPoint_t * rp);


/**
 * Predicate returning @c 1 (true) if this RenderPoint_t's "z" element is set.
 *
 * @param rp the RenderPoint_t structure.
 *
 * @return @c 1 (true) if this RenderPoint_t's "z" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof RenderPoint_t
 */
LIBSBML_EXTERN
int
RenderPoint_isSetZ(const RenderPoint_t * rp);


/**
 * Sets the value of the "x" element of this RenderPoint_t.
 *
 * @param rp the RenderPoint_t structure.
 *
 * @param x RelAbsVector_t value of the "x" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderPoint_t
 */
LIBSBML_EXTERN
int
RenderPoint_setX(RenderPoint_t * rp, const RelAbsVector_t* x);


/**
 * Sets the value of the "y" element of this RenderPoint_t.
 *
 * @param rp the RenderPoint_t structure.
 *
 * @param y RelAbsVector_t value of the "y" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderPoint_t
 */
LIBSBML_EXTERN
int
RenderPoint_setY(RenderPoint_t * rp, const RelAbsVector_t* y);


/**
 * Sets the value of the "z" element of this RenderPoint_t.
 *
 * @param rp the RenderPoint_t structure.
 *
 * @param z RelAbsVector_t value of the "z" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderPoint_t
 */
LIBSBML_EXTERN
int
RenderPoint_setZ(RenderPoint_t * rp, const RelAbsVector_t* z);


/**
 * Unsets the value of the "x" element of this RenderPoint_t.
 *
 * @param rp the RenderPoint_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderPoint_t
 */
LIBSBML_EXTERN
int
RenderPoint_unsetX(RenderPoint_t * rp);


/**
 * Unsets the value of the "y" element of this RenderPoint_t.
 *
 * @param rp the RenderPoint_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderPoint_t
 */
LIBSBML_EXTERN
int
RenderPoint_unsetY(RenderPoint_t * rp);


/**
 * Unsets the value of the "z" element of this RenderPoint_t.
 *
 * @param rp the RenderPoint_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RenderPoint_t
 */
LIBSBML_EXTERN
int
RenderPoint_unsetZ(RenderPoint_t * rp);


/**
 * Predicate returning @c 1 if this RenderPoint_t is of type RenderPoint_t
 *
 * @param rp the RenderPoint_t structure.
 *
 * @return @c 1 if this RenderPoint_t is of type RenderPoint_t, @c 0 otherwise
 *
 * @memberof RenderPoint_t
 */
LIBSBML_EXTERN
int
RenderPoint_isRenderPoint(const RenderPoint_t * rp);


/**
 * Predicate returning @c 1 if this RenderPoint_t is of type
 * RenderCubicBezier_t
 *
 * @param rp the RenderPoint_t structure.
 *
 * @return @c 1 if this RenderPoint_t is of type RenderCubicBezier_t, @c 0
 * otherwise
 *
 * @memberof RenderPoint_t
 */
LIBSBML_EXTERN
int
RenderPoint_isRenderCubicBezier(const RenderPoint_t * rp);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * RenderPoint_t object have been set.
 *
 * @param rp the RenderPoint_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * RenderPoint_t have been set, otherwise @c 0 (false) is returned.
 *
 *
 * @note The required attributes for the RenderPoint_t object are:
 * @li "x"
 * @li "y"
 *
 * @memberof RenderPoint_t
 */
LIBSBML_EXTERN
int
RenderPoint_hasRequiredAttributes(const RenderPoint_t * rp);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !RenderPoint_H__ */


