/**
 * @file    RelAbsVector.h
 * @brief Definition of the RelAbsVector class.
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
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class RelAbsVector
 * @sbmlbrief{render} Vectors with an absolute value and a relative value.
 *
 * For many elements in the render extension, it is necessary to specify
 * coordinates not in terms of absolute values, but rather in terms of
 * relative values or even a combination of absolute and relative values.
 * Such a pair of values where one represents an absolute value and the other
 * represents a relative value can be expressed by a RelAbsVector.  The
 * RelAbsVector class represents a pair of numerical values where one value
 * represents an absolute value and the other value is a relative value in
 * percent.
 *
 * The relative and absolute values to initialize a RelAbsVector object can
 * either be given as numerical datatypes (double) or as a valid value
 * string.  A value string is a combination of an absolute value and a
 * relative value and the absolute value if given has to come first. So valid
 * value strings would be: "5.0e3+20%", or "100%" or "4".
 */

#ifndef RelAbsVector_H__
#define RelAbsVector_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>

#ifdef __cplusplus

#include <string>
#include <ostream>
#include <sbml/packages/render/extension/RenderExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN RelAbsVector
{
protected:
  /** @cond doxygenLibsbmlInternal */
  
  double mAbs; 
  bool mIsSetAbs;
  double mRel;
  bool mIsSetRel;
  
  /** @endcond */

public:
  /**
   * Constructor with two values.
   * First value sets the absolute value, second sets the relative value (%). 
   *
   * @param a absolute value
   * @param r relative value in % (50 -> 50%)
   */
  RelAbsVector(double a=0.0, double r=0.0);


  /**
   * Constructor with a value string.
   * If the string does not represent a valid value, the relative and the
   * absolute component of the RelAbsVector are set to NaN.
   *
   * @param coordString value as a string
   */
  RelAbsVector(const std::string& coordString);

  
  /**
   * Copy constructor for RelAbsVector.
   *
   * @param orig the RelAbsVector instance to copy.
   */
  RelAbsVector(const RelAbsVector& orig);


  /**
   * Assignment operator for RelAbsVector.
   *
   * @param rhs the RelAbsVector object whose values are to be used as the
   * basis of the assignment.
   */
  RelAbsVector& operator=(const RelAbsVector& rhs);


  /**
   * Creates and returns a deep copy of this RelAbsVector object.
   *
   * @return a (deep) copy of this RelAbsVector object.
   */
  virtual RelAbsVector* clone() const;


  /**
   * Destroy this RelAbsVector object.
   */
  virtual ~RelAbsVector ();


  /**
   * Returns the absolute coordinate value.
   *
   * @return absolute value
   */
  double getAbsoluteValue() const;

  /**
   * Returns the relative coordinate value.
   *
   * @return absolute value
   */
  double getRelativeValue() const;

  /**
  * Returns the coordinate value.
  *
  * @return absolute value
  */
  std::string getCoordinate() const;

  /**
   * Predicate returning @c true if this RelAbsVector's "abs" attribute is set.
   *
   * @return @c true if this RelAbsVector's "abs" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetAbsoluteValue() const;


  /**
   * Predicate returning @c true if this RelAbsVector's "rel" attribute is set.
   *
   * @return @c true if this RelAbsVector's "rel" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetRelativeValue() const;


  /**
  * Predicate returning @c true if this RelAbsVector's coordinate attribute is set.
  *
  * @return @c true if this RelAbsVector's coordinate attribute has been set,
  * otherwise @c false is returned.
  */
  bool isSetCoordinate() const;



  /**
   * Sets the absolute coordinate value.
   *
   * Calling this function with an argument of @c 0.0 or @c NaN is
   * equivalent to unsetting the value.
   *
   * @param abs double value of the "abs" attribute to be set.
   */
  int setAbsoluteValue(double abs);

  /**
   * Sets the relative coordinate value.
   *
   * Calling this function with an argument of @c 0.0 or @c NaN is
   * equivalent to unsetting the value.
   *
   * @param rel double value of the "rel" attribute to be set.
   */
  int setRelativeValue(double rel);

  /**
   * Sets the relative and absolute value.
   *
   * Calling this function with an argument of @c 0.0 or @c NaN is
   * equivalent to unsetting the value.
   *
   * @param abs absolute value
   * @param rel relative value. If the relative value is omitted, it is set to 0.
   */
  int setCoordinate(double abs,double rel=0.0);

  /**
   * Sets the coordinates from the given string.
   * If the string does not represent a valid value, the relative and the
   * absolute component of the RelAbsVector are set to NaN.
   *
   * Calling this function with either cooredinate having a value of @c "0.0" 
   * or @c "NaN" is equivalent to unsetting the value.
   *
   * @param coordString value string
   */
  int setCoordinate(const std::string& coordString);


  /**
   * Unsets the value of the "abs" attribute of this RelAbsVector.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetAbsoluteValue();


  /**
   * Unsets the value of the relative coordinate attribute of this RelAbsVector.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetRelativeValue();


  /**
  * Unsets the value of the "rel" attribute of this RelAbsVector.
  *
  * @copydetails doc_returns_one_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  */
  int unsetCoordinate();


  /**
   * addition operator for RelAbsVector objects
   */
  RelAbsVector operator+(const RelAbsVector& other) const;

  /**
   * Divides a RelAbsVector object by a double value.
   *
   * @param x divisor
   *
   * @return result of division as a new RelAbsVector object
   */
  RelAbsVector operator/(double x) const;

  /**
   * Comparison operator.
   * Return true if two RelAbsVector objects are equal.
   *
   * @return bool true if the two RelAbsValueObjects are equal and
   * false otherwise.
   */
  bool operator==(const RelAbsVector& other) const;

  /**
   * Inverse comparison operator.
   * Return false if two RelAbsVector objects are equal.
   *
   * @return bool false if the two RelAbsValueObjects are equal and
   * true otherwise.
   */
  bool operator!=(const RelAbsVector& other) const;

  /**
   * @return an indication whether this element has been set 
   * (i.e., no zero entries for either relative or absolute coordinate)
   */
  bool empty() const;

  /** 
   * @return a string representation of this object
   */
  std::string toString() const;

  /** 
   * resets this element by setting the two coordinates to @c 0.0
   */
  void erase();

  /** @cond doxygenLibsbmlInternal */
  friend std::ostream& operator<<(std::ostream& os,const RelAbsVector& v);
  /** @endcond */
};

/**
 * Output operator for RelAbsVector objects.
 */
std::ostream& operator<<(std::ostream& os,const RelAbsVector& v);

LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new RelAbsVector_t .
 *
 * @param abs a double, the absolute value to assign to this
 * RelAbsVector_t.
 *
 * @param rel a double, the relative value to assign to this
 * RelAbsVector_t.
 *
 * @memberof RelAbsVector_t
 */
LIBSBML_EXTERN
RelAbsVector_t *
RelAbsVector_create(double abs, double rel);


/**
 * Creates and returns a deep copy of this RelAbsVector_t object.
 *
 * @param rav the RelAbsVector_t structure.
 *
 * @return a (deep) copy of this RelAbsVector_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof RelAbsVector_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
RelAbsVector_clone(const RelAbsVector_t* rav);


/**
 * Frees this RelAbsVector_t object.
 *
 * @param rav the RelAbsVector_t structure.
 *
 * @memberof RelAbsVector_t
 */
LIBSBML_EXTERN
void
RelAbsVector_free(RelAbsVector_t* rav);


/**
 * Returns the value of the "abs" attribute of this RelAbsVector_t.
 *
 * @param rav the RelAbsVector_t structure whose abs is sought.
 *
 * @return the value of the "abs" attribute of this RelAbsVector_t as a double.
 *
 * @memberof RelAbsVector_t
 */
LIBSBML_EXTERN
double
RelAbsVector_getAbsoluteValue(const RelAbsVector_t * rav);


/**
 * Returns the value of the "rel" attribute of this RelAbsVector_t.
 *
 * @param rav the RelAbsVector_t structure whose rel is sought.
 *
 * @return the value of the "rel" attribute of this RelAbsVector_t as a double.
 *
 * @memberof RelAbsVector_t
 */
LIBSBML_EXTERN
double
RelAbsVector_getRelativeValue(const RelAbsVector_t * rav);


/**
 * Predicate returning @c 1 (true) if this RelAbsVector_t's "abs" attribute is
 * set.
 *
 * @param rav the RelAbsVector_t structure.
 *
 * @return @c 1 (true) if this RelAbsVector_t's "abs" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof RelAbsVector_t
 */
LIBSBML_EXTERN
int
RelAbsVector_isSetAbsoluteValue(const RelAbsVector_t * rav);


/**
 * Predicate returning @c 1 (true) if this RelAbsVector_t's "rel" attribute is
 * set.
 *
 * @param rav the RelAbsVector_t structure.
 *
 * @return @c 1 (true) if this RelAbsVector_t's "rel" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof RelAbsVector_t
 */
LIBSBML_EXTERN
int
RelAbsVector_isSetRelativeValue(const RelAbsVector_t * rav);


/**
 * Sets the value of the "abs" attribute of this RelAbsVector_t.
 *
 * @param rav the RelAbsVector_t structure.
 *
 * @param abs double value of the "abs" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RelAbsVector_t
 */
LIBSBML_EXTERN
int
RelAbsVector_setAbsoluteValue(RelAbsVector_t * rav, double abs);


/**
 * Sets the value of the "rel" attribute of this RelAbsVector_t.
 *
 * @param rav the RelAbsVector_t structure.
 *
 * @param rel double value of the "rel" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RelAbsVector_t
 */
LIBSBML_EXTERN
int
RelAbsVector_setRelativeValue(RelAbsVector_t * rav, double rel);


/**
 * Unsets the value of the "abs" attribute of this RelAbsVector_t.
 *
 * @param rav the RelAbsVector_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RelAbsVector_t
 */
LIBSBML_EXTERN
int
RelAbsVector_unsetAbsoluteValue(RelAbsVector_t * rav);


/**
 * Unsets the value of the "rel" attribute of this RelAbsVector_t.
 *
 * @param rav the RelAbsVector_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof RelAbsVector_t
 */
LIBSBML_EXTERN
int
RelAbsVector_unsetRelativeValue(RelAbsVector_t * rav);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !RelAbsVector_H__ */


