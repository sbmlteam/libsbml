/**
 * @file    RelAbsVector.cpp
 * @brief   class for representing coordinates that
 *          can have a relative and an absolute part
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
 * ---------------------------------------------------------------------- -->*/

#include <sbml/packages/render/sbml/RelAbsVector.h>
#include <iostream>

#include <limits>
#include <math.h>
#include <string.h>
#include <stdlib.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus



/*
 * Constructor with two values.
 * First value sets the absolute value, second sets the relative value (%). 
 *
 * @param a absolute value
 * @param a relative value in % (50 -> 50%)
 */
RelAbsVector::RelAbsVector(double a, double r)
  : mAbs(0.0)
  , mIsSetAbs(false)
  , mRel(0.0)
  , mIsSetRel(false)
{
  setAbsoluteValue(a);
  setRelativeValue(r);
}


/*
 * Constructor with a value string.
 * If the string does not represent a valid value, the relative and the
 * absolute component of the RelAbsVector are set to NaN.
 */
RelAbsVector::RelAbsVector(const std::string& coordString)
  : mAbs(0.0)
  , mIsSetAbs ( false )
  , mRel(0.0)
  , mIsSetRel ( false )
{
  if (!coordString.empty())
  {
    setCoordinate(coordString);
  }
}


/*
* Copy constructor for RelAbsVector objects.
*/
RelAbsVector::RelAbsVector(const RelAbsVector& orig)
{
  mAbs = orig.mAbs;
  mIsSetAbs = orig.mIsSetAbs;
  mRel = orig.mRel;
  mIsSetRel = orig.mIsSetRel;
}


/*
 * Assignment operator for RelAbsVector objects.
 */
RelAbsVector& RelAbsVector::operator=(const RelAbsVector& rhs)
{
    if(&rhs!=this)
    {
        this->mAbs=rhs.mAbs;
        mIsSetAbs = rhs.mIsSetAbs;
        this->mRel=rhs.mRel;
        mIsSetRel = rhs.mIsSetRel;
    }
    return *this;
}


/*
 * Creates and returns a deep copy of this RelAbsVector object.
*/
RelAbsVector*
RelAbsVector::clone() const
{
  return new RelAbsVector(*this);
}


/*
 * Destructor for RelAbsVector.
 */
RelAbsVector::~RelAbsVector ()
{
}



/*
* Returns the absolute coordinate value.
*
* @return absolute value
*/
double 
RelAbsVector::getAbsoluteValue() const
{
  return mAbs;
}


/*
* Returns the relative coordinate value.
*
* @return absolute value
*/
double 
RelAbsVector::getRelativeValue() const
{
  return mRel;
}


std::string
RelAbsVector::getCoordinate() const
{
  return toString();
}


/*
 * Predicate returning @c true if this RelAbsVector's "abs" attribute is set.
 */
bool
RelAbsVector::isSetAbsoluteValue() const
{
  return mIsSetAbs;
}


/*
 * Predicate returning @c true if this RelAbsVector's "rel" attribute is set.
 */
bool
RelAbsVector::isSetRelativeValue() const
{
  return mIsSetRel;
}


/*
* Predicate returning @c true if this RelAbsVector's "rel" attribute is set.
*/
bool
RelAbsVector::isSetCoordinate() const
{
  return (!util_isNaN(mAbs) && !util_isNaN(mRel));
}


/*
* Sets the absolute coordinate value.
*
* @param abs absolute value to be set
*/
int 
RelAbsVector::setAbsoluteValue(double abs)
{
  this->mAbs = abs;
  // values of 0 or Nan are considered unset
  if (util_isEqual(abs, 0.0) || util_isNaN(abs))
  {
    mIsSetAbs = false;
  }
  else
  {
    mIsSetAbs = true;
  }
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the relative coordinate value.
*
* @param rel relative value to be set
*/
int 
RelAbsVector::setRelativeValue(double rel)
{
  this->mRel = rel;
  // values of 0 or Nan are considered unset
  if (util_isEqual(rel, 0.0) || util_isNaN(rel))
  {
    mIsSetRel = false;
  }
  else
  {
    mIsSetRel = true;
  }
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the relative and absolute value.
 *
 * @param abs absolute value
 * @param rel relative value. If the relative value is omitted, it is set to 0.
 */
int 
RelAbsVector::setCoordinate(double abs,double rel)
{
  setAbsoluteValue(abs);
  return setRelativeValue(rel);
}


/*
 * Sets the coordinatees from the given string.
 * If the string does not represent a valid value, the relative and the
 * absolute component of the RelAbsVector are set to NaN.
 *
 * @param coordString value string
 */
int
RelAbsVector::setCoordinate(const std::string& coordString)
{
    bool result=true;
    // first we remove all whitespaces from the string
    size_t i,iMax=coordString.size();
    std::string trimmed;
    for(i=0;i<iMax;++i)
    {
        if(!(coordString[i]==' ' || coordString[i]=='\t' || coordString[i]=='\n' || coordString[i]=='\r'))
        {
            trimmed+=coordString[i];
        }
    }
    if(trimmed.empty())
    {
        result=false;
    }
    else
    {
        char* s=new char[trimmed.size()+1];
        strncpy(s,trimmed.c_str(),trimmed.size()+1);
        char* p=s;
        char* pp;
        // p is changed if the value is parsed
        double value=strtod(p,&pp);
        // check if strtod stopped at the '%' and if it is the last character in the string
        if((*pp)=='%' && pp==(s+trimmed.size()-1))
        {
            // we only have a relative value
          setAbsoluteValue(0.0);
          setRelativeValue(value);
        }
        else
        {
            // either pp is at the end, then we only have an absolute value
            // or pp has stopped on a '+' or '-' which connects the relative to the absolute value
            // or we have an error
            if((*pp)=='\0')
            {
              setAbsoluteValue(value);
              setRelativeValue(0.0);
            }
            else if((*pp)=='+' || (*pp)=='-')
            {
              setAbsoluteValue(value);
              p=pp;
                double value=strtod(p,&pp);
                // pp must point to the '%' character
                if((*pp)!='%' || pp!=(s+trimmed.size()-1))
                {
                    result=false;
                }
                else
                {
                  setRelativeValue(value);
                }
            }
            else
            {
                result=false;
            }
        }
        delete[] s;
    }
    if(result==false)
    {
        // set relative and absolute value to NaN
      setAbsoluteValue(std::numeric_limits<double>::quiet_NaN());
      setRelativeValue(std::numeric_limits<double>::quiet_NaN());
    }
    return LIBSBML_OPERATION_SUCCESS;
}


/*
* unSets the absolute coordinate value.
*
*/
int
RelAbsVector::unsetAbsoluteValue()
{
  setAbsoluteValue(0.0);
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* UnSets the relative coordinate value.
*/
int
RelAbsVector::unsetRelativeValue()
{
  setRelativeValue(0.0);
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the relative and absolute value.
*
* @param abs absolute value
* @param rel relative value. If the relative value is omitted, it is set to 0.
*/
int
RelAbsVector::unsetCoordinate()
{
  setRelativeValue(0.0);
  setAbsoluteValue(0.0);
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * addition operator for RelAbsVector objects
 */
RelAbsVector 
RelAbsVector::operator+(const RelAbsVector& other) const
{
    return RelAbsVector(this->mAbs+other.mAbs,this->mRel+other.mRel);
}


/*
 * Divides a RelAbsVector object by a double value.
 *
 * @param x divisor
 *
 * @return result of division as a new RelAbsVector object
 */
RelAbsVector 
RelAbsVector::operator/(double x) const
{
    return RelAbsVector(this->mAbs/x,this->mRel/x);
}


/*
 * Comparison operator.
 * Return true if two RelAbsVector objects are equal.
 *
 * @return bool true if the two RelAbsValueObjects are equal and
 * false otherwise.
 */
bool RelAbsVector::operator==(const RelAbsVector& other) const
{
    bool result=true;
    if(this->mAbs==0.0)
    {
        result=(fabs(other.mAbs) < 1e-200);
    }
    else
    {
        result=(fabs((this->mAbs-other.mAbs)/this->mAbs) < 1e-200);
    }
    if(result)
    {
        if(this->mRel==0.0)
        {
            result=(fabs(other.mRel) < 1e-200);
        }
        else
        {
            result=(fabs((this->mRel-other.mRel)/this->mRel) < 1e-200);
        }
    }
    return result;
}


/*
 * Inverse comparison operator.
 * Return false if two RelAbsVector objects are equal.
 *
 * @return bool false if the two RelAbsValueObjects are equal and
 * true otherwise.
 */
bool 
RelAbsVector::operator!=(const RelAbsVector& other) const
{
    return !((*this)==other);
}


/*
 * Output operator for RelAbsVector objects.
 */
std::ostream& operator<<(std::ostream& os,const RelAbsVector& v)
{
    if(v.mAbs!=0.0 || v.mRel==0.0)
    {
        os << v.mAbs;
        if(v.mRel<0.0)
        {
            os << v.mRel << "%";
        }
        else if(v.mRel>0.0)
        {
            os << "+" << v.mRel << "%";
        }
    }
    else
    {
        os << v.mRel << "%";
    }
    return os;
}


bool 
RelAbsVector::empty() const
{
  return ((mAbs == 0.0 || util_isNaN(mAbs)) && (mRel == 0.0 || util_isNaN(mRel)));
}


std::string RelAbsVector::toString() const
{
  std::stringstream str;
  str << *this;
  return str.str();
}
void RelAbsVector::erase()
{
  mAbs = 0;
  mRel = 0;
}


#endif /* __cplusplus */


/*
 * Creates a new RelAbsVector_t .
 */
LIBSBML_EXTERN
RelAbsVector_t *
RelAbsVector_create(double abs, double rel)
{
  return new RelAbsVector(abs, rel);
}


/*
 * Creates and returns a deep copy of this RelAbsVector_t object.
 */
LIBSBML_EXTERN
RelAbsVector_t*
RelAbsVector_clone(const RelAbsVector_t* rav)
{
  if (rav != NULL)
  {
    return static_cast<RelAbsVector_t*>(rav->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this RelAbsVector_t object.
 */
LIBSBML_EXTERN
void
RelAbsVector_free(RelAbsVector_t* rav)
{
  if (rav != NULL)
  {
    delete rav;
  }
}


/*
 * Returns the value of the "abs" attribute of this RelAbsVector_t.
 */
LIBSBML_EXTERN
double
RelAbsVector_getAbsoluteValue(const RelAbsVector_t * rav)
{
  return (rav != NULL) ? rav->getAbsoluteValue() : util_NaN();
}


/*
 * Returns the value of the "rel" attribute of this RelAbsVector_t.
 */
LIBSBML_EXTERN
double
RelAbsVector_getRelativeValue(const RelAbsVector_t * rav)
{
  return (rav != NULL) ? rav->getRelativeValue() : util_NaN();
}


/*
 * Predicate returning @c 1 (true) if this RelAbsVector_t's "abs" attribute is
 * set.
 */
LIBSBML_EXTERN
int
RelAbsVector_isSetAbsoluteValue(const RelAbsVector_t * rav)
{
  return (rav != NULL) ? static_cast<int>(rav->isSetAbsoluteValue()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RelAbsVector_t's "rel" attribute is
 * set.
 */
LIBSBML_EXTERN
int
RelAbsVector_isSetRelativeValue(const RelAbsVector_t * rav)
{
  return (rav != NULL) ? static_cast<int>(rav->isSetRelativeValue()) : 0;
}


/*
 * Sets the value of the "abs" attribute of this RelAbsVector_t.
 */
LIBSBML_EXTERN
int
RelAbsVector_setAbsoluteValue(RelAbsVector_t * rav, double abs)
{
  return (rav != NULL) ? rav->setAbsoluteValue(abs) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "rel" attribute of this RelAbsVector_t.
 */
LIBSBML_EXTERN
int
RelAbsVector_setRelativeValue(RelAbsVector_t * rav, double rel)
{
  return (rav != NULL) ? rav->setRelativeValue(rel) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "abs" attribute of this RelAbsVector_t.
 */
LIBSBML_EXTERN
int
RelAbsVector_unsetAbsoluteValue(RelAbsVector_t * rav)
{
  return (rav != NULL) ? rav->unsetAbsoluteValue() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "rel" attribute of this RelAbsVector_t.
 */
LIBSBML_EXTERN
int
RelAbsVector_unsetRelativeValue(RelAbsVector_t * rav)
{
  return (rav != NULL) ? rav->unsetRelativeValue() : LIBSBML_INVALID_OBJECT;
}






LIBSBML_CPP_NAMESPACE_END


