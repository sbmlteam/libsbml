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

#include "RelAbsVector.h"
#include <iostream>

#include <limits>
#include <math.h>
#include <string.h>
#include <stdlib.h>


LIBSBML_CPP_NAMESPACE_BEGIN

/** @cond doxygenLibsbmlInternal */
/*
 * Constructor with two values.
 * First value sets the absolute value, second sets the relative value (%). 
 *
 * @param a absolute value
 * @param a relative value in % (50 -> 50%)
 */
RelAbsVector::RelAbsVector(double a, double r)
  : mAbs(a)
  , mRel(r)
{
}

/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Constructor with a value string.
 * If the string does not represent a valid value, the relative and the
 * absolute component of the RelAbsVector are set to NaN.
 */
RelAbsVector::RelAbsVector(const std::string& coordString)
  : mAbs(0.0)
  , mRel(0.0)
{
  if (!coordString.empty())
  setCoordinate(coordString);
}
/** @endcond */


/*
 * Destroy this object.
 */
RelAbsVector::~RelAbsVector ()
{
}



/** @cond doxygenLibsbmlInternal */
/*
 * Sets the relative and absolute value.
 *
 * @param abs absolute value
 * @param rel relative value. If the relative value is omitted, it is set to 0.
 */
void RelAbsVector::setCoordinate(double abs,double rel)
{
    this->mAbs=abs;
    this->mRel=rel;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the coordinatees from the given string.
 * If the string does not represent a valid value, the relative and the
 * absolute component of the RelAbsVector are set to NaN.
 *
 * @param coordString value string
 */
void RelAbsVector::setCoordinate(const std::string& coordString)
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
            this->mAbs=0.0;
            this->mRel=value;
        }
        else
        {
            // either pp is at the end, then we only have an absolute value
            // or pp has stopped on a '+' or '-' which connects the relative to the absolute value
            // or we have an error
            if((*pp)=='\0')
            {
                this->mAbs=value;
                this->mRel=0.0;
            }
            else if((*pp)=='+' || (*pp)=='-')
            {
                this->mAbs=value;
                p=pp;
                double value=strtod(p,&pp);
                // pp must point to the '%' character
                if((*pp)!='%' || pp!=(s+trimmed.size()-1))
                {
                    result=false;
                }
                else
                {
                    this->mRel=value;
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
        this->mAbs=std::numeric_limits<double>::quiet_NaN();
        this->mRel=std::numeric_limits<double>::quiet_NaN();
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the absolute coordinate value.
 *
 * @param abs absolute value to be set
 */
void RelAbsVector::setAbsoluteValue(double abs)
{
    this->mAbs=abs;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the relative coordinate value.
 *
 * @param rel relative value to be set
 */
void RelAbsVector::setRelativeValue(double rel)
{
    this->mRel=rel;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the absolute coordinate value.
 *
 * @return absolute value
 */
double RelAbsVector::getAbsoluteValue() const
{
    return this->mAbs;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the relative coordinate value.
 *
 * @return absolute value
 */
double RelAbsVector::getRelativeValue() const
{
    return this->mRel;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * addition operator for RelAbsVector objects
 */
RelAbsVector RelAbsVector::operator+(const RelAbsVector& other) const
{
    return RelAbsVector(this->mAbs+other.mAbs,this->mRel+other.mRel);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Divides a RelAbsVector object by a double value.
 *
 * @param x divisor
 *
 * @return result of division as a new RelAbsVector object
 */
RelAbsVector RelAbsVector::operator/(double x) const
{
    return RelAbsVector(this->mAbs/x,this->mRel/x);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
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
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Inverse comparison operator.
 * Return false if two RelAbsVector objects are equal.
 *
 * @return bool false if the two RelAbsValueObjects are equal and
 * true otherwise.
 */
bool RelAbsVector::operator!=(const RelAbsVector& other) const
{
    return !((*this)==other);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
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
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Assignment operator for RelAbsVector objects.
 */
RelAbsVector& RelAbsVector::operator=(const RelAbsVector& src)
{
    if(&src!=this)
    {
        this->mAbs=src.mAbs;
        this->mRel=src.mRel;
    }
    return *this;
}
bool RelAbsVector::empty() const
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
/** @endcond */

LIBSBML_CPP_NAMESPACE_END 
