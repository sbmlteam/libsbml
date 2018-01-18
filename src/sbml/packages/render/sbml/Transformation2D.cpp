/**
 * @file    Transformation2D.cpp
 * @brief   class representing a 2D affine transformation
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

#include "Transformation2D.h"

#include <string.h>
#include <stdlib.h>
#include <sstream>
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

LIBSBML_CPP_NAMESPACE_BEGIN


const double Transformation2D::IDENTITY2D[6]={1.0,0.0,0.0,1.0,0.0,0.0};

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Transformation2D object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
Transformation2D::Transformation2D (unsigned int level, unsigned int version, unsigned int pkgVersion) : 
    Transformation(level,version, pkgVersion)
{
    this->updateMatrix2D();
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Transformation2D object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
Transformation2D::Transformation2D (RenderPkgNamespaces* renderns):
    Transformation(renderns)
{
    this->updateMatrix2D();
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */

/*
 * Copy constructor.
 */
Transformation2D::Transformation2D(const Transformation2D& other)
  : Transformation(other)
{
  setMatrix2D(other.getMatrix2D());
}


/*
 * Destroy this object.
 */
Transformation2D::~Transformation2D ()
{
}



#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor with id and values for the matrix.
 */
Transformation2D::Transformation2D(RenderPkgNamespaces* renderns, const double matrix[6]):Transformation(renderns)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. Transformation2D::Transformation2D(const double matrix[6]) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
    this->setMatrix2D(matrix);
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */
#endif // OMIT_DEPRECATED

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Transformation2D object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * Transformation2D object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
 *
 * @param node the XMLNode object reference that describes the Transformation2D
 * object to be instantiated.
 */
Transformation2D::Transformation2D(const XMLNode& node, unsigned int l2version)
  :Transformation(node, l2version)
{
      ExpectedAttributes ea;
    addExpectedAttributes(ea);

    this->readAttributes(node.getAttributes(), ea);

    
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the XML element name of this object.
 *
 * This is overridden by subclasses to return a string appropriate to the
 * SBML component.  For example, Model defines it as returning "model",
 * CompartmentType defines it as returning "compartmentType", etc.
 */
const std::string& Transformation2D::getElementName() const
{
  static std::string name = "Transformation2D";
  return name;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the 2D matrix to the values given in the array.
 * The 3D matrix is updated accordingly.
 *
 * @param m array with new values to be set for this Transformation object.
 */
void Transformation2D::setMatrix2D(const double m[6])
{
    unsigned int i;
    for(i=0;i<6;++i)
    {
        mMatrix2D[i]=m[i];
    }
    this->updateMatrix3D();
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the 2D matrix which is an array of double values of length 6.
 *
 * @return a pointer to the array of numbers for the 2D transformation.
 */
const double* Transformation2D::getMatrix2D() const
{
    return mMatrix2D;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns a 2D identity matrix.
 * The matrix contains 6 double values.
 */
const double* Transformation2D::getIdentityMatrix2D()
{
    return IDENTITY2D;
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */
/*
 * Adds the transformation attribute to the given XMLAttributes object.
 *
 * @param transformation the transformation to add as attribute.
 *
 * @param att XMLAttributes where the attribute needs to be added to
 */
void Transformation2D::addTransformation2DAttributes(const Transformation2D& transformation,XMLAttributes& att)
{
    if(transformation.isSetMatrix() && memcmp(transformation.mMatrix,getIdentityMatrix(),12*sizeof(double))!=0)
    {
        att.add("transform",transformation.get2DTransformationString());
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.  For example:
 *
 *   SBase::writeAttributes(stream);
 *   stream.writeAttribute( "id"  , mId   );
 *   stream.writeAttribute( "name", mName );
 *   ...
 */
void Transformation2D::writeAttributes (XMLOutputStream& stream) const
{
  Transformation::writeAttributes(stream);
  
  if(this->isSetMatrix() && memcmp(this->mMatrix,getIdentityMatrix(),12*sizeof(double))!=0)
    {
        stream.writeAttribute("transform", getPrefix(), this->get2DTransformationString());
    }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the transformation array as a string for storage in an XML
 * attribute.
 */
std::string Transformation2D::get2DTransformationString() const
{
    // transform the matrix to a string
    std::ostringstream os;
    unsigned int i;
    os << mMatrix2D[0];
    for(i=1;i<6;++i)
    {
        os << "," << mMatrix2D[i];
    }
    return os.str();
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the 3D matrix from the 2D matrix.
 */
void Transformation2D::updateMatrix3D()
{
    mMatrix[0]=mMatrix2D[0];
    mMatrix[1]=mMatrix2D[1];
    mMatrix[2]=0.0;
    mMatrix[3]=mMatrix2D[2];
    mMatrix[4]=mMatrix2D[3];
    mMatrix[5]=0.0;
    mMatrix[6]=0.0;
    mMatrix[7]=0.0;
    mMatrix[8]=1.0;
    mMatrix[9]=mMatrix2D[4];
    mMatrix[10]=mMatrix2D[5];
    mMatrix[11]=0.0;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Fills the 2D matrix with data from the 3D matrix.
 */
void Transformation2D::updateMatrix2D()
{
    mMatrix2D[0]=mMatrix[0];
    mMatrix2D[1]=mMatrix[1];
    mMatrix2D[2]=mMatrix[3];
    mMatrix2D[3]=mMatrix[4];
    mMatrix2D[4]=mMatrix[9];
    mMatrix2D[5]=mMatrix[10];
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the 2D matrix to the values given in the array.
 * The 2D matrix is updated accordingly.
 *
 * @param m array with new values to be set for this Transformation object.
 */
void Transformation2D::setMatrix(const double m[12])
{
    this->Transformation::setMatrix(m);
    this->updateMatrix2D();
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
Transformation2D::addExpectedAttributes(ExpectedAttributes& attributes)
{
  Transformation::addExpectedAttributes(attributes);

  attributes.add("transform");
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void Transformation2D::readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{
    Transformation::readAttributes(attributes, expectedAttributes);
    std::string s;
    attributes.readInto("transform", s, getErrorLog(), false, getLine(), getColumn());
    if(!s.empty())
    {
        this->parseTransformation(s);
    }
    else
    {
        this->updateMatrix2D();
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Tries to parse the numerical values from the given string
 * and fill the matrix with them. The method will accept strings
 * representing 6 or 12 numerical values and fill the 2D or 3D matrix
 * accordingly.
 * The other matrix is updated automatically.
 *
 * @param transformationString string representing 6 or 12 numerical values.
 */
void Transformation2D::parseTransformation(const std::string& transformationString)
{
    // the string should contain a list of 6 komma seperated numbers
    // if it doesn't, we set the matrix to the identity matrix
    bool result=true;
    std::string delimiter=",";
    std::size_t lastPos=transformationString.find_first_not_of(delimiter);
    std::size_t pos;
    unsigned int index=0;
    while(lastPos!=std::string::npos)
    {
        if(index > 5)
        {
            result=false;
            break;
        }
        pos=transformationString.find_first_of(delimiter,lastPos);
        double value=strtod(transformationString.substr(lastPos,pos-lastPos).c_str(),NULL);
        mMatrix2D[index]=value;
        index++;
        lastPos=transformationString.find_first_not_of(delimiter,pos);
    }
    if(!result || index != 6)
    {
        this->setMatrix2D(Transformation2D::IDENTITY2D);
    }
    this->updateMatrix3D();
}
/** @endcond */

LIBSBML_CPP_NAMESPACE_END 
