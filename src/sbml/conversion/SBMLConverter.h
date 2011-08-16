/**
* @file    SBMLConverter.h
* @brief   Definition of SBMLConverter, the base class for SBML conversion.
* @author  Sarah Keating
* 
* <!--------------------------------------------------------------------------
* This file is part of libSBML.  Please visit http://sbml.org for more
* information about SBML, and the latest version of libSBML.
*
* Copyright (C) 2009-2011 jointly by the following organizations: 
*     1. California Institute of Technology, Pasadena, CA, USA
*     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
*  
* Copyright (C) 2006-2008 by the California Institute of Technology,
*     Pasadena, CA, USA 
*  
* Copyright (C) 2002-2005 jointly by the following organizations: 
*     1. California Institute of Technology, Pasadena, CA, USA
*     2. Japan Science and Technology Agency, Japan
* 
* This library is free software; you can redistribute it and/or modify it
* under the terms of the GNU Lesser General Public License as published by
* the Free Software Foundation.  A copy of the license agreement is provided
* in the file named "LICENSE.txt" included with this software distribution
* and also available online as http://sbml.org/software/libsbml/license.html
* ------------------------------------------------------------------------ -->
*/

#ifndef SBMLConverter_h
#define SBMLConverter_h

#include <sbml/SBMLNamespaces.h>
#include <sbml/conversion/ConversionProperties.h>
#include <sbml/SBMLTypes.h>


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SBMLConverter
{
public:

  /**
  * Constructor.
  */
  SBMLConverter ();

  /**
  * Copy constructor.
  */
  SBMLConverter(const SBMLConverter&);

  /**
  * Destroy this object.
  */
  virtual ~SBMLConverter ();

  /**
  * Assignment operator for SBMLConverter.
  */
  SBMLConverter& operator=(const SBMLConverter&);

  /**
  * Creates and returns a deep copy of this SBMLConverter.
  * 
  * @return a (deep) copy of this SBMLConverter.
  */
  virtual SBMLConverter* clone() const;

  /**
   * @return the current SBML document
   */
  virtual SBMLDocument* getDocument();

  /**
   * @return a const reference to the current SBML document
   */
  virtual const SBMLDocument* getDocument() const;

  /**
   * @return default properties for the converter
   */
  virtual ConversionProperties getDefaultProperties() const;

  /**
   * Convenience method returning the target namespaces of the currently 
   * set properties (or NULL). 
   */
  virtual SBMLNamespaces* getTargetNamespaces();

  /**
   * This function determines whether a given converter matches the 
   * configuration properties given. 
   * 
   * @param props the properties to match
   * 
   * @return <c>true</c> if this covnerter is a match, <c>false</c> otherwise.
   */
  virtual bool matchesProperties(const ConversionProperties &props) const;

  /** 
   * Sets the current SBML document
   * 
   * @param doc the document to use for this conversion
   * 
   * @return status code
   */
  virtual int setDocument(const SBMLDocument* doc);

  /** 
   * Sets the configuration properties to use for the converter
   * 
   * @param props the properties
   * 
   * @return status code
   */  
  virtual int setProperties(const ConversionProperties *props);

  /** 
   * @return the currently set configuration properties 
   */
  virtual ConversionProperties* getProperties() const;


  /** 
   * the actual conversion 
   * 
   * @return status code represeting success/failure/conversion impossible
   */
  virtual int convert(); 

#ifndef SWIG

#endif // SWIG



protected:
  /** @cond doxygen-libsbml-internal */

  SBMLDocument *   mDocument;
  ConversionProperties *mProps;

  friend class SBMLDocument;
  /** @endcond doxygen-libsbml-internal */


private:
  /** @cond doxygen-libsbml-internal */


  /** @endcond doxygen-libsbml-internal */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /* SBMLConverter_h */

