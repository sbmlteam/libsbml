/**
 * @file    SBMLUnitsConverter.h
 * @brief   Definition of SBMLUnitsConverter, for converting units to SI.
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

#ifndef SBMLUnitsConverter_h
#define SBMLUnitsConverter_h

#include <sbml/SBMLNamespaces.h>
#include <sbml/conversion/SBMLConverter.h>
#include <sbml/conversion/SBMLConverterRegister.h>
#include <sbml/SBMLTypes.h>


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SBMLUnitsConverter : public SBMLConverter
{
public:

  /** @cond doxygen-libsbml-internal */
  
  /* register with the ConversionRegistry */
  static void init();  
#ifndef SWIG
  typedef std::map<const std::string, const std::string> GlobalUnits;
  typedef GlobalUnits::iterator                  GlobalUnitsIter;
#endif

  /** @endcond doxygen-libsbml-internal */


  /**
   * Constructor.
   */
  SBMLUnitsConverter ();


  /**
   * Copy constructor.
   */
  SBMLUnitsConverter(const SBMLUnitsConverter&);


  
  /**
   * Destroy this object.
   */
  virtual ~SBMLUnitsConverter ();


  /**
   * Assignment operator for SBMLUnitsConverter.
   */
  SBMLUnitsConverter& operator=(const SBMLUnitsConverter&);


  /**
   * Creates and returns a deep copy of this SBMLUnitsConverter.
   * 
   * @return a (deep) copy of this SBMLUnitsConverter.
   */
  virtual SBMLUnitsConverter* clone() const;


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
   * This function converts the units in the model to base SI units;
   * namely metre, kilogram, second, Ampere, Kelvin, mole and candela.
   * 
   * Unit conversion will only be performed on models that are fully unit 
   * consistent, that is, all objects have associated units and there are no
   * literal numbers with no units specified. In the case of an Level 3
   * involving any math expression, this means that the 'timeUnits' attribute
   * on the <model> must be set and if there are reactions the 'extentUnits'
   * attribute must also be supplied.
   * 
   * This converter has the additional boolean property 'removeUnusedUnits' 
   * that can be used to tell the converter whether or not to remove any 
   * unitDefinitions that are not referred to, after conversion is complete.
   * You can set this value by adding the property using
   * <code>
   *   prop.addOption("removeUnusedUnits", false);
   * </code>
   * to add this property to the converter. The default behaviour is to
   * remove the unused unitDefinitions.
   * 
   * @return integer value indicating success/failure of the function or
   * indicating that this model cannot yet be converted.
   * The possible values returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   * @li @link OperationReturnValues_t#LIBSBML_CONV_CONVERSION_NOT_AVAILABLE LIBSBML_CONV_CONVERSION_NOT_AVAILABLE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_CONV_INVALID_SRC_DOCUMENT LIBSBML_CONV_INVALID_SRC_DOCUMENT @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   */
  virtual int convert();


  /**
   * @return default properties for the converter
   */
  virtual ConversionProperties getDefaultProperties() const;



#ifndef SWIG

#endif // SWIG


private:
  /** @cond doxygen-libsbml-internal */

  bool convertUnits(SBase& sb, Model& m);

  bool convertUnits(SBase& sb, Model& m, std::string &modelUnitAttribute, ASTNode *ast = 0);

  int applyNewUnitDefinition(SBase& sb, Model& m, UnitDefinition *newUD,
    std::string &modelUnitAttribute, ASTNode * ast);

  std::string existsAlready(Model& m, UnitDefinition *newUD);

  bool getRemoveUnusedUnitsFlag();

  void removeUnusedUnitDefinitions(Model& m);
  
  
  bool convertGlobalUnits(Model& m);

 
  bool convertCnUnits(Model& m);


  bool isUsed(Model& m, std::string unitSId);


  bool unacceptable_errors(unsigned int errors);

  bool hasCnUnits(Model& m);

  bool mathHasCnUnits(const ASTNode *ast);

  bool matchesCnUnits(Model& m, std::string& units);

  bool mathMatchesCnUnits(const ASTNode *ast, std::string& units);
  bool convertAST(ASTNode *ast, Model &m);

  unsigned int newIdCount;

  GlobalUnits mGlobalUnits;

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
#endif  /* SBMLUnitsConverter_h */

