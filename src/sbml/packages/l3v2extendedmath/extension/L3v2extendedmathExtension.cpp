/**
 * @file L3v2extendedmathExtension.cpp
 * @brief Implementation of L3v2extendedmathExtension.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */
#include <sbml/extension/SBMLExtensionRegister.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/extension/SBasePluginCreator.h>
#include <sbml/extension/SBMLDocumentPlugin.h>

#include <sbml/packages/l3v2extendedmath/extension/L3v2extendedmathExtension.h>
#include <sbml/packages/l3v2extendedmath/extension/L3v2extendedmathSBMLDocumentPlugin.h>
#include <sbml/packages/l3v2extendedmath/validator/L3v2extendedmathSBMLErrorTable.h>
#include <sbml/packages/l3v2extendedmath/extension/L3v2extendedmathASTPlugin.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Returns the nickname of the SBML Level&nbsp;3 package implemented by this
 * libSBML extension.
 */
const std::string&
L3v2extendedmathExtension::getPackageName()
{
  static const std::string pkgName = "l3v2extendedmath";
  return pkgName;
}


/*
 * Returns the default SBML Level implemented by this libSBML extension.
 */
unsigned int
L3v2extendedmathExtension::getDefaultLevel()
{
  return 3;
}


/*
 * Returns the default SBML Version implemented by this libSBML extension.
 */
unsigned int
L3v2extendedmathExtension::getDefaultVersion()
{
  return 1;
}


/*
 * Returns the default version of the SBML Level&nbsp;3 package implemented by
 * this libSBML extension.
 */
unsigned int
L3v2extendedmathExtension::getDefaultPackageVersion()
{
  return 1;
}


/*
 * Returns the XML namespace URI of the SBML Level&nbsp;3 package implemented
 * by this libSBML extension.
 */
const std::string&
L3v2extendedmathExtension::getXmlnsL3V1V1()
{
  static const std::string xmlns =
    "http://www.sbml.org/sbml/level3/version1/l3v2extendedmath/version1";
  return xmlns;
}



/*
* Returns the XML namespace URI of the SBML Level&nbsp;3 package implemented
* when used in l3v2.
*/
const std::string&
L3v2extendedmathExtension::getXmlnsL3V2()
{
  static const std::string xmlns =
    "http://www.sbml.org/sbml/level3/version2/core";
  return xmlns;
}


/**
 *
 * Adds this L3v2extendedmathExtension to the SBMLExtensionRegistry class
 *
 */
static SBMLExtensionRegister<L3v2extendedmathExtension>
  l3v2extendedmathExtensionRegistry;

//static
//const char* SBML_L3V2EXTENDEDMATH_TYPECODE_STRINGS[] =
//{
//};


/**
 *
 * Instantiate SBMLExtensionNamespaces<L3v2extendedmathExtension> for DLL
 *
 */
template class LIBSBML_EXTERN
  SBMLExtensionNamespaces<L3v2extendedmathExtension>;

/*
 * Creates a new L3v2extendedmathExtension instance.
 */
L3v2extendedmathExtension::L3v2extendedmathExtension()
{
}


/*
 * Copy constructor for L3v2extendedmathExtension.
 */
L3v2extendedmathExtension::L3v2extendedmathExtension(const
  L3v2extendedmathExtension& orig)
  : SBMLExtension( orig )
{
}


/*
 * Assignment operator for L3v2extendedmathExtension.
 */
L3v2extendedmathExtension&
L3v2extendedmathExtension::operator=(const L3v2extendedmathExtension& rhs)
{
  if (&rhs != this)
  {
    SBMLExtension::operator=(rhs);
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this L3v2extendedmathExtension object.
 */
L3v2extendedmathExtension*
L3v2extendedmathExtension::clone() const
{
  return new L3v2extendedmathExtension(*this);
}


/*
 * Destructor for L3v2extendedmathExtension.
 */
L3v2extendedmathExtension::~L3v2extendedmathExtension()
{
}


/*
 * Returns the name of this SBML Level&nbsp;3 package ("l3v2extendedmath").
 */
const std::string&
L3v2extendedmathExtension::getName() const
{
  return getPackageName();
}


/*
 * Returns a string representing the SBML XML namespace of this SBML
 * Level&nbsp;3 package.
 */
const std::string&
L3v2extendedmathExtension::getURI(unsigned int sbmlLevel,
                                  unsigned int sbmlVersion,
                                  unsigned int pkgVersion) const
{
  if (sbmlLevel == 3)
  {
    if (sbmlVersion == 1)
    {
      if (pkgVersion == 1)
      {
        return getXmlnsL3V1V1();
      }
    }
    else if (sbmlVersion > 1)
    {
      return getXmlnsL3V2();
    }
  }

  static std::string empty = "";
  return empty;
}


/*
 * Returns the SBML Level for the given URI of this package.
 */
unsigned int
L3v2extendedmathExtension::getLevel(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 3;
  }
  else if (uri == getXmlnsL3V2())
  {
    return 3;
  }
  return 0;
}


/*
 * Returns the Version within the SBML Level for the given URI of this package.
 */
unsigned int
L3v2extendedmathExtension::getVersion(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }
  else if (uri == getXmlnsL3V2())
  {
    return 2;
  }

  return 0;
}


/*
 * Returns the SBML Level&nbsp;3 package version for the given URI of this
 * package.
 */
unsigned int
L3v2extendedmathExtension::getPackageVersion(const std::string& uri) const
{
  if (uri == getXmlnsL3V1V1())
  {
    return 1;
  }

  return 0;
}


/*
 * Returns a L3v2extendedmathPkgNamespaces object.
 */
SBMLNamespaces*
L3v2extendedmathExtension::getSBMLExtensionNamespaces(const std::string& uri)
  const
{
  L3v2extendedmathPkgNamespaces* pkgns = NULL;

  if (uri == getXmlnsL3V1V1())
  {
    pkgns = new L3v2extendedmathPkgNamespaces(3, 1, 1);
  }
  else if (uri == getXmlnsL3V2())
  {
    pkgns = new L3v2extendedmathPkgNamespaces(3, 2, 0);
  }


  return pkgns;
}


/*
 * Takes a type code of the &ldquo;l3v2extendedmath&rdquo; package and returns
 * a string describing the code.
 */
const char*
L3v2extendedmathExtension::getStringFromTypeCode(int typeCode) const
{
  return NULL;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the entry in the error table at this index.
 */
packageErrorTableEntry
L3v2extendedmathExtension::getErrorTable(unsigned int index) const
{
  return l3v2extendedmathErrorTable[index];
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Return the index in the error table with the given errorId.
 */
unsigned int
L3v2extendedmathExtension::getErrorTableIndex(unsigned int errorId) const
{
  unsigned int tableSize =
    sizeof(l3v2extendedmathErrorTable)/sizeof(l3v2extendedmathErrorTable[0]);
  unsigned int index = 0;

  for (unsigned int i = 0; i < tableSize; i++)
  {
    if (errorId == l3v2extendedmathErrorTable[i].code)
    {
      index = i;
      break;
    }
  }

  return index;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the offset for the errorId range for the "l3v2extendedmath" package.
 */
unsigned int
L3v2extendedmathExtension::getErrorIdOffset() const
{
  return 1400000;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Initializes l3v2extendedmath extension by creating an object of this class
 * with the required SBasePlugin derived objects and registering the object to
 * the SBMLExtensionRegistry class
 */
void
L3v2extendedmathExtension::init()
{
  if (SBMLExtensionRegistry::getInstance().isRegistered(getPackageName()))
  {
    return;
  }

  L3v2extendedmathExtension l3v2extendedmathExtension;


  std::vector<std::string> packageURIs;

  packageURIs.push_back(getXmlnsL3V1V1());
  packageURIs.push_back(getXmlnsL3V2());

  SBaseExtensionPoint sbmldocExtPoint("core", SBML_DOCUMENT);

  SBasePluginCreator<L3v2extendedmathSBMLDocumentPlugin,
    L3v2extendedmathExtension> sbmldocPluginCreator(sbmldocExtPoint,
      packageURIs);

  l3v2extendedmathExtension.addSBasePluginCreator(&sbmldocPluginCreator);
  
  L3v2extendedmathASTPlugin math(getXmlnsL3V1V1());
  l3v2extendedmathExtension.setASTBasePlugin(&math);


  SBMLExtensionRegistry::getInstance().addExtension(&l3v2extendedmathExtension);
}

/** @endcond */




#endif /* __cplusplus */


//static
//const char* SBML_A_S_T__L3_V2__T_Y_P_E_S_STRINGS[] =
//{
//  "max"
//, "min"
//, "quotient"
//, "rateOf"
//, "rem"
//, "implies"
//, "invalid AST_L3V2_TYPES"
//};
//
//
///*
// * Returns the string version of the provided #AST_L3V2_TYPES_t enumeration.
// */
//LIBSBML_EXTERN
//const char*
//AST_L3V2_TYPES_toString(AST_L3V2_TYPES_t astlvtypes)
//{
//  int min = A_ST_L3V2_TYPES_FUNCTION_MAX;
//  int max = FUNCTION_RATE_INVALID;
//
//  if (astlvtypes < min || astlvtypes > max)
//  {
//    return "(Unknown AST_L3V2_TYPES value)";
//  }
//
//  return SBML_A_S_T__L3_V2__T_Y_P_E_S_STRINGS[astlvtypes - min];
//}
//
//
///*
// * Returns the #AST_L3V2_TYPES_t enumeration corresponding to the given string
// * or @sbmlconstant{FUNCTION_RATE_INVALID, AST_L3V2_TYPES_t} if there is no
// * such match.
// */
//LIBSBML_EXTERN
//AST_L3V2_TYPES_t
//AST_L3V2_TYPES_fromString(const char* code)
//{
//  static int size =
//    sizeof(SBML_A_S_T__L3_V2__T_Y_P_E_S_STRINGS)/sizeof(SBML_A_S_T__L3_V2__T_Y_P_E_S_STRINGS[0]);
//  std::string type(code);
//
//  for (int i = 0; i < size; i++)
//  {
//    if (type == SBML_A_S_T__L3_V2__T_Y_P_E_S_STRINGS[i])
//    {
//      return (AST_L3V2_TYPES_t)(i);
//    }
//  }
//
//  return FUNCTION_RATE_INVALID;
//}
//
//
///*
// * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
// * given #AST_L3V2_TYPES_t is valid.
// */
//LIBSBML_EXTERN
//int
//AST_L3V2_TYPES_isValid(AST_L3V2_TYPES_t astlvtypes)
//{
//  int min = A_ST_L3V2_TYPES_FUNCTION_MAX;
//  int max = FUNCTION_RATE_INVALID;
//
//  if (astlvtypes < min || astlvtypes >= max)
//  {
//    return 0;
//  }
//  else
//  {
//    return 1;
//  }
//}
//
//
///*
// * Predicate returning @c 1 (true) or @c 0 (false) depending on whether the
// * given string is a valid #AST_L3V2_TYPES_t.
// */
//LIBSBML_EXTERN
//int
//AST_L3V2_TYPES_isValidString(const char* code)
//{
//  return AST_L3V2_TYPES_isValid(AST_L3V2_TYPES_fromString(code));
//}
//



LIBSBML_CPP_NAMESPACE_END


