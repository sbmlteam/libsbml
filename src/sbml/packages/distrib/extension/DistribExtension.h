/**
 * @file DistribExtension.h
 * @brief Definition of DistribExtension.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 *
 * @class DistribExtension
 * @sbmlbrief{distrib} Base extension class for the package.
 *
 * @htmlinclude not-sbml-warning.html
 *
 * This is the Distrib package extension of the SBMLExtension class that is
 * used to facilitate libSBML plug-ins in the implementation of an
 * SBMLLevel&nbsp;3 package.
 *
 * @class DistribPkgNamespaces
 * @sbmlbrief{distrib} SBMLNamespaces extension.
 *
 * @htmlinclude not-sbml-warning.html
 */


#ifndef DistribExtension_H__
#define DistribExtension_H__


#include <sbml/common/extern.h>
#include <sbml/SBMLTypeCodes.h>


#ifdef __cplusplus


#include <sbml/extension/SBMLExtension.h>
#include <sbml/extension/SBMLExtensionNamespaces.h>
#include <sbml/extension/SBMLExtensionRegister.h>

#ifndef DISTRIB_CREATE_NS
#define DISTRIB_CREATE_NS(variable, sbmlns)\
EXTENSION_CREATE_NS(DistribPkgNamespaces, variable, sbmlns);
#endif

#ifndef DISTRIB_CREATE_NS_WITH_VERSION
#define DISTRIB_CREATE_NS_WITH_VERSION(variable, sbmlns, version)\
  EXTENSION_CREATE_NS_WITH_VERSION(DistribPkgNamespaces, variable, sbmlns, version);
#endif

#include <vector>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DistribExtension : public SBMLExtension
{
public:

  /**
   * Returns the nickname of the SBML Level&nbsp;3 package implemented by this
   * libSBML extension.
   *
   * @return the package nickname, as a string.
   *
   * @copydetails doc_note_static_methods
   */
  static const std::string& getPackageName();


  /**
   * Returns the default SBML Level implemented by this libSBML extension.
   *
   * @return the SBML Level, as an unsigned integer.
   *
   * @copydetails doc_note_static_methods
   */
  static unsigned int getDefaultLevel();


  /**
   * Returns the default SBML Version implemented by this libSBML extension.
   *
   * @return the Version within the default SBML Level, as an unsigned integer.
   *
   * @copydetails doc_note_static_methods
   */
  static unsigned int getDefaultVersion();


  /**
   * Returns the default version of the SBML Level&nbsp;3 package implemented
   * by this libSBML extension.
   *
   * @return the default version number of the SBML Level&nbsp;3 package
   * definition, as an unsigned integer.
   *
   * @copydetails doc_note_static_methods
   */
  static unsigned int getDefaultPackageVersion();


  /**
   * Returns the XML namespace URI of the SBML Level&nbsp;3 package implemented
   * by this libSBML extension.
   *
   * @return the XML namespace, as a string.
   *
   * @copydetails doc_note_static_methods
   */
  static const std::string& getXmlnsL3V1V1();


  /**
   * Returns the XML namespace URI of the SBML Level&nbsp;3 package implemented
   * by this libSBML extension.
   *
   * @return the XML namespace, as a string.
   *
   * @copydetails doc_note_static_methods
   */
  static const std::string& getXmlnsL3V2V1();


  /**
   * Creates a new DistribExtension instance.
   */
  DistribExtension();


  /**
   * Copy constructor for DistribExtension.
   *
   * @param orig the DistribExtension instance to copy.
   */
  DistribExtension(const DistribExtension& orig);


  /**
   * Assignment operator for DistribExtension.
   *
   * @param rhs the DistribExtension object whose values are to be used as the
   * basis of the assignment.
   */
  DistribExtension& operator=(const DistribExtension& rhs);


  /**
   * Creates and returns a deep copy of this DistribExtension object.
   *
   * @return a (deep) copy of this DistribExtension object.
   */
  virtual DistribExtension* clone() const;


  /**
   * Destructor for DistribExtension.
   */
  virtual ~DistribExtension();


  /**
   * Returns the name of this SBML Level&nbsp;3 package ("distrib").
   *
   * @return a string representing the name of this package ("distrib").
   */
  virtual const std::string& getName() const;


  /**
   * Returns a string representing the SBML XML namespace of this SBML
   * Level&nbsp;3 package.
   *
   * @param sbmlLevel the level of SBML.
   *
   * @param sbmlVersion the version of SBML.
   *
   * @param pkgVersion the version of this package.
   *
   * @return a string representing the name of this package ("distrib").
   *
   * The namespace URI constructed by this method corresponds to the
   * combination of the Level and Version of SBML, and the Version of the SBML
   * Level&nbsp;3 package. (At the time of this writing, the only SBML Level
   * that supports packages is Level&nbsp;3, so the value of @p sbmlLevel is
   * necessarily always <code>3</code>.)
   */
  virtual const std::string& getURI(unsigned int sbmlLevel,
                                    unsigned int sbmlVersion,
                                    unsigned int pkgVersion) const;


  /**
   * Returns the SBML Level for the given URI of this package.
   *
   * @param uri the string of the URI that represents one of the versions of
   * the "distrib" package.
   *
   * @return the SBML Level for the given URI of this package, or @c 0 if the
   * given URI is invalid, or for a different package.
   */
  virtual unsigned int getLevel(const std::string& uri) const;


  /**
   * Returns the Version within the SBML Level for the given URI of this
   * package.
   *
   * @param uri the string of the URI that represents one of the versions of
   * the "distrib" package.
   *
   * @return the SBML Version within the SBML Level for the given URI of this
   * package, or @c 0 if the given URI is invalid, or for a different package.
   */
  virtual unsigned int getVersion(const std::string& uri) const;


  /**
   * Returns the SBML Level&nbsp;3 package version for the given URI of this
   * package.
   *
   * @param uri the string of the URI that represents one of the versions of
   * the "distrib" package.
   *
   * @return the version of the SBML Level&nbsp;3 package for the given URI of
   * this package, or @c 0 if the given URI is invalid, or for a different
   * package.
   */
  virtual unsigned int getPackageVersion(const std::string& uri) const;


  /**
   * Returns a DistribPkgNamespaces object.
   *
   * @param uri the string of the URI that represents one of the versions of
   * the "distrib" package.
   *
   * @return DistribPkgNamespaces object corresponding to the given URI of this
   * package, or @c NULL if the given URI is not defined in the "distrib"
   * package.
   */
  virtual SBMLNamespaces* getSBMLExtensionNamespaces(const std::string& uri)
    const;


  /**
   * Takes a type code of the &ldquo;distrib&rdquo; package and returns a
   * string describing the code.
   *
   * @param typeCode a libSBML type code defined by the libSBML extension
   * implementing support for the SBML Level&nbsp;3 &ldquo;distrib&rdquo;
   * package.
   *
   * @return a text string representing the type code given by @p typeCode. If
   * the type code is unrecognized for this implementation of the libSBML
   * &ldquo;distrib&rdquo; package, the string returned will be <code>"(Unknown
   * SBML Distrib Type)"</code>.
   */
  virtual const char* getStringFromTypeCode(int typeCode) const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the entry in the error table at this index.
   *
   * @param index an unsigned integer representing the index of the error.
   *
   * @return packageErrorTableEntry object in the DistribSBMLErrorTable.
   */
  virtual packageErrorTableEntry getErrorTable(unsigned int index) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Return the index in the error table with the given errorId.
   *
   * @param errorId an unsigned integer representing the errorId of the error.
   *
   * @return unsigned int representing the index in the DistribSBMLErrorTable
   * corresponding to the errorId given.
   */
  virtual unsigned int getErrorTableIndex(unsigned int errorId) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the offset for the errorId range for the "distrib" package.
   *
   * @return unsigned int representing the offset for errors in the
   * DistribSBMLErrorTable.
   */
  virtual unsigned int getErrorIdOffset() const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns true if the package has multiple versions.
   *
   * @return true if multiple versions, false otherwise.
   */
  virtual bool hasMultiplePackageVersions() const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Initializes distrib extension by creating an object of this class with the
   * required SBasePlugin derived objects and registering the object to the
   * SBMLExtensionRegistry class
   *
   * This function is automatically invoked when creatingthe following global
   * object in DistribExtension.cpp
   *
   * static SBMLExtensionRegister<DistribExtension> distribExtensionRegistry;
   */
  static void init();

  /** @endcond */


};

/**
 *
 * Required typedef definitions
 *
 * DistribPkgNamespace is derived from SBMLNamespaces class and used when
 * creating an object of SBase derived classes defined in the distrib package
 *
 * SBMLExtensionNamespaces<DistribExtension> must be instantiated in
 * DistribExtension.cpp for DLL
 *
 */
typedef SBMLExtensionNamespaces<DistribExtension> DistribPkgNamespaces;


LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




LIBSBML_CPP_NAMESPACE_BEGIN


/**
 * @enum SBMLDistribTypeCode_t
 * @brief SBMLDistribTypeCode_t Enumeration of possible types in the libSBML
 * &ldquo;distrib&rdquo; package implementation.
 *
 * @copydetails doc_what_are_typecodes
 *
 * @copydetails doc_additional_typecode_details
 */
typedef enum
{
  SBML_DISTRIB_DRAWFROMDISTRIBUTION                  =  1500  /*!<DistribDrawFromDistribution */
, SBML_DISTRIB_DISTRIBINPUT                          =  1501  /*!<DistribInput */
, SBML_DISTRIB_DISTRIBUTION                          =  1502  /*!<DistribDistribution */
, SBML_DISTRIB_UNIVARIATEDISTRIBUTION                =  1503  /*!<DistribUnivariateDistribution */
, SBML_DISTRIB_MULTIVARIATEDISTRIBUTION              =  1504  /*!<DistribMultivariateDistribution */
, SBML_DISTRIB_CONTINUOUSUNIVARIATEDISTRIBUTION      =  1505  /*!<DistribContinuousUnivariateDistribution */
, SBML_DISTRIB_DISCRETEUNIVARIATEDISTRIBUTION        =  1506  /*!<DistribDiscreteUnivariateDistribution */
, SBML_DISTRIB_CATEGORICALUNIVARIATEDISTRIBUTION     =  1507  /*!<DistribCategoricalUnivariateDistribution */
, SBML_DISTRIB_UNCERTVALUE                           =  1508  /*!<DistribUncertValue */
, SBML_DISTRIB_UNCERTBOUND                           =  1509  /*!<DistribUncertBound */
, SBML_DISTRIB_EXTERNALDISTRIBUTION                  =  1510  /*!<DistribExternalDistribution */
, SBML_DISTRIB_EXTERNALPARAMETER                     =  1511  /*!<DistribExternalParameter */
, SBML_DISTRIB_NORMALDISTRIBUTION                    =  1512  /*!<DistribNormalDistribution */
, SBML_DISTRIB_UNIFORMDISTRIBUTION                   =  1513  /*!<DistribUniformDistribution */
, SBML_DISTRIB_CATEGORICALDISTRIBUTION               =  1514  /*!<DistribCategoricalDistribution */
, SBML_DISTRIB_CATEGORY                              =  1515  /*!<DistribCategory */
, SBML_DISTRIB_BERNOULLIDISTRIBUTION                 =  1516  /*!<DistribBernoulliDistribution */
, SBML_DISTRIB_BETADISTRIBUTION                      =  1517  /*!<DistribBetaDistribution */
, SBML_DISTRIB_BINOMIALDISTRIBUTION                  =  1518  /*!<DistribBinomialDistribution */
, SBML_DISTRIB_CAUCHYDISTRIBUTION                    =  1519  /*!<DistribCauchyDistribution */
, SBML_DISTRIB_CHISQUAREDISTRIBUTION                 =  1520  /*!<DistribChiSquareDistribution */
, SBML_DISTRIB_EXPONENTIALDISTRIBUTION               =  1521  /*!<DistribExponentialDistribution */
, SBML_DISTRIB_FDISTRIBUTION                         =  1522  /*!<DistribFDistribution */
, SBML_DISTRIB_GAMMADISTRIBUTION                     =  1523  /*!<DistribGammaDistribution */
, SBML_DISTRIB_GEOMETRICLDISTRIBUTION                =  1524  /*!<DistribGeometricDistribution */
, SBML_DISTRIB_HYPERGEOMETRICDISTRIBUTION            =  1525  /*!<DistribHypergeometricDistribution */
, SBML_DISTRIB_INVERSEGAMMADISTRIBUTION              =  1526  /*!<DistribInverseGammaDistribution */
, SBML_DISTRIB_LAPLACEDISTRIBUTION                   =  1527  /*!<DistribLaPlaceDistribution */
, SBML_DISTRIB_LOGNORMALDISTRIBUTION                 =  1528  /*!<DistribLogNormalDistribution */
, SBML_DISTRIB_LOGISTICDISTRIBUTION                  =  1529  /*!<DistribLogisticDistribution */
, SBML_DISTRIB_NEGATIVEBINOMIALDISTRIBUTION          =  1530  /*!<DistribNegativeBinomialDistribution */
, SBML_DISTRIB_PARETODISTRIBUTION                    =  1531  /*!<DistribParetoDistribution */
, SBML_DISTRIB_POISSONDISTRIBUTION                   =  1532  /*!<DistribPoissonDistribution */
, SBML_DISTRIB_RAYLEIGHDISTRIBUTION                  =  1533  /*!<DistribRayleighDistribution */
, SBML_DISTRIB_STUDENTTDISTRIBUTION                  =  1534  /*!<DistribStudentTDistribution */
, SBML_DISTRIB_WEIBULLDISTRIBUTION                   =  1535  /*!<DistribWeibullDistribution */
, SBML_DISTRIB_UNCERTAINTY                           =  1536  /*!<DistribUncertainty */
, SBML_DISTRIB_UNCERTSTATISTICS                      =  1537  /*!<DistribUncertStatistics */
, SBML_DISTRIB_UNCERTSTATISTICSPAN                   =  1538  /*!<DistribUncertStatisticSpan */
} SBMLDistribTypeCode_t;




LIBSBML_CPP_NAMESPACE_END




#endif /* !DistribExtension_H__ */


