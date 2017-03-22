/**
 * @file    MultiASTPlugin.h
 * @brief   Definition of MultiASTPlugin, the plugin class of
 *          multi package for the AST element.
 * @author  Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2011 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
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

#ifndef MultiASTPlugin_h
#define MultiASTPlugin_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/multi/common/multifwd.h>
#include <sbml/SBMLTypeCodes.h>

#ifdef __cplusplus

#include <sbml/SBMLErrorLog.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/extension/ASTBasePlugin.h>

LIBSBML_CPP_NAMESPACE_BEGIN




class LIBSBML_EXTERN MultiASTPlugin : public ASTBasePlugin
{
public:

  /**
   * Constructor
   */
  MultiASTPlugin (const std::string &uri);


  /**
   * Copy constructor. Creates a copy of this SBase object.
   */
  MultiASTPlugin(const MultiASTPlugin& orig);


  /**
   * Destroy this object.
   */
  virtual ~MultiASTPlugin ();


  /**
   * Assignment operator for MultiASTPlugin.
   */
  MultiASTPlugin& operator=(const MultiASTPlugin& orig);


  /**
   * Creates and returns a deep copy of this MultiASTPlugin object.
   * 
   * @return a (deep) copy of this SBase object
   */
  virtual MultiASTPlugin* clone () const;



  /**
   * Subclasses must override this method to create, store, and then
   * return an SBML object corresponding to the next XMLToken in the
   * XMLInputStream if they have their specific elements.
   *
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual MultiASTPlugin* createObject (XMLInputStream& stream);


  /**
   * Subclasses must override this method to write out their contained
   * SBML objects as XML elements if they have their specific elements.
   */
  virtual void writeElements (XMLOutputStream& stream) const;



  // ---------------------------------------------------------
  //
  // virtual functions (internal implementation) which should
  // be overridden by subclasses.
  //
  // ---------------------------------------------------------

  /** @cond doxygenLibsbmlInternal */


  /**
   * Returns the prefix of the package extension of this plugin object.
   *
   * @return the prefix of the package extension of this plugin object.
   */
  virtual const std::string& getPrefix() const;


  /**
   * Sets the parent SBML object of this plugin object to
   * this object and child elements (if any).
   * (Creates a child-parent relationship by this plugin object)
   *
   * This function is called when this object is created by
   * the parent element.
   * Subclasses must override this this function if they have one
   * or more child elements.Also, SBasePlugin::connectToParent()
   * must be called in the overridden function.
   *
   * @param sbase the SBase object to use
   *
   * @see setSBMLDocument
   * @see enablePackageInternal
   */
#ifndef LIBSBML_USE_LEGACY_MATH
  void connectToParent (ASTBase *astbase);
#else
  void connectToParent(ASTNode *astbase);
#endif

  /**
   * Enables/Disables the given package with child elements in this plugin
   * object (if any).
   * (This is an internal implementation invoked from
   *  SBase::enablePakcageInternal() function)
   *
   * @note Subclasses in which one or more SBase derived elements are
   * defined must override this function.
   *
   * @see setSBMLDocument
   * @see connectToParent
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix, bool flag);
  /** @endcond */

  const std::string& getSpeciesReference() const;

  bool isSetSpeciesReference() const;

  int setSpeciesReference(const std::string& speciesReference);

  int unsetSpeciesReference();

  const std::string& getRepresentationType() const;

  bool isSetRepresentationType() const;

  int setRepresentationType(const std::string& representationType);

  int unsetRepresentationType();

  //virtual void write(XMLOutputStream& stream) const;

//  using ASTBasePlugin::read;

  virtual bool read(XMLInputStream& stream, const std::string& reqd_prefix,
                                            const XMLToken& currentElement);

  virtual void addExpectedAttributes(ExpectedAttributes& attributes, 
                                     XMLInputStream& stream, int type);


//  using ASTBasePlugin::readAttributes;

  virtual bool readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes,
                               XMLInputStream& stream, const XMLToken& element,
                               int type);


  virtual void writeAttributes(XMLOutputStream& stream, int type) const;
  virtual void writeXMLNS(XMLOutputStream& stream) const;
  //  virtual int getTypeFromName(const std::string& name) const;
  //virtual const char * getNameFromType(int type) const;

  /**
   * Renames the speciesReference SIdRef attribute on this node.
   *
   * @param oldid the old identifier.
   * @param newid the new identifier.
   */
  virtual void renameSIdRefs(const std::string& oldid, const std::string& newid);
protected:
  /** @cond doxygenLibsbmlInternal */

  /*-- data members --*/

  std::string mSpeciesReference;
  std::string mRepresentationType;


  bool hasAttributesSet() const;

  /** @endcond  */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* MultiASTPlugin_h */
