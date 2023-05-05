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
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
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
 *
 * @class MultiASTPlugin
 * @sbmlbrief{multi} Extension of ASTBasePlugin.
 *
 * @htmlinclude not-sbml-warning.html
 *
 * The MultiASTPlugin object is used to extend the standard SBML AST
 * (abstract syntax tree) base object (ASTBase) to allow a "ci" element
 * (@sbmlconstant{AST_NAME,ASTNodeType_t}) to have an optional
 * "speciesReference" attribute.  This attribute is used to distinguish which
 * version of a Species should be used in the mathematics.  If a "template"
 * type Species appears as both a reactant and a product in the same
 * Reaction, for example, it may have one amount as a reactant and a
 * different amount as a product, since the same template is being used to
 * match slightly different pools of elements in each case.  By defining the
 * "speciesReference" attribute on an @sbmlconstant{AST_NAME,ASTNodeType_t}
 * that references that Species, the modeler may determine which amount is
 * being referenced.  Similarly, an @sbmlconstant{AST_NAME,ASTNodeType_t}
 * node may reference a SpeciesFeature that appears in multiple Species in
 * the Reaction, and this attribute can be used to specify which one should
 * be used.
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
   * Creates a new MultiASTPlugin object using the given parameters.
   *
   * @copydetails doc_what_are_xmlnamespaces
   *
   * @param uri the URI of the SBML Level&nbsp;3 package implemented by
   * this libSBML package extension.
   */
  MultiASTPlugin (const std::string& uri);


  /**
   * Copy constructor.
   *
   * @param orig the MultiASTPlugin instance to copy.
   */
  MultiASTPlugin(const MultiASTPlugin& orig);


  /**
   * Destructor for MultiASTPlugin.
   */
  virtual ~MultiASTPlugin ();


  /**
   * Assignment operator for MultiModelPlugin.
   *
   * @param orig the object whose values are used as the basis
   * of the assignment.
   */
  MultiASTPlugin& operator=(const MultiASTPlugin& orig);


  /**
   * Creates and returns a deep copy of this MultiASTPlugin object.
   *
   * @return a (deep) copy of this SBase object
   */
  virtual MultiASTPlugin* clone () const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses must override this method to create, store, and then
   * return an SBML object corresponding to the next XMLToken in the
   * XMLInputStream if they have their specific elements.
   *
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or @c NULL if the token was not recognized.
   */
  virtual MultiASTPlugin* createObject (XMLInputStream& stream);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses must override this method to write out their contained
   * SBML objects as XML elements if they have their specific elements.
   */
  virtual void writeElements (XMLOutputStream& stream) const;
  /** @endcond */


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
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
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
  void connectToParent(ASTNode *astbase);
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
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

  /**
   * Returns the value of the "speciesReference" attribute of this MultiASTPlugin.
   *
   * @return the value of the "speciesReference" attribute of this MultiASTPlugin as a string.
   */
  const std::string& getSpeciesReference() const;

  /**
   * Predicate returning @c true if this MultiASTPlugin's "speciesReference" attribute is set.
   *
   * @return @c true if this MultiASTPlugin's "speciesReference" attribute has been set, otherwise
   * @c false is returned.
   */
  bool isSetSpeciesReference() const;

  /**
   * Sets the value of the "speciesReference" attribute of this MultiASTPlugin.
   *
   * @param speciesReference std::string& value of the "speciesReference" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p speciesReference = @c NULL or an empty string is
   * equivalent to calling unsetSpeciesReference().
   */
  int setSpeciesReference(const std::string& speciesReference);

  /**
   * Unsets the value of the "speciesReference" attribute of this MultiASTPlugin.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetSpeciesReference();

  /**
  * Returns the value of the "representationType" attribute of this MultiASTPlugin.
  *
  * @return the value of the "representationType" attribute of this MultiASTPlugin as a string.
  */
  const std::string& getRepresentationType() const;

  /**
   * Predicate returning @c true if this MultiASTPlugin's "representationType" attribute is set.
   *
   * @return @c true if this MultiASTPlugin's "representationType" attribute has been set, otherwise
   * @c false is returned.
   */
  bool isSetRepresentationType() const;

  /**
   * Sets the value of the "representationType" attribute of this MultiASTPlugin.
   *
   * @param representationType std::string& value of the "representationType" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p representationType = @c NULL or an empty string is
   * equivalent to calling unsetRepresentationType().
   */
  int setRepresentationType(const std::string& representationType);

  /**
   * Unsets the value of the "representationType" attribute of this MultiASTPlugin.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetRepresentationType();

  /** @cond doxygenLibsbmlInternal */
  /**
   * Does nothing!  readAttributes, below, does everything necessary.
   */
  virtual bool read(XMLInputStream& stream, const std::string& reqd_prefix,
                                            const XMLToken& currentElement);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Adds the expected attributes for this element
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes, 
                                     XMLInputStream& stream, int type);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Reads the expected attributes into the member data variables
   */
  virtual bool readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes,
                               XMLInputStream& stream, const XMLToken& element,
                               int type);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Writes the attributes to the stream
   */
  virtual void writeAttributes(XMLOutputStream& stream, int type) const;
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Writes the xml namespace attribute to the stream
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;
  /** @endcond */

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
