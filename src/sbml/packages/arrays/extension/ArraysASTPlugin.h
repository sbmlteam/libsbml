/**
 * @file    ArraysASTPlugin.h
 * @brief   Definition of ArraysASTPlugin, the plugin class of
 *          arrays package for the AST element.
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

#ifndef ArraysASTPlugin_h
#define ArraysASTPlugin_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/arrays/common/arraysfwd.h>
#include <sbml/SBMLTypeCodes.h>

#ifdef __cplusplus

#include <sbml/SBMLErrorLog.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/extension/ASTBasePlugin.h>
#include <sbml/packages/arrays/sbml/Dimension.h>
#include <sbml/packages/arrays/sbml/Index.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN ArraysASTPlugin : public ASTBasePlugin
{
public:

  /**
   * Constructor
   */
  ArraysASTPlugin (const std::string &uri);

  ArraysASTPlugin ();

  /**
   * Copy constructor. Creates a copy of this SBase object.
   */
  ArraysASTPlugin(const ArraysASTPlugin& orig);


  /**
   * Destroy this object.
   */
  virtual ~ArraysASTPlugin ();


  /**
   * Assignment operator for ArraysASTPlugin.
   */
  ArraysASTPlugin& operator=(const ArraysASTPlugin& orig);


  /**
   * Creates and returns a deep copy of this ArraysASTPlugin object.
   * 
   * @return a (deep) copy of this SBase object
   */
  virtual ArraysASTPlugin* clone () const;



  /**
   * Subclasses must override this method to create, store, and then
   * return an SBML object corresponding to the next XMLToken in the
   * XMLInputStream if they have their specific elements.
   *
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual NewASTNode* createObject (XMLInputStream& stream);


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

  /** @cond doxygen-libsbml-internal */

  /**
   * Sets the parent SBMLDocument of this plugin object.
   *
   * Subclasses which contain one or more SBase derived elements must
   * override this function.
   *
   * @param d the SBMLDocument object to use
   *
   * @see connectToParent
   * @see enablePackageInternal
   */
  virtual void setSBMLDocument (SBMLDocument* d);


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
  void connectToParent (ASTBase *astbase);


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
  /** @endcond doxygen-libsbml-internal */

  virtual NewASTNode* getMath() const;

  virtual bool isSetMath() const;

  virtual int setMath(const NewASTNode* math);

  virtual int unsetMath();

    virtual bool read(XMLInputStream& stream);

  virtual bool representsNumberNode(int type) const;
  virtual bool isFunction(int type) const;
  virtual bool representsUnaryFunction(int type) const;
  virtual bool representsBinaryFunction(int type) const;
  virtual bool representsNaryFunction(int type) const;

    virtual int getTypeFromName(const std::string& name) const;
  virtual const char * getNameFromType(int type) const;

protected:
  /** @cond doxygen-libsbml-internal */

  /*-- data members --*/

  NewASTNode* mMath;


  /** @endcond doxygen-libsbml-internal */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* ArraysASTPlugin_h */
