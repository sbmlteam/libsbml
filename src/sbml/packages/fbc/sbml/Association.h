/**
 * @file    Association.h
 * @brief   Definition of Association, the contents of a GeneAssociation.
 * @author  Frank T. Bergmann
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2009-2013 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class Association
 * @ingroup fbc
 * @brief @htmlinclude pkg-marker-fbc.html
 * Implementation of the 'fbc' package %Association construct.
 */

#ifndef Association_H__
#define Association_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/fbc/common/fbcfwd.h>

#ifdef __cplusplus

#include <string>
#include <vector>

#include <sbml/SBase.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/ListOf.h>
#include <sbml/packages/fbc/extension/FbcExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN

enum AssociationTypeCode_t
{
    GENE_ASSOCIATION      = 0
   , AND_ASSOCIATION      = 1
   , OR_ASSOCIATION       = 2
   , UNKNOWN_ASSOCIATION       = 3
};


class LIBSBML_EXTERN Association : public SBase
{
protected:
  /** @cond doxygenLibsbmlInternal */
  AssociationTypeCode_t mType;
  std::string mReference;
  std::vector<Association> mAssociations;
  /** @endcond */

public:

  /**
   * Creates a new Association with the given level, version, and package version.
   */
   Association(unsigned int level      = FbcExtension::getDefaultLevel(),
          unsigned int version    = FbcExtension::getDefaultVersion(),
          unsigned int pkgVersion = FbcExtension::getDefaultPackageVersion());


   Association(const XMLNode& node, FbcPkgNamespaces* fbcns);
  
  /**
   * Creates a new Association with the given FbcPkgNamespaces object.
   */
   Association(FbcPkgNamespaces* fbcns);


  /**
   * Copy constructor.
   */
   Association(const Association& source);


  /**
   * Assignment operator.
   */
   Association& operator=(const Association& source);


  /**
   * Destructor.
   */ 
  virtual ~Association ();


  /**
   * Returns the string of the "type" attribute of this Association.
   *
   * @return the string of the "type" attribute of this Association.
   */
  virtual const AssociationTypeCode_t getType () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Association's "type" attribute has been set.
   *
   * @return @c true if this Association's "type" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetType () const;

  
  /**
   * Sets the SIdRef string of the "type" attribute of this Association.
   *
   * @param type a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setType (const AssociationTypeCode_t type);


  /**
   * Unsets the value of the "id" attribute of this Association.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetType ();

  
  /**
   * Returns the string of the "reference" attribute of this Association.
   *
   * @return the string of the "reference" attribute of this Association.
   */
  virtual const std::string& getReference () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * Association's "reference" attribute has been set.
   *
   * @return @c true if this Association's "reference" attribute has been set, 
   * otherwise @c false is returned.
   */
  virtual bool isSetReference () const;

  
  /**
   * Sets the SIdRef string of the "reference" attribute of this Association.
   *
   * @param reference a SIdRef string to be set.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setReference (const std::string& reference);


  /**
   * Unsets the value of the "id" attribute of this Association.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetReference ();

  virtual int addGene(const std::string& id);
  virtual unsigned int getNumAssociations();
  virtual int addAssociation(Association &association);
  virtual int removeAssociation(int index);
  virtual int clearAssociations();

  virtual Association* createAnd();
  virtual Association* createOr();
  virtual Association* createGene(const std::string reference = "" );

  XMLNode toXML() const;
  
  /**
   * Returns the XML element name of
   * this SBML object.
   *
   * @return the string of the name of this element.
   */
  virtual const std::string& getElementName () const ;


  /**
   * @return a (deep) copy of this Association.
   */
  virtual Association* clone () const;


  /**
   * @return the typecode (int) of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  int getTypeCode () const;


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.  For example:
   *
   *   SBase::writeElements(stream);
   *   mReactants.write(stream);
   *   mProducts.write(stream);
   *   ...
   */
  virtual void writeElements (XMLOutputStream& stream) const;
  /** @endcond */


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the SBML object's next
   * sibling object (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;
  
  
  /**
   * Parses a gene association in infix format. These look like this: 
   * 
   * (b2422) and (b2425) and (b2423) and (b2424) or (b2422) and (b2423) and (b2424) and (b2413) and (b3917)
   * 
   * @return the parsed association, or @c NULL in case of an error.
   */
  static Association* parseInfixAssociation(const std::string& association);
  
  /** 
   * Converts this association into an infix string.
   *
   *
   * @return the association as infix string.
   */
  std::string toInfix() const;
  
protected:
  /** @cond doxygenLibsbmlInternal */
  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase*
  createObject (XMLInputStream& stream);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to get the list of
   * expected attributes.
   * This function is invoked from corresponding readAttributes()
   * function.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual void readAttributes (const XMLAttributes& attributes, 
                               const ExpectedAttributes& expectedAttributes);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.  For example:
   *
   *   SBase::writeAttributes(stream);
   *   stream.writeAttribute( "id"  , mId   );
   *   stream.writeAttribute( "name", mName );
   *   ...
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;
  /** @endcond */
};


LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/*
 * C API will be added here.
 */

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* Association_H__ */
