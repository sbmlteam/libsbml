/*
 * SubListOfSpeciesFeatures.h
 *
 *  Created on: Apr 1, 2016
 *      Author: Fengkai Zhang
 */

#ifndef SubListOfSpeciesFeatures_H__
#define SubListOfSpeciesFeatures_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/multi/common/multifwd.h>

typedef enum
{
    MULTI_RELATION_AND
  , MULTI_RELATION_OR
  , MULTI_RELATION_NOT
  , MULTI_RELATION_UNKNOWN
} Relation_t;



#ifdef __cplusplus

#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>

#include <sbml/packages/multi/extension/MultiExtension.h>
#include <sbml/packages/multi/sbml/SpeciesFeature.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN SubListOfSpeciesFeatures : public ListOf
{

protected:

////  std::string   mId;
  Relation_t mRelation;
  std::string   mComponent;

public:

  /**
   * Creates a new SubListOfSpeciesFeatures with the given level, version, and package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this SubListOfSpeciesFeatures
   *
   * @param version an unsigned int, the SBML Version to assign to this SubListOfSpeciesFeatures
   *
   * @param pkgVersion an unsigned int, the SBML Multi Version to assign to this SubListOfSpeciesFeatures
   */
  SubListOfSpeciesFeatures(unsigned int level      = MultiExtension::getDefaultLevel(),
                        unsigned int version    = MultiExtension::getDefaultVersion(),
                        unsigned int pkgVersion = MultiExtension::getDefaultPackageVersion());


  /**
   * Creates a new SubListOfSpeciesFeatures with the given MultiPkgNamespaces object.
   *
   * @param multins the MultiPkgNamespaces object
   */
  SubListOfSpeciesFeatures(MultiPkgNamespaces* multins);


  /**
   * Creates and return a copy of SubListOfSpeciesFeatures.
   *
   * @param subListOfSpeciesFeatures this SubListOfSpeciesFeatures object
   */
  SubListOfSpeciesFeatures(const SubListOfSpeciesFeatures & multins);


   /**
   * Creates and returns a deep copy of this SubListOfSpeciesFeatures object.
   *
   * @return a (deep) copy of this SubListOfSpeciesFeatures object.
   */
  virtual SubListOfSpeciesFeatures* clone () const;


   /**
   * Destroys this SubListOfSpeciesFeatures object.
   */
  virtual ~SubListOfSpeciesFeatures();

  /**
  * Returns the value of the "id" attribute of this SubListOfSpeciesFeatures.
  *
  * @return the value of the "id" attribute of this SubListOfSpeciesFeatures as a string.
  */
 virtual const std::string& getId() const;


 /**
  * Predicate returning @c true or @c false depending on whether this
  * SubListOfSpeciesFeatures's "id" attribute has been set.
  *
  * @return @c true if this SubListOfSpeciesFeatures's "id" attribute has been set,
  * otherwise @c false is returned.
  */
 virtual bool isSetId() const;


 /**
  * Sets the value of the "id" attribute of this SubListOfSpeciesFeatures.
  *
  * @param id; const std::string& value of the "id" attribute to be set
  *
  * @return integer value indicating success/failure of the
  * function.  @if clike The value is drawn from the
  * enumeration #OperationReturnValues_t. @endif The possible values
  * returned by this function are:
  * @li LIBSBML_OPERATION_SUCCESS
  * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
  */
 virtual int setId(const std::string& id);


 /**
  * Unsets the value of the "id" attribute of this SubListOfSpeciesFeatures.
  *
  * @return integer value indicating success/failure of the
  * function.  @if clike The value is drawn from the
  * enumeration #OperationReturnValues_t. @endif The possible values
  * returned by this function are:
  * @li LIBSBML_OPERATION_SUCCESS
  * @li LIBSBML_OPERATION_FAILED
  */
 virtual int unsetId();


 /**
  * Returns the value of the "name" attribute of this SubListOfSpeciesFeatures.
  *
  * @return the value of the "name" attribute of this SubListOfSpeciesFeatures as a string.
  */
 virtual const std::string& getName() const;


 /**
  * Predicate returning @c true or @c false depending on whether this
  * SubListOfSpeciesFeatures's "name" attribute has been set.
  *
  * @return @c true if this SubListOfSpeciesFeatures' "name" attribute has been set,
  * otherwise @c false is returned.
  */
 virtual bool isSetName() const;


 /**
  * Sets the value of the "name" attribute of this SubListOfSpeciesFeatures.
  *
  * @param name; const std::string& value of the "name" attribute to be set
  *
  * @return integer value indicating success/failure of the
  * function.  @if clike The value is drawn from the
  * enumeration #OperationReturnValues_t. @endif The possible values
  * returned by this function are:
  * @li LIBSBML_OPERATION_SUCCESS
  * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
  */
 virtual int setName(const std::string& name);


 /**
  * Unsets the value of the "name" attribute of this SubListOfSpeciesFeatures.
  *
  * @return integer value indicating success/failure of the
  * function.  @if clike The value is drawn from the
  * enumeration #OperationReturnValues_t. @endif The possible values
  * returned by this function are:
  * @li LIBSBML_OPERATION_SUCCESS
  * @li LIBSBML_OPERATION_FAILED
  */
 virtual int unsetName();


/**
  * Returns the value of the "component" attribute of this SubListOfSpeciesFeatures.
  *
  * @return the value of the "component" attribute of this SubListOfSpeciesFeatures as a string.
  */
 virtual const std::string& getComponent() const;


 /**
  * Predicate returning @c true or @c false depending on whether this
  * SubListOfSpeciesFeatures's "component" attribute has been set.
  *
  * @return @c true if this SubListOfSpeciesFeatures's "component" attribute has been set,
  * otherwise @c false is returned.
  */
 virtual bool isSetComponent() const;


 /**
  * Sets the value of the "component" attribute of this SubListOfSpeciesFeatures.
  *
  * @param component; const std::string& value of the "component" attribute to be set
  *
  * @return integer value indicating success/failure of the
  * function.  @if clike The value is drawn from the
  * enumeration #OperationReturnValues_t. @endif The possible values
  * returned by this function are:
  * @li LIBSBML_OPERATION_SUCCESS
  * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
  */
 virtual int setComponent(const std::string& component);


 /**
  * Unsets the value of the "component" attribute of this SubListOfSpeciesFeatures.
  *
  * @return integer value indicating success/failure of the
  * function.  @if clike The value is drawn from the
  * enumeration #OperationReturnValues_t. @endif The possible values
  * returned by this function are:
  * @li LIBSBML_OPERATION_SUCCESS
  * @li LIBSBML_OPERATION_FAILED
  */
 virtual int unsetComponent();

 /**
  * Creates a new SpeciesFeature object and adds it to this SubListOfSpeciesFeatures object.
  *
  * @return the newly created SpeciesFeature object.
  */
 SpeciesFeature* createSpeciesFeature ();


   /**
   * Get a SpeciesFeature from the SubListOfSpeciesFeatures.
   *
   * @param n the index number of the SpeciesFeature to get.
   *
   * @return the nth SpeciesFeature in this SubListOfSpeciesFeatures.
   *
   * @see size()
   */
  virtual SpeciesFeature* get(unsigned int n);


  /**
   * Get a SpeciesFeature from the SubListOfSpeciesFeatures.
   *
   * @param n the index number of the SpeciesFeature to get.
   *
   * @return the nth SpeciesFeature in this SubListOfSpeciesFeatures.
   *
   * @see size()
   */
  virtual const SpeciesFeature* get(unsigned int n) const;


  /**
   * Get a SpeciesFeature from the SubListOfSpeciesFeatures
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SpeciesFeature to get.
   *
   * @return SpeciesFeature in this SubListOfSpeciesFeatures
   * with the given id or NULL if no such
   * SpeciesFeature exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual SpeciesFeature* get(const std::string& sid);


  /**
   * Get a SpeciesFeature from the SubListOfSpeciesFeatures
   * based on its identifier.
   *
   * @param sid a string representing the identifier
   * of the SpeciesFeature to get.
   *
   * @return SpeciesFeature in this SubListOfSpeciesFeatures
   * with the given id or NULL if no such
   * SpeciesFeature exists.
   *
   * @see get(unsigned int n)   *
   * @see size()
   */
  virtual const SpeciesFeature* get(const std::string& sid) const;


  /**
   * Removes the nth SpeciesFeature from this SubListOfSpeciesFeatures
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the SpeciesFeature to remove.
   *
   * @see size()
   */
  virtual SpeciesFeature* remove(unsigned int n);


  /**
   * Removes the SpeciesFeature from this SubListOfSpeciesFeatures with the given identifier
   * and returns a pointer to it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then
   * @c NULL is returned.
   *
   * @param sid the identifier of the SpeciesFeature to remove.
   *
   * @return the SpeciesFeature removed. As mentioned above, the caller owns the
   * returned item.
   */
  virtual SpeciesFeature* remove(const std::string& sid);


   /**
   * Returns the value of the "relation" attribute of this SubListOfSpeciesFeatures.
   *
   * @return the value of the "relation" attribute of this SubListOfSpeciesFeatures as a FIX ME.
   */
  Relation_t getRelation() const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * SubListOfSpeciesFeatures's "relation" attribute has been set.
   *
   * @return @c true if this SubListOfSpeciesFeatures's "relation" attribute has been set,
   * otherwise @c false is returned.
   */
  virtual bool isSetRelation() const;


  /**
   * Sets the value of the "relation" attribute of this SubListOfSpeciesFeatures.
   *
   * @param relation; FIX ME value of the "relation" attribute to be set
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
   */
  virtual int setRelation(Relation_t relation);


  /**
   * Unsets the value of the "relation" attribute of this SubListOfSpeciesFeatures.
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li LIBSBML_OPERATION_SUCCESS
   * @li LIBSBML_OPERATION_FAILED
   */
  virtual int unsetRelation();


  /**
   * Returns the XML element name of this object, which for SubListOfSpeciesFeatures, is
   * always @c "subListOfSpeciesFeatures".
   *
   * @return the name of this element, i.e. @c "subListOfSpeciesFeatures".
   */
  virtual const std::string& getElementName () const;


  /**
   * Returns the libSBML type code for this SBML object.
   *
   * @if clike LibSBML attaches an identifying code to every kind of SBML
   * object.  These are known as <em>SBML type codes</em>.  The set of
   * possible type codes is defined in the enumeration #SBMLTypeCode_t.
   * The names of the type codes all begin with the characters @c
   * SBML_. @endif@if java LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.  In
   * other languages, the set of type codes is stored in an enumeration; in
   * the Java language interface for libSBML, the type codes are defined as
   * static integer constants in the interface class {@link
   * libsbmlConstants}.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if python LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the Python language interface for libSBML, the type
   * codes are defined as static integer constants in the interface class
   * @link libsbml@endlink.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if csharp LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the C# language interface for libSBML, the type codes
   * are defined as static integer constants in the interface class @link
   * libsbmlcs.libsbml@endlink.  The names of the type codes all begin with
   * the characters @c SBML_. @endif
   *
   * @return the SBML type code for this object, or
   * @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual int getTypeCode () const;


  /**
   * Returns the libSBML type code for the SBML objects
   * contained in this ListOf object
   *
   * @if clike LibSBML attaches an identifying code to every kind of SBML
   * object.  These are known as <em>SBML type codes</em>.  The set of
   * possible type codes is defined in the enumeration #SBMLTypeCode_t.
   * The names of the type codes all begin with the characters @c
   * SBML_. @endif@if java LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.  In
   * other languages, the set of type codes is stored in an enumeration; in
   * the Java language interface for libSBML, the type codes are defined as
   * static integer constants in the interface class {@link
   * libsbmlConstants}.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if python LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the Python language interface for libSBML, the type
   * codes are defined as static integer constants in the interface class
   * @link libsbml@endlink.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if csharp LibSBML attaches an identifying
   * code to every kind of SBML object.  These are known as <em>SBML type
   * codes</em>.  In the C# language interface for libSBML, the type codes
   * are defined as static integer constants in the interface class @link
   * libsbmlcs.libsbml@endlink.  The names of the type codes all begin with
   * the characters @c SBML_. @endif
   *
   * @return the SBML type code for the objects in this ListOf instance, or
   * @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual int getItemTypeCode () const;

  /** @cond doxygenLibsbmlInternal */

  /**
   * Connects to child elements.
   */
  virtual void connectToChild ();

  /** @endcond doxygenLibsbmlInternal */


  unsigned int getNumSpeciesFeatures() const;


  /** @cond doxygenLibsbmlInternal */
  /**
   */
  /**
   * Accepts the given SBMLVisitor.
   */
  virtual bool accept (SBMLVisitor& v) const;

  /** @endcond */


protected:

  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new SpeciesFeature in this ListOfSpeciesFeatures
   */
  virtual SBase* createObject(XMLInputStream& stream);


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

  /**
   * Write the namespace for the Multi package.
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;


  /** @endcond doxygenLibsbmlInternal */

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
   * Reads the attributes of corresponding package in SBMLDocument element.
   */
  virtual void readAttributes (const XMLAttributes& attributes,
                               const ExpectedAttributes& expectedAttributes);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Writes the attributes of corresponding package in SBMLDocument element.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;

  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /**
   */
  virtual void writeElements (XMLOutputStream& stream) const;

  /** @endcond */

};

LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_isValidRelation(Relation_t relation);


LIBSBML_EXTERN
int
SubListOfSpeciesFeatures_isValidRelationString(const char* s);


LIBSBML_EXTERN
const char*
Relation_toString(Relation_t relation);


LIBSBML_EXTERN
Relation_t
Relation_fromString(const char* s);

LIBSBML_CPP_NAMESPACE_END

#endif  /*  __cplusplus  */

#endif /* SubListOfSpeciesFeatures_H__ */
