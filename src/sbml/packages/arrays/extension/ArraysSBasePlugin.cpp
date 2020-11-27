/**
 * @file ArraysSBasePlugin.cpp
 * @brief Implementation of the ArraysSBasePlugin class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
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
#include <sbml/packages/arrays/extension/ArraysSBasePlugin.h>
#include <sbml/packages/arrays/validator/ArraysSBMLError.h>
#include <sbml/util/ElementFilter.h>
#include <sbml/Model.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new ArraysSBasePlugin using the given uri, prefix and package
 * namespace.
 */
ArraysSBasePlugin::ArraysSBasePlugin(const std::string& uri,
                                     const std::string& prefix,
                                     ArraysPkgNamespaces* arraysns)
  : SBasePlugin(uri, prefix, arraysns)
  , mIndices (arraysns)
  , mDimensions (arraysns)
{
  connectToChild();
}


/*
 * Copy constructor for ArraysSBasePlugin.
 */
ArraysSBasePlugin::ArraysSBasePlugin(const ArraysSBasePlugin& orig)
  : SBasePlugin( orig )
  , mIndices ( orig.mIndices )
  , mDimensions ( orig.mDimensions )
{
  connectToChild();
}


/*
 * Assignment operator for ArraysSBasePlugin.
 */
ArraysSBasePlugin&
ArraysSBasePlugin::operator=(const ArraysSBasePlugin& rhs)
{
  if (&rhs != this)
  {
    SBasePlugin::operator=(rhs);
    mIndices = rhs.mIndices;
    mDimensions = rhs.mDimensions;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this ArraysSBasePlugin object.
 */
ArraysSBasePlugin*
ArraysSBasePlugin::clone() const
{
  return new ArraysSBasePlugin(*this);
}


/*
 * Destructor for ArraysSBasePlugin.
 */
ArraysSBasePlugin::~ArraysSBasePlugin()
{
}


/*
 * Returns the ListOfIndices from this ArraysSBasePlugin.
 */
const ListOfIndices*
ArraysSBasePlugin::getListOfIndices() const
{
  return &mIndices;
}


/*
 * Returns the ListOfIndices from this ArraysSBasePlugin.
 */
ListOfIndices*
ArraysSBasePlugin::getListOfIndices()
{
  return &mIndices;
}


/*
 * Get an Index from the ArraysSBasePlugin.
 */
Index*
ArraysSBasePlugin::getIndex(unsigned int n)
{
  return static_cast< Index*>(mIndices.get(n));
}


/*
 * Get an Index from the ArraysSBasePlugin.
 */
const Index*
ArraysSBasePlugin::getIndex(unsigned int n) const
{
  return static_cast<const Index*>(mIndices.get(n));
}


/*
* Get a Index from the ArraysSBasePlugin based on the arrayDimension to which it
* refers.
*/
const Index*
ArraysSBasePlugin::getIndexByArrayDimension(unsigned int arrayDimension) const
{
  return mIndices.getByArrayDimension(arrayDimension);
}


/*
* Get a Index from the ArraysSBasePlugin based on the arrayDimension to which it
* refers.
*/
Index*
ArraysSBasePlugin::getIndexByArrayDimension(unsigned int arrayDimension)
{
  return mIndices.getByArrayDimension(arrayDimension);
}


/*
 * Adds a copy of the given Index to this ArraysSBasePlugin.
 */
int
ArraysSBasePlugin::addIndex(const Index* i)
{
  if (i == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (i->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != i->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != i->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != i->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    return mIndices.append(i);
  }
}


/*
 * Get the number of Index objects in this ArraysSBasePlugin.
 */
unsigned int
ArraysSBasePlugin::getNumIndices() const
{
  return mIndices.size();
}


/*
 * Creates a new Index object, adds it to this ArraysSBasePlugin object and
 * returns the Index object created.
 */
Index*
ArraysSBasePlugin::createIndex()
{
  Index* i = NULL;

  try
  {
    ARRAYS_CREATE_NS(arraysns, getSBMLNamespaces());
    i = new Index(arraysns);
    delete arraysns;
  }
  catch (...)
  {
  }

  if (i != NULL)
  {
    mIndices.appendAndOwn(i);
  }

  return i;
}


/*
 * Removes the nth Index from this ArraysSBasePlugin and returns a pointer to
 * it.
 */
Index*
ArraysSBasePlugin::removeIndex(unsigned int n)
{
  return static_cast<Index*>(mIndices.remove(n));
}


/*
 * Returns the ListOfDimensions from this ArraysSBasePlugin.
 */
const ListOfDimensions*
ArraysSBasePlugin::getListOfDimensions() const
{
  return &mDimensions;
}


/*
 * Returns the ListOfDimensions from this ArraysSBasePlugin.
 */
ListOfDimensions*
ArraysSBasePlugin::getListOfDimensions()
{
  return &mDimensions;
}


/*
 * Get a Dimension from the ArraysSBasePlugin.
 */
Dimension*
ArraysSBasePlugin::getDimension(unsigned int n)
{
  return static_cast< Dimension*>(mDimensions.get(n));
}


/*
 * Get a Dimension from the ArraysSBasePlugin.
 */
const Dimension*
ArraysSBasePlugin::getDimension(unsigned int n) const
{
  return static_cast<const Dimension*>(mDimensions.get(n));
}


/*
 * Get a Dimension from the ArraysSBasePlugin based on its identifier.
 */
Dimension*
ArraysSBasePlugin::getDimension(const std::string& sid)
{
  return static_cast< Dimension*>(mDimensions.get(sid));
}


/*
 * Get a Dimension from the ArraysSBasePlugin based on its identifier.
 */
const Dimension*
ArraysSBasePlugin::getDimension(const std::string& sid) const
{
  return static_cast<const Dimension*>(mDimensions.get(sid));
}


/*
 * Get a Dimension from the ArraysSBasePlugin based on the Size to which it
 * refers.
 */
const Dimension*
ArraysSBasePlugin::getDimensionBySize(const std::string& sid) const
{
  return mDimensions.getBySize(sid);
}


/*
 * Get a Dimension from the ArraysSBasePlugin based on the Size to which it
 * refers.
 */
Dimension*
ArraysSBasePlugin::getDimensionBySize(const std::string& sid)
{
  return mDimensions.getBySize(sid);
}


/*
* Get a Dimension from the ArraysSBasePlugin based on the arrayDimension to which it
* refers.
*/
const Dimension*
ArraysSBasePlugin::getDimensionByArrayDimension(unsigned int arrayDimension) const
{
  return mDimensions.getByArrayDimension(arrayDimension);
}


/*
* Get a Dimension from the ArraysSBasePlugin based on the arrayDimension to which it
* refers.
*/
Dimension*
ArraysSBasePlugin::getDimensionByArrayDimension(unsigned int arrayDimension)
{
  return mDimensions.getByArrayDimension(arrayDimension);
}


/*
 * Adds a copy of the given Dimension to this ArraysSBasePlugin.
 */
int
ArraysSBasePlugin::addDimension(const Dimension* d)
{
  if (d == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (d->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != d->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != d->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != d->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else if (d->isSetId() && (mDimensions.get(d->getId())) != NULL)
  {
    return LIBSBML_DUPLICATE_OBJECT_ID;
  }
  else
  {
    return mDimensions.append(d);
  }
}


/*
 * Get the number of Dimension objects in this ArraysSBasePlugin.
 */
unsigned int
ArraysSBasePlugin::getNumDimensions() const
{
  return mDimensions.size();
}


/*
 * Creates a new Dimension object, adds it to this ArraysSBasePlugin object and
 * returns the Dimension object created.
 */
Dimension*
ArraysSBasePlugin::createDimension()
{
  Dimension* d = NULL;

  try
  {
    ARRAYS_CREATE_NS(arraysns, getSBMLNamespaces());
    d = new Dimension(arraysns);
    delete arraysns;
  }
  catch (...)
  {
  }

  if (d != NULL)
  {
    mDimensions.appendAndOwn(d);
  }

  return d;
}


/*
 * Removes the nth Dimension from this ArraysSBasePlugin and returns a pointer
 * to it.
 */
Dimension*
ArraysSBasePlugin::removeDimension(unsigned int n)
{
  return static_cast<Dimension*>(mDimensions.remove(n));
}


/*
 * Removes the Dimension from this ArraysSBasePlugin based on its identifier
 * and returns a pointer to it.
 */
Dimension*
ArraysSBasePlugin::removeDimension(const std::string& sid)
{
  return static_cast<Dimension*>(mDimensions.remove(sid));
}


/*
 * Predicate returning @c true if all the required elements for this
 * ArraysSBasePlugin object have been set.
 */
bool
ArraysSBasePlugin::hasRequiredElements() const
{
  bool allPresent = true;

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
ArraysSBasePlugin::writeElements(XMLOutputStream& stream) const
{
  if (getNumIndices() > 0)
  {
    mIndices.write(stream);
  }

  if (getNumDimensions() > 0)
  {
    mDimensions.write(stream);
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
ArraysSBasePlugin::accept(SBMLVisitor& v) const
{
  const SBase* sb = static_cast<const SBase*>(this->getParentSBMLObject());
  v.visit(*sb);
  v.leave(*sb);

  mIndices.accept(v);

  mDimensions.accept(v);

  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
ArraysSBasePlugin::setSBMLDocument(SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  mIndices.setSBMLDocument(d);

  mDimensions.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
ArraysSBasePlugin::connectToChild()
{
  connectToParent(getParentSBMLObject());
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to parent element
 */
void
ArraysSBasePlugin::connectToParent(SBase* base)
{
  SBasePlugin::connectToParent(base);

  mIndices.connectToParent(base);

  mDimensions.connectToParent(base);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
ArraysSBasePlugin::enablePackageInternal(const std::string& pkgURI,
                                         const std::string& pkgPrefix,
                                         bool flag)
{
  if (mIndices.size() > 0)
  {
    mIndices.enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
  if (mDimensions.size() > 0)
  {
    mDimensions.enablePackageInternal(pkgURI, pkgPrefix, flag);
  }
}

/** @endcond */

std::vector<unsigned int> 
ArraysSBasePlugin::getNumArrayElements() const
{
  std::vector<unsigned int> arraySize;
  const Model * model = (const Model*)(getParentSBMLObject()->getAncestorOfType(SBML_MODEL));
  if (model == NULL)
  {
    return arraySize;
  }

  for (unsigned int i = getNumDimensions(); i > 0; i--)
  {
    unsigned int thisDim = getNumElementsInDimension(i-1);
    arraySize.push_back(thisDim);
  }

  return arraySize;

}



unsigned int 
ArraysSBasePlugin::getNumElementsInDimension(unsigned int arrayDimension) const
{
  const Dimension* dim = getDimensionByArrayDimension(arrayDimension);
  const Model * model = (const Model*)(getParentSBMLObject()->getAncestorOfType(SBML_MODEL));
  unsigned int num = 0;
  if (dim == NULL || model == NULL)
  {
    return num;
  }

  if (dim->isSetSize())
  {
    const Parameter* p = model->getParameter(dim->getSize());
    if (p != NULL && p->isSetValue())
    {
      num = (unsigned int)(p->getValue());
    }
  }

  return num;
}

unsigned int
ArraysSBasePlugin::getNumImpliedDimensions() const
{
  unsigned int num = 0;
  if (getNumDimensions() == 0)
  {
    const SBase * parent = getParent();
    if (parent != NULL)
    {
      const ArraysSBasePlugin* plugin = 
        static_cast<const ArraysSBasePlugin*>(parent->getPlugin("arrays"));

      if (plugin != NULL)
      {
        return plugin->getNumDimensions();
      }

    }
    return num;
  }
  else
  {
    return getNumDimensions();
  }
}

SBase*
ArraysSBasePlugin::getParent() const
{
  if (this->getParentSBMLObject()->getTypeCode() == SBML_ARRAYS_DIMENSION)
  {
    return NULL;
  }
  SBase* parent = const_cast<SBase*>(this->getParentSBMLObject())->getParentSBMLObject();
  if (parent->getTypeCode() == SBML_LIST_OF)
  {
    return parent->getParentSBMLObject();
  }
  else
  {
    return parent;
  }

}

/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this ArraysSBasePlugin.
 */
int
ArraysSBasePlugin::getAttribute(const std::string& attributeName,
                                bool& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this ArraysSBasePlugin.
 */
int
ArraysSBasePlugin::getAttribute(const std::string& attributeName,
                                int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this ArraysSBasePlugin.
 */
int
ArraysSBasePlugin::getAttribute(const std::string& attributeName,
                                double& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this ArraysSBasePlugin.
 */
int
ArraysSBasePlugin::getAttribute(const std::string& attributeName,
                                unsigned int& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this ArraysSBasePlugin.
 */
int
ArraysSBasePlugin::getAttribute(const std::string& attributeName,
                                std::string& value) const
{
  int return_value = SBasePlugin::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this ArraysSBasePlugin's attribute
 * "attributeName" is set.
 */
bool
ArraysSBasePlugin::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBasePlugin::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ArraysSBasePlugin.
 */
int
ArraysSBasePlugin::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ArraysSBasePlugin.
 */
int
ArraysSBasePlugin::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ArraysSBasePlugin.
 */
int
ArraysSBasePlugin::setAttribute(const std::string& attributeName,
                                double value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ArraysSBasePlugin.
 */
int
ArraysSBasePlugin::setAttribute(const std::string& attributeName,
                                unsigned int value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this ArraysSBasePlugin.
 */
int
ArraysSBasePlugin::setAttribute(const std::string& attributeName,
                                const std::string& value)
{
  int return_value = SBasePlugin::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this ArraysSBasePlugin.
 */
int
ArraysSBasePlugin::unsetAttribute(const std::string& attributeName)
{
  int value = SBasePlugin::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this ArraysSBasePlugin.
 */
SBase*
ArraysSBasePlugin::createChildObject(const std::string& elementName)
{
  SBase* obj = NULL;

  if (elementName == "index")
  {
    return createIndex();
  }
  else if (elementName == "dimension")
  {
    return createDimension();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this ArraysSBasePlugin.
 */
unsigned int
ArraysSBasePlugin::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "index")
  {
    return getNumIndices();
  }
  else if (elementName == "dimension")
  {
    return getNumDimensions();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this ArraysSBasePlugin.
 */
SBase*
ArraysSBasePlugin::getObject(const std::string& elementName,
                             unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "index")
  {
    return getIndex(index);
  }
  else if (elementName == "dimension")
  {
    return getDimension(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
ArraysSBasePlugin::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mIndices.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mDimensions.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
ArraysSBasePlugin::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mIndices.getMetaId() == metaid)
  {
    return &mIndices;
  }

  if (mDimensions.getMetaId() == metaid)
  {
    return &mDimensions;
  }

  obj = mIndices.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  obj = mDimensions.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
List*
ArraysSBasePlugin::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_LIST(ret, sublist, mIndices, filter);
  ADD_FILTERED_LIST(ret, sublist, mDimensions, filter);

  return ret;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Append items from model (used in comp flattening)
 */
int
ArraysSBasePlugin::appendFrom(const Model* model)
{
  int ret = LIBSBML_OPERATION_SUCCESS;

  if (model == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  const ArraysSBasePlugin* plug = static_cast<const
    ArraysSBasePlugin*>(model->getPlugin(getPrefix()));

  if (plug == NULL)
  {
    return ret;
  }

  Model* parent = static_cast<Model*>(getParentSBMLObject());

  if (parent == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }

  ret = mIndices.appendFrom(plug->getListOfIndices());

  if (ret != LIBSBML_OPERATION_SUCCESS)
  {
    return ret;
  }

  ret = mDimensions.appendFrom(plug->getListOfDimensions());

  if (ret != LIBSBML_OPERATION_SUCCESS)
  {
    return ret;
  }

  return ret;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
ArraysSBasePlugin::createObject(XMLInputStream& stream)
{
  SBase* obj = NULL;

  const std::string& name = stream.peek().getName();
  const XMLNamespaces& xmlns = stream.peek().getNamespaces();
  const std::string& prefix = stream.peek().getPrefix();

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ?
    xmlns.getPrefix(mURI) : mPrefix;

  if (prefix == targetPrefix)
  {
    if (name == "listOfIndices")
    {
      if (mIndices.size() != 0)
      {
        getErrorLog()->logPackageError("arrays", ArraysSBaseAllowedElements,
          getPackageVersion(), getLevel(), getVersion());
      }

      obj = &mIndices;
      if (targetPrefix.empty())
      {
        mIndices.getSBMLDocument()->enableDefaultNS(mURI, true);
      }
    }
    else if (name == "listOfDimensions")
    {
      if (mDimensions.size() != 0)
      {
        getErrorLog()->logPackageError("arrays", ArraysSBaseAllowedElements,
          getPackageVersion(), getLevel(), getVersion());
      }

      obj = &mDimensions;
      if (targetPrefix.empty())
      {
        mDimensions.getSBMLDocument()->enableDefaultNS(mURI, true);
      }
    }
  }

  connectToChild();

  return obj;
}

/** @endcond */




#endif /* __cplusplus */




LIBSBML_CPP_NAMESPACE_END


