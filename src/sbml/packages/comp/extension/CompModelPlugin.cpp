/**
 * @file    CompModelPlugin.cpp
 * @brief   Implementation of CompModelPlugin, the plugin class of
 *          comp package for the SBase element.
 * @author  Lucian Smith
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2011 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */

#include <ostream>
#include <iostream>
#include <vector>

#include <sbml/common/libsbml-version.h>
#include <sbml/packages/comp/common/compfwd.h>
#include <sbml/packages/comp/extension/CompModelPlugin.h>
#include <sbml/packages/comp/extension/CompExtension.h>
#include <sbml/packages/comp/validator/CompSBMLError.h>
#include <sbml/Model.h>

#include <sbml/util/ElementFilter.h>
#include <sbml/util/IdentifierTransformer.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

CompModelPlugin::CompModelPlugin (const std::string &uri, const std::string &prefix, CompPkgNamespaces *compns)
  : CompSBasePlugin(uri,prefix, compns)
  , mListOfSubmodels(compns)
  , mListOfPorts(compns)
  , mDivider("__")
{
  connectToChild();
}


CompModelPlugin::CompModelPlugin(const CompModelPlugin& orig)
  : CompSBasePlugin(orig)
  , mListOfSubmodels(orig.mListOfSubmodels)
  , mListOfPorts(orig.mListOfPorts)
  , mDivider("__")
{
  connectToChild();
}


CompModelPlugin::~CompModelPlugin () 
{
}

CompModelPlugin& 
CompModelPlugin::operator=(const CompModelPlugin& orig)
{
  if(&orig!=this)
  {
    CompSBasePlugin::operator =(orig);
    mListOfSubmodels = orig.mListOfSubmodels;
    mListOfPorts = orig.mListOfPorts;
    mDivider = orig.mDivider;
    connectToChild();
  }
  return *this;
}


CompModelPlugin* 
CompModelPlugin::clone () const
{
  return new CompModelPlugin(*this);  
}


/** @cond doxygen-libsbml-internal */
SBase*
CompModelPlugin::createObject(XMLInputStream& stream)
{
  SBase*        object = 0;

  const std::string&   name   = stream.peek().getName();
  const XMLNamespaces& xmlns  = stream.peek().getNamespaces();
  const std::string&   prefix = stream.peek().getPrefix();

  const std::string& targetPrefix = (xmlns.hasURI(mURI)) ? xmlns.getPrefix(mURI) : mPrefix;
  
  if (prefix == targetPrefix)
  {
    if ( name == "listOfSubmodels" ) 
    {
      if (mListOfSubmodels.size() != 0)
      {
        getErrorLog()->logPackageError("comp", CompOneListOfOnModel, 
          getPackageVersion(), getLevel(), getVersion());
      }

      object = &mListOfSubmodels;
    
      if (targetPrefix.empty())
      {
        //
        // (NOTE)
        //
        // A top-level element (listOfSubmodels) of the comp extension is located 
        // in a default namespace, and thus xmlns=".." attribute must be added to 
        // the element.
        // This is done by invoking SBMLDocument::enableDefaultNS() function with 
        // the two arguments (the uri of this package and true value).
        //
        mListOfSubmodels.getSBMLDocument()->enableDefaultNS(mURI,true);
      }
    }
    if ( name == "listOfPorts" ) 
    {
      if (mListOfPorts.size() != 0)
      {
        getErrorLog()->logPackageError("comp", CompOneListOfOnModel, 
          getPackageVersion(), getLevel(), getVersion());
      }

      object = &mListOfPorts;
    
      if (targetPrefix.empty())
      {
        mListOfPorts.getSBMLDocument()->enableDefaultNS(mURI,true);
      }
    }
  }    

  return object;
}
/** @endcond */

/** @cond doxygen-libsbml-internal */
void
CompModelPlugin::writeElements (XMLOutputStream& stream) const
{
  if (getNumSubmodels() > 0)
  {
    mListOfSubmodels.write(stream);
  }
  if (getNumPorts() > 0)
  {
    mListOfPorts.write(stream);
  }    
}
/** @endcond */

SBase* 
CompModelPlugin::getElementBySId(std::string id)
{
  if (id.empty()) return NULL;
  SBase* obj = mListOfSubmodels.getElementBySId(id);
  if (obj != NULL) return obj;
  obj = mListOfPorts.getElementBySId(id);
  if (obj != NULL) return obj;
  return NULL;
}


SBase*
CompModelPlugin::getElementByMetaId(std::string metaid)
{
  if (metaid.empty()) return NULL;
  if (mListOfSubmodels.getMetaId() == metaid) return &mListOfSubmodels;
  if (mListOfPorts.getMetaId() == metaid) return &mListOfPorts;

  SBase* obj = mListOfSubmodels.getElementByMetaId(metaid);
  if (obj != NULL) return obj;
  obj = mListOfPorts.getElementByMetaId(metaid);
  if (obj != NULL) return obj;
  return NULL;
}

List*
  CompModelPlugin::getAllElements(ElementFilter *filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mListOfSubmodels, filter);
  ADD_FILTERED_LIST(ret, sublist, mListOfPorts, filter);

  return ret;
}


const ListOfSubmodels*
CompModelPlugin::getListOfSubmodels () const
{
  return &mListOfSubmodels;
}


Submodel*
CompModelPlugin::removeSubmodel(unsigned int index)
{
  return mListOfSubmodels.remove(index);
}


Submodel* 
CompModelPlugin::getSubmodel (unsigned int index)
{
  return mListOfSubmodels.get(index);
}

const Submodel* 
CompModelPlugin::getSubmodel (unsigned int index) const
{
  return mListOfSubmodels.get(index);
}


Submodel* 
CompModelPlugin::getSubmodel (std::string id)
{
  return mListOfSubmodels.get(id);
}

const Submodel* 
CompModelPlugin::getSubmodel (std::string id) const
{
  return mListOfSubmodels.get(id);
}


int
CompModelPlugin::addSubmodel (const Submodel* submodel)
{
  if (submodel == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (!(submodel->hasRequiredAttributes()) || !(submodel->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != submodel->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != submodel->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != submodel->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    return mListOfSubmodels.append(submodel);
  }
}


unsigned int
CompModelPlugin::getNumSubmodels () const
{
  return mListOfSubmodels.size();
}


/*
 * Creates a Submodel object, adds it to the end of the submodel
 * objects list and returns a reference to the newly created object.
 */
Submodel*
CompModelPlugin::createSubmodel ()
{
  COMP_CREATE_NS(compns, getSBMLNamespaces());
  Submodel* m = new Submodel(compns);
  mListOfSubmodels.appendAndOwn(m);
  return m;
}


/*
 * Returns the listofports object that holds all ports.
 */ 
const ListOfPorts*
CompModelPlugin::getListOfPorts () const
{
  return &mListOfPorts;
}


/*
 * Remove the portRef with the given index.
 * A pointer to the removed port is returned.
 * If no portRef has been removed, @c NULL is returned.
 */
Port*
CompModelPlugin::removePort(unsigned int index)
{
  return mListOfPorts.remove(index);
}


int 
CompModelPlugin::setDivider(const std::string& divider)
{
  if (divider.empty()) return LIBSBML_OPERATION_FAILED;
  if (!SyntaxChecker::isValidSBMLSId("a" + divider + "a")) return LIBSBML_OPERATION_FAILED;
  mDivider = divider;
  return LIBSBML_OPERATION_SUCCESS;
}


string 
CompModelPlugin::getDivider()
{
  return mDivider;
}

/*
 * Returns the port with the given index.
 * If the index is invalid, @c NULL is returned.
 */ 
Port* 
CompModelPlugin::getPort (unsigned int index)
{
  return mListOfPorts.get(index);
}

/*
 * Returns the port with the given index.
 * If the index is invalid, @c NULL is returned.
 */ 
const Port* 
CompModelPlugin::getPort (unsigned int index) const
{
  return mListOfPorts.get(index);
}

/*
 * Returns the port with the given @p id.
 * If the id is invalid, @c NULL is returned.
 */ 
Port* 
CompModelPlugin::getPort (std::string id)
{
  return mListOfPorts.get(id);
}

/*
 * Returns the port with the given @p id.
 * If the id is invalid, @c NULL is returned.
 */ 
const Port* 
CompModelPlugin::getPort (std::string id) const
{
  return mListOfPorts.get(id);
}


int
CompModelPlugin::addPort (const Port* port)
{
  if (port == NULL)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (!(port->hasRequiredAttributes()) || !(port->hasRequiredElements()))
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != port->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != port->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (getPackageVersion() != port->getPackageVersion())
  {
    return LIBSBML_PKG_VERSION_MISMATCH;
  }
  else
  {
    return mListOfPorts.append(port);
  }
}


unsigned int
CompModelPlugin::getNumPorts () const
{
  return mListOfPorts.size();
}


/*
 * Creates a Port object, adds it to the end of the portRef
 * objects list and returns a reference to the newly created object.
 */
Port*
CompModelPlugin::createPort ()
{
  COMP_CREATE_NS(compns, getSBMLNamespaces());
  Port* m = new Port(compns);
  mListOfPorts.appendAndOwn(m);
  return m;
}


/** @cond doxygen-libsbml-internal */
/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void 
CompModelPlugin::setSBMLDocument (SBMLDocument* d)
{
  SBasePlugin::setSBMLDocument(d);

  mListOfSubmodels.setSBMLDocument(d);  
  mListOfPorts.setSBMLDocument(d);  
}
/** @endcond */


/** @cond doxygen-libsbml-internal */
void
CompModelPlugin::connectToChild()
{
  CompSBasePlugin::connectToChild();
  connectToParent(getParentSBMLObject());
}
/** @endcond */


/** @cond doxygen-libsbml-internal */
void
CompModelPlugin::connectToParent (SBase* sbase)
{
  CompSBasePlugin::connectToParent(sbase);
  mListOfSubmodels.connectToParent(sbase);
  mListOfPorts.connectToParent(sbase);
}
/** @endcond */


/** @cond doxygen-libsbml-internal */
/*
 * Enables/Disables the given package with child elements in this plugin
 * object (if any).
 */
void
CompModelPlugin::enablePackageInternal(const std::string& pkgURI,
                                       const std::string& pkgPrefix, bool flag)
{
  mListOfSubmodels.enablePackageInternal(pkgURI,pkgPrefix,flag);
  mListOfPorts.enablePackageInternal(pkgURI,pkgPrefix,flag);
}
/** @endcond */


Model* CompModelPlugin::flattenModel() const
{
  //First make a copy of our parent (the model to be flattened):
  const Model* parent = static_cast<const Model*>(getParentSBMLObject());
  
  if (parent==NULL) {
    return NULL;
  }
  //doc needs to be non-const so that the error messages can be updated.  Otherwise, nothing changes.
  SBMLDocument* doc = const_cast<SBMLDocument*>(getSBMLDocument());
  if (doc==NULL) {
    return NULL;
  }

  //Set the original document so that it can find the model definitions 
  //and external model definitions while we flatten.
  Model* flat = parent->clone();
  flat->setSBMLDocument(doc);
  CompModelPlugin* flatplug = 
    static_cast<CompModelPlugin*>(flat->getPlugin(getPrefix()));

  // Now instantiate its submodels and 
  // follow all renaming/deletion/replacement rules.
  vector<const Model*> submods;
  int success = flatplug->instantiateSubmodels();

  if (success != LIBSBML_OPERATION_SUCCESS) {
    //instantiateSubmodels sets its own error messages.
    delete flat;
    return NULL;
  }

  //Now start the aggregation process.  
  //This goes from the bottom up, calling 'appendFrom' iteratively 
  //(from the plugin).
  for (unsigned int sm=0; sm<flatplug->getNumSubmodels(); sm++) 
  {
    Model* submodel = flatplug->getSubmodel(sm)->getInstantiation();
    if (submodel==NULL) {
      //getInstantiation should be calling a cached value by now, but if not, it will set its own error messages.
      delete flat;
      return NULL;
    }
    CompModelPlugin* submodplug = 
      static_cast<CompModelPlugin*>(submodel->getPlugin(getPrefix()));

    if (submodplug != NULL) {
      //Strip the ports from the submodel, as we no longer need them.
      while (submodplug->getNumPorts() > 0) 
      {
        submodplug->removePort(0);
      }
    }
    success = flat->appendFrom(submodel);
    if (success != LIBSBML_OPERATION_SUCCESS) {
      string error = "The submodel '" + submodel->getId() + "' could not be used to create the flattened model:  appending the elements of the submodel to the elements of the parent model failed.";
      doc->getErrorLog()->logPackageError("comp", CompFlatModelNotValid, 1, 3, 1, error);
      delete flat;
      return NULL;
    }
  }

  // Now we clear the saved referenced elements in the local Port objects, 
  // but point them to the new object if necessary.
  flatplug->resetPorts();

  // Next, strip the package info from 'flat'.  
  // We're going to remove everything but the Ports:
  flatplug->mListOfSubmodels.clear();
  flatplug->clearReplacedElements();
  flatplug->unsetReplacedBy();
  
  List* allelements = flat->getAllElements();
  
  vector<SBase*> nonReplacedElements;
  
  for (unsigned int el=0; el<allelements->getSize(); el++) 
  {
    SBase* element = static_cast<SBase*>(allelements->get(el));
    int type = element->getTypeCode();
    if (!(type==SBML_COMP_REPLACEDBY ||
          type==SBML_COMP_REPLACEDELEMENT ||
          type==SBML_COMP_SBASEREF)) 
    {
            nonReplacedElements.push_back(element);
    }
  }

  for (unsigned int el=0; el<nonReplacedElements.size(); el++) 
  {
    SBase* element = nonReplacedElements[el];
    CompSBasePlugin* elplug = 
      static_cast<CompSBasePlugin*>(element->getPlugin(getPrefix()));
    if (elplug != NULL) 
    {
      elplug->clearReplacedElements();
      elplug->unsetReplacedBy();
    }
  }

  //Finally, unset the document again.
  flat->setSBMLDocument(NULL);

  return flat;
}

/** @cond doxygen-libsbml-internal */
void CompModelPlugin::resetPorts()
{
  for (unsigned int p=0; p<getNumPorts(); p++) {
    Port* port = getPort(p);
    SBase* referenced = port->getReferencedElement();
    if (port->isSetSBaseRef()) {
      port->unsetSBaseRef();
      port->unsetIdRef();
      port->unsetMetaIdRef();
      port->unsetUnitRef();
      int type = referenced->getTypeCode();
      if (referenced->isSetId() && 
          type != SBML_INITIAL_ASSIGNMENT &&
          type != SBML_ASSIGNMENT_RULE &&
          type != SBML_RATE_RULE &&
          type != SBML_EVENT_ASSIGNMENT) {
        if (type==SBML_UNIT_DEFINITION) {
          port->setUnitRef(referenced->getId());
        }
        else {
          port->setIdRef(referenced->getId());
        }
      }
      else if (referenced->isSetMetaId()) {
        port->setMetaIdRef(referenced->getMetaId());
      }
      else {
        stringstream newname;
        newname << "auto_port_" << p;
        referenced->setMetaId(newname.str());
        port->setMetaIdRef(newname.str());
      }
    }
    port->clearReferencedElement();
  }
}
/** @endcond */


int 
CompModelPlugin::appendFrom(const Model* model)
{
  if (model==NULL) 
    return LIBSBML_INVALID_OBJECT;

  const CompModelPlugin* modplug = 
    static_cast<const CompModelPlugin*>(model->getPlugin(getPrefix()));
  
  if (modplug==NULL) 
    return LIBSBML_INVALID_OBJECT;

  Model* parent = static_cast<Model*>(getParentSBMLObject());

  if (parent==NULL) 
    return LIBSBML_INVALID_OBJECT;
  
  for (unsigned int sm = 0; sm<modplug->getNumSubmodels(); sm++) 
  {
    const Submodel* sub = modplug->getSubmodel(sm);
    int ret = parent->appendFrom(sub->getInstantiation());
    
    if (ret != LIBSBML_OPERATION_SUCCESS) 
      return ret;
  }
  return LIBSBML_OPERATION_SUCCESS;
}

/*
 * Loop through all Submodels in this Model, instantiate all of them, 
 * perform all deletions, and synchronize all replacements.  
 * The resulting models are stored in the Submodel objects, 
 * and available from 'Submodel::getInstantiation()'
 */
int 
CompModelPlugin::instantiateSubmodels()
{
  Model* model = static_cast<Model*>(getParentSBMLObject());
  
  if (model==NULL) 
    return LIBSBML_INVALID_OBJECT;
  
  int ret;

  // First we instantiate all the submodels.  
  // This acts recursively downward through the stack.
  for (unsigned int sub=0; sub<mListOfSubmodels.size(); sub++) 
  {
    Submodel* submodel = mListOfSubmodels.get(sub);
    // Instead of 'instantiate', since we might have already 
    // been instantiated ourselves from above.
    const Model* submodinst = submodel->getInstantiation(); 
    
    if (submodinst == NULL ) {
      //'getInstantiation' already sets any errors that might have occurred.
      return LIBSBML_OPERATION_FAILED;
    }
  }

  // Next, recursively find all the targets of SBaseRef elements 
  // and save them, since we're about to rename everything and 
  // we won't be able to find things by name any more.
  ret = saveAllReferencedElements(); 
  if (ret != LIBSBML_OPERATION_SUCCESS) {
    //saveAllReferencedElements sets any errors.
    return ret;
  }

  // Perform deletions (top-down):  
  // need to do this before renaming in case we delete a local parameter.
  ret = performDeletions();
  if (ret != LIBSBML_OPERATION_SUCCESS) {
    return ret;
  }

  //Next, we rename *all* the elements so everything is unique.
  ret = renameAllIDsAndPrepend("");
  if (ret != LIBSBML_OPERATION_SUCCESS) {
    return ret;
  }

  //Perform replacements and conversions (top-down)
  return performReplacementsAndConversions();
}

int CompModelPlugin::saveAllReferencedElements()
{
  SBMLDocument* doc = getSBMLDocument();
  Model* model = static_cast<Model*>(getParentSBMLObject());
  if (model==NULL) {
    if (doc) {
      string error = "No Model parent of the 'comp' model plugin.";
      doc->getErrorLog()->logPackageError("comp", CompModelFlatteningFailed, 1, 3, 1, error);
    }
    return LIBSBML_OPERATION_FAILED;
  }
  int ret = LIBSBML_OPERATION_SUCCESS;

  //Get a list of everything, pull out anything that's a deletion, replacement, or port, and save what they're pointing to.
  List* allElements = model->getAllElements();
  for (unsigned int el=0; el<allElements->getSize(); el++) {
    SBase* element = static_cast<SBase*>(allElements->get(el));
    int type = element->getTypeCode();
    if (type==SBML_COMP_DELETION ||
        type==SBML_COMP_REPLACEDBY ||
        type==SBML_COMP_REPLACEDELEMENT ||
        type==SBML_COMP_PORT) {
          //Don't worry about SBML_COMP_SBASEREF because they're all children of one of the above types.
          SBaseRef* reference = static_cast<SBaseRef*>(element);
          ret = reference->saveReferencedElement();
          if (ret != LIBSBML_OPERATION_SUCCESS) {
            return ret;
          }
    }
  }

  //Now call saveAllReferencedElements for all instantiated submodels.
  for (unsigned long sm=0; sm<getNumSubmodels(); sm++) {
    Model* sub = getSubmodel(sm)->getInstantiation();
    if (sub==NULL) {
      return LIBSBML_OPERATION_FAILED;
    }
    CompModelPlugin* subplug = static_cast<CompModelPlugin*>(sub->getPlugin(getPrefix()));
    if (subplug==NULL) {
      return LIBSBML_OPERATION_FAILED;
    }
    ret = subplug->saveAllReferencedElements();
    if (ret != LIBSBML_OPERATION_SUCCESS) {
      return ret;
    }
  }

  return LIBSBML_OPERATION_SUCCESS;
}

int 
CompModelPlugin::renameAllIDsAndPrepend(const std::string& prefix)
{
  SBMLDocument* doc = getSBMLDocument();
  Model* model = static_cast<Model*>(getParentSBMLObject());
  if (model==NULL) {
    if (doc) {
      string error = "No parent model could be found for the given 'comp' model plugin element.";
      doc->getErrorLog()->logPackageError("comp", CompFlatModelNotValid, 1, 3, 1, error);
    }
    return LIBSBML_INVALID_OBJECT;
  }

  //First rename the elements in all instantiated submodels.
  vector<string> submodids;

  for (unsigned int sm=0; sm<getNumSubmodels(); sm++) {
    Submodel* subm=getSubmodel(sm);
    if (subm==NULL) {
      if (doc) {
        stringstream error;
        error << "No valid submodel number " << sm << "for model " << model->getId();
        doc->getErrorLog()->logPackageError("comp", CompFlatModelNotValid, 1, 3, 1, error.str());
      }
      return LIBSBML_OPERATION_FAILED;
    }
    if (!subm->isSetId()) {
      if (doc) {
        stringstream error;
        error << "Submodel number " << sm << "for model " << model->getId() << " is invalid: it has no 'id' attribute set.";
        doc->getErrorLog()->logPackageError("comp", CompSubmodelAllowedAttributes, 1, 3, 1, error.str());
      }
      return LIBSBML_INVALID_OBJECT;
    }
    submodids.push_back(subm->getId());
  }

  //Check to see if any of the various submodel ids are used as a prefix 
  List* allElements = model->getAllElements();
  findUniqueSubmodPrefixes(submodids, allElements);

  //Now that we've found valid prefixes for all our submodels, call this function recursively on them.
  for (unsigned int sm=0; sm<getNumSubmodels(); sm++) {
    Submodel* subm=getSubmodel(sm); //already checked this above.
    Model* inst = subm->getInstantiation();
    if (inst==NULL) {
      //'getInstantiation' will set its own error messages.
      return LIBSBML_OPERATION_FAILED;
    }
    CompModelPlugin* instp = static_cast<CompModelPlugin*>(inst->getPlugin(getPrefix()));
    if (instp==NULL) {
      if (doc) {
        //Shouldn't happen:  'getInstantiation' turns on the comp plugin.
        string error = "No valid 'comp' plugin for the model instantiated from submodel " + subm->getId();
        doc->getErrorLog()->logPackageError("comp", CompFlatModelNotValid, 1, 3, 1, error);
      }
      return LIBSBML_OPERATION_FAILED;
    }
    int ret = instp->renameAllIDsAndPrepend(prefix + submodids[sm]);
    if (ret != LIBSBML_OPERATION_SUCCESS) {
      //'renameAllIds..' will set its own error messages.
      return ret;
    }
  }

  //Finally, actually rename the elements in *this* model with the prefix.
  if (prefix.empty()) return LIBSBML_OPERATION_SUCCESS; //Nothing to add

  //Rename the SIds, UnitSIds, and MetaIDs, and references to them.
  renameIDs(allElements, prefix);
  return LIBSBML_OPERATION_SUCCESS;
}


/** @cond doxygen-libsbml-internal */
void CompModelPlugin::findUniqueSubmodPrefixes(vector<string>& submodids, List* allElements)
{
  vector<int> suffixes(submodids.size(), 0);
  bool done=false;
  while (!done) {
    done = true;
    for (size_t str=0; str<submodids.size(); str++) {
      stringstream fullprefix;
      fullprefix << submodids[str];
      if (suffixes[str] > 0) {
        fullprefix << suffixes[str];
      }
      fullprefix << getDivider();
      for (unsigned long el=0; el<allElements->getSize(); el++) {
        SBase* element = static_cast<SBase*>(allElements->get(el));
        if (element==NULL) {
          assert(false);
          continue;
        }
        if (element->isSetId() && element->getId().find(fullprefix.str())==0) {
          done = false;
          continue;
        }
        else if (element->isSetMetaId() && element->getMetaId().find(fullprefix.str())==0) {
          done = false;
          continue;
        }
        else if (element->hasNonstandardIdentifierBeginningWith(fullprefix.str())) {
          done = false;
          continue;
        }
        else {
          for (unsigned int p=0; p<element->getNumPlugins(); p++) {
            if (element->getPlugin(p)->hasIdentifierBeginningWith(fullprefix.str())) {
              done = false;
              continue;
            }
          }
        }
      }
      if (!done) {
        suffixes[str]++;
        continue; //Start over from the first ID; otherwise we end up checking a lot of things twice.
      }
    }
  }
  //Now change the submodid's:
  for (size_t str=0; str<submodids.size(); str++) {
    stringstream fullprefix;
    fullprefix << submodids[str];
    if (suffixes[str] > 0) {
      fullprefix << suffixes[str];
    }
    fullprefix << getDivider();
    submodids[str] = fullprefix.str();
  }
}
/** @endcond */

/** 
 * Simple IdentifierTransformer, that prefixes all given 
 * elements with the prefix given to the constructor. 
 * 
 * this will prefix metaids, unitsids and sids. 
 */ 
class PrefixTransformer : public IdentifierTransformer
{
  string mPrefix;
public: 
  PrefixTransformer (const string& prefix) 
  : mPrefix(prefix) {}
  
  virtual int transform(SBase* element)
  {
    // if there is nothing to do return ... 
    if (element == NULL) 
	  return LIBSBML_OPERATION_SUCCESS;
	
	// prefix meta id if we have one ... 
    if (element->isSetMetaId())
    {
      if (element->setMetaId(mPrefix + element->getMetaId()) != LIBSBML_OPERATION_SUCCESS)
        return LIBSBML_OPERATION_FAILED;
    }
	
	// prefix other ids (unitsid, or sid) ...
    // skip local parameters
    if (element->isSetId() && element->getTypeCode() != SBML_LOCAL_PARAMETER)
    {
      if (element->setId(mPrefix + element->getId()) != LIBSBML_OPERATION_SUCCESS)
        return LIBSBML_OPERATION_FAILED;
    }
    return LIBSBML_OPERATION_SUCCESS;
  }
};

/** @cond doxygen-libsbml-internal */
void CompModelPlugin::renameIDs(List* allElements, const string& prefix)
{
  if (prefix=="") return; //Nothing to prepend.
  vector<pair<string, string> > renamedSIds;
  vector<pair<string, string> > renamedUnitSIds;
  vector<pair<string, string> > renamedMetaIds;
  //PrefixTransformer trans(prefix);
  for (unsigned long el=0; el < allElements->getSize(); ++el) 
  {
    SBase* element = static_cast<SBase*>(allElements->get(el));
    string id = element->getId();
    string metaid = element->getMetaId();
    //element->transformIdentifiers(&trans);
    element->prependStringToAllIdentifiers(prefix);
    if (element->getTypeCode() == SBML_LOCAL_PARAMETER) {
      element->setId(id); //Change it back.  This would perhaps be better served by overriding 'prependStringToAllIdentifiers' but hey.
    }
    string newid = element->getId();
    string newmetaid = element->getMetaId();
    if (id != newid) {
      int type = element->getTypeCode();
      if (type==SBML_UNIT_DEFINITION) {
        renamedUnitSIds.push_back(make_pair(id, newid));
      }
      else if (type==SBML_COMP_PORT) {
        //Do nothing--these can only be referenced from outside the Model, so they need to be handled specially.
        // (In the default case, we throw them away).
      }
      else {
        //This is a little dangerous, but hey!  What's a little danger between friends!
        //(What we are assuming is that any attribute you can get with 'getId' is of the type 'SId')
        renamedSIds.push_back(make_pair(id, newid));
      }
    }
    if (metaid != newmetaid) {
      renamedMetaIds.push_back(make_pair(metaid, newmetaid));
    }
  }

  for (unsigned long el=0; el<allElements->getSize(); el++) {
    SBase* element = static_cast<SBase*>(allElements->get(el));
    for (size_t id=0; id<renamedSIds.size(); id++) 
    {
      element->renameSIdRefs(renamedSIds[id].first, renamedSIds[id].second);
    }
    for (size_t uid=0; uid<renamedUnitSIds.size(); uid++) 
    {
      element->renameUnitSIdRefs(renamedUnitSIds[uid].first, renamedUnitSIds[uid].second);
    }
    for (size_t mid=0; mid<renamedMetaIds.size(); mid++) 
    {
      element->renameMetaIdRefs(renamedMetaIds[mid].first, renamedMetaIds[mid].second);
    }
  }
}
/** @endcond */

int CompModelPlugin::performDeletions()
{
  int ret = LIBSBML_OPERATION_SUCCESS;
  SBMLDocument* doc = getSBMLDocument();
  Model* model = static_cast<Model*>(getParentSBMLObject());
  if (model==NULL) {
    if (doc) {
      string error = "No parent model could be found for the given 'comp' model plugin element.";
      doc->getErrorLog()->logPackageError("comp", CompFlatModelNotValid, 1, 3, 1, error);
    }
    return LIBSBML_OPERATION_FAILED;
  }

  //Since deletions only exist in submodels, loop through the submodels.
  for (unsigned int sub=0; sub<getNumSubmodels(); sub++) {
    Submodel* submodel = getSubmodel(sub);
    //First perform any deletions
    for (unsigned int d=0; d<submodel->getNumDeletions(); d++) {
      Deletion* deletion = submodel->getDeletion(d);
      ret = deletion->performDeletion();
      if (ret!=LIBSBML_OPERATION_SUCCESS) {
        return ret;
      }
    }
    //Next perform any deletions in that instantiated submodel (some of which may have just been deleted)
    Model* mod = submodel->getInstantiation();
    if (mod==NULL) {
      //getInstantiation sets its own error messages.
      return LIBSBML_OPERATION_FAILED;
    }
    CompModelPlugin* modplug = static_cast<CompModelPlugin*>(mod->getPlugin(getPrefix()));
    if (modplug==NULL) {
      if (doc) {
        //Shouldn't happen:  'getInstantiation' turns on the comp plugin.
        string error = "No valid 'comp' plugin for the model instantiated from submodel " + submodel->getId();
        doc->getErrorLog()->logPackageError("comp", CompFlatModelNotValid, 1, 3, 1, error);
      }
      return LIBSBML_OPERATION_FAILED;
    }
    modplug->performDeletions();
  }
  return ret;
}

int CompModelPlugin::performReplacementsAndConversions()
{
  int ret = LIBSBML_OPERATION_SUCCESS;
  SBMLDocument* doc = getSBMLDocument();
  Model* model = static_cast<Model*>(getParentSBMLObject());
  if (model==NULL) {
    if (doc) {
      string error = "No parent model could be found for the given 'comp' model plugin element.";
      doc->getErrorLog()->logPackageError("comp", CompFlatModelNotValid, 1, 3, 1, error);
    }
    return LIBSBML_OPERATION_FAILED;
  }
  List* allElements = model->getAllElements();
  vector<ReplacedElement*> res;
  vector<ReplacedBy*> rbs;
  //Collect replaced elements and replaced by's.
  for (unsigned int e=0; e<allElements->getSize(); e++) {
    SBase* element = static_cast<SBase*>(allElements->get(e));
    int type = element->getTypeCode();
    if (type==SBML_COMP_REPLACEDELEMENT) {
      ReplacedElement* reference = static_cast<ReplacedElement*>(element);
      res.push_back(reference);
    }
    if (type==SBML_COMP_REPLACEDBY) {
      ReplacedBy* reference = static_cast<ReplacedBy*>(element);
      rbs.push_back(reference);
    }
  }
    
  //Replacements one way:
  for (size_t re=0; re<res.size(); re++) {
    ret = res[re]->performReplacement();
    if (ret != LIBSBML_OPERATION_SUCCESS) {
      return ret;
    }
  }

  //And replacements the other way
  for (size_t rb=0; rb<rbs.size(); rb++) {
    ret = rbs[rb]->performReplacement();
    if (ret != LIBSBML_OPERATION_SUCCESS) return ret;
  }

  //Now do the same thing for anything left over in the submodels
  for (unsigned int sub=0; sub<getNumSubmodels(); sub++) {
    Submodel* submodel = getSubmodel(sub);
    Model* mod = submodel->getInstantiation();
    if (mod==NULL) return LIBSBML_OPERATION_FAILED;
    CompModelPlugin* modplug = static_cast<CompModelPlugin*>(mod->getPlugin(getPrefix()));
    if (modplug==NULL) return LIBSBML_OPERATION_FAILED;
    //'left behind' converions (not LaHaye-style)
    ret = submodel->convertTimeAndExtent();
    if (ret != LIBSBML_OPERATION_SUCCESS) return ret;
    ret = modplug->performReplacementsAndConversions();
    if (ret != LIBSBML_OPERATION_SUCCESS) return ret;
  }
  return ret;
}

/** @cond doxygen-libsbml-internal */

bool 
CompModelPlugin::accept(SBMLVisitor& v) const
{
  const Model * model = static_cast<const Model * >(this->getParentSBMLObject()); 
  
  v.visit(*model);
  v.leave(*model);

  for (unsigned int i = 0; i < getNumSubmodels(); i++)
  {
    getSubmodel(i)->accept(v);
  }
  for (unsigned int i = 0; i < getNumPorts(); i++)
  {
    getPort(i)->accept(v);
  }

  return true;
}

/** @endcond */


LIBSBML_EXTERN
Submodel_t *
CompModelPlugin_createSubmodel(CompModelPlugin_t * modelPlug)
{
  return modelPlug->createSubmodel();
}

LIBSBML_CPP_NAMESPACE_END

