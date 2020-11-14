  /** @cond doxygenLibsbmlInternal */

/**
 * @file:   MultiConsistencyConstraints.cpp
 * @brief:  Implementation of the MultiConsistencyConstraints class
 * @author: Fengkai Zhang
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
 * Copyright (C) 2009-2013 jointly by the following organizations:
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

#ifndef  AddingConstraintsToValidator

#include <sbml/validator/VConstraint.h>
#include <sbml/packages/multi/common/MultiExtensionTypes.h>


#endif  /* AddingConstrainstToValidator */

#include <sbml/validator/ConstraintMacros.h>
#include <sbml/packages/multi/validator/MultiSBMLError.h>

#include "MultiMathCiCheckSpeciesReference.h"
#include "MultiMathCiCheckRepresentationType.h"

/** @cond doxygenIgnored */

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

static const SpeciesTypeComponentIndex * __getSpeciesTypeComponentIndexFromComponentId(const Model & model, const std::string & componentId)
{
  const SpeciesTypeComponentIndex * stci = NULL;
  const MultiModelPlugin * mPlugin =
      dynamic_cast<const MultiModelPlugin*>(model.getPlugin("multi"));
  if (mPlugin != 0) {
    for (unsigned int i = 0; stci == NULL && i < mPlugin->getNumMultiSpeciesTypes(); i++) {
          const MultiSpeciesType * speciesType = mPlugin->getMultiSpeciesType(i);
          if (speciesType) {
              stci = speciesType->getSpeciesTypeComponentIndex(componentId);
          }
      }
  }
  return stci;
}

static const SpeciesTypeInstance * __getSpeciesTypeInstanceFromComponentId(const Model & model, const std::string & componentId)
{
  const SpeciesTypeInstance * sti = NULL;
  const MultiModelPlugin * mPlugin =
      dynamic_cast<const MultiModelPlugin*>(model.getPlugin("multi"));
  if (mPlugin != 0) {
    const SpeciesTypeComponentIndex * stci = __getSpeciesTypeComponentIndexFromComponentId(model, componentId);
    if (stci) {
      const std::string& nextComponentId = stci->getComponent();

      sti = __getSpeciesTypeInstanceFromComponentId(model, nextComponentId); // recursive
    }

    if (!sti) {
      for (unsigned int i = 0; sti == NULL && i < mPlugin->getNumMultiSpeciesTypes(); i++) {
            const MultiSpeciesType * speciesType = mPlugin->getMultiSpeciesType(i);
            if (speciesType) {
                sti = speciesType->getSpeciesTypeInstance(componentId);
            }
      }
    }
  }
  return sti;
}

static const MultiSpeciesType* __getSpeciesTypeFromComponentId(const Model & model, const std::string & componentId)
{
  const MultiSpeciesType * st = NULL;
  const MultiModelPlugin * mPlugin =
      dynamic_cast<const MultiModelPlugin*>(model.getPlugin("multi"));

  if (mPlugin != 0) {

    const SpeciesTypeComponentIndex * stci = __getSpeciesTypeComponentIndexFromComponentId(model, componentId);
    if (stci) {
      const std::string& nextComponentId = stci->getComponent();

      st = __getSpeciesTypeFromComponentId(model, nextComponentId); // recursive
    }

    if (!st) {
      std::string stId = componentId;
      const SpeciesTypeInstance * sti = __getSpeciesTypeInstanceFromComponentId(model, componentId);
      if (sti != NULL) {
        stId = sti->getSpeciesType();
      }

      if (mPlugin != 0) {
        st = mPlugin->getMultiSpeciesType(stId);
      }
    }
  }

  return st;
}

static const SpeciesFeatureType* __getSpeciesTypeFromComponent(const Model & model, const std::string & speciesTypeId,
    const std::string & speciesFeatureId)
{
  const SpeciesFeatureType * sft = NULL;
  bool good = true;

  const MultiModelPlugin * modelPlugin =
      dynamic_cast<const MultiModelPlugin*>(model.getPlugin("multi"));

  if (modelPlugin == 0)
  {
    good = false;
  }

  const MultiSpeciesType * speciesType = 0;
  if (good)
  {
    speciesType = modelPlugin->getMultiSpeciesType(speciesTypeId);
  }

  if (speciesType == 0)
  {
    good = false;
  }

  if (good)
  {
    sft = speciesType->getSpeciesFeatureType(speciesFeatureId);

    if (sft == NULL) {
      for (unsigned int i = 0;
          good && sft == NULL
              && i < speciesType->getNumSpeciesTypeInstances(); i++)
      {
        const SpeciesTypeInstance * speciesTypeInstance =
            speciesType->getSpeciesTypeInstance(i);
        const std::string& refSpeciesTypeId =
            speciesTypeInstance->getSpeciesType();

        sft = __getSpeciesTypeFromComponent(model, refSpeciesTypeId, speciesFeatureId);
      }

    }
  }

  return sft;
}

static bool __isSpeciesTypeComponent(const Model & model, const std::string & componentId)
{
  bool found = false;
  const MultiModelPlugin * mPlugin =
      dynamic_cast<const MultiModelPlugin*>(model.getPlugin("multi"));
  if (mPlugin != 0) {
    const MultiSpeciesType * st = mPlugin->getMultiSpeciesType(componentId);
    found = (st != NULL);

      for (unsigned int i = 0; !found && i < mPlugin->getNumMultiSpeciesTypes(); i++) {
          const MultiSpeciesType * speciesType = mPlugin->getMultiSpeciesType(i);
          if (speciesType) {
              if (speciesType->getId() == componentId) {
                  found = true;
              }
              found = (speciesType->getSpeciesTypeInstance(componentId) != NULL);
              found = found || (speciesType->getSpeciesTypeComponentIndex(componentId) != NULL);
          }
      }
  }
  return found;
}

static bool __isSpeciesTypeInstanceOrIndex(const Model & model, const std::string & componentId)
{
  bool found = false;
  const MultiModelPlugin * mPlugin =
      dynamic_cast<const MultiModelPlugin*>(model.getPlugin("multi"));
  if (mPlugin != 0) {
      for (unsigned int i = 0; !found && i < mPlugin->getNumMultiSpeciesTypes(); i++) {
          const MultiSpeciesType * speciesType = mPlugin->getMultiSpeciesType(i);
          if (speciesType) {
              found = found || (speciesType->getSpeciesTypeInstance(componentId) != NULL);
              found = found || (speciesType->getSpeciesTypeComponentIndex(componentId) != NULL);
          }
      }
  }
  return found;
}

static bool __isSpeciesTypeComponent(const Model & model, const std::string & speciesTypeId,const std::string & componentId)
{
        bool isComponent = false;
        bool good = true;

        if (speciesTypeId == componentId) return true;

        const MultiModelPlugin * modelPlugin =
                        dynamic_cast<const MultiModelPlugin*>(model.getPlugin("multi"));

        if (modelPlugin == 0) {
                good = false;
        }

        const MultiSpeciesType * speciesType = 0;
        if (good) {
                 speciesType = modelPlugin->getMultiSpeciesType(speciesTypeId);
        }

        if (speciesType == 0) {
                good = false;
        }

        if (good) {
                for(unsigned int i = 0; good && !isComponent && i < speciesType->getNumSpeciesTypeInstances(); i++) {
                        const SpeciesTypeInstance * speciesTypeInstance = speciesType->getSpeciesTypeInstance(i);
                        if (speciesTypeInstance->getId() == componentId) {
                                isComponent = true;
                        }
                        else {
                                std::string refSpeciesTypeId = speciesTypeInstance->getSpeciesType();
                                isComponent = __isSpeciesTypeComponent(model, refSpeciesTypeId, componentId);
                        }
                }

                for(unsigned int i = 0; good && !isComponent && i < speciesType->getNumSpeciesTypeComponentIndexes(); i++) {
                        const SpeciesTypeComponentIndex * speciesTypeComponentIndex = speciesType->getSpeciesTypeComponentIndex(i);
                        if (speciesTypeComponentIndex->getId() == componentId) {
                                isComponent = true;
                        }
                }
        }

        return isComponent;
}


//static bool __isSpeciesFeature(const Model & model, const std::string & speciesId,const std::string & speciesFeatureId)
//{
//        bool isSpeciesFeature = false;
//        bool good = true;
//
//
//        const Species * species = 0;
//        if (good) {
//                 species = model.getSpecies(speciesId);
//        }
//
//        if (species == 0) {
//                good = false;
//        }
//
//        if (good) {
//            const MultiSpeciesPlugin * speciesPlugin =
//                dynamic_cast<const MultiSpeciesPlugin*>(species->getPlugin("multi"));
//
//            if (speciesPlugin == 0) {
//                    good = false;
//            }
//
//            for(unsigned int i = 0; good && !isSpeciesFeature && i < speciesPlugin->getNumSpeciesFeatures(); i++) {
//                const SpeciesFeature * speciesFeature = speciesPlugin->getSpeciesFeature(i);
//                if (speciesFeature->getId() == speciesFeatureId) {
//                        isSpeciesFeature = true;
//                }
//            }
//
//            for(unsigned int i = 0; good && !isSpeciesFeature && i < speciesPlugin->getNumSubListOfSpeciesFeatures(); i++) {
//                const SubListOfSpeciesFeatures * subListOfSpeciesFeatures = speciesPlugin->getSubListOfSpeciesFeatures(i);
//                for (unsigned int j = 0; good && !isSpeciesFeature && j < subListOfSpeciesFeatures->getNumSpeciesFeatures(); j++) {
//                    const SpeciesFeature * speciesFeature = subListOfSpeciesFeatures->get(j);
//                    if (speciesFeature->getId() == speciesFeatureId) {
//                         isSpeciesFeature = true;
//                    }
//                }
//            }
//
//        }
//
//        return isSpeciesFeature;
//}

static bool __isReferencedByChildCompartment(const Compartment * compartment, const std::string & compartmentId)
{
  bool isReferenced = false;

  const Model * model = compartment->getModel();

  const MultiCompartmentPlugin * mCompartmentPlugin =
      dynamic_cast<const MultiCompartmentPlugin *>(compartment->getPlugin("multi"));

  if (mCompartmentPlugin != NULL) {
    for (unsigned i = 0; !isReferenced && i < mCompartmentPlugin->getNumCompartmentReferences(); i++) {
      const CompartmentReference * cr = mCompartmentPlugin->getCompartmentReference(i);
      isReferenced = (compartmentId == cr->getCompartment());
      if (!isReferenced) {
        const Compartment * childCompartment = model->getCompartment(cr->getCompartment());
        if (childCompartment != NULL) {
          isReferenced = __isReferencedByChildCompartment(childCompartment, compartmentId);
        }
      }
    }
  }

  return isReferenced;
}

LIBSBML_CPP_NAMESPACE_END


//static const Compartment * __getParentCompartment(const Compartment * compartment)
//{
//	const Compartment * parentCompartment = NULL;
//
//	const Model * model = compartment->getModel();
//
//	if (model == NULL) {
//		return parentCompartment; // NULL
//	}
//
//	const ListOfCompartments * listOfCompartments = model->getListOfCompartments();
//
//	for (unsigned int i = 0; parentCompartment == NULL && i < model->getNumCompartments(); i++) {
//		const Compartment * c = model->getCompartment(i);
//		if (c != compartment) {
//			const MultiCompartmentPlugin * mPlugin =
//					dynamic_cast<const MultiCompartmentPlugin *> (compartment->getPlugin("multi"));
//			if (mPlugin != NULL) {
//				const ListOfCompartmentReferences * listCompartmentReferences =
//						mPlugin->getListOfCompartmentReferences();
//				for (unsigned int j = 0; parentCompartment == NULL && j < mPlugin->getNumCompartmentReferences(); j++) {
//					const CompartmentReference * cr = mPlugin->getCompartmentReference(j);
//					if (cr->getCompartment() == compartment->getId()) {
//						parentCompartment = c;
//					}
//				}
//			}
//		}
//	}
//
//	return parentCompartment;
//}

//static bool __isBindingSiteSpeciesTypeComponent(const Model & model, const std::string & componentId)
//{
//  bool found = false;
//  const MultiModelPlugin * mPlugin =
//      dynamic_cast<const MultiModelPlugin*>(model.getPlugin("multi"));
//  if (mPlugin != 0) {
//
//	  if (__isSpeciesTypeComponent(model, componentId)) {
//
//	  }
//	  else if (__isSpeciesTypeInstanceComponent(model, componentId)) {
//
//	  }
//	  else if (__isSpeciesTypeIndexComponent(model, componentId)) {
//
//	  }
//
//  }
//  return found;
//}



/** @endcond */

/** PUT CONSTRAINTS HERE */

//************************************
// General rules about the Multi package

// MultiNSUndeclared                     = 7010101 - inexplicitly checked at read when creating plugin at 'SBMLDocument::readAttributes()'
// MultiElementNotInNs                   = 7010102 - inexplicitly checked at read when creating plugin at 'SBMLDocument::readAttributes()'

// SK renumbered from 70101nn to 70201nn

// MultiSBML_RequiredAttMissing          = 7020101 - caught at read at 'MultiSBMLDocumentPlugin::readAttributes()'
// MultiSBML_RequiredAttMustBeBoolean    = 7020102 - caught at read at 'MultiSBMLDocumentPlugin::readAttributes()'
// MultiSBML_RequiredAttMustBeTrue       = 7020103 - caught at read at 'MultiSBMLDocumentPlugin::readAttributes()'

//************************************
// Rules for extended Model objects

// SK renumbered from 70201nn to 70202nn
// MultiLofStps_OnlyOne                  = 7020201 - caught at read at 'MultiModelPlugin::createObject()'
// MultiLofStps_NoEmpty                  = 7020202 - caught at read at 'SBase::checkListOfPopulated()', TODO: need update if core is revised.
// MultiLofStps_AllowedAtts              = 7020203 - caught at read at 'MultiSpeciesType::readAttributes()'
// MultiLofStps_AllowedElts              = 7020204 - caught at read at 'SBase::logUnknownElement()'

//************************************
// Rules for extended Compartment objects

// SK renumbered from 70202nn to 70203nn
// MultiExCpa_AllowedMultiAtts           = 7020301 - caught at read at 'MultiCompartmentPlugin::readAttributes()'
// MultiExCpa_IsTypeAtt_Invalid          = 7020302 - caught at read at 'MultiCompartmentPlugin::readAttributes()'
// MultiExCpa_IsTypeAtt_Required         = 7020303 - caught at read at 'MultiCompartmentPlugin::readAttributes()'

// MultiExCpa_IsTypeAtt_SameAsParent     = 7020304
/* !< Extended Compartment: 'isType' attribute, if referenced, must be same as that of the containing compartment */

START_CONSTRAINT (MultiExCpa_IsTypeAtt_SameAsParent, Compartment, compartment)
{
  const MultiCompartmentPlugin * compPlug =
    dynamic_cast<const MultiCompartmentPlugin*>(compartment.getPlugin("multi"));

  pre (compPlug != 0);

  std::string parentCompartmentId = compartment.getId();
  bool parentCompartmentIsType = compPlug->isSetIsType() && compPlug->getIsType();

  for (unsigned int i = 0; i < compPlug->getNumCompartmentReferences(); i++) {
      const CompartmentReference * cRef = compPlug->getCompartmentReference(i);
      std::string referencedCompartmentId = cRef->getCompartment();
      const Compartment * referencedCompartment = m.getCompartment(referencedCompartmentId);

      if (referencedCompartment) {
      const MultiCompartmentPlugin * referencedCompPlug =
      dynamic_cast<const MultiCompartmentPlugin*>(referencedCompartment->getPlugin("multi"));
      bool referencedCompartmentIsType = referencedCompPlug->isSetIsType() && referencedCompPlug->getIsType();

      inv(parentCompartmentIsType == referencedCompartmentIsType);
      }
  }
}
END_CONSTRAINT

// MultiExCpa_CpaTypAtt_Restrict         = 7020305
/*!< Extended Compartment: Compartment type can not reference another compartment type */

START_CONSTRAINT (MultiExCpa_CpaTypAtt_Restrict, Compartment, compartment)
{
  const MultiCompartmentPlugin * compPlug =
    dynamic_cast<const MultiCompartmentPlugin*>(compartment.getPlugin("multi"));

  pre (compPlug != 0);

  const bool isType = compPlug->isSetIsType() && compPlug->getIsType();
  if (isType) {
      inv(compPlug->isSetCompartmentType() == false);
  }
}
END_CONSTRAINT

// MultiLofCpaRefs_OnlyOne               = 7020306 - caught at read at 'MultiCompartmentPlugin::createObject()'
// MultiLofCpaRefs_NoEmpty               = 7020307 - caught at read at 'SBase::checkListOfPopulated()', TODO: need update if core is revised.
// MultiLofCpaRefs_AllowedAtts           = 7020308 - caught at read at 'CompartmentReference::readAttributes()'
// MultiLofCpaRefs_AllowedElts           = 7020309 - caught at read at 'SBase::logUnknownElement()'

//************************************
// SK moved block to preserve numbering
//// Rules for CompartmentReference objects
//
//// MultiCpaRef_AllowedCoreAtts           = 7020301 - caught at read at 'CompartmentReference::readAttributes()'
//// MultiCpaRef_AllowedCoreElts           = 7020302 - caught at read at 'SBase::logUnknownElement()'
//// MultiCpaRef_AllowedMultiAtts          = 7020303 - caught at read at 'CompartmentReference::readAttributes()'
//
//// MultiCpaRef_CompartmentAtt_Ref        = 7020304
///*!< CompartmentReference: 'compartment' must be the 'id' of a compartment */
//
//START_CONSTRAINT (MultiCpaRef_CompartmentAtt_Ref, CompartmentReference, compartmentReference)
//{
//  std::string compartmentId = compartmentReference.getCompartment();
//  inv(m.getCompartment(compartmentId) != 0);
//}
//END_CONSTRAINT
//
//// MultiCpaRef_IdRequiredOrOptional      = 7020305
///*!< CompartmentReference: 'multi:id' is required when referencing the same compartment */
//
//START_CONSTRAINT (MultiCpaRef_IdRequiredOrOptional, Compartment, compartment)
//{
//  const MultiCompartmentPlugin * compPlug =
//      dynamic_cast<const MultiCompartmentPlugin*>(compartment.getPlugin("multi"));
//
//  pre (compPlug != 0);
//
//  const ListOfCompartmentReferences * listOfCompartmentReferences = compPlug->getListOfCompartmentReferences();
//
//  for (unsigned int i = 0; i < listOfCompartmentReferences->size(); i++) {
//      const CompartmentReference * compartmentReference = listOfCompartmentReferences->get(i);
//      std::string compartmentId = compartmentReference->getCompartment();
//
//      for (unsigned int j = i + 1; j < listOfCompartmentReferences->size(); j++) {
//          const CompartmentReference * anotherCompartmentReference = listOfCompartmentReferences->get(j);
//          std::string anotherCompartmentId = anotherCompartmentReference->getCompartment();
//
//          if (compartmentId == anotherCompartmentId) {
//              inv(compartmentReference->isSetId() == true);
//              inv(anotherCompartmentReference->isSetId() == true);
//          }
//      }
//  }
//}
//END_CONSTRAINT
//
//// MultiCpaRef_NoReferenceToAnyParent      = 7020306
///*!< CompartmentReference: A compartmentReference cannot reference any parent compartment */
//START_CONSTRAINT (MultiCpaRef_NoReferenceToAnyParent, Compartment, compartment)
//{
//	const string & compartmentId = compartment.getId();
//	bool hasCircularReference = __isReferencedByChildCompartment(&compartment, compartmentId);
//	inv(hasCircularReference == false);
//}
//END_CONSTRAINT
//
//
//************************************
// Rules for SpeciesType objects

// MultiSpt_AllowedCoreAtts              = 7020401 - caught at read at 'MultiSpeciesType::readAttributes()'
// MultiSpt_AllowedCoreElts              = 7020402 - caught at read at 'SBase::logUnknownElement()'
// MultiSpt_AllowedMultiAtts             = 7020403 - caught at read at 'MultiSpeciesType::readAttributes()'

// MultiSpt_CompartmentAtt_Ref           = 7020404
/*!< SpeciesType: 'compartment' must be the 'id' of a compartment */

START_CONSTRAINT (MultiSpt_CompartmentAtt_Ref, MultiSpeciesType, speciesType)
{
  if (!speciesType.isSetCompartment()) return;

  std::string compartmentId = speciesType.getCompartment();
  inv(m.getCompartment(compartmentId) != 0);
}
END_CONSTRAINT

// MultiSpt_ListOfDefs_NoEmpty           = 7020405 - caught at read at 'SBase::checkListOfPopulated()', TODO: need update if core is revised.
// MultiLofSpeFtrTyps_onlyOne            = 7020406 - caught at read at 'MultiSpeciesType::createObject()'
// MultiLofSpeFtrTyps_Elts               = 7020407 - caught at read at 'SBase::logUnknownElement()'
// MultiLofSpeFtrTyps_AllowedAtts        = 7020408 - caught at read at 'SpeciesFeatureType::readAttributes()'
// MultiLofSptInss_onlyOne               = 7020409 - caught at read at 'MultiSpeciesType::createObject()'
// MultiLofSptInss_Elts                  = 7020410 - caught at read at 'SBase::logUnknownElement()'
// MultiLofSptInss_AllowedAtts           = 7020411 - caught at read at 'SpeciesTypeInstance::readAttributes()'
// MultiLofSptCpoInds_onlyOne            = 7020412 - caught at read at 'MultiSpeciesType::createObject()'
// MultiLofSptCpoInds_Elts               = 7020413 - caught at read at 'SBase::logUnknownElement()'
// MultiLofSptCpoInds_AllowedAtts        = 7020414 - caught at read at 'SpeciesTypeComponentIndex::readAttributes()'
// MultiLofInSptBnds_onlyOne             = 7020415 - caught at read at 'MultiSpeciesType::createObject()'
// MultiLofInSptBnds_Elts                = 7020416 - caught at read at 'SBase::logUnknownElement()'
// MultiLofInSptBnds_AllowedAtts         = 7020417 - caught at read at 'InSpeciesTypeBond::readAttributes()'

//************************************
// Rules for BindingSiteSpeciesType objects

// MultiBstSpt_Restrict                  = 7020501
/*!< BindingSiteSpeciesType: Not permitted to have listOfSpeciesTypeInstances */

START_CONSTRAINT (MultiBstSpt_Restrict, MultiSpeciesType, speciesType)
{
  if (speciesType.getTypeCode() == SBML_MULTI_BINDING_SITE_SPECIES_TYPE) {
    inv(speciesType.getNumSpeciesTypeInstances() == 0);
  }
}
END_CONSTRAINT

//************************************
// Rules for SpeciesFeatureType objects

// MultiSpeFtrTyp_AllowedCoreAtts        = 7020601 - caught at read at 'MultiSpeciesFeatureType::readAttributes()'
// MultiSpeFtrTyp_AllowedCoreElts        = 7020602 - caught at read at 'SBase::logUnknownElement()'
// MultiSpeFtrTyp_AllowedMultiAtts       = 7020603 - caught at read at 'MultiSpeciesFeatureType::readAttributes()'
// MultiSpeFtrTyp_OccAtt_Ref             = 7020604 - caught at read at 'MultiSpeciesFeatureType::readAttributes()'

// MultiSpeFtrTyp_RestrictElt            = 7020605
/*!< SpeciesFeatureType: Required to have one listOfPossibleSpeciesFeatureValues */

START_CONSTRAINT (MultiSpeFtrTyp_RestrictElt, SpeciesFeatureType, speciesFeatureType)
{
//  inv(speciesFeatureType.getNumPossibleSpeciesFeatureValues() > 0);
}
END_CONSTRAINT

// MultiLofPsbSpeFtrVals_AllowedAtts     = 7020606 - caught at read at 'PossibleSpeciesFeatureValue::readAttributes()'
// MultiLofPsbSpeFtrVals_Elts            = 7020607 - caught at read at 'SBase::logUnknownElement()'
// MultiLofPsbSpeFtrVals_NoEmpty         = 7020608 - caught at read at 'SBase::checkListOfPopulated()', TODO: need update if core is revised.

//************************************
// Rules for PossibleSpeciesFeatureValue objects

// MultiPsbSpeFtrVal_AllowedCoreAtts     = 7020701 - caught at read at 'PossibleSpeciesFeatureValue::readAttributes()'
// MultiPsbSpeFtrVal_AllowedCoreElts     = 7020702 - caught at read at 'SBase::logUnknownElement()'
// MultiPsbSpeFtrVal_AllowedMultiAtts    = 7020703 - caught at read at 'PossibleSpeciesFeatureValue::readAttributes()'

// MultiPsbSpeFtrVal_NumAtt_Ref          = 7020704
/*!< PossibleSpeciesFeatureValue: 'numbericValue' must be the 'id' of a parameter */

START_CONSTRAINT (MultiPsbSpeFtrVal_NumAtt_Ref, PossibleSpeciesFeatureValue, possibleSpeciesFeatureValue)
{
  if (possibleSpeciesFeatureValue.isSetNumericValue()) {
    std::string numericValueId = possibleSpeciesFeatureValue.getNumericValue();
    inv(m.getParameter(numericValueId) != 0);
  }
}
END_CONSTRAINT

//************************************
// Rules for SpeciesTypeInstance objects

// MultiSptIns_AllowedCoreAtts           = 7020801 - caught at read at 'SpeciesTypeInstance::readAttributes()'
// MultiSptIns_AllowedCoreElts           = 7020802 - caught at read at 'SBase::logUnknownElement()'
// MultiSptIns_AllowedMultiAtts          = 7020803 - caught at read at 'SpeciesTypeInstance::readAttributes()'

// MultiSptIns_SptAtt_Ref                = 7020805
/*!< SpeciesTypeInstance: 'speciesType' must be the 'id' of a speciesType */

START_CONSTRAINT (MultiSptIns_SptAtt_Ref, SpeciesTypeInstance, speciesTypeInstance)
{
  std::string speciesTypeId = speciesTypeInstance.getSpeciesType();
  const MultiModelPlugin * mPlugin =
      dynamic_cast<const MultiModelPlugin*>(m.getPlugin("multi"));

  pre (mPlugin != 0);

  inv(mPlugin->getMultiSpeciesType(speciesTypeId) != 0);
}
END_CONSTRAINT

// MultiSptIns_CpaRefAtt_Ref             = 7020806
/*!< SpeciesTypeInstance: 'compartmentReference' must be the 'id' of a compartmentReference */

START_CONSTRAINT (MultiSptIns_CpaRefAtt_Ref, SpeciesTypeInstance, speciesTypeInstance)
{

  bool found = !speciesTypeInstance.isSetCompartmentReference();

  if (!found) {
      std::string compartmentReferenceId = speciesTypeInstance.getCompartmentReference();

      for (unsigned int i = 0; !found && i < m.getNumCompartments(); i++) {

        const MultiCompartmentPlugin * compartmentPlugin =
            dynamic_cast<const MultiCompartmentPlugin*>(m.getCompartment(i)->getPlugin("multi"));

        if (compartmentPlugin != 0) {
            if (compartmentPlugin->getCompartmentReference(compartmentReferenceId)) {
                found = true;
            }
        }
      }
  }

  inv(found == true);
}
END_CONSTRAINT


//************************************
// Rules for SpeciesTypeComponentIndex objects

// MultiSptCpoInd_AllowedCoreAtts        = 7020901 - caught at read at 'SpeciesTypeComponentIndex::readAttributes()'
// MultiSptCpoInd_AllowedCoreElts        = 7020902 - caught at read at 'SBase::logUnknownElement()'
// MultiSptCpoInd_AllowedMultiAtts       = 7020903 - caught at read at 'SpeciesTypeComponentIndex::readAttributes()'

// MultiSptCpoInd_CpoAtt_Ref             = 7020904
/*!< SpeciesTypeComponentIndex: 'component' must be the 'id' of a component */

START_CONSTRAINT (MultiSptCpoInd_CpoAtt_Ref, SpeciesTypeComponentIndex, speciesTypeComponentIndex)
{
  std::string componentId = speciesTypeComponentIndex.getComponent();
  bool found = __isSpeciesTypeComponent(m, componentId);
  inv (found == true);
}
END_CONSTRAINT

// MultiSptCpoInd_IdParAtt_Ref           = 7020907
/*!< SpeciesTypeComponentIndex: 'identifyingParent' must be the 'id' of a component */

START_CONSTRAINT (MultiSptCpoInd_IdParAtt_Ref, SpeciesTypeComponentIndex, speciesTypeComponentIndex)
{
  if (speciesTypeComponentIndex.isSetIdentifyingParent()) {
      std::string identifyingParentId = speciesTypeComponentIndex.getIdentifyingParent();
      bool found = __isSpeciesTypeComponent(m, identifyingParentId);
      inv (found == true);
  }
}
END_CONSTRAINT

//************************************
// Rules for InSpeciesTypeBond objects

// MultiInSptBnd_AllowedCoreAtts         = 7021101 - caught at read at 'InSpeciesTypeBond::readAttributes()'
// MultiInSptBnd_AllowedCoreElts         = 7021102 - caught at read at 'SBase::logUnknownElement()'
// MultiInSptBnd_AllowedMultiAtts        = 7021103 - caught at read at 'InSpeciesTypeBond::readAttributes()'

// MultiInSptBnd_Bst1Att_Ref             = 7021104
/*!< InSpeciesTypeBond: 'bindingSite1' must be the 'id' of a speciesTypeInstance or speciesTypeComponentIndex */

START_CONSTRAINT (MultiInSptBnd_Bst1Att_Ref, InSpeciesTypeBond, inSpeciesTypeBond)
{
  std::string bindingSite1Id = inSpeciesTypeBond.getBindingSite1();
  inv( __isSpeciesTypeInstanceOrIndex(m, bindingSite1Id) == true);
}
END_CONSTRAINT

// MultiInSptBnd_Bst2Att_Ref             = 7021105
/*!< InSpeciesTypeBond: 'bindingSite2' must be the 'id' of a speciesTypeInstance or speciesTypeComponentIndex */

START_CONSTRAINT (MultiInSptBnd_Bst2Att_Ref, InSpeciesTypeBond, inSpeciesTypeBond)
{
  std::string bindingSite2Id = inSpeciesTypeBond.getBindingSite2();
  inv( __isSpeciesTypeInstanceOrIndex(m, bindingSite2Id) == true);
}
END_CONSTRAINT

// MultiInSptBnd_TwoBstAtts_NotSame      = 7021106
/*!< InSpeciesTypeBond: 'bindingSite1' and 'bindingSite2' can not reference the same binding site */

START_CONSTRAINT (MultiInSptBnd_TwoBstAtts_NotSame, InSpeciesTypeBond, inSpeciesTypeBond)
{
  std::string bindingSite1Id = inSpeciesTypeBond.getBindingSite1();
  std::string bindingSite2Id = inSpeciesTypeBond.getBindingSite2();
  inv( bindingSite1Id != bindingSite2Id);
}
END_CONSTRAINT

//************************************
// Rules for extended Species objects

// MultiExSpe_AllowedMultiAtts           = 7021201 - caught at read at 'MultiSpeciesPlugin::readAttributes()'

// MultiExSpe_RestrictSpeciesTypeAtt     = 7021202
/*!< Extended Species: SpeciesType attribute must have value of the id of a speciesType */

START_CONSTRAINT (MultiExSpe_RestrictSpeciesTypeAtt, Species, species)
{
  const MultiSpeciesPlugin * speciesPlugin =
      dynamic_cast<const MultiSpeciesPlugin*>(species.getPlugin("multi"));

  pre (speciesPlugin != 0);

  if (speciesPlugin->isSetSpeciesType()) {
      std::string speciesTypeId = speciesPlugin->getSpeciesType();

      const MultiModelPlugin * mPlugin =
          dynamic_cast<const MultiModelPlugin*>(m.getPlugin("multi"));

      pre (mPlugin != 0);

      inv (mPlugin->getMultiSpeciesType(speciesTypeId) != 0);
  }
}
END_CONSTRAINT

// MultiExSpe_NoEmptyListOfDefs          = 7021203 - caught at read at 'SBase::checkListOfPopulated()', TODO: need update if core is revised.
// MultiLofOutBsts_AllowedAtts           = 7021204 - caught at read at 'OutwardBindingSite::readAttributes()'
// MultiLofOutBsts_AllowedElts           = 7021205 - caught at read at 'SBase::logUnknownElement()'
// MultiLofSpeFtrs_AllowedAtts           = 7021206 - caught at read at 'SpeciesFeature::readAttributes()'
// MultiSubLofSpeFtrs_AllowedMultiAtts   = 7021207 - caught at read at 'SubListOfSpeciesFeatures::readAttributes()'
// MultiSubLofSpeFtrs_RelationAtt_Ref    = 7021208 - caught at read at 'SubListOfSpeciesFeatures::readAttributes()'
// MultiLofSpeFtrs_AllowedElts           = 7021209 - caught at read at 'SBase::logUnknownElement()'
// MultiSubLofSpeFtrs_AllowedCoreAtts    = 7021210 - caught at read at 'SubListOfSpeciesFeatures::readAttributes()'
// MultiSubLofSpeFtrs_AllowedElts        = 7021211 - caught at read at 'SBase::logUnknownElement()'

// MultiSubLofSpeFtrs_CpoAtt_Ref         = 7021212
/*!< SubListOfSpeciesFeatures: 'component' must be the 'id' of a 'SpeciesType' component */

START_CONSTRAINT (MultiSubLofSpeFtrs_CpoAtt_Ref, SubListOfSpeciesFeatures, subListOfSpeciesFeatures)
{
  const MultiModelPlugin * mPlugin =
      dynamic_cast<const MultiModelPlugin*>(m.getPlugin("multi"));

  pre (mPlugin != 0);

  bool valid = !subListOfSpeciesFeatures.isSetComponent();

  if (!valid) {

    const std::string& subLofSpeFtrsComponentId = subListOfSpeciesFeatures.getComponent();

    const MultiSpeciesType * st = __getSpeciesTypeFromComponentId(m, subLofSpeFtrsComponentId);

    valid = (st != NULL);
  }

  inv(valid);
}
END_CONSTRAINT

// MultiExSpe_RestrictSpeciesTypeAtt     = 7021213
/*!< Extended Species: SpeciesType attribute must have value of the id of a speciesType */

START_CONSTRAINT (MultiExSpe_ReqSpt_LofOutBsts, Species, species)
{
  const MultiSpeciesPlugin * speciesPlugin =
      dynamic_cast<const MultiSpeciesPlugin*>(species.getPlugin("multi"));

  pre (speciesPlugin != 0);

  if (speciesPlugin->getListOfOutwardBindingSites()->size() > 0) {
      inv (speciesPlugin->isSetSpeciesType());
  }
}
END_CONSTRAINT


// MultiExSpe_ReqSpt_LofSpeFtrs     = 7021214
/*!< Extended Species: SpeciesType attribute must have value of the id of a speciesType */

START_CONSTRAINT (MultiExSpe_ReqSpt_LofSpeFtrs, Species, species)
{
  const MultiSpeciesPlugin * speciesPlugin =
        dynamic_cast<const MultiSpeciesPlugin*>(species.getPlugin("multi"));

    pre (speciesPlugin != 0);

    if (speciesPlugin->getListOfSpeciesFeatures()->size() > 0) {
        inv (speciesPlugin->isSetSpeciesType());
    }
}
END_CONSTRAINT


// MultiSubLofSpeFtrs_RelationAndOcc     = 7021215
/*!< SubListOfSpeciesFeatures: 'relation' can only be 'and' when referencing a speciesFeatureType with occur > 1  */

START_CONSTRAINT (MultiSubLofSpeFtrs_RelationAndOcc, SubListOfSpeciesFeatures, subListOfSpeciesFeatures)
{
  bool valid = true;

  if (subListOfSpeciesFeatures.isSetRelation() &&
    subListOfSpeciesFeatures.getRelation() != MULTI_RELATION_AND)
  {
    unsigned i;
    for (i = 0; valid && i < subListOfSpeciesFeatures.getNumSpeciesFeatures(); i++) {
      const SpeciesFeature * spf = subListOfSpeciesFeatures.get(i);
      const std::string& sftId = spf->getSpeciesFeatureType();
      const std::string& componentId = spf->getComponent();
      std::string& stId1 = const_cast<std::string&>(componentId);
      if (componentId.empty()) {
        const SBase * parent = subListOfSpeciesFeatures.getParentSBMLObject();
        if (dynamic_cast<const ListOfSpeciesFeatures *>(parent)) {
          parent = parent->getParentSBMLObject();

          const Species * sp =
              dynamic_cast<const Species *> (parent);

          if (sp != NULL) {
            const MultiSpeciesPlugin * speciesPlugin =
                  dynamic_cast<const MultiSpeciesPlugin*>(sp->getPlugin("multi"));
            if (speciesPlugin != NULL) {
              stId1 = speciesPlugin->getSpeciesType();
            }
          }
        }
      }

      const std::string& stId = stId1;
      const SpeciesFeatureType* sft = __getSpeciesTypeFromComponent(m, stId, sftId);

      if (sft && sft->getOccur() > 1) {
        valid = false;
      }
    }
  }

  inv(valid);
}
END_CONSTRAINT


// MultiSubLofSpeFtrs_RelationAndOcc     = 7021216
/*!< SubListOfSpeciesFeatures: must have at least two 'speciesFeatures'  */

START_CONSTRAINT (MultiSubLofSpeFtrs_TwoSpeFtrs, SubListOfSpeciesFeatures, subListOfSpeciesFeatures)
{
  inv(subListOfSpeciesFeatures.getNumSpeciesFeatures() > 1);
}
END_CONSTRAINT



//************************************
// Rules for OutwardBindingSite objects

// MultiOutBst_AllowedCoreAtts           = 7021301 - caught at read at 'OutwardBindingSite::readAttributes()'
// MultiOutBst_AllowedCoreElts           = 7021302 - caught at read at 'SBase::logUnknownElement()'
// MultiOutBst_AllowedMultiAtts          = 7021303 - caught at read at 'OutwardBindingSite::readAttributes()'
// MultiOutBst_BdgStaAtt_Ref             = 7021304 - caught at read at 'OutwardBindingSite::readAttributes()'

// MultiOutBst_CpoAtt_Ref                = 7021305
/*!< OutwardBindingSite: 'component' must be the 'id' of a 'BindingSiteSpeciesType' component */

START_CONSTRAINT (MultiOutBst_CpoAtt_Ref, OutwardBindingSite, outwardBindingSite)
{
  const MultiModelPlugin * mPlugin =
      dynamic_cast<const MultiModelPlugin*>(m.getPlugin("multi"));

  pre (mPlugin != 0);

  const std::string& bstComponentId = outwardBindingSite.getComponent();

  const MultiSpeciesType * st = __getSpeciesTypeFromComponentId(m, bstComponentId);
  const BindingSiteSpeciesType * bst =
      dynamic_cast<const BindingSiteSpeciesType*>(st);

  inv(bst != NULL);
}
END_CONSTRAINT

// MultiOutBst_NotInBond                = 7021306
/*!< OutwardBindingSite: An outwardBindingSite can not be in a bond of the species */

START_CONSTRAINT (MultiOutBst_NotInBond, OutwardBindingSite, outwardBindingSite)
{
  // compare only component ids in bonds, TODO: check all possible component ids of binding site in bonds
  const MultiModelPlugin * mPlugin =
      dynamic_cast<const MultiModelPlugin*>(m.getPlugin("multi"));

  pre (mPlugin != 0);

  std::string bstComponentId = outwardBindingSite.getComponent();

  const SBase * parent = outwardBindingSite.getParentSBMLObject()->getParentSBMLObject();
  const Species * species = dynamic_cast<const Species *> (parent);
  pre (species != 0);

  const MultiSpeciesPlugin * spPlugin =
      dynamic_cast<const MultiSpeciesPlugin *>(species->getPlugin("multi"));
  pre (spPlugin != 0);

  std::string stId = spPlugin->getSpeciesType();

  const MultiSpeciesType * st = mPlugin->getMultiSpeciesType(stId);
  pre (st != 0);

  bool found = false;
  for (unsigned int i = 0; !found && i < st->getNumInSpeciesTypeBonds(); i++) {
  const InSpeciesTypeBond * inSpeciesTypeBond = st->getInSpeciesTypeBond(i);
  std::string bst1 = inSpeciesTypeBond->getBindingSite1();
  inv(bst1 != bstComponentId);

  std::string bst2 = inSpeciesTypeBond->getBindingSite2();
  inv(bst2 != bstComponentId);
  }
}
END_CONSTRAINT

//************************************
// Rules for SpeciesFeature objects

// MultiSpeFtr_AllowedCoreAtts           = 7021401 - caught at read at 'SpeciesFeature::readAttributes()'
// MultiSpeFtr_AllowedCoreElts           = 7021402 - caught at read at 'SBase::logUnknownElement()'
// MultiSpeFtr_AllowedMultiAtts          = 7021403 - caught at read at 'SpeciesFeature::readAttributes()'

// MultiSpeFtr_SpeFtrTypAtt_Ref          = 7021404
/*!< SpeciesFeature: 'speciesFeatureType' must be the 'id' of a speciesFeatureType */

START_CONSTRAINT (MultiSpeFtr_SpeFtrTypAtt_Ref, SpeciesFeature, speciesFeature)
{
  const MultiModelPlugin * mPlugin =
      dynamic_cast<const MultiModelPlugin*>(m.getPlugin("multi"));

  pre (mPlugin != 0);

  std::string speciesFeatureTypeId = speciesFeature.getSpeciesFeatureType();

  bool found = false;

  for (unsigned int i = 0; !found && i < mPlugin->getNumMultiSpeciesTypes(); i++ ) {
      const MultiSpeciesType * speciesType = mPlugin->getMultiSpeciesType(i);
      for (unsigned int j = 0; !found && j < speciesType->getNumSpeciesFeatureTypes(); j++) {
          const SpeciesFeatureType * speciesFeatureType = speciesType->getSpeciesFeatureType(j);
          if (speciesFeatureType->getId() == speciesFeatureTypeId) {
              found = true;
          }
      }
  }

  inv(found == true);
}
END_CONSTRAINT

// MultiSpeFtr_OccAtt_Ref                = 7021405
/*!< SpeciesFeature: 'occur' must be a positiveInteger with restriction */

START_CONSTRAINT (MultiSpeFtr_OccAtt_Ref, SpeciesFeature, speciesFeature)
{
  const MultiModelPlugin * mPlugin =
      dynamic_cast<const MultiModelPlugin*>(m.getPlugin("multi"));

  pre (mPlugin != 0);

  std::string sftId = speciesFeature.getSpeciesFeatureType();
  unsigned int sfOccur = speciesFeature.getOccur();
  unsigned int sftOccur = 0;

  const SBase * sbaseListOfSpeciesFeatures = speciesFeature.getParentSBMLObject();
  pre (sbaseListOfSpeciesFeatures != 0);

  const SBase * sbaseSpecies = sbaseListOfSpeciesFeatures->getParentSBMLObject();
  pre (sbaseSpecies != 0);

  const Species * species = dynamic_cast<const Species *> (sbaseSpecies);
  if (species == NULL) { // possible from subListOfSpeciesFeatures?
      species = dynamic_cast<const Species *>(sbaseSpecies->getParentSBMLObject());
  }
  pre (species != 0);

  const MultiSpeciesPlugin * spPlugin =
      dynamic_cast<const MultiSpeciesPlugin*>(species->getPlugin("multi"));
  pre (spPlugin !=0);

  std::string sptId = spPlugin->getSpeciesType();

  bool found = false;
  for (unsigned int i = 0; !found && i < mPlugin->getNumMultiSpeciesTypes(); i++) {
      const MultiSpeciesType * spt = mPlugin->getMultiSpeciesType(i);
      if (sptId == spt->getId()) {
          for (unsigned int j = 0; !found && j < spt->getNumSpeciesFeatureTypes(); j++) {
              const SpeciesFeatureType * sft = spt->getSpeciesFeatureType(j);
              if (sftId == sft->getId()) {
                  found = true;
                  sftOccur = sft->getOccur();
              }
          }
      }
  }

  pre (found == true);

  inv (sftOccur >= sfOccur);
}
END_CONSTRAINT

// MultiSpeFtr_CpoAtt_Ref                = 7021406
/*< SpeciesFeature: 'component' must be the 'id' of a component */

START_CONSTRAINT (MultiSpeFtr_CpoAtt_Ref, SpeciesFeature, speciesFeature)
{
  if (speciesFeature.isSetComponent()) {
      std::string componentId = speciesFeature.getComponent();
      bool found = __isSpeciesTypeComponent(m, componentId);
      inv (found == true);
  }
}
END_CONSTRAINT

// MultiSpeFtr_RestrictElts              = 7021407
/*!< SpeciesFeature: Required one listOfSpeciesFeatureValues  */

START_CONSTRAINT (MultiSpeFtr_RestrictElts, SpeciesFeature, speciesFeature)
{
 // pre(m.getLevel() == 2);
 // inv (speciesFeature.getNumSpeciesFeatureValues() > 0);
}
END_CONSTRAINT

// MultiLofSpeFtrVals_NoEmpty            = 7021408 - caught at read at 'SBase::checkListOfPopulated()', TODO: need update if core is revised.
// MultiLofSpeFtrVals_AllowedAtts        = 7021409 - caught at read at 'SpeciesFeatureValue::readAttributes()'
// MultiLofSpeFtrVals_AllowedElts        = 7021410 - caught at read at 'SBase::logUnknownElement()'

//************************************
// Rules for SpeciesFeatureValue objects

// MultiSpeFtrVal_AllowedCoreAtts        = 7021501 - caught at read at 'SpeciesFeatureValue::readAttributes()'
// MultiSpeFtrVal_AllowedCoreElts        = 7021502 - caught at read at 'SBase::logUnknownElement()'
// MultiSpeFtrVal_AllowedMultiAtts       = 7021503 - caught at read at 'SpeciesFeatureValue::readAttributes()'

// MultiSpeFtrVal_ValAtt_Ref             = 7021504
/*!< SpeciesFeatureValue: 'value' must be the 'id' of a possibleSpeciesFeatureValue */

START_CONSTRAINT (MultiSpeFtrVal_ValAtt_Ref, SpeciesFeatureValue, speciesFeatureValue)
{
  const MultiModelPlugin * mPlugin =
      dynamic_cast<const MultiModelPlugin*>(m.getPlugin("multi"));

  pre (mPlugin != 0);

  std::string sfv_value = speciesFeatureValue.getValue();
  std::string sftId;

  const SBase * sbaseListOfSpeciesFeatureValues = speciesFeatureValue.getParentSBMLObject();
  const SBase * sbaseSpeciesFeature = NULL;
  if (sbaseListOfSpeciesFeatureValues) {
    sbaseSpeciesFeature = sbaseListOfSpeciesFeatureValues->getParentSBMLObject();
  }

  // speciesFeature
  const SpeciesFeature * speciesFeature =
      dynamic_cast<const SpeciesFeature *> (sbaseSpeciesFeature);

  const SBase * sbaseListOfSpeciesFeatures = NULL;
  if (speciesFeature) {
    sftId = speciesFeature->getSpeciesFeatureType();
    sbaseListOfSpeciesFeatures = sbaseSpeciesFeature->getParentSBMLObject();
  }

  const SBase * sbaseSpecies = NULL;
  if (sbaseListOfSpeciesFeatures) {
    sbaseSpecies = sbaseListOfSpeciesFeatures->getParentSBMLObject();
  }

  bool found = false;

  // species
  const Species * species = dynamic_cast<const Species *>(sbaseSpecies);
  if (species == NULL) { // possible from subListOfSpeciesFeatures?
      species = dynamic_cast<const Species *>(sbaseSpecies->getParentSBMLObject());
  }

  if (species) {
    const MultiSpeciesPlugin * spPlugin =
        dynamic_cast<const MultiSpeciesPlugin*>(species->getPlugin("multi"));
    if (spPlugin) {
      // if the speciesType is not set we will not find the reference
      // and may log a false error
      pre(spPlugin->isSetSpeciesType());
      std::string sptId = spPlugin->getSpeciesType();

      const SpeciesFeatureType * sft = __getSpeciesTypeFromComponent(m, sptId, sftId);

      if (sft) {
        const PossibleSpeciesFeatureValue * psfv = sft->getPossibleSpeciesFeatureValue(sfv_value);
        found = (psfv != NULL);
      }
    }
  }

  inv (found == true);
}
END_CONSTRAINT

//************************************
// Rules for IntraSpeciesReaction objects

// MultiIntSpeRec_AllowedAtts            = 7021601 - caught at read at 'IntraSpeciesReaction::readAttributes()'
// MultiIntSpeRec_AllowedCoreElts        = 7021602 - caught at read at 'SBase::logUnknownElement()'

//************************************
// Rules for extended SimpleSpeciesReference objects

// MultiExSplSpeRef_AllowedMultiAtts     = 7021701 - caught at read at 'MultiSimpleSpeciesReferencePlugin::readAttributes()'

// MultiExSplSpeRef_CpaRefAtt_Ref        = 7021702
/* !< Extended SimpleSpeciesReference: 'compartmentReference' must be the 'id' of a compartmentReference */

START_CONSTRAINT (MultiExSplSpeRef_CpaRefAtt_Ref, SpeciesReference, simpleSpeciesReference)
{

  const MultiSimpleSpeciesReferencePlugin * simpleSpeciesRefPlugin =
      dynamic_cast<const MultiSimpleSpeciesReferencePlugin*>(simpleSpeciesReference.getPlugin("multi"));

  pre (simpleSpeciesRefPlugin != 0);

  if (simpleSpeciesRefPlugin->isSetCompartmentReference()) {
      std::string compRefId = simpleSpeciesRefPlugin->getCompartmentReference();

      bool found = false;
      for (unsigned int i = 0; !found && i < m.getNumCompartments(); i++) {
          const Compartment * compartment = m.getCompartment(i);
          const MultiCompartmentPlugin * compartmentPlugin =
              dynamic_cast<const MultiCompartmentPlugin*>(compartment->getPlugin("multi"));
          if (compartmentPlugin != 0) {
              for (unsigned int j = 0; !found && j < compartmentPlugin->getNumCompartmentReferences(); j++) {
                  const CompartmentReference * compartmentRef = compartmentPlugin->getCompartmentReference(j);
                  if (compartmentRef->isSetId() && compartmentRef->getId() == compRefId) {
                      found = true;
                  }
              }
          }
      }

      inv (found == true);
  }
}
END_CONSTRAINT

//************************************
// Rules for extended SpeciesReference objects

// MultiLofSptCpoMapsInPro_NoEmpty       = 7021801 - caught at read at 'SBase::checkListOfPopulated()', TODO: need update if core is revised.
// MultiLofSptCpoMapsInPro_AllowedAtts   = 7021802 - caught at read at 'SpeciesTypeComponentMapInProduct::readAttributes()'
// MultiLofSptCpoMapsInPro_AllowedElts   = 7021803 - caught at read at 'SBase::logUnknownElement()'

//************************************
// Rules for SpeciesTypeComponentMapInProduct objects

// MultiSptCpoMapInPro_AllowedCoreAtts   = 7021901 - caught at read at 'SpeciesTypeComponentMapInProduct::readAttributes()'
// MultiSptCpoMapInPro_AllowedCoreElts   = 7021902 - caught at read at 'SBase::logUnknownElement()'
// MultiSptCpoMapInPro_AllowedMultiAtts  = 7021903 - caught at read at 'SpeciesTypeComponentMapInProduct::readAttributes()'

// MultiSptCpoMapInPro_RctAtt_Ref        = 7021904
/*!< SpeciesTypeComponentMapInProduct: 'reactant' must be the 'id' of a reactant speciesReference */

START_CONSTRAINT (MultiSptCpoMapInPro_RctAtt_Ref, SpeciesTypeComponentMapInProduct, speciesTypeComponentMapInProduct)
{
  std::string reactantId = speciesTypeComponentMapInProduct.getReactant();

  const SBase * sbaseListOfSpeciesTypeComponentMapsInProduct = speciesTypeComponentMapInProduct.getParentSBMLObject();
  pre(sbaseListOfSpeciesTypeComponentMapsInProduct != NULL);

  const SBase * sbaseSpeciesReference = sbaseListOfSpeciesTypeComponentMapsInProduct->getParentSBMLObject();
  pre(sbaseSpeciesReference != NULL);

  const SBase * sbaseListOfSpeciesReferences = sbaseSpeciesReference->getParentSBMLObject();
  pre (sbaseListOfSpeciesReferences != NULL);

  const SBase * sbaseReaction = sbaseListOfSpeciesReferences->getParentSBMLObject();
  const Reaction * reaction = dynamic_cast<const Reaction *> (sbaseReaction);
  pre (reaction != NULL);

  bool found = false;
  for (unsigned int i = 0; !found && i < reaction->getNumReactants(); i++) {
    const SpeciesReference * sRef = reaction->getReactant(i);
    if (sRef != 0 && sRef->isSetId() && sRef->getId() == reactantId) {
      found = true;
    }
  }

  inv(found == true);
}
END_CONSTRAINT

// MultiSptCpoMapInPro_RctCpoAtt_Ref     = 7021905
/*!< SpeciesTypeComponentMapInProduct: 'reactantComponent' must be the 'id' of a reactant component */

START_CONSTRAINT (MultiSptCpoMapInPro_RctCpoAtt_Ref, SpeciesTypeComponentMapInProduct, speciesTypeComponentMapInProduct)
{
  std::string reactantId = speciesTypeComponentMapInProduct.getReactant();
  std::string reactantComponentId = speciesTypeComponentMapInProduct.getReactantComponent();

  const SBase * sbaseListOfSpeciesTypeComponentMapsInProduct = speciesTypeComponentMapInProduct.getParentSBMLObject();
  pre (sbaseListOfSpeciesTypeComponentMapsInProduct != NULL);

  // parent of map list -- speciesReference
  const SBase * sbaseSpeciesReference = sbaseListOfSpeciesTypeComponentMapsInProduct->getParentSBMLObject();
  pre (sbaseSpeciesReference != NULL);

  // parent of speciesReference -- list
  const SBase * sbaseListOfSpeciesReferences = sbaseSpeciesReference->getParentSBMLObject();
  pre(sbaseListOfSpeciesReferences != NULL);

  const SBase * sbaseReaction = sbaseListOfSpeciesReferences->getParentSBMLObject();
  const Reaction * reaction = dynamic_cast<const Reaction *> (sbaseReaction);
  pre (reaction != NULL);

  // scan reactants
  bool foundReactant = false;
  bool good = false;
  for (unsigned int i = 0; !foundReactant && i < reaction->getNumReactants(); i++) {
    const SpeciesReference * sRef = reaction->getReactant(i);

    // reactant found
    if (sRef != 0 && sRef->isSetId() && sRef->getId() == reactantId) {
      foundReactant = true;

      // species
      const std::string speciesId = sRef->getSpecies();
      const Species * species = m.getSpecies(speciesId);
      pre (species != NULL);

      const MultiSpeciesPlugin * speciesPlugin =
            dynamic_cast<const MultiSpeciesPlugin*>(species->getPlugin("multi"));
      pre (speciesPlugin != NULL);

      std::string speciesTypeId = speciesPlugin->getSpeciesType();
      good = __isSpeciesTypeComponent(m, speciesTypeId, reactantComponentId);
    }
  }

  if (foundReactant) {
    inv(good == true);
  }
}
END_CONSTRAINT

// MultiSptCpoMapInPro_ProCpoAtt_Ref     = 7021906
/*!< SpeciesTypeComponentMapInProduct: 'productComponent' must be the 'id' of a product component */

START_CONSTRAINT (MultiSptCpoMapInPro_ProCpoAtt_Ref, SpeciesTypeComponentMapInProduct, speciesTypeComponentMapInProduct)
{
  std::string productComponentId = speciesTypeComponentMapInProduct.getProductComponent();

  // must have model extended
  const MultiModelPlugin * modelPlugin =
      dynamic_cast<const MultiModelPlugin*>(m.getPlugin("multi"));
  pre (modelPlugin != NULL);

  // parent of map -- list
  const SBase * sbaseListOfSpeciesTypeComponentMapsInProduct = speciesTypeComponentMapInProduct.getParentSBMLObject();
  pre (sbaseListOfSpeciesTypeComponentMapsInProduct != NULL);

  // parent of map list -- speciesReference
  const SBase * sbaseSpeciesReference = sbaseListOfSpeciesTypeComponentMapsInProduct->getParentSBMLObject();
  const SpeciesReference * speciesReference =
        dynamic_cast<const SpeciesReference*> (sbaseSpeciesReference);
  pre (speciesReference != NULL);

  std::string speciesId = speciesReference->getSpecies();
  const Species * species = m.getSpecies(speciesId);
  pre (species != NULL);

  const MultiSpeciesPlugin * speciesPlugin =
      dynamic_cast<const MultiSpeciesPlugin*>(species->getPlugin("multi"));
  pre (speciesPlugin != NULL);
  std::string speciesTypeId = speciesPlugin->getSpeciesType();
  inv( __isSpeciesTypeComponent(m, speciesTypeId, productComponentId));
}
END_CONSTRAINT

//************************************
// SK moved from 7020301
// Rules for CompartmentReference objects

// MultiCpaRef_AllowedCoreAtts           = 7022001 - caught at read at 'CompartmentReference::readAttributes()'
// MultiCpaRef_AllowedCoreElts           = 7022002 - caught at read at 'SBase::logUnknownElement()'
// MultiCpaRef_AllowedMultiAtts          = 7022003 - caught at read at 'CompartmentReference::readAttributes()'

// MultiCpaRef_CompartmentAtt_Ref        = 7022004
/*!< CompartmentReference: 'compartment' must be the 'id' of a compartment */

START_CONSTRAINT(MultiCpaRef_CompartmentAtt_Ref, CompartmentReference, compartmentReference)
{
  std::string compartmentId = compartmentReference.getCompartment();
  inv(m.getCompartment(compartmentId) != 0);
}
END_CONSTRAINT

// MultiCpaRef_IdRequiredOrOptional      = 7022005
/*!< CompartmentReference: 'multi:id' is required when referencing the same compartment */

START_CONSTRAINT(MultiCpaRef_IdRequiredOrOptional, Compartment, compartment)
{
  const MultiCompartmentPlugin * compPlug =
    dynamic_cast<const MultiCompartmentPlugin*>(compartment.getPlugin("multi"));

  pre(compPlug != 0);

  const ListOfCompartmentReferences * listOfCompartmentReferences = compPlug->getListOfCompartmentReferences();

  for (unsigned int i = 0; i < listOfCompartmentReferences->size(); i++) {
    const CompartmentReference * compartmentReference = listOfCompartmentReferences->get(i);
    std::string compartmentId = compartmentReference->getCompartment();

    for (unsigned int j = i + 1; j < listOfCompartmentReferences->size(); j++) {
      const CompartmentReference * anotherCompartmentReference = listOfCompartmentReferences->get(j);
      std::string anotherCompartmentId = anotherCompartmentReference->getCompartment();

      if (compartmentId == anotherCompartmentId) {
        inv(compartmentReference->isSetId() == true);
        inv(anotherCompartmentReference->isSetId() == true);
      }
    }
  }
}
END_CONSTRAINT

// MultiCpaRef_NoReferenceToAnyParent      = 7022006
/*!< CompartmentReference: A compartmentReference cannot reference any parent compartment */
START_CONSTRAINT(MultiCpaRef_NoReferenceToAnyParent, Compartment, compartment)
{
  const string & compartmentId = compartment.getId();
  bool hasCircularReference = __isReferencedByChildCompartment(&compartment, compartmentId);
  inv(hasCircularReference == false);
}
END_CONSTRAINT




//************************************
// Rules for extended ci elements in Math objects

//SK Moved to MathML consistency validator

//// MultiMathCi_AllowedMultiAtts          = 7022101 - caught at read and report error code 10201
//
//// MultiMathCi_SpeRefAtt_Ref             = 7022102
///*!< Math ci element: 'speciesReference' must be the 'id' of a speciesReference */
//EXTERN_CONSTRAINT (MultiMathCi_SpeRefAtt_Ref, MultiMathCiCheckSpeciesReference)
//
//// MultiMathCi_RepTypAtt_Ref             = 7022103
///*!< Math ci element: 'representationType' must be a value of the Multi data type 'RepresentationType' */
//EXTERN_CONSTRAINT (MultiMathCi_RepTypAtt_Ref, MultiMathCiCheckRepresentationType)
//
  /** @endcond */


