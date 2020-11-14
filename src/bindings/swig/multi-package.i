#ifdef USE_MULTI

%newobject removePossibleSpeciesFeatureValue;
%newobject removeSpeciesFeatureValue;
%newobject removeCompartmentReference;
%newobject removeSpeciesTypeInstance;
%newobject removeInSpeciesTypeBond;
%newobject removeOutwardBindingSite;
%newobject removeSpeciesFeatureType;
%newobject removeSpeciesTypeComponentIndex;
%newobject removeSpeciesFeature;
%newobject removeSpeciesTypeComponentMapInProduct;
%newobject removeMultiSpeciesType;
%newobject removeSubListOfSpeciesFeatures;


%template (MultiPkgNamespaces) SBMLExtensionNamespaces<MultiExtension>;

%include <sbml/packages/multi/extension/MultiExtension.h>
%include <sbml/packages/multi/extension/MultiSBMLDocumentPlugin.h>
%include <sbml/packages/multi/extension/MultiModelPlugin.h>
%include <sbml/packages/multi/extension/MultiCompartmentPlugin.h>
%include <sbml/packages/multi/extension/MultiSpeciesPlugin.h>
%include <sbml/packages/multi/extension/MultiSimpleSpeciesReferencePlugin.h>
%include <sbml/packages/multi/extension/MultiSpeciesReferencePlugin.h>
%include <sbml/packages/multi/extension/MultiListOfReactionsPlugin.h>
%include <sbml/packages/multi/sbml/PossibleSpeciesFeatureValue.h>
%include <sbml/packages/multi/sbml/SpeciesFeatureValue.h>
%include <sbml/packages/multi/sbml/CompartmentReference.h>
%include <sbml/packages/multi/sbml/SpeciesTypeInstance.h>
%include <sbml/packages/multi/sbml/InSpeciesTypeBond.h>
%include <sbml/packages/multi/sbml/OutwardBindingSite.h>
%include <sbml/packages/multi/sbml/SpeciesFeatureType.h>
%include <sbml/packages/multi/sbml/SpeciesTypeComponentIndex.h>
%include <sbml/packages/multi/sbml/SpeciesFeature.h>
%include <sbml/packages/multi/sbml/SpeciesTypeComponentMapInProduct.h>
%include <sbml/packages/multi/sbml/SubListOfSpeciesFeatures.h>
%include <sbml/packages/multi/sbml/MultiSpeciesType.h>
%include <sbml/packages/multi/sbml/BindingSiteSpeciesType.h>
%include <sbml/packages/multi/sbml/IntraSpeciesReaction.h>

%include <sbml/packages/multi/extension/MultiASTPlugin.h>

#endif /* USE_MULTI */

