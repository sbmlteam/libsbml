#ifdef USE_DYN

%newobject removeDynElement;
%newobject removeSpatialComponent;

%template (DynPkgNamespaces) SBMLExtensionNamespaces<DynExtension>;

%include <sbml/packages/dyn/extension/DynExtension.h>
%include <sbml/packages/dyn/extension/DynCompartmentPlugin.h>
%include <sbml/packages/dyn/extension/DynEventPlugin.h>
%include <sbml/packages/dyn/extension/DynSBasePlugin.h>
%include <sbml/packages/dyn/sbml/DynElement.h>
%include <sbml/packages/dyn/sbml/SpatialComponent.h>

#endif /* USE_DYN */

