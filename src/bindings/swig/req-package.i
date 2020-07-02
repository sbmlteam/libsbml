#ifdef USE_REQUIREDELEMENTS

%newobject removeChangedMath;

%template (ReqPkgNamespaces) SBMLExtensionNamespaces<ReqExtension>;

%include <sbml/packages/req/extension/ReqExtension.h>
%include <sbml/packages/req/extension/ReqSBasePlugin.h>
%include <sbml/packages/req/sbml/ChangedMath.h>

#endif /* USE_REQUIREDELEMENTS */

