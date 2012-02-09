%feature("docstring") SBMLExtension "
 The core component of SBML\'s package extension.

 SBMLExtension class (abstract class) is a core component of package extension
 which needs to be extended by package developers. 
 The class provides functions for getting common attributes of package extension 
 (e.g., package name, package version, and etc.), functions for adding (registering) 
 each instantiated SBasePluginCreator object, and a static function (defined in each 
 SBMLExtension extended class) for initializing/registering the package extension 
 when the library of the package is loaded.

 @section howto How to implement an SBMLExtension extended class for each package extension

 Package developers must implement an SBMLExtension extended class for
 their packages (e.g. GroupsExtension class is implemented for groups extension).
 The extended class is implemented based on the following steps:
";
