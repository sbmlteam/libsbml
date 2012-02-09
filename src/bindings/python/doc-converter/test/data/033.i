%feature("docstring") ListOfFunctionDefinitions "
 LibSBML implementation of SBML\'s %ListOfFunctionDefinitions construct.
 
 The various ListOf___ classes in %SBML are merely containers used for
 organizing the main components of an %SBML model.  All are derived from
 the abstract class SBase, and inherit the various attributes and
 subelements of SBase, such as \'metaid\' as and \'annotation\'.  The
 ListOf___ classes do not add any attributes of their own.

 The relationship between the lists and the rest of an %SBML model is
 illustrated by the following (for %SBML Level&nbsp;2 Version&nbsp;4):

 @image html listof-illustration.jpg \'ListOf___ elements in an SBML Model\'
 @image latex listof-illustration.jpg \'ListOf___ elements in an SBML Model\'

 Readers may wonder about the motivations for using the ListOf___
 containers.  A simpler approach in XML might be to place the components
 all directly at the top level of the model definition.  The choice made
 in SBML is to group them within XML elements named after
 ListOf<em>Classname</em>, in part because it helps organize the
 components.  More importantly, the fact that the container classes are
 derived from SBase means that software tools can add information @em about
 the lists themselves into each list container\'s \'annotation\'.

 @see ListOfFunctionDefinitions
 @see ListOfUnitDefinitions
 @see ListOfCompartmentTypes
 @see ListOfSpeciesTypes
 @see ListOfCompartments
 @see ListOfSpecies
 @see ListOfParameters
 @see ListOfInitialAssignments
 @see ListOfRules
 @see ListOfConstraints
 @see ListOfReactions
 @see ListOfEvents
";

