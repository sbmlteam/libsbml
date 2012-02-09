%feature("docstring") ListOfFunctionDefinitions "
 LibSBML implementation of SBML's ListOfFunctionDefinitions construct.

 The various ListOf___ classes in SBML are merely containers used for
 organizing the main components of an SBML model.  All are derived
 from  the abstract class SBase, and inherit the various attributes
 and  subelements of SBase, such as 'metaid' as and 'annotation'.  The
 ListOf___ classes do not add any attributes of their own.

 The relationship between the lists and the rest of an SBML model is
 illustrated by the following (for SBML Level 2 Version 4):

 <?xml version=\"1.0\" encoding=\"UTF-8\"?>
 <sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" 
       level=\"3\" version=\"1\">
   <model id=\"My_Model\">
     <listOfFunctionDefinitions>
       <functionDefinition> ... </functionDefinition> 
     </listOfFunctionDefinitions>
     <listOfUnitDefinitions>
       <unitDefinition> ... </unitDefinition> 
     </listOfUnitDefinitions>
     <listOfCompartments>
       <compartment> ... </compartment> 
     </listOfCompartments>
     <listOfSpecies>
       <species> ... </species> 
     </listOfSpecies>
     <listOfParameters>
       <parameter> ... </parameter> 
     </listOfParameters>
     <listOfInitialAssignments>
       <initialAssignment> ... </initialAssignment> 
     </listOfInitialAssignments>
     <listOfRules>
       ... elements of subclasses of Rule ...
     </listOfRules>
     <listOfConstraints>
       <constraint> ... </constraint> 
     </listOfConstraints>
     <listOfReactions>
       <reaction> ... </reaction> 
     </listOfReactions>
     <listOfEvents>
       <event> ... </event> 
     </listOfEvents>
   </model>
 </sbml>

  Readers may wonder about the motivations for using the ListOf___
 containers.  A simpler approach in XML might be to place the
 components  all directly at the top level of the model definition.
 The choice made  in SBML is to group them within XML elements named
 after  ListOfClassname, in part because it helps organize the
 components.  More importantly, the fact that the container classes
 are  derived from SBase means that software tools can add information
 about  the lists themselves into each list container's 'annotation'.

 See also ListOfFunctionDefinitions, ListOfUnitDefinitions,
 ListOfCompartmentTypes, ListOfSpeciesTypes, ListOfCompartments,
 ListOfSpecies, ListOfParameters, ListOfInitialAssignments,
 ListOfRules, ListOfConstraints, ListOfReactions, ListOfEvents.
";

