%feature("docstring") Model "
 LibSBML implementation of SBML's Model construct.

 In an SBML model definition, a single object of class Model serves as
 the overall container for the lists of the various model components.
 All of the lists are optional, but if a given list container is
 present  within the model, the list must not be empty; that is, it
 must have  length one or more.  The following are the components and
 lists  permitted in different Levels and Versions of SBML

 
   *  In SBML Level 1, the components are: UnitDefinition,
 Compartment,  Species, Parameter, Rule, and Reaction.  Instances of
 the classes are  placed inside instances of classes
 ListOfUnitDefinitions,  ListOfCompartments, ListOfSpecies,
 ListOfParameters, ListOfRules, and  ListOfReactions.

 *  In SBML Level 2 Version 1, the components are: FunctionDefinition,
 UnitDefinition, Compartment, Species, Parameter, Rule, Reaction and
 Event.  Instances of the classes are placed inside instances of
 classes  ListOfFunctionDefinitions, ListOfUnitDefinitions,
 ListOfCompartments,  ListOfSpecies, ListOfParameters, ListOfRules,
 ListOfReactions, and  ListOfEvents.

 *  In SBML Level 2 Versions 2, 3 and 4, the components are:
 FunctionDefinition, UnitDefinition, CompartmentType, SpeciesType,
 Compartment, Species, Parameter, InitialAssignment, Rule, Constraint,
 Reaction and Event.  Instances of the classes are placed inside
 instances of classes ListOfFunctionDefinitions,
 ListOfUnitDefinitions,  ListOfCompartmentTypes, ListOfSpeciesTypes,
 ListOfCompartments,  ListOfSpecies, ListOfParameters,
 ListOfInitialAssignments, ListOfRules,  ListOfConstraints,
 ListOfReactions, and ListOfEvents.

 *  In SBML Level 3 Version 1, the components are: FunctionDefinition,
 UnitDefinition, Compartment, Species, Parameter, InitialAssignment,
 Rule, Constraint, Reaction and Event.  Instances of the classes are
 placed inside instances of classes ListOfFunctionDefinitions,
 ListOfUnitDefinitions, ListOfCompartments, ListOfSpecies,
 ListOfParameters, ListOfInitialAssignments, ListOfRules,
 ListOfConstraints, ListOfReactions, and ListOfEvents.

 Although all the lists are optional, there are dependencies between
 SBML  components such that defining some components requires defining
 others.  An example is that defining a species requires defining a
 compartment,  and defining a reaction requires defining a species.
 The dependencies  are explained in more detail in the SBML
 specifications.

 In addition to the above lists and attributes, the Model class in
 both  SBML Level 2 and Level 3 has the usual two attributes of 'id'
 and 'name', and both are optional.  As is the case for other SBML
 components with 'id' and 'name' attributes, they must be used
 according  to the guidelines described in the SBML specifications.
 (Within the  frameworks of SBML Level 2 and Level 3 Version 1 Core, a
 Model object identifier has no assigned meaning, but extension
 packages  planned for SBML Level 3 are likely to make use of this
 identifier.)

 Finally, SBML Level 3 has introduced a number of additional Model
 attributes.  They are discussed in a separate section below.

 Approaches to creating objects using the libSBML  API
 ======================================================================

 LibSBML provides two main mechanisms for creating objects: class
 constructors  (e.g., Species.Species() ),   and createObject()
 methods (such as Model.createSpecies()) provided by certain Object
 classes such as Model.  These  multiple mechanisms are provided by
 libSBML for flexibility and to  support different use-cases, but they
 also have different implications  for the overall model structure.

 In general, the recommended approach is to use the createObject()
 methods.  These  methods both create an object and link it to the
 parent in one step.  Here is an example: 
    # Create an SBMLDocument object in Level 3 Version 1 format:
    
      sbmlDoc = SBMLDocument(3, 1)
    
      # Create a Model object inside the SBMLDocument object and set
      # its identifier.  The call to setId() returns a status code
      # to indicate whether the assignment was successful.  Code 0
      # means success; see the documentation for Model\'s setId() for 
      # more information.
    
      model = sbmlDoc.createModel()
      model.setId(\"BestModelEver\")
    
      # Create a Species object inside the Model and set its identifier.
      # Again, the setId() returns a status code to indicate whether the
      # assignment was successful.  Code 0 means success; see the
      # documentation for Specie\'s setId() for more information.
    
      sp = model.createSpecies()
      sp.setId(\"BestSpeciesEver\")

  The createObject() methods return a  pointer to the object created,
 but they also add the object to the  relevant list of object
 instances contained in the parent.  (These lists  become the
 <listOfObjects> elements in the  finished XML rendition of SBML.)  In
 the example above,  Model.createSpecies() adds the created species
 directly to the

   *  list in the model.  Subsequently,  methods called on the species
 change the species in the model (which is  what is expected in most
 situations).

 Consistency and adherence to SBML specifications
 ======================================================================

 To make it easier for applications to do whatever they need,  libSBML
 is relatively lax when it comes to enforcing correctness and
 completeness of models during model construction and editing.
 Essentially, libSBML will not in most cases check automatically  that
 a model's components have valid attribute values, or that the
 overall model is consistent and free of errors -- even obvious errors
 such as duplication of identifiers.  This allows applications great
 leeway in how they build their models, but it means that software
 authors must take deliberate steps to ensure that the model will be,
 in  the end, valid SBML.  These steps include such things as keeping
 track  of the identifiers used in a model, manually performing
 updates in  certain situations where an entity is referenced in more
 than one place  (e.g., a species that is referenced by multiple
 SpeciesReference  objects), and so on.

 That said, libSBML does provide powerful features for deliberately
 performing validation of SBML when an application decides it is time
 to  do so.  The interfaces to these facilities are on the
 SBMLDocument  class, in the form of
 SBMLDocument.checkInternalConsistency() and
 SBMLDocument.checkConsistency().  Please refer to the documentation
 for  SBMLDocument for more information about this.

 While applications may play fast and loose and live like free spirits
 during the construction and editing of SBML models, they should
 always  make sure to call SBMLDocument.checkInternalConsistency()
 and/or  SBMLDocument.checkConsistency() before writing out the final
 version of  an SBML model.

 Model attributes introduced in SBML Level 3
 ======================================================================

 As mentioned above, the Model class has a number of optional
 attributes  in SBML Level 3 Version 1 Core.  These are
 'substanceUnits',  'timeUnits', 'volumeUnits', 'areaUnits',
 'lengthUnits', 'extentUnits',  and 'conversionFactor.  The following
 provide more information about  them.

 The 'substanceUnits' attribute
 ......................................................................

 The 'substanceUnits' attribute is used to specify the unit of
 measurement associated with substance quantities of Species objects
 that  do not specify units explicitly.  If a given Species object
 definition  does not specify its unit of substance quantity via the
 'substanceUnits'  attribute on the Species object instance, then that
 species inherits the  value of the Model 'substanceUnits' attribute.
 If the Model does not  define a value for this attribute, then there
 is no unit to inherit, and  all species that do not specify
 individual 'substanceUnits' attribute  values then have no declared
 units for their quantities.  The  SBML Level 3 Version 1 Core
 specification provides more  details.

 Note that when the identifier of a species appears in a model's
 mathematical expressions, the unit of measurement associated with
 that  identifier is not solely determined by setting 'substanceUnits'
 on Model or Species.  Please see the discussion about units given in
 the documentation for the Species class.

 The 'timeUnits' attribute
 ......................................................................

 The 'timeUnits' attribute on SBML Level 3's Model object is used to
 specify the unit in which time is measured in the model.  This
 attribute  on Model is the only way to specify a unit for time in a
 model.  It is a global attribute; time is measured in the model
 everywhere in  the same way.  This is particularly relevant to
 Reaction and RateRule  objects in a model: all Reaction and RateRule
 objects in SBML define  per-time values, and the unit of time is
 given by the 'timeUnits'  attribute on the Model object instance.  If
 the Model 'timeUnits'  attribute has no value, it means that the unit
 of time is not defined  for the model's reactions and rate rules.
 Leaving it unspecified in an  SBML model does not result in an
 invalid model in SBML Level 3;  however, as a matter of best
 practice, we strongly recommend that all  models specify units of
 measurement for time.

 The 'volumeUnits', 'areaUnits', and  'lengthUnits' attributes
 ......................................................................

 The attributes 'volumeUnits', 'areaUnits' and 'lengthUnits' together
 are  used to set the units of measurements for the sizes of
 Compartment  objects in an SBML Level 3 model when those objects do
 not  otherwise specify units.  The three attributes correspond to the
 most  common cases of compartment dimensions: 'volumeUnits' for
 compartments  having a 'spatialDimensions' attribute value of '3',
 'areaUnits' for  compartments having a 'spatialDimensions' attribute
 value of '2', and  'lengthUnits' for compartments having a
 'spatialDimensions' attribute  value of '1'.  The attributes are not
 applicable to compartments  whose 'spatialDimensions' attribute
 values are not one of '1', '2' or '3'.

 If a given Compartment object instance does not provide a value for
 its  'units' attribute, then the unit of measurement of that
 compartment's  size is inherited from the value specified by the
 Model 'volumeUnits',  'areaUnits' or 'lengthUnits' attribute, as
 appropriate based on the  Compartment object's 'spatialDimensions'
 attribute value.  If the Model  object does not define the relevant
 attribute, then there are no units  to inherit, and all Compartment
 objects that do not set a value for  their 'units' attribute then
 have no units associated with  their compartment sizes.

 The use of three separate attributes is a carry-over from SBML  Level
 2.  Note that it is entirely possible for a model to define a  value
 for two or more of the attributes 'volumeUnits', 'areaUnits' and
 'lengthUnits' simultaneously, because SBML models may contain
 compartments with different numbers of dimensions.

 The 'extentUnits' attribute
 ......................................................................

 Reactions are processes that occur over time.  These processes
 involve  events of some sort, where a single ``reaction event'' is
 one in which  some set of entities (known as reactants, products and
 modifiers in  SBML) interact, once.  The extent of a reaction is a
 measure of  how many times the reaction has occurred, while the time
 derivative of  the extent gives the instantaneous rate at which the
 reaction is  occurring.  Thus, what is colloquially referred to as
 the 'rate of the  reaction' is in fact equal to the rate of change of
 reaction extent.

 In SBML Level 3, the combination of 'extentUnits' and 'timeUnits'
 defines the units of kinetic laws in SBML and establishes how the
 numerical value of each KineticLaw object's mathematical formula is
 meant to be interpreted in a model.  The units of the kinetic laws
 are  taken to be 'extentUnits' divided by 'timeUnits'.

 Note that this embodies an important principle in SBML Level 3
 models: all reactions in an SBML model must have the same units  for
 the rate of change of extent.  In other words, the units of all
 reaction rates in the model must be the same.  There is only  one
 global value for 'extentUnits' and one global value for 'timeUnits'.

 The 'conversionFactor' attribute
 ......................................................................

 The attribute 'conversionFactor' in SBML Level 3's Model object
 defines a global value inherited by all Species object instances that
 do  not define separate values for their 'conversionFactor'
 attributes.  The  value of this attribute must refer to a Parameter
 object instance  defined in the model.  The Parameter object in
 question must be a  constant; ie it must have its 'constant'
 attribute value set to 'true'.

 If a given Species object definition does not specify a conversion
 factor via the 'conversionFactor' attribute on Species, then the
 species  inherits the conversion factor specified by the Model
 'conversionFactor'  attribute.  If the Model does not define a value
 for this attribute,  then there is no conversion factor to inherit.
 More information about  conversion factors is provided in the SBML
 Level 3 Version 1  specification.
";
