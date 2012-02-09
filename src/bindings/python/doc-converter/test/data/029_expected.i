%feature("docstring") SBase "
 LibSBML implementation of SBase, the base class of most SBML objects.

 Most components in SBML are derived from a single abstract base type,
 SBase.  In addition to serving as the parent class for most other
 classes of objects in SBML, this base type is designed to allow a
 modeler or a software package to attach arbitrary information to each
 major element or list in an SBML model.

 SBase has an optional subelement called 'notes'.  It is intended to
 serve as a place for storing optional information intended to be seen
 by  humans.  An example use of the 'notes' element would be to
 contain  formatted user comments about the model element in which the
 'notes'  element is enclosed.  There are certain conditions on the
 XHTML content  permitted inside the 'notes' element; please consult
 the SBML  specification document corresponding to the SBML Level and
 Version  of your model for more information about the requirements
 for 'notes'  content.

 SBase has another optional subelement called 'annotation'.  Whereas
 the  'notes' element described above is a container for content to be
 shown  directly to humans, the 'annotation' element is a container
 for optional  software-generated content not meant to be shown to
 humans.  The  element's content type is XML type  'any', allowing
 essentially arbitrary data content.  SBML places  only a few
 restrictions on the organization of the content; these are  intended
 to help software tools read and write the data as well as help
 reduce conflicts between annotations added by different tools.  As is
 the case with 'notes', it is important to refer to the SBML
 specification document corresponding to the SBML Level and Version
 of your model for more information about the requirements for
 'annotation' content.

 It is worth pointing out that the 'annotation' element in the
 definition  of SBase exists in order that software developers may
 attach optional  application-specific data to the elements in an SBML
 model.  However, it  is important that this facility not be misused.
 In particular, it is  critical that data essential to a model
 definition or that can  be encoded in existing SBML elements is not
 stored in  'annotation'. Parameter values, functional dependencies
 between model  elements, etc., should not be recorded as annotations.
 It is crucial to  keep in mind the fact that data placed in
 annotations can be freely  ignored by software applications.  If such
 data affects the  interpretation of a model, then software
 interoperability is greatly  impeded.

 SBML Level 2 introduced an optional SBase attribute named 'metaid'
 for  supporting metadata annotations using RDF (Resource Description
 Format). The  attribute value has the data type XML ID, the XML
 identifier  type, which means each 'metaid' value must be globally
 unique within an  SBML file.  (Importantly, this uniqueness criterion
 applies across any  attribute with type XML  ID, not just the
 'metaid' attribute used by SBML -- something to  be aware of if your
 application-specific XML content inside the  'annotation' subelement
 happens to use XML ID.)  The 'metaid' value  serves to identify a
 model component for purposes such as referencing  that component from
 metadata placed within 'annotation' subelements.

 Beginning with SBML Level 2 Version 3, SBase also has an optional
 attribute named 'sboTerm' for supporting the use of the Systems
 Biology  Ontology.  In SBML proper, the data type of the attribute is
 a string of  the form 'SBO:NNNNNNN', where 'NNNNNNN' is a seven digit
 integer number;  libSBML simplifies the representation by only
 storing the 'NNNNNNN'  integer portion.  Thus, in libSBML, the
 'sboTerm' attribute on SBase has  data type int, and SBO identifiers
 are stored simply as integers.  (For convenience, SBase offers
 methods for returning both the integer  form and a text-string form
 of the SBO identifier.)  SBO terms are a  type of optional
 annotation, and each different class of SBML object  derived from
 SBase imposes its own requirements about the values  permitted for
 'sboTerm'.  Please consult the SBML Level 2  Version 4 specification
 for more information about the use of SBO  and the 'sboTerm'
 attribute.

 Finally, note that, in the list of methods on SBase, there is no
 public  constructor because SBase is an abstract class.  The
 constructors reside  in the subclasses derived from SBase.

 Standard format for annotations linking data  resources
 ======================================================================

 SBML Level 2 Versions 2, 3 and 4, and Level 3, define a proposed
 regular format for encoding two particular categories of annotations:
 (a) references to controlled vocabulary terms and database
 identifiers  which define and describe biological and biochemical
 entities in a  model; and (b) descriptions of the provenance of a
 model, including its  author(s) and modification history.
";
