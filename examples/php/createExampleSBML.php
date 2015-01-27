<?php
include("libSBML.php");


// The SBML Level and Version of the example SBML models.
//
$Level   = 2;
$Version = 4;

/**
 *
 * Creates an SBML model represented in "7.1 A Simple example application of SBML"
 * in the SBML Level 2 Version 4 Specification.
 *
 */
function createExampleEnzymaticReaction() {

  global $Level, $Version;
  
  //---------------------------------------------------------------------------
  //
  // Creates an SBMLDocument object 
  //
  //---------------------------------------------------------------------------

  $sbmlDoc = new SBMLDocument($Level,$Version);

  //---------------------------------------------------------------------------
  //
  // Creates a Model object inside the SBMLDocument object. 
  //
  //---------------------------------------------------------------------------

  $model = $sbmlDoc->createModel();
  $model->setId("EnzymaticReaction");

  //---------------------------------------------------------------------------
  //
  // Creates UnitDefinition objects inside the Model object.
  //
  //---------------------------------------------------------------------------

  
  //---------------------------------------------------------------------------  
  // (UnitDefinition1) Creates an UnitDefinition object ("per_second")
  //---------------------------------------------------------------------------

  $unitdef = $model->createUnitDefinition();
  $unitdef->setId("per_second");

  //  Creates an Unit inside the UnitDefinition object 

  $unit = $unitdef->createUnit();
  $unit->setKind(UNIT_KIND_SECOND);
  $unit->setExponent(-1);

  //--------------------------------------------------------------------------------
  // (UnitDefinition2) Creates an UnitDefinition object ("litre_per_mole_per_second") 
  //--------------------------------------------------------------------------------
    
  // Note that we can reuse the pointers 'unitdef' and 'unit' because the
  // actual UnitDefinition object (along with the Unit objects within it)
  // is already attached to the Model object.

  $unitdef = $model->createUnitDefinition();
  $unitdef->setId("litre_per_mole_per_second");
    
  //  Creates an Unit inside the UnitDefinition object ("litre_per_mole_per_second")

  $unit = $unitdef->createUnit();
  $unit->setKind(UNIT_KIND_MOLE);
  $unit->setExponent(-1);

  //  Creates an Unit inside the UnitDefinition object ("litre_per_mole_per_second")

  $unit = $unitdef->createUnit();
  $unit->setKind(UNIT_KIND_LITRE);
  $unit->setExponent(1);

  //  Creates an Unit inside the UnitDefinition object ("litre_per_mole_per_second")

  $unit = $unitdef->createUnit();
  $unit->setKind(UNIT_KIND_SECOND);
  $unit->setExponent(-1);


  //---------------------------------------------------------------------------
  //
  // Creates a Compartment object inside the Model object. 
  //
  //---------------------------------------------------------------------------

  $compName = "cytosol";

  // Creates a Compartment object ("cytosol")

  $comp = $model->createCompartment();
  $comp->setId($compName);
 
  // Sets the "size" attribute of the Compartment object.
  //
  // We are not setting the units on the compartment size explicitly, so
  // the units of this Compartment object will be the default SBML units of
  // volume, which are liters.
  //
  $comp->setSize(1e-14);


  //---------------------------------------------------------------------------
  //
  // Creates Species objects inside the Model object. 
  //
  //---------------------------------------------------------------------------
  
  //---------------------------------------------------------------------------
  // (Species1) Creates a Species object ("ES")
  //---------------------------------------------------------------------------

  // Create the Species objects inside the Model object. 

  $sp = $model->createSpecies();
  $sp->setId("ES");
  $sp->setName("ES");

  // Sets the "compartment" attribute of the Species object to identify the 
  // compartment in which the Species object is located.

  $sp->setCompartment($compName);

  // Sets the "initialAmount" attribute of the Species object.
  //
  //  In SBML, the units of a Species object's initial quantity are
  //  determined by two attributes, "substanceUnits" and
  //  "hasOnlySubstanceUnits", and the "spatialDimensions" attribute
  //  of the Compartment object ("cytosol") in which the species
  //  object is located.  Here, we are using the default values for
  //  "substanceUnits" (which is "mole") and "hasOnlySubstanceUnits"
  //  (which is "false").  The compartment in which the species is
  //  located uses volume units of liters, so the units of these
  //  species (when the species appear in numerical formulas in the
  //  model) will be moles/liters.  
  //
  $sp->setInitialAmount(0);

  //---------------------------------------------------------------------------
  // (Species2) Creates a Species object ("P")
  //---------------------------------------------------------------------------

  $sp = $model->createSpecies();
  $sp->setCompartment($compName);
  $sp->setId("P");
  $sp->setName("P");
  $sp->setInitialAmount(0);

  //---------------------------------------------------------------------------
  // (Species3) Creates a Species object ("S")
  //---------------------------------------------------------------------------

  $sp = $model->createSpecies();
  $sp->setCompartment($compName);
  $sp->setId("S");
  $sp->setName("S");
  $sp->setInitialAmount(1e-20);

  //---------------------------------------------------------------------------
  // (Species4) Creates a Species object ("E")
  //---------------------------------------------------------------------------

  $sp = $model->createSpecies();
  $sp->setCompartment($compName);
  $sp->setId("E");
  $sp->setName("E");
  $sp->setInitialAmount(5e-21);

  
  //---------------------------------------------------------------------------
  //
  // Creates Reaction objects inside the Model object. 
  //
  //---------------------------------------------------------------------------
  
 
  //---------------------------------------------------------------------------
  // (Reaction1) Creates a Reaction object ("veq").
  //---------------------------------------------------------------------------

  $reaction = $model->createReaction();
  $reaction->setId("veq");

  // (Reactant1) Creates a Reactant object that references Species "E"
  // in the model.  The object will be created within the reaction in the
  // SBML <listOfReactants>.

  $spr = $reaction->createReactant();
  $spr->setSpecies("E");

  // (Reactant2) Creates a Reactant object that references Species "S"
  // in the model.

  $spr = $reaction->createReactant();
  $spr->setSpecies("S");

  //---------------------------------------------------------------------------
  // (Product1) Creates a Product object that references Species "ES" in
  // the model.
  //---------------------------------------------------------------------------

  $spr = $reaction->createProduct();
  $spr->setSpecies("ES");

  //---------------------------------------------------------------------------
  // Creates a KineticLaw object inside the Reaction object ("veq"). 
  //---------------------------------------------------------------------------

  $kl = $reaction->createKineticLaw();

   //---------------------------------------------------------------------------
   // Creates an ASTNode object which represents the following math of the
   // KineticLaw.
   //
   //      <math xmlns="http://www.w3.org/1998/Math/MathML">
   //        <apply>
   //          <times/>
   //          <ci> cytosol </ci>
   //          <apply>
   //            <minus/>
   //            <apply>
   //              <times/>
   //              <ci> kon </ci>
   //              <ci> E </ci>
   //              <ci> S </ci>
   //            </apply>
   //            <apply>
   //              <times/>
   //              <ci> koff </ci>
   //              <ci> ES </ci>
   //            </apply>
   //          </apply>
   //        </apply>
   //      </math>
   //
   //---------------------------------------------------------------------------

   //------------------------------------------
   //
   // create nodes representing the variables
   //
   //------------------------------------------

   $astCytosol = new ASTNode(AST_NAME);
   $astCytosol->setName("cytosol");

   $astKon = new ASTNode(AST_NAME);
   $astKon->setName("kon");

   $astKoff = new ASTNode(AST_NAME);
   $astKoff->setName("koff");

   $astE = new ASTNode(AST_NAME);
   $astE->setName("E");

   $astS = new ASTNode(AST_NAME);
   $astS->setName("S");

   $astES = new ASTNode(AST_NAME);
   $astES->setName("ES");


   //--------------------------------------------
   //
   // create node representing
   //            <apply>
   //              <times/>
   //              <ci> koff </ci>
   //              <ci> ES </ci>
   //            </apply>
   //
   //--------------------------------------------

   $astTimes1 = new ASTNode(AST_TIMES);
   $astTimes1->addChild($astKoff);
   $astTimes1->addChild($astES);

   //--------------------------------------------
   //
   // create node representing
   //            <apply>
   //              <times/>
   //              <ci> kon </ci>
   //              <ci> E </ci>
   //              <ci> S </ci>
   //            </apply>
   //
   //
   // (NOTES)
   //
   //  Since there is a restriction with an ASTNode of "<times/>" operation
   //  such that the ASTNode is a binary class and thus only two operands can
   //  be directly added, the following code in this comment block is invalid
   //  because the code directly adds three <ci> ASTNodes to <times/> ASTNode.
   //
   //    ASTNode *astTimes = new ASTNode(AST_TIMES);
   //    astTimes->addChild(astKon);
   //    astTimes->addChild(astE);
   //    astTimes->addChild(astS);
   //
   // The following valid code after this comment block creates the ASTNode
   // as a binary tree.
   //
   // Please see "Converting between ASTs and text strings" described
   // at http://sbml.org/Software/libSBML/docs/cpp-api/class_a_s_t_node.html
   // for the detailed information.
   //
   //--------------------------------------------

   $astTimes2 = new ASTNode(AST_TIMES);
   $astTimes2->addChild($astE);
   $astTimes2->addChild($astS);

   $astTimes = new ASTNode(AST_TIMES);
   $astTimes->addChild($astKon);
   $astTimes->addChild($astTimes2);

   //--------------------------------------------
   //
   // create node representing
   //          <apply>
   //            <minus/>
   //            <apply>
   //              <times/>
   //              <ci> kon </ci>
   //              <ci> E </ci>
   //              <ci> S </ci>
   //            </apply>
   //            <apply>
   //              <times/>
   //              <ci> koff </ci>
   //              <ci> ES </ci>
   //            </apply>
   //          </apply>
   //
   //--------------------------------------------

   $astMinus = new ASTNode(AST_MINUS);
   $astMinus->addChild($astTimes);
   $astMinus->addChild($astTimes1);


   //--------------------------------------------
   //
   // create node representing
   //        <apply>
   //          <times/>
   //          <ci> cytosol </ci>
   //          <apply>
   //            <minus/>
   //            <apply>
   //              <times/>
   //              <ci> kon </ci>
   //              <ci> E </ci>
   //              <ci> S </ci>
   //            </apply>
   //            <apply>
   //              <times/>
   //              <ci> koff </ci>
   //              <ci> ES </ci>
   //            </apply>
   //          </apply>
   //        </apply>
   //
   //--------------------------------------------

   $astMath = new ASTNode(AST_TIMES);
   $astMath->addChild($astCytosol);
   $astMath->addChild($astMinus);

   //---------------------------------------------
   //
   // set the Math element
   //
   //------------------------------------------------

   $kl->setMath($astMath);

  //---------------------------------------------------------------------------
  // Creates local Parameter objects inside the KineticLaw object.
  //---------------------------------------------------------------------------

  // Creates a Parameter ("kon")

  $para = $kl->createParameter();
  $para->setId("kon");
  $para->setValue(1000000);
  $para->setUnits("litre_per_mole_per_second");

  // Creates a Parameter ("koff")

  $para = $kl->createParameter();
  $para->setId("koff");
  $para->setValue(0.2);
  $para->setUnits("per_second");


  //---------------------------------------------------------------------------
  // (Reaction2) Creates a Reaction object ("vcat") .
  //---------------------------------------------------------------------------
  
  $reaction = $model->createReaction();
  $reaction->setId("vcat");
  $reaction->setReversible(false);

  //---------------------------------------------------------------------------
  // Creates Reactant objects inside the Reaction object ("vcat"). 
  //---------------------------------------------------------------------------

  // (Reactant1) Creates a Reactant object that references Species "ES" in the
  // model.

  $spr = $reaction->createReactant();
  $spr->setSpecies("ES");

  //---------------------------------------------------------------------------
  // Creates a Product object inside the Reaction object ("vcat"). 
  //---------------------------------------------------------------------------
  
  // (Product1) Creates a Product object that references Species "E" in the model.

  $spr = $reaction->createProduct();
  $spr->setSpecies("E");

  // (Product2) Creates a Product object that references Species "P" in the model.

  $spr = $reaction->createProduct();
  $spr->setSpecies("P");

  //---------------------------------------------------------------------------
  // Creates a KineticLaw object inside the Reaction object ("vcat"). 
  //---------------------------------------------------------------------------
  
  $kl = $reaction->createKineticLaw();

  //---------------------------------------------------------------------------
  // Sets a math (ASTNode object) to the KineticLaw object.
  //---------------------------------------------------------------------------

  // To create mathematical expressions, one would typically construct
  // an ASTNode tree as the above example code which creates a math of another
  // KineticLaw object.  Here, to save some space and illustrate another approach 
  // of doing it, we will write out the formula in MathML form and then use a 
  // libSBML convenience function to create the ASTNode tree for us.  
  // (This is a bit dangerous; it's very easy to make mistakes when writing MathML 
  // by hand, so in a real program, we would not really want to do it this way.)

  $mathXMLString = '<math xmlns="http://www.w3.org/1998/Math/MathML">
                           <apply>
                             <times/>
                             <ci> cytosol </ci>
                             <ci> kcat </ci>
                             <ci> ES </ci>
                           </apply>
                         </math>';

  $astMath = libsbml::readMathMLFromString($mathXMLString);

  $kl->setMath($astMath);
  
  //---------------------------------------------------------------------------
  // Creates local Parameter objects inside the KineticLaw object.
  //---------------------------------------------------------------------------

  // Creates a Parameter ("kcat")

  $para = $kl->createParameter();
  $para->setId("kcat");
  $para->setValue(0.1);
  $para->setUnits("per_second");


  // Returns the created SBMLDocument object.
  // The returned object must be explicitly deleted by the caller,
  // otherwise a memory leak will happen.

  return $sbmlDoc;

}

// create example model
$sbmlDoc = createExampleEnzymaticReaction(); 

// check consistency
$sbmlDoc->checkConsistency();
if ($sbmlDoc->getErrorLog()->getNumFailsWithSeverity(LIBSBML_SEV_ERROR) > 0) {
  echo "file is not valid!";
  $sbmlDoc->printErrors();
}

// write file
$writer = new SBMLWriter();
$writer->writeSBML($sbmlDoc, "enzymaticreaction.xml");

?>

