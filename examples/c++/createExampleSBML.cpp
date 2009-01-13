/**
 * @file    createExampleSBML.cpp
 * @brief   creates example SBML models presented in SBML specification.
 * @author  Akiya Jouraku
 *
 * $Id$
 * $HeadURL$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

#include <iostream>
#include <sbml/SBMLTypes.h>

using namespace std;

//
// Functions for creating the Example SBML documents.
//
SBMLDocument* createExampleEnzymaticReaction(); 
SBMLDocument* createExampleInvolvingUnits();

//
// Helper functions for writing/validation the created SBML document.
//
bool writeAndValidateExampleSBML(SBMLDocument *sbmlDoc, const string& filename);
bool validateExampleSBML(SBMLDocument *sbmlDoc);
bool writeExampleSBML(const SBMLDocument *sbmlDoc, const string& filename);

//
// These variables are used by SBMLWriter::setProgramName(ProgramName) and
// SBMLWriter::setProgramVersion(ProgramVersion) functions respectively when
// writing an SBML document.
//
const static string ProgramName    = "createExampleModels";
const static string ProgramVersion = "1.0.0";

//
// The SBML leve/version of example SBML models.
//
const static unsigned int Level   = 2;
const static unsigned int Version = 4;

//===============================================================================
//
// Main routine
//
//  Creates SBML models represented in "7 Example models expressed in XML using
//  SBML" in SBML Level 2 Version 4 Specification(*). 
//
//   (*) The specification is available at the following URL:
//       http://sbml.org/Documents/Specifications
//  
//  Please see createExampleXXXXXX() functions to know how to create example 
//  models by using libSBML APIs. 
//
//===============================================================================
//
int
main (int argc, char *argv[])
{
  SBMLDocument* sbmlDoc = 0;
  bool result = true;

  try
  {
    //-------------------------------------------------
    // 7.1 A Simple example application of SBML
    //-------------------------------------------------
    sbmlDoc = createExampleEnzymaticReaction(); 
    result  = writeAndValidateExampleSBML(sbmlDoc, "enzymaticreaction.xml");
    delete sbmlDoc;
    if (!result) return 1;
  }
  catch(std::bad_alloc& e)
  {
    cerr << e.what() << ": Failed to allocate heap buffer." << endl;
    return 1;
  }
  catch(...)
  {
    cerr << "Exception thrown." << endl;
    return 1;
  }


  return 0;
}


//===============================================================================
//
//
// Functions for creating the Example SBML documents.
//
//
//===============================================================================


/**
 *
 * Creates an SBML model represented in "7.1 A Simple example application of SBML"
 * in SBML Level 2 Version 4 Specification.
 *
 */
SBMLDocument* createExampleEnzymaticReaction()
{
  const unsigned int level   = Level;
  const unsigned int version = Version;

  //---------------------------------------------------------------------------
  //
  // Creates an SBMLDocument object 
  //
  //---------------------------------------------------------------------------

  SBMLDocument* sbmlDoc = new SBMLDocument(level,version);

  //---------------------------------------------------------------------------
  //
  // Creates a Model object inside the SBMLDocument object. 
  //
  //---------------------------------------------------------------------------

  Model* model = sbmlDoc->createModel();

  model->setName("EnzymaticReaction");

  //---------------------------------------------------------------------------
  //
  // Creates UnitDefinition objects inside the Model object.
  //
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------  
  // (UnitDefinition1) Creates an UnitDefinition object ("per_second")
  //---------------------------------------------------------------------------

  UnitDefinition* unitdef = model->createUnitDefinition();
  unitdef->setId("per_second");

  //  Creates an Unit inside the UnitDefinition object 

  Unit* unit = unitdef->createUnit();
  unit->setKind(UNIT_KIND_SECOND);
  unit->setExponent(-1);

  //--------------------------------------------------------------------------------
  // (UnitDefinition2) Creates an UnitDefinition object ("litre_per_mole_per_second") 
  //--------------------------------------------------------------------------------
    
  unitdef = model->createUnitDefinition();
  unitdef->setId("litre_per_mole_per_second");
    
  //  Creates an Unit inside the UnitDefinition object ("litre_per_mole_per_second")

  unit = unitdef->createUnit();
  unit->setKind(UNIT_KIND_MOLE);
  unit->setExponent(-1);

  //  Creates an Unit inside the UnitDefinition object ("litre_per_mole_per_second")

  unit = unitdef->createUnit();
  unit->setKind(UNIT_KIND_LITRE);
  unit->setExponent(1);

  //  Creates an Unit inside the UnitDefinition object ("litre_per_mole_per_second")

  unit = unitdef->createUnit();
  unit->setKind(UNIT_KIND_SECOND);
  unit->setExponent(-1);


  //---------------------------------------------------------------------------
  //
  // Creates a Compartment object inside the Model object. 
  //
  //---------------------------------------------------------------------------

  const string compName = "cytosol";

  // Creates a Compartment object ("cytosol")

  Compartment* comp = model->createCompartment();

  comp->setId(compName);
 
  // Sets the "size" attribute of the Compartment object.
  //
  //   The units of this Compartment object is the default SBML 
  //   units of volume (litre), and thus we don't have to explicitly invoke 
  //   setUnits("litre") function to set the default units.
  //
  comp->setSize(1e-14);


  //---------------------------------------------------------------------------
  //
  // Creates Species objects inside the Model object. 
  //
  //---------------------------------------------------------------------------
  
  //---------------------------------------------------------------------------
  // (Species1) Creates a Species object ("ES")
  //---------------------------------------------------------------------------

  Species* sp = model->createSpecies();

  // Sets the "compartment" attribute of the Species object to identify the 
  // compartnet in which the Species object located.
  sp->setCompartment(compName);

  sp->setId("ES");
  sp->setName("ES");

  // Sets the "initialAmount" attribute of the Species object.
  //
  //  The units of this Species object is determined by two attributes of this 
  //  Species object ("subStanceUnits" and "hasOnlySubstanceUnits") and the
  //  "spatialDimension" attribute of the Compartment object ("cytosol") in which 
  //  this species object located.
  //  Since the default values are used for "subStanceUnits" (substance (mole)) 
  //  and "hasOnlySubstanceUnits" (false) and the value of "spatialDimension" (3) 
  //  is greater than 0, the units of this Species object is  mole/litre . 
  //
  sp->setInitialAmount(0);

  //---------------------------------------------------------------------------
  // (Species2) Creates a Species object ("P")
  //---------------------------------------------------------------------------

  sp = model->createSpecies();
  sp->setCompartment(compName);
  sp->setId("P");
  sp->setName("P");
  sp->setInitialAmount(0);

  //---------------------------------------------------------------------------
  // (Species3) Creates a Species object ("S")
  //---------------------------------------------------------------------------

  sp = model->createSpecies();
  sp->setCompartment(compName);
  sp->setId("S");
  sp->setName("S");
  sp->setInitialAmount(1e-20);

  //---------------------------------------------------------------------------
  // (Species4) Creates a Species object ("E")
  //---------------------------------------------------------------------------

  sp = model->createSpecies();
  sp->setCompartment(compName);
  sp->setId("E");
  sp->setName("E");
  sp->setInitialAmount(5e-21);

  
  //---------------------------------------------------------------------------
  //
  // Creates Reaction objects inside the Model object. 
  //
  //---------------------------------------------------------------------------
  
  //---------------------------------------------------------------------------
  // (Reaction1) Creates a Reaction object ("veq").
  //---------------------------------------------------------------------------

  Reaction* reac = model->createReaction();
  reac->setId("veq");

  //---------------------------------------------------------------------------
  // Creates Reactant objects inside the Reaction object ("veq"). 
  //---------------------------------------------------------------------------

  // (Reactant1) Creates a Reactant object that references Species "E"
  // in the model.
  SpeciesReference* spr = reac->createReactant();
  spr->setSpecies("E");

  // (Reactant2) Creates a Reactant object that references Species "S"
  // in the model.
  spr = reac->createReactant();
  spr->setSpecies("S");

  //---------------------------------------------------------------------------
  // Creates a Product object inside the Reaction object ("veq"). 
  //---------------------------------------------------------------------------

  // Creates a Product object that references Species "ES" in the model. 
  spr = reac->createProduct();
  spr->setSpecies("ES");


  //---------------------------------------------------------------------------
  // Creates a KineticLaw object inside the Reaction object ("veq"). 
  //---------------------------------------------------------------------------
  
  KineticLaw* kl = reac->createKineticLaw();

  //---------------------------------------------------------------------------
  // Sets a math (ASTNode object) to the KineticLaw object.
  //---------------------------------------------------------------------------

  string mathXMLString = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">"
                         "  <apply>"
                         "    <times/>"
                         "    <ci> cytosol </ci>"
                         "    <apply>"
                         "      <minus/>"
                         "      <apply>"
                         "        <times/>"
                         "        <ci> kon </ci>"
                         "        <ci> E </ci>"
                         "        <ci> S </ci>"
                         "      </apply>"
                         "      <apply>"
                         "        <times/>"
                         "          <ci> koff </ci>"
                         "          <ci> ES </ci>"
                         "      </apply>"
                         "    </apply>"
                         "  </apply>"
                         "</math>";

  kl->setMath(readMathMLFromString(mathXMLString.c_str()));

  //---------------------------------------------------------------------------
  // Creates local Parameter objects inside the KineticLaw object.
  //---------------------------------------------------------------------------

  // Creates a Parameter ("kon")

  Parameter* para = kl->createParameter();
  para->setId("kon");
  para->setValue(1000000);
  para->setUnits("litre_per_mole_per_second");

  // Creates a Parameter ("koff")

  para = kl->createParameter();
  para->setId("koff");
  para->setValue(0.2);
  para->setUnits("per_second");


  //---------------------------------------------------------------------------
  // (Reaction2) Creates a Reaction object ("vcat") .
  //---------------------------------------------------------------------------
  
  reac = model->createReaction();
  reac->setId("vcat");
  reac->setReversible(false);

  //---------------------------------------------------------------------------
  // Creates Reactant objects inside the Reaction object ("vcat"). 
  //---------------------------------------------------------------------------

  // (Reactant1) Creates a Reactant object that references Species "ES" in the
  // model.
  spr = reac->createReactant();
  spr->setSpecies("ES");

  //---------------------------------------------------------------------------
  // Creates a Product object inside the Reaction object ("vcat"). 
  //---------------------------------------------------------------------------
  
  // (Product1) Creates a Product object that references Species "E" in the model.
  spr = reac->createProduct();
  spr->setSpecies("E");

  // (Product2) Creates a Product object that references Species "P" in the model.
  spr = reac->createProduct();
  spr->setSpecies("P");


  //---------------------------------------------------------------------------
  // Creates a KineticLaw object inside the Reaction object ("vcat"). 
  //---------------------------------------------------------------------------
  
  kl = reac->createKineticLaw();

  //---------------------------------------------------------------------------
  // Sets a math (ASTNode object) to the KineticLaw object.
  //---------------------------------------------------------------------------

  mathXMLString = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">"
                  "  <apply>"
                  "    <times/>"
                  "    <ci> cytosol </ci>"
                  "    <ci> kcat </ci>"
                  "    <ci> ES </ci>"
                  "  </apply>"
                  "</math>";
  kl->setMath(readMathMLFromString(mathXMLString.c_str()));

  //---------------------------------------------------------------------------
  // Creates local Parameter objects inside the KineticLaw object.
  //---------------------------------------------------------------------------

  // Creates a Parameter ("kcat")

  para = kl->createParameter();
  para->setId("kcat");
  para->setValue(0.1);
  para->setUnits("per_second");


  // Returns the created SBMLDocument object.
  // The returned object must be explicitly deleted by the caller,
  // otherwise memory leak will happen.

  return sbmlDoc;

}


/**
 *
 * Creates an SBML model represented in "7.2 Example involving units"
 * in SBML Specification.
 *
 *  (not implemented yet)
 *
 */
SBMLDocument* createExampleInvolvingUnits()
{
  const unsigned int level   = Level;
  const unsigned int version = Version;

  //---------------------------------------------------------------------------
  //
  // Creates an SBMLDocument object 
  //
  //---------------------------------------------------------------------------

  SBMLDocument* sbmlDoc = new SBMLDocument(level,version);

  //---------------------------------------------------------------------------
  //
  // Creates a Model object inside the SBMLDocument object. 
  //
  //---------------------------------------------------------------------------

  //Model* model = sbmlDoc->createModel();

  // Returns the created SBMLDocument object.
  // The returned object must be explicitly deleted by the caller,
  // otherwise memory leak will happen.

  return sbmlDoc;
}



//===============================================================================
//
//
// Helper functions for writing/validating the given SBML documents.
// 
//
//===============================================================================

/*
 *
 * Writes and Validates the given SBMLDocument.
 * (writeExampleSBML and validateExampleSBML functions are internally invoked).
 *
 */
bool writeAndValidateExampleSBML(SBMLDocument *sbmlDoc, const string& filename)
{
  if (!writeExampleSBML(sbmlDoc, filename) )
  {
    return false;
  }
  else if (!validateExampleSBML(sbmlDoc))
  {
    return false;
  } 
  return true;
}


/**
 *
 * Writes the given SBMLDocument to the given file.
 *
 */ 
bool writeExampleSBML(const SBMLDocument* sbmlDoc, const string& filename)
{
  SBMLWriter sbmlWriter;
  sbmlWriter.setProgramName(ProgramName);
  sbmlWriter.setProgramVersion(ProgramVersion);
 
  bool result = sbmlWriter.writeSBML(sbmlDoc, filename);

  if (result)
  {
    cout << "Created " << filename << endl;
    return true;
  }
  else
  {
    cerr << "Failed to write " << filename << endl;
    return false;
  }
}


/**
 *  
 *  Validates the given SBMLDocument.
 *
 *   This function is based on validateSBML.cpp implemented by
 *   Sarah Keating, Ben Bornstein, and Michael Hucka.
 *
 */
bool validateExampleSBML (SBMLDocument* sbmlDoc)
{
  if (!sbmlDoc)
  {
    cerr << "validateExamplSBML: the given SBML Document is 0" << endl;
    return false;
  }
 
  bool   seriousErrors = false;
  bool   noErrors      = true;
  string errorMessageRead;
  string errorMessageCC;
  unsigned int numReadErrors = 0;
  unsigned int numReadWarns  = 0;
  unsigned int numCCErrors   = 0;
  unsigned int numCCWarns    = 0;

  unsigned int numErrors = sbmlDoc->getNumErrors();

  if ( numErrors > 0 )
  {
    noErrors = false;
    for (unsigned int i=0; i < numErrors; i++)
    {
      const SBMLError* sbmlErr = sbmlDoc->getError(i);
      if ( sbmlErr->isFatal() || sbmlErr->isError() )
      {
        seriousErrors = true;
        ++numReadErrors;
      }
      else
      {
        ++numReadWarns;
      }
    }
    ostringstream oss;
    sbmlDoc->printErrors(oss);
    errorMessageRead = oss.str(); 
  }

  // If serious errors are encountered while reading an SBML document, it
  // does not make sense to go on and do full consistency checking because
  // the model may be nonsense in the first place.
 
  if (seriousErrors)
  {
    errorMessageRead += "Further consistency check and validation aborted."; 
  }
  else
  {
    numErrors = sbmlDoc->checkConsistency();
    if ( numErrors > 0 )
    {
      noErrors = false;
      for (unsigned int i=0; i < numErrors; i++)
      {
        const SBMLError* sbmlErr = sbmlDoc->getError(i);
        if ( sbmlErr->isFatal() || sbmlErr->isError() )
        {
          ++numCCErrors;
        }
        else
        {
          ++numCCWarns;
        }      
      } 
      ostringstream oss;
      sbmlDoc->printErrors(oss);
      errorMessageCC = oss.str(); 
    }
  }

  if (noErrors) return true;
  
  //
  // Prints errors/wranings (if any)
  //

  if ( numReadErrors > 0 )
  {
    cout << numReadErrors << " : validation error(s)" << endl;
  }
  if ( numReadWarns > 0 )
  {
    cout << numReadWarns << " : validation warning(s)" << endl;
  }
  cout << errorMessageRead;

  if ( numCCErrors > 0 )
  {
    cout << numCCErrors << " : consistency error(s)" << endl;
  }
  if ( numCCWarns > 0 )
  {
    cout << numCCWarns << " : consistency warning(s)" << endl;
  }
  cout << errorMessageCC;

  if ( numReadErrors > 0 || numCCErrors > 0 )
  {
    return false;
  }

  return true;
}



