/**
 * @file    spec_example1.c
 * @brief   SBML hierarchical composition example
 * @author  Lucian Smith
 * @author  Sarah Keating
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

/*#include <iostream>
#include <sstream>
*/
#include <sbml/SBMLTypes.h>
#include <sbml/extension/SBMLDocumentPlugin.h>


#include <sbml/extension/SBMLExtensionRegister.h>

#include <sbml/packages/comp/common/compfwd.h>
#include <sbml/packages/comp/extension/CompSBMLDocumentPlugin.h>
#include <sbml/packages/comp/extension/CompModelPlugin.h>
#include <sbml/packages/comp/sbml/Submodel.h>

LIBSBML_CPP_NAMESPACE_USE


int main(int argc,char** argv)
{
  int retval = 0;
  XMLNamespaces_t * comp;
  SBMLNamespaces_t * sbmlns;
  SBMLDocument_t * doc;
  Compartment_t * c;
  Species_t * s1;
  Species_t * s2;
  Species_t * s3;
  Species_t * s4;
  Reaction_t * r1;
  Reaction_t * r2;
  SpeciesReference_t *sr1;
  SpeciesReference_t *sr2;
  SpeciesReference_t *sr3;
  SpeciesReference_t *sr4;
  SpeciesReference_t *sr5;
  SpeciesReference_t *sr6;
  Model_t * model;
  CompSBMLDocumentPlugin_t * docPlug;
  CompModelPlugin_t *modelPlug;
  Submodel_t * sub1;
  Submodel_t * sub2;
  ModelDefinition_t * modDef;

  /* get comp registry entry */
  SBMLExtension_t *sbmlext = SBMLExtensionRegistry_getExtension("comp");

  /* create the sbml namespaces object with comp */
  comp = XMLNamespaces_create();
  XMLNamespaces_add(comp, SBMLExtension_getURI(sbmlext, 3, 1, 1), "comp");
  
  sbmlns = SBMLNamespaces_create(3, 1);
  SBMLNamespaces_addNamespaces(sbmlns, comp);

  /* create the document */
  doc = SBMLDocument_createWithSBMLNamespaces(sbmlns);

  /* set the comp reqd attribute to true */
  docPlug = (CompSBMLDocumentPlugin_t * )(SBase_getPlugin((SBase_t*)(doc), "comp"));
  SBMLDocumentPlugin_setRequired((SBMLDocumentPlugin_t*)(docPlug), 1);


  /* create the submodel */
  modDef = CompSBMLDocumentPlugin_createModelDefinition(docPlug);

  Model_setId((Model_t*)(modDef), "enzyme");
  Model_setName((Model_t*)(modDef), "enzyme");

  c = Model_createCompartment((Model_t*)(modDef));
  Compartment_setId(c, "comp");
  Compartment_setConstant(c, 1);
  Compartment_setSize(c, 1.0);
  Compartment_setSpatialDimensions(c, 3);

  s1 = Model_createSpecies((Model_t*)(modDef));
  Species_setId(s1, "S");
  Species_setCompartment(s1, "comp");
  Species_setHasOnlySubstanceUnits(s1, 0);
  Species_setBoundaryCondition(s1, 0);
  Species_setConstant(s1, 0);
  
  s2 = Model_createSpecies((Model_t*)(modDef));
  Species_setId(s2, "E");
  Species_setCompartment(s2, "comp");
  Species_setHasOnlySubstanceUnits(s2, 0);
  Species_setBoundaryCondition(s2, 0);
  Species_setConstant(s2, 0);
  
  s3 = Model_createSpecies((Model_t*)(modDef));
  Species_setId(s3, "D");
  Species_setCompartment(s3, "comp");
  Species_setHasOnlySubstanceUnits(s3, 0);
  Species_setBoundaryCondition(s3, 0);
  Species_setConstant(s3, 0);

  s4 = Model_createSpecies((Model_t*)(modDef));
  Species_setId(s4, "ES");
  Species_setCompartment(s4, "comp");
  Species_setHasOnlySubstanceUnits(s4, 0);
  Species_setBoundaryCondition(s4, 0);
  Species_setConstant(s4, 0);

  r1 = Model_createReaction((Model_t*)(modDef));
  Reaction_setId(r1, "J0");
  Reaction_setReversible(r1, 1);
  Reaction_setFast(r1, 0);

  sr1 = Reaction_createReactant(r1);
  SpeciesReference_setSpecies(sr1, "S");
  SpeciesReference_setStoichiometry(sr1, 1.0);
  SpeciesReference_setConstant(sr1, 1);


  sr2 = Reaction_createReactant(r1);
  SpeciesReference_setSpecies(sr2, "E");
  SpeciesReference_setStoichiometry(sr2, 1.0);
  SpeciesReference_setConstant(sr2, 1);

  sr3 = Reaction_createProduct(r1);
  SpeciesReference_setSpecies(sr3, "ES");
  SpeciesReference_setStoichiometry(sr3, 1.0);
  SpeciesReference_setConstant(sr3, 1);

  r2 = Model_createReaction((Model_t*)(modDef));
  Reaction_setId(r2, "J1");
  Reaction_setReversible(r2, 1);
  Reaction_setFast(r2, 0);

  sr4 = Reaction_createProduct(r2);
  SpeciesReference_setSpecies(sr4, "E");
  SpeciesReference_setStoichiometry(sr4, 1.0);
  SpeciesReference_setConstant(sr4, 1);

  sr5 = Reaction_createReactant(r2);
  SpeciesReference_setSpecies(sr5, "ES");
  SpeciesReference_setStoichiometry(sr5, 1.0);
  SpeciesReference_setConstant(sr5, 1);

  sr6 = Reaction_createProduct(r2);
  SpeciesReference_setSpecies(sr6, "D");
  SpeciesReference_setStoichiometry(sr6, 1.0);
  SpeciesReference_setConstant(sr6, 1);

  /* create the model */
  model = SBMLDocument_createModel(doc);
  Model_setId(model, "aggregate");

  /* add the submodels*/
  modelPlug = (CompModelPlugin_t*)(SBase_getPlugin((SBase_t *)(model), "comp"));

  sub1 = CompModelPlugin_createSubmodel(modelPlug);
  Submodel_setId(sub1, "submod1");
  Submodel_setModelRef(sub1, "enzyme");


  sub2 = CompModelPlugin_createSubmodel(modelPlug);
  Submodel_setId(sub2, "submod2");
  Submodel_setModelRef(sub2, "enzyme");

  /* write the file */
  writeSBMLToFile(doc, "enzyme_model.xml");

  return retval;
}
