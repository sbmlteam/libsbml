/**
* @file    groups_example1.c
* @brief   Groups example
* @author  Sarah Keating
*
* <!--------------------------------------------------------------------------
* This sample program is distributed under a different license than the rest
* of libSBML.  This program uses the open-source MIT license, as follows:
*
* Copyright (c) 2013-2018 by the California Institute of Technology
* (California, USA), the European Bioinformatics Institute (EMBL-EBI, UK)
* and the University of Heidelberg (Germany), with support from the National
* Institutes of Health (USA) under grant R01GM070923.  All rights reserved.
*
* Permission is hereby granted, free of charge, to any person obtaining a
* copy of this software and associated documentation files (the "Software"),
* to deal in the Software without restriction, including without limitation
* the rights to use, copy, modify, merge, publish, distribute, sublicense,
* and/or sell copies of the Software, and to permit persons to whom the
* Software is furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
* DEALINGS IN THE SOFTWARE.
*
* Neither the name of the California Institute of Technology (Caltech), nor
* of the European Bioinformatics Institute (EMBL-EBI), nor of the University
* of Heidelberg, nor the names of any contributors, may be used to endorse
* or promote products derived from this software without specific prior
* written permission.
* ------------------------------------------------------------------------ -->
*/

#include <sbml/SBMLTypes.h>
#include <sbml/extension/SBMLDocumentPlugin.h>


#include <sbml/extension/SBMLExtensionRegister.h>

#include <sbml/packages/groups/common/groupsfwd.h>
#include <sbml/packages/groups/common/GroupsExtensionTypes.h>

#include <sbml/packages/groups/extension/GroupsExtension.h>




int main(int argc,char** argv)
{
  int retval = 0;
  XMLNamespaces_t * groups;
  SBMLNamespaces_t * sbmlns;
  SBMLDocument_t * doc;
  Compartment_t * compartment;
  Species_t * species;
  Model_t * model;
  SBMLDocumentPlugin_t * docPlug;
  SBasePlugin_t * modelPlug;
  Group_t * group;
  Member_t * member;

  /* get groups registry entry */
  SBMLExtension_t *sbmlext = SBMLExtensionRegistry_getExtension("groups");

  /* create the sbml namespaces object with groups */
  groups = XMLNamespaces_create();
  XMLNamespaces_add(groups, SBMLExtension_getURI(sbmlext, 3, 1, 1), "groups");
  
  sbmlns = SBMLNamespaces_create(3, 1);
  SBMLNamespaces_addNamespaces(sbmlns, groups);

  /* create the document */
  doc = SBMLDocument_createWithSBMLNamespaces(sbmlns);

  /* set the groups reqd attribute to false */
  docPlug = (SBMLDocumentPlugin_t*)(SBase_getPlugin((SBase_t*)(doc), "groups"));
  SBMLDocumentPlugin_setRequired(docPlug, 0);

  // create the Model

  model = SBMLDocument_createModel(doc);

  // create the Compartment

  compartment = Model_createCompartment(model);
  Compartment_setId(compartment, "cytosol");
  Compartment_setConstant(compartment, 1);
  Compartment_setSize(compartment, 1);

  compartment = Model_createCompartment(model);
  Compartment_setId(compartment, "mitochon");
  Compartment_setConstant(compartment, 1);
  Compartment_setSize(compartment, 1);

  // create the Species

  species = Model_createSpecies(model);
  Species_setId(species, "ATPc");
  Species_setCompartment(species, "cytosol");
  Species_setBoundaryCondition(species, 0);
  Species_setConstant(species, 0);
  Species_setHasOnlySubstanceUnits(species, 0);

  species = Model_createSpecies(model);
  Species_setId(species, "ATPm");
  Species_setCompartment(species, "mitochon");
  Species_setBoundaryCondition(species, 0);
  Species_setConstant(species, 0);
  Species_setHasOnlySubstanceUnits(species, 0);

  // create the Groups
  // Get a SBasePlugin_t object plugged in the model object.

  modelPlug = SBase_getPlugin((SBase_t *)(model), "groups");

  // create the group object and add it to model plugin

  group = Group_create(3, 1, 1);
  Group_setId(group, "ATP");
  Group_setKind(group, GROUP_KIND_CLASSIFICATION);
  SBase_setSBOTerm(group, 252);

  member = Group_createMember(group);
  Member_setIdRef(member, "ATPc");

  member = Group_createMember(group);
  Member_setIdRef(member, "ATPm");

  // we need a function here
  GroupsModelPlugin_addGroup((GroupsModelPlugin_t*)(modelPlug), group);

  /* write the file */
  writeSBMLToFile(doc, "groups_example1.xml");

  return retval;
}
