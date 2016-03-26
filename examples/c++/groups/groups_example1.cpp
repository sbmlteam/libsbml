/**
 * @file    example1.cpp
 * @brief   SBML Groups example
 * @author  Akiya Jouraku
 *
 * $Id: example1.cpp 13540 2011-04-08 22:12:58Z fbergmann $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/examples/groups/example1.cpp $
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

#include "sbml/SBMLTypes.h"
#include "sbml/packages/groups/common/GroupsExtensionTypes.h"

LIBSBML_CPP_NAMESPACE_USE

int main(int argc,char** argv){

//
// Creates an SBMLNamespaces object with the given SBML level, version
// package name, package version.
//
// (NOTE) By defualt, the name of package (i.e. "groups") will be used
// if the arugment for the prefix is missing or empty. Thus the argument
// for the prefix can be added as follows:
//
//    SBMLNamespaces sbmlns(3,1,"groups",1,"GROUP");
//

SBMLNamespaces sbmlns(3,1,"groups",1);

//
// (NOTES) The above code creating an SBMLNamespaces object can be replaced 
//         with one of the following other styles.
//
// (1) Creates an SBMLNamespace object with a SBML core namespace and then
//     adds a groups package namespace to the object. 
//
//         SBMLNamespaces sbmlns(3,1);
//         sbmlns.addPkgNamespace("groups",1);
//
//          OR
//
//         SBMLNamespaces sbmlns(3,1);
//         sbmlns.addNamespace(GroupsExtension::XmlnsL3V1V1,"groups");
//
// (2) Creates a GroupsPkgNamespaces object (SBMLNamespace derived class for
//     groups package. The class is basically used for createing an SBase derived
//     objects defined in the groups package) with the given SBML level, version, 
//     and package version
//
//        GroupsPkgNamespaces sbmlns(3,1,1);
//     


// create the document

SBMLDocument *document = new SBMLDocument(&sbmlns);
document->setPackageRequired("groups", false);

// create the Model

Model* model=document->createModel();

// create the Compartment

Compartment* compartment = model->createCompartment();
compartment->setId("cytosol");
compartment->setConstant(true);

compartment=model->createCompartment();
compartment->setId("mitochon");
compartment->setConstant(true);

// create the Species

Species* species = model->createSpecies();
species->setId("ATPc");
species->setCompartment("cytosol");
species->setInitialConcentration(1);
species->setHasOnlySubstanceUnits(false);
species->setBoundaryCondition(false);
species->setConstant(false);

species = model->createSpecies();
species->setId("ATPm");
species->setCompartment("mitochon");
species->setInitialConcentration(2);
species->setHasOnlySubstanceUnits(false);
species->setBoundaryCondition(false);
species->setConstant(false);

// create the Groups

//
// Get a GroupsModelPlugin object plugged in the model object.
//
// The type of the returned value of SBase::getPlugin() function is SBasePlugin*, and
// thus the value needs to be casted for the corresponding derived class. 
//
GroupsModelPlugin* mplugin = static_cast<GroupsModelPlugin*>(model->getPlugin("groups"));

//
// Creates a Group object via GroupsModelPlugin object.
//
Group* group = mplugin->createGroup();

group->setId("ATP");
group->setKind(GROUP_KIND_CLASSIFICATION);
group->setSBOTerm("SBO:0000252");

Member* member = group->createMember();
member->setIdRef("ATPc");

member = group->createMember();
member->setIdRef("ATPm");

writeSBML(document,"groups_example1.xml");
delete document;

}
