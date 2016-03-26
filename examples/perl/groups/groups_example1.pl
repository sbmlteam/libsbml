#!/usr/bin/perl
# -*-Perl-*-
#  
#  @file    example1.pl
#  @brief   SBML Groups example
#  @author  Akiya Jouraku
#  @author  Frank Bergmann (perl port)
#  
#  $Id: example1.pl 11938 2010-09-20 02:04:23Z mhucka $
#  $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/examples/groups/example1.pl $
#  
#  This file is part of libSBML.  Please visit http://sbml.org for more
#  information about SBML, and the latest version of libSBML.
#  

use blib '../../src/bindings/perl';
use LibSBML;
use strict;
# 
#  Creates an SBMLNamespaces object with the given SBML level, version
#  package name, package version.
# 
#  (NOTE) By defualt, the name of package (i.e. "groups") will be used
#  if the arugment for the prefix is missing or empty. Thus the argument
#  for the prefix can be added as follows:
# 
#     SBMLNamespaces sbmlns(3,1,"groups",1,"GROUP");
# 

my $sbmlns = new LibSBML::SBMLNamespaces(3,1,"groups",1);

# 
#  (NOTES) The above code creating an SBMLNamespaces object can be replaced 
#          with one of the following other styles.
# 
#  (1) Creates an SBMLNamespace object with a SBML core namespace and then
#      adds a groups package namespace to the object. 
# 
#          SBMLNamespaces sbmlns(3,1);
#          sbmlns.addPkgNamespace("groups",1);
# 
#           OR
# 
#          SBMLNamespaces sbmlns(3,1);
#          sbmlns.addNamespace(GroupsExtension::XmlnsL3V1V1,"groups");
# 
#  (2) Creates a GroupsPkgNamespaces object (SBMLNamespace derived class for
#      groups package. The class is basically used for createing an SBase derived
#      objects defined in the groups package) with the given SBML level, version, 
#      and package version
# 
#         GroupsPkgNamespaces sbmlns(3,1,1);
#      


#  create the document

my $document = new LibSBML::SBMLDocument($sbmlns);

#  create the Model

my $model=$document->createModel();

#  create the Compartment

my $compartment = $model->createCompartment();
$compartment->setId("cytosol");
$compartment->setConstant(1);

my $compartment=$model->createCompartment();
$compartment->setId("mitochon");
$compartment->setConstant(1);

#  create the Species

my $species = $model->createSpecies();
$species->setId("ATPc");
$species->setCompartment("cytosol");
$species->setInitialConcentration(1);
$species->setHasOnlySubstanceUnits(0);
$species->setBoundaryCondition(0);
$species->setConstant(0);

$species = $model->createSpecies();
$species->setId("ATPm");
$species->setCompartment("mitochon");
$species->setInitialConcentration(2);
$species->setHasOnlySubstanceUnits(0);
$species->setBoundaryCondition(0);
$species->setConstant(0);

#  create the Groups

# 
#  Get a GroupsModelPlugin object plugged in the model object.
# 
#  The type of the returned value of SBase::getPlugin() function is SBasePlugin, and
#  thus the value needs to be casted for the corresponding derived class. 
# 
my $mplugin = $model->getPlugin("groups");

# 
#  Creates a Group object via GroupsModelPlugin object.
# 
my $group = $mplugin->createGroup();

$group->setId("ATP");
$group->setKind("classification");
$group->setSBOTerm("SBO:0000252");

my $member = $group->createMember();
$member->setIdRef("ATPc");

$member = $group->createMember();
$member->setIdRef("ATPm");

LibSBML::writeSBML($document,"groups_example1-perl.xml");

