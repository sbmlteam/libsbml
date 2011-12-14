#!/usr/bin/env perl
# -*-Perl-*-
## 
## @file    printAnnotation.pl
## @brief   Prints annotation strings for each element
## @author  Akiya Jouraku
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


use LibSBML;
no strict;
sub printAnnotation {
  $sb = $_[0];
  $id = defined $_[1] ? $_[1] : '';

  if (not $sb->isSetAnnotation()) {
	return;        
  }
  $pid = "";
  
  if ($sb->isSetId()) {
      $pid = $sb->getId();
  }
  print("----- ", $sb->getElementName(), " (", $pid, ") annotation -----", "\n");
  print($sb->getAnnotationString(), "\n");
  print("\n");
}

if ($#ARGV != 0) {
  print "Usage: printAnnotation filename\n";
  exit 1;
}

$filename = $ARGV[0];
$document = LibSBML::readSBML($filename);

$errors = $document->getNumErrors();

print("\n");
print("$filename: " , $filename , "\n");
print("\n");

if (errors > 0) {
    $document->printErrors();
    exit errors;
}

# Model

$m = $document->getModel();
printAnnotation($m);

for ($i = 0; $i < $m->getNumReactions(); $i++) {
    $re = $m->getReaction($i);
    printAnnotation($re);

    # SpeciesReference (Reacatant)  
    for ($j = 0; $j < $re->getNumReactants(); $j++) {
        $rt = $re->getReactant($j);
        if ($rt->isSetAnnotation()){
		print("     ");
        }
        printAnnotation($rt, $rt->getSpecies());
    }

    # SpeciesReference (Product)   
    for ($j = 0; $j < $re->getNumProducts(); $j++) {
        $rt = $re->getProduct($j);
        if ($rt->isSetAnnotation()){
		print("     ");
        }
        printAnnotation($rt, $rt->getSpecies());
    }

    # ModifierSpeciesReference (Modifiers)  
    for ($j = 0; $j < $re->getNumModifiers(); $j++) {
        $md = $re->getModifier($j);
        if (md->isSetAnnotation()) {
		print("     ");
        }
        printAnnotation($md, $md->getSpecies());
    }

    # KineticLaw   
    if ($re->isSetKineticLaw()) {
        $kl = $re->getKineticLaw();
        if ($kl->isSetAnnotation()) {
		print("   ");
        }
        printAnnotation($kl);

        # Parameter   
        for ($j = 0; $j < $kl->getNumParameters(); $j++) {
            $pa = $kl->getParameter($j);
            if ($pa->isSetAnnotation()) {
			print("      ");
            }
            printAnnotation($pa);
        }
    }
}
# Species 
for ($i = 0; $i < $m->getNumSpecies(); $i++) {
    $sp = $m->getSpecies($i);
    printAnnotation($sp);
}
# Compartments 
for ($i = 0; $i < $m->getNumCompartments(); $i++) {
    $sp = $m->getCompartment($i);
    printAnnotation($sp);
}
# FunctionDefinition 
for ($i=0; $i<$m->getNumFunctionDefinitions(); $i++) {
    $sp = $m->getFunctionDefinition($i);
    printAnnotation($sp);
}
# UnitDefinition 
for ($i=0; $i<$m->getNumUnitDefinitions(); $i++) {
    $sp = $m->getUnitDefinition($i);
    printAnnotation($sp);
}
# Parameter 
for ($i = 0; $i <$m->getNumParameters(); $i++) {
    $sp = $m->getParameter($i);
    printAnnotation($sp);
}
# Rule 
for ($i = 0; $i <$m->getNumRules(); $i++) {
    $sp = $m->getRule($i);
    printAnnotation($sp);
}
# InitialAssignment 
for ($i = 0; $i <$m->getNumInitialAssignments(); $i++) {
    $sp = $m->getInitialAssignment($i);
    printAnnotation($sp);
}
# Event 
for ($i = 0; $i <$m->getNumEvents(); $i++) {
    $sp = $m->getEvent($i);
    printAnnotation($sp);

    # Trigger 
    if ($sp->isSetTrigger()) {
        $tg = $sp->getTrigger();
        if ($tg->isSetAnnotation()) {
		print("   ");
        }
        printAnnotation($tg);
    }
    # Delay 
    if ($sp->isSetDelay()) {
        $dl = $sp->getDelay();
        if ($dl->isSetAnnotation()) {
		print("   ");
        }
        printAnnotation($dl);
    }
    # EventAssignment 
    for ($j = 0; $j <$sp->getNumEventAssignments(); $j++) {
        $ea = $sp->getEventAssignment($j);
        if ($ea->isSetAnnotation()) {
		print("   ");
        }
        printAnnotation($ea);
    }
}
# SpeciesType 
for ($i = 0; $i < $m->getNumSpeciesTypes(); $i++) {
    $sp = $m->getSpeciesType($i);
    printAnnotation($sp);
}
# Constraints 
for ($i = 0; $i <$m->getNumConstraints(); $i++) {
    $sp = $m->getConstraint($i);
    printAnnotation($sp);
}
exit $errors;
  
