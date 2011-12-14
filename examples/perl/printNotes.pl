#!/usr/bin/env perl
# -*-Perl-*-
## 
## @file    printNotes.pl
## @brief   Prints notes strings for each element
## @author  Akiya Jouraku
## 
## This file is part of libSBML->  Please visit http://sbml->org for more
## information about SBML, and the latest version of libSBML->
## 


use LibSBML;
no strict;

sub printNotes {
  $sb = $_[0];
  $id = defined $_[1] ? $_[1] : '';

  if (not $sb->isSetNotes()) {
	return;        
  }
  $pid = "";
  
  if ($sb->isSetId()) {
      $pid = $sb->getId();
  }
  print("----- ", $sb->getElementName(), " (", $pid, ") notes -----", "\n");
  print($sb->getNotesString(), "\n");
  print("\n");
}

if ($#ARGV != 0) {
  print "Usage: printNotes filename\n";
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
printNotes($m);

for ($i = 0; $i < $m->getNumReactions(); $i++) {
    $re = $m->getReaction($i);
    printNotes($re);

    # SpeciesReference (Reacatant)  
    for ($j = 0; $j < $re->getNumReactants(); $j++) {
        $rt = $re->getReactant($j);
        if ($rt->isSetNotes()){
		print("     ");
        }
        printNotes($rt, $rt->getSpecies());
    }

    # SpeciesReference (Product)   
    for ($j = 0; $j < $re->getNumProducts(); $j++) {
        $rt = $re->getProduct($j);
        if ($rt->isSetNotes()){
		print("     ");
        }
        printNotes($rt, $rt->getSpecies());
    }

    # ModifierSpeciesReference (Modifiers)  
    for ($j = 0; $j < $re->getNumModifiers(); $j++) {
        $md = $re->getModifier($j);
        if (md->isSetNotes()) {
		print("     ");
        }
        printNotes($md, $md->getSpecies());
    }

    # KineticLaw   
    if ($re->isSetKineticLaw()) {
        $kl = $re->getKineticLaw();
        if ($kl->isSetNotes()) {
		print("   ");
        }
        printNotes($kl);

        # Parameter   
        for ($j = 0; $j < $kl->getNumParameters(); $j++) {
            $pa = $kl->getParameter($j);
            if ($pa->isSetNotes()) {
			print("      ");
            }
            printNotes($pa);
        }
    }
}
# Species 
for ($i = 0; $i < $m->getNumSpecies(); $i++) {
    $sp = $m->getSpecies($i);
    printNotes($sp);
}
# Compartments 
for ($i = 0; $i < $m->getNumCompartments(); $i++) {
    $sp = $m->getCompartment($i);
    printNotes($sp);
}
# FunctionDefinition 
for ($i=0; $i<$m->getNumFunctionDefinitions(); $i++) {
    $sp = $m->getFunctionDefinition($i);
    printNotes($sp);
}
# UnitDefinition 
for ($i=0; $i<$m->getNumUnitDefinitions(); $i++) {
    $sp = $m->getUnitDefinition($i);
    printNotes($sp);
}
# Parameter 
for ($i = 0; $i <$m->getNumParameters(); $i++) {
    $sp = $m->getParameter($i);
    printNotes($sp);
}
# Rule 
for ($i = 0; $i <$m->getNumRules(); $i++) {
    $sp = $m->getRule($i);
    printNotes($sp);
}
# InitialAssignment 
for ($i = 0; $i <$m->getNumInitialAssignments(); $i++) {
    $sp = $m->getInitialAssignment($i);
    printNotes($sp);
}
# Event 
for ($i = 0; $i <$m->getNumEvents(); $i++) {
    $sp = $m->getEvent($i);
    printNotes($sp);

    # Trigger 
    if ($sp->isSetTrigger()) {
        $tg = $sp->getTrigger();
        if ($tg->isSetNotes()) {
		print("   ");
        }
        printNotes($tg);
    }
    # Delay 
    if ($sp->isSetDelay()) {
        $dl = $sp->getDelay();
        if ($dl->isSetNotes()) {
		print("   ");
        }
        printNotes($dl);
    }
    # EventAssignment 
    for ($j = 0; $j <$sp->getNumEventAssignments(); $j++) {
        $ea = $sp->getEventAssignment($j);
        if ($ea->isSetNotes()) {
		print("   ");
        }
        printNotes($ea);
    }
}
# SpeciesType 
for ($i = 0; $i < $m->getNumSpeciesTypes(); $i++) {
    $sp = $m->getSpeciesType($i);
    printNotes($sp);
}
# Constraints 
for ($i = 0; $i <$m->getNumConstraints(); $i++) {
    $sp = $m->getConstraint($i);
    printNotes($sp);
}
exit $errors;
  
