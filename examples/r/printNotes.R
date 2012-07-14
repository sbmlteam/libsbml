# 
# @file    printNotes.R
# @brief   Prints notes strings for each element
# @author  Frank Bergmann
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Usage: R --slave -f printNotes.R --args <full path to input file>
#
library(libSBML)


printNotes <- function(sb, id ) {
  
  if (!is.null(sb) && SBase_isSetNotes(sb)) {  
  cat("----- ",SBase_getElementName(sb),
      " (",id,") notes -----\n",
	  SBase_getNotesString(sb),"\n\n")
  }
}

args <- commandArgs(trailingOnly = TRUE)


if (length(args) != 1)
{
  stop("Usage: printNotes input-filename\n");
}

filename  = args[1];
document  = readSBML(filename);

errors = SBMLErrorLog_getNumFailsWithSeverity(
			SBMLDocument_getErrorLog(document), 
			enumToInteger("LIBSBML_SEV_ERROR", "_XMLErrorSeverity_t")
		 );
		 
cat("\n",filename,"\n\n");

if(errors > 0) {
  SBMLDocument_printErrors(document);
  q(status=errors);
}


# Model 

m = SBMLDocument_getModel(document);
printNotes(m, Model_getId(m));

for(i in seq_len(Model_getNumReactions(m))) {

  re = Model_getReaction( m, i-1);
  printNotes(re, Reaction_getId(re));

  # SpeciesReference (Reactant) 

  for(j in seq_len(Reaction_getNumReactants( re))) {
    rt =  Reaction_getReactant(re, j-1);
	
    if (SBase_isSetNotes( rt)) cat("   ");
    printNotes(rt, SimpleSpeciesReference_getSpecies( rt ) );
  }

  # SpeciesReference (Product) 

  for(j in seq_len(Reaction_getNumProducts( re ))) {
    rt = Reaction_getProduct( re, j-1);
    if (SBase_isSetNotes( rt)) cat("   ");
    printNotes(rt, SimpleSpeciesReference_getSpecies( rt ) );
  }

  # ModifierSpeciesReference (Modifiers) 

  for(j in seq_len(Reaction_getNumModifiers( re )))  {
    md = Reaction_getModifier(re, j-1);
    if (SBase_isSetNotes( md)) cat("   ");
    printNotes(md, SimpleSpeciesReference_getSpecies( md ) );
  }

  # KineticLaw 

  if(Reaction_isSetKineticLaw( re )) {
    kl = Reaction_getKineticLaw( re );
    if (SBase_isSetNotes( kl)) cat("   ");
    printNotes(kl, "");

    # Parameter 

    for(j in seq_len(KineticLaw_getNumParameters( kl ))) {
      pa = KineticLaw_getParameter( kl, j-1);
	  if (SBase_isSetNotes( pa)) cat("   ");
      printNotes(pa, Parameter_getId(pa));
    }
  }
  
}

# Species 

for(i in seq_len(Model_getNumSpecies(m))) {
  sp = Model_getSpecies(m, i-1);
  printNotes(sp, Species_getId(sp));
}

# Compartments 

for(i in seq_len(Model_getNumCompartments( m ))) {
  sp = Model_getCompartment(m, i-1);
  printNotes(sp, Compartment_getId(sp));
}

# FunctionDefinition 

for(i in seq_len(Model_getNumFunctionDefinitions(m))) {
  sp = Model_getFunctionDefinition(m, i-1);
  printNotes(sp, FunctionDefinition_getId(sp));
}

# UnitDefinition 

for(i in seq_len(Model_getNumUnitDefinitions(m))) {
  sp = Model_getUnitDefinition( m, i-1);
  printNotes(sp, UnitDefinition_getId(sp));
}

# Parameter 
for(i in seq_len(Model_getNumParameters( m ))) {
  sp = Model_getParameter( m, i-1);
  printNotes(sp, Parameter_getId(sp));
}

# Rule 

for(i in seq_len(Model_getNumRules( m ))) {
  sp = Model_getRule(m, i-1);
  printNotes(sp, "");
}

# InitialAssignment 

for(i in seq_len(Model_getNumInitialAssignments(m))) {
  sp = Model_getInitialAssignment(m, i-1);
  printNotes(sp, "");
}

# Event 

for(i in seq_len(Model_getNumEvents(m))) {
  sp = Model_getEvent(m, i-1);
  printNotes(sp, Event_getId(sp));

  # Trigger 
  if(Event_isSetTrigger( sp )) {
    tg = Event_getTrigger(sp);
    if (SBase_isSetNotes(  tg)) cat( "   " );
    printNotes(tg, "");
  }

  # Delay 

  if(Event_isSetDelay(sp))  {
    dl = Event_getDelay(sp);
    if (SBase_isSetNotes(  dl)) cat( "   " );
    printNotes( dl, "");
  }

  # EventAssignment 

  for(j in seq_len(Event_getNumEventAssignments(sp))) {
    ea = Event_getEventAssignment(sp, j-1);
    if (SBase_isSetNotes(  ea)) cat( "   " );      
    printNotes(ea, "");
  }
}

# SpeciesType 

for(i in seq_len(Model_getNumSpeciesTypes(m))) {
  sp = Model_getSpeciesType(m, i-1);
  printNotes(sp, SpeciesType_getId(sp));
}

# Constraints 

for(i in seq_len(Model_getNumConstraints(m))) {
  sp = Model_getConstraint(m, i-1);
  printNotes(sp, "");
}

q(status=errors);



