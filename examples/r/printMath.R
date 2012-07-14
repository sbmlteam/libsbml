# 
# @file    printMath.R
# @brief   Prints Rule, Reaction, and Event formulas in a given SBML Document
# @author  Frank Bergmann
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Usage: R --slave -f printMath.R --args <full path to input file>
#
library(libSBML)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1)
{
  stop("Usage: printMath input-filename\n");
}


printFunctionDefinition <- function(n, fd) {
  if ( FunctionDefinition_isSetMath(fd) )
  {
    cat("FunctionDefinition ",n,", ",FunctionDefinition_getId(fd),"(");

    math = FunctionDefinition_getMath(fd);

    # Print function arguments. 
    if (ASTNode_getNumChildren(math) > 1) {
      cat(ASTNode_getName( ASTNode_getLeftChild(math) ));

      for (n in seq_len(ASTNode_getNumChildren(math) - 1)) {
        cat(", ", ASTNode_getName( ASTNode_getChild(math, n) ));
      }
    }

    cat(") := ");

    # Print function body. 
    if (ASTNode_getNumChildren(math) == 0) {
      cat("(no body defined)");
    } else {
      math    = ASTNode_getChild(math, ASTNode_getNumChildren(math) - 1);
      formula = formulaToString(math);
      cat(formula,"\n");      
    }
  }
}


printRuleMath <- function(n, r) {
  if ( Rule_isSetMath(r) ) {
    formula = formulaToString( Rule_getMath(r) );
    cat("Rule ",n,", formula: ",formula,"\n");    
  }
}


printReactionMath <- function(n, r)
{
  if (Reaction_isSetKineticLaw(r)) {
    kl = Reaction_getKineticLaw(r);

    if ( KineticLaw_isSetMath(kl) ) {
      formula = formulaToString( KineticLaw_getMath(kl) );
      cat("Reaction ",n,", formula: ",formula,"\n");      
    }
  }
}


printEventAssignmentMath <- function(n, ea) {
  if ( EventAssignment_isSetMath(ea) ) {
    variable = EventAssignment_getVariable(ea);
    formula  = formulaToString( EventAssignment_getMath(ea) );

    cat("  EventAssignment ",n,", trigger: ",variable," = ",formula,"\n");

  }
}


printEventMath <- function(n, e) {
  if ( Event_isSetDelay(e) ) {
    delay = Event_getDelay(e);
    formula = formulaToString( Delay_getMath(delay) );
    cat("Event ",n," delay: ",formula,"\n");    
  }

  if ( Event_isSetTrigger(e) ) {
    trigger = Event_getTrigger(e);

    formula = formulaToString( Trigger_getMath(trigger) );
    cat("Event ",n," trigger: ",formula,"\n");    
  }

  for (i in seq_len(Event_getNumEventAssignments(e))) {
    printEventAssignmentMath(i, Event_getEventAssignment(e, i-1));
  }

  cat("\n");
}


printMath <- function(m) {

  for (n in seq_len(Model_getNumFunctionDefinitions(m))){
    printFunctionDefinition(n, Model_getFunctionDefinition(m, n-1));
  }
  
  for (n in seq_len(Model_getNumRules(m))){
    printRuleMath(n , Model_getRule(m, n-1));
  }

  cat("\n");

  for (n in seq_len(Model_getNumReactions(m))){
    printReactionMath(n, Model_getReaction(m, n-1));
  }

  cat("\n");

  for (n in seq_len(Model_getNumEvents(m))){
    printEventMath(n , Model_getEvent(m, n-1));
  }
}



d = readSBML(args[1]);
m = SBMLDocument_getModel(d);

SBMLDocument_printErrors(d);

printMath(m);
cat("\n");

q(status=0);


