library(libSBML)

# read the file
doc <- libSBML::readSBMLFromFile('species_initial_values.xml')

# if all is good we get a non-null model back
model <- doc$getModel()


# now we can go through all species
for ( i in 0:(model$getNumSpecies() -1) )
{
  current <- model$getSpecies(i)

  # now the species can have several cases that determines
  # their initial value 
  
  # it could be that the species is fully determined by an assignment rule
  # (that apply at all times), so we have to check rules first
  rule <- model$getRule(current$getId())
  if (!is.null(rule))
  {
    # ok there is a rule for this species so lets figure out what its type
    # is as that determines whether it applies at t0
    rule_type <- rule$getTypeCode()
    type_name <- libSBML::SBMLTypeCode_toString(rule_type, 'core')
    if (type_name == "AssignmentRule")
    {
      # the initial value is determined by the formula
      math <- rule$getMath()
      if (!is.null(math))
      {
        formula <- libSBML::formulaToL3String(math)
        print(paste('Species: ', current$getId(), ' is determined at all times by formula: ', formula))
        
        # no need to look at other values so continue for another one
        next
      }
    }
    
    if (type_name == "RateRule")
    {
      math <- rule$getMath()
      if (!is.null(math))
      {
        formula <- libSBML::formulaToL3String(math)
        print(paste('Species: ', current$getId(), ' has an ode rule with formula: ', formula))
        
        # even though there is an ODE attached to the species, its initial value is needed
      }
    }
  }
  
  
  # it could have an initial assignment
  ia <- model$getInitialAssignment(current$getId())
  if (!is.null(ia))
  {
    math <- ia$getMath()
    if (!is.null(math))
    {
      formula <- libSBML::formulaToL3String(math)
      print(paste("Species: ", current$getId(), " has an initial assignment with formula: ", formula))
      
      # as soon as you have that formula, no initial concentration / amount applies
      # so we don't have to look at anything else for this species
      next
    }
  }
  
  
  # it could have an initial amount
  if (current$isSetInitialAmount())
  {
    print (paste("Species: ", current$getId(), "has initial amount: ", current$getInitialAmount()))
  }
  
  # it could have an initial concentration
  if (current$isSetInitialConcentration())
  {
    print (paste("Species: ", current$getId(), "has initial concentration: ", current$getInitialConcentration()))
  }
}