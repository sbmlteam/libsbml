#!/usr/bin/env ruby
#
## 
## @file    printNotes.py
## @brief   prints Notes strings for each element
## @author  Akiya Jouraku
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 



require 'libSBML'


def printNotes(sb, id="")
  if not sb.isSetNotes
	return
  end
  
  pid = ""
  
  if sb.isSetId
      pid = sb.getId
  end
  puts "-----  #{sb.getElementName}  (#{pid}) Notes -----", "\n"
  puts sb.getNotesString(), "\n"
  puts "\n"
end


if ARGV.size != 1
  puts "Usage: printNotes filename"
  exit(1)
end

filename = ARGV[0]
document = LibSBML::readSBML(filename)
  
errors = document.getNumErrors
  
puts "filename: ", filename, "\n"
  
if errors > 0
  document.printErrors
  return errors
end
  
 
# Model

m = document.getModel
printNotes(m)

m.getNumReactions.times do |i|
    re = m.getReaction(i)
    printNotes(re)

    # SpeciesReference (Reacatant)  
    re.getNumReactants.times do |j|
        rt = re.getReactant(j)
        if rt.isSetNotes
          puts("     ")
        end
        printNotes(rt, rt.getSpecies)
     end

    # SpeciesReference (Product) 
    re.getNumProducts().times do |j|
        rt = re.getProduct(j)
        if rt.isSetNotes
          puts("     ")
        end
        printNotes(rt, rt.getSpecies)
    end

    # ModifierSpeciesReference (Modifiers)  
    re.getNumModifiers.times do |j|
        md = re.getModifier(j)
        if md.isSetNotes
          puts("     ")
        end
        printNotes(md, md.getSpecies)
    end

    # KineticLaw   
    if re.isSetKineticLaw
        kl = re.getKineticLaw
        if kl.isSetNotes
          puts("   ");
        end
        printNotes(kl)

        # Parameter   
        kl.getNumParameters.times do |j|
            pa = kl.getParameter(j);
            if pa.isSetNotes
              puts("      ");
            end
            printNotes(pa)
        end
     end
end

# Species 
m.getNumSpecies.times do |i|
    sp = m.getSpecies(i)
    printNotes(sp)
end

# Compartments 
m.getNumCompartments.times do |i|
    sp = m.getCompartment(i)
    printNotes(sp)
end

# FunctionDefinition 
m.getNumFunctionDefinitions.times do |i|
    sp = m.getFunctionDefinition(i)
    printNotes(sp)
end

# UnitDefinition 
m.getNumUnitDefinitions.times do |i|
    sp = m.getUnitDefinition(i)
    printNotes(sp)
end

# Parameter 
m.getNumParameters.times do |i|
    sp = m.getParameter(i)
    printNotes(sp)
end

# Rule 
m.getNumRules.times do |i|
    sp = m.getRule(i)
    printNotes(sp)
end

# InitialAssignment 
m.getNumInitialAssignments.times do |i|
    sp = m.getInitialAssignment(i)
    printNotes(sp)
end

# Event 
m.getNumEvents.times do |i|
    sp = m.getEvent(i)
    printNotes(sp)

    # Trigger 
    if sp.isSetTrigger
        tg = sp.getTrigger
        if tg.isSetNotes
          puts("   ")
        end
        printNotes(tg)
    end

    # Delay 
    if sp.isSetDelay
        dl = sp.getDelay()
        if dl.isSetNotes
          puts("   ")
        end
        printNotes(dl)
    end

    # EventAssignment 
    sp.getNumEventAssignments.times do |j|
        ea = sp.getEventAssignment(j)
        if ea.isSetNotes
          puts("   ")
        end
        printNotes(ea)
    end
end
# SpeciesType 
m.getNumSpeciesTypes.times do |i|
    sp = m.getSpeciesType(i)
    printNotes(sp)
end

# Constraints 
m.getNumConstraints.times do |i|
    sp = m.getConstraint(i)
    printNotes(sp)
end
exit(errors)
