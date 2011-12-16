#!/usr/bin/env ruby
#
## 
## @file    printAnnotation.py
## @brief   prints annotation strings for each element
## @author  Akiya Jouraku
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 



require 'libSBML'


def printAnnotation(sb, id="")
  if not sb.isSetAnnotation
	return
  end
  
  pid = ""
  
  if sb.isSetId
      pid = sb.getId
  end
  puts "-----  #{sb.getElementName}  (#{pid}) annotation -----", "\n"
  puts sb.getAnnotationString(), "\n"
  puts "\n"
end


if ARGV.size != 1
  puts "Usage: printAnnotation filename"
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
printAnnotation(m)

m.getNumReactions.times do |i|
    re = m.getReaction(i)
    printAnnotation(re)

    # SpeciesReference (Reacatant)  
    re.getNumReactants.times do |j|
        rt = re.getReactant(j)
        if rt.isSetAnnotation
		puts("     ")
        end
        printAnnotation(rt, rt.getSpecies)
     end

    # SpeciesReference (Product) 
    re.getNumProducts().times do |j|
        rt = re.getProduct(j)
        if rt.isSetAnnotation
		puts("     ")
        end
        printAnnotation(rt, rt.getSpecies)
    end

    # ModifierSpeciesReference (Modifiers)  
    re.getNumModifiers.times do |j|
        md = re.getModifier(j)
        if md.isSetAnnotation
		puts("     ")
        end
        printAnnotation(md, md.getSpecies)
    end

    # KineticLaw   
    if re.isSetKineticLaw
        kl = re.getKineticLaw
        if kl.isSetAnnotation
		puts("   ");
        end
        printAnnotation(kl)

        # Parameter   
        kl.getNumParameters.times do |j|
            pa = kl.getParameter(j);
            if pa.isSetAnnotation
			puts("      ");
            end
            printAnnotation(pa)
        end
     end
end

# Species 
m.getNumSpecies.times do |i|
    sp = m.getSpecies(i)
    printAnnotation(sp)
end

# Compartments 
m.getNumCompartments.times do |i|
    sp = m.getCompartment(i)
    printAnnotation(sp)
end

# FunctionDefinition 
m.getNumFunctionDefinitions.times do |i|
    sp = m.getFunctionDefinition(i)
    printAnnotation(sp)
end

# UnitDefinition 
m.getNumUnitDefinitions.times do |i|
    sp = m.getUnitDefinition(i)
    printAnnotation(sp)
end

# Parameter 
m.getNumParameters.times do |i|
    sp = m.getParameter(i)
    printAnnotation(sp)
end

# Rule 
m.getNumRules.times do |i|
    sp = m.getRule(i)
    printAnnotation(sp)
end

# InitialAssignment 
m.getNumInitialAssignments.times do |i|
    sp = m.getInitialAssignment(i)
    printAnnotation(sp)
end

# Event 
m.getNumEvents.times do |i|
    sp = m.getEvent(i)
    printAnnotation(sp)

    # Trigger 
    if sp.isSetTrigger
        tg = sp.getTrigger
        if tg.isSetAnnotation
		puts("   ")
        end
        printAnnotation(tg)
    end

    # Delay 
    if sp.isSetDelay
        dl = sp.getDelay()
        if dl.isSetAnnotation
		puts("   ")
        end
        printAnnotation(dl)
    end

    # EventAssignment 
    sp.getNumEventAssignments.times do |j|
        ea = sp.getEventAssignment(j)
        if ea.isSetAnnotation
		puts("   ")
        end
        printAnnotation(ea)
    end
end
# SpeciesType 
m.getNumSpeciesTypes.times do |i|
    sp = m.getSpeciesType(i)
    printAnnotation(sp)
end

# Constraints 
m.getNumConstraints.times do |i|
    sp = m.getConstraint(i)
    printAnnotation(sp)
end
exit(errors)
