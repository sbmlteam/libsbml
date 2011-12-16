#!/usr/bin/env ruby
#
## 
## @file    printUnits.py
## @brief   Prints some unit information about the model
## @author  Sarah Keating
## @author  Michael Hucka
## 
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


require 'libSBML'


if (ARGV.size != 1)
    puts "Usage: printUnits filename"
    return 1;
end

filename = ARGV[0];
document = LibSBML::readSBML(filename)

if (document.getNumErrors > 0)
    puts "Encountered the following SBML errors:"
    document.printErrors
    exit(1);
end
model = document.getModel

if (model == nil)
    puts "No model present."
    exit(1);
end
model.getNumSpecies.times do |i|
    s = model.getSpecies(i)
    puts "Species #{i}: #{LibSBML::UnitDefinition.printUnits(s.getDerivedUnitDefinition)}\n"
end
model.getNumCompartments.times do |i|
    c = model.getCompartment(i)
    puts "Compartment #{i}: #{LibSBML::UnitDefinition.printUnits(c.getDerivedUnitDefinition)}\n"
end
model.getNumParameters.times do |i|
    p = model.getParameter(i)
    puts "Parameter #{i}: #{LibSBML::UnitDefinition.printUnits(p.getDerivedUnitDefinition)}\n"
end
model.getNumInitialAssignments.times do |i|
    ia = model.getInitialAssignment(i)
    print("InitialAssignment #{i}: #{LibSBML::UnitDefinition.printUnits(ia.getDerivedUnitDefinition)}\n")
    tmp = "no"
    if (ia.containsUndeclaredUnits)
	  tmp = "yes"
    end
    print("        undeclared units: #{tmp}")
end
model.getNumEvents.times do |i|
    e = model.getEvent(i)
    puts "Event #{i}: "

    if (e.isSetDelay)
        print("\n\tDelay: #{LibSBML::UnitDefinition.printUnits(e.getDelay.getDerivedUnitDefinition)}\n")
        tmp = "no"
        if (e.getDelay.containsUndeclaredUnits)
	      tmp = "yes"
        end
        print("        undeclared units: #{tmp}")
    end
    e.getNumEventAssignments.times do |j|
        ea = e.getEventAssignment(j)
        print("\n\tEventAssignment #{j}: #{LibSBML::UnitDefinition.printUnits(ea.getDerivedUnitDefinition)}\n")
        tmp = "no"
        if (ea.containsUndeclaredUnits)
	      tmp = "yes"
        end
        print("        undeclared units: #{tmp}")
    end
end
model.getNumReactions.times do |i|
    r = model.getReaction(i)

    print("\nReaction #{i}: ")

    if (r.isSetKineticLaw)
        print("Kinetic Law: #{LibSBML::UnitDefinition.printUnits(r.getKineticLaw.getDerivedUnitDefinition)}\n")
        tmp = "no"
        if (r.getKineticLaw.containsUndeclaredUnits)
	      tmp = "yes"
        end
        print("        undeclared units: #{tmp}")
    end
    r.getNumReactants.times do |j|
        sr = r.getReactant(j)

        if (sr.isSetStoichiometryMath)
            print("Reactant stoichiometryMath #{j}: #{LibSBML::UnitDefinition.printUnits(sr.getStoichiometryMath.getDerivedUnitDefinition)}\n")
            tmp = "no"
            if (sr.getStoichiometryMath.containsUndeclaredUnits)
	          tmp = "yes"
            end
            print("        undeclared units: #{tmp}")
        end
    end
    r.getNumProducts.times do |j|
        sr = r.getProduct(j)

        if (sr.isSetStoichiometryMath)
            print("Product stoichiometryMath #{j}: #{LibSBML::UnitDefinition.printUnits(sr.getStoichiometryMath.getDerivedUnitDefinition)}\n")
            tmp = "no"
            if (sr.getStoichiometryMath.containsUndeclaredUnits)
	          tmp = "yes"
            end
           print("        undeclared units: #{tmp}")
        end
    end
end
model.getNumRules.times do |i|
    r = model.getRule(i)
    print("\nRule #{i}: #{LibSBML::UnitDefinition.printUnits(r.getDerivedUnitDefinition)}\n")
    tmp = "no"
    if (r.getStoichiometryMath.containsUndeclaredUnits)
	  tmp = "yes"
    end
    print("        undeclared units: #{tmp}")
end
puts
exit(0);
  
