#!/usr/bin/env ruby
#
## 
## @file    unsetNotes.py
## @brief   unset notes for each element
## @author  Akiya Jouraku
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


require 'libSBML'


if (ARGV.size != 2):
    print("\nUsage: unsetNotes <input-filename> <output-filename>" + "\n" + "\n")
    exit(1);
end

filename = ARGV[0];

document = LibSBML::readSBML(filename)


errors = document.getNumErrors

if (errors > 0)
    document.printErrors
    return errors;
end

m = document.getModel
m.unsetNotes

m.getNumReactions.times do |i|
    re = m.getReaction(i)      
    re.unsetNotes

    re.getNumReactants.times do |j|
        rt = re.getReactant(j)
        rt.unsetNotes          
    end
    re.getNumProducts.times do |j|
        rt = re.getProduct(j)
        rt.unsetNotes
    end
    re.getNumModifiers.times do |j|
        md = re.getModifier(j)
        md.unsetNotes
    end
    if (re.isSetKineticLaw)
        kl = re.getKineticLaw
        kl.unsetNotes

        kl.getNumParameters.times do |j|
            pa = kl.getParameter(j)
            pa.unsetNotes
        end
    end
end
m.getNumSpecies.times do |i|
    sp = m.getSpecies(i)
    sp.unsetNotes
end
m.getNumCompartments.times do |i|
    sp = m.getCompartment(i)
    sp.unsetNotes
end
m.getNumFunctionDefinitions.times do |i|
    sp = m.getFunctionDefinition(i)
    sp.unsetNotes
end
m.getNumUnitDefinitions.times do |i|
    sp = m.getUnitDefinition(i)
    sp.unsetNotes
end
m.getNumParameters.times do |i|
    sp = m.getParameter(i)
    sp.unsetNotes
end
m.getNumRules.times do |i|
    sp = m.getRule(i)
    sp.unsetNotes
end
m.getNumInitialAssignments.times do |i|
    sp = m.getInitialAssignment(i)
    sp.unsetNotes
end
m.getNumEvents.times do |i|
    sp = m.getEvent(i)
    sp.unsetNotes

    sp.getNumEventAssignments.times do |j|
        ea = sp.getEventAssignment(j)
        ea.unsetNotes
    end
end
m.getNumSpeciesTypes.times do |i|
    sp = m.getSpeciesType(i)
    sp.unsetNotes
end
m.getNumConstraints.times do |i|
    sp = m.getConstraint(i)
    sp.unsetNotes
end
LibSBML::writeSBML(document, ARGV[1])

exit(errors);

