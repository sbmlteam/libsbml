#!/usr/bin/env ruby
#
## 
## @file    unsetAnnotation.py
## @brief   unset annotation for each element
## @author  Akiya Jouraku
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


require 'libSBML'


if (ARGV.size != 2):
    print("\nUsage: unsetAnnotation <input-filename> <output-filename>" + "\n" + "\n")
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
m.unsetAnnotation

m.getNumReactions.times do |i|
    re = m.getReaction(i)      
    re.unsetAnnotation

    re.getNumReactants.times do |j|
        rt = re.getReactant(j)
        rt.unsetAnnotation          
    end
    re.getNumProducts.times do |j|
        rt = re.getProduct(j)
        rt.unsetAnnotation
    end
    re.getNumModifiers.times do |j|
        md = re.getModifier(j)
        md.unsetAnnotation
    end
    if (re.isSetKineticLaw)
        kl = re.getKineticLaw
        kl.unsetAnnotation

        kl.getNumParameters.times do |j|
            pa = kl.getParameter(j)
            pa.unsetAnnotation
        end
    end
end
m.getNumSpecies.times do |i|
    sp = m.getSpecies(i)
    sp.unsetAnnotation
end
m.getNumCompartments.times do |i|
    sp = m.getCompartment(i)
    sp.unsetAnnotation
end
m.getNumFunctionDefinitions.times do |i|
    sp = m.getFunctionDefinition(i)
    sp.unsetAnnotation
end
m.getNumUnitDefinitions.times do |i|
    sp = m.getUnitDefinition(i)
    sp.unsetAnnotation
end
m.getNumParameters.times do |i|
    sp = m.getParameter(i)
    sp.unsetAnnotation
end
m.getNumRules.times do |i|
    sp = m.getRule(i)
    sp.unsetAnnotation
end
m.getNumInitialAssignments.times do |i|
    sp = m.getInitialAssignment(i)
    sp.unsetAnnotation
end
m.getNumEvents.times do |i|
    sp = m.getEvent(i)
    sp.unsetAnnotation

    sp.getNumEventAssignments.times do |j|
        ea = sp.getEventAssignment(j)
        ea.unsetAnnotation
    end
end
m.getNumSpeciesTypes.times do |i|
    sp = m.getSpeciesType(i)
    sp.unsetAnnotation
end
m.getNumConstraints.times do |i|
    sp = m.getConstraint(i)
    sp.unsetAnnotation
end
LibSBML::writeSBML(document, ARGV[1])

exit(errors);

