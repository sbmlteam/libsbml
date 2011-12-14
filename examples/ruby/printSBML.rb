#!/usr/bin/env ruby
#
## 
## @file    printModel.py
## @brief   Prints some information about the top-level model
## @author  Sarah Keating
## @author  Ben Bornstein
## @author  Michael Hucka
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


require 'libSBML'

if (ARGV.size != 1)
    print("\n" + "Usage: printSBML filename" + "\n" + "\n")
    return 1;
end

filename = ARGV[0];
document = LibSBML::readSBML(filename)

if (document.getNumErrors > 0)
    printLine("Encountered the following SBML errors:" + "\n")
    document.printErrors
    exit(1);
end

level = document.getLevel
version = document.getVersion

print("\nFile: #{filename} (Level #{level}, version #{version})\n")

model = document.getModel

if (model == nil)
    print("No model present." + "\n")
    exit(1);
end

idString = "id"
if (level == 1)
  idString = "name"
end
id = "(empty)"
if (model.isSetId)
  id = model.getId
end
print("                 #{idString}: #{id}\n")

if (model.isSetSBOTerm)
    print("      model sboTerm: " + model.getSBOTerm + "\n")
end

print("functionDefinitions: #{model.getNumFunctionDefinitions}\n")
print("    unitDefinitions: #{model.getNumUnitDefinitions}\n")
print("   compartmentTypes: #{model.getNumCompartmentTypes}\n")
print("        specieTypes: #{model.getNumSpeciesTypes}\n")
print("       compartments: #{model.getNumCompartments}\n")
print("            species: #{model.getNumSpecies}\n")
print("         parameters: #{model.getNumParameters}\n")
print(" initialAssignments: #{model.getNumInitialAssignments}\n")
print("              rules: #{model.getNumRules}\n")
print("        constraints: #{model.getNumConstraints}\n")
print("          reactions: #{model.getNumReactions}\n")
print("             events: #{model.getNumEvents}\n")
print("\n")

exit(0);
 
