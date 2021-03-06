#!/usr/bin/env ruby
#
## 
## @file    printAnnotation.py
## @brief   prints annotation strings for each element
## @author  Akiya Jouraku
## 
## <!--------------------------------------------------------------------------
## This sample program is distributed under a different license than the rest
## of libSBML.  This program uses the open-source MIT license, as follows:
##
## Copyright (c) 2013-2018 by the California Institute of Technology
## (California, USA), the European Bioinformatics Institute (EMBL-EBI, UK)
## and the University of Heidelberg (Germany), with support from the National
## Institutes of Health (USA) under grant R01GM070923.  All rights reserved.
##
## Permission is hereby granted, free of charge, to any person obtaining a
## copy of this software and associated documentation files (the "Software"),
## to deal in the Software without restriction, including without limitation
## the rights to use, copy, modify, merge, publish, distribute, sublicense,
## and/or sell copies of the Software, and to permit persons to whom the
## Software is furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in
## all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
## THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
## FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
## DEALINGS IN THE SOFTWARE.
##
## Neither the name of the California Institute of Technology (Caltech), nor
## of the European Bioinformatics Institute (EMBL-EBI), nor of the University
## of Heidelberg, nor the names of any contributors, may be used to endorse
## or promote products derived from this software without specific prior
## written permission.
## ------------------------------------------------------------------------ -->
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
