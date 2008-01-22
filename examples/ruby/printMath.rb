#!/usr/bin/env ruby
#
# @file    printMath.rb
# @brief   Prints Rule, Reaction, and Event formulas in a given SBML Document
# @author  Alex Gutteridge (Ruby conversion of examples/c/printMath.c)
# @author  Ben Bornstein
#
# $Id$
# $Source$
#
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#

require 'libSBML'
     
module LibSBML
  class Model
    def printMath
      getNumFunctionDefinitions.times do |n|
        puts "Function #{n}: #{getFunctionDefinition(n)}" 
      end
      getNumRules.times do |n|
        puts "Rule #{n}: #{getRule(n)}"
      end  
      getNumReactions.times do |n|
        puts "Reaction #{n}: #{getReaction(n)}"
      end  
      getNumEvents.times do |n|
        puts "Event #{n} #{getEvent(n)}"
      end
    end
  end
  
  class FunctionDefinition
     def to_s
       s = ""
       if isSetMath
         s << "#{getId}("
         math = getMath
         
         #Print function args
         if math.getNumChildren > 1
           s << math.getLeftChild.getName
           ((math.getNumChildren)-2).times do |n|
             s << ", " + math.getChild(n+1).getName
           end
         end
         
         s << ") := "

         #Print function body
         if math.getNumChildren == 0
           s << "(no body defined)"
         else
           s << LibSBML::formulaToString(math.getChild(math.getNumChildren-1))
         end

         return s

       end
     end
  end
  class Rule
     def to_s
        if isSetMath
          LibSBML::formulaToString(getMath)
        end  
     end
  end
  class Reaction
    def to_s
      if isSetKineticLaw and getKineticLaw.isSetMath
         LibSBML::formulaToString(getKineticLaw.getMath)                                           
      end
    end
  end
  class EventAssignment
    def to_s
      if isSetMath
        "#{getVariable} = #{LibSBML::formulaToString(getMath)}" 
      end
    end
  end
  class Event
    def to_s
      d = ''
      t = ''
      a = []
      if isSetDelay
        d = "Delay: #{LibSBML::formulaToString(getDelay.getMath)} "
      end
      if isSetTrigger
        t = "Trigger: #{LibSBML::formulaToString(getTrigger.getMath)} "
      end
      getNumEventAssignments.times do |n|
         a << getEventAssignment(n).to_s 
      end
      d + t + a.join(", ")   
    end
  end
end
 
if ARGV.size != 1
  puts "Usage: printMath filename"
  exit(1)
end

d = LibSBML::readSBML(ARGV[0])
d.printErrors

d.getModel.printMath
