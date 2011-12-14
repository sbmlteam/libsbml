#!/usr/bin/env ruby
#
## 
## @file    translateMath.py
## @brief   Translates infix formulas into MathML and vice-versa
## @author  Sarah Keating
## @author  Ben Bornstein
## 
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


require 'libSBML'


#
#Translates the given infix formula into MathML.
#
#@return the MathML as a string.  The caller owns the memory and is
#responsible for freeing it.
#
def translateInfix(formula)
    math = LibSBML::parseFormula(formula);
    return LibSBML::writeMathMLToString(math);
end
# 
# Translates the given MathML into an infix formula.  The MathML must
# contain no leading whitespace, but an XML header is optional.
# 
# @return the infix formula as a string.  The caller owns the memory and
# is responsible for freeing it.
# 
def translateMathML(xml)
    math = LibSBML::readMathMLFromString(xml);
    return LibSBML::formulaToString(math);
end

# don't print the exception 
trap("SIGINT") { exit! }

puts "This program translates infix formulas into MathML and"
puts "vice-versa.  Enter or return on an empty line triggers"
puts "translation. Ctrl-C quits"

sb = ""  
begin
  while true
      puts "Enter infix formula or MathML expression (Ctrl-C to quit):"
      print ("> ")
      STDOUT.flush
  
      line = gets
      while line != nil:
          trimmed = line.strip!
          length = trimmed.size
          if (length > 0)
              sb = sb + trimmed;
          else
              str = sb;
              result = ""
              if (str[0] == 60)
    	  result = translateMathML(str)
              else
    	  result =  translateInfix(str)
              end    
              print("Result:\n\n #{result}\n\n");
              sb = "";
              break;
          end
          line = gets
      end
   end
rescue SystemExit, Interrupt, Exception
end
