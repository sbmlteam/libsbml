#!/usr/bin/env ruby
#
## 
## \file    addModelHistory.py
## \brief   adds Model History to a model
## \author  Sarah Keating
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 



require 'libSBML'


def printStatus(message, status)
  statusString = "";
  if status == LibSBML::LIBSBML_OPERATION_SUCCESS
    statusString = "succeeded";
  elsif status == LibSBML::LIBSBML_INVALID_OBJECT
    statusString = "invalid object";
  elsif status == LibSBML::LIBSBML_OPERATION_FAILED
    statusString = "operation failed";
  else
    statusString = "unknown";          
  end
  puts "#{message} #{statusString}"
end


if ARGV.size != 2:
  puts "usage: addModelHistory <input-filename> <output-filename>"
  puts "Adds a model history to the model"
  exit(2)
end

d = LibSBML::readSBML(ARGV[0]);
errors = d.getNumErrors

if errors > 0
    print("Read Error(s):" + "\n");
    d.printErrors();  
    print("Correct the above and re-run." + "\n");
    exit(errors);
end

h = LibSBML::ModelHistory.new

c = LibSBML::ModelCreator.new
c.setFamilyName("Keating");
c.setGivenName("Sarah");
c.setEmail("sbml-team@caltech.edu");
c.setOrganization("University of Hertfordshire");

status = h.addCreator(c);
printStatus("Status for addCreator: ", status);


date = LibSBML::Date.new("1999-11-13T06:54:32");
date2 = LibSBML::Date.new("2007-11-30T06:54:00-02:00");

status = h.setCreatedDate(date);
printStatus("Set created date:      ", status);

status = h.setModifiedDate(date2);
printStatus("Set modified date:     ", status);

status = d.getModel().setModelHistory(h);
printStatus("Set model history:     ", status);


LibSBML::writeSBML(d, ARGV[1]);

exit(errors);



