The LibSBMLLineNumbers.cs file goes constructs invalid SBMLDocuments containing
all the different SBML components. Next it validates the document printing the
errors and error locations in CSV format. 

Once compiled the program can simply run with the libsbml C# bindings using the 
different XML parsers which enables us to compare the error location reported by
each parser.

See also pivotal: 
https://www.pivotaltracker.com/story/show/23689513
