Invokes the cloc program to count lines of code in the libsbml directory.

WINDOWS
=======

count.bat: produces results_cloc.xml


Mac / Unix / CYGWIN
===================
Analog to the windows scripts, these scripts will produce the same files. The only difference
is that they search for cloc in the environment variable CLOC, or in the path. 

count.sh: produces results_cloc.xml in this folder 


There is then the python script: process_count.py which looks for 'results_cloc.xml', parses
the information and produces the file loc_table.tex which is the table of results in tex form.



Notes on CLOC
=============
 (For future reference)
 
1. In order to add the count for swig interface files I needed to add an additional definitions.
   Do NOT use --force-lang-def as this overrides all CLOCS defaults and makes counting MATLAB files
   impossible as these require more than just the definition.

2. When excluding directories the directory to exclude should not have more than one part
   of the path i.e. to exclude src/bindings just put 'bindings'.   
   
3. It does not actually count the R extension R files, you need to force it to count 'r' files.
