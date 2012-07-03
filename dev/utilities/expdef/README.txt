EXPDEF
======
A utility that can generate DEF / LIB files for DLLs. Two executables are
 in this folder: 

expdef.exe: The original executable 
            from http://purefractalsolutions.com/show.php?a=utils/expdef
			
This tool will only work for 32bit libraries, after slight adaptation of 
the code 

expdef64.exe: the 64bit version (for generating 64bit import libs for 64bit 
dlls). 

Usage: 
======

expdef(64).exe -d<def filename> -l <dll filename>

This will load the dll file, create a DEF file with the given filename, and 
run the linker to produce an import library. For example:

expdef.exe -dRlib.def -l R.dll

when executed in the R library folder will produce the files: 

Rlib.def, Rlib.lib and Rlib.exp. Of course the visual studio command line tools
have to be available. In order to generate the 64bit import library, the 64bit 
command line tools have to be loaded first. 

