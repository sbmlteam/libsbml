## @file    setup.py
## @brief   Python distutils code for libSBML Python module
## @author  Michael Hucka
## @author  Ben Bornstein
## @author  Ben Kovitz
## @author  Frank Bergmann (fbergman@caltech.edu)
## 
## $Id$
## $HeadURL$
##
##<!---------------------------------------------------------------------------
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
##
## Copyright 2005-2010 California Institute of Technology.
## Copyright 2002-2005 California Institute of Technology and
##                     Japan Science and Technology Corporation.
## 
## This library is free software; you can redistribute it and/or modify it
## under the terms of the GNU Lesser General Public License as published by
## the Free Software Foundation.  A copy of the license agreement is provided
## in the file named "LICENSE.txt" included with this software distribution
## and also available online as http://sbml.org/software/libsbml/license.html
##----------------------------------------------------------------------- -->*/

import platform

# figure out whether this is a 64bit or 32 bit installation
basepath = '../../../win32/'

from distutils.core import setup, Extension

setup(name             = "libsbml", 
      version          = "4.3.0",
      description      = "LibSBML Python API",
      long_description = ("LibSBML is a library for reading, writing and "+
                          "manipulating the Systems Biology Markup Language "+
                          "(SBML).  It is written in ISO C and C++, supports "+
                          "SBML Levels 1, 2 and 3, and runs on Linux, Microsoft "+
                          "Windows, and Apple MacOS X.  For more information "+
                          "about SBML, please see http://sbml.org."),
      license          = "LGPL",
      author           = "SBML Team",
      author_email     = "libsbml-team@caltech.edu",
      url              = "http://sbml.org",
	  packages         = ["libsbml"],
      package_dir      = {'libsbml': 'libsbml'},
      data_files       = [#('Lib/site-packages/libsbml', [basepath + 'bin/libsbml.dll']), 
						 ('Lib/site-packages', ['libsbml.pth'])],
	  ext_package      = "libsbml",
      ext_modules      = [Extension("_libsbml", 
							sources = [ "libsbml_wrap.cpp"],
							define_macros =  [('WIN32', None), 
								('USE_LAYOUT', None),
								('LIBSBML_EXPORTS', None)],
							include_dirs = [basepath + "include", 
								basepath + "include/sbml", "."],
							libraries = ["libsbml"],
							library_dirs = [basepath + "lib"]
						    )
                         ]
)
