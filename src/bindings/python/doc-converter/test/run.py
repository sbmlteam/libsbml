#!/usr/bin/env python
##
## @file    run.py
## @brief   Unit test runner for libSBML Python doc converter
## @author  Mike Hucka
## 
## <!--------------------------------------------------------------------------
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
##
## Copyright (C) 2009-2013 jointly by the following organizations: 
##     1. California Institute of Technology, Pasadena, CA, USA
##     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
##  
## Copyright (C) 2006-2008 by the California Institute of Technology,
##     Pasadena, CA, USA 
##  
## Copyright (C) 2002-2005 jointly by the following organizations: 
##     1. California Institute of Technology, Pasadena, CA, USA
##     2. Japan Science and Technology Agency, Japan
## 
## This library is free software; you can redistribute it and/or modify it
## under the terms of the GNU Lesser General Public License as published by
## the Free Software Foundation.  A copy of the license agreement is provided
## in the file named "LICENSE.txt" included with this software distribution
## and also available online as http://sbml.org/software/libsbml/license.html
## ------------------------------------------------------------------------ -->

import argparse
import os
import sys
import unittest

# Configure command line arguments and process them.
# If -h is provided, argparse will print the help and automatically exit.

help_prolog = "Run tests for libSBML's Python document string converter."
help_epilog = '''If give no arguments, this will run all the unit tests in 
the "cases" subdirectory..'''

parser = argparse.ArgumentParser(description=help_prolog, epilog=help_epilog)
parser.parse_args()

# Run the tests.

ourdir = os.path.dirname(os.path.abspath(__file__))
tests  = unittest.TestLoader().discover(start_dir=(ourdir + '/cases'))
runner = unittest.runner.TextTestRunner()
runner.run(tests)
