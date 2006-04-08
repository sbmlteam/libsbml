/**
 * \file    validateSBML.cpp
 * \brief   Validates an SBML file against the appropriate schema
 * \author  Sarah Keating and Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#include <iostream>

#include <sbml/SBMLTypes.h>
#include "util.h"


using namespace std;


int
main (int argc, char *argv[])
{
  const char* filename;

  unsigned long start, stop, size;
  unsigned int  errors = 0;

  SBMLDocument *d;


  if (argc != 2)
  {
    cout << endl << " usage: validateSBML <filename>" << endl << endl;
    return 2;
  }



  SBMLReader sr;

  filename = argv[1];

  start = getCurrentMillis();
  d     = sr.readSBML(filename);
  stop  = getCurrentMillis();

  errors  = d->getNumErrors();
  errors += d->checkConsistency();

  size = getFileSize(filename);

  cout << endl;
  cout << "        filename: " << filename     << endl;
  cout << "       file size: " << size         << endl;
  cout << "  read time (ms): " << stop - start << endl;
  cout << "        error(s): " << errors       << endl;

  if (errors > 0) d->printErrors(cout);
  cout << endl;

  delete d;
  return errors;
}
