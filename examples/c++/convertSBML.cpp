/**
 * \file    convertSBML.cpp
 * \brief   Converts SBML L1 documents (any version) to L2v1
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


using namespace std;


int
main (int argc, char *argv[])
{
  SBMLDocument* d;
  unsigned int  errors;


  if (argc != 3)
  {
    cout << endl
         << "  usage: convertSBML <input-filename> <output-filename>" << endl
         << "  Converts an SBML L1 file to L2 or vice versa"          << endl
         << endl;
    return 2;
  }


  d      = readSBML(argv[1]);
  errors = d->getNumErrors();

  if (errors > 0)
  {
    cout << "Read Error(s):" << endl;
	  d->printErrors(cout);

    cout << "Conversion skipped.  Correct the above and re-run." << endl;
  }
  else
  {
    unsigned int level   = d->getLevel() == 2 ? 1 : 2;
    unsigned int version = 2;
    d->setLevelAndVersion(level, version);
  
    errors = d->getNumErrors();

    if (errors > 0)
    {
      cout << "Conversion Error(s):" << endl;
      d->printErrors(cout);

      cout << "Conversion skipped.  Either libSBML does not (yet) have " << endl
           << "ability to convert this model or (automatic) conversion " << endl
           << "is not possible." << endl;
    }
    else
    {
      writeSBML(d, argv[2]);
    }
  }


  delete d;
  return errors;
}
