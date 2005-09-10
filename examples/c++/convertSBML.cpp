/**
 * \file    convertSBML.cpp
 * \brief   Converts SBML L1 documents (any version) to L2v1
 * \author  Sarah Keating and Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Sarah Keating
 *
 *     The SBML Team
 *     STRI
 *     University of Hertfordshire
 *     Hatfield, UK
 *
 *     http://sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */

#include "sbml/common/common.h"
#include "sbml/util/util.h"

#include "sbml/SBMLReader.h"
#include "sbml/SBMLWriter.h"
#include "sbml/SBMLTypes.h"



#include <iostream>


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
    return 1;
  }


  d      = readSBML(argv[1]);
  errors = d->getNumWarnings() + d->getNumErrors() + d->getNumFatals();

  if (errors > 0)
  {
    cout << "Read Error(s):" << endl;

    d->printWarnings(cout);
	  d->printErrors  (cout);
	  d->printFatals  (cout);

    cout << "Conversion skipped.  Correct the above and re-run." << endl;
  }
  else
  {
    d->setLevel( d->getLevel() == 2 ? 1 : 2 );
  
    errors = d->getNumWarnings() + d->getNumErrors() + d->getNumFatals();

    if (errors > 0)
    {
      cout << "Conversion Error(s):" << endl;

      d->printWarnings(cout);
      d->printErrors  (cout);
      d->printFatals  (cout);

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
