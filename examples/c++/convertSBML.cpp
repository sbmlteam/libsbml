/**
 * Filename    : convertSBML.cpp
 * Description : Converts SBML L1 documents (any version) to L2v1
 * Author(s)   : SBML Team <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2005-04-18
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
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


#include "common/common.h"
#include "sbml/SBMLReader.h"
#include "sbml/SBMLWriter.h"
#include "sbml/SBMLTypes.h"

#include "util/util.h"

#include <iostream>
using namespace std;


int
main (int argc, char *argv[])
{
  unsigned int errors = 0, conv_errors;

  SBMLDocument * d   = new SBMLDocument();
  SBMLReader   * sr  = new SBMLReader();
  SBMLWriter   * sw  = new SBMLWriter();

  if (argc != 3)
  {
    cout << "\n  usage: convertSBML <input-filename> <output-filename>\n\n";
    return 1;
  }

  d = sr->readSBML(argv[1]);

  errors = d->getNumWarnings() + d->getNumErrors() + d->getNumFatals();

  if (errors > 0)
  {
    cout << "Error(s):\n";

    d->printWarnings(cout);
	  d->printErrors  (cout);
	  d->printFatals  (cout);

    cout << "Conversion skipped.  Correct the above and re-run.\n";
  }
  else
  {
    if (d->getLevel() == 1)
    {
      d->setLevel(2);
 	    sw->write(*d, argv[2]);
    }
    else
    {
      d->setLevel(1);
  
      conv_errors = d->getNumWarnings() + d->getNumErrors() + d->getNumFatals();

      if (conv_errors > 0)
      {
        cout << "Error(s):\n";

        d->printWarnings(cout);
	      d->printErrors  (cout);
	      d->printFatals  (cout);

        cout << "Conversion skipped.  Correct the above and re-run.\n";
      }
      else
      { 	    
        sw->write(*d, argv[2]);
      }
    }
  }


  delete (sw);
  delete(d);
 
  return errors;
}
