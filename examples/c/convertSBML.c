/**
 * \file    convertSBML.c
 * \brief   Converts SBML L1 documents (any version) to L2v1
 * \author  Ben Bornstein
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
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include "common/common.h"
#include "sbml/SBMLReader.h"
#include "sbml/SBMLWriter.h"
#include "sbml/SBMLTypes.h"

#include "util/util.h"


int
main (int argc, char *argv[])
{
  unsigned int    errors;
  SBMLDocument_t *d;


  if (argc != 3)
  {
    printf("\n  usage: convertSBML <input-filename> <output-filename>\n");
    printf("  Converts an SBML L1 file to L2 or vice-versa.\n\n");
    return 1;
  }


  d = readSBML(argv[1]);

  errors = SBMLDocument_getNumWarnings(d) + SBMLDocument_getNumErrors(d) +
           SBMLDocument_getNumFatals(d);

  if (errors > 0)
  {
    printf("Read Error(s):\n");

    SBMLDocument_printWarnings(d, stdout);
    SBMLDocument_printErrors  (d, stdout);
    SBMLDocument_printFatals  (d, stdout);

    printf("Conversion skipped.  Correct the above and re-run.\n");
  }
  else
  {
    SBMLDocument_setLevel(d, SBMLDocument_getLevel(d) == 2 ? 1 : 2);

    errors = SBMLDocument_getNumWarnings(d) + SBMLDocument_getNumErrors(d) +
             SBMLDocument_getNumFatals(d);

    if (errors > 0)
    {
      printf("Conversion Error(s):\n");

      SBMLDocument_printWarnings(d, stdout);
      SBMLDocument_printErrors  (d, stdout);
      SBMLDocument_printFatals  (d, stdout);

      printf("Conversion skipped.  Either libSBML does not (yet) have ");
      printf("ability to convert this model or (automatic) conversion ");
      printf("is not possible.");
    }
    else
    { 	    
      writeSBML(d, argv[2]);
    }
  }

  SBMLDocument_free(d);
  return errors;
}
