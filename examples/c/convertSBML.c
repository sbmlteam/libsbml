/**
 * \file    convertSBML.c
 * \brief   Converts SBML L1 documents (any version) to L2v1
 * \author  Ben Bornstein
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


#include <stdio.h>
#include <sbml/SBMLTypes.h>


int
main (int argc, char *argv[])
{
  unsigned int    errors;
  SBMLDocument_t *d;


  if (argc != 3)
  {
    printf("\n  usage: convertSBML <input-filename> <output-filename>\n");
    printf("  Converts an SBML L1 file to L2 or vice-versa.\n\n");
    return 2;
  }


  d      = readSBML(argv[1]);
  errors = SBMLDocument_getNumErrors(d);

  if (errors > 0)
  {
    printf("Read Error(s):\n");
    SBMLDocument_printErrors(d, stdout);

    printf("Conversion skipped.  Correct the above and re-run.\n");
  }
  else
  {
    unsigned int level   = SBMLDocument_getLevel(d) == 2 ? 1 : 2;
    unsigned int version = 2;
    SBMLDocument_setLevelAndVersion(d, level, version);

    errors = SBMLDocument_getNumErrors(d);

    if (errors > 0)
    {
      printf("Conversion Error(s):\n");
      SBMLDocument_printErrors(d, stdout);

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
