/**
 * @file    util.c
 * @brief   Supporting functions for example code
 * @author  Ben Bornstein
 * @author  Michael Hucka
 *
 * $Id$
 * $Source$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


/**
 * @return the number of milliseconds elapsed since the Epoch.
 */
unsigned long
getCurrentMillis (void);

/**
 * @return the size (in bytes) of the given filename.
 */
unsigned long
getFileSize (const char *filename);

/**
 * Removes whitespace from both ends of the given string.  The string
 * is modified in-place.
 */
void
trim_whitespace (char *s);

/**
 * The function get_line reads a line from a file (in this case "stdin" and
 * returns it as a string.  It is taken from the utilities library of the
 * VIENNA RNA PACKAGE ( http://www.tbi.univie.ac.at/~ivo/RNA/ )
 */
char*
get_line (FILE *fp);
