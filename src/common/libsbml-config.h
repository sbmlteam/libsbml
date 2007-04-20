/**
 * @file    libsbml-config.h
 * @brief   Decides whether to #include Unix or Windows configuration variables.
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and Japan Science and
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


#ifndef LIBSBML_CONFIG_H
#define LIBSBML_CONFIG_H 1


#if defined(WIN32) && !defined(CYGWIN)
#  include <sbml/common/libsbml-config-win.h>
#else
#  include <sbml/common/libsbml-config-unix.h>
#endif


#endif  /* LIBSBML_CONFIG_H */
