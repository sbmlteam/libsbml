/**
 * @file    SBMLNamespaces.cpp
 * @brief   SBMLNamespaces class to store level/version and namespace 
 * @author  Sarah Keating
 *
 * $Id:  $
 * $HeadURL:  $
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2009 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->
 */

#include <sbml/SBMLNamespaces.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */



SBMLNamespaces::SBMLNamespaces(unsigned int level, unsigned int version)
{
  mLevel = level;
  mVersion = version;
  mNamespaces = new XMLNamespaces();

  switch (level)
  {
  case 1:
    mNamespaces->add(SBML_XMLNS_L1, "sbml");
    break;
  case 2:
  default:
    switch (version)
    {
    case 1:
      mNamespaces->add(SBML_XMLNS_L2V1, "sbml");
      break;
    case 2:
      mNamespaces->add(SBML_XMLNS_L2V2, "sbml");
      break;
    case 3:
      mNamespaces->add(SBML_XMLNS_L2V3, "sbml");
      break;
    case 4:
    default:
      mNamespaces->add(SBML_XMLNS_L2V4, "sbml");
      break;
    }
    break;
  }
}

SBMLNamespaces::~SBMLNamespaces()
{
  if (mNamespaces)
    delete mNamespaces;
}


SBMLNamespaces * 
SBMLNamespaces::getSBMLNamespaces(unsigned int level,
                                  unsigned int version)
{
  return new SBMLNamespaces(level, version);
}


std::string 
SBMLNamespaces::getSBMLNamespaceURI(unsigned int level,
                                 unsigned int version)
{
  switch (level)
  {
  case 1:
    return SBML_XMLNS_L1;
    break;
  case 2:
  default:
    switch (version)
    {
    case 1:
      return SBML_XMLNS_L2V1;
      break;
    case 2:
      return SBML_XMLNS_L2V2;
      break;
    case 3:
      return SBML_XMLNS_L2V3;
      break;
    case 4:
    default:
      return SBML_XMLNS_L2V4;
      break;
    }
    break;
  }
}


unsigned int 
SBMLNamespaces::getLevel()
{
  return mLevel;
}


unsigned int 
SBMLNamespaces::getVersion()
{
  return mVersion;
}


XMLNamespaces * 
SBMLNamespaces::getNamespaces()
{
  return mNamespaces;
}

