/**
 * @file    SBMLTransforms.cpp
 * @brief   Transform functions
 * @author  Sarah Keating
 *
 * $Id:  $
 * $HeadURL: $
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
 *----------------------------------------------------------------------- -->*/

#include <sbml/SBMLTransforms.h>
#include <cstring>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */

LIBSBML_CPP_NAMESPACE_BEGIN

void
SBMLTransforms::replaceFD(ASTNode * node, const ListOfFunctionDefinitions *lofd)
{
  bool replaced = false;

  /* write a list of fd ids */
  IdList ids;
  unsigned int i;
  for (i = 0; i < lofd->size(); i++)
  {
    ids.append(lofd->get(i)->getId());
  }
  
  /* if any of these ids exist in the ASTnode replace */
  do 
  {
    for (i = 0; i < lofd->size(); i++)
    {
      replaceFD(node, lofd->get(i));
    }

    replaced = !(checkNodeForIds(node, ids));
  } 
  while (!replaced);
}

void
SBMLTransforms::replaceFD(ASTNode * node, const FunctionDefinition *fd)
{
  
  if (node->isFunction() && node->getName() == fd->getId())
  {
   replaceBvars(node, fd);
   for (unsigned int j = 0; j < node->getNumChildren(); j++)
   {
     replaceFD(node->getChild(j), fd);
   }
  }
  else
  {
    for (unsigned int i = 0; i < node->getNumChildren(); i++)
    {
      replaceFD(node->getChild(i), fd);
    }
  }
}
void
SBMLTransforms::replaceBvars(ASTNode * node, const FunctionDefinition *fd)
{
  ASTNode * fdMath = NULL;
  unsigned int noBvars;
  unsigned int nodeCount;


  if (fd && fd->isSetMath())
    {
      noBvars = fd->getNumArguments();
      if (noBvars == 0)
      {
        fdMath = fd->getMath()->getLeftChild()->deepCopy();
      }
      else
      {
        fdMath = fd->getMath()->getRightChild()->deepCopy();
      }

      for (unsigned int i = 0, nodeCount = 0; i < noBvars; i++, nodeCount++)
      {
        if (nodeCount < node->getNumChildren())
          fdMath->replaceArgument(fd->getArgument(i)->getName(), 
                                            node->getChild(nodeCount));
      }
    }
    (*node) = *fdMath;
}

bool
SBMLTransforms::checkNodeForIds(ASTNode * node, IdList ids)
{
  bool present = false;
  unsigned int i = 0;
  unsigned int numChildren = node->getNumChildren();

  if (node != NULL && node->getType() == AST_FUNCTION)
  {
    if (ids.contains(node->getName()))
    {
      present = true;
    }
  }

  while (!present && i < numChildren)
  {
    present = checkNodeForIds(node->getChild(i), ids);
    i++;
  }
  
  return present;
}

/** @cond doxygen-c-only */


LIBSBML_CPP_NAMESPACE_END

/** @endcond doxygen-c-only */

