/**
 * \file    RDFAnnotation.cpp
 * \brief   RDFAnnotation I/O
 * \author  Sarah Keating
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


#include <limits>
#include <iomanip>
#include <string>
#include <sstream>

#include <cstdlib>

#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLErrorLog.h>

#include <sbml/SBase.h>
#include <sbml/layout/Layout.h>


#include <sbml/SBMLErrorLog.h>

#include <sbml/util/util.h>
#include <sbml/util/List.h>

#include <sbml/annotation/ModelHistory.h>

#include "LayoutAnnotation.h"


using namespace std;

/**
 * takes an annotation that has been read into the model
 * identifies the RDF elements
 * and creates a List of CVTerms from the annotation
 */
LIBSBML_EXTERN
void 
parseLayoutAnnotation(XMLNode * annotation, ListOf& layouts)
{

  const string&  name = annotation->getName();
  const XMLNode*  LayoutTop = NULL;
  Layout* layout;
  unsigned int n = 0;

  // need to find the RDF desciption opening annotation
  if (name == "annotation" && annotation->getNumChildren() > 0)
  {
    while (n < annotation->getNumChildren())
    {
      const string &name1 = annotation->getChild(n).getName();
      if (name1 == "listOfLayouts") // also check the namespace
      {
        const XMLNamespaces& namespaces=annotation->getChild(n).getNamespaces();
        if(namespaces.getIndex("http://projects.eml.org/bcb/sbml/level2")!=-1)
        {
          LayoutTop = &(annotation->getChild(n).getChild(0));
          break;
        }
      }
      n++;
    }
  }

  // find qualifier nodes and create 

  
  n = 0;
  if (LayoutTop)
  {
    while (n < LayoutTop->getNumChildren())
    {
      const string &name2 = LayoutTop->getChild(n).getName();
      if (name2 == "layout")
      {
        layout = new Layout(LayoutTop->getChild(n));
        layouts.appendAndOwn(layout);
      }
      n++;
    }
  }
  
}


