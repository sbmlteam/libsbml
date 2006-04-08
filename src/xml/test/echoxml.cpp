/**
 * \file    echoxml.cpp
 * \brief   Echos an XML file using SAX, DOM, or an XML token stream.
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2006 California Institute of Technology and Japan Science and
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
#include <string>

#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLParser.h>

#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include "EchoHandler.h"


using namespace std;


void
usage ()
{
  cerr << "usage: echoxml filename.xml mode library"            << endl
       << "where: mode     is one of: sax, tok, or dom"         << endl
       << "       library  is one of: expat, xerces, or libxml" << endl;
  exit(2);
}


int
main (int argc, char *argv[])
{
  const char* filename;
  const char* library;
  string mode;


  if (argc != 4) usage();

  filename = argv[1];
  mode     = argv[2];
  library  = argv[3];

  if (mode == "sax")
  {
    EchoHandler  handler;
    XMLParser*   parser = XMLParser::create(handler, library);

    if (parser) parser->parse(filename);
    delete parser;
  }
  else if (mode == "tok")
  {
    XMLInputStream  istream(filename, true, library);
    XMLOutputStream ostream(cout);

    ostream.setAutoIndent(false);
    while ( istream.isEOF() == false ) ostream << istream.next();
  }
  else if (mode == "dom")
  {
    XMLInputStream  istream(filename, true, library);
    XMLOutputStream ostream(cout);

    ostream.setAutoIndent(false);
    if ( istream.isGood() ) ostream << XMLNode(istream);
  }
  else
  {
    usage();
  }

  cout << endl;

  return 0;
}
