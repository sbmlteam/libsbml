/**
 * \file    ExpatToXerces.h
 * \brief   Maps Expat XML events to a Xerces-C++ XML SAX2 event handler
 * \author  Ben Bornstein
 * 
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and
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


#ifndef ExpatToXerces_h
#define ExpatToXerces_h


#ifdef __cplusplus


#include <string>
#include <vector>

#include "common.h"
#include "Expat.h"


class Attributes;


/**
 * ExpatToXerces is an intermediate bridge class.  It provies a Xerces-C++
 * SAX2 interface to subclasses, but the underlying XML parser is Expat.
 * It accomplishes this by specializing the Expat class and routing its
 * event handlers to the corresponding Xerces event handlers, mapping
 * parameters, their formats, and types as necessary.
 *
 * For now only the Xerces elements startElement(), endElement(), and
 * characters() supported.
 *
 * NOTE: When subclassing, do not override the Expat onStartElement(),
 * onEndElement(), and onCharacterData() handlers.  These are part of the
 * Expat interface and need to be handled by this class in order to
 * delegate to the Xerces interface.
 *
 * @author Ben Bornstein
 */
class ExpatToXerces : public Expat
{
public:

  /**
   * Creates a new ExpatToXerces interface.
   */
  ExpatToXerces ();

  /**
   * Destroys this ExpatToXerces interface.
   */
  virtual ~ExpatToXerces ();


  /* ------------------------------------------------------------ */
  /*  Xerces-C++ Interface                                        */
  /*  --------------------                                        */
  /*                                                              */
  /*  Override the methods below if your application is written   */
  /*  to the use the Xerces-C++ SAX2 interface.                   */
  /* ------------------------------------------------------------ */

  /**
   * Receive notification of the start of an element.
   *
   * By default, do nothing. Application writers may override this method
   * in a subclass to take specific actions at the start of each element
   * (such as allocating a new tree node or writing output to a file).
   *
   * @param  uri        The URI of the associated namespace for this element
   * @param  localname  The local part of the element name
   * @param  qname      The qualified name of this element
   * @param  attrs      The specified or defaulted attributes
   */
  virtual void startElement
  (
    const XMLCh* const  uri,
    const XMLCh* const  localname,
    const XMLCh* const  qname,
    const Attributes&   attrs
  ) = 0;

  /**
   * Receive notification of the end of an element.
   *
   * By default, do nothing. Application writers may override this method
   * in a subclass to take specific actions at the end of each element
   * (such as finalising a tree node or writing output to a file).
   *
   * @param  uri        The URI of the associated namespace for this element
   * @param  localname  The local part of the element name
   * @param  qname      The qualified name of this element
   */
  virtual void endElement
  (
    const XMLCh* const  uri,
    const XMLCh* const  localname,
    const XMLCh* const  qname
  ) = 0;

  /**
   * Receive notification of character data inside an element.
   *
   * By default, do nothing. Application writers may override this method
   * to take specific actions for each chunk of character data (such as
   * adding the data to a node or buffer, or printing it to a file).
   *
   * @param  chars   The characters
   * @param  length  The number of characters to use from the character array
   */
  virtual void characters
  (
    const XMLCh* const chars
    , const unsigned int length
  ) = 0;




  /* ------------------------------------------------------------ */
  /*  Expat Interface                                             */
  /*  ---------------                                             */
  /*                                                              */
  /*  The methods below are overridden from the parent class      */
  /*  Expat to delegate to the methods above.  When subclassing   */
  /*  this class, override only the methods above; *do not*       */
  /*  override the methods below.                                 */
  /* ------------------------------------------------------------ */

  /**
   * Start element handler
   *
   * @param  const XML_Char*   name
   * @param  const XML_Char**  pAttrs
   */
  virtual void onStartElement (const XML_Char* name, const XML_Char** pAttrs);

  /**
   * End element handler
   *
   * @param  const XML_Char*  name
   */
  virtual void onEndElement (const XML_Char* name);

  /**
   * Character data handler
   *
   * @param  const XML_Char*  data
   * @param  int              length
   */
  virtual void onCharacterData (const XML_Char* data, int length);

  /**
   * Start namespace declaration handler
   *
   * @param  const XML_Char*  prefix
   * @param  const XML_Char*  URI
   */
  virtual void onStartNamespaceDecl(const XML_Char* prefix,
                                    const XML_Char* URI);


private:

  /**
   * @return a new set of raw Expat attributes with all xmlns:prefix="URI"
   * attributes followed by attributes in oldAttrs.
   */
  const XML_Char** prependNamespaceAttrs (const XML_Char** oldAttrs);

  /**
   * Sets mQName to prefix + ':' + localname
   */
  void setQName (XML_Char* prefix, XML_Char* localname);


  std::string mQName;

  std::vector<std::string> mNamespaceAttrNames;
  std::vector<std::string> mNamespaceAttrValues;

  static const char SepChar;
};


#endif  /* __cplusplus */
#endif  /* ExpatToXerces_h */
