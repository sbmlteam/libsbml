/**
 * Filename    : Event.hpp
 * Description : SBML Event
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-05-03
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


#ifndef Event_hpp
#define Event_hpp


#include <string>

#include "extern.h"
#include "SBase.hpp"
#include "ListOf.hpp"


class ASTNode;
class EventAssignment;
class SBMLVisitor;


class Event : public SBase
{
public:

  /**
   * Creates a new Event, optionally with its id, trigger and delay
   * attribute set.  Trigger and delay may be specified as infix formula
   * strings.
   */
  LIBSBML_EXTERN
  Event (   const std::string&  id      = ""
          , const std::string&  trigger = ""
          , const std::string&  delay   = "" );

  /**
   * Creates a new Event with an id and trigger and (optionally) delay
   * attributes set.
   */
  LIBSBML_EXTERN
  Event (   const std::string&  id
          , ASTNode*            trigger
          , ASTNode*            delay   = NULL );

  /**
   * Destroys this Event.
   */
  LIBSBML_EXTERN
  virtual ~Event ();


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next Event
   * (if available).
   */
  LIBSBML_EXTERN
  bool accept (SBMLVisitor& v) const;

  /**
   * @return the id of this Event.
   */
  LIBSBML_EXTERN
  const std::string& getId () const;

  /**
   * @return the name of this Event.
   */
  LIBSBML_EXTERN
  const std::string& getName () const;

  /**
   * @return the trigger of this Event.
   */
  LIBSBML_EXTERN
  const ASTNode* getTrigger () const;

  /**
   * @return the delay of this Event.
   */
  LIBSBML_EXTERN
  const ASTNode* getDelay () const;

  /**
   * @return the timeUnits of this Event.
   */
  LIBSBML_EXTERN
  const std::string& getTimeUnits () const;

  /**
   * @return true if the id of this Event has been set, false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetId () const;

  /**
   * @return true if the name of this Event has been set, false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetName () const;

  /**
   * @return true if the trigger of this Event has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetTrigger () const;

  /**
   * @return true if the delay of this Event has been set, false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetDelay () const;

  /**
   * @return true if the timeUnits of this Event has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetTimeUnits () const;

  /**
   * Sets the id of this Event to a copy of sid.
   */
  LIBSBML_EXTERN
  void setId (const std::string& sid);

  /**
   * Sets the name of this Event to a copy of string.
   */
  LIBSBML_EXTERN
  void setName (const std::string& str);

  /**
   * Sets the trigger of this Event to the given ASTNode.
   *
   * The node <b>is not copied</b> and this Event <b>takes ownership</b> of
   * it; i.e. subsequent calls to this function or a call to Event_free()
   * will free the ASTNode (and any child nodes).
   */
  LIBSBML_EXTERN
  void setTrigger (ASTNode* math);

  /**
   * Sets the delay of this Event to the given ASTNode.
   *
   * The node <b>is not copied</b> and this Event <b>takes ownership</b> of
   * it; i.e. subsequent calls to this function or a call to Event_free()
   * will free the ASTNode (and any child nodes).
   */
  LIBSBML_EXTERN
  void setDelay (ASTNode* math);

  /**
   * Sets the timeUnits of this Event to a copy of sid.
   */
  LIBSBML_EXTERN
  void setTimeUnits (const std::string& sid);

  /**
   * Unsets the id of this Event.
   */
  LIBSBML_EXTERN
  void unsetId ();

  /**
   * Unsets the name of this Event.
   */
  LIBSBML_EXTERN
  void unsetName ();

  /**
   * Unsets the delay of this Event.
   */
  LIBSBML_EXTERN
  void unsetDelay ();

  /**
   * Unsets the timeUnits of this Event.
   */
  LIBSBML_EXTERN
  void unsetTimeUnits ();

  /**
   * Appends the given EventAssignment to this Event.
   */
  LIBSBML_EXTERN
  void addEventAssignment (EventAssignment& ea);

  /**
   * @return the list of EventAssignments for this Event.
   */
  LIBSBML_EXTERN
  ListOf& getListOfEventAssignments ();

  /**
   * @return the list of EventAssignments for this Event.
   */
  LIBSBML_EXTERN
  const ListOf& getListOfEventAssignments () const;

  /**
   * @return the nth EventAssignment of this Event.
   */
  LIBSBML_EXTERN
  EventAssignment* getEventAssignment (unsigned int n) const;

  /**
   * @return the number of EventAssignments in this Event.
   */
  LIBSBML_EXTERN
  unsigned int getNumEventAssignments () const;


protected:

  std::string id;
  std::string name;
  ASTNode*    trigger;
  ASTNode*    delay;
  std::string timeUnits;
  ListOf      eventAssignment;


  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  // Event_hpp
