/**
 * \file    SBase.cpp
 * \brief   Base object of all SBML objects
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


#include "common.h"

#include "SBMLFormatter.h"
#include "SBase.h"


SBase::SBase (): line(0), column(0), mNamespaces(0)
{
}


SBase::~SBase ()
{
  delete mNamespaces;
}


/**
 * SBase "objects" are abstract, i.e., they are not created.  Rather,
 * specific "subclasses" are created (e.g., Model) and their SBASE_FIELDS
 * are initialized with this function.  The type of the specific "subclass"
 * is indicated by the given SBMLTypeCode.
 */
void
SBase::init (SBMLTypeCode_t tc)
{
  typecode = tc;
  line     = 0;
  column   = 0;
}


/**
 * @return the type of this SBML object.
 */
LIBSBML_EXTERN
SBMLTypeCode_t
SBase::getTypeCode () const
{
  return typecode;
}


/**
 * @return the column number for this SBML object.
 */
LIBSBML_EXTERN
unsigned int
SBase::getColumn () const
{
  return column;
}


/**
 * @return the line number for this SBML object.
 */
LIBSBML_EXTERN
unsigned int
SBase::getLine () const
{
  return line;
}


/**
 * @return the metaid for this SBML object.
 */
LIBSBML_EXTERN
const std::string&
SBase::getMetaId () const
{
  return metaid;
}


/**
 * @return the notes for this SBML object.
 */
LIBSBML_EXTERN
const std::string&
SBase::getNotes () const
{
  return notes;
}


/**
 * @return the annotation for this SBML object.
 */
LIBSBML_EXTERN
const std::string&
SBase::getAnnotation () const
{
  return annotation;
}


/**
 * @return a list of XML namespaces defined on this SBML object.
 */
LIBSBML_EXTERN
XMLNamespaceList&
SBase::getNamespaces ()
{
  if (mNamespaces == NULL) mNamespaces = new XMLNamespaceList;
  return *mNamespaces;
}


/**
 * @return true if this SBML object has any XML namespaces defined on it,
 * false otherwise.
 */
LIBSBML_EXTERN
bool
SBase::hasNamespaces () const
{
  return (mNamespaces != NULL);
}


/**
 * @return true if the metaid for this SBML object has been set, false
 * otherwise.
 */
LIBSBML_EXTERN
bool
SBase::isSetMetaId () const
{
  return ! metaid.empty();
}


/**
 * @return true if the notes for this SBML object has been set, false
 * otherwise.
 */
LIBSBML_EXTERN
bool
SBase::isSetNotes () const
{
  return ! notes.empty();
}


/**
 * @return true if the annotation for this SBML object has been set, false
 * otherwise.
 */
bool
SBase::isSetAnnotation () const
{
  return ! annotation.empty();
}


/**
 * Sets the metaid field of the given SBML object to a copy of metaid.  If
 * object already has a metaid, the existing string is freed before the new
 * one is copied.
 */
LIBSBML_EXTERN
void
SBase::setMetaId (const std::string& id)
{
  metaid = id;
}


/**
 * Sets the notes field of the given SBML object to a copy of notes.  If
 * object already has notes, the existing string is freed before the new
 * one is copied.
 */
LIBSBML_EXTERN
void
SBase::setNotes (const std::string& xhtml)
{
  notes = xhtml;
}


/**
 * Sets the annotation field of the given SBML object to a copy of
 * annotations.  If object already has an annotation, the existing string
 * is freed before the new one is copied.
 */
LIBSBML_EXTERN
void
SBase::setAnnotation (const std::string& xml)
{
  annotation = xml;
}


/**
 * @return the partial SBML that describes this SBML object.
 */
LIBSBML_EXTERN
char*
SBase::toSBML (unsigned int level, unsigned int version)
{
  MemBufFormatTarget* target;
  SBMLFormatter*      formatter;
  char*               result;


#ifndef USE_EXPAT
  XMLPlatformUtils::Initialize();
#endif  // USE_EXPAT

  target    = new MemBufFormatTarget();
  formatter = new SBMLFormatter("UTF-8", target, false);

  *formatter <<
    ((level == 1) ? SBMLFormatter::Level1 : SBMLFormatter::Level2);

  *formatter <<
    ((version == 1) ? SBMLFormatter::Version1 : SBMLFormatter::Version2);

  switch ( this->getTypeCode() )
  {
    case SBML_COMPARTMENT:
      *formatter << static_cast<Compartment&>(*this);
      break;

    case SBML_DOCUMENT:
      *formatter << static_cast<SBMLDocument&>(*this);
      break;

    case SBML_EVENT:
      *formatter << static_cast<Event&>(*this);
      break;
      
    case SBML_EVENT_ASSIGNMENT:
      *formatter << static_cast<Event&>(*this);
      break;

    case SBML_FUNCTION_DEFINITION:
      *formatter << static_cast<FunctionDefinition&>(*this);
      break;

    case SBML_KINETIC_LAW:
      *formatter << static_cast<KineticLaw&>(*this);
      break;

    case SBML_MODEL:
      *formatter << static_cast<Model&>(*this);
      break;
      
    case SBML_PARAMETER:
      *formatter << static_cast<Parameter&>(*this);
      break;
      
    case SBML_REACTION:
      *formatter << static_cast<Reaction&>(*this);
      break;      

    case SBML_SPECIES:
      *formatter << static_cast<Species&>(*this);
      break;

    case SBML_SPECIES_REFERENCE:
      *formatter << static_cast<SpeciesReference&>(*this);
      break;

    case SBML_MODIFIER_SPECIES_REFERENCE:
      *formatter << static_cast<ModifierSpeciesReference&>(*this);
      break;      

    case SBML_UNIT_DEFINITION:
      *formatter << static_cast<UnitDefinition&>(*this);
      break;

    case SBML_UNIT:
      *formatter << static_cast<Unit&>(*this);
      break;
      
    case SBML_ALGEBRAIC_RULE:
      *formatter << static_cast<AlgebraicRule&>(*this);
      break;
      
    case SBML_ASSIGNMENT_RULE:
      *formatter << static_cast<AssignmentRule&>(*this);
      break;

    case SBML_RATE_RULE:
      *formatter << static_cast<RateRule&>(*this);
      break;
      
    case SBML_SPECIES_CONCENTRATION_RULE:
      *formatter << static_cast<SpeciesConcentrationRule&>(*this);
      break;
      
    case SBML_COMPARTMENT_VOLUME_RULE:
      *formatter << static_cast<CompartmentVolumeRule&>(*this);
      break;

    case SBML_PARAMETER_RULE:
      *formatter << static_cast<ParameterRule&>(*this);
      break;

    case SBML_LIST_OF:
      break;
  }

  result = safe_strdup( (char *) target->getRawBuffer() );

  delete target;
  delete formatter;

  return result;
}


/**
 * Unsets the metaid for this SBML object.
 */
LIBSBML_EXTERN
void
SBase::unsetMetaId ()
{
  metaid.erase();
}


/**
 * Unsets the notes for this SBML object.
 */
LIBSBML_EXTERN
void
SBase::unsetNotes ()
{
  notes.erase();
}

/**
 * Unsets the annotation for this SBML object.
 */
LIBSBML_EXTERN
void
SBase::unsetAnnotation ()
{
  annotation.erase();
}


/**
 * SBase "objects" are abstract, i.e., they are not created.  Rather,
 * specific "subclasses" are created (e.g., Model) and their SBASE_FIELDS
 * are initialized with this function.  The type of the specific "subclass"
 * is indicated by the given SBMLTypeCode.
 */
LIBSBML_EXTERN
void
SBase_init (SBase_t *sb, SBMLTypeCode_t tc)
{
  static_cast<SBase*>(sb)->init(tc);
}


/**
 * Clears (frees) only the SBASE_FIELDS of sb.
 */
LIBSBML_EXTERN
void
SBase_clear (SBase_t *sb)
{
  if (sb == NULL) return;


  SBase *x = static_cast<SBase*>(sb);


  x->unsetMetaId();
  x->unsetNotes();
  x->unsetAnnotation();
}


/**
 * @return the type of this SBML object.
 */
LIBSBML_EXTERN
SBMLTypeCode_t
SBase_getTypeCode (const SBase_t *sb)
{
  return static_cast<const SBase*>(sb)->getTypeCode();
}


/**
 * @return the column number for this SBML object.
 */
LIBSBML_EXTERN
unsigned int
SBase_getColumn (const SBase_t *sb)
{
  return static_cast<const SBase*>(sb)->getColumn();
}


/**
 * @return the line number for this SBML object.
 */
LIBSBML_EXTERN
unsigned int
SBase_getLine (const SBase_t *sb)
{
  return static_cast<const SBase*>(sb)->getLine();
}


/**
 * @return the metaid for this SBML object.
 */
LIBSBML_EXTERN
const char *
SBase_getMetaId (const SBase_t *sb)
{
  const SBase *x = static_cast<const SBase*>(sb);


  return x->isSetMetaId() ? x->getMetaId().c_str() : NULL;
}


/**
 * @return the notes for this SBML object.
 */
LIBSBML_EXTERN
const char *
SBase_getNotes (const SBase_t *sb)
{
  const SBase *x = static_cast<const SBase*>(sb);


  return x->isSetNotes() ? x->getNotes().c_str() : NULL;
}


/**
 * @return the annotation for this SBML object.
 */
LIBSBML_EXTERN
const char *
SBase_getAnnotation (const SBase_t *sb)
{
  const SBase *x = static_cast<const SBase*>(sb);


  return x->isSetAnnotation() ? x->getAnnotation().c_str() : NULL;
}


/**
 * @return 1 if the metaid for this SBML object has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetMetaId (const SBase_t *sb)
{
  return static_cast<const SBase*>(sb)->isSetMetaId();
}


/**
 * @return 1 if the notes for this SBML object has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetNotes (const SBase_t *sb)
{
  return static_cast<const SBase*>(sb)->isSetNotes();
}


/**
 * @return 1 if the annotation for this SBML object has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetAnnotation (const SBase_t *sb)
{
  return static_cast<const SBase*>(sb)->isSetAnnotation();
}


/**
 * Sets the metaid field of the given SBML object to a copy of metaid.  If
 * object already has a metaid, the existing string is freed before the new
 * one is copied.
 */
LIBSBML_EXTERN
void
SBase_setMetaId (SBase_t *sb, const char *metaid)
{
  if (metaid == NULL)
  {
    static_cast<SBase*>(sb)->unsetMetaId();
  }
  else
  {
    static_cast<SBase*>(sb)->setMetaId(metaid);
  }
}


/**
 * Sets the notes field of the given SBML object to a copy of notes.  If
 * object already has notes, the existing string is freed before the new
 * one is copied.
 */
LIBSBML_EXTERN
void
SBase_setNotes (SBase_t *sb, const char *notes)
{
  if (notes == NULL)
  {
    static_cast<SBase*>(sb)->unsetNotes();
  }
  else
  {
    static_cast<SBase*>(sb)->setNotes(notes);
  }
}


/**
 * Sets the annotation field of the given SBML object to a copy of
 * annotations.  If object already has an annotation, the existing string
 * is freed before the new one is copied.
 */
LIBSBML_EXTERN
void
SBase_setAnnotation (SBase_t *sb, const char *annotation)
{
  if (annotation == NULL)
  {
    static_cast<SBase*>(sb)->unsetAnnotation();
  }
  else
  {
    static_cast<SBase*>(sb)->setAnnotation(annotation);
  }
}


/**
 * Unsets the metaid for this SBML object.  This is equivalent to:
 * safe_free(sb->metaid); s->metaid = NULL;
 */
LIBSBML_EXTERN
void
SBase_unsetMetaId (SBase_t *sb)
{
  static_cast<SBase*>(sb)->unsetMetaId();
}


/**
 * Unsets the notes for this SBML object.  This is equivalent to:
 * safe_free(sb->notes); s->notes = NULL;
 */
LIBSBML_EXTERN
void
SBase_unsetNotes (SBase_t *sb)
{
  static_cast<SBase*>(sb)->unsetNotes();
}


/**
 * Unsets the annotation for this SBML object.  This is equivalent to:
 * safe_free(sb->annotation); s->annotation = NULL;
 */
LIBSBML_EXTERN
void
SBase_unsetAnnotation (SBase_t *sb)
{
  static_cast<SBase*>(sb)->unsetAnnotation();
}
