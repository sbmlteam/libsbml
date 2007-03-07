/**
 * \file    SpeciesType.h
 * \brief   SBML SpeciesType
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


#ifndef SpeciesType_h
#define SpeciesType_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>


class SBMLVisitor;


class LIBSBML_EXTERN SpeciesType : public SBase
{
public:

  /**
   * Creates a new SpeciesType, optionally with its id and name attributes
   * set.
   */
  SpeciesType (const std::string& id = "", const std::string& name = "");

  /**
   * Destroys this SpeciesType.
   */
  virtual ~SpeciesType ();


  /**
  * Copy constructor.
  */
  SpeciesType(const SpeciesType& orig);


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next
   * SpeciesType (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return a (deep) copy of this SpeciesType.
   */
  virtual SBase* clone () const;


  /**
   * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   */
  virtual const std::string& getElementName () const;


protected:
  /**
   * Subclasses should override this method to read (and store) XHTML,
   * MathML, etc. directly from the XMLInputStream.
   *
   * @return true if the subclass read from the stream, false otherwise.
   */
  virtual bool readOtherXML (XMLInputStream& stream);

  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual void readAttributes (const XMLAttributes& attributes);

  /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;
};



class LIBSBML_EXTERN ListOfSpeciesTypes : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfSpeciesTypes.
   */
  virtual SBase* clone () const;

  /**
   * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
   * SBML_UNKNOWN (default).
   */
  virtual SBMLTypeCode_t getItemTypeCode () const;

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   */
  virtual const std::string& getElementName () const;

  /**
   * @return the ordinal position of the element with respect to its
   * siblings or -1 (default) to indicate the position is not significant.
   */
  virtual int getElementPosition () const;


protected:

  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


/**
 * Creates a new SpeciesType and returns a pointer to it.
 */
LIBSBML_EXTERN
SpeciesType_t *
SpeciesType_create (void);

/**
 * Creates a new SpeciesType with the given id and name and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
SpeciesType_t *
SpeciesType_createWith (const char *sid, const char *name);

/**
 * Frees the given SpeciesType.
 */
LIBSBML_EXTERN
void
SpeciesType_free (SpeciesType_t *st);

/**
 * @return a (deep) copy of the given SpeciesType.
 */
LIBSBML_EXTERN
SpeciesType_t *
SpeciesType_clone (const SpeciesType_t *st);


/**
 * @return the id of this SpeciesType.
 */
LIBSBML_EXTERN
const char *
SpeciesType_getId (const SpeciesType_t *st);

/**
 * @return the name of this SpeciesType.
 */
LIBSBML_EXTERN
const char *
SpeciesType_getName (const SpeciesType_t *st);


/**
 * @return true (non-zero) if the id of this SpeciesType has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
SpeciesType_isSetId (const SpeciesType_t *st);

/**
 * @return true (non-zero) if the name of this SpeciesType has been
 * set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
SpeciesType_isSetName (const SpeciesType_t *st);


/**
 * Sets the id of this SpeciesType to a copy of sid.
 */
LIBSBML_EXTERN
void
SpeciesType_setId (SpeciesType_t *st, const char *sid);

/**
 * Sets the name of this SpeciesType to a copy of name.
 */
LIBSBML_EXTERN
void
SpeciesType_setName (SpeciesType_t *st, const char *name);


/**
 * Unsets the name of this SpeciesType.
 */
LIBSBML_EXTERN
void
SpeciesType_unsetName (SpeciesType_t *st);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* SpeciesType_h */
