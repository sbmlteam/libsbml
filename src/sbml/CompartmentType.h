/**
 * \file    CompartmentType.h
 * \brief   SBML CompartmentType
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


#ifndef CompartmentType_h
#define CompartmentType_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>


class SBMLVisitor;


class LIBSBML_EXTERN CompartmentType : public SBase
{
public:

  /**
   * Creates a new CompartmentType, optionally with its id and name attributes
   * set.
   */
  CompartmentType (const std::string& id = "", const std::string& name = "");

  /**
   * Destroys this CompartmentType.
   */
  virtual ~CompartmentType ();


  /**
  * Copy constructor.
  */
  CompartmentType(const CompartmentType& orig);


  /**
   * Assignment operator
   */
  CompartmentType& operator=(const CompartmentType& orig);

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next
   * CompartmentType (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return a (deep) copy of this CompartmentType.
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



class LIBSBML_EXTERN ListOfCompartmentTypes : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfCompartmentTypes.
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
 * Creates a new CompartmentType and returns a pointer to it.
 */
LIBSBML_EXTERN
CompartmentType_t *
CompartmentType_create (void);

/**
 * Creates a new CompartmentType with the given id and name and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
CompartmentType_t *
CompartmentType_createWith (const char *sid, const char *name);

/**
 * Frees the given CompartmentType.
 */
LIBSBML_EXTERN
void
CompartmentType_free (CompartmentType_t *ct);

/**
 * @return a (deep) copy of the given CompartmentType.
 */
LIBSBML_EXTERN
CompartmentType_t *
CompartmentType_clone (const CompartmentType_t *ct);


/**
 * @return the id of this CompartmentType.
 */
LIBSBML_EXTERN
const char *
CompartmentType_getId (const CompartmentType_t *ct);

/**
 * @return the name of this CompartmentType.
 */
LIBSBML_EXTERN
const char *
CompartmentType_getName (const CompartmentType_t *ct);


/**
 * @return true (non-zero) if the id of this CompartmentType has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
CompartmentType_isSetId (const CompartmentType_t *ct);

/**
 * @return true (non-zero) if the name of this CompartmentType has been
 * set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
CompartmentType_isSetName (const CompartmentType_t *ct);


/**
 * Sets the id of this CompartmentType to a copy of sid.
 */
LIBSBML_EXTERN
void
CompartmentType_setId (CompartmentType_t *ct, const char *sid);

/**
 * Sets the name of this CompartmentType to a copy of name.
 */
LIBSBML_EXTERN
void
CompartmentType_setName (CompartmentType_t *ct, const char *name);


/**
 * Unsets the name of this CompartmentType.
 */
LIBSBML_EXTERN
void
CompartmentType_unsetName (CompartmentType_t *ct);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* CompartmentType_h */
