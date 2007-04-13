/**
 * \file    Parameter.h
 * \brief   SBML Parameter
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and Japan Science and
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


#ifndef Parameter_h
#define Parameter_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>


class SBMLVisitor;


class LIBSBML_EXTERN Parameter : public SBase
{
public:

  /**
   * Creates a new Parameter, optionally with its id and name attributes
   * set.
   */
  Parameter (const std::string& id = "", const std::string& name = "");

  /**
   * Creates a new Parameter, with its id and value attributes set and
   * optionally its units and constant attributes.
   */
  Parameter (   const std::string&  id
              , double              value
              , const std::string&  units    = ""
              , bool                constant = true );

  /**
   * Destroys this Parameter.
   */
  virtual ~Parameter ();


  /**
  * Copy constructor. Creates a copy of this Parameter.
  */
  Parameter(const Parameter& orig);


  /**
   * Assignment operator.
   */
  Parameter& operator=(const Parameter& orig);

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the parent Model's or
   * KineticLaw's next Parameter (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return a (deep) copy of this Parameter.
   */
  virtual SBase* clone () const;

  /**
   * Initializes the fields of this Parameter to their defaults:
   *
   *   - constant = true  (L2 only)
   */
  void initDefaults ();


  /**
   * @return the value of this Parameter.
   */
  double getValue () const;

  /**
   * @return the units of this Parameter.
   */
  const std::string& getUnits () const;

  /**
   * @return true if this Parameter is constant, false otherwise.
   */
  bool getConstant () const;

  /**
   * @return true if the value of this Parameter has been set, false
   * otherwise.
   *
   * In SBML L1v1, a Parameter value is required and therefore <b>should
   * always be set</b>.  In L1v2 and beyond, a value is optional and as
   * such may or may not be set.
   */
  bool isSetValue () const;

  /**
   * @return true if the units of this Parameter has been set, false
   * otherwise.
   */
  bool isSetUnits () const;

  /**
   * Sets the initialAmount of this Parameter to value and marks the field
   * as set.
   */
  void setValue (double value);

  /**
   * Sets the units of this Parameter to a copy of sid.
   */
  void setUnits (const std::string& sname);

  /**
   * Sets the constant field of this Parameter to value.
   */
  void setConstant (bool value);

  /**
   * Unsets the value of this Parameter.
   *
   * In SBML L1v1, a Parameter value is required and therefore <b>should
   * always be set</b>.  In L1v2 and beyond, a value is optional and as
   * such may or may not be set.
   */
  void unsetValue ();

  /**
   * Unsets the units of this Parameter.
   */
  void unsetUnits ();

  /**
   * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;

  /**
   * @return the name of this element ie "parameter".
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


  double       mValue;
  std::string  mUnits;
  bool         mConstant;

  bool mIsSetValue;
};



class LIBSBML_EXTERN ListOfParameters : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfParameters.
   */
  virtual SBase* clone () const;

  /**
   * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
   * SBML_UNKNOWN (default).
   */
  virtual SBMLTypeCode_t getItemTypeCode () const;

  /**
 * @return the name of this element ie "listOfParameters".
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
 * Creates a new Parameter and returns a pointer to it.
 */
LIBSBML_EXTERN
Parameter_t *
Parameter_create (void);

/**
 * Creates a new Parameter with the given id, value and units and returns
 * a pointer to it.  This convenience function is functionally equivalent
 * to:
 *
 *   Parameter_t *p = Parameter_create();
 *   Parameter_setId(p, id); Parameter_setValue(p, value); ... ;
 */
LIBSBML_EXTERN
Parameter_t *
Parameter_createWith (const char *sid, double value, const char *units);

/**
 * Frees the given Parameter.
 */
LIBSBML_EXTERN
void
Parameter_free (Parameter_t *p);

/**
 * @return a (deep) copy of the given Parameter.
 */
LIBSBML_EXTERN
Parameter_t *
Parameter_clone (const Parameter_t *p);

/**
 * Initializes the fields of this Parameter to their defaults:
 *
 *   - constant = 1  (true)  (L2 only)
 */
LIBSBML_EXTERN
void
Parameter_initDefaults (Parameter_t *p);


/**
 * @return the id of this Parameter.
 */
LIBSBML_EXTERN
const char *
Parameter_getId (const Parameter_t *p);

/**
 * @return the name of this Parameter.
 */
LIBSBML_EXTERN
const char *
Parameter_getName (const Parameter_t *p);

/**
 * @return the value of this Parameter.
 */
LIBSBML_EXTERN
double
Parameter_getValue (const Parameter_t *p);

/**
 * @return the units of this Parameter.
 */
LIBSBML_EXTERN
const char *
Parameter_getUnits (const Parameter_t *p);

/**
 * @return true (non-zero) if this Parameter is constant, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Parameter_getConstant (const Parameter_t *p);

/**
 * @return true (non-zero) if the id of this Parameter has been set, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
Parameter_isSetId (const Parameter_t *p);

/**
 * @return true (non-zero) if the name of this Parameter has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Parameter_isSetName (const Parameter_t *p);

/**
 * @return true (non-zero) if the value of this Parameter has been set,
 * false (0) otherwise.
 *
 * In SBML L1v1, a Parameter value is required and therefore <b>should
 * always be set</b>.  In L1v2 and beyond, a value is optional and as such
 * may or may not be set.
 */
LIBSBML_EXTERN
int
Parameter_isSetValue (const Parameter_t *p);

/**
 * @return true (non-zero) if the units of this Parameter has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Parameter_isSetUnits (const Parameter_t *p);

/**
 * Sets the id of this Parameter to a copy of sid.
 */
LIBSBML_EXTERN
void
Parameter_setId (Parameter_t *p, const char *sid);

/**
 * Sets the name of this Parameter to a copy of string.
 */
LIBSBML_EXTERN
void
Parameter_setName (Parameter_t *p, const char *string);

/**
 * Sets the value of this Parameter to value and marks the field as set.
 */
LIBSBML_EXTERN
void
Parameter_setValue (Parameter_t *p, double value);

/**
 * Sets the units of this Parameter to a copy of sid.
 */
LIBSBML_EXTERN
void
Parameter_setUnits (Parameter_t *p, const char *sid);

/**
 * Sets the constant of this Parameter to value (boolean).
 */
LIBSBML_EXTERN
void
Parameter_setConstant (Parameter_t *p, int value);

/**
 * Unsets the name of this Parameter.
 */
LIBSBML_EXTERN
void
Parameter_unsetName (Parameter_t *p);

/**
 * Unsets the value of this Parameter.
 *
 * In SBML L1v1, a Parameter value is required and therefore <b>should
 * always be set</b>.  In L1v2 and beyond, a value is optional and as such
 * may or may not be set.
 */
LIBSBML_EXTERN
void
Parameter_unsetValue (Parameter_t *p);


/**
 * Unsets the units of this Parameter.
 */
LIBSBML_EXTERN
void
Parameter_unsetUnits (Parameter_t *p);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* Parameter_h */
