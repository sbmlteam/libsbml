/**
 * @file    Compartment.h
 * @brief   SBML Compartment
 * @author  Ben Bornstein
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


#ifndef Compartment_h
#define Compartment_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/annotation/RDFAnnotation.h>

#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


class SBMLVisitor;


class LIBSBML_EXTERN Compartment : public SBase
{
public:

  /**
   * Creates a new Compartment, optionally with its id and name attributes
   * set.
   */
  Compartment (const string& id = "", const string& name = "");

  /**
   * Destroys this Compartment.
   */
  virtual ~Compartment ();


  /**
   * Copy constructor. Creates a copy of this compartment.
   */
  Compartment(const Compartment& orig);


  /**
   * Assignment operator
   */
  Compartment& operator=(const Compartment& orig);

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next
   * Compartment (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return a (deep) copy of this Compartment.
   */
  virtual SBase* clone () const;

  /**
   * Initializes the fields of this Compartment to their defaults:
   *
   *   - volume            = 1.0          (L1 only)
   *   - spatialDimensions = 3            (L2 only)
   *   - constant          = 1    (true)  (L2 only)
   */
  void initDefaults ();


  /**
   * @return the compartmentType of this Compartment.
   */
  const string& getCompartmentType () const;

  /**
   * @return the spatialDimensions of this Compartment.
   */
  unsigned int getSpatialDimensions () const;

  /**
   * @return the size (volume in L1) of this Compartment.
   */
  double getSize () const;

  /**
   * @return the volume (size in L2) of this Compartment.
   */
  double getVolume () const;

  /**
   * @return the units of this Compartment.
   */
  const string& getUnits () const;

  /**
   * @return the outside of this Compartment.
   */
  const string& getOutside () const;

  /**
   * @return true if this Compartment is constant, false otherwise.
   */
  bool getConstant () const;


  /**
   * @return true if the compartmentType of this Compartment has been set,
   * false otherwise.
   */
  bool isSetCompartmentType () const;

  /**
   * @return true if the size (volume in L1) of this Compartment has been
   * set, false otherwise.
   */
  bool isSetSize () const;

  /**
   * @return true if the volume (size in L2) of this Compartment has been
   * set, false otherwise.
   *
   * In SBML L1, a Compartment volume has a default value (1.0) and
   * therefore <b>should always be set</b>.  In L2, volume (size) is
   * optional with no default value and as such may or may not be set.
   */
  bool isSetVolume () const;

  /**
   * @return true if the units of this Compartment has been set, false
   * otherwise.
   */
  bool isSetUnits () const;

  /**
   * @return true if the outside of this Compartment has been set, false
   * otherwise.
   */
  bool isSetOutside () const;


  /**
   * Sets the compartmentType field of this Compartment to a copy of sid.
   */
  void setCompartmentType (const string& sid);

  /**
   * Sets the spatialDimensions of this Compartment to value.
   *
   * If value is not one of [0, 1, 2, 3] the function will have no effect
   * (i.e. spatialDimensions will not be set).
   */
  void setSpatialDimensions (unsigned int value);

  /**
   * Sets the size (volume in L1) of this Compartment to value.
   */
  void setSize (double value);

  /**
   * Sets the volume (size in L2) of this Compartment to value.
   */
  void setVolume (double value);

  /**
   * Sets the units of this Compartment to a copy of sid.
   */
  void setUnits (const string& sid);

  /**
   * Sets the outside of this Compartment to a copy of sid.
   */
  void setOutside (const string& sid);

  /**
   * Sets the constant field of this Compartment to value.
   */
  void setConstant (bool value);


  /**
   * Unsets the compartmentType of this Compartment.
   */
  void unsetCompartmentType ();

  /**
   * Unsets the size (volume in L1) of this Compartment.
   */
  void unsetSize ();

  /**
   * Unsets the volume (size in L2) of this Compartment.
   *
   * In SBML L1, a Compartment volume has a default value (1.0) and
   * therefore <b>should always be set</b>.  In L2, volume is optional with
   * no default value and as such may or may not be set.
   */
  void unsetVolume ();

  /**
   * Unsets the units of this Compartment.
   */
  void unsetUnits ();

  /**
   * Unsets the outside of this Compartment.
   */
  void unsetOutside ();


  /**
   * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;

  /**
   * @return the name of this element ie "compartment".
   */
  virtual const string& getElementName () const;


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

  string        mCompartmentType;
  unsigned int  mSpatialDimensions;
  double        mSize;
  string        mUnits;
  string        mOutside;
  bool          mConstant;

  bool  mIsSetSize;
};



class LIBSBML_EXTERN ListOfCompartments : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfCompartments.
   */
  virtual SBase* clone () const;

  /**
   * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
   * SBML_UNKNOWN (default).
   */
  virtual SBMLTypeCode_t getItemTypeCode () const;

  /**
   * @return the name of this element ie "listOfCompartments".
   */
  virtual const string& getElementName () const;

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


#endif /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


/**
 * Creates a new Compartment and returns a pointer to it.
 */
LIBSBML_EXTERN
Compartment_t *
Compartment_create (void);

/**
 * Creates a new Compartment with the given id and name and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
Compartment_t *
Compartment_createWith (const char *sid, const char *name);

/**
 * Frees the given Compartment.
 */
LIBSBML_EXTERN
void
Compartment_free (Compartment_t *c);

/**
 * @return a (deep) copy of the given Compartment.
 */
LIBSBML_EXTERN
Compartment_t *
Compartment_clone (const Compartment_t* c);

/**
 * Initializes the fields of this Compartment to their defaults:
 *
 *   - volume            = 1.0          (L1 only)
 *   - spatialDimensions = 3            (L2 only)
 *   - constant          = 1    (true)  (L2 only)
 */
LIBSBML_EXTERN
void
Compartment_initDefaults (Compartment_t *c);


/**
 * @return the id of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getId (const Compartment_t *c);

/**
 * @return the name of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getName (const Compartment_t *c);

/**
 * @return the compartmentType of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getCompartmentType (const Compartment_t *c);

/**
 * @return the spatialDimensions of this Compartment.
 */
LIBSBML_EXTERN
unsigned int
Compartment_getSpatialDimensions (const Compartment_t *c);

/**
 * @return the size (volume in L1) of this Compartment.
 */
LIBSBML_EXTERN
double
Compartment_getSize (const Compartment_t *c);

/**
 * @return the volume (size in L2) of this Compartment.
 */
LIBSBML_EXTERN
double
Compartment_getVolume (const Compartment_t *c);

/**
 * @return the units of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getUnits (const Compartment_t *c);

/**
 * @return the outside of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getOutside (const Compartment_t *c);

/**
 * @return true (non-zero) if this Compartment is constant, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Compartment_getConstant (const Compartment_t *c);


/**
 * @return true (non-zero) if the id of this Compartment has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetId (const Compartment_t *c);


/**
 * @return true (non-zero) if the name of this Compartment has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetName (const Compartment_t *c);

/**
 * @return true (non-zero) if the compartmentType of this Compartment has
 * been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetCompartmentType (const Compartment_t *c);

/**
 * @return true (non-zero) if the size (volume in L1) of this Compartment
 * has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetSize (const Compartment_t *c);

/**
 * @return true (non-zero) if the volume (size in L2) of this Compartment
 * has been set, false (0) otherwise.
 *
 * In SBML L1, a Compartment volume has a default value (1.0) and therefore
 * <b>should always be set</b>.  In L2, volume (size) is optional with no
 * default value and as such may or may not be set.
 */
LIBSBML_EXTERN
int
Compartment_isSetVolume (const Compartment_t *c);

/**
 * @return true (non-zero) if the units of this Compartment has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetUnits (const Compartment_t *c);

/**
 * @return true (non-zero) if the outside of this Compartment has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetOutside (const Compartment_t *c);


/**
 * Sets the id of this Compartment to a copy of sid.
 */
LIBSBML_EXTERN
void
Compartment_setId (Compartment_t *c, const char *sid);

/**
 * Sets the name of this Compartment to a copy of string.
 */
LIBSBML_EXTERN
void
Compartment_setName (Compartment_t *c, const char *string);

/**
 * Sets the compartmentType of this Compartment to a copy of sid.
 */
LIBSBML_EXTERN
void
Compartment_setCompartmentType (Compartment_t *c, const char *sid);

/**
 * Sets the spatialDimensions of this Compartment to value.
 *
 * If value is not one of [0, 1, 2, 3] the function will have no effect
 * (i.e. spatialDimensions will not be set).
 */
LIBSBML_EXTERN
void
Compartment_setSpatialDimensions (Compartment_t *c, unsigned int value);

/**
 * Sets the size (volume in L1) of this Compartment to value.
 */
LIBSBML_EXTERN
void
Compartment_setSize (Compartment_t *c, double value);

/**
 * Sets the volume (size in L2) of this Compartment to value.
 */
LIBSBML_EXTERN
void
Compartment_setVolume (Compartment_t *c, double value);

/**
 * Sets the units of this Compartment to a copy of sid.
 */
LIBSBML_EXTERN
void
Compartment_setUnits (Compartment_t *c, const char *sid);

/**
 * Sets the outside of this Compartment to a copy of sid.
 */
LIBSBML_EXTERN
void
Compartment_setOutside (Compartment_t *c, const char *sid);

/**
 * Sets the constant field of this Compartment to value (boolean).
 */
LIBSBML_EXTERN
void
Compartment_setConstant (Compartment_t *c, int value);


/**
 * Unsets the name of this Compartment.
 */
LIBSBML_EXTERN
void
Compartment_unsetName (Compartment_t *c);

/**
 * Unsets the compartmentType of this Compartment.
 */
LIBSBML_EXTERN
void
Compartment_unsetCompartmentType (Compartment_t *c);

/**
 * Unsets the size (volume in L1) of this Compartment.
 */
LIBSBML_EXTERN
void
Compartment_unsetSize (Compartment_t *c);

/**
 * Unsets the volume (size in L2) of this Compartment.
 *
 * In SBML L1, a Compartment volume has a default value (1.0) and therefore
 * <b>should always be set</b>.  In L2, volume (size) is optional with no
 * default value and as such may or may not be set.
 */
LIBSBML_EXTERN
void
Compartment_unsetVolume (Compartment_t *c);

/**
 * Unsets the units of this Compartment.
 */
LIBSBML_EXTERN
void
Compartment_unsetUnits (Compartment_t *c);

/**
 * Unsets the outside of this Compartment.
 */
LIBSBML_EXTERN
void
Compartment_unsetOutside (Compartment_t *c);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* Compartment_h */
