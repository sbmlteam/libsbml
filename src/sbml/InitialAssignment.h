/**
 * \file    InitialAssignment.h
 * \brief   SBML InitialAssignment
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


#ifndef InitialAssignment_h
#define InitialAssignment_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>


class ASTNode;
class SBMLVisitor;


class LIBSBML_EXTERN InitialAssignment : public SBase
{
public:

  /**
   * Creates a new InitialAssignment, optionally with its symbol attributes
   * set.
   */
  InitialAssignment (const std::string& symbol = "");

  /**
   * Copies this InitialAssignment.
   */
  InitialAssignment (const InitialAssignment& rhs);

  /**
   * Destroys this InitialAssignment.
   */
  virtual ~InitialAssignment ();


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next
   * InitialAssignment (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return a (deep) copy of this InitialAssignment.
   */
  virtual SBase* clone () const;


  /**
   * @return the symbol for this InitialAssignment.
   */
  const std::string& getSymbol () const;

  /**
   * @return the math for this InitialAssignment.
   */
  const ASTNode* getMath () const;

  /**
   * @return the sboTerm of this InitialAssignment as an integer.  If not
   * set, sboTerm will be -1.  Use SBML::sboTermToString() to convert the
   * sboTerm to a zero-padded, seven digit string.
   */
  int getSBOTerm () const;


  /**
   * @return tru if the symbol of this InitialAssignment has been set,
   * false otherwise.
   */
  bool isSetSymbol () const;

  /**
   * @return true if the math for this InitialAssignment has been set,
   * false otherwise.
   */
  bool isSetMath () const;

  /**
   * @return true if the sboTerm of this InitialAssignment has been set,
   * false otherwise.
   */
  bool isSetSBOTerm () const;


  /**
   * Sets the symbol of this InitialAssignment to a copy of sid.
   */
  void setSymbol (const std::string& sid);

  /**
   * Sets the math of this InitialAssignment to a copy of the given
   * ASTNode.
   */
  void setMath (const ASTNode* math);

  /**
   * Sets the sboTerm field of this InitialAssignment to value.
   */
  void setSBOTerm (int sboTerm);


  /**
   * Unsets the sboTerm of this InitialAssignment.
   */
  void unsetSBOTerm ();


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

  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;


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


  ASTNode* mMath;
  int      mSBOTerm;
};



class LIBSBML_EXTERN ListOfInitialAssignments : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfInitialAssignments.
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
 * Creates a new InitialAssignment and returns a pointer to it.
 */
LIBSBML_EXTERN
InitialAssignment_t *
InitialAssignment_create (void);


/**
 * Creates a new InitialAssignment, optionally with its symbol attributes
 * set.
 */
LIBSBML_EXTERN
InitialAssignment_t *
InitialAssignment_createWith (const char *symbol);


/**
 * Frees the given InitialAssignment.
 */
LIBSBML_EXTERN
void
InitialAssignment_free (InitialAssignment_t *ia);


/**
 * @return a (deep) copy of the given InitialAssignment.
 */
LIBSBML_EXTERN
InitialAssignment_t *
InitialAssignment_clone (const InitialAssignment_t *ia);


/**
 * @return the symbol for this InitialAssignment
 */
LIBSBML_EXTERN
const char *
InitialAssignment_getSymbol (const InitialAssignment_t *ia);


/**
 * @return the math for this InitialAssignment.
 */
LIBSBML_EXTERN
const ASTNode_t *
InitialAssignment_getMath (const InitialAssignment_t *ia);


/**
 * @return the sboTerm of this InitialAssignment as an integer.  If not
 * set, sboTerm will be -1.  Use SBML_sboTermToString() to convert the
 * sboTerm to a zero-padded, seven digit string.
 */
LIBSBML_EXTERN
int
InitialAssignment_getSBOTerm (const InitialAssignment_t *ia);


/**
 * @return true (non-zero) if the symbol of this InitialAssignment has been
 * set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
InitialAssignment_isSetSymbol (const InitialAssignment_t *ia);


/**
 * @return true (non-zero) if the math of this InitialAssignment has been
 * set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
InitialAssignment_isSetMath (const InitialAssignment_t *ia);


/**
 * @return true (non-zero) if the sboTerm of this InitialAssignment has
 * been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
InitialAssignment_isSetSBOTerm (const InitialAssignment_t *ia);


/**
 * Sets the symbol of this InitialAssignment to a copy of sid.
 */
LIBSBML_EXTERN
void
InitialAssignment_setSymbol (InitialAssignment_t *ia, const char *sid);


/**
 * Sets the math of this InitialAssignment to a copy of the given
 * ASTNode.
 */
LIBSBML_EXTERN
void
InitialAssignment_setMath (InitialAssignment_t *ia, const ASTNode_t *math);


/**
 * Sets the sboTerm field of this InitialAssignment to value.
 */
LIBSBML_EXTERN
void
InitialAssignment_setSBOTerm (InitialAssignment_t *ia, int sboTerm);


/**
 * Unsets the sboTerm of this InitialAssignment.
 */
LIBSBML_EXTERN
void
InitialAssignment_unsetSBOTerm (InitialAssignment_t *ia);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* InitialAssignment_h */
