/**
 * \file    FunctionDefinition.h
 * \brief   SBML FunctionDefinition
 * \author  Ben Bornstein
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


#ifndef FunctionDefinition_h
#define FunctionDefinition_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>


class ASTNode;
class SBMLVisitor;


class LIBSBML_EXTERN FunctionDefinition : public SBase
{
public:

  /**
   * Creates a new FunctionDefinition, optionally with its id and math
   * attributes set.
   */
  FunctionDefinition (const std::string& id, const ASTNode* math);

  /**
   * Creates a new FunctionDefinition, optionally with its id and math (via
   * an infix formula string) attributes set.
   */
  FunctionDefinition (  const std::string& id      = ""
                      , const std::string& formula = "" );

  /**
   * Copies this FunctionDefinition.
   */
  FunctionDefinition (const FunctionDefinition& rhs);

  /**
   * Destroys this FunctionDefinition.
   */
  virtual ~FunctionDefinition ();


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next
   * FunctionDefinition (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return a (deep) copy of this FunctionDefinition.
   */
  virtual SBase* clone () const;


  /**
   * @return the math of this FunctionDefinition.
   */
  const ASTNode* getMath () const;

  /**
   * @return true if the math of this FunctionDefinition has been set, false
   * otherwise.
   */
  bool isSetMath () const;

  /**
   * Sets the math of this FunctionDefinition to the given ASTNode.
   */
  void setMath (const ASTNode* math);


  /**
   * @return the nth argument (bound variable) passed to this
   * FunctionDefinition.
   */
  const ASTNode* getArgument (unsigned int n) const;

  /**
   * @return the argument (bound variable) in this FunctionDefinition with
   * the given name or NULL if no such argument exists.
   */
  const ASTNode* getArgument (const std::string& name) const;

  /**
   * @return the body of this FunctionDefinition, or NULL if no body is
   * defined.
   */
  const ASTNode* getBody () const;

  /**
   * @return the number of arguments (bound variables) that must be passed
   * to this FunctionDefinition.
   */
  unsigned int getNumArguments () const;


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


  ASTNode*  mMath;
};



class LIBSBML_EXTERN ListOfFunctionDefinitions : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfFunctionDefinitions.
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
 * Creates a new FunctionDefinition and returns a pointer to it.
 */
LIBSBML_EXTERN
FunctionDefinition_t *
FunctionDefinition_create (void);


/**
 * Creates a new FunctionDefinition with the given id and math and returns
 * a pointer to it.  This convenience function is functionally equivalent
 * to:
 *
 *   fd = FunctionDefinition_create();
 *   FunctionDefinition_setId(fd, id); FunctionDefinition_setMath(fd, math);
 */
LIBSBML_EXTERN
FunctionDefinition_t *
FunctionDefinition_createWith (const char *sid, ASTNode_t *math);


/**
 * Frees the given FunctionDefinition.
 */
LIBSBML_EXTERN
void
FunctionDefinition_free (FunctionDefinition_t *fd);


/**
 * @return the id of this FunctionDefinition.
 */
LIBSBML_EXTERN
const char *
FunctionDefinition_getId (const FunctionDefinition_t *fd);


/**
 * @return the name of this FunctionDefinition.
 */
LIBSBML_EXTERN
const char *
FunctionDefinition_getName (const FunctionDefinition_t *fd);


/**
 * @return the math of this FunctionDefinition.
 */
LIBSBML_EXTERN
const ASTNode_t *
FunctionDefinition_getMath (const FunctionDefinition_t *fd);


/**
 * @return 1 if the id of this FunctionDefinition has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
FunctionDefinition_isSetId (const FunctionDefinition_t *fd);


/**
 * @return 1 if the name of this FunctionDefinition has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
FunctionDefinition_isSetName (const FunctionDefinition_t *fd);


/**
 * @return 1 if the math of this FunctionDefinition has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
FunctionDefinition_isSetMath (const FunctionDefinition_t *fd);


/**
 * Sets the id of this FunctionDefinition to a copy of sid.
 */
LIBSBML_EXTERN
void
FunctionDefinition_setId (FunctionDefinition_t *fd, const char *sid);


/**
 * Sets the name of this FunctionDefinition to a copy of name.
 */
LIBSBML_EXTERN
void
FunctionDefinition_setName (FunctionDefinition_t *fd, const char *name);


/**
 * Sets the math of this FunctionDefinition to the given ASTNode.
 */
LIBSBML_EXTERN
void
FunctionDefinition_setMath (FunctionDefinition_t *fd, const ASTNode_t *math);


/**
 * Unsets the name of this FunctionDefinition.
 */
LIBSBML_EXTERN
void
FunctionDefinition_unsetName (FunctionDefinition_t *fd);


/**
 * @return the nth argument (bound variable) passed to this
 * FunctionDefinition.
 */
LIBSBML_EXTERN
const ASTNode_t *
FunctionDefinition_getArgument(const FunctionDefinition_t *fd, unsigned int n);


/**
 * @return the argument (bound variable) in this FunctionDefinition with
 * the given name or NULL if no such argument exists.
 */
LIBSBML_EXTERN
const ASTNode_t *
FunctionDefinition_getArgumentByName (  FunctionDefinition_t *fd
                                      , const char *name );


/**
 * @return the body of this FunctionDefinition, or NULL if no body is
 * defined.
 */
LIBSBML_EXTERN
const ASTNode_t *
FunctionDefinition_getBody (const FunctionDefinition_t *fd);


/**
 * @return the number of arguments (bound variables) that must be passed
 * to this FunctionDefinition.
 */
LIBSBML_EXTERN
unsigned int
FunctionDefinition_getNumArguments (const FunctionDefinition_t *fd);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* FunctionDefinition_h */
