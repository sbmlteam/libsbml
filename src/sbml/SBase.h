/**
 * @file    SBase.h
 * @brief   Base object of all SBML objects
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


#ifndef SBase_h
#define SBase_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/SBMLTypeCodes.h>
#include <sbml/annotation/RDFAnnotation.h>
#include <sbml/annotation/CVTerm.h>
#include <sbml/util/List.h>


#ifdef __cplusplus


#include <string>


class SBMLErrorLog;
class SBMLVisitor;
class SBMLDocument;
class Model;

class List;

class XMLAttributes;
class XMLInputStream;
class XMLNode;
class XMLNamespaces;
class XMLOutputStream;
class XMLToken;


class LIBSBML_EXTERN SBase
{
public:

  /**
   * Destroy this SBase object.
   */
  virtual ~SBase ();


  /**
   * Assignment operator
   */
  SBase& operator=(const SBase& orig);

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the SBML object's next
   * sibling object (if available).
   */
  virtual bool accept (SBMLVisitor& v) const = 0;

  /**
   * @return a (deep) copy of this SBML object.
   */
  virtual SBase* clone () const = 0;


  /**
   * @return the metaid of this SBML object.
   */
  const std::string& getMetaId () const;


  /**
   * @return the metaid of this SBML object.
   */
  std::string& getMetaId ();

  /**
   * @return the id of this SBML object.
   */
  const std::string& getId () const;

  /**
   * @return the name of this SBML object.
   */
  const std::string& getName () const;

  /**
   * @return the notes of this SBML object.
   */
  XMLNode* getNotes();

  /**
   * @return the annotation of this SBML object.
   */
  XMLNode* getAnnotation ();

  /**
   * @return the Namespaces associated with this SBML object
   */
  virtual XMLNamespaces* getNamespaces() const ;
 
  /**
   * @return the parent SBMLDocument of this SBML object.
   */
  const SBMLDocument* getSBMLDocument () const;

  /**
   * @return the sboTerm as an integer.  If not set,
   * sboTerm will be -1. 
   */
  int getSBOTerm () const;

  /**
   * @return the line number of this SBML object.
   */
  unsigned int getLine () const;

  /**
   * @return the column number of this SBML object.
   */
  unsigned int getColumn () const;

  /**
   * @return true if the metaid of this SBML object has been set, false
   * otherwise.
   */
  bool isSetMetaId () const;

  /**
   * @return true if the id of this SBML object has been set, false
   * otherwise.
   */
  bool isSetId () const;

  /**
   * @return true if the name of this SBML object has been set, false
   * otherwise.
   */
  bool isSetName () const;

  /**
   * @return true if the notes of this SBML object has been set, false
   * otherwise.
   */
  bool isSetNotes () const;

  /**
   * @return true if the annotation of this SBML object has been set,
   * false otherwise.
   */
  bool isSetAnnotation () const;

  /**
   * @return true if the sboTerm has been set, false
   * otherwise.
   */
  bool isSetSBOTerm () const;

  /**
   * Sets the metaid field of the given SBML object to a copy of metaid.
   */
  void setMetaId (const std::string& metaid);

  /**
   * Sets the id of this SBML object to a copy of sid.
   */
  void setId (const std::string& sid);

  /**
   * Sets the name of this SBML object to a copy of name.
   */
  void setName (const std::string& name);

  /**
   * Sets the annotation of this SBML object to a copy of annotation.
   */
  void setAnnotation (XMLNode* annotation);

  /**
   * Appends annotation to the existing annotations.
   * This allows other annotations to be preserved whilst
   * adding additional information.
   */
  void appendAnnotation (XMLNode* annotation);

  /**
   * Sets the notes of this SBML object to a copy of notes.
   */
  void setNotes(XMLNode* notes);

  /**
   * Appends notes to the existing notes.
   * This allows other notes to be preserved whilst
   * adding additional information.
   */
  void appendNotes(XMLNode* notes);

  /**
   * Sets the parent SBMLDocument of this SBML object.
   */
  virtual void setSBMLDocument (SBMLDocument* d);

  /**
   * Sets the sboTerm field to value.
   */
  void setSBOTerm (int value);

  /**
   * Unsets the metaid of this SBML object.
   */
  void unsetMetaId ();

  /**
   * Unsets the id of this SBML object.
   */
  void unsetId ();

  /**
   * Unsets the name of this SBML object.
   */
  void unsetName ();

  /**
   * Unsets the notes of this SBML object.
   */
  void unsetNotes ();

  /**
   * Unsets the annotation of this SBML object.
   */
  void unsetAnnotation ();

  /**
   * Unsets the sboTerm of this SBML object.
   */
  void unsetSBOTerm ();

  /**
   * Adds a copy of the given CVTerm to this SBML object.
   */
  void addCVTerm(CVTerm * term);

  /**
   * @return the list of CVTerms for this SBML object.
   */
  List* getCVTerms();

  /**
   * @return the list of CVTerms for this SBML object.
   */
  List* getCVTerms()  const;

  /**
   * @return the parent Model of this SBML object.
   */
  const Model* getModel () const;

  /**
   * @return the SBML level of this SBML object.
   */
  unsigned int getLevel () const;

  /**
   * @return the SBML version of this SBML object.
   */
  unsigned int getVersion () const;

  /**
   * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * This method MAY return the typecode of this SBML object or it MAY
   * return SBML_UNKNOWN.  That is, subclasses of SBase are not required to
   * implement this method to return a typecode.  This method is meant
   * primarily for the LibSBML C interface where class and subclass
   * information is not readily available.
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;

  /**
   * Subclasses should override this method to return the name
   * of the SBML object.
   */
  virtual const std::string& getElementName () const = 0;


  //TO DO Move these to Model
  /**
   * functions to get and set ModelHistory
   */
  void setModelHistory(ModelHistory * history);

  ModelHistory* getModelHistory() const;
  ModelHistory* getModelHistory();
  
  /**
   * @return the partial SBML that describes this SBML object.
   */
  char* toSBML ();

  /**
   * Reads (initializes) this SBML object by reading from XMLInputStream.
   */
  void read (XMLInputStream& stream);

  /**
   * Writes (serializes) this SBML object by writing it to XMLOutputStream.
   */
  void write (XMLOutputStream& stream) const;

  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.  For example:
   *
   *   SBase::writeElements(stream);
   *   mReactans.write(stream);
   *   mProducts.write(stream);
   *   ...
   */
  virtual void writeElements (XMLOutputStream& stream) const;

protected:

  /**
   * Only subclasses may create SBase objects.
   */
  SBase (const std::string& id = "", const std::string& name = "", int sboTerm = -1);

  /**
   * Creates a new SBase object with the given sboTerm.
   * Only subclasses may create SBase objects.
   */
  SBase (int sboTerm);

  /**
  * Copy constructor. Creates a copy of this SBase object.
  */
  SBase(const SBase& orig);

  /**
   * Subclasses should override this method to create, store, and then
   * return an SBML object corresponding to the next XMLToken in the
   * XMLInputStream.
   *
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);

  /**
   * Subclasses should override this method to read (and store) XHTML,
   * MathML, etc. directly from the XMLInputStream.
   *
   * @return true if the subclass read from the stream, false otherwise.
   */
  virtual bool readOtherXML (XMLInputStream& stream);

  /**
   * The SBML XML Schema is written such that the order of child elements
   * is significant.  LibSBML can read elements out of order.  If you
   * override this method to indicate the ordinal position of element with
   * respect to its siblings, libSBML will log an error if the element is
   * read out of order.
   *
   * @return the ordinal position of the element with respect to its
   * siblings or -1 (default) to indicate the position is not significant.
   */
  virtual int getElementPosition () const;


  /**
   * @return the SBMLErrorLog used to log errors during while reading and
   * validating SBML.
   */
  SBMLErrorLog* getErrorLog ();

  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual
  void readAttributes (const XMLAttributes& attributes);

  /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.  For example:
   *
   *   SBase::writeAttributes(stream);
   *   stream.writeAttribute( "id"  , mId   );
   *   stream.writeAttribute( "name", mName );
   *   ...
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;


  /**
   * Checks that SBML element has been read in the proper order.  If object
   * is not in the expected position, an error is logged.
   */
  void checkOrderAndLogError (SBase* object, int expected);

  /**
   * Checks that an SBML ListOf element has been populated.  
   * If a listOf element has been declared with no elements, 
   * an error is logged.
   */
  void checkListOfPopulated(SBase* object);
  
  /**
    * Checks the syntax of a metaid attribute.
    * The syntax of a metaid is XML 1.0 type ID. The literal representation of 
    * this type consists of strings of characters restricted to:
    *
    *  - NCNameChar ::= letter | digit | '.' | '-' | ' ' | ':' | CombiningChar | Extender
    *  - ID ::= ( letter | ' ' | ':' ) NCNameChar*
    *
    * If the syntax of the metaid attribute of this object is incorrect, 
    * an error is logged
    */
  void checkMetaIdSyntax();

  /**
    * Checks the syntax of the id attribute.
    * The syntax of an id is of type SId which is defined as:
    *
    *  - letter ::= 'a'..'z','A'..'Z'
    *  - digit  ::= '0'..'9'
    *  - idChar ::= letter | digit | '_'
    *  - SId    ::= ( letter | '_' ) idChar*
    *
    * If the syntax of the id attribute of this object is incorrect, 
    * an error is logged
    */
  void checkIdSyntax();

  /**
    * Checks the annotation does not declare an sbml namespace.
    * If the annotation declares an sbml namespace an error is logged.
    */
  void checkAnnotation();

  /**
  * Checks that the XHTML is valid.
  * If the xhtml does not conform to the specification of valid xhtml within
  * an sbml document, an error is logged.
  */
  void checkXHTML(const XMLNode *);

  /**
    * Checks if a character is part of the Unicode Letter set.
    * @return true if the character is a part of the set, false otherwise.
    */
  bool isUnicodeLetter(std::string::iterator, unsigned int);

  /**
    * Checks if a character is part of the Unicode Digit set.
    * @return true if the character is a part of the set, false otherwise.
    */
  bool isUnicodeDigit(std::string::iterator, unsigned int);

  /**
    * Checks if a character is part of the Unicode CombiningChar set.
    * @return true if the character is a part of the set, false otherwise.
    */
  bool isCombiningChar(std::string::iterator, unsigned int);

  /**
    * Checks if a character is part of the Unicode Extender set.
    * @return true if the character is a part of the set, false otherwise.
    */
  bool isExtender(std::string::iterator, unsigned int);

  std::string mMetaId;
  std::string mId;
  std::string mName;

  XMLNode* mNotes;
  XMLNode* mAnnotation;

  XMLNamespaces* mNamespaces;

  SBMLDocument* mSBML;

  int mSBOTerm;

  unsigned int mLine;
  unsigned int mColumn;


  /* storing annotations */
  List * mCVTerms;
  ModelHistory*   mHistory;



private:

  /**
   * Stores the location (line and column) and any XML namespaces (for
   * roundtripping) declared on this SBML (XML) element.
   */
  void setSBaseFields (const XMLToken& element);
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


/**
 * @return the metaid of this SBML object.
 */
LIBSBML_EXTERN
const char *
SBase_getMetaId (const SBase_t *sb);

/**
 * @return the id of this SBML object.
 */
LIBSBML_EXTERN
const char *
SBase_getId (const SBase_t *sb);

/**
 * @return the name of this SBML object.
 */
LIBSBML_EXTERN
const char *
SBase_getName (const SBase_t *sb);


/**
 * @return the parent SBMLDocument of this SBML object.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBase_getSBMLDocument (SBase_t *sb);

/**
 * @return the sboTerm of this SBML object.
 */
LIBSBML_EXTERN
int
SBase_getSBOTerm (const SBase_t *sb);

/**
 * @return the line number of this SBML object.
 */
LIBSBML_EXTERN
unsigned int
SBase_getLine (const SBase_t *sb);

/**
 * @return the column number of this SBML object.
 */
LIBSBML_EXTERN
unsigned int
SBase_getColumn (const SBase_t *sb);


/**
 * @return 1 if the metaid of this SBML object has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetMetaId (const SBase_t *sb);

/**
 * @return 1 if the id of this SBML object has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetId (const SBase_t *sb);

/**
 * @return 1 if the name of this SBML object has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetName (const SBase_t *sb);

/**
 * @return 1 if the notes of this SBML object has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetNotes (const SBase_t *sb);

/**
 * @return 1 if the annotation of this SBML object has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetAnnotation (const SBase_t *sb);


/**
 * @return 1 if the sboTerm of this SBML object has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
SBase_isSetSBOTerm(const SBase_t *sb);


/**
 * Sets the metaid field of the given SBML object to a copy of metaid.
 */
LIBSBML_EXTERN
void
SBase_setMetaId (SBase_t *sb, const char *metaid);

/**
 * Sets the id field of the given SBML object to a copy of sid.
 */
LIBSBML_EXTERN
void
SBase_setId (SBase_t *sb, const char *sid);

/**
 * Sets the name field of the given SBML object to a copy of name.
 */
LIBSBML_EXTERN
void
SBase_setName (SBase_t *sb, const char *name);


/**
 * Sets the sboTerm field of the given SBML object to value.
 */
LIBSBML_EXTERN
void
SBase_setSBOTerm (SBase_t *sb, int value);


/**
 * Unsets the metaid of this SBML object.
 */
LIBSBML_EXTERN
void
SBase_unsetMetaId (SBase_t *sb);

/**
 * Unsets the id of this SBML object.
 */
LIBSBML_EXTERN
void
SBase_unsetId (SBase_t *sb);

/**
 * Unsets the name of this SBML object.
 */
LIBSBML_EXTERN
void
SBase_unsetName (SBase_t *sb);

/**
 * Unsets the notes of this SBML object.
 */
LIBSBML_EXTERN
void
SBase_unsetNotes (SBase_t *sb);

/**
 * Unsets the annotation of this SBML object.
 */
LIBSBML_EXTERN
void
SBase_unsetAnnotation (SBase_t *sb);


/**
 * Unsets the sboTerm of this SBML object.
 */
LIBSBML_EXTERN
void
SBase_unsetSBOTerm (SBase_t *sb);


/**
 * @return the parent Model of this SBML object.
 */
LIBSBML_EXTERN
const Model_t *
SBase_getModel (const SBase_t *sb);

/**
 * @return the SBML level of this SBML object.
 */
LIBSBML_EXTERN
unsigned int
SBase_getLevel (const SBase_t *sb);

/**
 * @return the SBML version of this SBML object.
 */
LIBSBML_EXTERN
unsigned int
SBase_getVersion (const SBase_t *sb);

/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 */
LIBSBML_EXTERN
SBMLTypeCode_t
SBase_getTypeCode (const SBase_t *sb);


/**
 * @return the XML element name of this SBML object.
 */
LIBSBML_EXTERN
const char *
SBase_getElementName (const SBase_t *sb);

END_C_DECLS


#endif  /* !SWIG   */
#endif  /* SBase_h */
