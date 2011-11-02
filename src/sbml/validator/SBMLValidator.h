/**
* @file    SBMLValidator.h
* @brief   Definition of SBMLValidator, the base class for user callable SBML validators.
* @author  Frank Bergmann
* 
* <!--------------------------------------------------------------------------
* This file is part of libSBML.  Please visit http://sbml.org for more
* information about SBML, and the latest version of libSBML.
*
* Copyright (C) 2009-2011 jointly by the following organizations: 
*     1. California Institute of Technology, Pasadena, CA, USA
*     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
*  
* Copyright (C) 2006-2008 by the California Institute of Technology,
*     Pasadena, CA, USA 
*  
* Copyright (C) 2002-2005 jointly by the following organizations: 
*     1. California Institute of Technology, Pasadena, CA, USA
*     2. Japan Science and Technology Agency, Japan
* 
* This library is free software; you can redistribute it and/or modify it
* under the terms of the GNU Lesser General Public License as published by
* the Free Software Foundation.  A copy of the license agreement is provided
* in the file named "LICENSE.txt" included with this software distribution
* and also available online as http://sbml.org/software/libsbml/license.html
* ------------------------------------------------------------------------ -->
*/

#ifndef SBMLValidator_h
#define SBMLValidator_h

#include <sbml/SBMLNamespaces.h>
#include <sbml/SBMLTypes.h>


#ifdef __cplusplus


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN SBMLValidator
{
public:

  /**
  * Constructor.
  */
  SBMLValidator ();

  /**
  * Copy constructor.
  */
  SBMLValidator(const SBMLValidator&);

  /**
  * Destroy this object.
  */
  virtual ~SBMLValidator ();

  /**
  * Assignment operator for SBMLConverter.
  */
  SBMLValidator& operator=(const SBMLValidator&);

  /**
  * Creates and returns a deep copy of this SBMLConverter.
  * 
  * @return a (deep) copy of this SBMLConverter.
  */
  virtual SBMLValidator* clone() const;

  /**
   * @return the current SBML document
   */
  virtual SBMLDocument* getDocument();

  /**
   * @return a const reference to the current SBML document
   */
  virtual const SBMLDocument* getDocument() const;

  /** 
   * Sets the current SBML document
   * 
   * @param doc the document to use for this validation
   * 
   * @return status code
   */
  virtual int setDocument(const SBMLDocument* doc);

  /** 
   * the actual validation code 
   * 
   * @return number of validation errors encountered
   */
  virtual unsigned int validate(); 

  
  /**
   * Clears the Validator's list of failures.
   *
   * If you are validating multiple SBML documents with the same Validator,
   * call this method after you have processed the list of failures from
   * the last Validation run and before validating the next document.
   */
  virtual void clearFailures ();

    /**
   * Get the list of SBMLError objects (if any) logged as a result
   * of running the validator.
   * 
   * @return a list of failures logged during validation.
   */
  const std::vector<SBMLError>& getFailures () const;


  /**
   * Adds the given failure to this list of Validators failures.
   */
  void logFailure (const SBMLError& msg);

    /**
   * Validates the given SBMLDocument.  Failures logged during
   * validation may be retrieved via getFailures().
   *
   * @return the number of validation errors that occurred.
   */
  unsigned int validate (const SBMLDocument& d);


  /**
   * Validates the given SBMLDocument.  Failures logged during
   * validation may be retrieved via getFailures().
   *
   * @return the number of validation errors that occurred.
   */
  unsigned int validate (const std::string& filename);

  /**
   * Returns the list of errors or warnings logged during parsing, 
   * consistency checking, or attempted translation of this model.
   * 
   * @return the SBMLErrorLog used for the SBMLDocument
   * 
   * @see SBMLDocument::getNumErrors()
   */
  SBMLErrorLog* getErrorLog ();

  /**
   * Returns the Model object stored in the SBMLDocument.
   *
   * It is important to note that this method <em>does not create</em> a
   * Model instance.  The model in the SBMLDocument must have been created
   * at some prior time, for example using SBMLDocument::createModel() 
   * or SBMLDocument::setModel(@if java Model m@endif).
   * This method returns @c NULL if a model does not yet exist.
   * 
   * @return the Model contained in the SBMLDocument.
   *
   */
  const Model* getModel () const;


  /**
   * Returns the Model object stored in the SBMLDocument.
   *
   * It is important to note that this method <em>does not create</em> a
   * Model instance.  The model in the SBMLDocument must have been created
   * at some prior time, for example using SBMLDocument::createModel() 
   * or SBMLDocument::setModel(@if java Model m@endif).
   * This method returns @c NULL if a model does not yet exist.
   * 
   * @return the Model contained in the SBMLDocument.
   *
   */
  Model* getModel ();

  /** 
   * Returns the local number of failures
   * 
   * This method returns the number of failures logged by this validator 
   * (that is it does not include the number of the documents error log).
   *
   * @return the number of errors logged by this validator. 
   */
  unsigned int getNumFailures() const;

  /** 
   * Returns the failure at the given index. 
   *
   * @param n the zero based index of failures
   * 
   * @return the failure at the given index
   */
  SBMLError* getFailure (unsigned int n) const;


#ifndef SWIG

#endif // SWIG



protected:
  /** @cond doxygen-libsbml-internal */
  std::vector<SBMLError>  mFailures;
  SBMLDocument *   mDocument;
  friend class SBMLDocument;
  /** @endcond */


private:
  /** @cond doxygen-libsbml-internal */


  /** @endcond */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /* SBMLValidator_h */


