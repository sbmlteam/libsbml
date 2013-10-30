/**
 * @file    SBMLFileResolver.h
 * @brief   Definition of SBMLFileResolver, the file system based resolver for SBML Documents.
 * @author  Frank Bergmann
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
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
 *
 * @class SBMLFileResolver
 * @sbmlpackage comp
 * @brief @htmlinclude pkg-marker-comp.html
 * Base class for SBML resolvers.
 *
 * @htmlinclude libsbml-facility-only-warning.html
 *
 * The SBMLFileResolver class is the class used for the resolving URIs for
 * relative or absolute files for SBML documents.
 */

#ifndef SBMLFileResolver_h
#define SBMLFileResolver_h

#include <sbml/common/sbmlfwd.h>
#ifdef __cplusplus

#include <string>
#include <vector>
#include <sbml/packages/comp/util/SBMLResolver.h>


LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN SBMLFileResolver : public SBMLResolver
{
public:

  /**
   * Creates a new SBMLFileResolver object.
   */
  SBMLFileResolver ();


  /**
   * Copy constructor.  Creates a copy of an SBMLFileResolver object.
   *
   * @param c the SBMLFileResolver object to copy.
   *
   * @throws @if python ValueError @else SBMLConstructorException @endif@~
   * Thrown if the argument @p orig is @c NULL.
   */
  SBMLFileResolver(const SBMLFileResolver& c);


  /**
   * Destroy this SBMLFileResolver object.
   */
  virtual ~SBMLFileResolver ();


  /**
   * Assignment operator for SBMLFileResolver.
   *
   * @param rhs The object whose values are used as the basis of the
   * assignment.
   *
   * @throws @if python ValueError @else SBMLConstructorException @endif@~
   * Thrown if the argument @p rhs is @c NULL.
   */
  SBMLFileResolver& operator=(const SBMLFileResolver& rhs);


  /**
   * Creates and returns a deep copy of this SBMLFileResolver object.
   *
   * @return a (deep) copy of this SBMLFileResolver object.
   */
  virtual SBMLFileResolver* clone() const;


  /**
   * Resolves the document for the given URI.
   *
   * @param uri the URI to the target document
   * @param baseUri base URI, in case the URI is a relative one
   *
   * @return  the document, if this resolver can resolve the document or NULL.
   */
  virtual SBMLDocument* resolve(const std::string &uri, const std::string& baseUri="") const;


  /**
   * Resolves the full URI for the given URI without actually reading the
   * document.
   *
   * @param uri the URI to the target document
   * @param baseUri base URI, in case the URI is a relative one
   *
   * @return  the full URI to the document, if this resolver can resolve the document or NULL.
   */
  virtual SBMLUri* resolveUri(const std::string &uri, const std::string& baseUri="") const;


  /**
   * Sets the list of additional directories in which to search for files to resolve.
   *
   * @param dirs A vector of strings which contain directories
   */
  virtual void setAdditionalDirs(const std::vector<std::string>& dirs);


  /**
   * Removes the list of additional directories in which to search for files
   * to resolve.  Only absolute or relative directories will be searched.
   */
  virtual void clearAdditionalDirs();


  /**
   * Adds a directory to the list of additional directories in which to search for files to resolve.
   *
   * @param dir The directory to add
   */
  virtual void addAdditionalDir(const std::string& dir);

#ifndef SWIG

#endif // SWIG


protected:
  /** @cond doxygenLibsbmlInternal */
  std::vector<std::string> mAdditionalDirs;
  /** @endcond */


private:
  /** @cond doxygenLibsbmlInternal */
  static bool fileExists(const std::string& fileName);
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
#endif  /* SBMLFileResolver_h */

