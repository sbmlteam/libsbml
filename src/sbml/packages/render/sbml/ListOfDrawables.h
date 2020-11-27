/**
 * @file ListOfDrawables.h
 * @brief Definition of the ListOfDrawables class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class ListOfDrawables
 * @sbmlbrief{render} A list of Transformation2D objects.
 * 
 * The ListOfDrawables is a container for Transformation2D elements.  It is
 * implemented in libSBML only, and does not appear in the "render"
 * specification, where the RenderGroup object contains child
 * Transformation2D objects directly.
 * 
 * @copydetails doc_what_is_listof
 *
 * @htmlinclude not-sbml-warning.html
 *
 * @see Transformation2D
 * @see RenderGroup
 */


#ifndef ListOfDrawables_H__
#define ListOfDrawables_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/ListOf.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/Transformation2D.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class Image;
class Ellipse;
class Rectangle;
class Polygon;
class RenderGroup;
class LineEnding;
class Text;
class RenderCurve;

class LIBSBML_EXTERN ListOfDrawables : public ListOf
{

public:

  /**
   * Creates a new ListOfDrawables using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * ListOfDrawables.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * ListOfDrawables.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this ListOfDrawables.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfDrawables(unsigned int level = RenderExtension::getDefaultLevel(),
                  unsigned int version = RenderExtension::getDefaultVersion(),
                  unsigned int pkgVersion =
                    RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new ListOfDrawables using the given RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  ListOfDrawables(RenderPkgNamespaces *renderns);


  /**
   * Copy constructor for ListOfDrawables.
   *
   * @param orig the ListOfDrawables instance to copy.
   */
  ListOfDrawables(const ListOfDrawables& orig);


  /**
   * Assignment operator for ListOfDrawables.
   *
   * @param rhs the ListOfDrawables object whose values are to be used as the
   * basis of the assignment.
   */
  ListOfDrawables& operator=(const ListOfDrawables& rhs);


  /**
   * Creates and returns a deep copy of this ListOfDrawables object.
   *
   * @return a (deep) copy of this ListOfDrawables object.
   */
  virtual ListOfDrawables* clone() const;


  /**
   * Destructor for ListOfDrawables.
   */
  virtual ~ListOfDrawables();


  /**
   * Get a Transformation2D from the ListOfDrawables.
   *
   * @param n an unsigned int representing the index of the Transformation2D to
   * retrieve.
   *
   * @return the nth Transformation2D in this ListOfDrawables.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addTransformation2D(const Transformation2D* object)
   * @see createCurve() 
   * @see createEllipse() 
   * @see createGroup() 
   * @see createImage() 
   * @see createLineEnding() 
   * @see createPolygon() 
   * @see createRectangle() 
   * @see createText()
   * @see get(const std::string& sid)
   * @see getNumTransformation2Ds()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual Transformation2D* get(unsigned int n);


  /**
   * Get a Transformation2D from the ListOfDrawables.
   *
   * @param n an unsigned int representing the index of the Transformation2D to
   * retrieve.
   *
   * @return the nth Transformation2D in this ListOfDrawables.
   * If the index @p n is invalid, @c NULL is returned.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addTransformation2D(const Transformation2D* object)
   * @see createCurve() 
   * @see createEllipse() 
   * @see createGroup() 
   * @see createImage() 
   * @see createLineEnding() 
   * @see createPolygon() 
   * @see createRectangle() 
   * @see createText()
   * @see get(const std::string& sid)
   * @see getNumTransformation2Ds()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const Transformation2D* get(unsigned int n) const;


  /**
   * Get a Transformation2D from the ListOfDrawables based on its identifier.
   *
   * @param sid a string representing the identifier of the Transformation2D to
   * retrieve.
   *
   * @return the Transformation2D in this ListOfDrawables with the given @p sid
   * or @c NULL if no such Transformation2D exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addTransformation2D(const Transformation2D* object)
   * @see createCurve() 
   * @see createEllipse() 
   * @see createGroup() 
   * @see createImage() 
   * @see createLineEnding() 
   * @see createPolygon() 
   * @see createRectangle() 
   * @see createText()
   * @see get(unsigned int n)
   * @see getNumTransformation2Ds()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual Transformation2D* get(const std::string& sid);


  /**
   * Get a Transformation2D from the ListOfDrawables based on its identifier.
   *
   * @param sid a string representing the identifier of the Transformation2D to
   * retrieve.
   *
   * @return the Transformation2D in this ListOfDrawables with the given @p sid
   * or @c NULL if no such Transformation2D exists.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addTransformation2D(const Transformation2D* object)
   * @see createCurve() 
   * @see createEllipse() 
   * @see createGroup() 
   * @see createImage() 
   * @see createLineEnding() 
   * @see createPolygon() 
   * @see createRectangle() 
   * @see createText()
   * @see get(unsigned int n)
   * @see getNumTransformation2Ds()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  virtual const Transformation2D* get(const std::string& sid) const;


  /**
   * Removes the nth Transformation2D from this ListOfDrawables and returns a
   * pointer to it.
   *
   * @param n an unsigned int representing the index of the Transformation2D to
   * remove.
   *
   * @return a pointer to the nth Transformation2D in this ListOfDrawables.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addTransformation2D(const Transformation2D* object)
   * @see createCurve() 
   * @see createEllipse() 
   * @see createGroup() 
   * @see createImage() 
   * @see createLineEnding() 
   * @see createPolygon() 
   * @see createRectangle() 
   * @see createText()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumTransformation2Ds()
   * @see remove(const std::string& sid)
   */
  virtual Transformation2D* remove(unsigned int n);


  /**
   * Removes the Transformation2D from this ListOfDrawables based on its
   * identifier and returns a pointer to it.
   *
   * @param sid a string representing the identifier of the Transformation2D to
   * remove.
   *
   * @return the Transformation2D in this ListOfDrawables based on the
   * identifier or NULL if no such Transformation2D exists.
   *
   * @copydetails doc_warning_returns_owned_pointer
   *
   * @see addTransformation2D(const Transformation2D* object)
   * @see createCurve()
   * @see createEllipse()
   * @see createGroup() 
   * @see createImage() 
   * @see createLineEnding() 
   * @see createPolygon() 
   * @see createRectangle() 
   * @see createText()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumTransformation2Ds()
   * @see remove(unsigned int n)
   */
  virtual Transformation2D* remove(const std::string& sid);


  /**
   * Adds a copy of the given Transformation2D to this ListOfDrawables.
   *
   * @param td the Transformation2D object to add.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_LEVEL_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_VERSION_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_PKG_VERSION_MISMATCH, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_DUPLICATE_OBJECT_ID, OperationReturnValues_t}
   *
   * @copydetails doc_note_object_is_copied
   *
   * @see createCurve() 
   * @see createEllipse() 
   * @see createGroup() 
   * @see createImage() 
   * @see createLineEnding() 
   * @see createPolygon() 
   * @see createRectangle()
   * @see createText()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumTransformation2Ds()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  int addTransformation2D(const Transformation2D* td);


  /**
   * Get the number of Transformation2D objects in this ListOfDrawables.
   *
   * @return the number of Transformation2D objects in this ListOfDrawables.
   *
   *
   * @see addTransformation2D(const Transformation2D* object)
   * @see createCurve() 
   * @see createEllipse() 
   * @see createGroup() 
   * @see createImage() 
   * @see createLineEnding() 
   * @see createPolygon() 
   * @see createRectangle()
   * @see createText()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  unsigned int getNumTransformation2Ds() const;


  /**
   * Creates a new Image object, adds it to this ListOfDrawables object and
   * returns the Image object created.
   *
   * @return a new Image object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addTransformation2D(const Transformation2D* object)
   * @see createCurve() 
   * @see createEllipse() 
   * @see createGroup() 
   * @see createLineEnding() 
   * @see createPolygon() 
   * @see createRectangle() 
   * @see createText()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumTransformation2Ds()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  Image* createImage();


  /**
   * Creates a new Ellipse object, adds it to this ListOfDrawables object and
   * returns the Ellipse object created.
   *
   * @return a new Ellipse object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addTransformation2D(const Transformation2D* object)
   * @see createCurve() 
   * @see createGroup() 
   * @see createImage() 
   * @see createLineEnding() 
   * @see createPolygon() 
   * @see createRectangle() 
   * @see createText()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumTransformation2Ds()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  Ellipse* createEllipse();


  /**
   * Creates a new Rectangle object, adds it to this ListOfDrawables object and
   * returns the Rectangle object created.
   *
   * @return a new Rectangle object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addTransformation2D(const Transformation2D* object)
   * @see createCurve() 
   * @see createEllipse() 
   * @see createGroup() 
   * @see createImage() 
   * @see createLineEnding() 
   * @see createPolygon() 
   * @see createText()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumTransformation2Ds()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  Rectangle* createRectangle();


  /**
   * Creates a new Polygon object, adds it to this ListOfDrawables object and
   * returns the Polygon object created.
   *
   * @return a new Polygon object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addTransformation2D(const Transformation2D* object)
   * @see createCurve() 
   * @see createEllipse() 
   * @see createGroup() 
   * @see createImage() 
   * @see createLineEnding() 
   * @see createRectangle() 
   * @see createText()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumTransformation2Ds()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  Polygon* createPolygon();


  /**
   * Creates a new RenderGroup object, adds it to this ListOfDrawables object
   * and returns the RenderGroup object created.
   *
   * @return a new RenderGroup object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addTransformation2D(const Transformation2D* object)
   * @see createCurve() 
   * @see createEllipse() 
   * @see createImage() 
   * @see createLineEnding() 
   * @see createPolygon() 
   * @see createRectangle() 
   * @see createText()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumTransformation2Ds()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  RenderGroup* createGroup();


  /**
   * Creates a new LineEnding object, adds it to this ListOfDrawables object
   * and returns the LineEnding object created.
   *
   * @return a new LineEnding object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addTransformation2D(const Transformation2D* object)
   * @see createCurve() 
   * @see createEllipse() 
   * @see createGroup() 
   * @see createImage() 
   * @see createPolygon() 
   * @see createRectangle() 
   * @see createText()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumTransformation2Ds()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  LineEnding* createLineEnding();


  /**
   * Creates a new Text object, adds it to this ListOfDrawables object and
   * returns the Text object created.
   *
   * @return a new Text object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addTransformation2D(const Transformation2D* object)
   * @see createCurve() 
   * @see createEllipse() 
   * @see createGroup() 
   * @see createImage() 
   * @see createLineEnding() 
   * @see createPolygon() 
   * @see createRectangle() 
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumTransformation2Ds()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  Text* createText();


  /**
   * Creates a new RenderCurve object, adds it to this ListOfDrawables object
   * and returns the RenderCurve object created.
   *
   * @return a new RenderCurve object instance.
   *
   * @copydetails doc_returned_unowned_pointer
   *
   * @see addTransformation2D(const Transformation2D* object)
   * @see createEllipse() 
   * @see createGroup() 
   * @see createImage() 
   * @see createLineEnding() 
   * @see createPolygon() 
   * @see createRectangle() 
   * @see createText()
   * @see get(const std::string& sid)
   * @see get(unsigned int n)
   * @see getNumTransformation2Ds()
   * @see remove(const std::string& sid)
   * @see remove(unsigned int n)
   */
  RenderCurve* createCurve();


  /**
   * Returns the XML element name of this ListOfDrawables object.
   *
   * For ListOfDrawables, the XML element name is always @c "listOfDrawables".
   *
   * @return the name of this element, i.e. @c "listOfDrawables".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this ListOfDrawables object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_LIST_OF, SBMLTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   */
  virtual int getTypeCode() const;


  /**
   * Returns the libSBML type code for the SBML objects contained in this
   * ListOfDrawables object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML typecode for the objects contained in this
   * ListOfDrawables:
   * @sbmlconstant{SBML_RENDER_TRANSFORMATION2D, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getItemTypeCode() const;




  #ifndef SWIG




  #endif /* !SWIG */


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Creates a new Transformation2D in this ListOfDrawables
   */
  virtual SBase* createObject(XMLInputStream& stream);

  /** @endcond */


  friend class RenderGroup;

  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the namespace for the Render package
   */
  virtual void writeXMLNS(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * checks concrete types
   */
  virtual bool isValidTypeForList(SBase* item);

  /** @endcond */


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Get a Transformation2D_t from the ListOf_t.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the Transformation2D_t to
 * retrieve.
 *
 * @return the nth Transformation2D_t in this ListOf_t.
 * If the index @p n is invalid, @c NULL is returned.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfDrawables_t
 */
LIBSBML_EXTERN
Transformation2D_t*
ListOfDrawables_getTransformation2D(ListOf_t* lo, unsigned int n);


/**
 * Get a Transformation2D_t from the ListOf_t based on its identifier.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the Transformation2D_t to
 * retrieve.
 *
 * @return the Transformation2D_t in this ListOf_t with the given @p sid or
 * @c NULL if no such Transformation2D_t exists.
 *
 * @copydetails doc_returned_unowned_pointer
 *
 * @memberof ListOfDrawables_t
 */
LIBSBML_EXTERN
Transformation2D_t*
ListOfDrawables_getById(ListOf_t* lo, const char *sid);


/**
 * Removes the nth Transformation2D_t from this ListOf_t and returns a pointer
 * to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param n an unsigned int representing the index of the Transformation2D_t to
 * remove.
 *
 * @return a pointer to the nth Transformation2D_t in this ListOf_t.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfDrawables_t
 */
LIBSBML_EXTERN
Transformation2D_t*
ListOfDrawables_remove(ListOf_t* lo, unsigned int n);


/**
 * Removes the Transformation2D_t from this ListOf_t based on its identifier
 * and returns a pointer to it.
 *
 * @param lo the ListOf_t structure to search.
 *
 * @param sid a string representing the identifier of the Transformation2D_t to
 * remove.
 *
 * @return the Transformation2D_t in this ListOf_t based on the identifier or
 * NULL if no such Transformation2D_t exists.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof ListOfDrawables_t
 */
LIBSBML_EXTERN
Transformation2D_t*
ListOfDrawables_removeById(ListOf_t* lo, const char* sid);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !ListOfDrawables_H__ */


