/**
 * Filename    : GraphicalObject.h
 * Description : SBML Layout GraphicalObject C++ Header
 * Organization: European Media Laboratories Research gGmbH
 * Created     : 2004-07-15
 *
 * Copyright 2004 European Media Laboratories Research gGmbH
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * European Media Laboratories Research gGmbH have no obligations to
 * provide maintenance, support, updates, enhancements or modifications.
 * In no event shall the European Media Laboratories Research gGmbH be
 * liable to any party for direct, indirect, special, incidental or
 * consequential damages, including lost profits, arising out of the use of
 * this software and its documentation, even if the European Media
 * Laboratories Research gGmbH have been advised of the possibility of such
 * damage.  See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ralph Gauges
 *     Bioinformatics Group
 *     European Media Laboratories Research gGmbH
 *     Schloss-Wolfsbrunnenweg 31c
 *     69118 Heidelberg
 *     Germany
 *
 *     http://www.eml-research.de/english/Research/BCB/
 *     mailto:ralph.gauges@eml-r.villa-bosch.de
 *
 * Contributor(s):
 */


#ifndef GraphicalObject_H__
#define GraphicalObject_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>



#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/layout/BoundingBox.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN GraphicalObject : public SBase
{
protected:

  std::string mId;

  BoundingBox mBoundingBox;
        

public:

  /**
   * Creates a new GraphicalObject.
   */
  
  GraphicalObject ();
  
  GraphicalObject (unsigned int level, unsigned int version);
  
  GraphicalObject (SBMLNamespaces *sbmlns);

  /**
   * Creates a new GraphicalObject with the given id.
   */
  GraphicalObject (const std::string& id);

  /**
   * Creates a new GraphicalObject with the given id and 2D coordinates for
   * the bounding box.
   */
  
  GraphicalObject (const std::string& id,
                   double x, double y, double w, double h);

  /**
   * Creates a new GraphicalObject with the given id and 3D coordinates for
   * the bounding box.
   */
  
  GraphicalObject (const std::string& id,
                   double x, double y, double z,
                   double w, double h, double d);

  /**
   * Creates a new GraphicalObject with the given id and 3D coordinates for
   * the bounding box.
   */
  
  GraphicalObject (const std::string& id, const Point* p, const Dimensions* d);

  /**
   * Creates a new GraphicalObject with the given id and 3D coordinates for
   * the bounding box.
   */
  
  GraphicalObject (const std::string& id, const BoundingBox* bb);


  /**
   * Creates a new GraphicalObject from the given XMLNode
   */
   GraphicalObject(const XMLNode& node);

  /**
   * Copy constructor.
   */
   GraphicalObject(const GraphicalObject& source);

  /**
   * Assignment operator.
   */
   virtual GraphicalObject& operator=(const GraphicalObject& source);


  /**
   * Destructor.
   */ 
  
  virtual ~GraphicalObject ();

  /**
   * Does nothing. No defaults are defined for GraphicalObject.
   */ 
  
  void initDefaults ();

  /**
   * Returns the value of the "id" attribute of this GraphicalObject.
   */
  const std::string& getId () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * GraphicalObject's "id" attribute has been set.
   */
  bool isSetId () const;

  
  /**
   * Sets the value of the "id" attribute of this GraphicalObject.
   */
  int setId (const std::string& id);


  /**
   * Unsets the value of the "id" attribute of this GraphicalObject.
   */
  void unsetId ();


  
  /**
   * Sets the boundingbox for the GraphicalObject.
   */ 
  
  void setBoundingBox (const BoundingBox* bb);

  /**
   * Returns the bounding box for the GraphicalObject.
   */ 
  
  BoundingBox* getBoundingBox ();

  /**
   * Returns the bounding box for the GraphicalObject.
   */ 
  
  const BoundingBox* getBoundingBox() const;

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

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   */
  virtual const std::string& getElementName () const ;

  /**
   * @return a (deep) copy of this Model.
   */
  virtual SBase* clone () const;

  /**
   * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  SBMLTypeCode_t
  getTypeCode () const;

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the SBML object's next
   * sibling object (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;
    

   /**
    * Creates an XMLNode object from this.
    */
    virtual XMLNode toXML() const;

    /**
     * Returns true if all required attributes are set
     * on the graphical object.
     * Currently the only required attribute is the id.
     */
    virtual bool hasRequiredAttributes() const;


    
protected:
  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase*
  createObject (XMLInputStream& stream);

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


};

LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS



/**
 * Creates a new GraphicalObject.
 */
LIBSBML_EXTERN
GraphicalObject_t *
GraphicalObject_create (void);

/**
 * Creates a new GraphicalObject_t structure using the given SBML @p 
 * level and @p version values and a set of XMLNamespaces.
 *
 * @param level an unsigned int, the SBML Level to assign to this 
 * GraphicalObject
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * GraphicalObject
 * 
 * @param xmlns XMLNamespaces, a pointer to an array of XMLNamespaces to
 * assign to this GraphicalObject
 *
 * @return a pointer to the newly created GraphicalObject_t structure.
 *
 * @note Once a GraphicalObject has been added to an SBMLDocument, the @p 
 * level, @p version and @p xmlns namespaces for the document @em override 
 * those used to create the Reaction.  Despite this, the ability 
 * to supply the values at creation time is an important aid to creating 
 * valid SBML.  Knowledge of the intended SBML Level and Version 
 * determine whether it is valid to assign a particular value to an 
 * attribute, or whether it is valid to add an object to an existing 
 * SBMLDocument.
 */
LIBSBML_EXTERN
GraphicalObject_t *
GraphicalObject_createWithLevelVersionAndNamespaces (unsigned int level,
              unsigned int version);


/**
 * Creates a GraphicalObject from a template.
 */
LIBSBML_EXTERN
GraphicalObject_t *
GraphicalObject_createFrom (const GraphicalObject_t *temp);

/**
 * Frees all memory taken up by the GraphicalObject.
 */ 
LIBSBML_EXTERN
void
GraphicalObject_free (GraphicalObject_t *go);


/**
 * Sets the boundingbox for the GraphicalObject.
 */ 
LIBSBML_EXTERN
void
GraphicalObject_setBoundingBox (GraphicalObject_t *go, const BoundingBox_t *bb);

/**
 * Returns the bounding box for the GraphicalObject.
 */ 
LIBSBML_EXTERN
BoundingBox_t *
GraphicalObject_getBoundingBox (GraphicalObject_t *go);

/**
 * Does nothing. No defaults are defined for GraphicalObject.
 */ 
LIBSBML_EXTERN
void
GraphicalObject_initDefaults (GraphicalObject_t *go);

/**
 * @return a (deep) copy of this Model.
 */
LIBSBML_EXTERN
GraphicalObject_t *
GraphicalObject_clone (const GraphicalObject_t *m);


LIBSBML_EXTERN
int
GraphicalObject_isSetId (const GraphicalObject_t *go);

LIBSBML_EXTERN
const char *
GraphicalObject_getId (const GraphicalObject_t *go);


LIBSBML_EXTERN
int
GraphicalObject_setId (GraphicalObject_t *go, const char *sid);


LIBSBML_EXTERN
void
GraphicalObject_unsetId (GraphicalObject_t *go);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG */
#endif  /* GraphicalObject_H__ */
