/**
 * @file    CompartmentGlyph.h
 * @brief   Definition of CompartmentGlyph for SBML Layout.
 * @author  Ralph Gauges
 * 
 * <!--------------------------------------------------------------------------
 * Description : SBML Layout CompartmentGlyph C++ Header
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
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
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
 *
 *     Akiya Jouraku <jouraku@bio.keio.ac.jp>
 *     Modified this file for package extension in libSBML5
 *------------------------------------------------------------------------- -->
 *
 * @class CompartmentGlyph
 * @ingroup layout
 * @brief @htmlinclude pkg-marker-layout.html
 * Representation of a compartment glyph.
 */

#ifndef CompartmentGlyph_H__
#define CompartmentGlyph_H__

#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/layout/common/layoutfwd.h>


#ifdef __cplusplus 


#include <string>
#include <sbml/packages/layout/sbml/GraphicalObject.h>

LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN CompartmentGlyph : public GraphicalObject
{
protected:
  /** @cond doxygenLibsbmlInternal */
  std::string mCompartment;
  double      mOrder;
  bool        mIsSetOrder;

  /** @endcond */
        
  friend class LayoutHandler;


public:
        
  /**
   * Default Constructor which creates a new CompartmentGlyph.  Id and
   * associated compartment id are unset.
   */
  
  CompartmentGlyph (unsigned int level      = LayoutExtension::getDefaultLevel(),
                    unsigned int version    = LayoutExtension::getDefaultVersion(),
                    unsigned int pkgVersion = LayoutExtension::getDefaultPackageVersion());

  /**
   * Ctor.
   */
   CompartmentGlyph(LayoutPkgNamespaces* layoutns);

        
  /**
   * Constructor which creates a new CompartmentGlyph with the given @p id.
   *
   * (FOR BACKWARD COMPATIBILITY)
   *
   */
  
  CompartmentGlyph (LayoutPkgNamespaces* layoutns, const std::string& id);

  /**
   * Constructor which creates a new CompartmentGlyph.  Id and associated
   * compartment id are set to copies of the values given as arguments.
   *
   * (FOR BACKWARD COMPATIBILITY)
   */
  
  CompartmentGlyph (LayoutPkgNamespaces* layoutns, 
                    const std::string& id, const std::string& compartmentId);


  /**
   * Creates a new CompartmentGlyph from the given XMLNode
   *
   * (FOR BACKWARD COMPATIBILITY)
   */
   CompartmentGlyph(const XMLNode& node, unsigned int l2version=4);

  /**
   * Copy constructor.
   */
   CompartmentGlyph(const CompartmentGlyph& source);

  /**
   * Assignment operator.
   */
  virtual  CompartmentGlyph& operator=(const CompartmentGlyph& source);

  /**
   * Destructor.
   */          
  virtual ~CompartmentGlyph ();

  /**
   * Returns the id of the associated compartment.
   */        
  
  const std::string& getCompartmentId () const;
        
  /**
   * Sets the id of the associated compartment.
   */ 
  
  int setCompartmentId (const std::string& id);

  /**
   * Returns true if the id of the associated compartment is not the empty
   * string.
   */  
  
  bool isSetCompartmentId () const;
     
  /**
   * Returns the compartment order.
   */          
  double getOrder () const;
        
  /**
   * Sets the compartment order
   */   
  int setOrder (double order);

  /**
   * Sets the compartment order
   */   
  int unsetOrder ();
  
  /**
   * Returns true if the compartment order has been set
   */    
  bool isSetOrder () const;
	 
  /**
   * Renames all the @c SIdRef attributes on this element, including any
   * found in MathML content (if such exists).
   *
   * This method works by looking at all attributes and (if appropriate)
   * mathematical formulas, comparing the identifiers to the value of @p
   * oldid.  If any matches are found, the matching identifiers are replaced
   * with @p newid.  The method does @em not descend into child elements.
   *
   * @param oldid the old identifier
   * @param newid the new identifier
   */
  virtual void renameSIdRefs(const std::string& oldid, const std::string& newid);

  /**
   * Calls initDefaults from GraphicalObject.
   */ 
  
  void initDefaults ();

  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.  For example:
   *
   *   SBase::writeElements(stream);
   *   mReactants.write(stream);
   *   mProducts.write(stream);
   *   ...
   */
  virtual void writeElements (XMLOutputStream& stream) const;
  /** @endcond */

  /**
   * Returns the XML element name of
   * this SBML object.
   */
  virtual const std::string& getElementName () const ;

  /**
   * @return a (deep) copy of this CompartmentGlyph.
   */
  virtual CompartmentGlyph* clone () const;


  /**
   * Returns the libSBML type code of this object instance.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @link SBMLLayoutTypeCode_t#SBML_LAYOUT_COMPARTMENTGLYPH SBML_LAYOUT_COMPARTMENTGLYPH@endlink
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode () const;


   /**
    * Creates an XMLNode object from this.
    */
    virtual XMLNode toXML() const;
    
protected:
  /** @cond doxygenLibsbmlInternal */
  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase*
  createObject (XMLInputStream& stream);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to get the list of
   * expected attributes.
   * This function is invoked from corresponding readAttributes()
   * function.
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual void readAttributes (const XMLAttributes& attributes, 
                               const ExpectedAttributes& expectedAttributes);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
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
  /** @endcond */
};


#endif /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


/*
 * Creates a new CompartmentGlyph and returns the pointer to it.
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_create ();


/*
 * Creates a new CompartmentGlyph from a template.
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_createFrom (const CompartmentGlyph_t *cg);

/*
 * Creates a new CompartmentGlyph with the given @p sid
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_createWith (const char *sid);

/*
 * Creates a new CompartmentGlyph with the given @p sid
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_createWithCompartmentId (const char *sid, const char *compId);

/*
 * Frees the memory taken by the given compartment glyph.
 */
LIBSBML_EXTERN
void
CompartmentGlyph_free (CompartmentGlyph_t *cg);


/*
 * Sets the reference compartment for the compartment glyph.
 */
LIBSBML_EXTERN
void
CompartmentGlyph_setCompartmentId (CompartmentGlyph_t *cg, const char *id);

/*
 * Gets the reference compartments id for the given compartment glyph.
 */
LIBSBML_EXTERN
const char *
CompartmentGlyph_getCompartmentId (const CompartmentGlyph_t *cg);

/*
 * Returns 0 if the reference compartment has not been set for this glyph
 * and 1 otherwise.
 */
LIBSBML_EXTERN
int
CompartmentGlyph_isSetCompartmentId (const CompartmentGlyph_t *cg);

/*
 * Returns the compartment order.
 */          
LIBSBML_EXTERN
double
CompartmentGlyph_getOrder (const CompartmentGlyph_t *cg);
      
/*
 * Sets the compartment order
 */   
LIBSBML_EXTERN
int
CompartmentGlyph_setOrder (CompartmentGlyph_t *cg, double order);

/*
 * Sets the compartment order
 */   
LIBSBML_EXTERN
int
CompartmentGlyph_unsetOrder (CompartmentGlyph_t *cg);

/*
 * Returns true if the compartment order has been set
 */    
LIBSBML_EXTERN
int
CompartmentGlyph_isSetOrder (const CompartmentGlyph_t *cg);

/*
 * Calls initDefaults from GraphicalObject.
 */ 
LIBSBML_EXTERN
void
CompartmentGlyph_initDefaults (CompartmentGlyph_t *cg);

/*
 * @return a (deep) copy of this CompartmentGlyph.
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_clone (const CompartmentGlyph_t *m);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* CompartmentGlyph_H__ */
