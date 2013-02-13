/**
 * @file    CompartmentGlyph.cpp
 * @brief   Implementation of CompartmentGlyph for SBML Layout.
 * @author  Ralph Gauges
 * 
 * <!--------------------------------------------------------------------------
 * Description : SBML Layout CompartmentGlyph source
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
 *
 */


#include <sbml/packages/layout/sbml/CompartmentGlyph.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>

#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/packages/layout/extension/LayoutExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Default Constructor which creates a new CompartmentGlyph.  Id and
 * associated compartment id are unset.
 */
CompartmentGlyph::CompartmentGlyph (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : GraphicalObject(level,version,pkgVersion)
   ,mCompartment("")
{
  //
  // (NOTE) Developers don't have to invoke setSBMLNamespacesAndOwn function as follows (commentted line)
  //        in this constuctor because the function is properly invoked in the constructor of the
  //        base class (GraphicalObject).
  //

  //setSBMLNamespacesAndOwn(new LayoutPkgNamespaces(level,version,pkgVersion));  
}

CompartmentGlyph::CompartmentGlyph(LayoutPkgNamespaces* layoutns)
 : GraphicalObject(layoutns)
  ,mCompartment("")
{
  //
  // (NOTE) Developers don't have to invoke setElementNamespace function as follows (commentted line)
  //        in this constuctor because the function is properly invoked in the constructor of the
  //        base class (GraphicalObject).
  //

  // setElementNamespace(layoutns->getURI());

  //
  // load package extensions bound with this object (if any) 
  //
  loadPlugins(layoutns);
}


/*
 * Constructor which creates a new CompartmentGlyph with the given @p id.
 */
CompartmentGlyph::CompartmentGlyph (LayoutPkgNamespaces* layoutns, const std::string& id)
  : GraphicalObject(layoutns)
  ,mCompartment("")
{
  //
  // (NOTE) Developers don't have to invoke setElementNamespace function as follows (commentted line)
  //        in this constuctor because the function is properly invoked in the constructor of the
  //        base class (GraphicalObject).
  //

  // setElementNamespace(layoutns->getURI());

  //
  // load package extensions bound with this object (if any) 
  //
  loadPlugins(layoutns);
}

/*
 * Constructor which creates a new CompartmentGlyph.  Id and associated
 * compartment id are set to copies of the values given as arguments.
 */
CompartmentGlyph::CompartmentGlyph (LayoutPkgNamespaces* layoutns, const std::string& id, const std::string& compId)
  : GraphicalObject(layoutns)
  , mCompartment(compId)
{
  //
  // (NOTE) Developers don't have to invoke setElementNamespace function as follows (commentted line)
  //        in this constuctor because the function is properly invoked in the constructor of the
  //        base class (GraphicalObject).
  //

  // setElementNamespace(layoutns->getURI());

  //
  // load package extensions bound with this object (if any) 
  //
  loadPlugins(layoutns);
}

/*
 * Creates a new CompartmentGlyph from the given XMLNode
 */
CompartmentGlyph::CompartmentGlyph(const XMLNode& node, unsigned int l2version)
  : GraphicalObject(node,l2version)
   ,mCompartment("")
{
    const XMLAttributes& attributes=node.getAttributes();
    ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->readAttributes(attributes,ea);    
}

/*
 * Copy constructor.
 */
CompartmentGlyph::CompartmentGlyph(const CompartmentGlyph& source):GraphicalObject(source)
{
    this->mCompartment=source.getCompartmentId();
}

/*
 * Assignment operator.
 */
CompartmentGlyph& CompartmentGlyph::operator=(const CompartmentGlyph& source)
{
  if(&source!=this)
  {
    GraphicalObject::operator=(source);
    this->mCompartment=source.getCompartmentId();    
  }
  
  return *this;
}

/*
 * Destructor.
 */        
CompartmentGlyph::~CompartmentGlyph ()
{
} 


/*
 * Returns the id of the associated compartment.
 */        
const std::string&
CompartmentGlyph::getCompartmentId () const
{
  return this->mCompartment;
}


/*
 * Sets the id of the associated compartment.
 */ 
int
CompartmentGlyph::setCompartmentId (const std::string& id)
{
  if (!(SyntaxChecker::isValidInternalSId(id)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCompartment = id;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Returns true if the id of the associated compartment is not the empty
 * string.
 */  
bool 
CompartmentGlyph::isSetCompartmentId () const
{
  return ! this->mCompartment.empty();
}


/*
 * Calls initDefaults from GraphicalObject.
 */ 
void CompartmentGlyph::initDefaults ()
{
  GraphicalObject::initDefaults();
}

/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.  For example:
 *
 *   SBase::writeElements(stream);
 *   mReactans.write(stream);
 *   mProducts.write(stream);
 *   ...
 */
void CompartmentGlyph::writeElements (XMLOutputStream& stream) const
{
  GraphicalObject::writeElements(stream);
  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}

/*
 * Returns the XML element name of
 * this SBML object.
 */
const std::string& CompartmentGlyph::getElementName () const 
{
  static const std::string name = "compartmentGlyph";
  return name;
}

/*
 * @return a (deep) copy of this CompartmentGlyph.
 */
CompartmentGlyph* 
CompartmentGlyph::clone () const
{
    return new CompartmentGlyph(*this);
}


/** @cond doxygen-libsbml-internal */
SBase*
CompartmentGlyph::createObject (XMLInputStream& stream)
{
  //const std::string& name   = stream.peek().getName();
  SBase*        object = 0;

  object=GraphicalObject::createObject(stream);
  
  return object;
}
/** @endcond */

/** @cond doxygen-libsbml-internal */
void
CompartmentGlyph::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GraphicalObject::addExpectedAttributes(attributes);

  attributes.add("compartment");
}
/** @endcond */

/** @cond doxygen-libsbml-internal */
void CompartmentGlyph::readAttributes (const XMLAttributes& attributes,
                                       const ExpectedAttributes& expectedAttributes)
{
  GraphicalObject::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("compartment", mCompartment, getErrorLog(), false, getLine(), getColumn());
  if (assigned && mCompartment.empty())
    {
      logEmptyString(mCompartment, sbmlLevel, sbmlVersion, "<compartmentGlyph>");
    }
  if (!SyntaxChecker::isValidInternalSId(mCompartment)) logError(InvalidIdSyntax);
}
/** @endcond */

/** @cond doxygen-libsbml-internal */
void CompartmentGlyph::writeAttributes (XMLOutputStream& stream) const
{
  GraphicalObject::writeAttributes(stream);
  if(this->isSetCompartmentId())
  {
    stream.writeAttribute("compartment", getPrefix(), mCompartment);
  }

  //
  // (EXTENSION) will be written by GraphicalObject!
  //
  //SBase::writeExtensionAttributes(stream);

}
/** @endcond */

/*
 * Returns the package type code for this object.
 */
int
CompartmentGlyph::getTypeCode () const
{
  return SBML_LAYOUT_COMPARTMENTGLYPH;
}


/*
 * Creates an XMLNode object from this.
 */
XMLNode CompartmentGlyph::toXML() const
{
  return getXmlNodeForSBase(this);
}


/**
 * Creates a new CompartmentGlyph and returns the pointer to it.
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_create(void)
{
  return new(std::nothrow) CompartmentGlyph;
}


/**
 * Creates a new CompartmentGlyph from a template.
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_createFrom (const CompartmentGlyph_t *temp)
{
  return new(std::nothrow) CompartmentGlyph(*temp);
}


/**
 * Creates a new CompartmentGlyph with the given @p id
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_createWith (const char *id)
{
  LayoutPkgNamespaces layoutns;
  return new(std::nothrow) CompartmentGlyph(&layoutns, id ? id : "", "");
}


/**
 * Creates a new CompartmentGlyph with the given @p id
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_createWithCompartmentId (const char *sid, const char *compId)
{
  LayoutPkgNamespaces layoutns;
  return new(std::nothrow) CompartmentGlyph(&layoutns, sid ? sid : "", compId ? compId : "");
}


/**
 * Frees the memory taken by the given compartment glyph.
 */
LIBSBML_EXTERN
void
CompartmentGlyph_free (CompartmentGlyph_t *cg)
{
  delete cg;
}


/**
 * Sets the reference compartment for the compartment glyph.
 */
LIBSBML_EXTERN
void
CompartmentGlyph_setCompartmentId (CompartmentGlyph_t *cg, const char* id)
{
    static_cast<CompartmentGlyph*>(cg)->setCompartmentId( id ? id : "" );
}


/**
 * Gets the reference compartments id for the given compartment glyph.
 */
LIBSBML_EXTERN
const char *
CompartmentGlyph_getCompartmentId (const CompartmentGlyph_t *cg)
{
    return cg->isSetCompartmentId() ? cg->getCompartmentId().c_str() : NULL;
}


/**
 * Returns 0 if the reference compartment has not been set for this glyph
 * and 1 otherwise.
 */
LIBSBML_EXTERN
int
CompartmentGlyph_isSetCompartmentId (const CompartmentGlyph_t *cg)
{
  return static_cast<int>( cg->isSetCompartmentId() );
}


/**
 * Calls initDefaults from GraphicalObject.
 */ 
LIBSBML_EXTERN
void
CompartmentGlyph_initDefaults (CompartmentGlyph_t *cg)
{
  cg->initDefaults();
}

/**
 * @return a (deep) copy of this CompartmentGlyph.
 */
LIBSBML_EXTERN
CompartmentGlyph_t *
CompartmentGlyph_clone (const CompartmentGlyph_t *m)
{
  return static_cast<CompartmentGlyph*>( m->clone() );
}

LIBSBML_CPP_NAMESPACE_END
