class MultiPkgNamespaces(SBMLNamespaces):
    """
    @sbmlpackage{multi}
    
    @htmlinclude pkg-marker-multi.html SBMLNamespaces extension for the package.
    
    @htmlinclude not-sbml-warning.html
    
    SBML Level&nbsp;3 &ldquo;packages&rdquo; add features on top of SBML
    Level&nbsp;3 Core.  When a model definition uses an SBML package, it must
    declare the Level and Version of SBML Core and the Version of the package
    being used.  The package name, the SBML Level and Version, and the package
    Version correspond uniquely to an XML namespace added to the XML encoding
    of the SBML model.
    
    LibSBML Level&nbsp;3 <em>extensions</em> are implementations of support
    for SBML Level&nbsp;3 packages.  Each package is implemented as a separate
    extension.  To allow software applications to query the level and version
    information of an extension's package implementation, libSBML uses
    specialized object classes.  For the extension implementing the SBML
    &ldquo;multi&rdquo; package, the object class is MultiPkgNamespaces.
    (This class is a specialization of a common base class called
    <code>SBMLExtensionNamespaces</code> that is not exposed in the libSBML
    programming language interfaces other than C++.)
    
    Objects of class MultiPkgNamespaces can be passed to constructors of SBML
    components defined by &ldquo;multi&rdquo; in order to ensure that the
    correct component structure is created.  This is necessary because
    different versions of an SBML Level&nbsp;3 package may introduce
    differences in the definition of the components defined by the package.
    (For example, later editions of a package may introduce new attributes on
    a component that are not present in earlier editions of the package
    specification.)
    
    @see CompPkgNamespaces
    @see FbcPkgNamespaces
    @see LayoutPkgNamespaces
    @see QualPkgNamespaces
    @see GroupsPkgNamespaces
    """
    def clone(self):
        """
        clone(MultiPkgNamespaces self) -> MultiPkgNamespaces

        Creates and returns a deep copy of this SBMLExtensionNamespaces.

        @return a (deep) copy of this SBMLExtensionNamespaces.

        """

    def getURI(self):
        """
        getURI(MultiPkgNamespaces self) -> string

        Returns a string representing the SBML XML namespace of this
        object.

        @return a string representing the SBML namespace that reflects the
        SBML Level and Version of this object.

        """

    def getPackageName(self):
        """
        getPackageName(MultiPkgNamespaces self) -> string

        Returns the name of the main package for this namespace.

        @return the name of the main package for this namespace.
        'core' will be returned if this namespace is defined in the SBML 
        core.

        """
