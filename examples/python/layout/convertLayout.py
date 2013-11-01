from libsbml import *
doc  = readSBMLFromFile('D:/DropboxSBML/Dropbox/Bug1867-l3.xml')
layoutNsUri = "http://projects.eml.org/bcb/sbml/level2"
layoutNs = LayoutPkgNamespaces(2, 4)
renderNsUri = "http://projects.eml.org/bcb/sbml/render/level2"
renderNs = RenderPkgNamespaces(2, 4)

prop = ConversionProperties(SBMLNamespaces(2,4))
prop.addOption('strict', False)
prop.addOption('setLevelAndVersion', True)
prop.addOption('ignorePackages', True)
doc.convert(prop)


docPlugin = doc.getPlugin("layout")
if docPlugin != None:
	docPlugin.setElementNamespace(layoutNsUri)

doc.getSBMLNamespaces().removePackageNamespace(3, 1, "layout", 1);        
doc.getSBMLNamespaces().addPackageNamespace("layout", 1);        

rdocPlugin = doc.getPlugin("render");
if rdocPlugin!= None:
	rdocPlugin->setElementNamespace(renderNsUri);

doc.getSBMLNamespaces().removePackageNamespace(3, 1, "render", 1);        
doc.getSBMLNamespaces().addPackageNamespace("render", 1);        

print doc.toSBML()

