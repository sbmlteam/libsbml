from libsbml import *

arraysNs = ArraysPkgNamespaces();
doc = SBMLDocument(arraysNs);
doc.setPackageRequired("arrays", True);
model = doc.createModel();

# create compartment
comp = model.createCompartment();
comp.setMetaId("dd");
comp.setId("s");
comp.setConstant(True);

# set dimensions
compPlugin = comp.getPlugin("arrays");
dim = compPlugin.createDimension();
dim.setId("i");
dim.setSize("n");

# create species
species = model.createSpecies();
species.setId("A");
species.setCompartment("s");
species.setHasOnlySubstanceUnits(False);
species.setBoundaryCondition(False);
species.setConstant(False);

splugin = species.getPlugin("arrays");
dim = splugin.createDimension();
dim.setId("i");
dim.setSize("n");

species = model.createSpecies();
species.setId("B");
species.setCompartment("s");
species.setHasOnlySubstanceUnits(False);
species.setBoundaryCondition(False);
species.setConstant(False);

splugin = species.getPlugin("arrays");
dim = splugin.createDimension();
dim.setId("i");
dim.setSize("n");

species = model.createSpecies();
species.setId("C");
species.setCompartment("s");
species.setHasOnlySubstanceUnits(False);
species.setBoundaryCondition(False);
species.setConstant(False);

splugin = species.getPlugin("arrays");
dim = splugin.createDimension();
dim.setId("i");
dim.setSize("n");

# create parameter
param = model.createParameter();
param.setId("n");
param.setValue(100);
param.setConstant(True);

# create reaction
reaction = model.createReaction();
reaction.setId("reaction1");
reaction.setReversible(False);
reaction.setFast(False);

reactionPlugin = reaction.getPlugin("arrays");
dim = reactionPlugin.createDimension();
dim.setId("i");
dim.setSize("n");

speciesRef = reaction.createReactant();
speciesRef.setSpecies("A");
speciesRef.setConstant(False);
refPlugin = speciesRef.getPlugin("arrays");
index = refPlugin.createIndex();
ast = ASTNode(AST_LINEAR_ALGEBRA_SELECTOR);
ci1 = ASTNode(AST_NAME);
ci1.setName("A");
ast.addChild(ci1);
ci2 = ASTNode(AST_NAME);
ci2.setName("i");
ast.addChild(ci2);
index.setMath(ast);

speciesRef = reaction.createProduct();
speciesRef.setSpecies("C");
speciesRef.setConstant(False);
refPlugin = speciesRef.getPlugin("arrays");
index = refPlugin.createIndex();
ast = ASTNode(AST_LINEAR_ALGEBRA_SELECTOR);
ci1 = ASTNode(AST_NAME);
ci1.setName("C");
ast.addChild(ci1);
ci2 = ASTNode(AST_NAME);
ci2.setName("i");
ast.addChild(ci2);
index.setMath(ast);

writeSBMLToFile(doc, "arrays1.xml");
