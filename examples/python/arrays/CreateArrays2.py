from libsbml import *

arraysNs = ArraysPkgNamespaces();
doc = SBMLDocument(arraysNs);
doc.setPackageRequired("arrays", True);
model = doc.createModel();

# create parameters
param = model.createParameter();
param.setId("n");
param.setValue(10);
param.setConstant(True);

param = model.createParameter();
param.setId("m");
param.setValue(10);
param.setConstant(True);

param = model.createParameter();
param.setId("x");
param.setValue(5.7);
param.setConstant(True);

paramPlugin = param.getPlugin("arrays");
dim = paramPlugin.createDimension();
dim.setId("i");
dim.setSize("n");

param = model.createParameter();
param.setId("y");
param.setConstant(False);

paramPlugin = param.getPlugin("arrays");
dim = paramPlugin.createDimension();
dim.setId("i");
dim.setSize("n");

param = model.createParameter();
param.setId("z");
param.setConstant(False);

paramPlugin = param.getPlugin("arrays");
dim = paramPlugin.createDimension();
dim.setId("i");
dim.setSize("n");

# create initial assignments

assignment = model.createInitialAssignment();
assignment.setSymbol("y");
ast = ASTNode(AST_REAL);
ast.setValue(3.2);
assignment.setMath(ast);

assignment = model.createInitialAssignment();
assignment.setSymbol("z");
ast = ASTNode(AST_REAL);
ast.setValue(5.7);
assignment.setMath(ast);

assignmentPlugin = assignment.getPlugin("arrays");
dim = assignmentPlugin.createDimension();
dim.setId("i");
dim.setSize("m");

index = assignmentPlugin.createIndex();
newAst = ASTNode(AST_LINEAR_ALGEBRA_SELECTOR);
ci1 = ASTNode(AST_NAME);
ci1.setName("z");
newAst.addChild(ci1);
ci2 = ASTNode(AST_NAME);
ci2.setName("i");
newAst.addChild(ci2);
index.setMath(newAst);

assignment = model.createInitialAssignment();
assignment.setSymbol("z");
ast = ASTNode(AST_REAL);
ast.setValue(3.2);
assignment.setMath(ast);

assignmentPlugin = assignment.getPlugin("arrays");
dim = assignmentPlugin.createDimension();
dim.setId("i");
dim.setSize("m");

index = assignmentPlugin.createIndex();
newAst = ASTNode(AST_LINEAR_ALGEBRA_SELECTOR);
ci = ASTNode(AST_NAME);
ci.setName("z");
newAst.addChild(ci);
plus = ASTNode(AST_PLUS);

ci1 = ASTNode(AST_NAME);
ci1.setName("i");
plus.addChild(ci1);
ci2 = ASTNode(AST_NAME);
ci2.setName("m");
plus.addChild(ci2);
newAst.addChild(plus);
index.setMath(newAst);

writeSBMLToFile(doc, "arrays2.xml");
