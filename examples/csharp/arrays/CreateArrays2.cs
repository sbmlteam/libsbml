using libsbmlcs;

class Program
{
  public static void Main(string[] args)
  {
    ArraysPkgNamespaces arraysNs = new ArraysPkgNamespaces();
    SBMLDocument doc = new SBMLDocument(arraysNs);
    doc.setPackageRequired("arrays", true);
    Model model = doc.createModel();

    // create parameters
    Parameter param = model.createParameter();
    param.setId("n");
    param.setValue(10);
    param.setConstant(true);

    param = model.createParameter();
    param.setId("m");
    param.setValue(10);
    param.setConstant(true);

    param = model.createParameter();
    param.setId("x");
    param.setValue(5.7);
    param.setConstant(true);

    ArraysSBasePlugin paramPlugin = (ArraysSBasePlugin) param.getPlugin("arrays");
    Dimension dim = paramPlugin.createDimension();
    dim.setId("i");
    dim.setSize("n");

    param = model.createParameter();
    param.setId("y");
    param.setConstant(false);

    paramPlugin = (ArraysSBasePlugin) param.getPlugin("arrays");
    dim = paramPlugin.createDimension();
    dim.setId("i");
    dim.setSize("n");

    param = model.createParameter();
    param.setId("z");
    param.setConstant(false);

    paramPlugin = (ArraysSBasePlugin) param.getPlugin("arrays");
    dim = paramPlugin.createDimension();
    dim.setId("i");
    dim.setSize("n");

    // create initial assignments

    InitialAssignment assignment = model.createInitialAssignment();
    assignment.setSymbol("y");
    ASTNode ast = new ASTNode(libsbml.AST_REAL);
    ast.setValue(3.2);
    assignment.setMath(ast);

    assignment = model.createInitialAssignment();
    assignment.setSymbol("z");
    ast = new ASTNode(libsbml.AST_REAL);
    ast.setValue(5.7);
    assignment.setMath(ast);

    ArraysSBasePlugin assignmentPlugin = (ArraysSBasePlugin) assignment.getPlugin("arrays");
    dim = assignmentPlugin.createDimension();
    dim.setId("i");
    dim.setSize("m");

    Index index = assignmentPlugin.createIndex();
    ASTNode newAst = new ASTNode(libsbml.AST_FUNCTION);
    newAst.setName("selector");
    ASTNode ci = new ASTNode(libsbml.AST_NAME);
    ci.setName("z");
    newAst.addChild(ci);
    ci = new ASTNode(libsbml.AST_NAME);
    ci.setName("i");
    newAst.addChild(ci);
    index.setMath(newAst);

    assignment = model.createInitialAssignment();
    assignment.setSymbol("z");
    ast = new ASTNode(libsbml.AST_REAL);
    ast.setValue(3.2);
    assignment.setMath(ast);

    assignmentPlugin = (ArraysSBasePlugin) assignment.getPlugin("arrays");
    dim = assignmentPlugin.createDimension();
    dim.setId("i");
    dim.setSize("m");

    index = assignmentPlugin.createIndex();
    newAst = new ASTNode(libsbml.AST_LINEAR_ALGEBRA_SELECTOR);
    ci = new ASTNode(libsbml.AST_NAME);
    ci.setName("z");
    newAst.addChild(ci);
    ASTNode plus = new ASTNode(libsbml.AST_PLUS);

    ci = new ASTNode(libsbml.AST_NAME);
    ci.setName("i");
    plus.addChild(ci);
    ci = new ASTNode(libsbml.AST_NAME);
    ci.setName("m");
    plus.addChild(ci);
    newAst.addChild(plus);
    index.setMath(newAst);

    libsbml.writeSBMLToFile(doc, "arrays2.xml");
  }
}