using libsbmlcs;

class Program
{
  public static void Main(string[] args)
  {
    ArraysPkgNamespaces arraysNs = new ArraysPkgNamespaces();
    SBMLDocument doc = new SBMLDocument(arraysNs);
    doc.setPackageRequired("arrays", true);
    Model model = doc.createModel();

    // create compartment
    Compartment comp = model.createCompartment();
    comp.setMetaId("dd");
    comp.setId("s");
    comp.setConstant(true);

    // set dimensions
    ArraysSBasePlugin compPlugin = (ArraysSBasePlugin) comp.getPlugin("arrays");
    Dimension dim = compPlugin.createDimension();
    dim.setId("i");
    dim.setSize("n");

    // create species
    Species species = model.createSpecies();
    species.setId("A");
    species.setCompartment("s");
    species.setHasOnlySubstanceUnits(false);
    species.setBoundaryCondition(false);
    species.setConstant(false);

    ArraysSBasePlugin splugin = (ArraysSBasePlugin) species.getPlugin("arrays");
    dim = splugin.createDimension();
    dim.setId("i");
    dim.setSize("n");

    species = model.createSpecies();
    species.setId("B");
    species.setCompartment("s");
    species.setHasOnlySubstanceUnits(false);
    species.setBoundaryCondition(false);
    species.setConstant(false);

    splugin = (ArraysSBasePlugin) species.getPlugin("arrays");
    dim = splugin.createDimension();
    dim.setId("i");
    dim.setSize("n");

    species = model.createSpecies();
    species.setId("C");
    species.setCompartment("s");
    species.setHasOnlySubstanceUnits(false);
    species.setBoundaryCondition(false);
    species.setConstant(false);

    splugin = (ArraysSBasePlugin) species.getPlugin("arrays");
    dim = splugin.createDimension();
    dim.setId("i");
    dim.setSize("n");

    // create parameter
    Parameter param = model.createParameter();
    param.setId("n");
    param.setValue(100);
    param.setConstant(true);

    // create reaction
    Reaction reaction = model.createReaction();
    reaction.setId("reaction1");
    reaction.setReversible(false);
    reaction.setFast(false);

    ArraysSBasePlugin reactionPlugin = (ArraysSBasePlugin) reaction.getPlugin("arrays");
    dim = reactionPlugin.createDimension();
    dim.setId("i");
    dim.setSize("n");

    SpeciesReference speciesRef = reaction.createReactant();
    speciesRef.setSpecies("A");
    speciesRef.setConstant(false);
    ArraysSBasePlugin refPlugin = (ArraysSBasePlugin) speciesRef.getPlugin("arrays");
    Index index = refPlugin.createIndex();
    ASTNode ast = new ASTNode(libsbml.AST_LINEAR_ALGEBRA_SELECTOR);
    ASTNode ci = new ASTNode(libsbml.AST_NAME);
    ci.setName("A");
    ast.addChild(ci);
    ci = new ASTNode(libsbml.AST_NAME);
    ci.setName("i");
    ast.addChild(ci);
    index.setMath(ast);

    speciesRef = reaction.createProduct();
    speciesRef.setSpecies("C");
    speciesRef.setConstant(false);
    refPlugin = (ArraysSBasePlugin) speciesRef.getPlugin("arrays");
    index = refPlugin.createIndex();
    ast = new ASTNode(libsbml.AST_LINEAR_ALGEBRA_SELECTOR);
    ci = new ASTNode(libsbml.AST_NAME);
    ci.setName("C");
    ast.addChild(ci);
    ci = new ASTNode(libsbml.AST_NAME);
    ci.setName("i");
    ast.addChild(ci);
    index.setMath(ast);

    libsbml.writeSBMLToFile(doc, "arrays1.xml");
  }
}