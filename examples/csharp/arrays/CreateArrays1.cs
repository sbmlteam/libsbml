using System;
using System.Collections.Generic;
using libsbmlcs;


class Program
{
    static void Main(string[] args)
    {
        var arraysNs = new ArraysPkgNamespaces();
        var doc = new SBMLDocument(arraysNs);
        doc.setPackageRequired("arrays", true);
        var model = doc.createModel();

        // create compartment
        var comp = model.createCompartment();
        comp.setMetaId("dd");
        comp.setId("s");
        comp.setConstant(true);

        // set dimensions
        var compPlugin = (ArraysSBasePlugin)comp.getPlugin("arrays");
        var dim = compPlugin.createDimension();
        dim.setId("i");
        dim.setSize("n");

        // create species
        var species = model.createSpecies();
        species.setId("A");
        species.setCompartment("s");
        species.setHasOnlySubstanceUnits(false);
        species.setBoundaryCondition(false);
        species.setConstant(false);

        var splugin = (ArraysSBasePlugin)species.getPlugin("arrays");
        dim = splugin.createDimension();
        dim.setId("i");
        dim.setSize("n");

        species = model.createSpecies();
        species.setId("B");
        species.setCompartment("s");
        species.setHasOnlySubstanceUnits(false);
        species.setBoundaryCondition(false);
        species.setConstant(false);

        splugin = (ArraysSBasePlugin)species.getPlugin("arrays");
        dim = splugin.createDimension();
        dim.setId("i");
        dim.setSize("n");

        species = model.createSpecies();
        species.setId("C");
        species.setCompartment("s");
        species.setHasOnlySubstanceUnits(false);
        species.setBoundaryCondition(false);
        species.setConstant(false);

        splugin = (ArraysSBasePlugin)species.getPlugin("arrays");
        dim = splugin.createDimension();
        dim.setId("i");
        dim.setSize("n");

        // create parameter
        var param = model.createParameter();
        param.setId("n");
        param.setValue(100);
        param.setConstant(true);

        // create reaction
        var reaction = model.createReaction();
        reaction.setId("reaction1");
        reaction.setReversible(false);
        reaction.setFast(false);

        var reactionPlugin = (ArraysSBasePlugin)reaction.getPlugin("arrays");
        dim = reactionPlugin.createDimension();
        dim.setId("i");
        dim.setSize("n");

        var speciesRef = reaction.createReactant();
        speciesRef.setSpecies("A");
        speciesRef.setConstant(false);
        var refPlugin = (ArraysSBasePlugin)speciesRef.getPlugin("arrays");
        var index = refPlugin.createIndex();
		var ast = new ASTNode(libsbml.AST_LINEAR_ALGEBRA_SELECTOR);
        var ci = new ASTNode(libsbml.AST_NAME);
        ci.setName("A");
        ast.addChild(ci);
        ci = new ASTNode(libsbml.AST_NAME);
        ci.setName("i");
        ast.addChild(ci);
        index.setMath(ast);

        speciesRef = reaction.createProduct();
        speciesRef.setSpecies("C");
        speciesRef.setConstant(false);
        refPlugin = (ArraysSBasePlugin)speciesRef.getPlugin("arrays");
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
