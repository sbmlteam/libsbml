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

        // create parameters
        var param = model.createParameter();
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

        var paramPlugin = (ArraysParameterPlugin)param.getPlugin("arrays");
        var dim = paramPlugin.createDimension();
        dim.setId("i");
        dim.setSize("n");

        param = model.createParameter();
        param.setId("y");
        param.setConstant(false);

        paramPlugin = (ArraysParameterPlugin)param.getPlugin("arrays");
        dim = paramPlugin.createDimension();
        dim.setId("i");
        dim.setSize("n");

        param = model.createParameter();
        param.setId("z");
        param.setConstant(false);

        paramPlugin = (ArraysParameterPlugin)param.getPlugin("arrays");
        dim = paramPlugin.createDimension();
        dim.setId("i");
        dim.setSize("n");

        // create initial assignments

        var assignment = model.createInitialAssignment();
        assignment.setSymbol("y");
        var ast = new ASTNode(libsbml.AST_REAL);
        ast.setValue(3.2);
        assignment.setMath(ast);

        assignment = model.createInitialAssignment();
        assignment.setSymbol("z");
        ast = new ASTNode(libsbml.AST_REAL);
        ast.setValue(5.7);
        assignment.setMath(ast);

        var assignmentPlugin = (ArraysInitialAssignmentPlugin)assignment.getPlugin("arrays");
        dim = assignmentPlugin.createDimension();
        dim.setId("i");
        dim.setSize("m");

        var index = assignmentPlugin.createIndex();
        var newAst = new NewASTNode(libsbml.AST_FUNCTION);
        newAst.setName("selector");
        var ci = new NewASTNode(libsbml.AST_NAME);
        ci.setName("z");
        newAst.addChild(ci);
        ci = new NewASTNode(libsbml.AST_NAME);
        ci.setName("i");
        newAst.addChild(ci);
        index.setMath(newAst);

        assignment = model.createInitialAssignment();
        assignment.setSymbol("z");
        ast = new ASTNode(libsbml.AST_REAL);
        ast.setValue(3.2);
        assignment.setMath(ast);

        assignmentPlugin = (ArraysInitialAssignmentPlugin)assignment.getPlugin("arrays");
        dim = assignmentPlugin.createDimension();
        dim.setId("i");
        dim.setSize("m");

        index = assignmentPlugin.createIndex();
		newAst = new NewASTNode(libsbml.AST_ARRAYS_FUNCTION_SELECTOR);
        ci = new NewASTNode(libsbml.AST_NAME);
        ci.setName("z");
        newAst.addChild(ci);
        var plus = new NewASTNode(libsbml.AST_PLUS);

        ci = new NewASTNode(libsbml.AST_NAME);
        ci.setName("i");
        plus.addChild(ci);
        ci = new NewASTNode(libsbml.AST_NAME);
        ci.setName("m");
        plus.addChild(ci);
        newAst.addChild(plus);
        index.setMath(newAst);

        libsbml.writeSBMLToFile(doc, "arrays2.xml");
    }
}
