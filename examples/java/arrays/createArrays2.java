import org.sbml.libsbml.ASTNode;
import org.sbml.libsbml.ArraysInitialAssignmentPlugin;
import org.sbml.libsbml.ArraysParameterPlugin;
import org.sbml.libsbml.ArraysPkgNamespaces;
import org.sbml.libsbml.Dimension;
import org.sbml.libsbml.Index;
import org.sbml.libsbml.InitialAssignment;
import org.sbml.libsbml.Model;
import org.sbml.libsbml.NewASTNode;
import org.sbml.libsbml.Parameter;
import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.libsbml;

public class createArrays2
{
    public static void main(String args[])
    {
        System.loadLibrary("sbmlj");
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

        ArraysParameterPlugin paramPlugin = (ArraysParameterPlugin) param.getPlugin("arrays");
        Dimension dim = paramPlugin.createDimension();
        dim.setId("i");
        dim.setSize("n");

        param = model.createParameter();
        param.setId("y");
        param.setConstant(false);

        paramPlugin = (ArraysParameterPlugin) param.getPlugin("arrays");
        dim = paramPlugin.createDimension();
        dim.setId("i");
        dim.setSize("n");

        param = model.createParameter();
        param.setId("z");
        param.setConstant(false);

        paramPlugin = (ArraysParameterPlugin) param.getPlugin("arrays");
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

        ArraysInitialAssignmentPlugin assignmentPlugin = (ArraysInitialAssignmentPlugin) assignment.getPlugin("arrays");
        dim = assignmentPlugin.createDimension();
        dim.setId("i");
        dim.setSize("m");

        Index index = assignmentPlugin.createIndex();
        NewASTNode newAst = new NewASTNode(libsbml.AST_ARRAYS_FUNCTION_SELECTOR);
        NewASTNode ci = new NewASTNode(libsbml.AST_NAME);
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

        assignmentPlugin = (ArraysInitialAssignmentPlugin) assignment.getPlugin("arrays");
        dim = assignmentPlugin.createDimension();
        dim.setId("i");
        dim.setSize("m");

        index = assignmentPlugin.createIndex();
        newAst = new NewASTNode(libsbml.AST_ARRAYS_FUNCTION_SELECTOR);
        ci = new NewASTNode(libsbml.AST_NAME);
        ci.setName("z");
        newAst.addChild(ci);
        NewASTNode plus = new NewASTNode(libsbml.AST_PLUS);

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
