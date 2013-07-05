import org.sbml.libsbml.ArraysCompartmentPlugin;
import org.sbml.libsbml.ArraysPkgNamespaces;
import org.sbml.libsbml.ArraysReactionPlugin;
import org.sbml.libsbml.ArraysSpeciesPlugin;
import org.sbml.libsbml.ArraysSpeciesReferencePlugin;
import org.sbml.libsbml.Compartment;
import org.sbml.libsbml.Dimension;
import org.sbml.libsbml.Index;
import org.sbml.libsbml.Model;
import org.sbml.libsbml.NewASTNode;
import org.sbml.libsbml.Parameter;
import org.sbml.libsbml.Reaction;
import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.Species;
import org.sbml.libsbml.SpeciesReference;
import org.sbml.libsbml.libsbml;

public class createArrays1
{
    public static void main(String args[])
    {
        System.loadLibrary("sbmlj");
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
        ArraysCompartmentPlugin compPlugin = (ArraysCompartmentPlugin) comp.getPlugin("arrays");
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

        ArraysSpeciesPlugin splugin = (ArraysSpeciesPlugin) species.getPlugin("arrays");
        dim = splugin.createDimension();
        dim.setId("i");
        dim.setSize("n");

        species = model.createSpecies();
        species.setId("B");
        species.setCompartment("s");
        species.setHasOnlySubstanceUnits(false);
        species.setBoundaryCondition(false);
        species.setConstant(false);

        splugin = (ArraysSpeciesPlugin) species.getPlugin("arrays");
        dim = splugin.createDimension();
        dim.setId("i");
        dim.setSize("n");

        species = model.createSpecies();
        species.setId("C");
        species.setCompartment("s");
        species.setHasOnlySubstanceUnits(false);
        species.setBoundaryCondition(false);
        species.setConstant(false);

        splugin = (ArraysSpeciesPlugin) species.getPlugin("arrays");
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

        ArraysReactionPlugin reactionPlugin = (ArraysReactionPlugin) reaction.getPlugin("arrays");
        dim = reactionPlugin.createDimension();
        dim.setId("i");
        dim.setSize("n");

        SpeciesReference speciesRef = reaction.createReactant();
        speciesRef.setSpecies("A");
        speciesRef.setConstant(false);
        ArraysSpeciesReferencePlugin refPlugin = (ArraysSpeciesReferencePlugin) speciesRef.getPlugin("arrays");
        Index index = refPlugin.createIndex();
        NewASTNode ast = new NewASTNode(libsbml.AST_ARRAYS_FUNCTION_SELECTOR);
        NewASTNode ci = new NewASTNode(libsbml.AST_NAME);
        ci.setName("A");
        ast.addChild(ci);
        ci = new NewASTNode(libsbml.AST_NAME);
        ci.setName("i");
        ast.addChild(ci);
        index.setMath(ast);

        speciesRef = reaction.createProduct();
        speciesRef.setSpecies("C");
        speciesRef.setConstant(false);
        refPlugin = (ArraysSpeciesReferencePlugin) speciesRef.getPlugin("arrays");
        index = refPlugin.createIndex();
        ast = new NewASTNode(libsbml.AST_FUNCTION);
        ast.setName("selector");
        ci = new NewASTNode(libsbml.AST_NAME);
        ci.setName("C");
        ast.addChild(ci);
        ci = new NewASTNode(libsbml.AST_NAME);
        ci.setName("i");
        ast.addChild(ci);
        index.setMath(ast);

        libsbml.writeSBMLToFile(doc, "arrays1.xml");
    }
}
