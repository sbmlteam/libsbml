using System;
using libsbmlcs;


public class fbc_Example1
{
    
    static void Main(string[] args)
    {

        SBMLNamespaces sbmlns = new SBMLNamespaces(3, 1, "fbc", 1);
        SBMLDocument document = new SBMLDocument(sbmlns);

        // create the Model

        Model model = document.createModel();

        // create the Compartment

        Compartment compartment = model.createCompartment();
        compartment.setId("compartment");
        compartment.setConstant(true);
        compartment.setSize(1);

        // create the Species

        Species species = model.createSpecies();
        species.setId("Node1");
        species.setCompartment("compartment");
        species.setConstant(false);
        species.setHasOnlySubstanceUnits(false);
        species.setBoundaryCondition(false);

        species = model.createSpecies();
        species.setId("Node2");
        species.setCompartment("compartment");
        species.setConstant(false);
        species.setHasOnlySubstanceUnits(false);
        species.setConstant(false);
        species.setHasOnlySubstanceUnits(false);
        species.setBoundaryCondition(false);

        species = model.createSpecies();
        species.setId("Node3");
        species.setCompartment("compartment");
        species.setConstant(false);
        species.setHasOnlySubstanceUnits(false);
        species.setBoundaryCondition(false);

        species = model.createSpecies();
        species.setId("Node4");
        species.setCompartment("compartment");
        species.setConstant(false);
        species.setHasOnlySubstanceUnits(false);
        species.setBoundaryCondition(false);

        species = model.createSpecies();
        species.setId("Node5");
        species.setCompartment("compartment");
        species.setConstant(false);
        species.setHasOnlySubstanceUnits(false);
        species.setBoundaryCondition(false);

        species = model.createSpecies();
        species.setId("Node6");
        species.setCompartment("compartment");
        species.setConstant(false);
        species.setHasOnlySubstanceUnits(false);
        species.setBoundaryCondition(false);

        species = model.createSpecies();
        species.setId("Node7");
        species.setCompartment("compartment");
        species.setConstant(false);
        species.setHasOnlySubstanceUnits(false);
        species.setBoundaryCondition(false);

        species = model.createSpecies();
        species.setId("Node8");
        species.setCompartment("compartment");
        species.setConstant(false);
        species.setHasOnlySubstanceUnits(false);
        species.setBoundaryCondition(false);

        species = model.createSpecies();
        species.setId("Node0");
        species.setCompartment("compartment");
        species.setConstant(false);
        species.setHasOnlySubstanceUnits(false);
        species.setBoundaryCondition(true);

        species = model.createSpecies();
        species.setId("Node9");
        species.setCompartment("compartment");
        species.setConstant(false);
        species.setHasOnlySubstanceUnits(false);
        species.setBoundaryCondition(true);

        Reaction reaction = model.createReaction();
        reaction.setId("J0");
        reaction.setReversible(false);
        reaction.setFast(false);
        SpeciesReference reactant = reaction.createReactant();
        reactant.setSpecies("Node0");
        reactant.setStoichiometry(1);
        reactant.setConstant(true);
        SpeciesReference product = reaction.createProduct();
        product.setSpecies("Node1");
        product.setStoichiometry(1);
        product.setConstant(true);

        reaction = model.createReaction();
        reaction.setId("J1");
        reaction.setReversible(false);
        reaction.setFast(false);
        reactant = reaction.createReactant();
        reactant.setSpecies("Node1");
        reactant.setStoichiometry(1);
        reactant.setConstant(true);
        product = reaction.createProduct();
        product.setSpecies("Node2");
        product.setStoichiometry(1);
        product.setConstant(true);

        reaction = model.createReaction();
        reaction.setId("J2");
        reaction.setReversible(false);
        reaction.setFast(false);
        reactant = reaction.createReactant();
        reactant.setSpecies("Node2");
        reactant.setStoichiometry(1);
        reactant.setConstant(true);
        product = reaction.createProduct();
        product.setSpecies("Node3");
        product.setStoichiometry(1);
        product.setConstant(true);

        reaction = model.createReaction();
        reaction.setId("J3");
        reaction.setReversible(false);
        reaction.setFast(false);
        reactant = reaction.createReactant();
        reactant.setSpecies("Node1");
        reactant.setStoichiometry(1);
        reactant.setConstant(true);
        product = reaction.createProduct();
        product.setSpecies("Node4");
        product.setStoichiometry(1);
        product.setConstant(true);

        reaction = model.createReaction();
        reaction.setId("J4");
        reaction.setReversible(false); 
        reaction.setFast(false);
        reactant = reaction.createReactant();
        reactant.setSpecies("Node4");
        reactant.setStoichiometry(1);
        reactant.setConstant(true);
        product = reaction.createProduct();
        product.setSpecies("Node3");
        product.setStoichiometry(1);
        product.setConstant(true);

        reaction = model.createReaction();
        reaction.setId("J5");
        reaction.setReversible(false);
        reaction.setFast(false);
        reactant = reaction.createReactant();
        reactant.setSpecies("Node3");
        reactant.setStoichiometry(1);
        reactant.setConstant(true);
        product = reaction.createProduct();
        product.setSpecies("Node5");
        product.setStoichiometry(1);
        product.setConstant(true);

        reaction = model.createReaction();
        reaction.setId("J6");
        reaction.setReversible(false);
        reaction.setFast(false);
        reactant = reaction.createReactant();
        reactant.setSpecies("Node5");
        reactant.setStoichiometry(1);
        reactant.setConstant(true);
        product = reaction.createProduct();
        product.setSpecies("Node6");
        product.setStoichiometry(1);
        product.setConstant(true);

        reaction = model.createReaction();
        reaction.setId("J7");
        reaction.setReversible(false);
        reaction.setFast(false);
        reactant = reaction.createReactant();
        reactant.setSpecies("Node6");
        reactant.setStoichiometry(1);
        reactant.setConstant(true);
        product = reaction.createProduct();
        product.setSpecies("Node7");
        product.setStoichiometry(1);
        product.setConstant(true);

        reaction = model.createReaction();
        reaction.setId("J8");
        reaction.setReversible(false);
        reaction.setFast(false);
        reactant = reaction.createReactant();
        reactant.setSpecies("Node5");
        reactant.setStoichiometry(1);
        reactant.setConstant(true);
        product = reaction.createProduct();
        product.setSpecies("Node8");
        product.setStoichiometry(1);
        product.setConstant(true);

        reaction = model.createReaction();
        reaction.setId("J9");
        reaction.setReversible(false);
        reaction.setFast(false);
        reactant = reaction.createReactant();
        reactant.setSpecies("Node8");
        reactant.setStoichiometry(1);
        reactant.setConstant(true);
        product = reaction.createProduct();
        product.setSpecies("Node7");
        product.setStoichiometry(1);
        product.setConstant(true);

        reaction = model.createReaction();
        reaction.setId("J10");
        reaction.setReversible(false);
        reaction.setFast(false);
        reactant = reaction.createReactant();
        reactant.setSpecies("Node7");
        reactant.setStoichiometry(1);
        reactant.setConstant(true);
        product = reaction.createProduct();
        product.setSpecies("Node9");
        product.setStoichiometry(1);
        product.setConstant(true);


        //
        // Get a FbcModelPlugin object plugged in the model object.
        //
        // The type of the returned value of SBase::getPlugin() function is SBasePlugin, and
        // thus the value needs to be casted for the corresponding derived class. 
        //
        FbcModelPlugin mplugin = (FbcModelPlugin)(model.getPlugin("fbc"));

        FluxBound bound = mplugin.createFluxBound();

        bound.setId("bound1");
        bound.setReaction("J0");
        bound.setOperation("equal");
        bound.setValue(10);

        Objective objective = mplugin.createObjective();
        objective.setId("obj1");
        objective.setType("maximize");
        
        // mark obj1 as active objective
        mplugin.setActiveObjectiveId("obj1");

        FluxObjective fluxObjective = objective.createFluxObjective();
        fluxObjective.setReaction("J8");
        fluxObjective.setCoefficient(1);

        libsbml.writeSBMLToFile(document, "fbc_example1.xml");
    }
}
