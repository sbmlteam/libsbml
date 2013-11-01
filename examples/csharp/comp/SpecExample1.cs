using System;
using libsbmlcs;

namespace CompExamples
{
    public class SpecExample1
    {
        private static int Main(string[] args)
        {
            var retval = 0;
            var sbmlns = new SBMLNamespaces(3, 1, "comp", 1);

            // create the document
            var document = new SBMLDocument(sbmlns);

            //Create our submodel
            var compdoc = (CompSBMLDocumentPlugin) (document.getPlugin("comp"));
            compdoc.setRequired(true);
            var mod1 = compdoc.createModelDefinition();
            mod1.setId("enzyme");
            mod1.setName("enzyme");
            var comp = mod1.createCompartment();
            comp.setSpatialDimensions(3);
            comp.setConstant(true);
            comp.setId("comp");
            comp.setSize(1L);
            var spec = new Species(3, 1);
            spec.setCompartment("comp");
            spec.setHasOnlySubstanceUnits(false);
            spec.setConstant(false);
            spec.setBoundaryCondition(false);
            spec.setId("S");
            mod1.addSpecies(spec);
            spec.setId("E");
            mod1.addSpecies(spec);
            spec.setId("D");
            mod1.addSpecies(spec);
            spec.setId("ES");
            mod1.addSpecies(spec);
            var rxn = new Reaction(3, 1);
            rxn.setReversible(true);
            rxn.setFast(false);
            var rxn2 = new Reaction(rxn);
            rxn.setId("J0");
            rxn2.setId("J1");
            var sr = new SpeciesReference(3, 1);
            sr.setConstant(true);
            sr.setStoichiometry(1);
            sr.setSpecies("S");
            rxn.addReactant(sr);
            sr.setSpecies("E");
            rxn.addReactant(sr);
            rxn2.addProduct(sr);
            sr.setSpecies("ES");
            rxn.addProduct(sr);
            rxn2.addReactant(sr);
            sr.setSpecies("D");
            rxn2.addProduct(sr);

            mod1.addReaction(rxn);
            mod1.addReaction(rxn2);

            // create the Model
            var model = document.createModel();
            model.setId("aggregate");

            // Create a submodel
            var mplugin = (CompModelPlugin) (model.getPlugin("comp"));
            var submod1 = mplugin.createSubmodel();
            submod1.setId("submod1");
            submod1.setModelRef("enzyme");

            var submod2 = new Submodel();
            submod2.setId("submod2");
            submod2.setModelRef("enzyme");
            mplugin.addSubmodel(submod2);

            libsbml.writeSBMLToFile(document, "enzyme_model.xml");
            document = libsbml.readSBMLFromFile("enzyme_model.xml");
            if (document == null)
            {
                Console.WriteLine("Error reading back in file.");
                retval = -1;
            }
            else
            {
                document.setConsistencyChecks(libsbml.LIBSBML_CAT_UNITS_CONSISTENCY, false);
                document.checkConsistency();
                if (document.getErrorLog().getNumFailsWithSeverity(2) > 0 ||
                    document.getErrorLog().getNumFailsWithSeverity(3) > 0)
                {
                    var stream = new OStringStream();
                    document.printErrors(stream);
                    Console.WriteLine("Errors encoutered when round-tripping  SBML file: \n" +
                                      stream.str());
                    retval = -1;
                }
                libsbml.writeSBMLToFile(document, "enzyme_model_rt.xml");
            }
            return retval;
        }
    }
}
