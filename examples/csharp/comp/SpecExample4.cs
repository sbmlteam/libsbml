using System;
using libsbmlcs;

namespace CompExamples
{
    public class SpecExample4
    {
        private static int Main(string[] args)
        {
            var retval = 0;
            var sbmlns = new SBMLNamespaces(3, 1, "comp", 1);

            // create the document
            var document = new SBMLDocument(sbmlns);
            var compdoc = (CompSBMLDocumentPlugin)(document.getPlugin("comp"));
            compdoc.setRequired(true);

            var mod1 = compdoc.createModelDefinition();
            mod1.setId("enzyme");
            mod1.setName("enzyme");
            var comp = mod1.createCompartment();
            comp.setSpatialDimensions(3);
            comp.setConstant(true);
            comp.setId("comp");
            comp.setSize(1L);
            var spec = new Species(sbmlns);
            //We have to construct it this way because we get the comp plugin from it later.
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

            var rxn = new Reaction(sbmlns);
            rxn.setReversible(true);
            rxn.setFast(false);
            var rxn2 = new Reaction(sbmlns);
            rxn2.setReversible(true);
            rxn2.setFast(false);
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

            var mod1plug = (CompModelPlugin)(mod1.getPlugin("comp"));
            var m1port = new Port();
            m1port.setIdRef("comp");
            m1port.setId("comp_port");
            mod1plug.addPort(m1port);
            m1port.setIdRef("S");
            m1port.setId("S_port");
            mod1plug.addPort(m1port);
            m1port.setIdRef("E");
            m1port.setId("E_port");
            mod1plug.addPort(m1port);
            m1port.setIdRef("D");
            m1port.setId("D_port");
            mod1plug.addPort(m1port);
            m1port.setIdRef("ES");
            m1port.setId("ES_port");
            mod1plug.addPort(m1port);
            m1port.setIdRef("J0");
            m1port.setId("J0_port");
            mod1plug.addPort(m1port);
            m1port.setIdRef("J1");
            m1port.setId("J1_port");
            mod1plug.addPort(m1port);


            //Define the 'simple' model
            var mod2 = compdoc.createModelDefinition();
            mod2.setId("simple");
            var comp2 = mod2.createCompartment();
            comp2.setSpatialDimensions(3);
            comp2.setConstant(true);
            comp2.setId("comp");
            comp2.setSize(1L);

            spec.setCompartment("comp");
            spec.setHasOnlySubstanceUnits(false);
            spec.setConstant(false);
            spec.setBoundaryCondition(false);
            spec.setId("S");
            spec.setInitialConcentration(5);
            mod2.addSpecies(spec);
            spec.setId("D");
            spec.setInitialConcentration(10);
            mod2.addSpecies(spec);

            var rxn3 = new Reaction(sbmlns);
            //We have to construct it this way because we get the comp plugin from it later.
            rxn3.setReversible(true);
            rxn3.setFast(false);
            rxn3.setId("J0");

            var sr2 = new SpeciesReference(3, 1); //These will not need plugins.
            sr2.setConstant(true);
            sr2.setStoichiometry(1);
            sr2.setSpecies("S");
            rxn3.addReactant(sr2);
            sr2.setSpecies("D");
            rxn3.addProduct(sr2);

            mod2.addReaction(rxn3);

            var mod2plug = (CompModelPlugin)(mod2.getPlugin("comp"));
            var port = new Port();
            port.setId("S_port");
            port.setIdRef("S");
            mod2plug.addPort(port);

            var port2 = mod2plug.createPort();
            port2.setId("D_port");
            port2.setIdRef("D");

            port.setId("comp_port");
            port.setIdRef("comp");
            mod2plug.addPort(port);

            port.setId("J0_port");
            port.setIdRef("J0");
            mod2plug.addPort(port);

            // create the Model
            var model = document.createModel();
            model.setId("complexified");

            // Set the submodels
            var mplugin = (CompModelPlugin)(model.getPlugin("comp"));
            var submod1 = mplugin.createSubmodel();
            submod1.setId("A");
            submod1.setModelRef("enzyme");
            var submod2 = mplugin.createSubmodel();
            submod2.setId("B");
            submod2.setModelRef("simple");
            var del = submod2.createDeletion();
            del.setPortRef("J0_port");
            del.setId("oldrxn");

            // Synchronize the compartments
            var mcomp = model.createCompartment();
            mcomp.setSpatialDimensions(3);
            mcomp.setConstant(true);
            mcomp.setId("comp");
            mcomp.setSize(1L);
            var compartplug = (CompSBasePlugin)(mcomp.getPlugin("comp"));
            var re = new ReplacedElement();
            re.setIdRef("comp");
            re.setSubmodelRef("A");
            compartplug.addReplacedElement(re);
            re.setSubmodelRef("B");
            re.unsetIdRef();
            re.setPortRef("comp_port");
            compartplug.addReplacedElement(re);

            //Synchronize the species
            spec.setId("S");
            spec.setInitialConcentration(5);
            var specplug = (CompSBasePlugin)(spec.getPlugin("comp"));
            var sre = specplug.createReplacedElement();
            sre.setSubmodelRef("A");
            sre.setIdRef("S");
            var sre2 = specplug.createReplacedElement();
            sre2.setSubmodelRef("B");
            sre2.setPortRef("S_port");
            model.addSpecies(spec);

            spec.setId("D");
            spec.setInitialConcentration(10);
            sre.setIdRef("D");
            sre2.setPortRef("D_port");
            model.addSpecies(spec);

            spec.setId("E");
            spec.unsetInitialConcentration();
            sre.unsetIdRef();
            sre2.unsetIdRef();
            sre.setPortRef("E_port");
            sre2.setDeletion("oldrxn");
            model.addSpecies(spec);

            spec.setId("ES");
            spec.unsetInitialConcentration();
            sre.setPortRef("ES_port");
            sre2.setDeletion("oldrxn");
            model.addSpecies(spec);

            //Synchronize the reactions
            var rxn1plug = (CompSBasePlugin)(rxn.getPlugin("comp"));
            var rxn2plug = (CompSBasePlugin)(rxn2.getPlugin("comp"));
            var deletion = new ReplacedElement();
            deletion.setDeletion("oldrxn");
            deletion.setSubmodelRef("B");
            rxn1plug.addReplacedElement(deletion);
            rxn2plug.addReplacedElement(deletion);
            var re2 = new ReplacedElement();
            re2.setSubmodelRef("A");
            re2.setPortRef("J0_port");
            rxn1plug.addReplacedElement(re2);
            re2.setPortRef("J1_port");
            rxn2plug.addReplacedElement(re2);

            model.addReaction(rxn);
            model.addReaction(rxn2);


            libsbml.writeSBMLToFile(document, "spec_example4.xml");
            document = libsbml.readSBMLFromFile("spec_example4.xml");
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
                libsbml.writeSBMLToFile(document, "spec_example4_rt.xml");
            }
            return retval;
        }
    }
}