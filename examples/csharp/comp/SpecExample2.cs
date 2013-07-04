using System;
using libsbmlcs;

namespace CompExamples
{
    public class SpecExample2
    {
        private static int Main(string[] args)
        {
            var sbmlns = new SBMLNamespaces(3, 1, "comp", 1);

            // create the document
            var document = new SBMLDocument(sbmlns);

            //Define the external model definitions
            var compdoc = (CompSBMLDocumentPlugin)(document.getPlugin("comp"));
            compdoc.setRequired(true);
            var extmod = compdoc.createExternalModelDefinition();
            extmod.setId("ExtMod1");
            extmod.setSource("enzyme_model.xml");
            extmod.setModelRef("enzyme");


            // create the main Model
            var model = document.createModel();

            // Set the submodels
            var mplugin = (CompModelPlugin)(model.getPlugin("comp"));
            var submod1 = mplugin.createSubmodel();
            submod1.setId("A");
            submod1.setModelRef("ExtMod1");
            var submod2 = mplugin.createSubmodel();
            submod2.setId("B");
            submod2.setModelRef("ExtMod1");

            // create a replacement compartment
            var comp = model.createCompartment();
            comp.setSpatialDimensions(3);
            comp.setConstant(true);
            comp.setId("comp");
            comp.setSize(1L);

            //Tell the model that this compartment replaces both of the inside ones.
            var compartplug = (CompSBasePlugin)(comp.getPlugin("comp"));
            var re = new ReplacedElement();
            re.setIdRef("comp");
            re.setSubmodelRef("A");
            compartplug.addReplacedElement(re);
            re.setSubmodelRef("B");
            compartplug.addReplacedElement(re);

            // create a replacement species
            var spec = model.createSpecies();
            spec.setCompartment("comp");
            spec.setHasOnlySubstanceUnits(false);
            spec.setConstant(false);
            spec.setBoundaryCondition(false);
            spec.setId("S");

            //Tell the model that this species replaces both of the inside ones.
            var spp = (CompSBasePlugin)(spec.getPlugin("comp"));
            re.setIdRef("S");
            re.setSubmodelRef("A");
            spp.addReplacedElement(re);
            re.setSubmodelRef("B");
            spp.addReplacedElement(re);


            libsbml.writeSBMLToFile(document, "spec_example2.xml");
            document = libsbml.readSBMLFromFile("spec_example2.xml");
            if (document == null)
            {
                Console.WriteLine("Error reading back in file.");
                return -1;
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
                    return -1;
                }
                libsbml.writeSBMLToFile(document, "spec_example2_rt.xml");
            }

            return 0;
        }
    }
}
