using System;
using libsbmlcs;

namespace CompExamples
{
    public class CompExample1
    {
        private static int Main(string[] args)
        {
            var retval = 0;
            int rv;
            var sbmlns = new SBMLNamespaces(3, 1, "comp", 1);

            // create the document
            var document = new SBMLDocument(sbmlns);

            // create the Model
            var model = document.createModel();

            // create a replacement parameter
            var parameter = model.createParameter();
            parameter.setId("x");
            parameter.setConstant(true);

            // create a parameter to be a conversion factor
            var param2 = model.createParameter();
            param2.setId("x_conv");
            param2.setMetaId("_110013");
            param2.setConstant(true);

            // create a parameter to be a conversion factor
            var param3 = model.createParameter();
            param3.setId("lcf");
            param3.setConstant(true);

            // Convert parameter to the plugin version so we can add the new attributes and replacements to it.
            var splugin = (CompSBasePlugin)(parameter.getPlugin("comp"));

            // Add a replaced element.
            var rep1 = splugin.createReplacedElement();
            rv = rep1.setSubmodelRef("submod1");
            rv = rep1.setConversionFactor("x_conv");
            rv = rep1.setIdRef("param1");

            // Add a second replaced element in a different way.
            var rep2 = new ReplacedElement();
            rv = rep2.setSubmodelRef("submod2");
            rv = rep2.setDeletion("del1");
            rv = splugin.addReplacedElement(rep2);

            //Now create a replaced element that points into a submodel.
            rep2.unsetDeletion();
            rep2.setIdRef("submod2");
            var sbr5 = rep2.createSBaseRef();
            sbr5.setIdRef("submodelG");
            var sbr6 = sbr5.createSBaseRef();
            sbr6.setIdRef("buriedElement");
            splugin.addReplacedElement(rep2);


            // Create a submodel
            var mplugin = (CompModelPlugin)(model.getPlugin("comp"));
            var compdoc = (CompSBMLDocumentPlugin)(document.getPlugin("comp"));
            compdoc.setRequired(true);

            var moddef1 = compdoc.createModelDefinition();
            moddef1.setId("Mod1");
            var m1param1 = moddef1.createParameter();
            m1param1.setId("param1");
            m1param1.setConstant(true);
            var m1param2 = moddef1.createParameter();
            m1param2.setId("param2");
            m1param2.setConstant(false);
            m1param2.setValue(3.2);

            var moddef2 = new ModelDefinition();
            moddef2.setId("Mod2");
            var subparam2 = moddef2.createParameter();
            subparam2.setId("subparam2");
            subparam2.setConstant(false);
            compdoc.addModelDefinition(moddef2);


            var extmod1 = compdoc.createExternalModelDefinition();
            extmod1.setId("ExtMod1");
            extmod1.setSource("urn:miriam:biomodels.db:BIOMD0000000127");

            var extmod2 = new ExternalModelDefinition();
            extmod2.setId("ExtMod2");
            extmod2.setSource("otherfile.xml");
            extmod2.setModelRef("modelnamethere");
            extmod2.setMd5("406022s908ge74sklj");
            compdoc.addExternalModelDefinition(extmod2);

            var submod1 = mplugin.createSubmodel();
            submod1.setId("submod1");
            submod1.setModelRef("Mod1");
            var del1 = submod1.createDeletion();
            del1.setId("deletionA");
            del1.setIdRef("param2");

            var submod2 = new Submodel();
            submod2.setId("submod2");
            submod2.setModelRef("ExtMod1");
            submod2.setSubstanceConversionFactor("subcf");
            submod2.setTimeConversionFactor("tcf");
            submod2.setExtentConversionFactor("xtf");
            var del2 = new Deletion();
            del2.setId("deletionB");
            del2.setMetaIdRef("_0010110");
            rv = submod2.addDeletion(del2);
            del2.setId("deletionC");
            del2.unsetMetaIdRef();
            del2.setPortRef("port2");
            rv = submod2.addDeletion(del2);
            del2.unsetId();
            del2.unsetPortRef();
            del2.setUnitRef("mph");
            rv = submod2.addDeletion(del2);
            del2.unsetUnitRef();
            del2.setIdRef("submodG");
            var sbr = del2.createSBaseRef();
            sbr.setIdRef("element5");
            rv = submod2.addDeletion(del2);
            var del3 = new Deletion();
            del3.setIdRef("submodG");
            var sbr2 = new SBaseRef();
            sbr2.setIdRef("subsubmodQ");
            var subsbr = sbr2.createSBaseRef();
            subsbr.setPortRef("toBdel");
            del3.setSBaseRef(sbr2);
            submod2.addDeletion(del3);
            mplugin.addSubmodel(submod2);

            var port1 = mplugin.createPort();
            port1.setId("port1");
            port1.setMetaIdRef("_110013");
            var port2 = new Port();
            port2.setId("port2");
            port2.setIdRef("x");
            mplugin.addPort(port2);
            port2.setId("port3");
            port2.setIdRef("submod2");
            port2.setSBaseRef(sbr2);
            mplugin.addPort(port2);

            libsbml.writeSBMLToFile(document, "comp_example1.xml");
            document = libsbml.readSBMLFromFile("comp_example1.xml");
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
                libsbml.writeSBMLToFile(document, "comp_example1_rt.xml");
            }

            return retval;
        }
    }
}
