import org.sbml.libsbml.ConversionProperties;
import org.sbml.libsbml.LayoutModelPlugin;
import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.SBMLNamespaces;
import org.sbml.libsbml.SBasePlugin;
import org.sbml.libsbml.libsbml;

/**
 * This Class converts a document (including layout) from L3 to L2 or from L2 to
 * L3V1
 * 
 * @author Frank Bergmann
 * 
 */
public class SimpleLayoutConverter
{

    public static void main(String args[])
    {
        // check arguments
        if (args.length != 2)
        {
            System.err.println("usage: SimpleLayoutConverter <input file> <output file>");
            System.exit(1);
        }

        // load native library
        System.loadLibrary("sbmlj");

        // read document
        SBMLDocument doc = libsbml.readSBMLFromFile(args[0]);

        // bail on error
        if (doc.getNumErrors(libsbml.LIBSBML_SEV_ERROR) > 0)
        {
            doc.checkConsistency();
            System.err.println("The document contains errors that need to be corrected first:");
            System.err.println(doc.getErrorLog().toString());
            System.exit(1);
        }

        // check for layout
        if (doc.getPlugin("layout") == null)
        {
            System.err.println("This document contains no layout, conversion skipped.");
            System.exit(0);
        }

        // perform the conversion
        if (doc.getLevel() == 3)
            convertToL2(doc);
        else
            convertToL3(doc);

        // write document to file
        libsbml.writeSBMLToFile(doc, args[1]);

    }


    /**
     * This function converts the given document to L3v1, converting also layout
     * and render elements
     * 
     * @param doc
     *            the document to convert
     */
    private static void convertToL3(SBMLDocument doc)
    {
        if (doc == null || doc.getModel() == null) return;

        String layoutNsUri = "http://www.sbml.org/sbml/level3/version1/layout/version1";
        String renderNsUri = "http://www.sbml.org/sbml/level3/version1/render/version1";

        LayoutModelPlugin plugin = (LayoutModelPlugin) doc.getModel()
                                                          .getPlugin("layout");

        // bail if we don't have layout
        if (plugin == null) return;

        // convert document
        ConversionProperties prop = new ConversionProperties(
                                                             new SBMLNamespaces(
                                                                                3,
                                                                                1));
        prop.addOption("strict", false);
        prop.addOption("setLevelAndVersion", true);
        prop.addOption("ignorePackages", true);

        if (doc.convert(prop) != libsbml.LIBSBML_OPERATION_SUCCESS)
        {
            System.err.println("Conversion failed!");
            doc.printErrors();
            System.exit(2);
        }

        // add new layout namespace and set required flag
        SBasePlugin docPlugin = doc.getPlugin("layout");

        // if we don't have layout there isnothing to do
        if (docPlugin == null) return;

        docPlugin.setElementNamespace(layoutNsUri);

        doc.getSBMLNamespaces().addPackageNamespace("layout", 1);
        doc.setPackageRequired("layout", false);

        // add enable render if needed
        SBasePlugin rdocPlugin = doc.getPlugin("render");
        if (rdocPlugin != null)
        {
            doc.getSBMLNamespaces().addPackageNamespace("render", 1);
        }
        else
        {
            doc.enablePackage(renderNsUri, "render", true);
        }
        doc.setPackageRequired("render", false);

    }


    /**
     * This function converts a SBML L3 document to an L2 document, moving the
     * layout package into the annotation
     * 
     * @param doc
     *            the document to convert
     */
    private static void convertToL2(SBMLDocument doc)
    {
        if (doc == null || doc.getModel() == null) return;

        String layoutNsUri = "http://projects.eml.org/bcb/sbml/level2";
        String renderNsUri = "http://projects.eml.org/bcb/sbml/render/level2";

        LayoutModelPlugin plugin = (LayoutModelPlugin) doc.getModel()
                                                          .getPlugin("layout");

        // bail if we don't have layout
        if (plugin == null) return;

        // perform the conversion
        ConversionProperties prop = new ConversionProperties(
                                                             new SBMLNamespaces(
                                                                                2,
                                                                                4));
        prop.addOption("strict", false);
        prop.addOption("setLevelAndVersion", true);
        prop.addOption("ignorePackages", true);

        if (doc.convert(prop) != libsbml.LIBSBML_OPERATION_SUCCESS)
        {
            System.err.println("Conversion failed!");
            doc.printErrors();
            System.exit(2);
        }

        SBasePlugin docPlugin = doc.getPlugin("layout");

        // if we don't have layout, there is nothing to do
        if (docPlugin == null) return;

        docPlugin.setElementNamespace(layoutNsUri);

        doc.getSBMLNamespaces().removePackageNamespace(3, 1, "layout", 1);
        doc.getSBMLNamespaces().addPackageNamespace("layout", 1);

        SBasePlugin rdocPlugin = doc.getPlugin("render");
        if (rdocPlugin != null)
        {
            rdocPlugin.setElementNamespace(renderNsUri);
            doc.getSBMLNamespaces().removePackageNamespace(3, 1, "render", 1);
            doc.getSBMLNamespaces().addPackageNamespace("render", 1);
        }
    }
}
