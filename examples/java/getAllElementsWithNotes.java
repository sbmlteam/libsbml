/** 
 * @file    getAllElementsWithNotes.java
 * @brief   Utility program, demontrating how to use the element filter
 *          class to search the model for elements with specific attributes
 *          in this example, we look for elements with notes
 * 
 * @author  Frank T. Bergmann
 * 
 * 
 * This file is part of libSBML.  Please visit http:#sbml.org for more
 * information about SBML, and the latest version of libSBML.
 * 
 */
import java.util.Vector;
import org.sbml.libsbml.ElementFilter;
import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.SBase;
import org.sbml.libsbml.SBaseList;
import org.sbml.libsbml.libsbml;

public class getAllElementsWithNotes
    extends ElementFilter
{

    /**
     * @param args
     */
    public static void main(String[] args)
    {
        System.loadLibrary("sbmlj");
        System.out.println("Using libSBML : "
            + libsbml.getLibSBMLDottedVersion());
        if (args.length != 1)
        {
            System.out.format("\nUsage: getAllElementsWithNotes filename\n\n");
            System.exit(1);
        }

        String filename = args[0];

        // read the document
        long start = System.currentTimeMillis();
        SBMLDocument document = libsbml.readSBMLFromFile(filename);
        long stop = System.currentTimeMillis();

        System.out.println();
        System.out.format("            filename: %s\n", filename);
        System.out.format("      read time (ms): %d\n", (stop - start));

        // stop in case of serious errors
        long errors = document.getNumErrors(libsbml.LIBSBML_SEV_ERROR);
        if (errors > 0)
        {
            System.out.format("            error(s): %d\n", errors);
            document.printErrors();
            System.exit((int) errors);
        }

        // create the filter to use
        getAllElementsWithNotes filter = new getAllElementsWithNotes();
        // get a list of all elements, as we will need to know all identifiers
        // so that we don't create duplicates.
        start = System.currentTimeMillis();
        System.out.println("    searching ......:");
        SBaseList allElements = document.getListOfAllElements(filter);
        stop = System.currentTimeMillis();
        System.out.format("  seaching took (ms): %d\n", (stop - start));
        System.out.format(" elements with notes: %d\n", (allElements.getSize()));

        // if we got here all went well ...

    }


    // <summary>
    // Constructor initializing this element filter
    // </summary>
    public getAllElementsWithNotes()
    {
    }


    @Override
    public boolean filter(SBase element)
    {
        // return in case we don't have a valid element
        if (element == null
            || !element.isSetNotes())
            return false;

        // otherwise we have notes set and want to keep the element
        if (element.isSetId())
            System.out.println("                     found : " + element.getId() );
        else 
            System.out.println("                     found element without id");

        return true;
    }


}
