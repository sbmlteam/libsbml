/**
 * @file    GetAllElementsWithNotes.cs
 * @brief   Utility program, demontrating how to use the element filter
 *          class to search the model for elements with specific attributes
 *          in this example, we look for elements with notes
 *
 * @author  Frank T. Bergmann
 *
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */
using System;
using System.Collections.Generic;
using System.Text;
using libsbmlcs;

/// <summary>
/// This class implements an element filter, that can be used to find elements
/// with notes
/// </summary>
public class NotesFilter : ElementFilter
{
    /// <summary>
    /// The program is to be invoked with one argument, the input file. 
    /// </summary>
    /// <param name="args">command line arguments</param>
    /// <returns>0 in case of no errors</returns>
    public static int Main(string[] args)
    {
        if (args.Length != 1)
        {
            Console.WriteLine("{0}Usage: getAllElementsWithNotes filename{0}{0}", Environment.NewLine);
            return 1;
        }

        string filename = args[0];

        // read the document
        long start = DateTime.Now.Ticks;
        SBMLDocument document = libsbml.readSBMLFromFile(filename);
        long stop = DateTime.Now.Ticks;


        Console.WriteLine();
        Console.WriteLine("            filename: {0}", filename);
        Console.WriteLine("      read time (ms): {0}", TimeSpan.FromTicks(stop - start).TotalMilliseconds);

        // stop in case of serious errors
        long errors = document.getNumErrors(libsbml.LIBSBML_SEV_ERROR);
        if (errors > 0)
        {
            Console.WriteLine("            error(s): {0}", errors);
            document.printErrors();
            return (int)errors;
        }


		// create the filter we want to use
		var filter = new NotesFilter();
        //  get a list of all elements with notes 
        start = DateTime.Now.Ticks;
		Console.WriteLine("    searching ......:");
        SBaseList allElements = document.getListOfAllElements(filter);
        stop = DateTime.Now.Ticks;
        Console.WriteLine("    search time (ms): {0}", TimeSpan.FromTicks(stop - start).TotalMilliseconds);
        Console.WriteLine();
        Console.WriteLine(" elements with notes: {0}", allElements.getSize());
        Console.WriteLine();

        // if we got here all went well ... 
        return 0;
    }

    /// <summary>
    /// Constructor initializing this element filter
    /// </summary>
    public NotesFilter()
    {
    }

    /// <summary>
    /// The function performing the filtering, here we just check 
    /// that we have a valid element, and that it has notes.
    /// </summary>
    /// <param name="element">the current element</param>
    /// <returns><b>true</b> if element is to be included, <b>false</b> otherwise</returns>
    public override bool filter(SBase element)
    {
        // return in case we don't have a valid element
        if (element == null || !element.isSetNotes())
            return false;

        // otherwise we have notes set and want to keep the element
        if (!element.isSetId())
            Console.WriteLine("                     found : {0}", element.getId());
		else
			Console.WriteLine("                     found element without id");

        return true;
    }


}
