import org.sbml.libsbml.libsbml;
import org.sbml.libsbml.SBMLDocument;

public class readSBML
{
  public static void main (String[] args)
  {
    if (args.length != 1)
    {
      System.out.println("Usage: java readSBML filename");
      System.exit(1);
    }

    SBMLDocument document = libsbml.readSBML(args[0]);

    System.out.println("  filename: " + args[0]);
    System.out.println("  error(s): " + document.getNumErrors());

    if (document.getNumErrors() > 0)
    {
      document.printErrors();
      System.exit(1);
    }
  }

  static
  {
    try
    {
      System.loadLibrary("sbmlj");
      /* Extra check to be sure we have access to libSBML: */
      Class.forName("org.sbml.libsbml.libsbml");
    }
    catch (Exception e)
    {
      System.err.println("Error: could not load the libSBML library");
      System.exit(1);
    }
  }
}
