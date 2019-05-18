// 
// @file    addRenderInformation.cs
// @brief   Adds render information to the given SBML file
// @author  Frank Bergmann
// 
// This file is part of libSBML.  Please visit http:#sbml.org for more
// information about SBML, and the latest version of libSBML.
// 

using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using libsbmlcs;

public class cs_addRenderInformation
{
    public static void addRenderInformation(RenderLayoutPlugin rPlugin)
    {
        if (rPlugin == null)
        {
            Console.WriteLine("could not add render information!");
            Environment.Exit(4);
        }

        LocalRenderInformation rInfo = rPlugin.createLocalRenderInformation();
        rInfo.setId("info");
        rInfo.setName("Example Render Information");
        rInfo.setProgramName("RenderInformation Examples");
        rInfo.setProgramVersion("1.0");

        // add some colors
        ColorDefinition color = rInfo.createColorDefinition();
        color.setId("black");
        color.setColorValue("#000000");

        color = rInfo.createColorDefinition();
        color.setId("silver");
        color.setColorValue("#c0c0c0");

        color = rInfo.createColorDefinition();
        color.setId("white");
        color.setColorValue("#FFFFFF");

        // add a linear gradient from black to white to silver
        LinearGradient gradient = rInfo.createLinearGradientDefinition();
        gradient.setId("simpleGradient");
        gradient.setPoint1(new RelAbsVector(), new RelAbsVector());
        gradient.setPoint2(new RelAbsVector(0, 100), new RelAbsVector(0, 100));

        GradientStop stop = gradient.createGradientStop();
        stop.setOffset(new RelAbsVector());
        stop.setStopColor("white");

        stop = gradient.createGradientStop();
        stop.setOffset(new RelAbsVector(0, 100));
        stop.setStopColor("silver");

        // add a species style that represents them as ellipses with the gradient above
        Style style = rInfo.createStyle("ellipseStyle");
        style.getGroup().setFillColor("simpleGradient");
        style.getGroup().setStroke("black");
        style.getGroup().setStrokeWidth(2.0);
        style.addType("SPECIESGLYPH");

        Ellipse ellipse = style.getGroup().createEllipse();
        ellipse.setCenter2D(new RelAbsVector(0, 50), new RelAbsVector(0, 50));
        ellipse.setRadii(new RelAbsVector(0, 50), new RelAbsVector(0, 50));

    }
    public static int Main(string[] args)
    {
        if (args.Length != 2)
        {
            Console.WriteLine(" Usage:  addRenderInformation <input file> <output file> \n" +
                    "      Adds a render information object to the input file");
            return 1;
        }


        string inputFile = args[0];
        string outputFile = args[1];

        SBMLDocument doc = libsbml.readSBMLFromFile(inputFile);
        long numErrors = doc.getNumErrors(libsbml.LIBSBML_SEV_ERROR);

        if (numErrors > 0)
        {
            Console.WriteLine("Encountered errors while reading the file. ");
            Console.WriteLine("Please correct the following errors and try again.");
            doc.printErrors();
            return 2;
        }

        Model model = doc.getModel();

        LayoutModelPlugin plugin = (LayoutModelPlugin)model.getPlugin("layout");

        if (plugin == null || plugin.getNumLayouts() == 0)
        {
            Console.WriteLine("The loaded model contains no layout information, please add these first.");
            return 3;
        }

        RenderListOfLayoutsPlugin lolPlugin = (RenderListOfLayoutsPlugin)plugin.getListOfLayouts().getPlugin("render");
        if (lolPlugin != null && lolPlugin.getNumGlobalRenderInformationObjects() > 0)
        {
            Console.WriteLine("The loaded model contains global Render information. ");
        }

        // add render information to the first layout
        Layout layout = plugin.getLayout(0);

        RenderLayoutPlugin rPlugin = (RenderLayoutPlugin)layout.getPlugin("render");
        if (rPlugin != null && rPlugin.getNumLocalRenderInformationObjects() > 0)
        {
            Console.WriteLine("The loaded model contains local Render information. ");
        }
        else
        {
            string uri = doc.getLevel() == 2 ? RenderExtension.getXmlnsL2() : RenderExtension.getXmlnsL3V1V1();

            // enable render package
            doc.enablePackage(uri, "render", true);
            doc.setPackageRequired("render", false);

            rPlugin = (RenderLayoutPlugin)layout.getPlugin("render");

            addRenderInformation(rPlugin);

            libsbml.writeSBMLToFile(doc, outputFile);
        }
        return 0;

    }

}
