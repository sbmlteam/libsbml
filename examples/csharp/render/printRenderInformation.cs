// 
// @file    printRenderInformation.cs
// @brief   prints an overview of the render information in the given SBML file
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

public class cs_printRenderInformation
{


    public static String toString(RelAbsVector vec)
    {
        return vec.getAbsoluteValue().ToString() + " + " + vec.getRelativeValue().ToString() + "%";
    }

    public static int Main(string[] args)
    {
        if (args.Length != 1)
        {
            Console.WriteLine(" Usage:  printRenderInformation <input file> \n" +
                    "      prints a summary of the render information object.");
            return 1;
        }


        string inputFile = args[0];

        SBMLDocument doc = libsbml.readSBMLFromFile(inputFile);

        Console.WriteLine("Using libSBML: {0} supporting packages for:", libsbml.getLibSBMLDottedVersion());
        for (int i = 0; i < SBMLExtensionRegistry.getNumRegisteredPackages(); ++i)
            Console.WriteLine("\t {0}", SBMLExtensionRegistry.getRegisteredPackageName(i));

        Console.WriteLine("\nThe document is: level {0}, version {1}", doc.getLevel(), doc.getVersion());
        for (int i = 0; i < doc.getNumPlugins(); ++i)
            Console.WriteLine("  doc uses package: {0}", doc.getPlugin(i).getElementNamespace());

        Console.WriteLine("\n");

        long numErrors = doc.getNumErrors(libsbml.LIBSBML_SEV_ERROR);

        if (numErrors > 0)
        {
            Console.WriteLine("Encountered errors while reading the file. ");
            Console.WriteLine("Please correct the following errors and try again.");
            doc.printErrors();
            return 2;
        }

        Model model = doc.getModel();

        LayoutModelPlugin plugin = (LayoutModelPlugin) model.getPlugin("layout");

        if (plugin == null || plugin.getNumLayouts() == 0)
        {
            Console.WriteLine("The loaded model contains no layout information, please add these first.");
            return 3;
        }

        RenderListOfLayoutsPlugin lolPlugin = (RenderListOfLayoutsPlugin)plugin.getListOfLayouts().getPlugin("render");
        if (lolPlugin != null && lolPlugin.getNumGlobalRenderInformationObjects() > 0)
        {
            Console.WriteLine("The loaded model contains global Render information: ");

            for (int i = 0; i < lolPlugin.getNumGlobalRenderInformationObjects(); ++i)
            {
                GlobalRenderInformation info = lolPlugin.getRenderInformation(i);
                print_render_info(info);
            }
        }

        Layout layout = plugin.getLayout(0);

        RenderLayoutPlugin rPlugin = (RenderLayoutPlugin)layout.getPlugin("render");
        if (rPlugin != null && rPlugin.getNumLocalRenderInformationObjects() > 0)
        {
            Console.WriteLine("The loaded model contains local Render information. ");
            // here we would do the same as above for the local render information ...
            for (int i = 0; i < rPlugin.getNumLocalRenderInformationObjects(); ++i)
            {
            LocalRenderInformation info = rPlugin.getRenderInformation(i);
            print_render_info(info);
            }
        }

        return 0;
    }

    public static void print_render_info(GlobalRenderInformation info)
    {
        print_render_info((RenderInformationBase)info);

        // and finally the styles
        Console.WriteLine("\nStyles: ");
        for (int j = 0; j < info.getNumGlobalStyles(); ++j)
        {
            GlobalStyle style = info.getGlobalStyle(j);
            print_style(style, j);
        }
    }

    public static void print_render_info(LocalRenderInformation info)
    {
        print_render_info((RenderInformationBase)info);

        // and finally the styles
        Console.WriteLine("\nStyles: ");
        for (int j = 0; j < info.getNumLocalStyles(); ++j)
        {
            LocalStyle style = info.getLocalStyle(j);
            print_style(style, j);
        }
    }

    public static void print_render_info(RenderInformationBase info)
    {
        if (info.isSetId())
            Console.WriteLine("  Id: " + info.getId());
        if (info.isSetName())
            Console.WriteLine("  Name: " + info.getName());
        if (info.isSetProgramName())
            Console.WriteLine("  Program Name: " + info.getProgramName());
        if (info.isSetProgramVersion())
            Console.WriteLine("  Program Version: " + info.getProgramVersion());
        if (info.isSetBackgroundColor())
            Console.WriteLine("  Background color: " + info.getBackgroundColor());

        Console.WriteLine("\nColor Definitions:");
        for (int j = 0; j < info.getNumColorDefinitions(); ++j)
        {
            ColorDefinition color = info.getColorDefinition(j);
            Console.WriteLine("\tcolor: " + j.ToString() +
                              " id: " + color.getId() +
                              " color: " + color.getValue());
        }

        Console.WriteLine("\nGradientDefinitions: ");
        for (int j = 0; j < info.getNumGradientDefinitions(); ++j)
        {
            GradientBase grad = info.getGradientDefinition(j);

            print_gradient_definition(grad);
        }

        // similarly for the remaining elements 
        Console.WriteLine("\nNumber of Line Endings: {0}", info.getNumLineEndings());
    }

    public static void print_style(Style style, int index)
    {
        Console.WriteLine("\tstyle " + index.ToString() + " id: " + style.getId() + " applies to: ");
        Console.WriteLine("\t\troles:" + style.createRoleString() +
                          " types: " + style.createTypeString());

        if (!style.isSetGroup())
            return;

        RenderGroup group = style.getGroup();
        if (group.isSetStroke())
            Console.WriteLine("\t\tstroke: {0}",group.getStroke());

        if (group.isSetFill())
            Console.WriteLine("\t\tfill: {0}", group.getFill());

        for (int j = 0; j < group.getNumElements(); ++j)
        {
            object element = group.getElement(j);
            if (element is GraphicalPrimitive2D)
                Console.WriteLine("\t\tsub element {0} stroke {1}, fill {2}", ((GraphicalPrimitive2D)element).getElementName(), ((GraphicalPrimitive2D)element).getStroke(), ((GraphicalPrimitive2D)element).getFill());
            else if (element is GraphicalPrimitive1D)
                Console.WriteLine("\t\tsub element {0} stroke {1}", ((GraphicalPrimitive1D)element).getElementName(), ((GraphicalPrimitive1D)element).getStroke());
        }

    }

    public static void print_gradient_definition(GradientBase element)
    {
        if (element is LinearGradient)
        {            
            LinearGradient grad = (LinearGradient)element;
            Console.WriteLine("\tLinear Gradient: " + grad.getId()
                              + " start: " + toString(grad.getXPoint1()) + ", " +
                              toString(grad.getYPoint1())
                              + " end: " + toString(grad.getXPoint2()) + ", " +
                              toString(grad.getYPoint2())
            );
        }
        else if (element is RadialGradient)
        {
            RadialGradient grad = (RadialGradient)element;
            Console.WriteLine("\tRadial Gradient: " + grad.getId()
                              + " center: " + toString(grad.getCenterX()) + ", " +
                              toString(grad.getCenterY())
                              + " focal: " + toString(grad.getFocalPointX()) + ", " +
                              toString(grad.getFocalPointY())
                );
        }

        for (int k = 0; k < element.getNumGradientStops(); ++k)
        {
            GradientStop stop = element.getGradientStop(k);
            if (stop == null)
            {
                continue;
            }
            Console.WriteLine("\t\tstop " + k.ToString() + " id: " + stop.getId() + " stop-color: " +
                              stop.getStopColor());
        }
    }
}
