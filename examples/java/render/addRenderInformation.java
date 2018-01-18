// 
// @file    addRenderInformation.java
// @brief   Adds render information to the given SBML file
// @author  Frank Bergmann
// 
// This file is part of libSBML.  Please visit http://sbml.org for more
// information about SBML, and the latest version of libSBML.
// 

import org.sbml.libsbml.ColorDefinition;
import org.sbml.libsbml.Ellipse;
import org.sbml.libsbml.GradientStop;
import org.sbml.libsbml.Layout;
import org.sbml.libsbml.LayoutModelPlugin;
import org.sbml.libsbml.LinearGradient;
import org.sbml.libsbml.LocalRenderInformation;
import org.sbml.libsbml.Model;
import org.sbml.libsbml.RelAbsVector;
import org.sbml.libsbml.RenderExtension;
import org.sbml.libsbml.RenderLayoutPlugin;
import org.sbml.libsbml.RenderListOfLayoutsPlugin;
import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.Style;
import org.sbml.libsbml.libsbml;

public class addRenderInformation {

	public static void addRenderInformationToPlugin(RenderLayoutPlugin rPlugin) {

		if (rPlugin == null) {

			System.err.println("could not add render information!");
			System.exit(4);
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

		// add a species style that represents them as ellipses with the
		// gradient above
		Style style = rInfo.createStyle("ellipseStyle");
		style.getGroup().setFillColor("simpleGradient");
		style.getGroup().setStroke("black");
		style.getGroup().setStrokeWidth(2.0);
		style.addType("SPECIESGLYPH");

		Ellipse ellipse = style.getGroup().createEllipse();
		ellipse.setCenter2D(new RelAbsVector(0, 50), new RelAbsVector(0, 50));
		ellipse.setRadii(new RelAbsVector(0, 50), new RelAbsVector(0, 50));

	}

	public static void main(String[] args) {
		if (args.length != 2) {
			System.err
					.println("usage: addRenderInformation <input file> <output file>");
			System.err
					.println("       Adds a render information object to the input file.");
			System.exit(1);
		}

		String inputFile = args[0];
		String outputFile = args[1];

		SBMLDocument doc = libsbml.readSBMLFromFile(inputFile);
		long numErrors = doc.getNumErrors();

		if (numErrors > 0) {
			System.err.println("Encountered errors while reading the file. ");
			System.err
					.println("Please correct the following errors and try again.");
			doc.printErrors();
			System.exit(2);
		}

		Model model = doc.getModel();

		LayoutModelPlugin plugin = (LayoutModelPlugin) model
				.getPlugin("layout");

		if (plugin == null || plugin.getNumLayouts() == 0) {
			System.err
					.println("The loaded model contains no layout information, please add these first.");
			System.exit(3);
		}

		RenderListOfLayoutsPlugin lolPlugin = (RenderListOfLayoutsPlugin) plugin
				.getListOfLayouts().getPlugin("render");
		if (lolPlugin != null
				&& lolPlugin.getNumGlobalRenderInformationObjects() > 0) {
			System.out
					.println("The loaded model contains global Render information. ");
		}

		// add render information to the first layout
		Layout layout = plugin.getLayout(0);

		RenderLayoutPlugin rPlugin = (RenderLayoutPlugin) layout
				.getPlugin("render");
		if (rPlugin != null
				&& rPlugin.getNumLocalRenderInformationObjects() > 0) {
			System.out
					.println("The loaded model contains local Render information. ");
		} else {
			String uri = (doc.getLevel() == 2 ? RenderExtension.getXmlnsL2()
					: RenderExtension.getXmlnsL3V1V1());

			// enable render package
			doc.enablePackage(uri, "render", true);
			doc.setPackageRequired("render", false);

			rPlugin = (RenderLayoutPlugin) layout.getPlugin("render");

			addRenderInformationToPlugin(rPlugin);

			libsbml.writeSBMLToFile(doc, outputFile);

		}
		System.exit(0);
	}
}
