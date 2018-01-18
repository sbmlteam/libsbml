// 
// @file    printRenderInformation.java
// @brief   prints an overview of the render information in the given SBML file
// @author  Frank Bergmann
// 
// This file is part of libSBML.  Please visit http://sbml.org for more
// information about SBML, and the latest version of libSBML.
// 
import org.sbml.libsbml.ColorDefinition;
import org.sbml.libsbml.GlobalRenderInformation;
import org.sbml.libsbml.GlobalStyle;
import org.sbml.libsbml.GradientBase;
import org.sbml.libsbml.GradientStop;
import org.sbml.libsbml.Layout;
import org.sbml.libsbml.LayoutModelPlugin;
import org.sbml.libsbml.LinearGradient;
import org.sbml.libsbml.Model;
import org.sbml.libsbml.RadialGradient;
import org.sbml.libsbml.RelAbsVector;
import org.sbml.libsbml.RenderLayoutPlugin;
import org.sbml.libsbml.RenderListOfLayoutsPlugin;
import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.libsbml;

public class printRenderInformation {
	public static String toString(RelAbsVector vec) {
		return vec.getAbsoluteValue() + " + " + vec.getRelativeValue() + "%";
	}

	public static void main(String[] args) {
		if (args.length != 1) {
			System.err.println("usage: printRenderInformation <input file> ");
			System.err
					.println("       prints a summary of the render information object.");
			System.exit(1);
		}

		String inputFile = args[0];

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
					.println("The loaded model contains global Render information: ");

			for (long i = 0; i < lolPlugin
					.getNumGlobalRenderInformationObjects(); i++) {
				GlobalRenderInformation info = lolPlugin
						.getRenderInformation(i);

				System.out.println("Id: " + info.getId());
				System.out.println("Name: " + info.getName());
				System.out.println("Program Name: " + info.getProgramName());
				System.out.println("Program Version: "
						+ info.getProgramVersion());
				System.out.println("Background color: "
						+ info.getBackgroundColor());

				System.out.println("Color Definitions:");
				for (long j = 0; j < info.getNumColorDefinitions(); j++) {
					ColorDefinition color = info.getColorDefinition(j);
					System.out.println("\tcolor: " + j + " id: "
							+ color.getId() + " color: "
							+ color.createValueString());
				}

				System.out.println("GradientDefinitions: ");
				for (long j = 0; j < info.getNumGradientDefinitions(); j++) {
					GradientBase gBase = info.getGradientDefinition(j);
					LinearGradient linear = (LinearGradient) gBase;
					RadialGradient radial = (RadialGradient) gBase;

					if (linear != null) {
						System.out.println("\tLinear Gradient: "
								+ linear.getId() + " start: "
								+ toString(linear.getXPoint1()) + ", "
								+ toString(linear.getYPoint1()) + " end: "
								+ toString(linear.getXPoint2()) + ", "
								+ toString(linear.getYPoint2()));
					} else if (radial != null) {
						System.out.println("\tRadial Gradient: "
								+ radial.getId() + " center: "
								+ toString(radial.getCenterX()) + ", "
								+ toString(radial.getCenterY()) + " focal: "
								+ toString(radial.getFocalPointX()) + ", "
								+ toString(radial.getFocalPointY()));
					}

					for (int k = 0; k < gBase.getNumGradientStops(); k++) {
						GradientStop stop = gBase.getGradientStop(k);
						System.out.println("\t\tstop " + k + " id: "
								+ stop.getId() + " stop-color: "
								+ stop.getStopColor());
					}

				} // gradient definitions

				// similarly for the remaining elements
				System.out.println("\tNumber of Line Endings: "
						+ info.getNumLineEndings());

				// and finally the styles

				for (long j = 0; j < info.getNumStyles(); j++) {
					GlobalStyle style = info.getStyle(j);

					System.out.println("\tstyle " + j + " id: " + style.getId()
							+ " applies to: ");
					System.out.println("\t\troles:" + style.createRoleString()
							+ " types: " + style.createTypeString());

				}

			} // for: global render information

		} // numGlobalRenderInformation > 0

		// add render information to the first layout
		Layout layout = plugin.getLayout(0);

		RenderLayoutPlugin rPlugin = (RenderLayoutPlugin) layout
				.getPlugin("render");
		if (rPlugin != null
				&& rPlugin.getNumLocalRenderInformationObjects() > 0) {
			System.out
					.println("The loaded model contains local Render information. ");
			// here we would do the same as above for the local render
			// information ...
		}

		System.exit(0);
	}
}
