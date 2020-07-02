/**
 * @file    spatial_example1.java
 * @brief   Creates a sample spatial model.
 * @author  Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This sample program is distributed under a different license than the rest
 * of libSBML.  This program uses the open-source MIT license, as follows:
 *
 * Copyright (c) 2013-2018 by the California Institute of Technology
 * (California, USA), the European Bioinformatics Institute (EMBL-EBI, UK)
 * and the University of Heidelberg (Germany), with support from the National
 * Institutes of Health (USA) under grant R01GM070923.  All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * Neither the name of the California Institute of Technology (Caltech), nor
 * of the European Bioinformatics Institute (EMBL-EBI), nor of the University
 * of Heidelberg, nor the names of any contributors, may be used to endorse
 * or promote products derived from this software without specific prior
 * written permission.
 * ------------------------------------------------------------------------ -->
 */

import org.sbml.libsbml.*;

public class spatial_example1
{
    public static void main(String args[])
    {
        System.loadLibrary("sbmlj");
        
        
        SpatialPkgNamespaces spatialNs = new SpatialPkgNamespaces();
        
        // create the document
        SBMLDocument doc = new SBMLDocument(spatialNs);
        doc.setPackageRequired("spatial", true);
        
        // create the model
        Model model = doc.createModel();
        model.setId("trial_spatial");
        model.setName("trial_spatial");

        // create compartment
        Compartment comp = model.createCompartment();
        comp.setId("cytosol");
        comp.setConstant(true);
        
        // create the Species
        Species species1 = model.createSpecies();
        species1.setId("ATPc");
        species1.setCompartment("cytosol");
        species1.setInitialConcentration(1.0);
        species1.setHasOnlySubstanceUnits(false);
        species1.setBoundaryCondition(false);
        species1.setConstant(false);
        // spatial package extension to species.
        SpatialSpeciesPlugin srplugin = (SpatialSpeciesPlugin)(species1.getPlugin("spatial"));
        srplugin.setIsSpatial(true);
        
        // add parameter for diff coeff of species1
        Parameter paramSp = model.createParameter();
        paramSp.setId(species1.getId()+"_dc");
        paramSp.setValue(1.0);
        // spatial package extension to parameter.
        SpatialParameterPlugin pplugin = (SpatialParameterPlugin)(paramSp.getPlugin("spatial"));
        DiffusionCoefficient diffCoeff = pplugin.createDiffusionCoefficient();
        diffCoeff.setVariable(species1.getId());
        diffCoeff.setType(libsbml.SPATIAL_DIFFUSIONKIND_ANISOTROPIC);
        diffCoeff.setCoordinateReference1(libsbml.SPATIAL_COORDINATEKIND_CARTESIAN_X);


        // add parameter for adv coeff of species1
        paramSp = model.createParameter();
        paramSp.setId(species1.getId()+"_ac");
        paramSp.setValue(1.5);
        // spatial package extension to parameter.
        pplugin = (SpatialParameterPlugin)(paramSp.getPlugin("spatial"));
        AdvectionCoefficient advCoeff = pplugin.createAdvectionCoefficient();
        advCoeff.setVariable(species1.getId());
        advCoeff.setCoordinate(libsbml.SPATIAL_COORDINATEKIND_CARTESIAN_X);
        
        // add parameter for boundary condition of species1
        paramSp = model.createParameter();
        paramSp.setId(species1.getId()+"_bc");
        paramSp.setValue(2.0);
        // spatial package extension to parameter.
        pplugin = (SpatialParameterPlugin)(paramSp.getPlugin("spatial"));
        BoundaryCondition boundCon = pplugin.createBoundaryCondition();
        boundCon.setVariable(species1.getId());  
        boundCon.setType(libsbml.SPATIAL_BOUNDARYKIND_DIRICHLET);
        boundCon.setCoordinateBoundary("Xmin");


        Species species2 = model.createSpecies();
        species2.setId("ADPc");
        species2.setCompartment("cytosol");
        species2.setInitialConcentration(1);
        species2.setHasOnlySubstanceUnits(false);
        species2.setBoundaryCondition(false);
        species2.setConstant(false);
        srplugin = (SpatialSpeciesPlugin)(species2.getPlugin("spatial"));
        srplugin.setIsSpatial(true);


        Reaction reaction = model.createReaction();
        reaction.setId("rxn1");
        reaction.setReversible(false);
        reaction.setFast(false);
        reaction.setCompartment("cytosol");
        SpatialReactionPlugin rplugin = (SpatialReactionPlugin)(reaction.getPlugin("spatial"));
        rplugin.setIsLocal(true);

        //
        // Get a SpatialModelPlugin object plugged in the model object.
        //
        // The type of the returned value of SBase::getPlugin() function is 
        // SBasePlugin, and thus the value needs to be casted for the 
        // corresponding derived class.
        //
        SpatialModelPlugin mplugin = (SpatialModelPlugin)(model.getPlugin("spatial"));

        //
        // Creates a geometry object via SpatialModelPlugin object.
        //
        Geometry geometry = mplugin.createGeometry();
        geometry.setCoordinateSystem(libsbml.SPATIAL_GEOMETRYKIND_CARTESIAN);

        CoordinateComponent coordX = geometry.createCoordinateComponent();
        coordX.setId("coordComp1");
        coordX.setType(libsbml.SPATIAL_COORDINATEKIND_CARTESIAN_X);
        coordX.setUnit("umeter");
        Boundary minX = coordX.createBoundaryMin();
        minX.setId("Xmin");
        minX.setValue(0.0);
        Boundary maxX = coordX.createBoundaryMax();
        maxX.setId("Xmax");
        maxX.setValue(10.0);

        Parameter paramX = model.createParameter();
        paramX.setId("x");
        paramX.setValue(8.0);
        // spatial package extension to parameter.
        // SpatialParameterPlugin pplugin;
        pplugin = (SpatialParameterPlugin)(paramX.getPlugin("spatial"));
        SpatialSymbolReference spSymRef = pplugin.createSpatialSymbolReference();
        spSymRef.setSpatialRef(coordX.getId());

        DomainType domainType = geometry.createDomainType();
        domainType.setId("dtype1");
        domainType.setSpatialDimensions(3);


        // Spatial package extension to compartment (mapping compartment with domainType)
        SpatialCompartmentPlugin cplugin = (SpatialCompartmentPlugin)(comp.getPlugin("spatial"));
        CompartmentMapping compMapping = cplugin.createCompartmentMapping();
        compMapping.setId("compMap1");
        compMapping.setDomainType(domainType.getId());
        compMapping.setUnitSize(1.0);
        
        Domain domain = geometry.createDomain();
        domain.setId("domain1");
        domain.setDomainType("dtype1");
        InteriorPoint internalPt1 = domain.createInteriorPoint();
        internalPt1.setCoord1(1.0);

        domain = geometry.createDomain();
        domain.setId("domain2");
        domain.setDomainType("dtype1");
        InteriorPoint internalPt2 = domain.createInteriorPoint();
        internalPt2.setCoord1(5.0);

        AdjacentDomains adjDomain = geometry.createAdjacentDomains();
        adjDomain.setId("adjDomain1");
        adjDomain.setDomain1("domain1");
        adjDomain.setDomain2("domain2");

        AnalyticGeometry analyticGeom = geometry.createAnalyticGeometry();
        analyticGeom.setId("analyticGeom1");
        AnalyticVolume analyticVol = analyticGeom.createAnalyticVolume();
        analyticVol.setId("analyticVol1");
        analyticVol.setDomainType(domainType.getId());
        analyticVol.setFunctionType(libsbml.SPATIAL_FUNCTIONKIND_LAYERED);
        analyticVol.setOrdinal(1);
        ASTNode mathNode = libsbml.parseL3Formula("x*x-1.0");
        analyticVol.setMath(mathNode);

        SampledFieldGeometry sfg = geometry.createSampledFieldGeometry();
        sfg.setId("sampledFieldGeom1");
        sfg.setSampledField("sampledField1");
        SampledField sampledField = geometry.createSampledField();
        sampledField.setId("sampledField1");
        sampledField.setNumSamples1(4);
        sampledField.setNumSamples2(4);
        sampledField.setNumSamples3(2);
        sampledField.setInterpolationType("linear");
        sampledField.setCompression("uncompressed");
        int[] samples = {
	                       // z=0
	                       0,0,0,0,
	                       0,1,1,0,
					       0,1,1,0,
					       0,0,0,0,
					       // z=1
					       0,0,0,0,
					       0,1,1,0,
					       0,1,1,0,
					       0,0,0,0
        };
        sampledField.setDataType("uint8");
        sampledField.setSamples(samples, 32);
        SampledVolume sampledVol = sfg.createSampledVolume();
        sampledVol.setId("sv_1");
        sampledVol.setDomainType(domainType.getId());
        sampledVol.setSampledValue(128.0);
        sampledVol.setMinValue(0.0);
        sampledVol.setMaxValue(255.0);
        
        libsbml.writeSBMLToFile(doc, "spatial_example1.xml");
    }


  static void println (String msg)
  {
    System.out.println(msg);
  }


  /**
   * Loads the SWIG-generated libSBML Java module when this class is
   * loaded, or reports a sensible diagnostic message about why it failed.
   */
  static
  {
    String varname;
    String shlibname;

    if (System.getProperty("os.name").startsWith("Mac OS"))
    {
      varname = "DYLD_LIBRARY_PATH";    // We're on a Mac.
      shlibname = "'libsbmlj.jnilib'";
    }
    else
    {
      varname = "LD_LIBRARY_PATH";      // We're not on a Mac.
      shlibname = "'libsbmlj.so' and/or 'libsbml.so'";
    }

    try
    {
      System.loadLibrary("sbmlj");
      // For extra safety, check that the jar file is in the classpath.
      Class.forName("org.sbml.libsbml.libsbml");
    }
    catch (UnsatisfiedLinkError e)
    {
      System.err.println("Error encountered while attempting to load libSBML:");
      System.err.println("Please check the value of your " + varname +
                         " environment variable and/or" +
                         " your 'java.library.path' system property" +
                         " (depending on which one you are using) to" +
                         " make sure it list the directories needed to" +
                         " find the " + shlibname + " library file and the" +
                         " libraries it depends upon (e.g., the XML parser).");
      System.exit(1);
    }
    catch (ClassNotFoundException e)
    {
      System.err.println("Error: unable to load the file 'libsbmlj.jar'." +
                         " It is likely that your -classpath command line " +
                         " setting or your CLASSPATH environment variable " +
                         " do not include the file 'libsbmlj.jar'.");
      e.printStackTrace();

      System.exit(1);
    }
    catch (SecurityException e)
    {
      System.err.println("Error encountered while attempting to load libSBML:");
      e.printStackTrace();
      System.err.println("Could not load the libSBML library files due to a"+
                         " security exception.\n");
      System.exit(1);
    }
  }
}
