#!/usr/bin/env python3
## 
## @file    createExampleSBML.py
## @brief   Creates example SBML models presented in the SBML specification.
## @author  Akiya Jouraku
## @author  Michael Hucka
## @author  Sarah Keating
## 
## <!--------------------------------------------------------------------------
## This sample program is distributed under a different license than the rest
## of libSBML.  This program uses the open-source MIT license, as follows:
##
## Copyright (c) 2013-2018 by the California Institute of Technology
## (California, USA), the European Bioinformatics Institute (EMBL-EBI, UK)
## and the University of Heidelberg (Germany), with support from the National
## Institutes of Health (USA) under grant R01GM070923.  All rights reserved.
##
## Permission is hereby granted, free of charge, to any person obtaining a
## copy of this software and associated documentation files (the "Software"),
## to deal in the Software without restriction, including without limitation
## the rights to use, copy, modify, merge, publish, distribute, sublicense,
## and/or sell copies of the Software, and to permit persons to whom the
## Software is furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in
## all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
## THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
## FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
## DEALINGS IN THE SOFTWARE.
##
## Neither the name of the California Institute of Technology (Caltech), nor
## of the European Bioinformatics Institute (EMBL-EBI), nor of the University
## of Heidelberg, nor the names of any contributors, may be used to endorse
## or promote products derived from this software without specific prior
## written permission.
## ------------------------------------------------------------------------ -->
## 


import sys
import os.path
from libsbml import *

#
# These variables are used in writeExampleSBML when writing an SBML
# document.  They are handed to libSBML functions in order to include
# the program information into comments within the SBML file.
#
ProgramName = "createExampleModels"
ProgramVersion = "1.0.0"

#
# The SBML Level and Version of the example SBML models.
#
Level = 2
Version = 4


# ===============================================================================
#
#
# Functions for creating the Example SBML documents.
#
#
# ===============================================================================


# 
# 
# Creates an SBML model represented in "7.1 A Simple example application of SBML"
# in the SBML Level 2 Version 4 Specification.
# 
# 

def createExampleEnzymaticReaction():
    level = Level
    version = Version

    # ---------------------------------------------------------------------------
    #
    # Creates an SBMLDocument object 
    #
    # ---------------------------------------------------------------------------

    sbmlDoc = SBMLDocument(level, version)

    # ---------------------------------------------------------------------------
    #
    # Creates a Model object inside the SBMLDocument object. 
    #
    # ---------------------------------------------------------------------------

    model = sbmlDoc.createModel()
    model.setId("EnzymaticReaction")

    # ---------------------------------------------------------------------------
    #
    # Creates UnitDefinition objects inside the Model object.
    #
    # ---------------------------------------------------------------------------

    # ---------------------------------------------------------------------------
    # (UnitDefinition1) Creates an UnitDefinition object ("per_second")
    # ---------------------------------------------------------------------------

    unitdef = model.createUnitDefinition()
    unitdef.setId("per_second")

    #  Creates an Unit inside the UnitDefinition object 

    unit = unitdef.createUnit()
    unit.setKind(UNIT_KIND_SECOND)
    unit.setExponent(-1)

    # --------------------------------------------------------------------------------
    # (UnitDefinition2) Creates an UnitDefinition object ("litre_per_mole_per_second") 
    # --------------------------------------------------------------------------------

    # Note that we can reuse the pointers 'unitdef' and 'unit' because the
    # actual UnitDefinition object (along with the Unit objects within it)
    # is already attached to the Model object.

    unitdef = model.createUnitDefinition()
    unitdef.setId("litre_per_mole_per_second")

    #  Creates an Unit inside the UnitDefinition object ("litre_per_mole_per_second")

    unit = unitdef.createUnit()
    unit.setKind(UNIT_KIND_MOLE)
    unit.setExponent(-1)

    #  Creates an Unit inside the UnitDefinition object ("litre_per_mole_per_second")

    unit = unitdef.createUnit()
    unit.setKind(UNIT_KIND_LITRE)
    unit.setExponent(1)

    #  Creates an Unit inside the UnitDefinition object ("litre_per_mole_per_second")

    unit = unitdef.createUnit()
    unit.setKind(UNIT_KIND_SECOND)
    unit.setExponent(-1)

    # ---------------------------------------------------------------------------
    #
    # Creates a Compartment object inside the Model object. 
    #
    # ---------------------------------------------------------------------------

    compName = "cytosol"

    # Creates a Compartment object ("cytosol")

    comp = model.createCompartment()
    comp.setId(compName)

    # Sets the "size" attribute of the Compartment object.
    #
    # We are not setting the units on the compartment size explicitly, so
    # the units of this Compartment object will be the default SBML units of
    # volume, which are liters.
    #
    comp.setSize(1e-14)

    # ---------------------------------------------------------------------------
    #
    # Creates Species objects inside the Model object. 
    #
    # ---------------------------------------------------------------------------

    # ---------------------------------------------------------------------------
    # (Species1) Creates a Species object ("ES")
    # ---------------------------------------------------------------------------

    # Create the Species objects inside the Model object. 

    sp = model.createSpecies()
    sp.setId("ES")
    sp.setName("ES")

    # Sets the "compartment" attribute of the Species object to identify the 
    # compartment in which the Species object is located.

    sp.setCompartment(compName)

    # Sets the "initialAmount" attribute of the Species object.
    #
    #  In SBML, the units of a Species object's initial quantity are
    #  determined by two attributes, "substanceUnits" and
    #  "hasOnlySubstanceUnits", and the "spatialDimensions" attribute
    #  of the Compartment object ("cytosol") in which the species
    #  object is located.  Here, we are using the default values for
    #  "substanceUnits" (which is "mole") and "hasOnlySubstanceUnits"
    #  (which is "False").  The compartment in which the species is
    #  located uses volume units of liters, so the units of these
    #  species (when the species appear in numerical formulas in the
    #  model) will be moles/liters.  
    #
    sp.setInitialAmount(0)

    # ---------------------------------------------------------------------------
    # (Species2) Creates a Species object ("P")
    # ---------------------------------------------------------------------------

    sp = model.createSpecies()
    sp.setCompartment(compName)
    sp.setId("P")
    sp.setName("P")
    sp.setInitialAmount(0)

    # ---------------------------------------------------------------------------
    # (Species3) Creates a Species object ("S")
    # ---------------------------------------------------------------------------

    sp = model.createSpecies()
    sp.setCompartment(compName)
    sp.setId("S")
    sp.setName("S")
    sp.setInitialAmount(1e-20)

    # ---------------------------------------------------------------------------
    # (Species4) Creates a Species object ("E")
    # ---------------------------------------------------------------------------

    sp = model.createSpecies()
    sp.setCompartment(compName)
    sp.setId("E")
    sp.setName("E")
    sp.setInitialAmount(5e-21)

    # ---------------------------------------------------------------------------
    #
    # Creates Reaction objects inside the Model object. 
    #
    # ---------------------------------------------------------------------------

    # ---------------------------------------------------------------------------
    # (Reaction1) Creates a Reaction object ("veq").
    # ---------------------------------------------------------------------------

    reaction = model.createReaction()
    reaction.setId("veq")

    # (Reactant1) Creates a Reactant object that references Species "E"
    # in the model.  The object will be created within the reaction in the
    # SBML <listOfReactants>.

    spr = reaction.createReactant()
    spr.setSpecies("E")

    # (Reactant2) Creates a Reactant object that references Species "S"
    # in the model.

    spr = reaction.createReactant()
    spr.setSpecies("S")

    # ---------------------------------------------------------------------------
    # (Product1) Creates a Product object that references Species "ES" in
    # the model.
    # ---------------------------------------------------------------------------

    spr = reaction.createProduct()
    spr.setSpecies("ES")

    # ---------------------------------------------------------------------------
    # Creates a KineticLaw object inside the Reaction object ("veq"). 
    # ---------------------------------------------------------------------------

    kl = reaction.createKineticLaw()

    # ---------------------------------------------------------------------------
    # Creates an object which represents the following math of the
    # KineticLaw.
    #
    #      <math xmlns="http://www.w3.org/1998/Math/MathML">
    #        <apply>
    #          <times/>
    #          <ci> cytosol </ci>
    #          <apply>
    #            <minus/>
    #            <apply>
    #              <times/>
    #              <ci> kon </ci>
    #              <ci> E </ci>
    #              <ci> S </ci>
    #            </apply>
    #            <apply>
    #              <times/>
    #              <ci> koff </ci>
    #              <ci> ES </ci>
    #            </apply>
    #          </apply>
    #        </apply>
    #      </math>
    #
    # ---------------------------------------------------------------------------

    # ------------------------------------------
    #
    # create nodes representing the variables
    #
    # ------------------------------------------

    astCytosol = ASTNode(AST_NAME)
    astCytosol.setName("cytosol")

    astKon = ASTNode(AST_NAME)
    astKon.setName("kon")

    astKoff = ASTNode(AST_NAME)
    astKoff.setName("koff")

    astE = ASTNode(AST_NAME)
    astE.setName("E")

    astS = ASTNode(AST_NAME)
    astS.setName("S")

    astES = ASTNode(AST_NAME)
    astES.setName("ES")

    # --------------------------------------------
    #
    # create node representing
    #            <apply>
    #              <times/>
    #              <ci> koff </ci>
    #              <ci> ES </ci>
    #            </apply>
    #
    # --------------------------------------------

    astTimes1 = ASTNode(AST_TIMES)
    astTimes1.addChild(astKoff)
    astTimes1.addChild(astES)

    # --------------------------------------------
    #
    # create node representing
    #            <apply>
    #              <times/>
    #              <ci> kon </ci>
    #              <ci> E </ci>
    #              <ci> S </ci>
    #            </apply>
    #
    #
    # (NOTES)
    #
    #  Since there is a restriction with an of "<times/>" operation
    #  such that the is a binary class and thus only two operands can
    #  be directly added, the following code in this comment block is invalid
    #  because the code directly adds three <ci> ASTNodes to <times/> ASTNode.
    #
    #    *astTimes = ASTNode(AST_TIMES);
    #    astTimes.addChild(astKon);
    #    astTimes.addChild(astE);
    #    astTimes.addChild(astS);
    #
    # The following valid code after this comment block creates the ASTNode
    # as a binary tree.
    #
    # Please see "Converting between ASTs and text strings" described
    # at http://sbml.org/Software/libSBML/docs/cpp-api/class_a_s_t_node.html
    # for the detailed information.
    #
    # --------------------------------------------

    astTimes2 = ASTNode(AST_TIMES)
    astTimes2.addChild(astE)
    astTimes2.addChild(astS)

    astTimes = ASTNode(AST_TIMES)
    astTimes.addChild(astKon)
    astTimes.addChild(astTimes2)

    # --------------------------------------------
    #
    # create node representing
    #          <apply>
    #            <minus/>
    #            <apply>
    #              <times/>
    #              <ci> kon </ci>
    #              <ci> E </ci>
    #              <ci> S </ci>
    #            </apply>
    #            <apply>
    #              <times/>
    #              <ci> koff </ci>
    #              <ci> ES </ci>
    #            </apply>
    #          </apply>
    #
    # --------------------------------------------

    astMinus = ASTNode(AST_MINUS)
    astMinus.addChild(astTimes)
    astMinus.addChild(astTimes1)

    # --------------------------------------------
    #
    # create node representing
    #        <apply>
    #          <times/>
    #          <ci> cytosol </ci>
    #          <apply>
    #            <minus/>
    #            <apply>
    #              <times/>
    #              <ci> kon </ci>
    #              <ci> E </ci>
    #              <ci> S </ci>
    #            </apply>
    #            <apply>
    #              <times/>
    #              <ci> koff </ci>
    #              <ci> ES </ci>
    #            </apply>
    #          </apply>
    #        </apply>
    #
    # --------------------------------------------

    astMath = ASTNode(AST_TIMES)
    astMath.addChild(astCytosol)
    astMath.addChild(astMinus)

    # ---------------------------------------------
    #
    # set the Math element
    #
    # ------------------------------------------------

    kl.setMath(astMath)

    # ---------------------------------------------------------------------------
    # Creates local Parameter objects inside the KineticLaw object.
    # ---------------------------------------------------------------------------

    # Creates a Parameter ("kon")

    para = kl.createParameter()
    para.setId("kon")
    para.setValue(1000000)
    para.setUnits("litre_per_mole_per_second")

    # Creates a Parameter ("koff")

    para = kl.createParameter()
    para.setId("koff")
    para.setValue(0.2)
    para.setUnits("per_second")

    # ---------------------------------------------------------------------------
    # (Reaction2) Creates a Reaction object ("vcat") .
    # ---------------------------------------------------------------------------

    reaction = model.createReaction()
    reaction.setId("vcat")
    reaction.setReversible(False)

    # ---------------------------------------------------------------------------
    # Creates Reactant objects inside the Reaction object ("vcat"). 
    # ---------------------------------------------------------------------------

    # (Reactant1) Creates a Reactant object that references Species "ES" in the
    # model.

    spr = reaction.createReactant()
    spr.setSpecies("ES")

    # ---------------------------------------------------------------------------
    # Creates a Product object inside the Reaction object ("vcat"). 
    # ---------------------------------------------------------------------------

    # (Product1) Creates a Product object that references Species "E" in the model.

    spr = reaction.createProduct()
    spr.setSpecies("E")

    # (Product2) Creates a Product object that references Species "P" in the model.

    spr = reaction.createProduct()
    spr.setSpecies("P")

    # ---------------------------------------------------------------------------
    # Creates a KineticLaw object inside the Reaction object ("vcat"). 
    # ---------------------------------------------------------------------------

    kl = reaction.createKineticLaw()

    # ---------------------------------------------------------------------------
    # Sets a math (object) to the KineticLaw object.
    # ---------------------------------------------------------------------------

    # To create mathematical expressions, one would typically construct
    # an tree as the above example code which creates a math of another
    # KineticLaw object.  Here, to save some space and illustrate another approach 
    # of doing it, we will write out the formula in MathML form and then use a 
    # libSBML convenience function to create the tree for us.  
    # (This is a bit dangerous; it's very easy to make mistakes when writing MathML 
    # by hand, so in a real program, we would not really want to do it this way.)

    mathXMLString = """<math xmlns="http://www.w3.org/1998/Math/MathML">
      <apply>
        <times/>
        <ci> cytosol </ci>
        <ci> kcat </ci>
        <ci> ES </ci>
      </apply>
    </math>
    """

    astMath = readMathMLFromString(mathXMLString)
    kl.setMath(astMath)

    # ---------------------------------------------------------------------------
    # Creates local Parameter objects inside the KineticLaw object.
    # ---------------------------------------------------------------------------

    # Creates a Parameter ("kcat")

    para = kl.createParameter()
    para.setId("kcat")
    para.setValue(0.1)
    para.setUnits("per_second")

    # Returns the created SBMLDocument object.
    # The returned object must be explicitly deleted by the caller,
    # otherwise a memory leak will happen.

    return sbmlDoc


#
# 
# Creates an SBML model represented in "7.2 Example involving units"
# in the SBML Level 2 Version 4 Specification.
# 
# 

def createExampleInvolvingUnits():
    level = Level
    version = Version

    # ---------------------------------------------------------------------------
    #
    # Creates an SBMLDocument object 
    #
    # ---------------------------------------------------------------------------

    sbmlDoc = SBMLDocument(level, version)

    # Adds the namespace for XHTML to the SBMLDocument object.  We need this
    # because we will add notes to the model.  (By default, the SBML document
    # created by SBMLDocument only declares the SBML XML namespace.)

    sbmlDoc.getNamespaces().add("http://www.w3.org/1999/xhtml", "xhtml")

    # ---------------------------------------------------------------------------
    #
    # Creates a Model object inside the SBMLDocument object. 
    #
    # ---------------------------------------------------------------------------

    model = sbmlDoc.createModel()
    model.setId("unitsExample")

    # ---------------------------------------------------------------------------
    #
    # Creates UnitDefinition objects inside the Model object.
    #
    # ---------------------------------------------------------------------------

    # ---------------------------------------------------------------------------
    # (UnitDefinition1) Creates an UnitDefinition object ("substance").
    #
    # This has the effect of redefining the default unit of subtance for the
    # whole model.
    # ---------------------------------------------------------------------------

    unitdef = model.createUnitDefinition()
    unitdef.setId("substance")

    #  Creates an Unit inside the UnitDefinition object 

    unit = unitdef.createUnit()
    unit.setKind(UNIT_KIND_MOLE)
    unit.setScale(-3)

    # --------------------------------------------------------------------------------
    # (UnitDefinition2) Creates an UnitDefinition object ("mmls") 
    # --------------------------------------------------------------------------------

    # Note that we can reuse the pointers 'unitdef' and 'unit' because the
    # actual UnitDefinition object (along with the Unit objects within it)
    # is already attached to the Model object.

    unitdef = model.createUnitDefinition()
    unitdef.setId("mmls")

    #  Creates an Unit inside the UnitDefinition object ("mmls")

    unit = unitdef.createUnit()
    unit.setKind(UNIT_KIND_MOLE)
    unit.setScale(-3)

    #  Creates an Unit inside the UnitDefinition object ("mmls")

    unit = unitdef.createUnit()
    unit.setKind(UNIT_KIND_LITRE)
    unit.setExponent(-1)

    #  Creates an Unit inside the UnitDefinition object ("mmls")

    unit = unitdef.createUnit()
    unit.setKind(UNIT_KIND_SECOND)
    unit.setExponent(-1)

    # --------------------------------------------------------------------------------
    # (UnitDefinition3) Creates an UnitDefinition object ("mml") 
    # --------------------------------------------------------------------------------

    unitdef = model.createUnitDefinition()
    unitdef.setId("mml")

    #  Creates an Unit inside the UnitDefinition object ("mml")

    unit = unitdef.createUnit()
    unit.setKind(UNIT_KIND_MOLE)
    unit.setScale(-3)

    #  Creates an Unit inside the UnitDefinition object ("mml")

    unit = unitdef.createUnit()
    unit.setKind(UNIT_KIND_LITRE)
    unit.setExponent(-1)

    # ---------------------------------------------------------------------------
    #
    # Creates a Compartment object inside the Model object. 
    #
    # ---------------------------------------------------------------------------

    compName = "cell"

    # Creates a Compartment object ("cell")

    comp = model.createCompartment()
    comp.setId(compName)

    # Sets the "size" attribute of the Compartment object.
    #
    #   The units of this Compartment object is the default SBML 
    #   units of volume (litre), and thus we don't have to explicitly invoke 
    #   setUnits("litre") function to set the default units.
    #
    comp.setSize(1)

    # ---------------------------------------------------------------------------
    #
    # Creates Species objects inside the Model object. 
    #
    # ---------------------------------------------------------------------------


    # ---------------------------------------------------------------------------
    # (Species1) Creates a Species object ("x0")
    # ---------------------------------------------------------------------------

    sp = model.createSpecies()
    sp.setId("x0")

    # Sets the "compartment" attribute of the Species object to identify the 
    # compartnet in which the Species object located.

    sp.setCompartment(compName)

    # Sets the "initialConcentration" attribute of the Species object.
    #
    #  The units of this Species object is determined by two attributes of this 
    #  Species object ("substanceUnits" and "hasOnlySubstanceUnits") and the
    #  "spatialDimensions" attribute of the Compartment object ("cytosol") in which 
    #  this species object is located.
    #  Since the default values are used for "substanceUnits" (substance (mole)) 
    #  and "hasOnlySubstanceUnits" (False) and the value of "spatialDimension" (3) 
    #  is greater than 0, the units of this Species object is  moles/liters . 
    #
    sp.setInitialConcentration(1)

    # ---------------------------------------------------------------------------
    # (Species2) Creates a Species object ("x1")
    # ---------------------------------------------------------------------------

    sp = model.createSpecies()
    sp.setId("x1")
    sp.setCompartment(compName)
    sp.setInitialConcentration(1)

    # ---------------------------------------------------------------------------
    # (Species3) Creates a Species object ("s1")
    # ---------------------------------------------------------------------------

    sp = model.createSpecies()
    sp.setCompartment(compName)
    sp.setId("s1")
    sp.setInitialConcentration(1)

    # ---------------------------------------------------------------------------
    # (Species4) Creates a Species object ("s2")
    # ---------------------------------------------------------------------------

    sp = model.createSpecies()
    sp.setCompartment(compName)
    sp.setId("s2")
    sp.setInitialConcentration(1)

    # ---------------------------------------------------------------------------
    #
    # Creates global Parameter objects inside the Model object. 
    #
    # ---------------------------------------------------------------------------

    # Creates a Parameter ("vm")  

    para = model.createParameter()
    para.setId("vm")
    para.setValue(2)
    para.setUnits("mmls")

    # Creates a Parameter ("km")  

    para = model.createParameter()
    para.setId("km")
    para.setValue(2)
    para.setUnits("mml")

    # ---------------------------------------------------------------------------
    #
    # Creates Reaction objects inside the Model object. 
    #
    # ---------------------------------------------------------------------------

    # ---------------------------------------------------------------------------
    # (Reaction1) Creates a Reaction object ("v1").
    # ---------------------------------------------------------------------------

    reaction = model.createReaction()
    reaction.setId("v1")

    # ---------------------------------------------------------------------------
    # Creates Reactant objects inside the Reaction object ("v1"). 
    # ---------------------------------------------------------------------------

    # (Reactant1) Creates a Reactant object that references Species "x0"
    # in the model.

    spr = reaction.createReactant()
    spr.setSpecies("x0")

    # ---------------------------------------------------------------------------
    # Creates a Product object inside the Reaction object ("v1"). 
    # ---------------------------------------------------------------------------

    # Creates a Product object that references Species "s1" in the model. 

    spr = reaction.createProduct()
    spr.setSpecies("s1")

    # ---------------------------------------------------------------------------
    # Creates a KineticLaw object inside the Reaction object ("v1"). 
    # ---------------------------------------------------------------------------

    kl = reaction.createKineticLaw()

    # Creates a <notes> element in the KineticLaw object.
    # Here we illustrate how to do it using a literal string.  This requires
    # known the required syntax of XHTML and the requirements for SBML <notes>
    # elements.  Later below, we show how to create notes using objects instead
    # of strings.

    notesString = "<xhtml:p> ((vm * s1)/(km + s1)) * cell </xhtml:p>"
    kl.setNotes(notesString)

    # ---------------------------------------------------------------------------
    # Creates an object which represents the following KineticLaw object.
    #
    #  <math xmlns="http://www.w3.org/1998/Math/MathML">
    #   <apply>
    #     <times/>
    #     <apply>
    #       <divide/>
    #       <apply>
    #         <times/>
    #           <ci> vm </ci>
    #           <ci> s1 </ci>
    #       </apply>
    #       <apply>
    #         <plus/>
    #           <ci> km </ci>
    #           <ci> s1 </ci>
    #       </apply>
    #     </apply>
    #     <ci> cell </ci>
    #    </apply>
    #  </math>
    # ---------------------------------------------------------------------------

    #
    # In the following code, objects, which construct an tree 
    # of the above math, are created and added in the order of preorder traversal 
    # of the tree (i.e. the order corresponds to the nested structure of the above 
    # MathML elements), and thus the following code maybe a bit more efficient but 
    # maybe a bit difficult to read.
    #

    astMath = ASTNode(AST_TIMES)

    astMath.addChild(ASTNode(AST_DIVIDE))
    astDivide = astMath.getLeftChild()

    astDivide.addChild(ASTNode(AST_TIMES))
    astTimes = astDivide.getLeftChild()

    astTimes.addChild(ASTNode(AST_NAME))
    astTimes.getLeftChild().setName("vm")

    astTimes.addChild(ASTNode(AST_NAME))
    astTimes.getRightChild().setName("s1")

    astDivide.addChild(ASTNode(AST_PLUS))
    astPlus = astDivide.getRightChild()

    astPlus.addChild(ASTNode(AST_NAME))
    astPlus.getLeftChild().setName("km")

    astPlus.addChild(ASTNode(AST_NAME))
    astPlus.getRightChild().setName("s1")

    astMath.addChild(ASTNode(AST_NAME))
    astMath.getRightChild().setName("cell")

    # ---------------------------------------------
    #
    # set the Math element
    #
    # ------------------------------------------------

    kl.setMath(astMath)

    # ---------------------------------------------------------------------------
    # (Reaction2) Creates a Reaction object ("v2").
    # ---------------------------------------------------------------------------

    reaction = model.createReaction()
    reaction.setId("v2")

    # ---------------------------------------------------------------------------
    # Creates Reactant objects inside the Reaction object ("v2"). 
    # ---------------------------------------------------------------------------

    # (Reactant2) Creates a Reactant object that references Species "s1"
    # in the model.

    spr = reaction.createReactant()
    spr.setSpecies("s1")

    # ---------------------------------------------------------------------------
    # Creates a Product object inside the Reaction object ("v2"). 
    # ---------------------------------------------------------------------------

    # Creates a Product object that references Species "s2" in the model. 

    spr = reaction.createProduct()
    spr.setSpecies("s2")

    # ---------------------------------------------------------------------------
    # Creates a KineticLaw object inside the Reaction object ("v2"). 
    # ---------------------------------------------------------------------------

    kl = reaction.createKineticLaw()

    # Sets a notes (by XMLNode) to the KineticLaw object.
    #
    # The following code is an alternative to using setNotes(const string&).
    # The equivalent code would be like this:
    #   
    #     notesString = "<xhtml:p>((vm * s2)/(km + s2))*cell</xhtml:p>";
    #     kl.setNotes(notesString);

    # Creates an of start element (<xhtml:p>) without attributes.

    notesXMLNode = XMLNode(
        XMLTriple("p", "", "xhtml"),
        XMLAttributes())

    # Adds a text element to the start element.

    notesXMLNode.addChild(XMLNode(" ((vm * s2)/(km + s2)) * cell "))

    # Adds it to the kineticLaw object.

    kl.setNotes(notesXMLNode)

    # ---------------------------------------------------------------------------
    # Sets a math (object) to the KineticLaw object.
    # ---------------------------------------------------------------------------

    # To create mathematical expressions, one would typically construct
    # an tree as the above example code which creates a math of another
    # KineticLaw object.  Here, to save some space and illustrate another approach 
    # of doing it, we will write out the formula in MathML form and then use a 
    # libSBML convenience function to create the tree for us.  
    # (This is a bit dangerous; it's very easy to make mistakes when writing MathML 
    # by hand, so in a real program, we would not really want to do it this way.)

    mathXMLString = """<math xmlns="http://www.w3.org/1998/Math/MathML">
    <apply>
      <times/>
      <apply>
        <divide/>
        <apply>
          <times/>
          <ci> vm </ci>
          <ci> s2 </ci>
        </apply>
        <apply>
          <plus/>
            <ci> km </ci>
            <ci> s2 </ci>
        </apply>
      </apply>
      <ci> cell </ci>
    </apply>
    </math>
	"""

    astMath = readMathMLFromString(mathXMLString)
    kl.setMath(astMath)

    # ---------------------------------------------------------------------------
    # (Reaction3) Creates a Reaction object ("v3").
    # ---------------------------------------------------------------------------

    reaction = model.createReaction()
    reaction.setId("v3")

    # ---------------------------------------------------------------------------
    # Creates Reactant objects inside the Reaction object ("v3"). 
    # ---------------------------------------------------------------------------

    # (Reactant2) Creates a Reactant object that references Species "s2"
    # in the model.

    spr = reaction.createReactant()
    spr.setSpecies("s2")

    # ---------------------------------------------------------------------------
    # Creates a Product object inside the Reaction object ("v3"). 
    # ---------------------------------------------------------------------------

    # Creates a Product object that references Species "x1" in the model. 

    spr = reaction.createProduct()
    spr.setSpecies("x1")

    # ---------------------------------------------------------------------------
    # Creates a KineticLaw object inside the Reaction object ("v3"). 
    # ---------------------------------------------------------------------------

    kl = reaction.createKineticLaw()

    # Sets a notes (by string) to the KineticLaw object.

    notesString = "<xhtml:p> ((vm * x1)/(km + x1)) * cell </xhtml:p>"
    kl.setNotes(notesString)

    # ---------------------------------------------------------------------------
    # Sets a math (object) to the KineticLaw object.
    # ---------------------------------------------------------------------------

    mathXMLString = """<math xmlns="http://www.w3.org/1998/Math/MathML">
      <apply>
        <times/>
        <apply>
          <divide/>
          <apply>
            <times/>
            <ci> vm </ci>
            <ci> x1 </ci>
          </apply>
          <apply>
            <plus/>
              <ci> km </ci>
              <ci> x1 </ci>
          </apply>
        </apply>
        <ci> cell </ci>
      </apply>
    </math>
	"""

    astMath = readMathMLFromString(mathXMLString)
    kl.setMath(astMath)

    # Returns the created SBMLDocument object.
    # The returned object must be explicitly deleted by the caller,
    # otherwise memory leak will happen.

    return sbmlDoc


#
# 
# Creates an SBML model represented in "7.8 Example involving function definitions"
# in the SBML Level 2 Version 4 Specification.
# 
# 

def createExampleInvolvingFunctionDefinitions():
    level = Level
    version = Version

    # ---------------------------------------------------------------------------
    #
    # Creates an SBMLDocument object 
    #
    # ---------------------------------------------------------------------------

    sbmlDoc = SBMLDocument(level, version)

    # ---------------------------------------------------------------------------
    #
    # Creates a Model object inside the SBMLDocument object. 
    #
    # ---------------------------------------------------------------------------

    model = sbmlDoc.createModel()
    model.setId("functionExample")

    # ---------------------------------------------------------------------------
    #
    # Creates a FunctionDefinition object inside the Model object. 
    #
    # ---------------------------------------------------------------------------

    fdef = model.createFunctionDefinition()
    fdef.setId("f")

    # Sets a math (object) to the FunctionDefinition object.

    mathXMLString = """<math xmlns="http://www.w3.org/1998/Math/MathML">
      <lambda>
        <bvar>
          <ci> x </ci>
        </bvar>
        <apply>
          <times/>
          <ci> x </ci>
          <cn> 2 </cn>
        </apply>
      </lambda>
    </math>
	"""

    astMath = readMathMLFromString(mathXMLString)
    fdef.setMath(astMath)

    # ---------------------------------------------------------------------------
    #
    # Creates a Compartment object inside the Model object. 
    #
    # ---------------------------------------------------------------------------

    compName = "compartmentOne"

    # Creates a Compartment object ("compartmentOne")

    comp = model.createCompartment()
    comp.setId(compName)

    # Sets the "size" attribute of the Compartment object.
    #
    #   The units of this Compartment object is the default SBML 
    #   units of volume (litre), and thus we don't have to explicitly invoke 
    #   setUnits("litre") function to set the default units.
    #
    comp.setSize(1)

    # ---------------------------------------------------------------------------
    #
    # Creates Species objects inside the Model object. 
    #
    # ---------------------------------------------------------------------------

    # ---------------------------------------------------------------------------
    # (Species1) Creates a Species object ("S1")
    # ---------------------------------------------------------------------------

    sp = model.createSpecies()
    sp.setId("S1")

    # Sets the "compartment" attribute of the Species object to identify the 
    # compartnet in which the Species object located.

    sp.setCompartment(compName)

    # Sets the "initialConcentration" attribute of the Species object.
    #
    #  The units of this Species object is determined by two attributes of this 
    #  Species object ("substanceUnits" and "hasOnlySubstanceUnits") and the
    #  "spatialDimension" attribute of the Compartment object ("cytosol") in which 
    #  this species object located.
    #  Since the default values are used for "substanceUnits" (substance (mole)) 
    #  and "hasOnlySubstanceUnits" (False) and the value of "spatialDimension" (3) 
    #  is greater than 0, the units of this Species object is  mole/litre . 
    #

    sp.setInitialConcentration(1)

    # ---------------------------------------------------------------------------
    # (Species2) Creates a Species object ("S2")
    # ---------------------------------------------------------------------------

    sp = model.createSpecies()
    sp.setId("S2")
    sp.setCompartment(compName)
    sp.setInitialConcentration(0)

    # ---------------------------------------------------------------------------
    #
    # Creates a global Parameter object inside the Model object. 
    #
    # ---------------------------------------------------------------------------

    # Creates a Parameter ("t")  

    para = model.createParameter()
    para.setId("t")
    para.setValue(1)
    para.setUnits("second")

    # ---------------------------------------------------------------------------
    #
    # Creates Reaction objects inside the Model object. 
    #
    # ---------------------------------------------------------------------------

    # ---------------------------------------------------------------------------
    # (Reaction1) Creates a Reaction object ("reaction_1").
    # ---------------------------------------------------------------------------

    reaction = model.createReaction()
    reaction.setId("reaction_1")
    reaction.setReversible(False)

    # ---------------------------------------------------------------------------
    # Creates Reactant objects inside the Reaction object ("reaction_1"). 
    # ---------------------------------------------------------------------------

    # (Reactant1) Creates a Reactant object that references Species "S1"
    # in the model.

    spr = reaction.createReactant()
    spr.setSpecies("S1")

    # ---------------------------------------------------------------------------
    # Creates a Product object inside the Reaction object ("reaction_1"). 
    # ---------------------------------------------------------------------------

    # Creates a Product object that references Species "S2" in the model. 

    spr = reaction.createProduct()
    spr.setSpecies("S2")

    # ---------------------------------------------------------------------------
    # Creates a KineticLaw object inside the Reaction object ("reaction_1"). 
    # ---------------------------------------------------------------------------

    kl = reaction.createKineticLaw()

    # ---------------------------------------------------------------------------
    # Sets a math (object) to the KineticLaw object.
    # ---------------------------------------------------------------------------

    mathXMLString = """<math xmlns="http://www.w3.org/1998/Math/MathML">
      <apply>
      <divide/>
        <apply>
          <times/>
          <apply>
            <ci> f </ci>
            <ci> S1 </ci>
          </apply>
          <ci> compartmentOne </ci>
        </apply>
        <ci> t </ci>
      </apply>
    </math>
	"""

    astMath = readMathMLFromString(mathXMLString)
    kl.setMath(astMath)

    # Returns the created SBMLDocument object.
    # The returned object must be explicitly deleted by the caller,
    # otherwise memory leak will happen.

    return sbmlDoc


# ===============================================================================
#
#
# Helper functions for writing/validating the given SBML documents.
# 
#
# ===============================================================================

#
# 
# Validates the given SBMLDocument.
#
#  This function is based on validateSBML.py implemented by
#  Sarah Keating, Ben Bornstein, and Michael Hucka.
#
#

def validateExampleSBML(sbmlDoc):
    if sbmlDoc is None:
        print("validateExampleSBML: given a None SBML Document")
        return False

    consistencyMessages = ""
    validationMessages = ""
    noProblems = True
    numCheckFailures = 0
    numConsistencyErrors = 0
    numConsistencyWarnings = 0
    numValidationErrors = 0
    numValidationWarnings = 0

    # LibSBML 3.3 is lenient when generating models from scratch using the
    # API for creating objects.  Once the whole model is done and before it
    # gets written out, it's important to check that the whole model is in
    # fact complete, consistent and valid.

    numCheckFailures = sbmlDoc.checkInternalConsistency()
    if numCheckFailures > 0:
        noProblems = False
        for i in range(0, numCheckFailures):
            sbmlErr = sbmlDoc.getError(i)
            if sbmlErr.isFatal() or sbmlErr.isError():
                numConsistencyErrors += 1
            else:
                numConsistencyWarnings += 1
        sbmlDoc.printErrors()

    # If the internal checks fail, it makes little sense to attempt
    # further validation, because the model may be too compromised to
    # be properly interpreted.

    if numConsistencyErrors > 0:
        consistencyMessages += "Further validation aborted."
    else:
        numCheckFailures = sbmlDoc.checkConsistency()
        if numCheckFailures > 0:
            noProblems = False
            for i in range(0, numCheckFailures):
                sbmlErr = sbmlDoc.getError(i)
                if sbmlErr.isFatal() or sbmlErr.isError():
                    numValidationErrors += 1
                else:
                    numValidationWarnings += 1
            sbmlDoc.printErrors()

    if noProblems:
        return True
    else:
        if numConsistencyErrors > 0:
            tmp = ""
            if numConsistencyErrors > 1:
                tmp = "s"
            print("ERROR: encountered " + str(
                numConsistencyErrors) + " consistency error" + tmp + " in model '" + sbmlDoc.getModel().getId() + "'.")

        if numConsistencyWarnings > 0:
            tmp = ""
            if numConsistencyWarnings > 1:
                tmp = "s"
            print("Notice: encountered " + str(numConsistencyWarnings)
                  + " consistency warning" + tmp
                  + " in model '" + sbmlDoc.getModel().getId() + "'.")
        print(consistencyMessages)

        if numValidationErrors > 0:
            tmp = ""
            if numValidationErrors > 1:
                tmp = "s"
            print("ERROR: encountered " + str(numValidationErrors) + " validation error" + tmp
                  + " in model '" + sbmlDoc.getModel().getId() + "'.")
        if numValidationWarnings > 0:
            tmp = ""
            if numValidationWarnings > 1:
                tmp = "s"
            print("Notice: encountered " + str(numValidationWarnings)
                  + " validation warning" + tmp
                  + " in model '" + sbmlDoc.getModel().getId() + "'.")
        print(validationMessages)

        return numConsistencyErrors == 0 and numValidationErrors == 0


# 
# 
# Writes the given SBMLDocument to the given file.
# 
# 
def writeExampleSBML(sbmlDoc, filename):
    result = writeSBML(sbmlDoc, filename)
    if result == 1:
        print("Wrote file '" + filename + "'")
        return True
    else:
        print("Failed to write '" + filename + "'")
        return False


# ===============================================================================
#
# Main routine
#
#  Creates SBML models represented in "Example models expressed in XML using
#  SBML" in Section 7 of the SBML Level 2 Version 4 specification(*). 
#
#   (*) The specification document is available at the following URL:
#       http://sbml.org/Documents/Specifications
#
# ===============================================================================
#
def main(args):
    sbmlDoc = None
    SBMLok = False

    try:
        # -------------------------------------------------
        # 7.1 A Simple example application of SBML
        # -------------------------------------------------

        sbmlDoc = createExampleEnzymaticReaction()
        SBMLok = validateExampleSBML(sbmlDoc)
        if SBMLok:
            writeExampleSBML(sbmlDoc, "enzymaticreaction.xml")
        if not SBMLok:
            return 1

        # -------------------------------------------------
        # 7.2 Example involving units
        # -------------------------------------------------

        sbmlDoc = createExampleInvolvingUnits()
        SBMLok = validateExampleSBML(sbmlDoc)
        if SBMLok:
            writeExampleSBML(sbmlDoc, "units.xml")
        if not SBMLok:
            return 1

        # -------------------------------------------------
        # 7.8 Example involving function definitions
        # -------------------------------------------------

        sbmlDoc = createExampleInvolvingFunctionDefinitions()
        SBMLok = validateExampleSBML(sbmlDoc)
        if SBMLok:
            writeExampleSBML(sbmlDoc, "functiondef.xml")
        if not SBMLok:
            return 1
    except:
        print("Unexpected exceptional condition encountered.")
        return 1

    # A 0 return status is the standard Unix/Linux way to say "all ok".
    return 0


if __name__ == '__main__':
    main(sys.argv)
