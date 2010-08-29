/**
 * @file    Rule.h
 * @brief   Definitions of Rule, ListOfRules, AlgebraicRule, AssignmentRule
 *          and RateRule.
 * @author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2010 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class Rule
 * @brief LibSBML implementation of %SBML's %Rule construct.
 *
 * In SBML, @em rules provide additional ways to define the values of
 * variables in a model, their relationships, and the dynamical behaviors
 * of those variables.  They enable encoding relationships that cannot be
 * expressed using Reaction nor InitialAssignment objects alone.
 *
 * The libSBML implementation of rules mirrors the SBML Level&nbsp;3
 * Version&nbsp;1 Core definition (which is in turn is very similar to the
 * Level&nbsp;2 Version&nbsp;4 definition), with Rule being the parent
 * class of three subclasses as explained below.  The Rule class itself
 * cannot be instantiated by user programs and has no constructor; only the
 * subclasses AssignmentRule, AlgebraicRule and RateRule can be
 * instantiated directly.
 *
 * @section general General summary of SBML rules
 *
 * In SBML Level&nbsp;3 as well as Level&nbsp;2, rules are separated into
 * three subclasses for the benefit of model analysis software.  The three
 * subclasses are based on the following three different possible
 * functional forms (where <em>x</em> is a variable, <em>f</em> is some
 * arbitrary function returning a numerical result, <b><em>V</em></b> is a
 * vector of variables that does not include <em>x</em>, and
 * <b><em>W</em></b> is a vector of variables that may include <em>x</em>):
 * 
 * <center>
 * <table border="0" cellpadding="0" style="font-size: small">
 * <tr><td width="120px"><em>Algebraic:</em></td><td width="250px">left-hand side is zero</td><td><em>0 = f(<b>W</b>)</em></td></tr>
 * <tr><td><em>Assignment:</em></td><td>left-hand side is a scalar:</td><td><em>x = f(<b>V</b>)</em></td></tr>
 * <tr><td><em>Rate:</em></td><td>left-hand side is a rate-of-change:</td><td><em>dx/dt = f(<b>W</b>)</em></td></tr>
 * </table>
 * </center>
 * 
 * In their general form given above, there is little to distinguish
 * between <em>assignment</em> and <em>algebraic</em> rules.  They are treated as
 * separate cases for the following reasons:
 *
 * <ul>
 * <li> <em>Assignment</em> rules can simply be evaluated to calculate
 * intermediate values for use in numerical methods.  They are statements
 * of equality that hold at all times.  (For assignments that are only
 * performed once, see InitialAssignment.)<p>
 * 
 * <li> SBML needs to place restrictions on assignment rules, for example
 * the restriction that assignment rules cannot contain algebraic loops.<p>
 * 
 * <li> Some simulators do not contain numerical solvers capable of solving
 * unconstrained algebraic equations, and providing more direct forms such
 * as assignment rules may enable those simulators to process models they
 * could not process if the same assignments were put in the form of
 * general algebraic equations;<p>
 * 
 * <li> Those simulators that <em>can</em> solve these algebraic equations make a
 * distinction between the different categories listed above; and<p>
 * 
 * <li> Some specialized numerical analyses of models may only be applicable
 * to models that do not contain <em>algebraic</em> rules.
 * </ul>
 * 
 * The approach taken to covering these cases in SBML is to define an
 * abstract Rule structure containing a subelement, "math", to hold the
 * right-hand side expression, then to derive subtypes of Rule that add
 * attributes to distinguish the cases of algebraic, assignment and rate
 * rules.  The "math" subelement must contain a MathML expression defining the
 * mathematical formula of the rule.  This MathML formula must return a
 * numerical value.  The formula can be an arbitrary expression referencing
 * the variables and other entities in an SBML model.
 * 
 * Each of the three subclasses of Rule (AssignmentRule, AlgebraicRule,
 * RateRule) inherit the the "math" subelement and other fields from SBase.
 * The AssignmentRule and RateRule classes add an additional attribute,
 * "variable".  See the definitions of AssignmentRule, AlgebraicRule and
 * RateRule for details about the structure and interpretation of each one.
 * 
 * @section additional-restrictions Additional restrictions on SBML rules
 * 
 * An important design goal of SBML rule semantics is to ensure that a
 * model's simulation and analysis results will not be dependent on when or
 * how often rules are evaluated.  To achieve this, SBML needs to place two
 * restrictions on rule use.  The first concerns algebraic loops in the system
 * of assignments in a model, and the second concerns overdetermined systems.
 * 
 * @subsection no-algebraic-loops A model must not contain algebraic loops
 * 
 * The combined set of InitialAssignment, AssignmentRule and KineticLaw
 * objects in a model constitute a set of assignment statements that should be
 * considered as a whole.  (A KineticLaw object is counted as an assignment
 * because it assigns a value to the symbol contained in the "id" attribute of
 * the Reaction object in which it is defined.)  This combined set of
 * assignment statements must not contain algebraic loops&mdash;dependency
 * chains between these statements must terminate.  To put this more formally,
 * consider a directed graph in which nodes are assignment statements and
 * directed arcs exist for each occurrence of an SBML species, compartment or
 * parameter symbol in an assignment statement's "math" subelement.  Let the
 * directed arcs point from the statement assigning the symbol to the
 * statements that contain the symbol in their "math" subelement expressions.
 * This graph must be acyclic.
 * 
 * SBML does not specify when or how often rules should be evaluated.
 * Eliminating algebraic loops ensures that assignment statements can be
 * evaluated any number of times without the result of those evaluations
 * changing.  As an example, consider the set of equations <em>x = x + 1</em>,
 * <em>y = z + 200</em> and <em>z = y + 100</em>.  If this set of equations
 * were interpreted as a set of assignment statements, it would be invalid
 * because the rule for <em>x</em> refers to <em>x</em> (exhibiting one type
 * of loop), and the rule for <em>y</em> refers to <em>z</em> while the rule
 * for <em>z</em> refers back to <em>y</em> (exhibiting another type of loop).
 * Conversely, the following set of equations would constitute a valid set of
 * assignment statements: <em>x = 10</em>, <em>y = z + 200</em>, and <em>z = x
 * + 100</em>.
 * 
 * @subsection no-overdetermined A model must not be overdetermined
 * 
 * An SBML model must not be overdetermined; that is, a model must not
 * define more equations than there are unknowns in a model.  An SBML model
 * that does not contain AlgebraicRule structures cannot be overdetermined.
 * 
 * LibSBML implements the static analysis procedure described in Appendix
 * B of the SBML Level&nbsp;3 Version&nbsp;1 Core specification for assessing
 * whether a model is overdetermined.
 * 
 * (In summary, assessing whether a given continuous, deterministic,
 * mathematical model is overdetermined does not require dynamic analysis; it
 * can be done by analyzing the system of equations created from the model.
 * One approach is to construct a bipartite graph in which one set of vertices
 * represents the variables and the other the set of vertices represents the
 * equations.  Place edges between vertices such that variables in the system
 * are linked to the equations that determine them.  For algebraic equations,
 * there will be edges between the equation and each variable occurring in the
 * equation.  For ordinary differential equations (such as those defined by
 * rate rules or implied by the reaction rate definitions), there will be a
 * single edge between the equation and the variable determined by that
 * differential equation.  A mathematical model is overdetermined if the
 * maximal matchings of the bipartite graph contain disconnected vertexes
 * representing equations.  If one maximal matching has this property, then
 * all the maximal matchings will have this property; i.e., it is only
 * necessary to find one maximal matching.)
 *
 * @section RuleType_t Rule types for SBML Level 1
 *
 * SBML Level 1 uses a different scheme than SBML Level&nbsp;2 and
 * Level&nbsp;3 for distinguishing rules; specifically, it uses an
 * attribute whose value is drawn from an enumeration of 3 values.  LibSBML
 * supports this using methods that work @if clike a libSBML enumeration
 * type, #RuleType_t, whose values are@endif@if java  with the enumeration
 * values@endif listed in the following table.  
 * 
 * <p>
 * <center>
 * <table width="90%" cellspacing="1" cellpadding="1" border="0" class="normal-font">
 *  <tr style="background: lightgray" class="normal-font">
 *      <td><strong>Enumerator</strong></td>
 *      <td><strong>Meaning</strong></td>
 *  </tr>
 * <tr><td><code>@link RuleType_t#RULE_TYPE_RATE RULE_TYPE_RATE@endlink</code></td><td>Indicates the rule is a "rate" rule.</td>
 * <tr><td><code>@link RuleType_t#RULE_TYPE_SCALAR RULE_TYPE_SCALAR@endlink</code></td><td>Indicates the rule is a "scalar" rule.</td>
 * <tr><td><code>@link RuleType_t#RULE_TYPE_INVALID RULE_TYPE_INVALID@endlink</code></td><td>Indicates the rule type is unknown or not
 * yet set.</td>
 * </table>
 * </center>
 *
 *
 * <!-- leave this next break as-is to work around some doxygen bug -->
 */ 
/**
 * @class AlgebraicRule
 * @brief LibSBML implementation of %SBML's %AlgebraicRule construct.
 *
 * The rule type AlgebraicRule is derived from the parent class Rule.  It
 * is used to express equations that are neither assignments of model
 * variables nor rates of change.  AlgebraicRule does not add any
 * attributes to the basic Rule; its role is simply to distinguish this
 * case from the other cases.
 *
 * In the context of a simulation, algebraic rules are in effect at all
 * times, <em>t</em> \f$\geq\f$ <em>0</em>.  For purposes of evaluating
 * expressions that involve the delay "csymbol" (see the SBML
 * specification), algebraic rules are considered to apply also at
 * <em>t</em> \f$\leq\f$ <em>0</em>.  Please consult the relevant SBML
 * specification for additional information about the semantics of
 * assignments, rules, and entity values for simulation time <em>t</em>
 * \f$\leq\f$ <em>0</em>.
 *
 * The ability to define arbitrary algebraic expressions in an SBML model
 * introduces the possibility that a model is mathematically overdetermined
 * by the overall system of equations constructed from its rules and
 * reactions.  An SBML model must not be overdetermined.  An SBML model
 * that does not contain AlgebraicRule structures cannot be overdetermined.
 *
 * Assessing whether a given continuous, deterministic, mathematical model
 * is overdetermined does not require dynamic analysis; it can be done by
 * analyzing the system of equations created from the model.  One approach
 * is to construct a bipartite graph in which one set of vertices
 * represents the variables and the other the set of vertices represents
 * the equations.  Place edges between vertices such that variables in the
 * system are linked to the equations that determine them.  For algebraic
 * equations, there will be edges between the equation and each variable
 * occurring in the equation.  For ordinary differential equations (such as
 * those defined by rate rules or implied by the reaction rate
 * definitions), there will be a single edge between the equation and the
 * variable determined by that differential equation.  A mathematical model
 * is overdetermined if the maximal matchings of the bipartite graph
 * contain disconnected vertexes representing equations.  (If one maximal
 * matching has this property, then all the maximal matchings will have
 * this property; i.e., it is only necessary to find one maximal matching.)
 * Appendix&nbsp;B of the SBML Level&nbsp;3 Version&nbsp;1 Core
 * specification document describes a method of applying this procedure to
 * specific SBML data objects.
 * 
 *
 * @section general General summary of SBML rules
 *
 * In SBML Level&nbsp;3 as well as Level&nbsp;2, rules are separated into
 * three subclasses for the benefit of model analysis software.  The three
 * subclasses are based on the following three different possible
 * functional forms (where <em>x</em> is a variable, <em>f</em> is some
 * arbitrary function returning a numerical result, <b><em>V</em></b> is a
 * vector of variables that does not include <em>x</em>, and
 * <b><em>W</em></b> is a vector of variables that may include <em>x</em>):
 * 
 * <center>
 * <table border="0" cellpadding="0" style="font-size: small">
 * <tr><td width="120px"><em>Algebraic:</em></td><td width="250px">left-hand side is zero</td><td><em>0 = f(<b>W</b>)</em></td></tr>
 * <tr><td><em>Assignment:</em></td><td>left-hand side is a scalar:</td><td><em>x = f(<b>V</b>)</em></td></tr>
 * <tr><td><em>Rate:</em></td><td>left-hand side is a rate-of-change:</td><td><em>dx/dt = f(<b>W</b>)</em></td></tr>
 * </table>
 * </center>
 * 
 * In their general form given above, there is little to distinguish
 * between <em>assignment</em> and <em>algebraic</em> rules.  They are treated as
 * separate cases for the following reasons:
 *
 * <ul>
 * <li> <em>Assignment</em> rules can simply be evaluated to calculate
 * intermediate values for use in numerical methods.  They are statements
 * of equality that hold at all times.  (For assignments that are only
 * performed once, see InitialAssignment.)<p>
 * 
 * <li> SBML needs to place restrictions on assignment rules, for example
 * the restriction that assignment rules cannot contain algebraic loops.<p>
 * 
 * <li> Some simulators do not contain numerical solvers capable of solving
 * unconstrained algebraic equations, and providing more direct forms such
 * as assignment rules may enable those simulators to process models they
 * could not process if the same assignments were put in the form of
 * general algebraic equations;<p>
 * 
 * <li> Those simulators that <em>can</em> solve these algebraic equations make a
 * distinction between the different categories listed above; and<p>
 * 
 * <li> Some specialized numerical analyses of models may only be applicable
 * to models that do not contain <em>algebraic</em> rules.
 * </ul>
 * 
 * The approach taken to covering these cases in SBML is to define an
 * abstract Rule structure containing a subelement, "math", to hold the
 * right-hand side expression, then to derive subtypes of Rule that add
 * attributes to distinguish the cases of algebraic, assignment and rate
 * rules.  The "math" subelement must contain a MathML expression defining the
 * mathematical formula of the rule.  This MathML formula must return a
 * numerical value.  The formula can be an arbitrary expression referencing
 * the variables and other entities in an SBML model.
 * 
 * Each of the three subclasses of Rule (AssignmentRule, AlgebraicRule,
 * RateRule) inherit the the "math" subelement and other fields from SBase.
 * The AssignmentRule and RateRule classes add an additional attribute,
 * "variable".  See the definitions of AssignmentRule, AlgebraicRule and
 * RateRule for details about the structure and interpretation of each one.
 * 
 * @section additional-restrictions Additional restrictions on SBML rules
 * 
 * An important design goal of SBML rule semantics is to ensure that a
 * model's simulation and analysis results will not be dependent on when or
 * how often rules are evaluated.  To achieve this, SBML needs to place two
 * restrictions on rule use.  The first concerns algebraic loops in the system
 * of assignments in a model, and the second concerns overdetermined systems.
 * 
 * @subsection no-algebraic-loops A model must not contain algebraic loops
 * 
 * The combined set of InitialAssignment, AssignmentRule and KineticLaw
 * objects in a model constitute a set of assignment statements that should be
 * considered as a whole.  (A KineticLaw object is counted as an assignment
 * because it assigns a value to the symbol contained in the "id" attribute of
 * the Reaction object in which it is defined.)  This combined set of
 * assignment statements must not contain algebraic loops&mdash;dependency
 * chains between these statements must terminate.  To put this more formally,
 * consider a directed graph in which nodes are assignment statements and
 * directed arcs exist for each occurrence of an SBML species, compartment or
 * parameter symbol in an assignment statement's "math" subelement.  Let the
 * directed arcs point from the statement assigning the symbol to the
 * statements that contain the symbol in their "math" subelement expressions.
 * This graph must be acyclic.
 * 
 * SBML does not specify when or how often rules should be evaluated.
 * Eliminating algebraic loops ensures that assignment statements can be
 * evaluated any number of times without the result of those evaluations
 * changing.  As an example, consider the set of equations <em>x = x + 1</em>,
 * <em>y = z + 200</em> and <em>z = y + 100</em>.  If this set of equations
 * were interpreted as a set of assignment statements, it would be invalid
 * because the rule for <em>x</em> refers to <em>x</em> (exhibiting one type
 * of loop), and the rule for <em>y</em> refers to <em>z</em> while the rule
 * for <em>z</em> refers back to <em>y</em> (exhibiting another type of loop).
 * Conversely, the following set of equations would constitute a valid set of
 * assignment statements: <em>x = 10</em>, <em>y = z + 200</em>, and <em>z = x
 * + 100</em>.
 * 
 * @subsection no-overdetermined A model must not be overdetermined
 * 
 * An SBML model must not be overdetermined; that is, a model must not
 * define more equations than there are unknowns in a model.  An SBML model
 * that does not contain AlgebraicRule structures cannot be overdetermined.
 * 
 * LibSBML implements the static analysis procedure described in Appendix
 * B of the SBML Level&nbsp;3 Version&nbsp;1 Core specification for assessing
 * whether a model is overdetermined.
 * 
 * (In summary, assessing whether a given continuous, deterministic,
 * mathematical model is overdetermined does not require dynamic analysis; it
 * can be done by analyzing the system of equations created from the model.
 * One approach is to construct a bipartite graph in which one set of vertices
 * represents the variables and the other the set of vertices represents the
 * equations.  Place edges between vertices such that variables in the system
 * are linked to the equations that determine them.  For algebraic equations,
 * there will be edges between the equation and each variable occurring in the
 * equation.  For ordinary differential equations (such as those defined by
 * rate rules or implied by the reaction rate definitions), there will be a
 * single edge between the equation and the variable determined by that
 * differential equation.  A mathematical model is overdetermined if the
 * maximal matchings of the bipartite graph contain disconnected vertexes
 * representing equations.  If one maximal matching has this property, then
 * all the maximal matchings will have this property; i.e., it is only
 * necessary to find one maximal matching.)
 *
 *
 * @section RuleType_t Rule types for SBML Level 1
 *
 * SBML Level 1 uses a different scheme than SBML Level&nbsp;2 and
 * Level&nbsp;3 for distinguishing rules; specifically, it uses an
 * attribute whose value is drawn from an enumeration of 3 values.  LibSBML
 * supports this using methods that work @if clike a libSBML enumeration
 * type, #RuleType_t, whose values are@endif@if java  with the enumeration
 * values@endif listed in the following table.  
 * 
 * <p>
 * <center>
 * <table width="90%" cellspacing="1" cellpadding="1" border="0" class="normal-font">
 *  <tr style="background: lightgray" class="normal-font">
 *      <td><strong>Enumerator</strong></td>
 *      <td><strong>Meaning</strong></td>
 *  </tr>
 * <tr><td><code>@link RuleType_t#RULE_TYPE_RATE RULE_TYPE_RATE@endlink</code></td><td>Indicates the rule is a "rate" rule.</td>
 * <tr><td><code>@link RuleType_t#RULE_TYPE_SCALAR RULE_TYPE_SCALAR@endlink</code></td><td>Indicates the rule is a "scalar" rule.</td>
 * <tr><td><code>@link RuleType_t#RULE_TYPE_INVALID RULE_TYPE_INVALID@endlink</code></td><td>Indicates the rule type is unknown or not
 * yet set.</td>
 * </table>
 * </center>
 *
 * <!-- leave this next break as-is to work around some doxygen bug -->
 */ 
/**
 * @class AssignmentRule
 * @brief LibSBML implementation of %SBML's %AssignmentRule construct.
 *
 * The rule type AssignmentRule is derived from the parent class Rule.  It
 * is used to express equations that set the values of variables.  The
 * left-hand side (the attribute named "variable") of an assignment rule
 * can refer to the identifier of a Species, SpeciesReference (in SBML
 * Level&nbsp;3), Compartment, or Parameter object in the model (but not a
 * Reaction).  The entity identified must have its "constant" attribute set
 * to @c false.  The effects of an AssignmentRule are in general terms the
 * same, but differ in the precise details depending on the type of
 * variable being set: <ul>

 * <li> <em>In the case of a species</em>, an AssignmentRule sets the
 * referenced species' quantity (whether a "concentration" or "amount") to
 * the value determined by the formula in the MathML subelement "math".
 * The unit associated with the value produced by the "math" formula @em
 * should (in SBML Level&nbsp;2 Version&nbsp;4 and later) or @em must (in
 * SBML releases prior to Level&nbsp;2 version&nbsp;4) be equal to the unit
 * associated with the species' quantity.  <em>Restrictions</em>: There
 * must not be both an AssignmentRule "variable" attribute and a
 * SpeciesReference "species" attribute having the same value, unless the
 * referenced Species object has its "boundaryCondition" attribute set to
 * @c true.  In other words, an assignment rule cannot be defined for a
 * species that is created or destroyed in a reaction unless that species
 * is defined as a boundary condition in the model.
 *
 * <li> (For SBML Level&nbsp;3 only) <em>In the case of a species
 * reference</em>, an AssignmentRule sets the stoichiometry of the
 * referenced reactant or product to the value determined by the formula in
 * "math".  The unit associated with the value produced by the "math"
 * formula should be consistent with the unit "dimensionless", because
 * reactant and product stoichiometries in reactions are dimensionless
 * quantities.
  *
 * <li> <em>In the case of a compartment</em>, an AssignmentRule sets the
 * referenced compartment's size to the value determined by the formula in
 * the "math" subelement of the AssignmentRule object.  The overall units
 * of the formula in "math" @em should (in SBML Level&nbsp;2 Version&nbsp;4
 * and later) or @em must (in SBML releases prior to Level&nbsp;2
 * version&nbsp;4) be the same as the units of the size of the compartment.
 *
 * <li> <em>In the case of a parameter</em>, an AssignmentRule sets the
 * referenced parameter's value to that determined by the formula in the
 * "math" subelement of the AssignmentRule object.  The overall units of
 * the formula in the "math" subelement @em should (in SBML Level&nbsp;2
 * Version&nbsp;4 and later) or @em must (in SBML releases prior to
 * Level&nbsp;2 version&nbsp;4) be the same as the units defined for the
 * parameter.  </ul>
 * 
 * In the context of a simulation, assignment rules are in effect at all
 * times, <em>t</em> \f$\geq\f$ <em>0</em>.  For purposes of evaluating
 * expressions that involve the <em>delay</em> "csymbol" (see the SBML
 * Level&nbsp;2 specification), assignment rules are considered to apply
 * also at <em>t</em> \f$\leq\f$ <em>0</em>.  Please consult the relevant
 * SBML specification for additional information about the semantics of
 * assignments, rules, and entity values for simulation time <em>t</em>
 * \f$\leq\f$ <em>0</em>.
 *
 * A model must not contain more than one AssignmentRule or RateRule
 * object having the same value of "variable"; in other words, in the set
 * of all assignment rules and rate rules in an SBML model, each variable
 * appearing in the left-hand sides can only appear once.  This simply
 * follows from the fact that an indeterminate system would result if a
 * model contained more than one assignment rule for the same variable or
 * both an assignment rule and a rate rule for the same variable.
 *
 * Similarly, a model must also not contain <em>both</em> an AssignmentRule
 * and an InitialAssignment for the same variable, because both kinds of
 * constructs apply prior to and at the start of simulation time, i.e.,
 * <em>t</em> \f$\leq\f$ <em>0</em>.  If a model contained both an initial
 * assignment and an assignment rule for the same variable, an
 * indeterminate system would result.
 *
 * The value calculated by an AssignmentRule object overrides the value
 * assigned to the given symbol by the object defining that symbol.  For
 * example, if a Compartment object's "size" attribute value is set in its
 * definition, and the model also contains an AssignmentRule object having
 * that compartment's "id" as its "variable" value, then the "size"
 * assigned in the Compartment object definition is ignored and the value
 * assigned based on the computation defined in the AssignmentRule.  This
 * does <em>not</em> mean that a definition for a given symbol can be
 * omitted if there is an AssignmentRule object for it.  For example, there
 * must be a Parameter definition for a given parameter if there is an
 * AssignmentRule for that parameter.  It is only a question of which value
 * definition takes precedence.
 * 
 * @section general General summary of SBML rules
 *
 * In SBML Level&nbsp;3 as well as Level&nbsp;2, rules are separated into three
 * subclasses for the benefit of model analysis software.  The three
 * subclasses are based on the following three different possible functional
 * forms (where <em>x</em> is a variable, <em>f</em> is some arbitrary
 * function returning a numerical result, <b><em>V</em></b> is a vector of
 * variables that does not include <em>x</em>, and <b><em>W</em></b> is a
 * vector of variables that may include <em>x</em>):
 * 
 * <center>
 * <table border="0" cellpadding="0" style="font-size: small">
 * <tr><td width="120px"><em>Algebraic:</em></td><td width="250px">left-hand side is zero</td><td><em>0 = f(<b>W</b>)</em></td></tr>
 * <tr><td><em>Assignment:</em></td><td>left-hand side is a scalar:</td><td><em>x = f(<b>V</b>)</em></td></tr>
 * <tr><td><em>Rate:</em></td><td>left-hand side is a rate-of-change:</td><td><em>dx/dt = f(<b>W</b>)</em></td></tr>
 * </table>
 * </center>
 * 
 * In their general form given above, there is little to distinguish
 * between <em>assignment</em> and <em>algebraic</em> rules.  They are treated as
 * separate cases for the following reasons:
 *
 * <ul>
 * <li> <em>Assignment</em> rules can simply be evaluated to calculate
 * intermediate values for use in numerical methods.  They are statements
 * of equality that hold at all times.  (For assignments that are only
 * performed once, see InitialAssignment.)<p>
 * 
 * <li> SBML needs to place restrictions on assignment rules, for example
 * the restriction that assignment rules cannot contain algebraic loops.<p>
 * 
 * <li> Some simulators do not contain numerical solvers capable of solving
 * unconstrained algebraic equations, and providing more direct forms such
 * as assignment rules may enable those simulators to process models they
 * could not process if the same assignments were put in the form of
 * general algebraic equations;<p>
 * 
 * <li> Those simulators that <em>can</em> solve these algebraic equations make a
 * distinction between the different categories listed above; and<p>
 * 
 * <li> Some specialized numerical analyses of models may only be applicable
 * to models that do not contain <em>algebraic</em> rules.
 * </ul>
 * 
 * The approach taken to covering these cases in SBML is to define an
 * abstract Rule structure containing a subelement, "math", to hold the
 * right-hand side expression, then to derive subtypes of Rule that add
 * attributes to distinguish the cases of algebraic, assignment and rate
 * rules.  The "math" subelement must contain a MathML expression defining the
 * mathematical formula of the rule.  This MathML formula must return a
 * numerical value.  The formula can be an arbitrary expression referencing
 * the variables and other entities in an SBML model.
 * 
 * Each of the three subclasses of Rule (AssignmentRule, AlgebraicRule,
 * RateRule) inherit the the "math" subelement and other fields from SBase.
 * The AssignmentRule and RateRule classes add an additional attribute,
 * "variable".  See the definitions of AssignmentRule, AlgebraicRule and
 * RateRule for details about the structure and interpretation of each one.
 * 
 * @section additional-restrictions Additional restrictions on SBML rules
 * 
 * An important design goal of SBML rule semantics is to ensure that a
 * model's simulation and analysis results will not be dependent on when or
 * how often rules are evaluated.  To achieve this, SBML needs to place two
 * restrictions on rule use.  The first concerns algebraic loops in the system
 * of assignments in a model, and the second concerns overdetermined systems.
 * 
 * @subsection no-algebraic-loops A model must not contain algebraic loops
 * 
 * The combined set of InitialAssignment, AssignmentRule and KineticLaw
 * objects in a model constitute a set of assignment statements that should be
 * considered as a whole.  (A KineticLaw object is counted as an assignment
 * because it assigns a value to the symbol contained in the "id" attribute of
 * the Reaction object in which it is defined.)  This combined set of
 * assignment statements must not contain algebraic loops&mdash;dependency
 * chains between these statements must terminate.  To put this more formally,
 * consider a directed graph in which nodes are assignment statements and
 * directed arcs exist for each occurrence of an SBML species, compartment or
 * parameter symbol in an assignment statement's "math" subelement.  Let the
 * directed arcs point from the statement assigning the symbol to the
 * statements that contain the symbol in their "math" subelement expressions.
 * This graph must be acyclic.
 * 
 * SBML does not specify when or how often rules should be evaluated.
 * Eliminating algebraic loops ensures that assignment statements can be
 * evaluated any number of times without the result of those evaluations
 * changing.  As an example, consider the set of equations <em>x = x + 1</em>,
 * <em>y = z + 200</em> and <em>z = y + 100</em>.  If this set of equations
 * were interpreted as a set of assignment statements, it would be invalid
 * because the rule for <em>x</em> refers to <em>x</em> (exhibiting one type
 * of loop), and the rule for <em>y</em> refers to <em>z</em> while the rule
 * for <em>z</em> refers back to <em>y</em> (exhibiting another type of loop).
 * Conversely, the following set of equations would constitute a valid set of
 * assignment statements: <em>x = 10</em>, <em>y = z + 200</em>, and <em>z = x
 * + 100</em>.
 * 
 * @subsection no-overdetermined A model must not be overdetermined
 * 
 * An SBML model must not be overdetermined; that is, a model must not
 * define more equations than there are unknowns in a model.  An SBML model
 * that does not contain AlgebraicRule structures cannot be overdetermined.
 * 
 * LibSBML implements the static analysis procedure described in Appendix
 * B of the SBML Level&nbsp;3 Version&nbsp;1 Core specification for assessing
 * whether a model is overdetermined.
 * 
 * (In summary, assessing whether a given continuous, deterministic,
 * mathematical model is overdetermined does not require dynamic analysis; it
 * can be done by analyzing the system of equations created from the model.
 * One approach is to construct a bipartite graph in which one set of vertices
 * represents the variables and the other the set of vertices represents the
 * equations.  Place edges between vertices such that variables in the system
 * are linked to the equations that determine them.  For algebraic equations,
 * there will be edges between the equation and each variable occurring in the
 * equation.  For ordinary differential equations (such as those defined by
 * rate rules or implied by the reaction rate definitions), there will be a
 * single edge between the equation and the variable determined by that
 * differential equation.  A mathematical model is overdetermined if the
 * maximal matchings of the bipartite graph contain disconnected vertexes
 * representing equations.  If one maximal matching has this property, then
 * all the maximal matchings will have this property; i.e., it is only
 * necessary to find one maximal matching.)
 *
 *
 * @section RuleType_t Rule types for SBML Level 1
 *
 * SBML Level 1 uses a different scheme than SBML Level&nbsp;2 and
 * Level&nbsp;3 for distinguishing rules; specifically, it uses an
 * attribute whose value is drawn from an enumeration of 3 values.  LibSBML
 * supports this using methods that work @if clike a libSBML enumeration
 * type, #RuleType_t, whose values are@endif@if java  with the enumeration
 * values@endif listed in the following table.  
 * 
 * <p>
 * <center>
 * <table width="90%" cellspacing="1" cellpadding="1" border="0" class="normal-font">
 *  <tr style="background: lightgray" class="normal-font">
 *      <td><strong>Enumerator</strong></td>
 *      <td><strong>Meaning</strong></td>
 *  </tr>
 * <tr><td><code>@link RuleType_t#RULE_TYPE_RATE RULE_TYPE_RATE@endlink</code></td><td>Indicates the rule is a "rate" rule.</td>
 * <tr><td><code>@link RuleType_t#RULE_TYPE_SCALAR RULE_TYPE_SCALAR@endlink</code></td><td>Indicates the rule is a "scalar" rule.</td>
 * <tr><td><code>@link RuleType_t#RULE_TYPE_INVALID RULE_TYPE_INVALID@endlink</code></td><td>Indicates the rule type is unknown or not
 * yet set.</td>
 * </table>
 * </center>
 *
 * <!-- leave this next break as-is to work around some doxygen bug -->
 */ 
/**
 * @class RateRule
 * @brief LibSBML implementation of %SBML's %RateRule construct.
 *
 * The rule type RateRule is derived from the parent class Rule.  It is
 * used to express equations that determine the rates of change of
 * variables.  The left-hand side (the "variable" attribute) can refer to
 * the identifier of a species, compartment, or parameter (but not a
 * reaction).  The entity identified must have its "constant" attribute set
 * to @c false.  The effects of a RateRule are in general terms the same,
 * but differ in the precise details depending on which variable is being
 * set:
 * 
 * <ul> <li> <em>In the case of a species</em>, a RateRule sets the rate of
 * change of the species' quantity (<em>concentration</em> or <em>amount of
 * substance</em>) to the value determined by the formula in the "math"
 * subelement of the RateRule object.  The overall units of the formula in
 * "math" @em should (in SBML Level&nbsp;2 Version&nbsp;4 and later) or @em
 * must (in SBML releases prior to Level&nbsp;2 version&nbsp;4) be equal to
 * the unit of <em>species quantity</em> divided by the model-wide unit of
 * <em>time</em>.  <em>Restrictions</em>: There must not be both a RateRule
 * "variable" attribute and a SpeciesReference "species" attribute having
 * the same value, unless that species has its "boundaryCondition"
 * attribute is set to @c true.  This means a rate rule cannot be defined
 * for a species that is created or destroyed in a reaction, unless that
 * species is defined as a boundary condition in the model.
 *
 * <li> (For SBML Level&nbsp;3 only) <em>In the case of a species
 * reference</em>, a RateRule sets the rate of change of the stoichiometry
 * of the referenced reactant or product to the value determined by the
 * formula in "math".  The unit associated with the value produced by the
 * "math" formula should be consistent with the unit "dimensionless"
 * divided by the model-wide unit of <em>time</em>.
 *
 * <li> <em>In the case of a compartment</em>, a RateRule sets the rate of
 * change of the compartment's size to the value determined by the formula
 * in the "math" subelement of the RateRule object.  The overall units of
 * the formula @em should (in SBML Level&nbsp;2 Version&nbsp;4 and
 * later) or @em must (in SBML releases prior to Level&nbsp;2
 * version&nbsp;4) be the units of the compartment's <em>size</em> divided
 * by the model-wide unit of <em>time</em>.
 *
 * <li> <em>In the case of a parameter</em>, a RateRule sets the rate of
 * change of the parameter's value to that determined by the formula in the
 * "math" subelement of the RateRule object.  The overall units of the
 * formula @em should (in SBML Level&nbsp;2 Version&nbsp;4 and
 * later) or @em must (in SBML releases prior to Level&nbsp;2
 * version&nbsp;4) be the Parameter object's "unit" attribute value divided
 * by the model-wide unit of <em>time</em>.
 * </ul>
 * 
 * In the context of a simulation, rate rules are in effect for simulation
 * time <em>t</em> &lt; <em>0</em>.  Please consult the relevant SBML
 * specification for additional information about the semantics of
 * assignments, rules, and entity values for simulation time <em>t</em>
 * \f$\leq\f$ <em>0</em>.
 *
 * As mentioned in the description of AssignmentRule, a model must not
 * contain more than one RateRule or AssignmentRule object having the same
 * value of "variable"; in other words, in the set of all assignment rules
 * and rate rules in an SBML model, each variable appearing in the
 * left-hand sides can only appear once.  This simply follows from the fact
 * that an indeterminate system would result if a model contained more than
 * one assignment rule for the same variable or both an assignment rule and
 * a rate rule for the same variable.
 * 
 * @section general General summary of SBML rules
 *
 * In SBML Level&nbsp;3 as well as Level&nbsp;2, rules are separated into
 * three subclasses for the benefit of model analysis software.  The three
 * subclasses are based on the following three different possible
 * functional forms (where <em>x</em> is a variable, <em>f</em> is some
 * arbitrary function returning a numerical result, <b><em>V</em></b> is a
 * vector of variables that does not include <em>x</em>, and
 * <b><em>W</em></b> is a vector of variables that may include <em>x</em>):
 * 
 * <center>
 * <table border="0" cellpadding="0" style="font-size: small">
 * <tr><td width="120px"><em>Algebraic:</em></td><td width="250px">left-hand side is zero</td><td><em>0 = f(<b>W</b>)</em></td></tr>
 * <tr><td><em>Assignment:</em></td><td>left-hand side is a scalar:</td><td><em>x = f(<b>V</b>)</em></td></tr>
 * <tr><td><em>Rate:</em></td><td>left-hand side is a rate-of-change:</td><td><em>dx/dt = f(<b>W</b>)</em></td></tr>
 * </table>
 * </center>
 * 
 * In their general form given above, there is little to distinguish
 * between <em>assignment</em> and <em>algebraic</em> rules.  They are treated as
 * separate cases for the following reasons:
 *
 * <ul>
 * <li> <em>Assignment</em> rules can simply be evaluated to calculate
 * intermediate values for use in numerical methods.  They are statements
 * of equality that hold at all times.  (For assignments that are only
 * performed once, see InitialAssignment.)<p>
 * 
 * <li> SBML needs to place restrictions on assignment rules, for example
 * the restriction that assignment rules cannot contain algebraic loops.<p>
 * 
 * <li> Some simulators do not contain numerical solvers capable of solving
 * unconstrained algebraic equations, and providing more direct forms such
 * as assignment rules may enable those simulators to process models they
 * could not process if the same assignments were put in the form of
 * general algebraic equations;<p>
 * 
 * <li> Those simulators that <em>can</em> solve these algebraic equations make a
 * distinction between the different categories listed above; and<p>
 * 
 * <li> Some specialized numerical analyses of models may only be applicable
 * to models that do not contain <em>algebraic</em> rules.
 * </ul>
 * 
 * The approach taken to covering these cases in SBML is to define an
 * abstract Rule structure containing a subelement, "math", to hold the
 * right-hand side expression, then to derive subtypes of Rule that add
 * attributes to distinguish the cases of algebraic, assignment and rate
 * rules.  The "math" subelement must contain a MathML expression defining the
 * mathematical formula of the rule.  This MathML formula must return a
 * numerical value.  The formula can be an arbitrary expression referencing
 * the variables and other entities in an SBML model.
 * 
 * Each of the three subclasses of Rule (AssignmentRule, AlgebraicRule,
 * RateRule) inherit the the "math" subelement and other fields from SBase.
 * The AssignmentRule and RateRule classes add an additional attribute,
 * "variable".  See the definitions of AssignmentRule, AlgebraicRule and
 * RateRule for details about the structure and interpretation of each one.
 * 
 * @section additional-restrictions Additional restrictions on SBML rules
 * 
 * An important design goal of SBML rule semantics is to ensure that a
 * model's simulation and analysis results will not be dependent on when or
 * how often rules are evaluated.  To achieve this, SBML needs to place two
 * restrictions on rule use.  The first concerns algebraic loops in the system
 * of assignments in a model, and the second concerns overdetermined systems.
 * 
 * @subsection no-algebraic-loops A model must not contain algebraic loops
 * 
 * The combined set of InitialAssignment, AssignmentRule and KineticLaw
 * objects in a model constitute a set of assignment statements that should be
 * considered as a whole.  (A KineticLaw object is counted as an assignment
 * because it assigns a value to the symbol contained in the "id" attribute of
 * the Reaction object in which it is defined.)  This combined set of
 * assignment statements must not contain algebraic loops&mdash;dependency
 * chains between these statements must terminate.  To put this more formally,
 * consider a directed graph in which nodes are assignment statements and
 * directed arcs exist for each occurrence of an SBML species, compartment or
 * parameter symbol in an assignment statement's "math" subelement.  Let the
 * directed arcs point from the statement assigning the symbol to the
 * statements that contain the symbol in their "math" subelement expressions.
 * This graph must be acyclic.
 * 
 * SBML does not specify when or how often rules should be evaluated.
 * Eliminating algebraic loops ensures that assignment statements can be
 * evaluated any number of times without the result of those evaluations
 * changing.  As an example, consider the set of equations <em>x = x + 1</em>,
 * <em>y = z + 200</em> and <em>z = y + 100</em>.  If this set of equations
 * were interpreted as a set of assignment statements, it would be invalid
 * because the rule for <em>x</em> refers to <em>x</em> (exhibiting one type
 * of loop), and the rule for <em>y</em> refers to <em>z</em> while the rule
 * for <em>z</em> refers back to <em>y</em> (exhibiting another type of loop).
 * Conversely, the following set of equations would constitute a valid set of
 * assignment statements: <em>x = 10</em>, <em>y = z + 200</em>, and <em>z = x
 * + 100</em>.
 * 
 * @subsection no-overdetermined A model must not be overdetermined
 * 
 * An SBML model must not be overdetermined; that is, a model must not
 * define more equations than there are unknowns in a model.  An SBML model
 * that does not contain AlgebraicRule structures cannot be overdetermined.
 * 
 * LibSBML implements the static analysis procedure described in Appendix
 * B of the SBML Level&nbsp;3 Version&nbsp;1 Core specification for assessing
 * whether a model is overdetermined.
 * 
 * (In summary, assessing whether a given continuous, deterministic,
 * mathematical model is overdetermined does not require dynamic analysis; it
 * can be done by analyzing the system of equations created from the model.
 * One approach is to construct a bipartite graph in which one set of vertices
 * represents the variables and the other the set of vertices represents the
 * equations.  Place edges between vertices such that variables in the system
 * are linked to the equations that determine them.  For algebraic equations,
 * there will be edges between the equation and each variable occurring in the
 * equation.  For ordinary differential equations (such as those defined by
 * rate rules or implied by the reaction rate definitions), there will be a
 * single edge between the equation and the variable determined by that
 * differential equation.  A mathematical model is overdetermined if the
 * maximal matchings of the bipartite graph contain disconnected vertexes
 * representing equations.  If one maximal matching has this property, then
 * all the maximal matchings will have this property; i.e., it is only
 * necessary to find one maximal matching.)
 *
 *
 * @section RuleType_t Rule types for SBML Level 1
 *
 * SBML Level 1 uses a different scheme than SBML Level&nbsp;2 and
 * Level&nbsp;3 for distinguishing rules; specifically, it uses an
 * attribute whose value is drawn from an enumeration of 3 values.  LibSBML
 * supports this using methods that work @if clike a libSBML enumeration
 * type, #RuleType_t, whose values are@endif@if java  with the enumeration
 * values@endif listed in the following table.  
 * 
 * <p>
 * <center>
 * <table width="90%" cellspacing="1" cellpadding="1" border="0" class="normal-font">
 *  <tr style="background: lightgray" class="normal-font">
 *      <td><strong>Enumerator</strong></td>
 *      <td><strong>Meaning</strong></td>
 *  </tr>
 * <tr><td><code>@link RuleType_t#RULE_TYPE_RATE RULE_TYPE_RATE@endlink</code></td><td>Indicates the rule is a "rate" rule.</td>
 * <tr><td><code>@link RuleType_t#RULE_TYPE_SCALAR RULE_TYPE_SCALAR@endlink</code></td><td>Indicates the rule is a "scalar" rule.</td>
 * <tr><td><code>@link RuleType_t#RULE_TYPE_INVALID RULE_TYPE_INVALID@endlink</code></td><td>Indicates the rule type is unknown or not
 * yet set.</td>
 * </table>
 * </center>
 *
 * <!-- leave this next break as-is to work around some doxygen bug -->
 */ 
/**
 * @class ListOfRules
 * @brief LibSBML implementation of SBML's %ListOfRules construct.
 * 
 * The various ListOf___ classes in %SBML are merely containers used for
 * organizing the main components of an %SBML model.  All are derived from
 * the abstract class SBase, and inherit the various attributes and
 * subelements of SBase, such as "metaid" as and "annotation".  The
 * ListOf___ classes do not add any attributes of their own.
 *
 * The relationship between the lists and the rest of an %SBML model is
 * illustrated by the following (for SBML Level&nbsp;3 and later versions
 * of SBML Level&nbsp;2 as well):
 *
 * @image html listof-illustration.jpg "ListOf___ elements in an SBML Model"
 * @image latex listof-illustration.jpg "ListOf___ elements in an SBML Model"
 *
 * Readers may wonder about the motivations for using the ListOf___
 * containers.  A simpler approach in XML might be to place the components
 * all directly at the top level of the model definition.  We chose instead
 * to group them within XML elements named after ListOf<em>Classname</em>,
 * in part because we believe this helps organize the components and makes
 * visual reading of models in XML easier.  More importantly, the fact that
 * the container classes are derived from SBase means that software tools
 * can add information about the lists themselves into each list
 * container's "annotation".
 *
 * @see ListOfFunctionDefinitions
 * @see ListOfUnitDefinitions
 * @see ListOfCompartmentTypes
 * @see ListOfSpeciesTypes
 * @see ListOfCompartments
 * @see ListOfSpecies
 * @see ListOfParameters
 * @see ListOfInitialAssignments
 * @see ListOfRules
 * @see ListOfConstraints
 * @see ListOfReactions
 * @see ListOfEvents
 */


#ifndef Rule_h
#define Rule_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


BEGIN_C_DECLS

/**
 * @enum RuleType_t
 */
typedef enum
{
    RULE_TYPE_RATE
  , RULE_TYPE_SCALAR
  , RULE_TYPE_INVALID
} RuleType_t;

END_C_DECLS


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class ASTNode;
class ListOfRules;
class SBMLVisitor;


class LIBSBML_EXTERN Rule : public SBase
{
public:

  /**
   * Destroys this Rule.
   */
  virtual ~Rule ();


  /**
   * Copy constructor; creates a copy of this Rule.
   */
  Rule (const Rule& orig);


  /**
   * Assignment operator for Rule.
   */
  Rule& operator=(const Rule& rhs);


  /**
   * Accepts the given SBMLVisitor for this instance of Rule.
   *
   * @param v the SBMLVisitor instance to be used.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether the Visitor would like to visit the next Rule object in the
   * list of rules within which @em the present object is embedded.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /**
   * Creates and returns a deep copy of this Rule.
   * 
   * @return a (deep) copy of this Rule.
   */
  virtual Rule* clone () const;


  /**
   * Returns the mathematical expression of this Rule in text-string form.
   *
   * The text string is produced by
   * @if clike SBML_formulaToString()@endif@if java <code><a href="libsbml.html#formulaToString(org.sbml.libsbml.ASTNode)">libsbml.formulaToString()</a></code>@endif; please consult
   * the documentation for that function to find out more about the format
   * of the text-string formula.
   * 
   * @return the formula text string for this Rule.
   *
   * @see getMath()
   *
   * @note The attribute "formula" is specific to SBML Level&nbsp;1; in
   * higher Levels of SBML, it has been replaced with a subelement named
   * "math".  However, libSBML provides a unified interface to the
   * underlying math expression and this method can be used for models
   * of all Levels of SBML.
   */
  const std::string& getFormula () const;


  /**
   * Get the mathematical formula of this Rule as an ASTNode tree.
   *
   * @return an ASTNode, the value of the "math" subelement of this Rule.
   *
   * @see getFormula()
   *
   * @note The subelement "math" is present in SBML Levels&nbsp;2
   * and&nbsp;3.  In SBML Level&nbsp;1, the equivalent construct is the
   * attribute named "formula".  LibSBML provides a unified interface to
   * the underlying math expression and this method can be used for models
   * of all Levels of SBML.
   */
  const ASTNode* getMath () const;


  /**
   * Get the value of the "variable" attribute of this Rule object.
   *
   * In SBML Level&nbsp;1, the different rule types each have a different
   * name for the attribute holding the reference to the object
   * constituting the left-hand side of the rule.  (E.g., for
   * SpeciesConcentrationRule the attribute is "species", for
   * CompartmentVolumeRule it is "compartment", etc.)  In SBML
   * Levels&nbsp;2 and&nbsp;3, the only two types of Rule objects with a
   * left-hand side object reference are AssignmentRule and RateRule, and
   * both of them use the same name for attribute: "variable".  In order to
   * make it easier for application developers to work with all Levels of
   * SBML, libSBML uses a uniform name for all of such attributes, and it
   * is "variable", regardless of whether Level&nbsp;1 rules or
   * Level&nbsp;2&ndash;3 rules are being used.
   * 
   * @return the identifier string stored as the "variable" attribute value
   * in this Rule, or @c NULL if this object is an AlgebraicRule object.
   */
  const std::string& getVariable () const;


  /**
   * Returns the units for the
   * mathematical formula of this Rule.
   * 
   * @return the identifier of the units for the expression of this Rule.
   *
   * @note The attribute "units" exists on SBML Level&nbsp;1 ParameterRule
   * objects only.  It is not present in SBML Levels&nbsp;2 and&nbsp;3.
   */
  const std::string& getUnits () const;


  /**
   * Predicate returning @c true if this
   * Rule's mathematical expression has been set.
   * 
   * This method is equivalent to isSetMath().  This version is present for
   * easier compatibility with SBML Level&nbsp;1, in which mathematical
   * formulas were written in text-string form.
   * 
   * @return @c true if the mathematical formula for this Rule has been
   * set, @c false otherwise.
   *
   * @see isSetMath()
   *
   * @note The attribute "formula" is specific to SBML Level&nbsp;1; in
   * higher Levels of SBML, it has been replaced with a subelement named
   * "math".  However, libSBML provides a unified interface to the
   * underlying math expression and this method can be used for models
   * of all Levels of SBML.
   */
  bool isSetFormula () const;


  /**
   * Predicate returning @c true if this
   * Rule's mathematical expression has been set.
   *
   * This method is equivalent to isSetFormula().
   * 
   * @return @c true if the formula (or equivalently the math) for this
   * Rule has been set, @c false otherwise.
   *
   * @note The subelement "math" is present in SBML Levels&nbsp;2
   * and&nbsp;3.  In SBML Level&nbsp;1, the equivalent construct is the
   * attribute named "formula".  LibSBML provides a unified interface to
   * the underlying math expression and this method can be used for models
   * of all Levels of SBML.
   */
  bool isSetMath () const;


  /**
   * Predicate returning @c true if this
   * Rule's "variable" attribute has been set.
   *
   * In SBML Level&nbsp;1, the different rule types each have a different
   * name for the attribute holding the reference to the object
   * constituting the left-hand side of the rule.  (E.g., for
   * SpeciesConcentrationRule the attribute is "species", for
   * CompartmentVolumeRule it is "compartment", etc.)  In SBML
   * Levels&nbsp;2 and&nbsp;3, the only two types of Rule objects with a
   * left-hand side object reference are AssignmentRule and RateRule, and
   * both of them use the same name for attribute: "variable".  In order to
   * make it easier for application developers to work with all Levels of
   * SBML, libSBML uses a uniform name for all such attributes, and it is
   * "variable", regardless of whether Level&nbsp;1 rules or
   * Level&nbsp;2&ndash;3 rules are being used.
   *
   * @return @c true if the "variable" attribute value of this Rule has
   * been set, @c false otherwise.
   */
  bool isSetVariable () const;


  /**
   * Predicate returning @c true
   * if this Rule's "units" attribute has been set.
   *
   * @return @c true if the units for this Rule has been set, @c false
   * otherwise
   *
   * @note The attribute "units" exists on SBML Level&nbsp;1 ParameterRule
   * objects only.  It is not present in SBML Levels&nbsp;2 and&nbsp;3.
   */
  bool isSetUnits () const;


  /**
   * Sets the "math" subelement of this Rule to an expression in
   * text-string form.
   *
   * This is equivalent to setMath().  The provision of using text-string
   * formulas is retained for easier SBML Level&nbsp;1 compatibility.  The
   * formula is converted to an ASTNode internally.
   *
   * @param formula a mathematical formula in text-string form.
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   *
   * @note The attribute "formula" is specific to SBML Level&nbsp;1; in
   * higher Levels of SBML, it has been replaced with a subelement named
   * "math".  However, libSBML provides a unified interface to the
   * underlying math expression and this method can be used for models
   * of all Levels of SBML.
   */
  int setFormula (const std::string& formula);


  /**
   * Sets the "math" subelement of this Rule to a copy of the given
   * ASTNode.
   *
   * @param math the ASTNode structure of the mathematical formula.
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_OBJECT LIBSBML_INVALID_OBJECT @endlink
   *
   * @note The subelement "math" is present in SBML Levels&nbsp;2
   * and&nbsp;3.  In SBML Level&nbsp;1, the equivalent construct is the
   * attribute named "formula".  LibSBML provides a unified interface to
   * the underlying math expression and this method can be used for models
   * of all Levels of SBML.
   */
  int setMath (const ASTNode* math);


  /**
   * Sets the "variable" attribute value of this Rule object.
   *
   * In SBML Level&nbsp;1, the different rule types each have a different
   * name for the attribute holding the reference to the object
   * constituting the left-hand side of the rule.  (E.g., for
   * SpeciesConcentrationRule the attribute is "species", for
   * CompartmentVolumeRule it is "compartment", etc.)  In SBML
   * Levels&nbsp;2 and&nbsp;3, the only two types of Rule objects with a
   * left-hand side object reference are AssignmentRule and RateRule, and
   * both of them use the same name for attribute: "variable".  In order to
   * make it easier for application developers to work with all Levels of
   * SBML, libSBML uses a uniform name for all such attributes, and it is
   * "variable", regardless of whether Level&nbsp;1 rules or
   * Level&nbsp;2&ndash;3 rules are being used.
   * 
   * @param sid the identifier of a Compartment, Species or Parameter
   * elsewhere in the enclosing Model object.
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink
   */
  int setVariable (const std::string& sid);


  /**
   * Sets the units for this Rule.
   *
   * @param sname the identifier of the units
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE @endlink
   * @li @link OperationReturnValues_t#LIBSBML_UNEXPECTED_ATTRIBUTE LIBSBML_UNEXPECTED_ATTRIBUTE @endlink
   *
   * @note The attribute "units" exists on SBML Level&nbsp;1 ParameterRule
   * objects only.  It is not present in SBML Levels&nbsp;2 and&nbsp;3.
   */
  int setUnits (const std::string& sname);


  /**
   * Unsets the "units" for this Rule.
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_FAILED LIBSBML_OPERATION_FAILED @endlink
   *
   * @note The attribute "units" exists on SBML Level&nbsp;1 ParameterRule
   * objects only.  It is not present in SBML Levels&nbsp;2 and&nbsp;3.
   */
  int unsetUnits ();


  /**
   * Calculates and returns a UnitDefinition that expresses the units of
   * measurement assumed for the "math" expression of this Rule.
   *
   * The units are calculated based on the mathematical expression in the
   * Rule and the model quantities referenced by <code>&lt;ci&gt;</code>
   * elements used within that expression.  The getDerivedUnitDefinition()
   * method returns the calculated units.
   *
   * Note that the functionality that facilitates unit analysis depends 
   * on the model as a whole.  Thus, in cases where the object has not 
   * been added to a model or the model itself is incomplete,
   * unit analysis is not possible and this method will return @c NULL.
   *
   * @warning Note that it is possible the "math" expression in the Rule
   * contains pure numbers or parameters with undeclared units.  In those
   * cases, it is not possible to calculate the units of the overall
   * expression without making assumptions.  LibSBML does not make
   * assumptions about the units, and getDerivedUnitDefinition() only
   * returns the units as far as it is able to determine them.  For
   * example, in an expression <em>X + Y</em>, if <em>X</em> has
   * unambiguously-defined units and <em>Y</em> does not, it will return
   * the units of <em>X</em>.  <strong>It is important that callers also
   * invoke the method</strong>
   * @if clike containsUndeclaredUnits()@endif@if java Rule::containsUndeclaredUnits()@endif
   * <strong>to determine whether this situation holds</strong>.  Callers may
   * wish to take suitable actions in those scenarios.
   * 
   * @return a UnitDefinition that expresses the units of the math 
   * expression of this Rule, or @c NULL if one cannot be constructed.
   *
   * @see containsUndeclaredUnits()
   */
  UnitDefinition * getDerivedUnitDefinition();


  /**
   * Calculates and returns a UnitDefinition that expresses the units of
   * measurement assumed for the "math" expression of this Rule.
   *
   * The units are calculated based on the mathematical expression in the
   * Rule and the model quantities referenced by <code>&lt;ci&gt;</code>
   * elements used within that expression.  The getDerivedUnitDefinition()
   * method returns the calculated units.
   *
   * Note that the functionality that facilitates unit analysis depends 
   * on the model as a whole.  Thus, in cases where the object has not 
   * been added to a model or the model itself is incomplete,
   * unit analysis is not possible and this method will return @c NULL.
   *
   * @warning Note that it is possible the "math" expression in the Rule
   * contains pure numbers or parameters with undeclared units.  In those
   * cases, it is not possible to calculate the units of the overall
   * expression without making assumptions.  LibSBML does not make
   * assumptions about the units, and getDerivedUnitDefinition() only
   * returns the units as far as it is able to determine them.  For
   * example, in an expression <em>X + Y</em>, if <em>X</em> has
   * unambiguously-defined units and <em>Y</em> does not, it will return
   * the units of <em>X</em>.  <strong>It is important that callers also
   * invoke the method</strong>
   * @if clike containsUndeclaredUnits()@endif@if java Rule::containsUndeclaredUnits()@endif
   * <strong>to determine whether this situation holds</strong>.  Callers
   * may wish to take suitable actions in those scenarios.
   * 
   * @return a UnitDefinition that expresses the units of the math 
   * expression of this Rule, or @c NULL if one cannot be constructed.
   *
   * @see containsUndeclaredUnits()
   */
  const UnitDefinition * getDerivedUnitDefinition() const;


  /**
   * Predicate returning @c true if 
   * the math expression of this Rule contains
   * parameters/numbers with undeclared units.
   * 
   * @return @c true if the math expression of this Rule
   * includes parameters/numbers 
   * with undeclared units, @c false otherwise.
   *
   * @note A return value of @c true indicates that the UnitDefinition
   * returned by getDerivedUnitDefinition() may not accurately represent
   * the units of the expression.
   *
   * @see getDerivedUnitDefinition()
   */
  bool containsUndeclaredUnits();


  /**
   * Predicate returning @c true if 
   * the math expression of this Rule contains
   * parameters/numbers with undeclared units.
   * 
   * @return @c true if the math expression of this Rule
   * includes parameters/numbers 
   * with undeclared units, @c false otherwise.
   *
   * @note A return value of @c true indicates that the UnitDefinition
   * returned by getDerivedUnitDefinition() may not accurately represent
   * the units of the expression.
   *
   * @see getDerivedUnitDefinition()
   */
  bool containsUndeclaredUnits() const;


  /**
   * Get the type of rule this is.
   * 
   * @return the rule type (a value drawn from the enumeration <a
   * class="el" href="#RuleType_t">RuleType_t</a>) of this Rule.  The value
   * will be either @link RuleType_t#RULE_TYPE_RATE RULE_TYPE_RATE@endlink
   * or @link RuleType_t#RULE_TYPE_SCALAR RULE_TYPE_SCALAR@endlink.
   *
   * @note The attribute "type" on Rule objects was present only in SBML
   * Level&nbsp;1.  In SBML Level&nbsp;2 and later, the type has been
   * replaced by subclassing the Rule object.
   */
  RuleType_t getType () const;


  /**
   * Predicate returning @c true if this
   * Rule is an AlgebraicRule.
   * 
   * @return @c true if this Rule is an AlgebraicRule, @c false otherwise.
   */
  bool isAlgebraic () const;


  /**
   * Predicate returning @c true if this
   * Rule is an AssignmentRule.
   * 
   * @return @c true if this Rule is an AssignmentRule, @c false otherwise.
   */
  bool isAssignment () const;


  /**
   * Predicate returning @c true if
   * this Rule is an CompartmentVolumeRule.
   *
   * @return @c true if this Rule is a CompartmentVolumeRule, @c false
   * otherwise.
   */
  bool isCompartmentVolume () const;


  /**
   * Predicate returning @c true if
   * this Rule is an ParameterRule.
   *
   * @return @c true if this Rule is a ParameterRule, @c false
   * otherwise.
   */
  bool isParameter () const;


  /**
   * Predicate returning @c true if this Rule
   * is a RateRule (SBML Levels&nbsp;2&ndash;3) or has a "type" attribute
   * value of @c "rate" (SBML Level&nbsp;1).
   *
   * @return @c true if this Rule is a RateRule (Level&nbsp;2) or has
   * type "rate" (Level&nbsp;1), @c false otherwise.
   */
  bool isRate () const;


  /**
   * Predicate returning @c true if this Rule
   * is an AssignmentRule (SBML Levels&nbsp;2&ndash;3) or has a "type"
   * attribute value of @c "scalar" (SBML Level&nbsp;1).
   *
   * @return @c true if this Rule is an AssignmentRule (Level&nbsp;2) or has
   * type "scalar" (Level&nbsp;1), @c false otherwise.
   */
  bool isScalar () const;


  /**
   * Predicate returning @c true if
   * this Rule is an SpeciesConcentrationRule.
   *
   * @return @c true if this Rule is a SpeciesConcentrationRule, @c false
   * otherwise.
   */
  bool isSpeciesConcentration () const;


  /**
   * Returns the libSBML type code for this %SBML object.
   * 
   * @if clike LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.
   * The set of possible type codes is defined in the enumeration
   * #SBMLTypeCode_t.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if java LibSBML attaches an
   * identifying code to every kind of SBML object.  These are known as
   * <em>SBML type codes</em>.  In other languages, the set of type codes
   * is stored in an enumeration; in the Java language interface for
   * libSBML, the type codes are defined as static integer constants in the
   * interface class {@link libsbmlConstants}.  The names of the type codes
   * all begin with the characters @c SBML_. @endif
   *
   * @return the SBML type code for this object, or @link
   * SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;


  /**
   * Returns the SBML Level&nbsp;1 type code for this Rule object.
   *
   * This only applies to SBML Level&nbsp;1 model objects.  If this is not
   * an SBML Level&nbsp;1 rule object, this method will return @link
   * SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink.
   * 
   * @return the SBML Level&nbsp;1 type code for this Rule (namely, @link
   * SBMLTypeCode_t#SBML_COMPARTMENT_VOLUME_RULE
   * SBML_COMPARTMENT_VOLUME_RULE@endlink, @link
   * SBMLTypeCode_t#SBML_PARAMETER_RULE SBML_PARAMETER_RULE@endlink, @link
   * SBMLTypeCode_t#SBML_SPECIES_CONCENTRATION_RULE
   * SBML_SPECIES_CONCENTRATION_RULE@endlink, or @link
   * SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink).
   */
  SBMLTypeCode_t getL1TypeCode () const;


  /**
   * Returns the XML element name of this object
   *
   * The returned value can be any of a number of different strings,
   * depending on the SBML Level in use and the kind of Rule object this
   * is.  The rules as of libSBML 4.1.0 are the following:
   * <ul>
   * <li> (Level&nbsp;2 and&nbsp;3) RateRule: returns @c "rateRule"
   * <li> (Level&nbsp;2 and&nbsp;3) AssignmentRule: returns @c "assignmentRule" 
   * <li> (Level&nbsp;2 and&nbsp;3) AlgebraicRule: returns @c "algebraicRule"
   * <li> (Level&nbsp;1 Version&nbsp;1) SpecieConcentrationRule: returns @c "specieConcentrationRule"
   * <li> (Level&nbsp;1 Version&nbsp;2) SpeciesConcentrationRule: returns @c "speciesConcentrationRule"
   * <li> (Level&nbsp;1) CompartmentVolumeRule: returns @c "compartmentVolumeRule"
   * <li> (Level&nbsp;1) ParameterRule: returns @c "parameterRule"
   * <li> Unknown rule type: returns @c "unknownRule"
   * </ul>
   *
   * Beware that the last (@c "unknownRule") is not a valid SBML element
   * name.
   * 
   * @return the name of this element
   */
  virtual const std::string& getElementName () const;


  /** @cond doxygen-libsbml-internal */
  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;
  /** @endcond */


  /**
   * Sets the SBML Level&nbsp;1 type code for this Rule.
   *
   * @param type the SBML Level&nbsp;1 type code for this Rule, drawn from
   * the enumeration #SBMLTypeCode_t.  The allowable values are @link
   * SBMLTypeCode_t#SBML_COMPARTMENT_VOLUME_RULE
   * SBML_COMPARTMENT_VOLUME_RULE@endlink, @link
   * SBMLTypeCode_t#SBML_PARAMETER_RULE SBML_PARAMETER_RULE@endlink, and
   * @link SBMLTypeCode_t#SBML_SPECIES_CONCENTRATION_RULE
   * SBML_SPECIES_CONCENTRATION_RULE@endlink.
   *
   * @return integer value indicating success/failure of the
   * function.  The possible values returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS@endlink
   * @li @link OperationReturnValues_t#LIBSBML_INVALID_ATTRIBUTE_VALUE LIBSBML_INVALID_ATTRIBUTE_VALUE@endlink
   * if given @p type value is not one of the above.
   *
   */
  int setL1TypeCode (SBMLTypeCode_t type);


  /**
   * Predicate returning @c true if all the
   * required elements for this Rule object have been set.
   *
   * The only required element for a Rule object is the "math" subelement.
   *
   * @return a boolean value indicating whether all the required
   * elements for this object have been defined.
   */
  virtual bool hasRequiredElements() const ;


  /**
   * Predicate returning @c true if all the
   * required attributes for this Rule object have been set.
   *
   * The required attributes for a Rule object depend on the type of Rule
   * it is.  For AssignmentRule and RateRule objects (and SBML
   * Level&nbsp1's SpeciesConcentrationRule, CompartmentVolumeRule, and
   * ParameterRule objects), the required attribute is "variable"; for
   * AlgebraicRule objects, there is no required attribute.
   *
   * @return a boolean value indicating whether all the required
   * elements for this object have been defined.
   */
  virtual bool hasRequiredAttributes() const ;



  /** @cond doxygen-libsbml-internal */

  /* function to set/get an identifier for unit checking */
  std::string getInternalId() const { return mInternalId; };
  void setInternalId(std::string id) { mInternalId = id; };
  /** @endcond */
  /** @cond doxygen-libsbml-internal */
  
  /* overload use of getId to retrieve variable */
  std::string getId() const { return mVariable; };
  /** @endcond */


protected:
  /** @cond doxygen-libsbml-internal */

  /**
   * Only subclasses may create Rules.
   */
  //Rule (  SBMLTypeCode_t      type
  //      , const std::string&  variable
  //      , const std::string&  formula );

  ///**
  // * Only subclasses may create Rules.
  // */
  Rule (  SBMLTypeCode_t      type
        , unsigned int        level
        , unsigned int        version );

  Rule (  SBMLTypeCode_t      type
        , SBMLNamespaces *    sbmlns );


  /**
   * Subclasses should override this method to read (and store) XHTML,
   * MathML, etc. directly from the XMLInputStream.
   *
   * @return true if the subclass read from the stream, false otherwise.
   */
  virtual bool readOtherXML (XMLInputStream& stream);


  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual void readAttributes (const XMLAttributes& attributes);

  void readL1Attributes (const XMLAttributes& attributes);

  void readL2Attributes (const XMLAttributes& attributes);

  void readL3Attributes (const XMLAttributes& attributes);

 /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;




  std::string mVariable;
  mutable std::string  mFormula;
  mutable ASTNode*     mMath;
  std::string          mUnits;

  SBMLTypeCode_t mType;
  SBMLTypeCode_t mL1Type;


  /* internal id used by unit checking */
  std::string mInternalId;

  friend class ListOfRules;

  /** @endcond */
};



class LIBSBML_EXTERN AlgebraicRule : public Rule
{
public:

  /**
   * Creates a new AlgebraicRule using the given SBML @p level and @p version
   * values.
   *
   * @param level an unsigned int, the SBML Level to assign to this AlgebraicRule
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * AlgebraicRule
   * 
   * @note Upon the addition of an AlgebraicRule object to an SBMLDocument
   * (e.g., using @if clike Model::addRule()@endif@if java Model::addRule(Rule r)@endif), the SBML Level, SBML Version
   * and XML namespace of the document @em override the values used
   * when creating the AlgebraicRule object via this constructor.  This is
   * necessary to ensure that an SBML document is a consistent structure.
   * Nevertheless, the ability to supply the values at the time of creation
   * of a AlgebraicRule is an important aid to producing valid SBML.
   * Knowledge of the intented SBML Level and Version determine whether it
   * is valid to assign a particular value to an attribute, or whether it
   * is valid to add an object to an existing SBMLDocument.
   */
  AlgebraicRule (unsigned int level, unsigned int version);


  /**
   * Creates a new AlgebraicRule using the given SBMLNamespaces object
   * @p sbmlns.
   *
   * The SBMLNamespaces object encapsulates SBML Level/Version/namespaces
   * information.  It is used to communicate the SBML Level, Version, and
   * (in Level&nbsp;3) packages used in addition to SBML Level&nbsp;3 Core.
   * A common approach to using this class constructor is to create an
   * SBMLNamespaces object somewhere in a program, once, then pass it to
   * object constructors such as this one when needed.
   *
   * @param sbmlns an SBMLNamespaces object.
   *
   * @note Upon the addition of a AlgebraicRule object to an SBMLDocument
   * (e.g., using @if clike Model::addRule()@endif@if java Model::addRule(Rule r)@endif, the SBML XML namespace of the
   * document @em overrides the value used when creating the AlgebraicRule
   * object via this constructor.  This is necessary to ensure that an SBML
   * document is a consistent structure.  Nevertheless, the ability to
   * supply the values at the time of creation of a AlgebraicRule is an
   * important aid to producing valid SBML.  Knowledge of the intented SBML
   * Level and Version determine whether it is valid to assign a particular
   * value to an attribute, or whether it is valid to add an object to an
   * existing SBMLDocument.
   */
  AlgebraicRule (SBMLNamespaces* sbmlns);


  /**
   * Destroys this AlgebraicRule.
   */
  virtual ~AlgebraicRule ();


  /**
   * Creates and returns a deep copy of this Rule.
   * 
   * @return a (deep) copy of this Rule.
   */
  virtual AlgebraicRule* clone () const;


  /**
   * Accepts the given SBMLVisitor for this instance of AlgebraicRule.
   *
   * @param v the SBMLVisitor instance to be used.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether the Visitor would like to visit the next AlgebraicRule object
   * in the list of rules within which @em the present object is embedded.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /** @cond doxygen-libsbml-internal */

  /**
   * sets the mInternalIdOnly flag
   */

  void setInternalIdOnly();
  bool getInternalIdOnly() const;
  
  /** @endcond */


  /**
   * Predicate returning @c true if
   * all the required attributes for this AlgebraicRule object
   * have been set.
   *
   * @note In SBML Levels&nbsp;2&ndash;3, there is no required attribute
   * for an AlgebraicRule object.  For Level&nbsp;1, the only required
   * attribute is "formula".
   * 
   * @return @c true if the required attributes have been set, @c false
   * otherwise.
   */
  virtual bool hasRequiredAttributes() const ;


protected:
  /** @cond doxygen-libsbml-internal */

  /* this is a constructor that takes no arguments and 
   * only exists because the validator code needs it
   */
  AlgebraicRule ();


  bool mInternalIdOnly;

  /* the validator classes need to be friends to access the 
   * protected constructor that takes no arguments
   */
  friend class Validator;
  friend class ConsistencyValidator;
  friend class IdentifierConsistencyValidator;
  friend class InternalConsistencyValidator;
  friend class L1CompatibilityValidator;
  friend class L2v1CompatibilityValidator;
  friend class L2v2CompatibilityValidator;
  friend class L2v3CompatibilityValidator;
  friend class L2v4CompatibilityValidator;
  friend class MathMLConsistencyValidator;
  friend class ModelingPracticeValidator;
  friend class OverdeterminedValidator;
  friend class SBOConsistencyValidator;
  friend class UnitConsistencyValidator;

  /** @endcond */

};



class LIBSBML_EXTERN AssignmentRule : public Rule
{
public:

  /**
   * Creates a new AssignmentRule using the given SBML @p level and @p version
   * values.
   *
   * @param level an unsigned int, the SBML Level to assign to this AssignmentRule
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * AssignmentRule
   * 
   * @note Upon the addition of an AssignmentRule object to an SBMLDocument
   * (e.g., using @if clike Model::addRule()@endif@if java Model::addRule(Rule r)@endif, the SBML Level, SBML Version
   * and XML namespace of the document @em override the values used
   * when creating the AssignmentRule object via this constructor.  This is
   * necessary to ensure that an SBML document is a consistent structure.
   * Nevertheless, the ability to supply the values at the time of creation
   * of a AssignmentRule is an important aid to producing valid SBML.
   * Knowledge of the intented SBML Level and Version determine whether it
   * is valid to assign a particular value to an attribute, or whether it
   * is valid to add an object to an existing SBMLDocument.
   */
  AssignmentRule (unsigned int level, unsigned int version);


  /**
   * Creates a new AssignmentRule using the given SBMLNamespaces object
   * @p sbmlns.
   *
   * The SBMLNamespaces object encapsulates SBML Level/Version/namespaces
   * information.  It is used to communicate the SBML Level, Version, and
   * (in Level&nbsp;3) packages used in addition to SBML Level&nbsp;3 Core.
   * A common approach to using this class constructor is to create an
   * SBMLNamespaces object somewhere in a program, once, then pass it to
   * object constructors such as this one when needed.
   *
   * @param sbmlns an SBMLNamespaces object.
   *
   * @note Upon the addition of a AssignmentRule object to an SBMLDocument
   * (e.g., using @if clike Model::addRule()@endif@if java Model::addRule(Rule r)@endif, the SBML XML namespace of
   * the document @em overrides the value used when creating the
   * AssignmentRule object via this constructor.  This is necessary to
   * ensure that an SBML document is a consistent structure.  Nevertheless,
   * the ability to supply the values at the time of creation of a
   * AssignmentRule is an important aid to producing valid SBML.  Knowledge
   * of the intented SBML Level and Version determine whether it is valid
   * to assign a particular value to an attribute, or whether it is valid
   * to add an object to an existing SBMLDocument.
   */
  AssignmentRule (SBMLNamespaces* sbmlns);


  /**
   * Destroys this AssignmentRule.
   */
  virtual ~AssignmentRule ();


  /**
   * Creates and returns a deep copy of this Rule.
   * 
   * @return a (deep) copy of this Rule.
   */
  virtual AssignmentRule* clone () const;


  /**
   * Accepts the given SBMLVisitor for this instance of AssignmentRule.
   *
   * @param v the SBMLVisitor instance to be used.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether the Visitor would like to visit the next AssignmentRule object
   * in the list of rules within which @em the present object is embedded.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /**
   * Predicate returning @c true if
   * all the required attributes for this AssignmentRule object
   * have been set.
   *
   * @note In SBML Levels&nbsp;2&ndash;3, the only required attribute for
   * an AssignmentRule object is "variable".  For Level&nbsp;1, where the
   * equivalent attribute is known by different names ("compartment",
   * "species", or "name", depending on the type of object), there is an
   * additional required attribute called "formula".
   * 
   * @return @c true if the required attributes have been set, @c false
   * otherwise.
   */
  virtual bool hasRequiredAttributes() const ;


protected:
  /** @cond doxygen-libsbml-internal */

  /* this is a constructor that takes no arguments and 
   * only exists because the validator code needs it
   */
  AssignmentRule ();


  //std::string mVariable;
  //std::string mName;

  /* the validator classes need to be friends to access the 
   * protected constructor that takes no arguments
   */
  friend class Validator;
  friend class ConsistencyValidator;
  friend class IdentifierConsistencyValidator;
  friend class InternalConsistencyValidator;
  friend class L1CompatibilityValidator;
  friend class L2v1CompatibilityValidator;
  friend class L2v2CompatibilityValidator;
  friend class L2v3CompatibilityValidator;
  friend class L2v4CompatibilityValidator;
  friend class MathMLConsistencyValidator;
  friend class ModelingPracticeValidator;
  friend class OverdeterminedValidator;
  friend class SBOConsistencyValidator;
  friend class UnitConsistencyValidator;

  /** @endcond */
};



class LIBSBML_EXTERN RateRule : public Rule
{
public:

  /**
   * Creates a new RateRule using the given SBML @p level and @p version
   * values.
   *
   * @param level an unsigned int, the SBML Level to assign to this RateRule
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * RateRule
   * 
   * @note Upon the addition of a RateRule object to an SBMLDocument
   * (e.g., using @if clike Model::addRule()@endif@if java Model::addRule(Rule r)@endif, the SBML Level, SBML Version
   * and XML namespace of the document @em override the values used
   * when creating the RateRule object via this constructor.  This is
   * necessary to ensure that an SBML document is a consistent structure.
   * Nevertheless, the ability to supply the values at the time of creation
   * of a RateRule is an important aid to producing valid SBML.  Knowledge
   * of the intented SBML Level and Version determine whether it is valid
   * to assign a particular value to an attribute, or whether it is valid
   * to add an object to an existing SBMLDocument.
  */
  RateRule (unsigned int level, unsigned int version);


  /**
   * Creates a new RateRule using the given SBMLNamespaces object
   * @p sbmlns.
   *
   * The SBMLNamespaces object encapsulates SBML Level/Version/namespaces
   * information.  It is used to communicate the SBML Level, Version, and
   * (in Level&nbsp;3) packages used in addition to SBML Level&nbsp;3 Core.
   * A common approach to using this class constructor is to create an
   * SBMLNamespaces object somewhere in a program, once, then pass it to
   * object constructors such as this one when needed.
   *
   * @param sbmlns an SBMLNamespaces object.
   *
   * @note Upon the addition of a RateRule object to an SBMLDocument (e.g.,
   * using @if clike Model::addRule()@endif@if java Model::addRule(Rule r)@endif, the SBML XML namespace of the document
   * @em overrides the value used when creating the RateRule object via
   * this constructor.  This is necessary to ensure that an SBML document
   * is a consistent structure.  Nevertheless, the ability to supply the
   * values at the time of creation of a RateRule is an important aid to
   * producing valid SBML.  Knowledge of the intented SBML Level and
   * Version determine whether it is valid to assign a particular value to
   * an attribute, or whether it is valid to add an object to an existing
   * SBMLDocument.
   */
  RateRule (SBMLNamespaces* sbmlns);


  /**
   * Destroys this RateRule.
   */
  virtual ~RateRule ();

  /**
   * Creates and returns a deep copy of this Rule.
   * 
   * @return a (deep) copy of this Rule.
   */
  virtual RateRule* clone () const;


  /**
   * Accepts the given SBMLVisitor.
   *
   * @param v the SBMLVisitor instance to be used.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether the Visitor would like to visit the next RateRule object
   * in the list of rules within which @em the present object is embedded.
   */
  virtual bool accept (SBMLVisitor& v) const;


  /**
   * Predicate returning @c true if
   * all the required attributes for this RateRule object
   * have been set.
   *
   * @note In SBML Levels&nbsp;2&ndash;3, the only required attribute for a
   * RateRule object is "variable".  For Level&nbsp;1, where the equivalent
   * attribute is known by different names ("compartment", "species", or
   * "name", depending on the type of object), there is an additional
   * required attribute called "formula".
   *
   * @return @c true if the required attributes have been set, @c false
   * otherwise.
   */
  virtual bool hasRequiredAttributes() const ;


protected:
  /** @cond doxygen-libsbml-internal */

  /* this is a constructor that takes no arguments and 
   * only exists because the validator code needs it
   */
  RateRule ();


  //std::string mVariable;
  //std::string mName;

  /* the validator classes need to be friends to access the 
   * protected constructor that takes no arguments
   */
  friend class Validator;
  friend class ConsistencyValidator;
  friend class IdentifierConsistencyValidator;
  friend class InternalConsistencyValidator;
  friend class L1CompatibilityValidator;
  friend class L2v1CompatibilityValidator;
  friend class L2v2CompatibilityValidator;
  friend class L2v3CompatibilityValidator;
  friend class L2v4CompatibilityValidator;
  friend class MathMLConsistencyValidator;
  friend class ModelingPracticeValidator;
  friend class OverdeterminedValidator;
  friend class SBOConsistencyValidator;
  friend class UnitConsistencyValidator;

  /** @endcond */
};



class LIBSBML_EXTERN ListOfRules : public ListOf
{
public:

  /**
   * Creates and returns a deep copy of this ListOfRules instance.
   *
   * @return a (deep) copy of this ListOfRules.
   */
  virtual ListOfRules* clone () const;


  /**
   * Returns the libSBML type code for this %SBML object.
   * 
   * @if clike LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.
   * The set of possible type codes is defined in the enumeration
   * #SBMLTypeCode_t.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if java LibSBML attaches an
   * identifying code to every kind of SBML object.  These are known as
   * <em>SBML type codes</em>.  In other languages, the set of type codes
   * is stored in an enumeration; in the Java language interface for
   * libSBML, the type codes are defined as static integer constants in
   * interface class {@link libsbmlConstants}.  The names of the type codes
   * all begin with the characters @c SBML_. @endif
   *
   * @return the SBML type code for this object, or @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const { return SBML_LIST_OF; };


  /**
   * Returns the libSBML type code for the objects contained in this ListOf
   * (i.e., Rule objects, if the list is non-empty).
   * 
   * @if clike LibSBML attaches an identifying code to every
   * kind of SBML object.  These are known as <em>SBML type codes</em>.
   * The set of possible type codes is defined in the enumeration
   * #SBMLTypeCode_t.  The names of the type codes all begin with the
   * characters @c SBML_. @endif@if java LibSBML attaches an
   * identifying code to every kind of SBML object.  These are known as
   * <em>SBML type codes</em>.  In other languages, the set of type codes
   * is stored in an enumeration; in the Java language interface for
   * libSBML, the type codes are defined as static integer constants in
   * interface class {@link libsbmlConstants}.  The names of the type codes
   * all begin with the characters @c SBML_. @endif
   * 
   * @return the SBML type code for the objects contained in this ListOf
   * instance, or @link SBMLTypeCode_t#SBML_UNKNOWN SBML_UNKNOWN@endlink (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getItemTypeCode () const;


  /**
   * Returns the XML element name of this object.
   *
   * For ListOfRules, the XML element name is @c "listOfRules".
   * 
   * @return the name of this element, i.e., @c "listOfRules".
   */
  virtual const std::string& getElementName () const;


  /**
   * Get a Rule from the ListOfRules.
   *
   * @param n the index number of the Rule to get.
   * 
   * @return the nth Rule in this ListOfRules.
   *
   * @see size()
   */
  virtual Rule * get(unsigned int n); 


  /**
   * Get a Rule from the ListOfRules.
   *
   * @param n the index number of the Rule to get.
   * 
   * @return the nth Rule in this ListOfRules.
   *
   * @see size()
   */
  virtual const Rule * get(unsigned int n) const; 


  /**
   * Get a Rule from the ListOfRules
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the Rule to get.
   * 
   * @return Rule in this ListOfRules
   * with the given id or @c NULL if no such
   * Rule exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual Rule* get (const std::string& sid);


  /**
   * Get a Rule from the ListOfRules
   * based on its identifier.
   *
   * @param sid a string representing the identifier 
   * of the Rule to get.
   * 
   * @return Rule in this ListOfRules
   * with the given id or @c NULL if no such
   * Rule exists.
   *
   * @see get(unsigned int n)
   * @see size()
   */
  virtual const Rule* get (const std::string& sid) const;


  /**
   * Removes the nth item from this ListOfRules items and returns a pointer to
   * it.
   *
   * The caller owns the returned item and is responsible for deleting it.
   *
   * @param n the index of the item to remove
   *
   * @see size()
   */
  virtual Rule* remove (unsigned int n);


  /**
   * Removes item in this ListOfRules items with the given identifier.
   *
   * The caller owns the returned item and is responsible for deleting it.
   * If none of the items in this list have the identifier @p sid, then @c
   * NULL is returned.
   *
   * @param sid the identifier of the item to remove
   *
   * @return the item removed.  As mentioned above, the caller owns the
   * returned item.
   */
  virtual Rule* remove (const std::string& sid);


  /** @cond doxygen-libsbml-internal */

  /**
   * Get the ordinal position of this element in the containing object
   * (which in this case is the Model object).
   *
   * The ordering of elements in the XML form of %SBML is generally fixed
   * for most components in %SBML.
   *
   * @return the ordinal position of the element with respect to its
   * siblings, or @c -1 (default) to indicate the position is not significant.
   */
  virtual int getElementPosition () const;

  /** @endcond */


protected:
  /** @cond doxygen-libsbml-internal */

  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or @c NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);

  /** @endcond */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/*-----------------------------------------------------------------------------
 * See the .cpp file for the documentation of the following functions.
 *---------------------------------------------------------------------------*/

LIBSBML_EXTERN
Rule_t *
Rule_createAlgebraic (unsigned int level, unsigned int version);


LIBSBML_EXTERN
Rule_t *
Rule_createAlgebraicWithNS (SBMLNamespaces_t *sbmlns);


LIBSBML_EXTERN
Rule_t *
Rule_createAssignment (unsigned int level, unsigned int version);


LIBSBML_EXTERN
Rule_t *
Rule_createAssignmentWithNS (SBMLNamespaces_t *sbmlns);


LIBSBML_EXTERN
Rule_t *
Rule_createRate (unsigned int level, unsigned int version);


LIBSBML_EXTERN
Rule_t *
Rule_createRateWithNS (SBMLNamespaces_t *sbmlns);


LIBSBML_EXTERN
void
Rule_free (Rule_t *r);


LIBSBML_EXTERN
Rule_t *
Rule_clone (const Rule_t *r);


LIBSBML_EXTERN
const XMLNamespaces_t *
Rule_getNamespaces(Rule_t *r);


LIBSBML_EXTERN
const char *
Rule_getFormula (const Rule_t *r);


LIBSBML_EXTERN
const ASTNode_t *
Rule_getMath (const Rule_t *r);


LIBSBML_EXTERN
RuleType_t
Rule_getType (const Rule_t *r);


LIBSBML_EXTERN
const char *
Rule_getVariable (const Rule_t *r);


LIBSBML_EXTERN
const char *
Rule_getUnits (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isSetFormula (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isSetMath (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isSetVariable (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isSetUnits (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_setFormula (Rule_t *r, const char *formula);


LIBSBML_EXTERN
int
Rule_setMath (Rule_t *r, const ASTNode_t *math);


LIBSBML_EXTERN
int
Rule_setVariable (Rule_t *r, const char *sid);


LIBSBML_EXTERN
int
Rule_setUnits (Rule_t *r, const char *sname);


LIBSBML_EXTERN
int
Rule_unsetUnits (Rule_t *r);


LIBSBML_EXTERN
int
Rule_isAlgebraic (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isAssignment (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isCompartmentVolume (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isParameter (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isRate (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isScalar (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_isSpeciesConcentration (const Rule_t *r);


LIBSBML_EXTERN
SBMLTypeCode_t
Rule_getTypeCode (const Rule_t *r);


LIBSBML_EXTERN
SBMLTypeCode_t
Rule_getL1TypeCode (const Rule_t *r);


LIBSBML_EXTERN
int
Rule_setL1TypeCode (Rule_t *r, SBMLTypeCode_t L1Type);


LIBSBML_EXTERN
UnitDefinition_t * 
Rule_getDerivedUnitDefinition(Rule_t *ia);


LIBSBML_EXTERN
int 
Rule_containsUndeclaredUnits(Rule_t *ia);

LIBSBML_EXTERN
Rule_t *
ListOfRules_getById (ListOf_t *lo, const char *sid);


LIBSBML_EXTERN
Rule_t *
ListOfRules_removeById (ListOf_t *lo, const char *sid);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* !SWIG  */
#endif  /* Rule_h */
