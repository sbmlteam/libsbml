#!/usr/bin/env perl
#
# @file   translateTests.pl
# @brief  Converts C/C++ libSBML test into C#, Java, Python, and Ruby files.
# @author Akiya Jouraku
# @author Frank Bergmann (fbergman@u.washington.edu, C# additions)
# 
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright (C) 2013-2018 jointly by the following organizations:
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
#     3. University of Heidelberg, Heidelberg, Germany
#
# Copyright (C) 2009-2013 jointly by the following organizations: 
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
#  
# Copyright (C) 2006-2008 by the California Institute of Technology,
#     Pasadena, CA, USA 
#  
# Copyright (C) 2002-2005 jointly by the following organizations: 
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. Japan Science and Technology Agency, Japan
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
#----------------------------------------------------------------------- -->*/

use strict;
use Getopt::Std;

(my $myname = $0) =~ s,.*[\/\\],,;
my $usage = <<EOF;
Usage: $myname [-d output_dir] [-h] [-r|-p|-j|-c] ctest_file [ ctest_file2 ... ]
  -r : Converts given C/C++ test files into Ruby scripts (*Default*)
  -p : Converts given C/C++ test files into Python scripts 
  -j : Converts given C/C++ test files into Java files
  -c : Converts given C/C++ test files into C# files

  -o output_dir : converted files will be generated in output_dir  

  -d level : turn on debugging with given integer level (1 or 2)
  -h : print this message
EOF

my %opts;
getopts('o:hrpjcd:',\%opts) or die $usage;

my $version = "2009-08-22";

######################################################################

my %SBaseClass = (
    Compartment               => 0,
    CompartmentType           => 0,
    Constraint                => 0,
    Delay                     => 0,
    Event                     => 0,
    EventAssignment           => 0,
    FormulaUnitsData          => 0,
    FunctionDefinition        => 0,
    InitialAssignment         => 0,
    KineticLaw                => 0,
    ListOf                    => 0,
    ListOfCompartmentTypes    => 0,
    ListOfCompartments        => 0,
    ListOfConstraints         => 0,
    ListOfEventAssignments    => 0,
    ListOfEvents              => 0,
    ListOfFunctionDefinitions => 0,
    ListOfInitialAssignments  => 0,
    ListOfParameters          => 0,
    ListOfReactions           => 0,
    ListOfRules               => 0,
    ListOfSpecies             => 0,
    ListOfSpeciesReferences   => 0,
    ListOfSpeciesTypes        => 0,
    ListOfUnitDefinitions     => 0,
    ListOfUnits               => 0,
    ModifierSpeciesReference  => 0,
    Model                     => 0,
    Parameter                 => 0,
    AlgebraicRule             => 0,
    AssignmentRule            => 0,
    RateRule                  => 0,
    Rule                      => 0,
    Reaction                  => 0,
    SBase                     => 0,
    SBMLDocument              => 0,
    Species                   => 0,
    SpeciesReference          => 0,
    SpeciesType               => 0,
    StoichiometryMath         => 0,
    Trigger                   => 0,
    Unit                      => 0,
    UnitDefinition            => 0,
# Extensions
	SBMLExtension             => 0,
# Groups Package
	Group                     => 0, 
	Member                    => 0,
	GroupsExtension           => 0, 
# Layout Package
	BoundingBox				  => 0,
	Point					  => 0,
	Dimensions				  => 0,
	TextGlyph				  => 0,
	SpeciesReferenceGlyph     => 0,
	CubicBezier				  => 0,
	SpeciesGlyph			  => 0,
	LineSegment				  => 0,
	CompartmentGlyph		  => 0,
	Layout					  => 0,
	Curve					  => 0,
	ReactionGlyph			  => 0,
	GraphicalObject			  => 0, 
	ListOfLayouts			  => 0,
);

my %MiscClass = (
GroupsPkgNamespaces		  => 0,
LayoutPkgNamespaces		  => 0, 
   ASTNode             => 0,
   SBMLReader          => 0,
   SBMLWriter          => 0,
   XMLOutputStream     => 0,
   XMLInputStream      => 0,
   XMLAttributes       => 0,
   XMLNamespaces       => 0,
   XMLTriple           => 0,
   XMLNode             => 0,
   XMLToken            => 0,
   XMLError            => 0,
   SBMLError           => 0,
   CVTerm              => 0,
   ModelHistory        => 0,
   ModelCreator        => 0,
   Date                => 0,
   RDFAnnotationParser => 0,
   SBMLNamespaces      => 0,
   SyntaxChecker       => 0,
   SBMLTransforms      => 0,
);

my %IgnoredFile = (
  TestRunner                   => 0,
  TestFormulaParser            => 0,
  TestFormulaFormatter         => 0,
  TestFormulaTokenizer         => 0,
  TestSBMLTransforms           => 0,
  TestSBMLConstructorException => 0,
  TestXMLErrorLog              => 0,
  TestXMLInputStream           => 0,
  EchoHandler                  => 0,
  echoxml                      => 0,
);

my %IgnoredClass = (
  FormulaUnitsData => 0,
  XMLErrorLog      => 0,
);

my %IgnoredFunc = (
  getNumFormulaUnitsData => 0,
  addFormulaUnitsData    => 0,
  getInternalId          => 0,
  setInternalId          => 0,
);

my %IgnoreTestFunc = (
# groups package
  test_GroupsExtension_assignment				=> 0,
  test_GroupsExtension_SBMLtypecode				=> 0,
# layout package
  test_BoundingBox_assignmentOperator           => 0,
  test_CompartmentGlyph_assignmentOperator      => 0,
  test_CubicBezier_assignmentOperator           => 0,
  test_Curve_assignmentOperator                 => 0,
  test_Dimensions_assignmentOperator            => 0,
  test_GraphicalObject_assignmentOperator       => 0,
  test_Layout_assignmentOperator                => 0,
  test_LineSegment_assignmentOperator           => 0,
  test_Point_assignmentOperator                 => 0,
  test_ReactionGlyph_assignmentOperator         => 0,
  test_SpeciesGlyph_assignmentOperator          => 0,
  test_SpeciesReferenceGlyph_assignmentOperator => 0,
  test_TextGlyph_assignmentOperator             => 0,
  test_Curve_createFrom_NULL					=> 0,
# sbml core
  test_ASTNode_getListOfNodes                    => 0,
  test_ASTNode_createFromToken                   => 0,
  test_XMLOutputStream_createFile                => 0,
  test_XMLOutputStream_createFileWithProgramInfo => 0,
  test_XMLAttributes_readInto_bool               => 0,
  test_XMLAttributes_readInto_long               => 0,
  test_XMLAttributes_readInto_int                => 0,
  test_XMLAttributes_readInto_double             => 0,
  test_XMLAttributes_create_C                    => 0,
  test_XMLAttributes_readInto_uint_C             => 0,
  test_XMLTriple_comparison                      => 0,
  test_XMLAttributes_add_removeResource          => 0,
  test_XMLAttributes_readInto_boolean_C          => 0,
  test_XMLAttributes_readInto_double_C           => 0,
  test_XMLAttributes_readInto_long_C             => 0,
  test_XMLAttributes_readInto_int_C              => 0,
  test_XMLAttributes_readInto_string_C           => 0,
  test_XMLErrorLog_create                        => 0,
  test_XMLErrorLog_add                           => 0,
  test_XMLErrorLog_clear                         => 0,
  test_SBML_formulaToString                      => 0,
  test_element_semantics                         => 0,
  test_element_semantics_URL                     => 0,
  test_element_semantics_URL_lambda              => 0,
  test_element_semantics_ann_lambda              => 0,
  test_element_semantics_annotation              => 0,
  test_element_semantics_annxml                  => 0,
  test_element_semantics_annxml_lambda           => 0,
  test_element_semantics_lambda                  => 0,
  test_MathMLFormatter_semantics                 => 0,
  test_MathMLFormatter_semantics_ann             => 0,
  test_MathMLFormatter_semantics_annxml          => 0,
  test_MathMLFormatter_semantics_url             => 0,
  test_MathMLFormatter_ci_definitionURL          => 0,
  test_ASTNode_replaceArgument                   => 0,
  test_SBase_addCVTerms_newBag                   => 0,
  test_Date_setHoursOffset_neg_arg               => 0,
  test_Date_setOffsetSign                        => 0,
  test_Model_copyConstructor                     => 0,
  test_Model_assignmentOperator                  => 0,
  test_Model_clone                               => 0,
  test_Model_add_get_FunctionDefinitions_neg_arg => 0,
  test_Model_add_get_UnitDefinitions_neg_arg     => 0,
  test_Model_add_get_Event_neg_arg               => 0,
  test_Reaction_removeModifier                   => 0,
  test_Reaction_getModifier                      => 0,
  test_Reaction_addModifier                      => 0,
  test_Reaction_getModifierById                  => 0,
  test_SpeciesReference_createModifier           => 0,
  test_Reaction_addModifier1                     => 0,
  test_Reaction_addModifier2                     => 0,
  test_Reaction_addModifier3                     => 0,
  test_Reaction_createModifier                   => 0,
  test_Reaction_removeModifier                   => 0,
  test_RDFAnnotation_testHasRDFAnnotation        => 0,
  test_RDFAnnotation_testHasAdditionalRDFAnnotation => 0,
  test_RDFAnnotation_testHasCVTermRDFAnnotation  => 0,
  test_RDFAnnotation_testHasHistoryRDFAnnotation => 0,
  test_RDFAnnotation_testHasCVTermRDFAnnotationBadAbout => 0,
  test_RDFAnnotation_testHasHistoryRDFAnnotationBadAbout => 0,
  test_RDFAnnotation_testCreateAnnotations       => 0,
  test_RDFAnnotation_deleteCVTerms               => 0,
);

######################################################################
# Language dependent variables
######################################################################

my %ModuleName = (
  ruby   => 'libSBML',
  python => 'libsbml',
  java   => 'libsbml',
  csharp => 'libsbmlcs',
);

my %Prefix = (
  ruby   => 'LibSBML::',
  python => 'libsbml.',
  java   => 'libsbml.',
  csharp => 'libsbml.',
);

my %IdNULL = (
  ruby   => 'nil',
  python => 'None',
  java   => 'null',
  csharp => 'null',
);

my %IdNULLChar = (
  ruby   => '"\\0"',
  python => '"\\0"',
  java   => "'\\0'",
  csharp => "'\\0'",
);

my %IdTRUE = (
  ruby   => 'true',
  python => 'True',
  java   => 'true',
  csharp => 'true',
);

my %IdFALSE = (
  ruby   => 'false',
  python => 'False',
  java   => 'false',
  csharp => 'false',
);

my %IdBOOL = (
  java   => 'boolean',
  csharp => 'bool',
);

my %IdCOUT = (
  ruby   => 'LibSBML::cout',
  python => 'libsbml.cout',
  java   => 'libsbml.cout',
  csharp => 'libsbml.cout',
);

my %IdOSS = (
  ruby   => 'oss = LibSBML::Ostringstream.new',
  python => 'oss = libsbml.ostringstream()',
  java   => 'OStringStream oss = new OStringStream();',
  csharp => 'OStringStream oss = new OStringStream();',
);

my %IdSTRING = (
  java   => 'String',
  csharp => 'string',
);

my %constDBL_EPSILON = (
  ruby   => "\@\@DBL_EPSILON",
  python => "DBL_EPSILON",
  java   => "DBL_EPSILON",
  csharp => "DBL_EPSILON",
);

my %constSBML_INT_MAX = (
  ruby   => "\@\@SBML_INT_MAX",
  python => "SBML_INT_MAX",
  java   => "SBML_INT_MAX",
  csharp => "SBML_INT_MAX",
);


my $JavaPackage = "org.sbml.libsbml.test";
my $CSNamespace = "LibSBMLCSTest";

###################################################################

my $TestDataDirectory = "../../sbml/test/test-data/";

my $Target = 'ruby';
my $OutputDir = '.';

my %patchGlobal;
my %patchClassTop;
my %patchFuncHead;
my %patchFuncTail;
my %patchFuncReplace;

my %FuncDef;
my %MacroDef;
my %FileProp;
my %GlobalVariable;
my %LocalVariable;
my %TmpLocalVariable;

my %pStatus;
my $CurFunc;
my $FlagIfdef;
my $CurLine;
my $IsMultiLine;
my $CurTestDir;

my $Debug = 0;

###################################################################
# reg-exp
###################################################################
#
# regular expression for nesting parenthesis
# (reference : Programming Perl 3rd edition)
#
my $re_np;       
$re_np = qr{
               \(
               (?:
                   (?> [^()]+ )
               |
                   (??{ $re_np })
               )*
               \)
           }x;

###################################################################
# main routine
###################################################################

$opts{'h'} and die $usage;
$opts{'p'} and $Target    = 'python';
$opts{'r'} and $Target    = 'ruby';
$opts{'j'} and $Target    = 'java';
$opts{'c'} and $Target    = 'csharp';
$opts{'o'} and $OutputDir = $opts{'o'};

if ( $opts{'d'} =~ /\d/ )
{
  $Debug = $opts{'d'};
}
elsif ( $opts{'d'} )
{
  print "Error: the -d option requires a numerical argument.\n";
  die $usage;
}

&reset();
&initPatch();

INFILES: for my $file (@ARGV)
{
  foreach ( keys %IgnoredFile )
  {
    if ( $file =~ /$_/ )
    {
      print "(ignored) $file\n" ;
      next INFILES;
    }
  }

  if ( $file =~ m| (\w+)/test/ |x )
  {
    $TestDataDirectory = "../../sbml/$1/test/test-data/";
    $CurTestDir = $1;
  }
  print "\nparsing $file\n\n" if $Debug;
  &parse($file);
  &writeCode($file);
  &reset();
}

###################################################################
# sub routines
###################################################################

sub reset
{
  %FuncDef   = undef;
  %MacroDef  = undef;
  %FileProp  = undef;
  %pStatus   = undef;
  %GlobalVariable = undef;
  %LocalVariable  = undef;
  %TmpLocalVariable  = undef;
  $CurFunc     = "";
  $CurLine     = "";
  $FlagIfdef   = 0;
  $IsMultiLine = 0;
  $CurTestDir  = 0;
}

sub getCreateObjString
{
  my ($cname, $arg) = @_;

  my $fcall = "";

  my $useXMLNS = 0;
  my $levelversion = 0;

  # skips the third argument in SBMLDocument(level,version,xmlns)
  if ( defined($SBaseClass{$cname}) )
  {
    my @args = split(",", $arg);
    if ( scalar(@args) == 3  )
    {
      if ( $args[2] eq "NULL" )
      {
        pop @args if scalar(@args) == 3; 
        $arg = join(",",@args);
      }
      else
      {
        $useXMLNS = $args[2]; 
        $levelversion = "$args[0],$args[1]";
      }
    }

  }
     
  ######################################################################
  # Ruby 
  ######################################################################
  if ( $Target eq 'ruby' )
  {
    if ($useXMLNS)
    {
      push (@{$FuncDef{$CurFunc}}, "sbmlns = LibSBML::SBMLNamespaces.new($levelversion)");
      push (@{$FuncDef{$CurFunc}}, "sbmlns.addNamespaces($useXMLNS)");
      $arg = "sbmlns";
    }

    $fcall = "$Prefix{$Target}" . $cname . ".new";
  }
  ######################################################################
  # Python
  ######################################################################
  if ( $Target eq 'python' )
  {
    if ($useXMLNS)
    {
      push (@{$FuncDef{$CurFunc}}, "sbmlns = libsbml.SBMLNamespaces($levelversion)");
      push (@{$FuncDef{$CurFunc}}, "sbmlns.addNamespaces($useXMLNS)");
      $arg = "sbmlns";
    }

    $fcall = "$Prefix{$Target}" . $cname;
  }
  ######################################################################
  # Java/C# 
  ######################################################################
  if ( ($Target eq 'java') || ($Target eq 'csharp') )
  {
    if ($useXMLNS)
    {
      unless ( $TmpLocalVariable{$CurFunc}{"sbmlns"} )
      {
        push (@{$FuncDef{$CurFunc}}, "SBMLNamespaces sbmlns = null;");
        $TmpLocalVariable{$CurFunc}{"sbmlns"} = 1; 
      }
      push (@{$FuncDef{$CurFunc}}, "sbmlns = new SBMLNamespaces($levelversion);");
      push (@{$FuncDef{$CurFunc}}, "sbmlns.addNamespaces($useXMLNS);");
      $arg = "sbmlns";
    }

    $fcall = "new  $cname";
  }

  
  $fcall .= "(" . $arg . ")";

  return $fcall; 
}


sub getCreateFileString
{
  my ($file) = @_;

  my $fcall = "";
     
  ######################################################################
  # Ruby 
  ######################################################################
  if ( $Target eq 'ruby' )
  {
    $fcall = "$Prefix{$Target}" . "ostream.new";
  }
  ######################################################################
  # Python
  ######################################################################
  if ( $Target eq 'python' )
  {
    $fcall = "$Prefix{$Target}" . "createOFStream";
  }
  ######################################################################
  # Java/C# 
  ######################################################################
  if ( ($Target eq 'java') || ($Target eq 'csharp') )
  {
      $fcall = "new  OFStream";
  }
  
  $fcall .= "(" . $file . ")";

  return $fcall; 
}


sub parse
{
  my ($file) = @_;

  (my $file_name = $file) =~ s|.*/||;

  open (FH, $file) or die "Can't open $file : $!/$?";
  while (<FH>)
  {
    chomp;
    my $line = $_;
    $CurLine = $line;
    $IsMultiLine = 0;

    print "$CurLine\n" if $Debug > 1;

    $line =~ s| /\* .*? \*/ ||xg;

    #
    # end of comments 
    #
    if ($pStatus{'inComment'})
    { 
      my $code = "";

      #
      # coments in header
      #
      if ( $line =~ /^ \s* \** \s* [\@\\]brief \s+ (.+) $/x )
      {
        $FileProp{$file_name}{'brief'} = $1;
      }
      elsif ( $line =~ /^ \s* \** \s* [\@\\]author \s+ (.+) $/x )
      {
        push ( @{ $FileProp{$file_name}{'author'} } , $1 );
      }

      if($line =~ m|\*/(.*)|)      
      {
        $pStatus{'inComment'} = 0;
        $code = $1; 
      }
      else
      {
        next;
      }

      next unless $code;
      $line = $code;
    }

    #
    # start of comments
    #
#    if ($line =~ m|((.*)/\*|)
    if ($line =~ m|^ \s* /\* |x)
    {
      $pStatus{'inComment'} = 1 unless ($line =~ m|\*/|);
      next;
    }
    # SHOULD BE FIXED
    elsif ($line =~ m|(.*?) (?<!http:) //|x)
    {
      my $substr = $1;
      next unless $substr;
      next if ($line =~ m|^ \s* //|x );
#print "[CPP Comment] -> $line\n";
      if ( $line !~ m| ([\"\']) [^\"\']* // [^\"\']* [\"\'] |x )
      {
        $line = $substr;
      }
    }

    #
    # end of #if... macro
    #
    if ($pStatus{'inPPIF'})
    {
      if ( $line =~ / \# \s* endif /x )
      {
        $pStatus{'inPPIF'} = 0;
      }
      next;
    }

    #
    # start of #if... macro
    #
    if ( $line =~ / \# \s* if /x )
    {
      $pStatus{'inPPIF'} = 1; 
      next;
    }

    #
    # parsing a setup function 
    #
    if ($pStatus{'inSETUP'})
    {
      if ($line =~ /^}/)
      {
        &parseMisc($line);        
        $pStatus{'inSETUP'} = 0;
      }
      else
      {
        next if ($line =~ /^\s*$/);
        print "[call parseMisc] -> $line\n" if $Debug;
        &parseMisc($line);        
      }
    }
    #
    # parsing a setup function 
    #
    elsif ($pStatus{'inTEARDOWN'})
    {
      if ($line =~ /^}/)
      {
        &parseMisc($line);        
        $pStatus{'inTEARDOWN'} = 0;
      }
      else
      {
        next if ($line =~ /^\s*$/);
        print "[call parseMisc] -> $line\n" if $Debug;
        &parseMisc($line);        
      }
    }
    #
    # parsing a test function
    #
    elsif ($pStatus{'inTEST'})
    {

      # ignore SBase_getCVTerms
      #next if ( $line =~ /SBase_getCVTerms/ );


      if ($line =~ /^ \s* END_TEST /x)
      {
        $pStatus{'inTEST'} = 0;
      }
      elsif ($line =~ /^\s* fail_(?: if | unless) \s* ($re_np) \s*;+\s* $/x )
      {
        my $block = $1;
        $block =~ s/^\s*\(//;
        $block =~ s/\)\s*$//;

	# remove ',NULL' from the tail
        $block =~ s/ , \s* NULL \s* $//x;

        print "[call parseAssertion] -> $block\n" if $Debug;
#print "[call parseAssertion] -> $block\n";
        &parseAssertion($block);
      }
      elsif ($line =~ /^ \s* fail_(?: if | unless) \s* \( ( [^;]+ ) /x )
      {
	  #
	  # read a next line
	  #
          my $block = $1;
# SHOULD BE FIXED
          $block .= <FH>; 
          chomp $block;
	  # remove ');' from the tail
          $block =~ s/ \) \s* ;+ \s* $//x;
	  # remove ',NULL' (if any) from the tail
          $block =~ s/, \s* NULL \s* $//x;
          $line = $block;
#print "[misc] -> $block\n";
          &parseAssertion($block);
      }
      elsif ($line =~ /^\s *fail \s* \( [^;]+ \s* $/x )
      {
          my $block = $line;

          # SHOULD BE FIXED
          $block .= <FH>; 
          chomp $block;
          $line = $block;

          &parseMisc($line);
      }
      else
      {
#print "[call parseMisc] -> $line\n";

        next if ($line =~ /^\s*$/);

        print "[call parseMisc] -> $line\n" if $Debug;

        my $block = $line;

        # SHOULD BE FIXED
        # Merge multiple lines (without ';$') into single line.
        if ( $line !~ /^ \s* ( [\#\{\}] | (if | else \s* if | else) ) /x )
        {
          BLOCK: 
          while( $line !~ /\;+ \s* $/x )
          {
            $line = <FH>;
            chomp $line;
            $line =~ s/^\s*//;

#print "[call parseMisc (while)] $line \n";

            if ( $line =~ /\;+ \s* $/x )
            {
              #$block =~ s/\+ (\s*?) $/$1/x;
              if ( $line !~ /^ \s* " (?:[^"]|\\")*? (?<!\\) "/x 
                && $line !~ /^ \s* ' (?:[^']|\\')*? (?<!\\) '/x 
              )
              {
                $block =~ s/\+ (\s*) \z/$1/x;
              }
              elsif ( ( $line !~ /\s+ = \s+/x )  && ( $block !~ / \+ \s* $/x ) )
              {
                $line = " + " . $line; 
              }

              chomp $block;
              $block .= $line;
              last BLOCK;
            }
            elsif ( $line =~ /^ \s* " (?:[^"]|\\")*? (?<!\\) " \s* $/x )
            {
               $line = $line . " + " . "\n    ";
            } 
            elsif ( $line =~ /^ \s* ' (?:[^']|\\')*? (?<!\\) ' \s* $/x )
            {
               $line = $line . " + " . "\n    "; 
            } 

            $block .= $line;
            $IsMultiLine = 1;
           }
#print "[call parseMisc (multiple)] $line -> $block\n";
         }
         &parseMisc($block);        
      }
    }
    else
    {
      #
      # start of a setup function found
      #
      if ($line =~ /_setup \w? \s* \(.*\)/x)
      {
        $pStatus{'inSETUP'}  = 1;
	##################################################
	# Ruby
	##################################################
	if ( $Target eq 'ruby' )
	{
          $CurFunc = "setup";
	}
	##################################################
	# Python
	##################################################
	elsif ( $Target eq 'python' )
	{
          $CurFunc = "setUp";
	}
	##################################################
	# Java
	##################################################
	elsif ( $Target eq 'java' )
	{
          $CurFunc = "protected void setUp() throws Exception";
	}
	##################################################
	# C#
	##################################################
	elsif ( $Target eq 'csharp' )
	{
          $CurFunc = "setUp";
	}

      }
      #
      # start of teardown function found
      #
      elsif ($line =~ /_teardown \w? \s* \(.*\)/x)
      {
        $pStatus{'inTEARDOWN'}  = 1;
	##################################################
	# Ruby
	##################################################
	if ( $Target eq 'ruby' )
	{
          $CurFunc = "teardown";
	}
	##################################################
	# Python
	##################################################
	elsif ( $Target eq 'python' )
	{
          $CurFunc = "tearDown";
	}
	##################################################
	# Java
	##################################################
	elsif ( $Target eq 'java' )
	{
          $CurFunc = "protected void tearDown() throws Exception";
	}
	##################################################
	# C#
	##################################################
	elsif ( $Target eq 'csharp' )
	{
          $CurFunc = "tearDown";
	}

      }
      #
      # start of a test function
      #
      elsif ($line =~ /^ \s* START_TEST \s* \( \s* (\w+) \s* \)/x )
      {
        $pStatus{'inTEST'}  = 1;
        $CurFunc = $1;
      }
      #
      # macro definition 
      #
      elsif ($line =~ /^\s* \#define/x )
      {
        &parseMisc($line);        
      }
      #
      # global variable
      #
      elsif ( $line =~ /^ (?: static \s* )? (?: (?: const | unsigned ) \s*)?  \s* ( \w+ ) \s* \*?  \s* (\w+) \s*\;+\s* $/x  )
      {
        my $type = $1;
        my $var  = $2;
        next if $1 =~ /return/;

	print "[global variable] $2\n" if $Debug;
	
        ################################################## 
        # Ruby/Python
        ################################################## 
        if ( ( $Target eq 'ruby' ) || ( $Target eq 'python') )
        {
	  $GlobalVariable{$var} = 1;
        }
        ################################################## 
        # Java / C#
        ################################################## 
        elsif ( ( $Target eq 'java' ) || ( $Target eq 'csharp') ){
    
#print "[global  variable] $CurLine\n";
          if ( $CurLine =~ /^ \s* \w*? \s* SpeciesReference .* (?: _createModifier | MSR )/x )
          {
            $type =~ s/^\s*  (\w*?)  \s* SpeciesReference_t/$1 SimpleSpeciesReference/x;
          }
          $type =~ s/_t$//;
          $type =~ s/^char$/$IdSTRING{$Target}/x;
          $type =~ s/^ostringstream$/OStringStream/;
	  $GlobalVariable{"$type $var"} = 1;
#print "[global  variable] $type\n";
        }
      }
      else{
#print "[ignored variable] $line\n";
      }
    }
  }
  close(FH);
}

sub parseAssertion
{
  my ($line) = @_;

  print "[parseAssertion(top)] -> $line\n" if $Debug;

  $line =~ s/ numeric_limits \s* < \s* double \s* > \s* ::infinity \s* $re_np /util_PosInf\(\)/x ;

  if ( $line =~ /^ \s* 
                     ( (?: (?: (?<!\\) " (?:[^"]|\\")*? (?<!\\) " |  (?<!\\) ' (?:[^']|\\')*? (?<!\\) ') | [^"'] )* )  
                     (?<=\w|["')]|\s) ( == | < | > | != | >= | <= ) (?=\w|[("'*]|\s) 
                     ( (?: (?: (?<!\\) " (?:[^"]|\\")*? (?<!\\) " |  (?<!\\) ' (?:[^']|\\')*? (?<!\\) ') | [^"'] )* )  
                   \s* $
                   /x 
    )
  {
    print "[parseAssertion(before)] $1 $2 $3\n" if $Debug;

    my $b1 = $1;
    my $b2 = $2;
    my $b3 = $3; 

    print "b1 = $b1\n" if $Debug > 2;
    print "b2 = $b2\n" if $Debug > 2;
    print "b3 = $b3\n" if $Debug > 2;

    my @block  = &parseBlock($b1); 
    my $left = join('',@block);

    my $op = $b2;

    @block = &parseBlock($b3); 
    my $right = join('',@block);

    print "b3 after parseBlock = $right\n" if $Debug > 2;

    #
    # Ignored functions
    #
    if ( ($left eq "") or ($right eq "") )
    {
      {
        print "(IGNORED) [parseAssertion] $left $op $right \n" if $Debug > 2;
        return;
      }
    }

    if ( ($left =~ /copy\.getName\(\)/ ) or ($right =~ /copy\.getName\(\)/) )
    {
      {
        print "(IGNORED) [parseAssertion] $left $op $right \n" if $Debug > 2;
        return;
      }
    }

    my $prexp = "";

    ######################################################################
    # Java/C#
    ######################################################################
    if ( ($Target eq 'java') || ($Target eq 'csharp') )
    {
      $prexp = qr{ \s* $re_np \s* $ }x; 
    }   

    if ( $b1 =~ /strcmp/ )
    {
      $right = $IdFALSE{$Target} if $right eq '0';
      $right = $IdTRUE{$Target}  if $right eq '1';
    }
    elsif ( $left =~ /
                  (?:
                  
                   (?:
                      isSet(
                            ModelHistory
                           |Id
                           |Formula
                           |InitialAmount
                           |InitialConcentration
                           |Message
                           |KineticLaw
                           |Annotation
                           |Notes
                           |Value
                           |Charge
                           |Math
                           |Constant
                           |Fast
                           |Reversible
                           |UseValuesFromTriggerTime
                           |BoundaryCondition
                           |HasOnlySubstanceUnits
                           |Units
                        )
                     )

                     |

                     (?:
                        setLevelAndVersion
                     ) 

                     |

                     (?:
                        get(
                            Fast
                           |Reversible
                           |BoundaryCondition
                           |Constant
                           |HasOnlySubstanceUnits
                           |ContainsUndeclaredUnits
                           |CanIgnoreUndeclaredUnits
                           |UseValuesFromTriggerTime
                         )
                     ) 

                     |

                     (?:
                        containsUndeclaredUnits
                     ) 

                     |

                     (?:
                         is( 
                             Sqrt 
                            |Log10 
                            |UMinus 
                            |Empty
                            |End
                            |EOF 
                            |Info
                            |Warning
                            |Error
                            |Fatal
                            |Good
                            |AttributesEmpty
                            |NamespacesEmpty
                            |Element
                            |Text
                            |Start
                            |SetFamilyName
                            |SetGivenName
                            |SetEmail
                            |SetOrganisation
                            |SetOrganization
                            |SetCreatedDate
                            |SetModifiedDate
                          )
                     ) 

                     |

                     (?:
                        has( Attribute | URI | NS | Prefix | Attr | Namespace(?:URI|NS|Prefix) )
                     ) 

                     |

                     (?:
                        util_isInf
                     ) 

                   ) $prexp
                  /x 
    )
    {
      $right = $IdFALSE{$Target} if $right eq '0';
      $right = $IdTRUE{$Target}  if $right eq '1';
    }
    elsif (  $CurLine =~ /ASTNode_getCharacter \s* $re_np /x
          && $left =~ /getCharacter /x )
    {
      $right =~ s/^\s+//;
      $right =~ s/\s+$//;
      $right = $IdNULLChar{$Target} if ( $right eq '0' || $right eq "'\\0'" );
    }
    elsif ( $left =~ / (?: getModel | getNamespaces ) \s* $re_np /x )
    {
      $right = $IdNULL{$Target} if $right eq '0';
    }
    elsif (  $CurLine =~ /ASTNode_getName \s* $re_np /x 
          && $left =~ /getName /x )
    {
      $right = $IdNULL{$Target} if ( $right eq "" || $right eq '0' );
    }
    elsif ( $CurLine !~ /Constraint_getMessage/ &&
            $left =~ /get(
                          MetaId
                          |Id
                          |Name
                          |Formula
                          |Variable
                          |TimeUnits
                          |Units
                          |Outside
                          |Symbol
                          |SubstanceUnits
                          |Species
                          |Compartment
                          |SpatialSizeUnits
                          |URI
                          |Prefix
                          |Index
                          |AttrValue
                          |AttrPrefix
                          |AttrURI
                          |NamespaceIndex
                          |Message
                          |SeverityAsString
                          |CategoryAsString
                          |SBOTermID
                          |ConversionFactor
                          |VolumeUnits
                          |AreaUnits
                          |LengthUnits
                        ) $prexp
                     /x 
    )
    {

      if ($right eq $IdNULL{$Target} )
      {
        if ( $line !~ /Model_get(Species|Compartment)/x )
         {
           ######################################################################
           # Python/Ruby/C#
           ######################################################################
           if ( ($Target eq 'python') || ($Target eq 'ruby') || ($Target eq 'csharp') )
           {
             $right = '""';
           }
           ######################################################################
           # Java
           ######################################################################
           elsif ( $Target eq 'java' )
           { 
             $right = "true";
             $left .= '.equals("")';
           }
         }
       }
       ######################################################################
       # Java
       ######################################################################
       elsif ( $Target eq 'java' )
       {
         unless (  $right =~ /^ \s* (?: 
                                    \-?(\d*\.)?\d+  
                                  | 
                                    libsbml\..+ 
                                  |
                                    true
                                  |
                                    false
                                  |
                                    null
                                  |
                                    '.*'
                                  |
                                    ".*"
                                 ) \s* $/x 
                 )
          {
           $left .= ".equals(${right})";
           $right = "true";
         }
       }

    }
    ######################################################################
    # Python 
    ######################################################################
    elsif ( ($Target eq 'python') 
             && 
            $left =~ / get(?: Event | FunctionDefinition | UnitDefinition ) /x )
    {
      # In Python, 'int' can't be passed to where 'unsigned int' required
      $left =~ s/ ( \- [0-9]+ ) \s* \) \s*$/99999)/x  ;
    }
    ######################################################################
    # Java/C#
    ######################################################################
    elsif ( ( ($Target eq 'java') || ($Target eq 'csharp') )
            && 
           $left =~ / get(\w+) $prexp /x )
    {
      my $cname = $1;
      $cname =~ s/By.*?//;
      if (   defined($SBaseClass{$cname}) || ($cname eq 'Modifier') 
          || ($cname eq 'Product') || ($cname eq 'Reactant'))
      {
        my $fcall;
        if ($op eq '==')
        {
          $fcall = "assertEquals(";
        }
        elsif ($op eq '!=')
        {
          $fcall = "assertNotEquals(";
        }
        $fcall .= "$left,$right);";
        push (@{$FuncDef{$CurFunc}}, $fcall);
        return;
      }
    }

    ######################################################################

    print "[parseAssertion(after)] -> $left $op $right\n" if $Debug;
    push (@{$FuncDef{$CurFunc}}, &addAssertion2($left,$right,$op));

  }
  elsif ($line =~ /^ \s* !? \s* strcmp/x )
  {
#     push (@{$FuncDef{$CurFunc}}, &parseBlock($line));
     ################################################## 
     # Ruby
     ################################################## 
     if ( $Target eq 'ruby') 
     {
       push (@{$FuncDef{$CurFunc}}, "assert (" . &parseBlock($line) . ")");
     }
     ################################################## 
     # Python
     ################################################## 
     elsif ( $Target eq 'python') 
     {
       push (@{$FuncDef{$CurFunc}}, "self.assertTrue(" . &parseBlock($line) . ")");
     }
     ################################################## 
     # Java / C#
     ################################################## 
     elsif  ( ( $Target eq 'java') || ( $Target eq 'csharp') )
     {
       push (@{$FuncDef{$CurFunc}}, "assertTrue(" . &parseBlock($line) . ");");
     }
     ################################################## 
  }
  else
  {
#print "[In parseAssertion(three before)] -> $line\n";
    my $bool_flag = 1;

    $bool_flag = 0 if ($line =~ s/^ \s* !//x);

    my $one = &parseBlock($line);

    push (@{$FuncDef{$CurFunc}}, &addAssertion1($one,$bool_flag));
  }

}

sub parseMisc
{
  my ($line) = @_;

  print "[parseMisc (before)] $line\n" if $Debug > 1;

# SHOULD BE FIXED
#  return if ($line =~ /_free\(.+\)/);

  my @block = &parseBlock($line);
  my $block = join('',@block);

  print "[parseMisc (after) ] $line -> $block\n" if $Debug > 1;

  next if $block =~ /^\s*$/;

  # variable definition only
  return if ($block =~ /^[_a-zA-Z0-9]+$/);

  if ( $block =~ /^ \s* \#ifdef \s* ( USE_(?: LIBXML | EXPAT | XERCES) )/x )
  {
    my $val = $1;
    ######################################################################
    # Ruby
    ######################################################################
    if ( $Target eq 'ruby' )
    {
      $block = "if ( \@\@$val == 1 )";  
    }
    ######################################################################
    # Python
    ######################################################################
    elsif ( $Target eq 'python' )
    {
      $block = "if  $val == 1 ";  
    }
    ######################################################################
    # Java / C#
    ######################################################################
    elsif ( ( $Target eq 'java' ) || ( $Target eq 'csharp' ) )
    {
      $block = "if ( $val == 1 )";  
    }

    push (@{$FuncDef{$CurFunc}}, $block);
    $block = "{";

    $FlagIfdef = 1;
  }
  elsif ( $block =~ /^ \s* \#endif /x )
  {
    if ( $FlagIfdef ){
      $block = "}";
      $FlagIfdef = 0;
    }
  }
  else
  {
    ######################################################################
    # Java / C#
    ######################################################################
    if ( ( $Target eq 'java' ) || ( $Target eq 'csharp' ) )
    {
      if ($block !~ /\s* \;+ \s*$/x )
      {
        $block .= ";" unless $block =~ /[{}]\s*$/x;
      }
    }
  }


#print "[parseMisc ] $line -> $block\n";

  push (@{$FuncDef{$CurFunc}}, $block);
}


sub parseBlock
{
  my ($line) = @_;

  return "" if ( $line =~ /^ \s* for \s* \(/x );

  print "[parseBlock (top)] $line\n" if $Debug > 1;

  ###################################################################### 

  if ( ( $Target eq 'python' ) || ($Target eq 'ruby') )
  {
    # remove static_cast<...>( ) and const_cast<...>()
    $line =~ s{ 
                (?:static|const|dynamic)_cast\s*< \s* \w+? \s* \*? \s*> \s* ($re_np) 
              }
              { 
                (my $r = $1)  =~ s,(?:^\(|\)$),,g;  
                $r;
              }xe; 

    # remove cast 
    $line =~ s/^\s* \(\s* [a-zA-Z_]+ \s* \* \s* \) \s*//x; 
  }
  else
  {
   # SHOULD BE FIXED

    my $innertype;
    $line =~ s{ 
                (?:static|const|dynamic)_cast\s*< \s* ( \w+? ) \s* \*? \s*> \s* ($re_np) 
              }
              { 
		$innertype = $1;
		(my $r = $2) =~ s,(?:^\(|\)$),,g;
                "(($innertype) $r)";
              }xe; 
    if ( $Target eq 'java')
    {
      $line =~ s/->clone \s* \(/.cloneObject\(/x;
    }
    $line =~ s/-> \s* (\w)/.$1/x;
    # remove cast 
    $line =~ s/^\s* \(\s* [a-zA-Z_]+ \s* \* \s* \) \s*//x; 

    if ( $line =~ /setValue/ and $innertype =~ /long/ )
    {
      # This should be handled in a better way, but I can't come up with
      # one now.  Calls to ASTNode setValue, if the first argument is
      # cast to long (instead of int), will be mapped to the wrong
      # setValue() variant by Java.

      $line =~ s/long/int/;
    }
  }

  if ( $Target eq 'csharp' ) 
  {
    $line =~ s/\b(object)\b/object1/x;
  }

  # remove 'std::'
  $line =~ s/^\s* std::nothrow//x; 
  $line =~ s/^\s* std:://x; 

  # util_PosInf(), util_NegInf(), util_NaN()
  return $line if ( $line =~ /^ \s* util_(PosInf|NegInf|NaN) \s* \( \s* \) \s* $/x );

  $line =~ s/ numeric_limits \s* < \s* double \s* > \s* :: \s* infinity \s* $re_np /util_PosInf\(\)/x ;
  $line =~ s/ numeric_limits \s* < \s* double \s* > \s* :: \s* quiet_NaN \s* $re_np /util_NaN\(\)/x ;

  # string  "..."
  return $line if ($line =~ /^ \s* " (?:[^"]|\\")*? (?<!\\) " \s* ;* \s* $/x);
  # string  '...' 
  return $line if ($line =~ /^ \s* ' (?:[^']|\\')*? (?<!\\) ' \s* ;* \s* $/x);

  # ModelHistory::getListCreators()->get(...) -> ModelHistory::getCreator(..)
  $line =~ s/ getListCreators\(\)->get /getCreator/gx;

  # "(...);" -> "..."
  $line =~ s/^ \s* \( (.*) \) \s* \; \s* $/$1/x;

  ###############################################################################
  #
  # macro definition
  #
  # (Pattern 1)  #define DEFINITION
  #
  ###############################################################################
  if ( $line =~ /^ \s* \#define \s* (.*) /x )
  {
    print "parseBlock(pattern 1) $line -> $1\n" if $Debug > 2;

    my $macro_def = &convertMacroDefine($1);

    return $macro_def;
  }
  ###############################################################################
  #
  # if/else if
  #
  # (Pattern 2)  if (...) | if (...) { | else if (...) | else if (...) {
  #
  ###############################################################################
  elsif( $line =~ /^ \s* (if|else if) \s* ($re_np) \s* ( {? ) \s* $/x )
  {
    print "parseBlock(pattern 2) $line -> $1 $2 $3\n" if $Debug > 2;

    my $b1 = $1;  
    my $b2 = $2;
    my $b3 = $3;

    $b2 =~ s/^\(//;
    $b2 =~ s/\)$//;

    $b2 = &parseBlock($b2);
    
    # SHOULD BE FIXED

    return $b1 . " (" . $b2 . ")" . $b3; 
  }
  ###############################################################################
  #
  # operators 
  #
  # (Pattern 3) LEFTBLOCK  (= | == | != | < | > | >= | <= ) RIGHTBLOCK
  #
  ###############################################################################
  elsif ( $line =~ /^ \s* 
                         ( (?: (?: (?<!\\) " (?:[^"]|\\")*? (?<!\\) " |  (?<!\\) ' (?:[^']|\\')*? (?<!\\) ') | [^"'] )* )  
                         (?<=\w|["')]|\s) ( = | == | < | > | != | >= | <= ) (?=\w|[("'*]|\s) 
                         ( (?: (?: (?<!\\) " (?:[^"]|\\")*? (?<!\\) " |  (?<!\\) ' (?:[^']|\\')*? (?<!\\) ') | [^"'] )* )  
                       \s* $
                   /x 
         )
  {
   my $b1 = $1;
   my $c1 = $2;
   my $b2 = $3;

   print "[parseBlock (pattern 3)] $b1 || $c1 || $b2\n" if $Debug > 2;

   print "[parseBlock left ...]\n" if $Debug > 2;
   my $left  = &parseBlock($b1, 1);

   print "[parseBlock right ...]\n" if $Debug > 2;
   my $right = &parseBlock($b2, 1);

   print "[parseBlock (operator)] $left || $right\n" if $Debug > 2;

   if ( ($left eq "") or ($right eq "") )
   {
     print "(IGNORED) [parseBlock (operator)] $b1 $c1 $b2 (left) $left (right) $right \n" if $Debug > 2;
     return "";
   }

   print "$b2 -> $right\n" if $b2 =~ /X0/ and $Debug > 2;
   print "$b2 -> $right\n" if $b2 =~ /wrap/ and $Debug > 2;

    if ( ( $Target eq 'java' ) || ($Target eq 'csharp'))
     {
       $right .= ";" if $line =~ /^ \s* \;+ \s* $/x; 
     }


    if ( $IsMultiLine )
    {
      if ($right =~ /^ \s* [\"\'] .+ [\"\'] \s* \;* \s* $/xs )  
      {
	print "[RIGHT] $right\n" if $Debug > 2;
        $right =~ s| \\n \" \" |\\n" + "|xs;
        if ( $Target eq 'python' ) 
        {
          $right =~ s/\;* \s* $//sx;
          $right = "wrapString(" . $right . ")";
        }
      }
    }
  

    return $left . " $c1 " . $right;
  }
  ###############################################################################
  #
  # SBASE_FUNC (C)
  #
  # (Pattern 4) CNAME_FNAME (..) | CNAME_FNAME (..) ; | ! CNAME_FNAME (..) | ! CNAME_FNAME (..) ;
  #
  ###############################################################################
  elsif( $line =~ /^ \s* ( !? \s* [A-Z]\w+? )_( \w+? ) \s* ($re_np) \s* ;* \s* $/x)
  {
    print "[parseBlock (pattern 4)] -> $1 $2 $3\n"    if $Debug > 1;
    print "[SBaseFunc] $1 $2 $3\n"          if $Debug > 2;
    print "[SBaseFunc] $line -> $1 $2 $3\n" if $Debug > 2;

    my $cname = $1;
    my $fname = $2;
    my $args  = $3;

    $args =~ s/^\(//;
    $args =~ s/\)$//;

    print "args reinterpreted as: $args\n"  if $Debug > 2;
    my @args   = &parseBlock($args);

    if ($cname eq 'SBMLTypeCode' )
    {
	    return "$Prefix{$Target}${cname}_${fname}(@args)";	  
    }

    print "[SBaseFunc (parse) ] $cname $fname @args \n" if $Debug > 2;

    return &convertSBaseCFuncCall($cname,$fname,@args);
  }
  ###############################################################################
  #
  # MISC_CFUNC (or CPP constructer)
  #
  # (Pattern 5) FNAME (..) | FNAME (..) ; | ! FNAME (..) | ! FNAME (..) ; 
  #
  ###############################################################################
  elsif( $line =~ /^ \s* ( !? \s* \w+ ) \s* ($re_np) \s* ;* \s* $/x)
  {
    print "[parseblock (pattern 5)] line $line\n" if $Debug > 1;

    # general methods
    my $fname = $1;
    my $args  = $2;
    $args =~ s/^\(//;
    $args =~ s/\)$//;
    my @args  = &parseBlock($args);

    print "[MiscFunc (parse) ] $fname || @args \n" if $Debug > 1;

    my $fcall;

    if ( defined( $SBaseClass{$fname} ) || defined( $MiscClass{$fname} ) )
    {
      print "[MiscFunc C++ constructor (parse) ] $fname || @args \n"if $Debug > 2;
      $fcall = &convertCPPNew($fname, @args); 
    }
    else
    {
      print "[MiscFunc C function (parse) ] $fname || @args \n"if $Debug > 2;
      $fcall = &convertCFuncCall($fname,@args);
    }

    return $fcall;
  }
  ###############################################################################
  # MISC_CPPFUNC  
  #
  # (Pattern 6) ( OBJ->FNAME(..) | OBJ.FNAME(..) | CNAME::FNAME(..) 
  #
  ###############################################################################
  elsif( $line =~ /^ \s* ( !? (?: \( \w+? \s* \*? \s* \) )?  \s* (?:\w+)? ) (->|\.|::) (\w+)  \s* ($re_np) \s* 
                     ((?:(?:->|\.)\w+\s*$re_np )*)  
                     \s*;*\s* $/x
       )
  {
    print "[parseblock (Pattern 6)] line $line\n" if $Debug > 1;

    # general methods
    my $obj     = $1;
    my $asymbol = $2;

    my $fname = $3;
    my $args  = $4;
    my $rest  = $5;

    print "[CPPFunc] obj $obj asymbol $asymbol fname $fname args $args rest $rest\n" if $Debug > 2;

    #
    # ignored functions
    #
    return "" if $fname =~ /^c_str$/;
    foreach ( keys %IgnoredFunc )
    {
      if ( $fname =~ /$_/ )
      {
        print "(IGNORED) [CPPFunc] $line \n" if $Debug > 2;
        return;
      }
    }

    $args =~ s/^\(//;
    $args =~ s/\)$//;

    my @args  = &parseBlock($args);
    $args = join(',',@args);

    my @rest  = &parseBlock($rest);
    $rest = join(',',@rest);

    # static function
    $asymbol = "." if $asymbol =~ "::";

    ################################################## 
    # Python
    ################################################## 
    if ( $Target eq 'python' )
    {
      my $val = $1 if $obj =~ /(\w+)/ ;
      $obj = "self.${obj}" if ( defined( $GlobalVariable{$val} ) );
    }

    ################################################## 
    # Python/Ruby
    ################################################## 
    if ( $Target eq 'ruby' )
    {
      my $val = $1 if $obj =~ /(\w+)/ ;
      $obj = "@@" . lc${obj} if ( defined( $GlobalVariable{$val} ) );
    }
    if ( ( $Target eq 'python' ) || ( $Target eq 'ruby') )
    {
      $obj = "$Prefix{$Target}${obj}" if ( defined( $SBaseClass{$obj} ) || defined( $MiscClass{$obj}) );
    }
    ################################################## 

    my $fcall = $obj . $asymbol . $fname . "(" . $args . ")" . $rest;

    return &convertCPPFuncCall($fcall);
  }
  ###############################################################################
  #
  # new 
  #
  # (Pattern 7)  new CNAME (...) ; | new CNAME (...) 
  #
  ###############################################################################
  elsif( $line =~ /^ \s* new \s* (?:$re_np)?  \s+ (\w+)  \s* ( (?:$re_np)? ) \s* ;* \s* $/x)
  {
    my $cname = $1;
    my $args  = $2;

    print "[parseBlock (pattern 7)] $cname $args\n" if $Debug > 1;

    $args =~ s/^\s*\(//;
    $args =~ s/\)\s*$//;

    print "[parseBlock (pattern 7)] cname $cname args $args : (CurLine) $CurLine\n" if $Debug > 2;

    my @args = &parseBlock($args);

    my $fcall = "";

    print "[CPPNew (args) ] cname $cname args @args\n" if $Debug > 2;

    $fcall = &convertCPPNew($cname, @args); 

    return $fcall;
  }
  ###############################################################################
  #
  # C++ constructor
  #
  # (Pattern 8)  CNAME variable (...) ;
  #
  ###############################################################################
  elsif( $line =~ /^ \s* (\w+) \s* (\w+) \s* ($re_np) \s* ;+ \s* $/x)
  {
    my $cname = $1;
    my $val   = $2;
    my $args  = $3;
    my $fcall = "";

    print "[parseBlock (pattern 8)] cname $cname val $val args \n" if $Debug > 2;

    $args =~ s/^\s*\(//;
    $args =~ s/\)\s*$//;

    my @arg = &parseBlock($args);
    $args = join(',',@arg);

    if ( defined( $IgnoredClass{$cname} ) )
    {
      print "(IGNORED) [CPP Constructer (args) ] cname $cname val $val\n" if $Debug > 2;
      $LocalVariable{$CurFunc}{$val} = 0;
      # ignored
    }
    elsif ( defined( $SBaseClass{$cname} ) || defined( $MiscClass{$cname}) )
    {
      return "" if ( $cname eq "Rule" );
      ################################################## 
      # Ruby
      ################################################## 
      if ( $Target eq 'ruby' )
      {
        $fcall = "$val = " . $Prefix{'ruby'} . $cname . ".new( $args )";
      }
      ################################################## 
      # Python
      ################################################## 
      elsif ( $Target eq 'python' )
      {
        $fcall = "$val = " . $Prefix{'python'} . $cname . "( $args )";
      }
      ################################################## 
      # Java / C#
      ################################################## 
      elsif ( ( $Target eq 'java' ) || ( $Target eq 'csharp' ) ) 
      {
        $fcall = "$cname $val = new $cname ( $args );";
      }
      ################################################## 
    }
    elsif ( $cname eq 'string' )
    {
      if ($args eq 'TestDataDirectory')
      {
        $args  = "\"$TestDataDirectory\"";

        ################################################## 
        # Ruby/Python
        ################################################## 
        if ( ( $Target eq 'ruby' ) || ( $Target eq 'python' ) )
        {
          $fcall = "$val = $args";
        }
        ################################################## 
        # Java
        ################################################## 
        elsif ($Target eq 'java')
        {
          $fcall = "$IdSTRING{$Target} $val = new $IdSTRING{$Target}( $args );";
        }
        ################################################## 
        # C#
        ################################################## 
        elsif ($Target eq 'csharp') 
        {
          $fcall = "$IdSTRING{$Target} $val =  $args;";
        }

      }
    }

    return $fcall;
  }

  ###############################################################################
  #
  # operators 
  #
  # (Pattern 8.5) LEFTBLOCK  (- | + | * | / | % ) RIGHTBLOCK
  #
  ###############################################################################
  elsif ( $line =~ /^ \s* 
                         ( (?: (?: (?<!\\) " (?:[^"]|\\")+? (?<!\\) " |  (?<!\\) ' (?:[^']|\\')+? (?<!\\) ') | [^"' ] )+ )  
                         \s*  (?<=\w|["')]|\s) (?<!\,\s|\de|\dE) ( \- | \+ | \/ | \% ) (?=\w|[("'*]|\s)  \s* 
                         ( (?: (?: (?<!\\) " (?:[^"]|\\")+? (?<!\\) " |  (?<!\\) ' (?:[^']|\\')+? (?<!\\) ') | [^"' ] )+ )  
                       \s* $
                   /x 
         )
  {
   my $b1 = $1;
   my $c1 = $2;
   my $b2 = $3;

   print "[parseBlock (pattern 8.5)] $b1 || $c1 || $b2\n" if $Debug > 2;

   my $left  = &parseBlock($b1);
   my $right = &parseBlock($b2);

   print "[parseBlock (operator)] $left || $right\n" if $Debug > 2;

   print "$b2 -> $right\n" if $b2 =~ /X0/ and $Debug > 2;
   print "$b2 -> $right\n" if $b2 =~ /wrap/ and $Debug > 2;

    return $left . " $c1 " . $right;
  }

  ###############################################################################
  #
  # comma-separated variables
  #
  # (Pattern 9)  , 
  #
  ###############################################################################
  elsif( $line =~ /,/)
  {
    my $org = $line;

    print "[parseBlock(Pattern 9)]  $line\n" if $Debug > 1;

    # escapes "," in  (.. , ..)
    $line =~ s{ ($re_np) }{ (my $r = $1) =~ s|,|_COMMA_|g; $r }xeg; 

    # escapes "," in ".. , .." or '.. , ..'
    $line =~ s{ ( " .*? (?<!\\) " ) }{ (my $r = $1) =~ s|,|_COMMA_|g; $r  }xeg;
    $line =~ s{ ( ' .*? (?<!\\) ' ) }{ (my $r = $1) =~ s|,|_COMMA_|g; $r  }xeg;

    my @args = split(',', $line);

    print "[split] $org -> @args\n" if $Debug > 2;

    for (@args)
    {
      s/_COMMA_/,/g;
    }

    # If we end up with only one item, we probably got caught in a comma
    # within arguments to a function call.  

    if (scalar(@args) == 1)
    {
      print "[parseBlock (line) ] $line\n" if $Debug > 1;
      return &convertVal($line);
    }

    for (@args)
    {
      next if /^\s* ([\"\']).*(??{$1}) \s* $/x;
      print "args -> $_\n" if $Debug > 2;
      $_ = &parseBlock($_);
    }

    print "[parseBlock(args) ] @args\n" if $Debug > 2;

    ##################################################
    # Java  / C#
    ##################################################
    if ( ($Target eq 'java') || ($Target eq 'csharp') )
    {
      if ( $CurLine !~ /[\"\'\(\)]/ )
      {
        my $cname = $args[0];
        $cname  =~ s/^\s*//;
        $cname  =~ s/\s*$//;
        $cname  =~ s/^ \s* (.+) \s+ .+ \s* $/$1/x;

        if ( defined( $SBaseClass{$cname} ) || defined( $MiscClass{$cname} )  )
        {
          return join(', ', @args);
        }
      }
    }

    return @args;     
  }
  ###############################################################################
  #
  # Others
  #
  # (Pattern 10)  , 
  #
  ###############################################################################
  else
  {
    print "[parseBlock (pattern 10)] $line\n" if $Debug > 1;
    return &convertVal($line);
  }
}



sub addAssertion1
{
  my ($line,$bool_flag) = @_;
  my $assertion;
  my $val = "";

  if ( $line =~ /UnitKind_( equals | isValidUnitKindString )/x)
  {
    $val = ($bool_flag) ? "1" : "0";
  }
  else
  {
    $val = ($bool_flag) ? $IdTRUE{$Target} : $IdFALSE{$Target};
  }

  ##################################################
  # Ruby 
  ##################################################
  if ($Target eq 'ruby')
  {
    $assertion = "assert_equal $val, $line"; 
  }
  ##################################################
  # Python
  ##################################################
  elsif ($Target eq 'python')
  {
    $assertion = "self.assertEqual( $val, $line )"; 
  }
  ##################################################
  # Java  / C#
  ##################################################
  elsif ( ($Target eq 'java') || ($Target eq 'csharp') )
  {
    $assertion = "assertEquals( $val, $line );"; 
  }
  ######################################################################

#print "[addAssertion1] -> $assertion\n";

  print "[addAssertion1] -> $assertion\n" if $Debug;

  return $assertion;
}


sub addAssertion2
{
  my ($left,$right,$op) = @_;

  my $assertion;

  if ( $CurLine =~ /SyntaxChecker_ (isValid(XMLID|SBMLSId|UnitSId) | 
                                    hasExpectedXHTMLSyntax )/x)
  {
    $left  = $IdFALSE{$Target} if ($left  eq '0');
    $left  = $IdTRUE{$Target}  if ($left  eq '1');
    $right = $IdFALSE{$Target} if ($right eq '0');
    $right = $IdTRUE{$Target}  if ($right eq '1');
  }

  ##################################################
  # Ruby 
  ##################################################
  if ($Target eq 'ruby')
  {
    if ( $CurLine =~ /fail_unless/ )
    {
      $assertion = "assert( " . $left . " " . $op . " " . $right . " )";
    }
    elsif ( $CurLine =~ /fail_if/ )
    {
      $assertion = "assert( !( " . $left . " " . $op . " " . $right . ") )";
    }
  }
  ##################################################
  # Python
  ##################################################
  elsif ($Target eq 'python')
  {
    if ( $CurLine =~ /fail_unless/ )
    {
      #if ( $CurLine =~ / \s+ \! \s+ /x )
      if ( $CurLine =~ / \( \s* \! \s* \w /x )
      {
        $assertion = "self.assertFalse(" . $left . " " . $op . " " . $right . ")";
      }
      else
      {
        $assertion = "self.assertTrue( " . $left . " " . $op . " " . $right . " )";
      }
    }
    elsif ( $CurLine =~ /fail_if/ )
    {
      $assertion = "self.assertFalse(" . $left . " " . $op . " " . $right . ")";
    }
  }
  ##################################################
  # Java/C#
  ##################################################
  elsif ( ( $Target eq 'java' ) || ( $Target eq 'csharp' ) )
  {
    my $equation = $left . " $op " . $right;

#print "[In addAssertion2] -> $equation\n";

    if ( $Target eq 'java' )
    {
      unless (  $right =~ /^ (?: 
                                [-+]?(\d+|\d*\.\d*)([eE][+-]?\d+)?
                              | 
                                libsbml\..+ 
                              |
                                true
                              |
                                false
                              |
                                null
                              |
                                '.*'
                              |
                                ".*"
                              |
                                [A-Z_]+
                             ) $/x 
             )
      {
        # sbml/TestCopyAndClone.cpp
        unless ( ($left =~ /getParentSBMLObject/ && $right =~ /getParentSBMLObject/)
                 || 
                 ($left =~ /get ( InitialAmount
                                |InitialConcentration
                                |ExponentAsDouble
                                |Multiplier
                                |Scale
                                |Size
                                |Stoichiometry
                                )
                          /x)
                  ||
                  ($CurLine =~ / getCharacter /x )
               )
        {
	  print "[turning into .equals] $left, $right\n" if $Debug > 2;
          $equation = $left . ".equals($right)";
          $equation = "!" . $equation if ( $op =~ "!=" );
        }
      }
    }

    if ( $CurLine =~ /fail_unless/ )
    {
      if ( $CurLine =~ / \( \s* \! \s* \w /x )
      {
        $assertion = "assertTrue( ! (". $equation . ") );";
      }
      else
      {
        $assertion = "assertTrue( ". $equation . " );";
      }
    }
    elsif ( $CurLine =~ /fail_if/ )
    {
      $assertion = "assertTrue( ! (". $equation . ") );";
    }
  }
  ######################################################################

  print "[In addAssertion2] -> $assertion\n" if $Debug;
#print "[In addAssertion2] -> $assertion\n";

  return $assertion;
}

sub convertMacroDefine
{
  my ($args) = @_;
  my $mname;
  my $marg;
  my $mdef;

  if ( $args =~ /^ \s* (\w+) ( \( \w+ \) )? \s* (.*)/x )
  {
    $mname = $1 . $2;
    $marg  = $2;
    $mdef  = $3;
  }

  $mname = $1 if ( $mname =~ /test_(.+)/ );

#print "[convertMacro] $mname || $marg || $mdef \n";

  ##################################################
  # Ruby / Python
  ##################################################
  if ( ( $Target eq 'ruby' ) || ( $Target eq 'python' ) )
  {
    push (@{$MacroDef{$mname}}, "{");

    if ( $mdef =~ /^ \s* " ( (?:[^"]|\\")*? ) (?<!\\) " \s* $/x )
    {
#print "[convertMacro(string)] $2 \n";
      push (@{$MacroDef{$mname}}, "return \"$1\"");
    } 
    elsif ( $mdef =~ /^ \s* ' ( (?:[^"]|\\")*? ) (?<!\\) ' \s* $/x )
    {
      push (@{$MacroDef{$mname}}, "return \"$1\"");
    } 
    elsif ( $mdef =~ /^ \s* ($re_np) \s* $/x )
    {
      push (@{$MacroDef{$mname}}, "return $1");
    } 
    else
    {
      my @mdef_args = split('\s+',$mdef);
      my $flag = 0;

#print "[convertMacro(function)] @mdef_args \n";

      for my $i (@mdef_args)
      {
        my $str = ($flag) ? "r += " : "r = ";
        $flag = 1 unless $flag;
        if ( defined ( $MacroDef{$i} ) )
        {
          $str .= "${i}()";       
        }
        else
        {
          $str .= "${i}"; 
        }

#print "[convertMacro(function)] $str \n";

        push (@{$MacroDef{$mname}}, $str);
      }
      push (@{$MacroDef{$mname}}, "return r");
    }

    push (@{$MacroDef{$mname}}, "}");
  }
  ##################################################
  # Java  / C#
  ##################################################
  elsif ( ($Target eq 'java') || ($Target eq 'csharp') )
  {
#    $mname = $mname;
    #
    # Currently, by default, type of arugment in macro is  
    # "String(Java) or string(C#)"
    #
    if ($mname =~ /isnan/)
    {
      $mname =~ s/ \( (\w+) \) /(double $1)/x;
    }
    elsif ($Target eq 'csharp')
    {
      $mname =~ s/ \( (\w+) \) /(string $1)/x;
    }
    else
    {
       $mname =~ s/ \( (\w+) \) /($IdSTRING{$Target} $1)/x;
    }

    #  matches "..."
    if ( $mdef =~ /^ \s* " ( (?:[^"]|\\")*? ) (?<!\\) " \s* $/x )
    {
#print "[convertMacro(string)] $2 \n";
      push (@{$MacroDef{$mname}}, "{");
      push (@{$MacroDef{$mname}}, "return \"$1\";");
    } 
    #  matches '...'
    elsif ( $mdef =~ /^ \s* ' ( (?:[^"]|\\")*? ) (?<!\\) ' \s* $/x )
    {
      push (@{$MacroDef{$mname}}, "{");
      push (@{$MacroDef{$mname}}, "return \"$1\";");
    } 
    #  matches (...)
    elsif ( $mdef =~ /^ \s* ($re_np) \s* $/x )
    {
      push (@{$MacroDef{$mname}}, "{");
      push (@{$MacroDef{$mname}}, "return $1;");
    } 
    else
    {
      my @mdef_args = split('\s+',$mdef);
      my $flag = 0;

#print "[convertMacro(function)] @mdef_args \n";

      push (@{$MacroDef{$mname}}, "{");
      for my $i (@mdef_args)
      {
        my $str;
        if ($flag)
        {
          $str =  "r += ";
        }
        else
        {
          #
          # currently, type of arugment in macro is fixed to "String"
          #
          $str = "$IdSTRING{$Target} r = ";
          $flag = 1;
        }

        if ( defined ( $MacroDef{$i} ) )
        {
          $str .= "${i}()";       
        }
        else
        {
          $str .= "${i}"; 
        }

#print "[convertMacro(function)] $str \n";

        push (@{$MacroDef{$mname}}, $str . ";");
      }
      push (@{$MacroDef{$mname}}, "return r;");
    }

    push (@{$MacroDef{$mname}}, "}");
  }

  ######################################################################
}

sub convertCPPFuncCall
{
  my($fcall) = @_;

#print "[convertCPPFuncCall] $fcall\n";

#  if ( $Target eq 'ruby' || $Target eq 'python' )
#  {
#    if ( $fcall =~ /XMLNode.convertStringToXMLNode/ ) 
#    {
#      $fcall = $Prefix{$Target} . $fcall; 
#    }
#  }

  ##################################################
  # Ruby 
  ##################################################
  if ( $Target eq 'ruby' )
  {
    $fcall =~ s/ -> /./gx;
    $fcall =~ s/^(\w+)\. / if ( defined( $GlobalVariable{$1} ) ) { "@@" . lc($1) . "." } else {lc($1). "."} /xe;
#    $fcall =~ s/^([A-Z]{1,3})\. / "@@" . lc($1) . "."/xe;
  }
  ##################################################
  # Python
  ##################################################
  elsif ( $Target eq 'python' )
  {
    $fcall =~ s/ -> /./gx;
#    $fcall =~ s/^([A-Z]{1,3})\. / "@@" . lc($1) . "."/xe;
  }
  ##################################################
  # Java / C#
  ##################################################
  elsif ( ($Target eq 'java') || ($Target eq 'csharp') )
  {
    $fcall =~ s/ -> /./gx;
    if ( $Target eq 'java' )
    {
      $fcall =~ s/clone \s* \(/cloneObject\(/gx;
    }
#    $fcall =~ s/^([A-Z]{1,3})\. / "@@" . lc($1) . "."/xe;
  }

  ######################################################################

  return $fcall;
}

sub convertCPPNew
{
  my($cname,@arg) = @_;
  my $fcall = "";

  #
  # ignored classes
  #
  if ( defined( $IgnoredClass{$cname} ) ) 
  {
    print "(IGNORED) [convertCPPNew] cname $cname arg @arg \n" if $Debug > 2;
    return $fcall;
  }

  if ( ($cname eq "Rule") and ($CurLine !~ / (.*) = (.*) /x ) ) 
  {
    print "(IGNORED) [convertCPPNew] cname $cname arg @arg \n" if $Debug > 2;
    return $fcall;
  }

#print "[convertCPPNew] cname $cname arg @arg \n";

  ##################################################
  # Ruby 
  ##################################################
  if ( $Target eq 'ruby' )
  {
    my $args = join(',', @arg);
    $fcall = $cname . ".new($args)";
    if ( defined( $SBaseClass{$cname} ) || defined( $MiscClass{$cname} )  )
    {
      $fcall = $Prefix{'ruby'} . $fcall;
    }      
    elsif ( $cname =~ 'ostringstream' )
    {
      $fcall =~ s/ostringstream/Ostringstream/;
      $fcall = $Prefix{'ruby'} . $fcall;
    }

  }
  ##################################################
  # Python
  ##################################################
  elsif ( $Target eq 'python' )
  {
    my $args = join(',', @arg);
    $fcall = $cname . "($args)";
    if ( defined( $SBaseClass{$cname} ) || defined( $MiscClass{$cname} )  )
    {
      $fcall = $Prefix{'python'} . $fcall;
    }      
    elsif ( $cname =~ 'ostringstream' )
    {
      $fcall = $Prefix{'python'} . $fcall;
    }
#print "[convertCPPNew] cname $cname arg @arg fcall $fcall \n";
  }
  ##################################################
  # Java / C#
  ##################################################
  elsif ( ($Target eq 'java') || ($Target eq 'csharp') )
  {
    my $args = join(',', @arg);
    $fcall = $cname . "($args)";
	$fcall =~ s/\(std\:nothrow\)//g;
    if ( defined( $SBaseClass{$cname} ) || defined( $MiscClass{$cname} )  )
    {
      $fcall = "new " . $fcall;
    }      
    elsif ( $cname =~ 'ostringstream' )
    {
      $fcall =~ s/ostringstream/OStringStream/;
#      $fcall = "new " . $Prefix{'java'} . $fcall;
      $fcall = "new " .  $fcall;
    }
  }
  ######################################################################

#print "[convertCPPNew] $fcall\n";

  return $fcall;
}

sub convertSBaseCFuncCall
{
  my($cname,$fname,@arg) = @_;
  my $fcall = "";

  print "[convertSBaseCFuncCall] cname = $cname\n" if $Debug > 2;
  print "[convertSBaseCFuncCall] fname = $fname\n" if $Debug > 2;
  print "[convertSBaseCFuncCall] arg   = @arg\n"   if $Debug > 2;

  foreach ( keys %IgnoredFunc )
  {
    if ( $fname =~ /$_/ )
    {
      print "(IGNORED) [convertSBaseCFuncCall] ${cname}_${fname} (@arg) \n" if $Debug > 2;
      return;
    }
  }

  if ( $fname !~ /get.*IndexByPrefix/ )
  {
    $fname =~ s/By[A-Z].+$//;
  }
  $fname =~ s/( (?:set|append) (?:Annotation|Notes) ) String /$1/x;
  $fname =~ s/^ set( Integer$ | Real$ | RealWithExponent | Rational)/setValue/x;
  $fname =~ s/( addAttr ) With (?:NS|Triple)/$1/x;
  $fname =~ s/( add ) With (?:Triple)/$1/x;
  $fname =~ s/( setSBOTerm ) ID /$1/x;
  $fname =~ s/( setExponent ) AsDouble /$1/x;
  $fname =~ s/( setSpatialDimensions ) AsDouble /$1/x;

  if ( $fname =~ /setLevelAndVersion/ )
  {
    if ( $fname =~ /NonStrict/ )
    {
      push (@arg, $IdFALSE{$Target} );      
    }
    else
    {
      push (@arg, $IdTRUE{$Target} );
    }
    $fname =~ s/(?:Non)?Strict//;
  }

  #
  # Removes the last argument because the constructer of Reaction 
  # class accepts only four arguments.
  #
  if ( ( $cname eq 'Reaction' ) and ( $fname eq 'createWithKineticLaw' ) )
  {
    if ( scalar(@arg) == 5 )
    {
      pop(@arg); 

      if ( $arg[3]  != 0 )
      {
        $arg[3] = $IdTRUE{$Target};
      }
      else
      {
        $arg[3] = $IdFALSE{$Target};
      }

    }
  } 

  # C functions corresponding to static C++ class functions
  if ( $cname eq 'XMLNode' ) 
  {
    if ( $fname eq 'convertStringToXMLNode' ) 
    {
      if ( ($Target eq 'java') || ($Target eq 'csharp') )
      {
        $fcall  = $cname . "." . $fname;
      }
      else
      {
        $fcall  = $Prefix{$Target} . $cname . "." . $fname;
      }
      $fcall .= '(' . join(",",@arg) . ')'; 
      return $fcall;
    }
    elsif ( $fname =~ /^create/ ) 
    {
      $fcall = "new " . $cname . '(' . join(",",@arg) . ')'; 
      return &parseBlock($fcall);
    }
  }

  if ( $cname eq 'UnitDefinition' ) 
  {
    if ( $fname eq 'printUnits' ) 
    {
      if ( ($Target eq 'java') || ($Target eq 'csharp') )
      {
        $fcall  = $cname . "." . $fname;
      }
      else
      {
        $fcall  = $Prefix{$Target} . $cname . "." . $fname;
      }

      if ( scalar(@arg) == 2)
      {
        if ( $arg[1]  != 0 )
        {
          $arg[1] = $IdTRUE{$Target};
        }
        else
        {
          $arg[1] = $IdFALSE{$Target};
        }
      }

      $fcall .= '(' . join(",",@arg) . ')'; 
      return $fcall;
    }
  }

  if ( $cname eq 'ListOf' )
  {
    if ($fname =~ /^clear/)
    {
      $arg[1] = $IdFALSE{$Target} if ( $arg[1] eq '0' );
      $arg[1] = $IdTRUE{$Target}  if ( $arg[1] eq '1' );
    }
  }

  if ( $cname eq 'RDFAnnotationParser' ||
       $cname eq 'SyntaxChecker' )
  {
    if ( ($Target eq 'java') || ($Target eq 'csharp') )
    {
      $fcall  = $cname . "." . $fname;
    }
    else
    {
      $fcall  = $Prefix{$Target} . $cname . "." . $fname;
    }

    $fcall .= '(' . join(",",@arg) . ')';
    return $fcall;
  }

#  print "[In convertSBaseCFuncCall] -> $cname $fname @arg\n";
#print "[convertSBaseCFuncCall] $cname || $fname || @arg\n";

  if ($fname =~ /^create.*WithLevelVersionAndNamespaces/)
  {
    $arg[2] = $IdNULL{$Target} if ( $arg[2] eq "0" ) ;
  }
    
  $fcall = $fname;
  if ($fname =~ /free$/)
  { 
    my $args = join(",", @arg);

    if ($Target eq 'python')
    {   
      # Crude way of simulating deletion in Python.
      #
      return "_dummyList = [ $args ]; _dummyList[:] = []; del _dummyList";
    }
    else
    {
      # FIXME need to find appropriate solutions for other languages.

      return ""  if $args =~ /$IdNULL{$Target}/;
      return "$args = $IdNULL{$Target}";
    }
  }
  elsif ( $cname eq 'XMLInputStream' )
  {
    if ($fname =~ /^create/)
    {
      $arg[1] = $IdFALSE{$Target} if ( $arg[1] eq '0' );
      $arg[1] = $IdTRUE{$Target}  if ( $arg[1] eq '1' );

      return &getCreateObjString($cname, join(",", @arg));
    }
  }
  elsif($fname =~ /^create$/)
  {
    return &getCreateObjString($cname, join(",", @arg));
#    return &getCreateObjString($cname,"");
  }
  elsif($fname =~ /^create(With|From)/)
  {
#print "$cname @arg\n";
    return &getCreateObjString($cname, join(",",@arg));
  }
  elsif ( $cname eq 'XMLOutputStream' )  
  {
    if ($fname =~ /^create/)
    {
      $arg[1] = $IdFALSE{$Target} if ( $arg[1] eq '0' );
      $arg[1] = $IdTRUE{$Target}  if ( $arg[1] eq '1' );

      $arg[2] = $IdFALSE{$Target} if ( $arg[2] eq '0' );
      $arg[2] = $IdTRUE{$Target}  if ( $arg[2] eq '1' );
    }

    if ($fname =~ /^createAsStdout/)
    {
      unshift ( @arg, $IdCOUT{$Target} );

      return &getCreateObjString($cname, join(",",@arg));
    }
    elsif ($fname =~ /^createAsString/)
    {
      push (@{$FuncDef{$CurFunc}}, $IdOSS{$Target});

      unshift ( @arg, "oss" );

      return &getCreateObjString($cname, join(",",@arg));
    }
    elsif ($fname =~ /^createFile/)
    {
      $arg[0] = &getCreateFileString($arg[0]);
      return &getCreateObjString($cname, join(",",@arg));
    }
    elsif ($fname =~ /^getString/)
    {
      return "oss.str()";
    }
  }
  elsif ( $cname eq 'Rule') 
  {
    if ($fname =~ /^create(Algebraic|Assignment|Rate)/)
    {
      my $dcname = $1 . "Rule";  

      return &getCreateObjString($dcname, join(",",@arg));
    }
  }
  elsif ( $cname eq 'SpeciesReference') 
  {
    if ($fname =~ /^createModifier/)
    {
      my $dcname = "ModifierSpeciesReference";

      return &getCreateObjString($dcname, join(",",@arg));
    }
  }
  elsif ( $cname eq 'UnitKind') 
  {
    if ($fname =~ /^forName/)
    {
      $fcall = $Prefix{$Target} . "UnitKind_forName";
      $fcall .= '(' . join(",",@arg) . ')'; 
      return $fcall;
    }
    elsif ($fname =~ /^equals/)
    {
      $fcall = $Prefix{$Target} . "UnitKind_equals";
      $arg[0] = '""' if $arg[0] eq $IdNULL{$Target};
      $fcall .= '(' . join(",",@arg) . ')'; 
      return $fcall;
    }
    elsif ($fname =~ /^isValidUnitKindString/)
    {
      $fcall = $Prefix{$Target} . "UnitKind_isValidUnitKindString";
      $arg[0] = '""' if $arg[0] eq $IdNULL{$Target};
      $fcall .= '(' . join(",",@arg) . ')'; 
      return $fcall;
    }
    elsif ($fname =~ /^toString/)
    {
      $fcall = $Prefix{$Target} . "UnitKind_toString";
      $arg[0] = '""' if $arg[0] eq $IdNULL{$Target};
      $fcall .= '(' . join(",",@arg) . ')'; 
      return $fcall;
    }
  }
  elsif ( $cname eq 'Unit') 
  {
    if ($fname =~ /^ (isBuiltIn|removeScale) /x )
    {
      ##################################################
      # Java / C#
      ##################################################
      if ( ($Target eq 'java') || ($Target eq 'csharp') )
      {
        $fcall = "Unit.$1";
      }
      ##################################################
      # Python/Ruby
      ##################################################
      else
      { 
        $fcall = $Prefix{$Target} . "Unit.$1";
      }

      $arg[0] = '""' if $arg[0] eq $IdNULL{$Target};
      $fcall .= '(' . join(",",@arg) . ')'; 
      return $fcall;
    }
  }
  elsif ( $cname eq 'SBML') 
  {
    if ($fname =~ /^parseFormula/)
    {
      $fcall = $Prefix{$Target} . "parseFormula";
      $fcall .= '(' . join(",",@arg) . ')'; 
      return $fcall;
    }
    elsif ($fname =~ /^formulaToString/)
    {
      $fcall = $Prefix{$Target}. "formulaToString";
      $fcall .= '(' . join(",",@arg) . ')'; 
      return $fcall;
    }
  }

  if (defined($arg[0]))
  {
    if ( $arg[0] =~ /^\((.*)\)/x )
    {
      my $actual = $1;
      if ( $Target eq 'python' )
      {
        $arg[0] = "(self." . $actual . ")" if ( defined( $GlobalVariable{$actual} ) );
      }
      elsif ( $Target eq 'ruby' )
      {
        $arg[0] = "@@" . lc($actual) . "" if ( defined( $GlobalVariable{$actual} ) );
      }
    }

    $fcall = $arg[0] . "." . $fname;

    if (defined($arg[1])) 
    {
      if ( $cname ne 'ASTNode' && 
           $fname =~ /^\s*set(  Id
                                |MetaId
                                |Name
                                |Variable
                                |TimeUnits
                                |Outside
                                |Units
                                |Formula
                                |SpatialSizeUnits
                                |SubstanceUnits
                                |Compartment
                                |CompartmentType
                                |Species
                                |SpeciesType
                                |Symbol
                                |ProgramName
                                |ProgramVersion
                                |DateAsString
                              ) \s* $
                      /x 
          )
      {
        $arg[1] = '""' if $arg[1] eq $IdNULL{$Target};
      }
      elsif ( $fname =~ /^ \s* set( BoundaryCondition
                                    |Constant
                                    |Fast
                                    |Reversible
                                    |BoundaryCondition
                                    |ConstanHasOnlySubstanceUnits
                                    |UseValuesFromTriggerTime
                                    |HasOnlySubstanceUnits
                                  )
                        /x
             )
      {
        $arg[1] = $IdFALSE{$Target} if $arg[1] == '0';
        $arg[1] = $IdTRUE{$Target}  if $arg[1] != '0';
      }
      if ($fname =~ /^ \s* set(Annotation|Notes) /x )
      {
        $arg[1] = "(XMLNode)null" if ( $arg[1] =~ /null/ );
      }
      elsif ($fname =~ /^(writeAttribute)/)
      {
        my $lfname = $1;

        ##################################################
        # Ruby & Python
        ##################################################
        if ($Target eq 'ruby' || $Target eq 'python' || $Target eq 'csharp' )
        {
          if ( $arg[1] =~ /bool/)
          {
            $arg[2] = $IdFALSE{$Target} if $arg[2] == '0';
            $arg[2] = $IdTRUE{$Target}  if $arg[2] != '0';
            $lfname .= "Bool" if ( $Target ne 'csharp' );
          }
        }
	elsif ( $Target eq 'java' )
        {
          if ( $arg[1] =~ /bool/)
          {
            $arg[2] = $IdFALSE{$Target} if $arg[2] == '0';
            $arg[2] = $IdTRUE{$Target}  if $arg[2] != '0';
	  }	  
	}

        $fcall = $arg[0] . "." . $lfname;
      }
      elsif ($fname =~ /^ (add)With(?: Namespace | Triple ) /x )
      {
        my $lfname = $1;

        $fcall = $arg[0] . "." . $lfname;
      }
      elsif ($fname =~ /^(getModifiedDate)FromList/)
      {
        my $lfname = $1;

        $fcall = $arg[0] . "." . $lfname;
      }
      elsif ($fname =~ /^(hasAttribute)/)
      {
        $fcall = $arg[0] . "." . $1;
      }
      elsif ($fname =~ /^(hasAttr)With/)
      {
        $fcall = $arg[0] . "." . $1;
      }

      shift(@arg);
      $fcall .= '(' . join(",",@arg) . ')'; 
    }
    else
    {
      $fcall .= '()'; 
    }
  }
  else
  {
    ##################################################
    # Java / C#
    ##################################################
    if ( ($Target eq 'java') || ($Target eq 'csharp') )
    {
      $fcall = $cname . "." . $fname. "()";
    }
    ##################################################
    # Python/Ruby
    ##################################################
    else
    {
      $fcall = $Prefix{$Target} . $cname . "." . $fname . "()";
    }

  }

  return $fcall;
}


sub convertCFuncCall
{
  my($fname,@arg) = @_;
  my $fcall = "";

#print "[convertCFuncCall] $fname || @arg\n";

  $fcall = $fname;
  if($fname =~ /^ (!?) \s* strcmp/x )
  {
    if ( $1 ne '!' )
    {
      ##################################################
      # Java
      ##################################################
      if($Target eq 'java')
      {
        $fcall = "!$arg[0].equals($arg[1])";
      }
	else
      {	    
        $fcall = "( " . $arg[1] . " != " . $arg[0] . " )";  
      }
    }
    else
    {
      ##################################################
      # Java
      ##################################################
      if($Target eq 'java')
      {
        $fcall = "$arg[0].equals($arg[1])";
      }
	else
      {	    
        $fcall = "( " . $arg[1] . " == " . $arg[0] . " )";  
      }
    }
  }
  elsif($fname =~ /^ \s* (read(?:SBML|MathML)(?:FromString)?)\s*$/x )
  {
    $fcall = "$Prefix{$Target}${1}(" . $arg[0] . ")";
  }
  elsif($fname =~ /^ \s* (parseLayoutAnnotation?)\s*$/x )
  {
    my $args = join(',', @arg);
    $fcall = "$Prefix{$Target}${1}(" . $args . ")";
  }		  
  elsif($fname =~ /^ \s* (write(?:SBML|MathML)(?:ToString)?)\s*$/x )
  {
    $fcall = "$Prefix{$Target}${1}(" . $arg[0] . ")";
  }
  elsif($fname =~ /^ \s* safe_strcat/x )
  {
    if ( $arg[0] =~ /TestDataDirectory/ )
    {
      $arg[1] =~ s/"//g;
      $arg[1] =~ s/^\s*//;
      $arg[1] =~ s/\s*$//;
      $arg[1] = '"' . $TestDataDirectory . $arg[1] . '"' if ( $arg[0] =~ /TestDataDirectory/ );
    }
    $fcall = $arg[1];
     #########
  }
  elsif( $fname =~ /^ \s* fail\b/x )
  {
    #
    # fail() is IGNORED for now.
    # return 'flunk ' . $args;
    # $fcall = '#flunk ' . $arg[0];
  } 
  elsif( $fname =~ /^ \s* wrap(SBML|XML|MathML) /x )
  {
    my $args = join(',', @arg);
    $fcall = "$fname". "(" . $args . ")";
  }
  elsif( $fname =~ /^ \s* equals /x )
  {
    my $args = join(',', @arg);
    $fcall = $fname . "(" . $args . ")";
    ##################################################
    # Python
    ##################################################
    $fcall = "self." . $fcall if($Target eq 'python');
  }
  elsif ( $fname =~ /^ \s*  (?: abs ) /x )
  {
    if ( $Target eq 'java' )
    {
      my $args = join(',', @arg);
      $fcall = "java.lang.Math.abs(" . $args . ")";
    }
    elsif ( $Target eq 'ruby' )
    {
      my $args = join(',', @arg);
      $fcall =  "(" . $args . ").abs";
    }
    elsif ( $Target eq 'csharp' )
    {
      my $args = join(',', @arg);
      $fcall =  "Math.Abs(" . $args . ")";
    }
    elsif ( $Target eq 'python' )
    {
      my $args = join(',', @arg);
      $fcall =  "abs(" . $args . ")";
    }
  }
  elsif ( $fname =~ /^ \s*  (?: test_isnan | util_isInf | isnan ) /x )
  {
    $fname = "isnan" if $fname =~ /isnan/;
    my $args = join(',', @arg);
    $fcall = "$fname". "(" . $args . ")";
  }
  elsif ($fname =~ /^ \s* free \s* $/x)
  { 
    my $args = join(",", @arg);
    if ( $args =~ /$IdNULL{$Target}/ )
    {
	  $fcall = "";
    }
    else
    {
      $fcall = "$args = $IdNULL{$Target}";
    }
  }
  else
  {
      # ignored
  } 

  return $fcall;
}



sub convertVal
{
  my ($val) = @_;

  print "[convertVal] $val\n" if $Debug > 1;

  my $is_tail = 0;
  my $cast = "";

  $val =~ s/^\s+//;
  $val =~ s/\s+$//;
  $is_tail = 1 if ($val =~ s/;+$//) ;

  # string  "..."
  return $val if ($val =~ /^ " (?:[^"]|\\")*? (?<!\\) " $/x);
  # string  '...' 
  return $val if ($val =~ /^ ' (?:[^']|\\')*? (?<!\\) ' $/x);

  # cast
  if ($val =~ /^ ( \( \w+ \s* \* \) ) \s* \(* (\w+) \)* $/x)
  {
    $cast = $1; 
    $val = $2; 
 
    print "[convertVal (typecast)] $cast || $val\n" if $Debug > 1;

    ################################################## 
    # Java
    ################################################## 
    if ( $Target eq 'java' )
    {
      if ( $CurLine =~ /^ \s* \w*? \s* SpeciesReference .* (?: _createModifier | MSR )/x )
      {
        $cast =~ s/^\s*  (\w*?)  \s* SpeciesReference_t/$1 SimpleSpeciesReference/x;
      }
    }
    ################################################## 
  }

  # dereference/reference
  $val = $1 if ($val =~ /^ \( \s* [\*&] \s* (\w+) \) /x);
  $val = $1 if ($val =~ /^ \s* [\*&] \s* \( (\w+) \) /x);
  $val = $1 if ($val =~ /^ \s* [\*&] \s* (\w+) /x);

  # type 
  if ($val =~ /^ (?: ( const | unsigned ) \s* )? (\w+? \s* \*?) \s* (\b \w+) $/x)
  {
    print "[convertVal (val)] $val (1) $1 (2) $2 (3) $3\n" if $Debug > 2;

    my $type;
    my $modifier = $1;
    my $maintype = $2;
    my $ivartmp  = $3;

    if ( $Target eq 'java' and $modifier =~ /unsigned/x and $maintype =~ /int/x )
    {
      $type = "long";
    }
    elsif ( $modifier =~ /const/x )
    {
      $type = "$maintype";
    }
    else
    {
      $type = "$modifier $maintype";
    }

    my $ivar = &parseBlock($ivartmp);

    $type =~ s/\s*//g;
    $ivar =~ s/\s*//g;

    print "[convertVal (type)/(val)] type $type val $ivar \n" if $Debug > 2;

    if ( defined ( $IgnoredClass{$type} ) )
    {
      print "(IGNORED) [convertVal (class ivar) ] $type $ivar \n" if $Debug > 2;
      $LocalVariable{$CurFunc}{$ivar} = 0;
      return;
    }
    elsif ( defined ( $SBaseClass{$type} ) || defined ( $MiscClass{$type} ) )
    {
      print "[convertVal (class $type) ] $type $ivar \n" if $Debug > 2;

      $LocalVariable{$CurFunc}{$ivar} = 1;

      # $type $ivar = $type(...);
      unless ( $is_tail )
      {
        return "$type $ivar" if ( ( $Target eq 'java' ) || ( $Target eq 'csharp' ));
        return $ivar;
      }

      # $type $ivar;
      my $r;
      
      ##################################################
      # Java / C#
      ##################################################
      if ( ( $Target eq 'java' ) || ( $Target eq 'csharp' ))
      {
        $r =  $type . " " . $ivar . " = ". &convertCPPNew($type);
      }
      ##################################################
      # Ruby/Python
      ##################################################
      else
      {
        $r =  $ivar . " = ". &convertCPPNew($type);
      }
      print "[convertVal (r)] $r \n" if $Debug > 2;

      return $r;
    }
    elsif ( $type =~ /delete/ )
    {
      if ( ( $Target eq 'java' ) || ( $Target eq 'csharp' ))
      {
        unless ( $LocalVariable{$CurFunc}{$ivar} or $GlobalVariable{$ivar} )
        {
           print "(IGNORED) delete $ivar \n" if $Debug > 2;
  	   return;
        }
      }

      ##################################################
      # Ruby
      ##################################################
      if ($Target eq 'ruby')
      {
        return "$ivar = nil"
      }
      ##################################################
      # Python
      ##################################################
      elsif ($Target eq 'python')
      {
        return "$ivar = None"        
      }
      ##################################################
      # Java / C#
      ##################################################
      elsif ( ( $Target eq 'java' ) || ( $Target eq 'csharp' ))
      {
        return "$ivar = null;"        
      }
    }
    else
    {
      print "[convertVal (type mangling) ]\n" if $Debug > 2;      

     ##################################################
     # Java / C#
     ##################################################
     if ( ( $Target eq 'java' ) || ( $Target eq 'csharp' ))
     {
      $type =~ s/\* \s* $//x;
      if ( $CurLine =~ /^ \s* \w*? \s* SpeciesReference .* (?: _createModifier | MSR )/x )
      {
        $type =~ s/^ \s* (\w*?) \s* SpeciesReference_t \s* $/$1 ModifierSpeciesReference/x;
      }
      elsif ( $CurLine =~ /^ \s* \w*? \s* SpeciesReference .*  \s* ssr /x )
      {
        $type =~ s/^ \s* (\w*?) \s* SpeciesReference_t \s* $/$1 SimpleSpeciesReference/x;
      }
      elsif ( $CurLine =~ /^ \s* \w*? \s* SpeciesReference .*  \s* msr /x )
      {
        $type =~ s/^ \s* (\w*?) \s* SpeciesReference_t \s* $/$1 ModifierSpeciesReference/x;
      }
      elsif ( $type =~ /^char$/ && $CurLine !~ /getCharacter(:!s)/x )
      {
	$type =~ s/^char$/$IdSTRING{$Target}/x;
      }

      $type =~ s/ _t \s* $//x;
      $type =~ s/^string$/$IdSTRING{$Target}/x;
      $type =~ s/^unsigned int$/long/x;

      if ( $Target ne 'java' && $Target ne 'csharp' )
      {
	$type =~ s/^int$/long/x;
      }

      print "[convertVal (type var) decl ] $type $ivar \n" if $Debug > 2;

      $LocalVariable{$CurFunc}{$ivar} = 1;

      print "[convertVal (return)] $type $ivar \n" if $Debug > 2;

      return "$type $ivar" 
     }
     ##################################################

      print "[convertVal (return)] $ivar \n" if $Debug > 2;

      return $ivar;
    }
  }

  print "[convertVal (var) ] $val \n" if $Debug > 2;

     $val = $IdNULL{$Target} if ( $val =~ /^ \s* NULL \s* $/x );
     # libSBML constants 
     $val = $Prefix{$Target} . $val  if ( $val =~ /^SBML_[A-Z]+/);
     $val = $Prefix{$Target} . $val  if ( $val =~ /^SPECIES_[A-Z]+/);
	 $val = $Prefix{$Target} . $val  if ( $val =~ /^LIBSBML_[A-Z]+/);
     $val = $Prefix{$Target} . $val  if ( $val =~ /^BIOLOGICAL_[A-Z]+/);
     $val = $Prefix{$Target} . $val  if ( $val =~ /^BQB_[A-Z]+/);
     $val = $Prefix{$Target} . $val  if ( $val =~ /^BQM_[A-Z]+/);
     $val = $Prefix{$Target} . $val  if ( $val =~ /^MODEL_[A-Z]+/);
     $val = $Prefix{$Target} . $val  if ( $val =~ /^AST_[A-Z]+/);
     $val = $Prefix{$Target} . $val  if ( $val =~ /^RULE_TYPE_[A-Z]+/);
     $val = $Prefix{$Target} . $val  if ( $val =~ /^UNIT_KIND_[A-Z]+/);
     $val = $Prefix{$Target} . $val  if ( $val =~ /^XMLFile[A-Z]/);

     $val = $Prefix{$Target} . $val  if ( $val =~ /^DuplicateXMLAttribute/);
     $val = $Prefix{$Target} . $val  if ( $val =~ /^EmptyListInReaction/);
     $val = $Prefix{$Target} . $val  if ( $val =~ /^OverdeterminedSystem/);
     $val = $Prefix{$Target} . $val  if ( $val =~ /^OffsetNoLongerValid/);
     $val = $Prefix{$Target} . $val  if ( $val =~ /^NoSBOTermsInL1/);
     $val = $Prefix{$Target} . $val  if ( $val =~ /^DisallowedMathMLEncodingUse/);
     $val = $Prefix{$Target} . $val  if ( $val =~ /^UnknownError/);

     $val = $constDBL_EPSILON{$Target} if ( $val =~ /^DBL_EPSILON$/);
     $val = $constSBML_INT_MAX{$Target} if ( $val =~ /SBML_INT_MAX/);
     $val = $IdTRUE{$Target}   if ( $val =~ /^true$/);
     $val = $IdFALSE{$Target}  if ( $val =~ /^false$/);

     if ($Target eq 'ruby')
     {
       # ruby class static variable
       $val = "@@" . lc($val) if ( defined( $GlobalVariable{$val} ) );
       # ruby local variable
       $val = lc($val)        if ( $val =~ /^[A-Z][_a-z0-9]+$/);
       $val = lc($val)        if ( $val =~ /^ ( N | EMAIL | ORG | NS | CVTerm[12] ) $/x);
       # floating point 
       $val = '0' . $val if ( $val =~ /^\.[0-9]+$/ );
     }
     elsif ($Target eq 'python')
     {
       $val = "self.${val}" if ( defined( $GlobalVariable{$val} ) );
     }
     $val =~ s/-> \s* (\w)/.$1/xg;

  ################################################## 

  return $val;
}


######################################################################
#
#  Output
#
######################################################################

# not used 
sub getFuncHeader
{
  my ($func_name, @args) = @_;
  my $func_header = "";

  ##################################################
  # Ruby
  ##################################################
  if($Target eq 'ruby')
  {
    $func_header .= "def $func_name"; 
    if ( @args )
    {
      $func_header .= "(" . join(',',@args) . ")";
    }
  }
  ##################################################
  # Python
  ##################################################
  elsif($Target eq 'python')
  {
    $func_header .= "def $func_name"; 
    if ( @args )
    {
      $func_header .= "(" . join(',',@args) . ")";
    }
    $func_header .= ":";
  }
  ################################################## 

  return $func_header;
}

# not used
sub getFuncFooter
{
  my $func_footer = "";

  ##################################################
  # Ruby
  ##################################################
  if ($Target eq 'ruby')
  {
    $func_footer = "end";
  }
  ################################################## 

  return $func_footer;
}

sub printFuncDef
{
  my ($func, $funcRef, $fh, $offset, $is_global) = @_;
  my $indent_offset = $offset;
  my $indent  = 0;
  my $inBlock = 0;

  return if $func =~ /^\s*$/;

  ##################################################
  # Ruby 
  ##################################################
  if ($Target eq 'ruby')
  {
    print $fh " " x ($indent+$indent_offset);
    print $fh "def $func\n";

    for(my $i=0; $i < @{$funcRef}; $i++ )
    {
       my $l = ${$funcRef}[$i]; 
       $l =~ s/^\s*//;

      if ( $l eq "}" ) 
      {
        $indent -= 2;
       if ($inBlock)
       {
         print $fh " " x ($indent+$indent_offset);
         print $fh "end" . "\n";
       }
        next;
      }
      elsif ( $l eq "{" )
      {
        $inBlock = 1;
        $indent += 2;
        next;
      }

      print $fh " " x ($indent+$indent_offset);
      print $fh $l . "\n";
    }

    $indent = 0;
    print $fh "\n";
  }
  ##################################################
  # Python
  ##################################################
  elsif ($Target eq 'python')
  {
    print $fh " " x ($indent+$indent_offset);

    if ($is_global)
    {
      print $fh "def $func";
      print $fh "()" unless $func =~ /$re_np/;
      print $fh ":\n";
    }
    else
    {
      print $fh "def $func(self):\n";
    }

    for(my $i=0; $i < @{$funcRef}; $i++ )
    {
       my $l = ${$funcRef}[$i]; 
       $l =~ s/^\s*//;

      if ( $l eq "}" ) 
      {
       if ($inBlock)
       {
         print $fh " " x ($indent+$indent_offset);
	 print $fh "pass";

        $indent -= 2;

         print $fh " " x ($indent+$indent_offset);
#         print $fh "}";
         print $fh "\n";
       }
       else
       {
         $indent -= 2;
       }

        next;
      }
      elsif ( $l eq "{" )
      {
#        print $fh "{";
        $inBlock = 1;
        $indent += 2;
        next;
      }

      print $fh " " x ($indent+$indent_offset);
      print $fh $l;
      print $fh ':' if ( $l =~ /^ \s* (if | elif | else )/x ) ;
      print $fh "\n";
    }

    $indent = 0;
    print $fh "\n";
  }
  ##################################################
  # Java / C#
  ##################################################
  elsif ( ($Target eq 'java') || ($Target eq 'csharp') )
  {
    print $fh " " x ($indent+$indent_offset);

    if ($is_global)
    {
      my $rtype = $IdSTRING{$Target};
      $rtype = $IdBOOL{$Target} if $func =~ /isnan/;
      print $fh "public $rtype $func";
      print $fh "()" unless $func =~ /$re_np/;
      print $fh "\n";
    }
    elsif ( $func !~ /protected/ )
    {
      print $fh "public void $func()\n"
    }
    else
    {
      print $fh "$func\n"
    }

    for(my $i=0; $i < @{$funcRef}; $i++ )
    {
       my $l = ${$funcRef}[$i]; 
       $l =~ s/^\s*//;

      if ( $l eq "}" ) 
      {
       if ($inBlock)
       {
#         print $fh " " x ($indent+$indent_offset);

         $indent -= 2;
#         print $fh "\n";
         print $fh " " x ($indent+$indent_offset);
         print $fh "}";
         print $fh "\n";
       }
       else
       {
         $indent -= 2;
       }

        next;
      }
      elsif ( $l eq "{" )
      {
        print $fh " " x ($indent+$indent_offset);
        print $fh "{\n";
        $inBlock = 1;
        $indent += 2;
        next;
      }

      print $fh " " x ($indent+$indent_offset);
      print $fh $l;
      print $fh "\n";
    }

    $indent = 0;
    print $fh "\n";
  }
  #################################################### 
}

sub writeCode
{
  my ($file) = @_;
  my $cat = 'sbml';

  $cat = $1 if ( $file =~ m|/src/([^/]*)/| ); 
  $file =~ s/.*\///;
  my $ofile = $file;


  ##################################################
  # Ruby/Python
  ##################################################
  if ( ( $Target eq 'ruby') || ( $Target eq 'python' ) )
  {
    # to avoid duplicated file names 
    $file =~ s/(CopyAndClone)/Annotation$1/ if ( $CurTestDir =~ /annotation/);
    $file =~ s/(CopyAndClone)/XML$1/        if ( $CurTestDir =~ /xml/);
    $file =~ s/(ReadFromFile[1-9])/Math$1/  if ( $CurTestDir =~ /math/);
  }


  ##################################################
  # Ruby
  ##################################################
  if ( $Target eq 'ruby')
  {
      $file =~ s/\.c$/.rb/;
      $file =~ s/\.cpp$/.rb/;
  }
  ##################################################
  # Python
  ##################################################
  if ( $Target eq 'python')
  {
      $file =~ s/\.c$/.py/;
      $file =~ s/\.cpp$/.py/;
  }
  ##################################################
  # Java
  ##################################################
  elsif ( $Target eq 'java')
  {
      $file =~ s/\.c$/.java/;
      $file =~ s/\.cpp$/.java/;
  }
  ##################################################
  # C#
  ##################################################
  elsif ( $Target eq 'csharp')
  {
      $file =~ s/\.c$/.cs/;
      $file =~ s/\.cpp$/.cs/;
  }  
  ################################################## 

  open(FH, ">${OutputDir}/$file") or die "Can't write ${OutputDir}/$file : $!/$?";

  &writeHeader($file,*FH, $cat, $ofile);

  ##################################################
  # Ruby
  ##################################################
  if ( $Target eq 'ruby' )
  {
    # print macro definitions
    map { &printFuncDef($_, $MacroDef{$_}, *FH, 2 , 0) } sort keys %MacroDef;
  }
  ##################################################
  # Java
  ##################################################
  elsif ( $Target eq 'java' )
  {
    # print macro definitions
    map { &printFuncDef($_, $MacroDef{$_}, *FH, 2, 1) } sort keys %MacroDef;
  }
  ##################################################
  #  C#
  ##################################################
  elsif ( $Target eq 'csharp' )
  {
    # print macro definitions
    map { &printFuncDef($_, $MacroDef{$_}, *FH, 4, 1) } sort keys %MacroDef;
  }
  ################################################## 

  (my $bname = $file) =~ s| \. .* ?$||gx;

  # append a patch code 
  print FH $patchClassTop{$Target}{$bname} if ( $patchClassTop{$Target}{$bname} ); 

  # append a patch code to the top of the corresponding function
  for ( keys %{ $patchFuncHead{$Target}{$bname} } )
  {
    if ( defined( $FuncDef{$_} ) )
    {
      my $tmp = shift @{ $FuncDef{$_} };
      unshift (  @{ $FuncDef{$_} }, $patchFuncHead{$Target}{$bname}{$_} );
      unshift (  @{ $FuncDef{$_} }, $tmp );
    }
  }

  # replace the function code with the corresponding path code
  for ( keys %{ $patchFuncReplace{$Target}{$bname} } )
  {
    if ( defined( $FuncDef{$_} ) )
    {
      @{ $FuncDef{$_} } = ();

      # to avoid an invalid indent in Python
      push ( @{ $FuncDef{$_} }, "{"  );

      push ( @{ $FuncDef{$_} }, $patchFuncReplace{$Target}{$bname}{$_}  );
    }
  }

  # print test functions
  my $offset_funcdef = 2;
  $offset_funcdef = 4 if ( $Target eq 'csharp' );
  map { &printFuncDef($_, $FuncDef{$_},  *FH, $offset_funcdef) unless ( defined( $IgnoreTestFunc{$_} ) ) } sort keys %FuncDef;

  &writeFooter(*FH,$bname);

  close(FH);

  print "(converted) $ofile => ${OutputDir}/$file\n";

}

sub writeHeader
{
  my ($file, $fh, $cat, $ofile) = @_;
  my $cname = $file;

  (my $lang = $Target) =~ s/^([a-z])/ uc $1 /xe;

  my $brief = $FileProp{$ofile}{'brief'};
  my @author;
  my $author;
  my $c;

  $c = '#' if (  ($Target eq 'ruby') || ($Target eq 'python' ) );
  $c = ' *' if ($Target eq 'java');
  $c = '/// ' if ($Target eq 'csharp');

  $author = $c;
  $author .= " \@author  Frank Bergmann (Csharp conversion)" if ($Target eq 'csharp');

  foreach ( @{ $FileProp{$ofile}{'author'} } )
  {
    push ( @author, "$c \@author  $_ \n");
  }
  chomp( $author[length(@author)-1] );

  my $head;

  $head = "/\*\n" if ($Target eq 'java');
  $head = "$c\n" if ($Target eq 'python');

  $head .= <<"EOF";
$c \@file    $file
$c \@brief   $brief
$author
$c \@author  Akiya Jouraku ($lang conversion)
@author
$c 
$c ====== WARNING ===== WARNING ===== WARNING ===== WARNING ===== WARNING ======
$c
$c DO NOT EDIT THIS FILE.
$c
$c This file was generated automatically by converting the file located at
$c src/$cat/test/$ofile
$c using the conversion program dev/utilities/translateTests/translateTests.pl.
$c Any changes made here will be lost the next time the file is regenerated.
$c
$c -----------------------------------------------------------------------------
$c This file is part of libSBML.  Please visit http://sbml.org for more
$c information about SBML, and the latest version of libSBML.
$c
$c Copyright 2005-2010 California Institute of Technology.
$c Copyright 2002-2005 California Institute of Technology and
$c                     Japan Science and Technology Corporation.
$c 
$c This library is free software; you can redistribute it and/or modify it
$c under the terms of the GNU Lesser General Public License as published by
$c the Free Software Foundation.  A copy of the license agreement is provided
$c in the file named "LICENSE.txt" included with this software distribution
$c and also available online as http://sbml.org/software/libsbml/license.html
$c -----------------------------------------------------------------------------
EOF

  ##################################################
  # Ruby
  ##################################################
  if ( $Target eq 'ruby')
  {
    $cname =~ s/\.rb$//;
    ################################################## 
    print $fh $head;
    print $fh "require 'test/unit'\n";
    print $fh "require '$ModuleName{'ruby'}'\n\n";
    ################################################## 

    # print patch for global variables/functions
    print $fh $patchGlobal{$Target}{$cname} if ( $patchGlobal{$Target}{$cname} ); 

    print $fh "class $cname < Test::Unit::TestCase\n\n" ;
  }
  ##################################################
  # Python
  ##################################################
  elsif ( $Target eq 'python')
  {
    $cname =~ s/\.py$//;
    ################################################## 
    print $fh $head . "\n";
    print $fh "import sys\n";
    print $fh "import unittest\n";
    print $fh "import $ModuleName{'python'}\n\n";
    ################################################## 

    # print patch for global variables/functions
    print $fh $patchGlobal{$Target}{$cname} if ( $patchGlobal{$Target}{$cname} ); 

    # print macro definitions
    map { &printFuncDef($_, $MacroDef{$_}, *FH, 0, 1) } sort keys %MacroDef;

    print $fh "\nclass $cname(unittest.TestCase):\n\n" ;
    for ( keys %GlobalVariable )
    {
	next unless $_;
	print $fh "  global $_\n";
	print $fh "  $_ = None\n";
    }
    print $fh "\n";
  }
  ##################################################
  # Java
  ##################################################
  elsif ( $Target eq 'java')
  {
    $cname =~ s/\.java$//;
    ################################################## 
    print $fh $head . " \*/\n";
    print $fh "\n";
    print $fh "package " . $JavaPackage . ".$CurTestDir" . ";\n";
    print $fh "\n";
    print $fh "import org.sbml.$ModuleName{'java'}.*;\n\n";
    print $fh "import java.io.File;\n";
    print $fh "import java.lang.AssertionError;\n";
#    print $fh "import junit.framework.TestCase;\n\n";
    ################################################## 

    # print patch for global variables/functions
    print $fh $patchGlobal{$Target}{$cname} if ( $patchGlobal{$Target}{$cname} ); 

#    print $fh "public class $cname extends TestCase {\n\n" ;
    print $fh "\n";
    print $fh "public class $cname {\n" ;

    print $fh <<"EOF";

  static void assertTrue(boolean condition) throws AssertionError
  {
    if (condition == true)
    {
      return;
    }
    throw new AssertionError();
  }

  static void assertEquals(Object a, Object b) throws AssertionError
  {
    if ( (a == null) && (b == null) )
    {
      return;
    }
    else if ( (a == null) || (b == null) )
    {
      throw new AssertionError();
    }
    else if (a.equals(b))
    {
      return;
    }

    throw new AssertionError();
  }

  static void assertNotEquals(Object a, Object b) throws AssertionError
  {
    if ( (a == null) && (b == null) )
    {
      throw new AssertionError();
    }
    else if ( (a == null) || (b == null) )
    {
      return;
    }
    else if (a.equals(b))
    {
      throw new AssertionError();
    }
  }

  static void assertEquals(boolean a, boolean b) throws AssertionError
  {
    if ( a == b )
    {
      return;
    }
    throw new AssertionError();
  }

  static void assertNotEquals(boolean a, boolean b) throws AssertionError
  {
    if ( a != b )
    {
      return;
    }
    throw new AssertionError();
  }

  static void assertEquals(int a, int b) throws AssertionError
  {
    if ( a == b )
    {
      return;
    }
    throw new AssertionError();
  }

  static void assertNotEquals(int a, int b) throws AssertionError
  {
    if ( a != b )
    {
      return;
    }
    throw new AssertionError();
  }
EOF

#    # print patch for class variables/functions
#    print $fh $patchClassTop{$Target}{$cname} if ( $patchClassTop{$Target}{$cname} ); 

    for ( keys %GlobalVariable )
    {
	next unless $_;
	print $fh "  private $_;\n";
    }
    print $fh "\n";
  }
  ##################################################
  # C#
  ##################################################
  elsif ( $Target eq 'csharp')
  {
    $cname =~ s/\.cs$//;
    ################################################## 
    print $fh $head . "\n";
    print $fh "\n";
    print $fh "namespace $CSNamespace.$CurTestDir {\n\n";
    print $fh "  using $ModuleName{'csharp'};\n\n";
    print $fh "  using System;\n\n";    
    print $fh "  using System.IO;\n\n";    
#    print $fh "import junit.framework.TestCase;\n\n";
    ################################################## 

    # print patch for global variables/functions
    print $fh $patchGlobal{$Target}{$cname} if ( $patchGlobal{$Target}{$cname} ); 

#    print $fh "public class $cname extends TestCase {\n\n" ;
    print $fh "  public class $cname {\n" ;
    print $fh "    public class AssertionError : System.Exception \n";
    print $fh "    {\n";
    print $fh "      public AssertionError() : base()\n";
    print $fh "      {\n";
    print $fh "        \n";
    print $fh "      }\n";
    print $fh "    }\n";
    print $fh "\n";

    print $fh <<"EOF";

    static void assertTrue(bool condition)
    {
      if (condition == true)
      {
        return;
      }
      throw new AssertionError();
    }

    static void assertEquals(object a, object b)
    {
      if ( (a == null) && (b == null) )
      {
        return;
      }
      else if ( (a == null) || (b == null) )
      {
        throw new AssertionError();
      }
      else if (a.Equals(b))
      {
        return;
      }
  
      throw new AssertionError();
    }

    static void assertNotEquals(object a, object b)
    {
      if ( (a == null) && (b == null) )
      {
        throw new AssertionError();
      }
      else if ( (a == null) || (b == null) )
      {
        return;
      }
      else if (a.Equals(b))
      {
        throw new AssertionError();
      }
    }

    static void assertEquals(bool a, bool b)
    {
      if ( a == b )
      {
        return;
      }
      throw new AssertionError();
    }

    static void assertNotEquals(bool a, bool b)
    {
      if ( a != b )
      {
        return;
      }
      throw new AssertionError();
    }

    static void assertEquals(int a, int b)
    {
      if ( a == b )
      {
        return;
      }
      throw new AssertionError();
    }

    static void assertNotEquals(int a, int b)
    {
      if ( a != b )
      {
        return;
      }
      throw new AssertionError();
    }

EOF


#    # print patch for class variables/functions
#    print $fh $patchClassTop{$Target}{$cname} if ( $patchClassTop{$Target}{$cname} ); 

    for ( keys %GlobalVariable )
    {
	next unless $_;
	print $fh "    private $_;\n";
    }
    print $fh "\n";
  }
  ##################################################
}

sub writeFooter
{
  my ($fh,$bname) = @_;


  ##################################################
  # Ruby
  ##################################################
  if ( $Target eq 'ruby')
  {
    print $fh "end\n";
  }
  ##################################################
  # Python
  ##################################################
  elsif ( $Target eq 'python')
  {
    my $tail = <<"EOF";
def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite($bname))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
EOF
    print $fh $tail;
  }
  ##################################################
  # Java
  ##################################################
  elsif ( $Target eq 'java')
  {
    my $tail = <<'EOF';
  /**
   * Loads the SWIG-generated libSBML Java module when this class is
   * loaded, or reports a sensible diagnostic message about why it failed.
   */
  static
  {
    String varname;
    String shlibname;

    if (System.getProperty("mrj.version") != null)
    {
      varname = "DYLD_LIBRARY_PATH";    // We're on a Mac.
      shlibname = "libsbmlj.jnilib and/or libsbml.dylib";
    }
    else
    {
      varname = "LD_LIBRARY_PATH";      // We're not on a Mac.
      shlibname = "libsbmlj.so and/or libsbml.so";
    }

    try
    {
      System.loadLibrary("sbmlj");
      // For extra safety, check that the jar file is in the classpath.
      Class.forName("org.sbml.libsbml.libsbml");
    }
    catch (SecurityException e)
    {
      e.printStackTrace();
      System.err.println("Could not load the libSBML library files due to a"+
                         " security exception.\n");
      System.exit(1);
    }
    catch (UnsatisfiedLinkError e)
    {
      e.printStackTrace();
      System.err.println("Error: could not link with the libSBML library files."+
                         " It is likely\nyour " + varname +
                         " environment variable does not include the directories\n"+
                         "containing the " + shlibname + " library files.\n");
      System.exit(1);
    }
    catch (ClassNotFoundException e)
    {
      e.printStackTrace();
      System.err.println("Error: unable to load the file libsbmlj.jar."+
                         " It is likely\nyour -classpath option and CLASSPATH" +
                         " environment variable\n"+
                         "do not include the path to libsbmlj.jar.\n");
      System.exit(1);
    }
  }
}
EOF
    print $fh $tail;
}
	  ##################################################
	  # C#
	  ##################################################
	  elsif ( $Target eq 'csharp')
	  {
		  my $tail = <<'EOF';
  }
}
EOF
    print $fh $tail;
  }
}

######################################################################
#
#  Patches
#
######################################################################

sub initPatch 
{
######################################################################
# Ruby
######################################################################

if ($Target eq 'ruby' )
{

#--------------------------------------------------

$patchFuncHead{'ruby'}{'TestReadSBML'}{'test_ReadSBML_line_col_numbers'} = <<'EOF';
  setXMLParser
EOF

#--------------------------------------------------

$patchClassTop{'ruby'}{'TestReadSBML'} = <<'EOF';
  @@USE_LIBXML = 0
  @@USE_EXPAT  = 0
  @@USE_XERCES = 0

  def setXMLParser
    make_config = "../../../config/makefile-common-vars.mk"

    File.foreach(make_config) do |line|
      @@USE_EXPAT  = 1 if line =~ /^ USE_EXPAT  \s* = \s* 1/x
      @@USE_LIBXML = 1 if line =~ /^ USE_LIBXML \s* = \s* 1/x
      @@USE_XERCES = 1 if line =~ /^ USE_XERCES \s* = \s* 1/x
    end
  end

EOF

#--------------------------------------------------

$patchClassTop{'ruby'}{'TestWriteSBML'} = <<'EOF';
  def util_NaN
    z = 0.0
    return 0.0/z
  end

  def util_PosInf
    z = 0.0
    return 1.0/z
  end

  def util_NegInf
    z = 0.0
    return -1.0/z
  end

  def equals(*x)
    case x.size
    when 2
      e, s = x
      return e == s
    when 1
      e, = x
      return e == @@oss.str()
    end
  end

EOF

$patchClassTop{'ruby'}{'TestXMLAttributes'} = $patchClassTop{'ruby'}{'TestWriteSBML'}; 
$patchClassTop{'ruby'}{'TestWriteMathML'}   = $patchClassTop{'ruby'}{'TestWriteSBML'}; 
$patchClassTop{'ruby'}{'TestWriteL3SBML'}   = $patchClassTop{'ruby'}{'TestWriteSBML'}; 

$patchClassTop{'ruby'}{'TestRDFAnnotation'}  = $patchClassTop{'ruby'}{'TestWriteSBML'}; 
$patchClassTop{'ruby'}{'TestRDFAnnotation2'} = $patchClassTop{'ruby'}{'TestWriteSBML'}; 

$patchClassTop{'ruby'}{'TestReadFromFile9'} = <<'EOF';
  def isnan(x)
    return (x != x)
  end
EOF

$patchClassTop{'ruby'}{'TestL3Compartment'}      = $patchClassTop{'ruby'}{'TestReadFromFile9'};
$patchClassTop{'ruby'}{'TestL3Unit'}             = $patchClassTop{'ruby'}{'TestReadFromFile9'};
$patchClassTop{'ruby'}{'TestL3Parameter'}        = $patchClassTop{'ruby'}{'TestReadFromFile9'};
$patchClassTop{'ruby'}{'TestL3Species'}          = $patchClassTop{'ruby'}{'TestReadFromFile9'};
$patchClassTop{'ruby'}{'TestL3SpeciesReference'} = $patchClassTop{'ruby'}{'TestReadFromFile9'};

#--------------------------------------------------

$patchClassTop{'ruby'}{'TestReadMathML'} = <<'EOF';
  def util_isInf(*x)
    e, = x 
    return ( e == util_PosInf() || e == util_NegInf() )
  end
EOF

$patchClassTop{'ruby'}{'TestReadMathML'} .=  $patchClassTop{'ruby'}{'TestWriteSBML'};

#--------------------------------------------------

$patchClassTop{'ruby'}{'TestASTNode'} = <<'EOF';
  @@DBL_EPSILON =  2.2204460492503131e-16
EOF

$patchClassTop{'ruby'}{'TestL3Unit'} .= <<'EOF';
  @@SBML_INT_MAX = 2147483647
EOF

$patchClassTop{'ruby'}{'TestSBase_newSetters'} .= <<'EOF';
  @@SBML_INT_MAX = 2147483647
EOF

$patchClassTop{'ruby'}{'TestReadFromFile9'} .= <<'EOF';
  @@SBML_INT_MAX = 2147483647
EOF
#--------------------------------------------------

$patchFuncReplace{'ruby'}{'TestWriteSBML'}{'test_WriteSBML_gzip'} = <<'EOF';
    file = [
                        "../../../examples/sample-models/from-spec/level-2/algebraicrules.xml",
                        "../../../examples/sample-models/from-spec/level-2/assignmentrules.xml",
                        "../../../examples/sample-models/from-spec/level-2/boundarycondition.xml",
                        "../../../examples/sample-models/from-spec/level-2/delay.xml",
                        "../../../examples/sample-models/from-spec/level-2/dimerization.xml",
                        "../../../examples/sample-models/from-spec/level-2/enzymekinetics.xml",
                        "../../../examples/sample-models/from-spec/level-2/events.xml",
                        "../../../examples/sample-models/from-spec/level-2/functiondef.xml",
                        "../../../examples/sample-models/from-spec/level-2/multicomp.xml",
                        "../../../examples/sample-models/from-spec/level-2/overdetermined.xml",
                        "../../../examples/sample-models/from-spec/level-2/twodimensional.xml",
                        "../../../examples/sample-models/from-spec/level-2/units.xml"
    ]
    gzfile = "test.xml.gz"
    file.each do |f|
      d = LibSBML::readSBML(f)
      assert( d != nil )
      if not LibSBML::SBMLWriter::hasZlib()
        assert( LibSBML::writeSBML(d,gzfile) == 0 )
        d = nil
        next
      end
      result = LibSBML::writeSBML(d,gzfile)
      assert_equal 1, result
      dg = LibSBML::readSBML(gzfile)
      assert( dg != nil )
      assert( ( dg.toSBML() != d.toSBML() ) == false )
      d = nil
      dg = nil
    end 
  end
EOF

$patchFuncReplace{'ruby'}{'TestWriteSBML'}{'test_WriteSBML_bzip2'} =  $patchFuncReplace{'ruby'}{'TestWriteSBML'}{'test_WriteSBML_gzip'};
$patchFuncReplace{'ruby'}{'TestWriteSBML'}{'test_WriteSBML_bzip2'} =~ s/gz/bz2/g;
$patchFuncReplace{'ruby'}{'TestWriteSBML'}{'test_WriteSBML_bzip2'} =~ s/hasZlib/hasBzip2/g;
  

$patchFuncReplace{'ruby'}{'TestWriteSBML'}{'test_WriteSBML_zip'} =  $patchFuncReplace{'ruby'}{'TestWriteSBML'}{'test_WriteSBML_gzip'};
$patchFuncReplace{'ruby'}{'TestWriteSBML'}{'test_WriteSBML_zip'} =~ s/gz/zip/g;


$patchFuncReplace{'ruby'}{'TestWriteL3SBML'}{'test_WriteL3SBML_gzip'} = $patchFuncReplace{'ruby'}{'TestWriteSBML'}{'test_WriteSBML_gzip'}; 
$patchFuncReplace{'ruby'}{'TestWriteL3SBML'}{'test_WriteL3SBML_gzip'} =~ s/level-2/level-3/g;

$patchFuncReplace{'ruby'}{'TestWriteL3SBML'}{'test_WriteL3SBML_bzip2'} = $patchFuncReplace{'ruby'}{'TestWriteSBML'}{'test_WriteSBML_bzip2'}; 
$patchFuncReplace{'ruby'}{'TestWriteL3SBML'}{'test_WriteL3SBML_bzip2'} =~ s/level-2/level-3/g;

$patchFuncReplace{'ruby'}{'TestWriteL3SBML'}{'test_WriteL3SBML_zip'} = $patchFuncReplace{'ruby'}{'TestWriteSBML'}{'test_WriteSBML_zip'}; 
$patchFuncReplace{'ruby'}{'TestWriteL3SBML'}{'test_WriteL3SBML_zip'} =~ s/level-2/level-3/g;

#--------------------------------------------------
$patchClassTop{'ruby'}{'TestL3ModelHistory'} = <<'EOF';
  def equals(*x)
    case x.size
    when 2
      e, s = x
      return e == s
    when 1
      e, = x
      return e == @@oss.str()
    end
  end

EOF

}
######################################################################
# Python
######################################################################
elsif ( $Target eq 'python' )
{

#--------------------------------------------------

$patchFuncHead{'python'}{'TestReadSBML'}{'test_ReadSBML_line_col_numbers'} = <<'EOF';
  setXMLParser()
EOF

#--------------------------------------------------

$patchGlobal{'python'}{'TestReadSBML'} = <<'EOF';
import re

USE_LIBXML = 0
USE_EXPAT  = 0
USE_XERCES = 0

def setXMLParser():
  make_config = "../../../config/makefile-common-vars.mk"

  global USE_LIBXML 
  global USE_EXPAT  
  global USE_XERCES 

  re_expat  = re.compile('^ USE_EXPAT   \s* = \s* 1', re.X)
  re_libxml = re.compile('^ USE_LIBXML  \s* = \s* 1', re.X)
  re_xerces = re.compile('^ USE_XERCES  \s* = \s* 1', re.X)

  f = open(make_config)
  for line in f:
    if re_expat.match(line)  : USE_EXPAT   = 1 
    if re_libxml.match(line) : USE_LIBXML  = 1 
    if re_xerces.match(line) : USE_XERCES  = 1 

def wrapString(s):
  return s
  pass

EOF

#--------------------------------------------------

$patchGlobal{'python'}{'TestWriteSBML'} = <<'EOF';
def util_NaN():
  z = 1e300
  z = z * z

  return z - z

def util_PosInf():
  z = 1e300
  z = z * z

  return z

def util_NegInf():
  z = 1e300
  z = z * z

  return -z 

def wrapString(s):
  return s
  pass

EOF

$patchGlobal{'python'}{'TestXMLAttributes'} = $patchGlobal{'python'}{'TestWriteSBML'}; 
$patchGlobal{'python'}{'TestWriteMathML'}   = $patchGlobal{'python'}{'TestWriteSBML'}; 
$patchGlobal{'python'}{'TestWriteL3SBML'}   = $patchGlobal{'python'}{'TestWriteSBML'}; 

$patchGlobal{'python'}{'TestReadFromFile9'} = <<'EOF';
def isnan(x):
  return (x != x)
  pass
EOF

$patchGlobal{'python'}{'TestL3Compartment'}      = $patchGlobal{'python'}{'TestReadFromFile9'};
$patchGlobal{'python'}{'TestL3Unit'}             = $patchGlobal{'python'}{'TestReadFromFile9'};
$patchGlobal{'python'}{'TestL3Parameter'}        = $patchGlobal{'python'}{'TestReadFromFile9'};
$patchGlobal{'python'}{'TestL3Species'}          = $patchGlobal{'python'}{'TestReadFromFile9'};
$patchGlobal{'python'}{'TestL3SpeciesReference'} = $patchGlobal{'python'}{'TestReadFromFile9'};

$patchClassTop{'python'}{'TestWriteSBML'} = <<'EOF';
  def equals(self, *x):
    if len(x) == 2:
      return x[0] == x[1]
    elif len(x) == 1:
      return x[0] == self.OSS.str()

EOF

$patchClassTop{'python'}{'TestL3ModelHistory'} = <<'EOF';
  def equals(self, *x):
    if len(x) == 2:
      return x[0] == x[1]
    elif len(x) == 1:
      return x[0] == self.OSS.str()

EOF

$patchClassTop{'python'}{'TestWriteMathML'}    = $patchClassTop{'python'}{'TestWriteSBML'}; 
$patchClassTop{'python'}{'TestRDFAnnotation'}  = $patchClassTop{'python'}{'TestWriteSBML'}; 
$patchClassTop{'python'}{'TestRDFAnnotation2'} = $patchClassTop{'python'}{'TestWriteSBML'}; 
$patchClassTop{'python'}{'TestWriteL3SBML'}    = $patchClassTop{'python'}{'TestWriteSBML'}; 

$patchClassTop{'python'}{'TestSBMLConstructorException'} = <<'EOF';
  currently_not_converted_by_ctest_converter = 1


EOF

$patchClassTop{'python'}{'TestXMLErrorLog'} = <<'EOF';
  currently_not_converted_by_ctest_converter = 1


EOF

#--------------------------------------------------

$patchGlobal{'python'}{'TestReadMathML'} = <<'EOF';
def util_isInf(*x):
  return ( (x[0] == util_PosInf()) or  (x[0] == util_NegInf()) )

EOF
$patchGlobal{'python'}{'TestReadMathML'} .=  $patchGlobal{'python'}{'TestWriteSBML'};

#--------------------------------------------------

$patchGlobal{'python'}{'TestSBase'} = <<'EOF';
def wrapString(s):
  return s
  pass

EOF

$patchGlobal{'python'}{'TestXMLInputStream'} = $patchGlobal{'python'}{'TestSBase'};
$patchGlobal{'python'}{'TestXMLOutputStream'} = $patchGlobal{'python'}{'TestSBase'};
$patchGlobal{'python'}{'TestXMLNode'} = $patchGlobal{'python'}{'TestSBase'};
$patchGlobal{'python'}{'TestRDFAnnotation'} = $patchGlobal{'python'}{'TestSBase'};
$patchGlobal{'python'}{'TestRDFAnnotation2'} = $patchGlobal{'python'}{'TestSBase'};
$patchGlobal{'python'}{'TestSBase_newSetters'} = $patchGlobal{'python'}{'TestSBase'};

#--------------------------------------------------

$patchGlobal{'python'}{'TestASTNode'} = <<'EOF';
DBL_EPSILON =  2.2204460492503131e-16

EOF

$patchGlobal{'python'}{'TestL3Unit'} .= <<'EOF';
SBML_INT_MAX = 2147483647
EOF

$patchGlobal{'python'}{'TestSBase_newSetters'} .= <<'EOF';
SBML_INT_MAX = 2147483647
EOF

$patchGlobal{'python'}{'TestReadFromFile9'} .= <<'EOF';
SBML_INT_MAX = 2147483647
EOF
#--------------------------------------------------

$patchFuncReplace{'python'}{'TestWriteSBML'}{'test_WriteSBML_gzip'} = <<'EOF';
    file = []
    file.append("../../../examples/sample-models/from-spec/level-2/algebraicrules.xml")
    file.append("../../../examples/sample-models/from-spec/level-2/assignmentrules.xml")
    file.append("../../../examples/sample-models/from-spec/level-2/boundarycondition.xml")
    file.append("../../../examples/sample-models/from-spec/level-2/delay.xml")
    file.append("../../../examples/sample-models/from-spec/level-2/dimerization.xml")
    file.append("../../../examples/sample-models/from-spec/level-2/enzymekinetics.xml")
    file.append("../../../examples/sample-models/from-spec/level-2/events.xml")
    file.append("../../../examples/sample-models/from-spec/level-2/functiondef.xml")
    file.append("../../../examples/sample-models/from-spec/level-2/multicomp.xml")
    file.append("../../../examples/sample-models/from-spec/level-2/overdetermined.xml")
    file.append("../../../examples/sample-models/from-spec/level-2/twodimensional.xml")
    file.append("../../../examples/sample-models/from-spec/level-2/units.xml")

    gzfile = "test.xml.gz"
    for f in file:
      d = libsbml.readSBML(f)
      self.assertIsNotNone( d )
      if not libsbml.SBMLWriter.hasZlib():
        self.assertEqual( libsbml.writeSBML(d,gzfile), 0 )
        d = None
        continue
      result = libsbml.writeSBML(d,gzfile)
      self.assertEqual( 1, result )
      dg = libsbml.readSBML(gzfile)
      self.assertIsNotNone( dg )
      self.assertEqual( dg.toSBML(), d.toSBML() )
      d = None
      dg = None
    pass
EOF

$patchFuncReplace{'python'}{'TestWriteSBML'}{'test_WriteSBML_bzip2'} = $patchFuncReplace{'python'}{'TestWriteSBML'}{'test_WriteSBML_gzip'};
$patchFuncReplace{'python'}{'TestWriteSBML'}{'test_WriteSBML_bzip2'} =~ s/gz/bz2/g;
$patchFuncReplace{'python'}{'TestWriteSBML'}{'test_WriteSBML_bzip2'} =~ s/hasZlib/hasBzip2/g;
  

$patchFuncReplace{'python'}{'TestWriteSBML'}{'test_WriteSBML_zip'} =  $patchFuncReplace{'python'}{'TestWriteSBML'}{'test_WriteSBML_gzip'};
$patchFuncReplace{'python'}{'TestWriteSBML'}{'test_WriteSBML_zip'} =~ s/gz/zip/g;


$patchFuncReplace{'python'}{'TestWriteL3SBML'}{'test_WriteL3SBML_gzip'} = $patchFuncReplace{'python'}{'TestWriteSBML'}{'test_WriteSBML_gzip'}; 
$patchFuncReplace{'python'}{'TestWriteL3SBML'}{'test_WriteL3SBML_gzip'} =~ s/level-2/level-3/g;

$patchFuncReplace{'python'}{'TestWriteL3SBML'}{'test_WriteL3SBML_bzip2'} = $patchFuncReplace{'python'}{'TestWriteSBML'}{'test_WriteSBML_bzip2'}; 
$patchFuncReplace{'python'}{'TestWriteL3SBML'}{'test_WriteL3SBML_bzip2'} =~ s/level-2/level-3/g;

$patchFuncReplace{'python'}{'TestWriteL3SBML'}{'test_WriteL3SBML_zip'} = $patchFuncReplace{'python'}{'TestWriteSBML'}{'test_WriteSBML_zip'}; 
$patchFuncReplace{'python'}{'TestWriteL3SBML'}{'test_WriteL3SBML_zip'} =~ s/level-2/level-3/g;

$patchGlobal{'python'}{'TestL3ModelHistory'} = <<'EOF';
def wrapString(s):
  return s
  pass

EOF


#--------------------------------------------------
}
######################################################################
# Java
######################################################################
elsif ( $Target eq 'java' )
{

#--------------------------------------------------

$patchFuncHead{'java'}{'TestReadSBML'}{'test_ReadSBML_line_col_numbers'} = <<'EOF';
  setXMLParser();

EOF

#--------------------------------------------------

$patchGlobal{'java'}{'TestReadSBML'} = <<'EOF';
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;

EOF
#--------------------------------------------------

$patchClassTop{'java'}{'TestReadSBML'} = <<'EOF';

  private int USE_LIBXML = 0;
  private int USE_EXPAT  = 0;
  private int USE_XERCES = 0;

  public void setXMLParser() 
  {
    String make_config = "../../../config/makefile-common-vars.mk";

    Pattern pt_expat  = Pattern.compile("^USE_EXPAT\\s*=\\s*1");
    Pattern pt_libxml = Pattern.compile("^USE_LIBXML\\s*=\\s*1");
    Pattern pt_xerces = Pattern.compile("^USE_XERCES\\s*=\\s*1");

    try
    {
      FileInputStream fis = new FileInputStream(make_config);
      InputStreamReader ir = new InputStreamReader(fis);
      BufferedReader br = new BufferedReader(ir);

      String line;
      while( (line = br.readLine()) != null)
      {
        Matcher m; 

        m = pt_libxml.matcher(line);
        if ( m.matches() )
        {
          USE_LIBXML = 1;
        }
  
        m = pt_expat.matcher(line);
        if ( m.matches() )
        {
          USE_EXPAT = 1;
        }

        m = pt_xerces.matcher(line);
        if ( m.matches() )
        {
          USE_XERCES = 1;
        }
      }
    }
    catch (Exception e)
    {
      System.exit(1);    
    }
  }

EOF

#--------------------------------------------------

$patchClassTop{'java'}{'TestWriteSBML'} = <<'EOF';

  public double util_NaN()
  {
    double z = 0.0;
    return 0.0/z;
  }

  public double util_PosInf()
  {
    double z = 0.0;
    return 1.0/z;
  }

  public double util_NegInf()
  {
    double z = 0.0;
    return -1.0/z;
  }

  public boolean equals(String s1, String s2)
  {
    return s1.equals(s2);
  }

EOF

$patchClassTop{'java'}{'TestXMLAttributes'} = <<'EOF';

  public double util_NaN()
  {
    double z = 0.0;
    return 0.0/z;
  }

  public double util_PosInf()
  {
    double z = 0.0;
    return 1.0/z;
  }

  public double util_NegInf()
  {
    double z = 0.0;
    return -1.0/z;
  }

EOF

$patchClassTop{'java'}{'TestWriteMathML'} = $patchClassTop{'java'}{'TestWriteSBML'}; 

$patchClassTop{'java'}{'TestRDFAnnotation'}  = $patchClassTop{'java'}{'TestWriteSBML'}; 
$patchClassTop{'java'}{'TestRDFAnnotation2'} = $patchClassTop{'java'}{'TestWriteSBML'}; 
$patchClassTop{'java'}{'TestReadFileFrom9'}  = $patchClassTop{'java'}{'TestWriteSBML'}; 
$patchClassTop{'java'}{'TestWriteL3SBML'}    = $patchClassTop{'java'}{'TestWriteSBML'}; 

$patchClassTop{'java'}{'TestReadMathML'} = <<'EOF';
  public boolean util_isInf(double x)
  {
    return ( (x == util_PosInf()) ||  (x == util_NegInf()) );
  }

EOF
$patchClassTop{'java'}{'TestReadMathML'} .= $patchClassTop{'java'}{'TestXMLAttributes'}; 

$patchClassTop{'java'}{'TestReadFromFile9'} = <<'EOF';
  public boolean isnan(double x)
  {
    return (x != x);
  }

EOF

$patchClassTop{'java'}{'TestL3Compartment'}      = $patchClassTop{'java'}{'TestReadFromFile9'};
$patchClassTop{'java'}{'TestL3Unit'}             = $patchClassTop{'java'}{'TestReadFromFile9'};
$patchClassTop{'java'}{'TestL3Parameter'}        = $patchClassTop{'java'}{'TestReadFromFile9'};
$patchClassTop{'java'}{'TestL3Species'}          = $patchClassTop{'java'}{'TestReadFromFile9'};
$patchClassTop{'java'}{'TestL3SpeciesReference'} = $patchClassTop{'java'}{'TestReadFromFile9'};

#--------------------------------------------------

$patchClassTop{'java'}{'TestASTNode'} = <<'EOF';
  public static final double DBL_EPSILON =  2.2204460492503131e-016;

EOF

$patchClassTop{'java'}{'TestL3Unit'} .= <<'EOF';
  public static final int SBML_INT_MAX = 2147483647;
EOF

$patchClassTop{'java'}{'TestSBase_newSetters'} .= <<'EOF';
  public static final int SBML_INT_MAX = 2147483647;
EOF

$patchClassTop{'java'}{'TestReadFromFile9'} .= <<'EOF';
  public static final int SBML_INT_MAX = 2147483647;
EOF
#--------------------------------------------------

$patchFuncReplace{'java'}{'TestWriteSBML'}{'test_WriteSBML_gzip'} = <<'EOF';
  
    int filenum = 12;
    String file[] = {
                        "../../../examples/sample-models/from-spec/level-2/algebraicrules.xml",
                        "../../../examples/sample-models/from-spec/level-2/assignmentrules.xml",
                        "../../../examples/sample-models/from-spec/level-2/boundarycondition.xml",
                        "../../../examples/sample-models/from-spec/level-2/delay.xml",
                        "../../../examples/sample-models/from-spec/level-2/dimerization.xml",
                        "../../../examples/sample-models/from-spec/level-2/enzymekinetics.xml",
                        "../../../examples/sample-models/from-spec/level-2/events.xml",
                        "../../../examples/sample-models/from-spec/level-2/functiondef.xml",
                        "../../../examples/sample-models/from-spec/level-2/multicomp.xml",
                        "../../../examples/sample-models/from-spec/level-2/overdetermined.xml",
                        "../../../examples/sample-models/from-spec/level-2/twodimensional.xml",
                        "../../../examples/sample-models/from-spec/level-2/units.xml"
    };
    String gzfile = "test.xml.gz";
    for(int i = 0; i < filenum; i++)
    {
      SBMLDocument d = libsbml.readSBML(file[i]);
      assertTrue( d != null );
      if (! SBMLWriter.hasZlib())
      {
        assertTrue( libsbml.writeSBML(d, gzfile) == 0 );
        d = null;
          continue;
      }
      boolean result = (libsbml.writeSBML(d, gzfile) != 0);
      assertEquals( true, result );
      SBMLDocument dg = libsbml.readSBML(gzfile);
      assertTrue( dg != null );
      assertTrue( !d.toSBML().equals(dg.toSBML()) == false );
      d = null;
      dg = null;
    }
  }
EOF

$patchFuncReplace{'java'}{'TestWriteSBML'}{'test_WriteSBML_bzip2'} = $patchFuncReplace{'java'}{'TestWriteSBML'}{'test_WriteSBML_gzip'};
$patchFuncReplace{'java'}{'TestWriteSBML'}{'test_WriteSBML_bzip2'} =~ s/gz/bz2/g;
$patchFuncReplace{'java'}{'TestWriteSBML'}{'test_WriteSBML_bzip2'} =~ s/hasZlib/hasBzip2/g;
  

$patchFuncReplace{'java'}{'TestWriteSBML'}{'test_WriteSBML_zip'} =  $patchFuncReplace{'java'}{'TestWriteSBML'}{'test_WriteSBML_gzip'};
$patchFuncReplace{'java'}{'TestWriteSBML'}{'test_WriteSBML_zip'} =~ s/gz/zip/g;


$patchFuncReplace{'java'}{'TestWriteL3SBML'}{'test_WriteL3SBML_gzip'} = $patchFuncReplace{'java'}{'TestWriteSBML'}{'test_WriteSBML_gzip'}; 
$patchFuncReplace{'java'}{'TestWriteL3SBML'}{'test_WriteL3SBML_gzip'} =~ s/level-2/level-3/g;

$patchFuncReplace{'java'}{'TestWriteL3SBML'}{'test_WriteL3SBML_bzip2'} = $patchFuncReplace{'java'}{'TestWriteSBML'}{'test_WriteSBML_bzip2'}; 
$patchFuncReplace{'java'}{'TestWriteL3SBML'}{'test_WriteL3SBML_bzip2'} =~ s/level-2/level-3/g;

$patchFuncReplace{'java'}{'TestWriteL3SBML'}{'test_WriteL3SBML_zip'} = $patchFuncReplace{'java'}{'TestWriteSBML'}{'test_WriteSBML_zip'}; 
$patchFuncReplace{'java'}{'TestWriteL3SBML'}{'test_WriteL3SBML_zip'} =~ s/level-2/level-3/g;

#--------------------------------------------------

$patchClassTop{'java'}{'TestL3ModelHistory'} = <<'EOF';

  public boolean equals(String s1, String s2)
  {
    return s1.equals(s2);
  }

EOF


#--------------------------------------------------
}
######################################################################
# C#
######################################################################
elsif ( $Target eq 'csharp' )
{

#--------------------------------------------------

$patchFuncHead{'csharp'}{'TestReadSBML'}{'test_ReadSBML_line_col_numbers'} = <<'EOF';
  //setXMLParser();
EOF

#--------------------------------------------------

#$patchGlobal{'csharp'}{'TestReadSBML'} = <<'EOF';
#using System.Xml;
#EOF

#--------------------------------------------------

$patchClassTop{'csharp'}{'TestReadSBML'} = <<'EOF';

  private int USE_LIBXML = 0;
  private int USE_EXPAT  = 0;
  private int USE_XERCES = 0;  

EOF

#--------------------------------------------------

$patchClassTop{'csharp'}{'TestWriteSBML'} = <<'EOF';

  public double util_NaN()
  {
    double z = 0.0;
    return 0.0/z;
  }

  public double util_PosInf()
  {
    double z = 0.0;
    return 1.0/z;
  }

  public double util_NegInf()
  {
    double z = 0.0;
    return -1.0/z;
  }

//  public bool equals(string s)
//  {
//    return s == OSS.str();
//  }

  public bool equals(string s1, string s2)
  {
    return (s1 ==s2);
  }

EOF


$patchClassTop{'csharp'}{'TestXMLAttributes'} = <<'EOF';

  public double util_NaN()
  {
    double z = 0.0;
    return 0.0/z;
  }

  public double util_PosInf()
  {
    double z = 0.0;
    return 1.0/z;
  }

  public double util_NegInf()
  {
    double z = 0.0;
    return -1.0/z;
  }

EOF


$patchClassTop{'csharp'}{'TestWriteMathML'}   = $patchClassTop{'csharp'}{'TestWriteSBML'}; 

$patchClassTop{'csharp'}{'TestRDFAnnotation'}  = $patchClassTop{'csharp'}{'TestWriteSBML'};
$patchClassTop{'csharp'}{'TestRDFAnnotation2'} = $patchClassTop{'csharp'}{'TestWriteSBML'};

$patchClassTop{'csharp'}{'TestReadFileFrom9'} = $patchClassTop{'csharp'}{'TestWriteSBML'};
$patchClassTop{'csharp'}{'TestWriteL3SBML'}   = $patchClassTop{'csharp'}{'TestWriteSBML'};

$patchClassTop{'csharp'}{'TestReadMathML'} = <<'EOF';
  public bool util_isInf(double x)
  {
    return ( (x == util_PosInf()) ||  (x == util_NegInf()) );
  }

EOF
$patchClassTop{'csharp'}{'TestReadMathML'} .= $patchClassTop{'csharp'}{'TestXMLAttributes'};

$patchClassTop{'csharp'}{'TestReadFromFile9'} = <<'EOF';
    public bool isnan(double x)
    {
      return (x != x);
    }

EOF

$patchClassTop{'csharp'}{'TestL3Compartment'}      = $patchClassTop{'csharp'}{'TestReadFromFile9'};
$patchClassTop{'csharp'}{'TestL3Unit'}             = $patchClassTop{'csharp'}{'TestReadFromFile9'};
$patchClassTop{'csharp'}{'TestL3Parameter'}        = $patchClassTop{'csharp'}{'TestReadFromFile9'};
$patchClassTop{'csharp'}{'TestL3Species'}          = $patchClassTop{'csharp'}{'TestReadFromFile9'};
$patchClassTop{'csharp'}{'TestL3SpeciesReference'} = $patchClassTop{'csharp'}{'TestReadFromFile9'};


#--------------------------------------------------

$patchClassTop{'csharp'}{'TestASTNode'} = <<'EOF';
  private const double DBL_EPSILON =  2.2204460492503131e-016;

EOF

$patchClassTop{'csharp'}{'TestL3Unit'} .= <<'EOF';
  private const int SBML_INT_MAX = 2147483647;
EOF

$patchClassTop{'csharp'}{'TestSBase_newSetters'} .= <<'EOF';
  private const int SBML_INT_MAX = 2147483647;
EOF

$patchClassTop{'csharp'}{'TestReadFromFile9'} .= <<'EOF';
  private const int SBML_INT_MAX = 2147483647;
EOF
#--------------------------------------------------

$patchFuncReplace{'csharp'}{'TestWriteSBML'}{'test_WriteSBML_gzip'} = <<'EOF';
  
      uint filenum = 12;
      string[] file = {
                        "../../../examples/sample-models/from-spec/level-2/algebraicrules.xml",
                        "../../../examples/sample-models/from-spec/level-2/assignmentrules.xml",
                        "../../../examples/sample-models/from-spec/level-2/boundarycondition.xml",
                        "../../../examples/sample-models/from-spec/level-2/delay.xml",
                        "../../../examples/sample-models/from-spec/level-2/dimerization.xml",
                        "../../../examples/sample-models/from-spec/level-2/enzymekinetics.xml",
                        "../../../examples/sample-models/from-spec/level-2/events.xml",
                        "../../../examples/sample-models/from-spec/level-2/functiondef.xml",
                        "../../../examples/sample-models/from-spec/level-2/multicomp.xml",
                        "../../../examples/sample-models/from-spec/level-2/overdetermined.xml",
                        "../../../examples/sample-models/from-spec/level-2/twodimensional.xml",
                        "../../../examples/sample-models/from-spec/level-2/units.xml"
      };
      string gzfile = "test.xml.gz";
      for(uint i = 0; i < filenum; i++) 
      { 
        SBMLDocument d = libsbml.readSBML(file[i]); 
        assertTrue( d != null );
        if (! SBMLWriter.hasZlib())
        {
          assertTrue( libsbml.writeSBML(d, gzfile) == 0 );
          d = null;
          continue;
        }
        int result = libsbml.writeSBML(d, gzfile);
        assertTrue( result != 0);
        SBMLDocument dg = libsbml.readSBML(gzfile);
        assertTrue( dg != null );
        assertTrue( ( dg.toSBML() != d.toSBML() ) == false );
        d = null;
        dg = null;
      }
  }
EOF

$patchFuncReplace{'csharp'}{'TestWriteSBML'}{'test_WriteSBML_bzip2'} = $patchFuncReplace{'csharp'}{'TestWriteSBML'}{'test_WriteSBML_gzip'};
$patchFuncReplace{'csharp'}{'TestWriteSBML'}{'test_WriteSBML_bzip2'} =~ s/gz/bz2/g;
$patchFuncReplace{'csharp'}{'TestWriteSBML'}{'test_WriteSBML_bzip2'} =~ s/hasZlib/hasBzip2/g;
  

$patchFuncReplace{'csharp'}{'TestWriteSBML'}{'test_WriteSBML_zip'} =  $patchFuncReplace{'csharp'}{'TestWriteSBML'}{'test_WriteSBML_gzip'};
$patchFuncReplace{'csharp'}{'TestWriteSBML'}{'test_WriteSBML_zip'} =~ s/gz/zip/g;


$patchFuncReplace{'csharp'}{'TestWriteL3SBML'}{'test_WriteL3SBML_gzip'} = $patchFuncReplace{'csharp'}{'TestWriteSBML'}{'test_WriteSBML_gzip'}; 
$patchFuncReplace{'csharp'}{'TestWriteL3SBML'}{'test_WriteL3SBML_gzip'} =~ s/level-2/level-3/g;

$patchFuncReplace{'csharp'}{'TestWriteL3SBML'}{'test_WriteL3SBML_bzip2'} = $patchFuncReplace{'csharp'}{'TestWriteSBML'}{'test_WriteSBML_bzip2'}; 
$patchFuncReplace{'csharp'}{'TestWriteL3SBML'}{'test_WriteL3SBML_bzip2'} =~ s/level-2/level-3/g;

$patchFuncReplace{'csharp'}{'TestWriteL3SBML'}{'test_WriteL3SBML_zip'} = $patchFuncReplace{'csharp'}{'TestWriteSBML'}{'test_WriteSBML_zip'}; 
$patchFuncReplace{'csharp'}{'TestWriteL3SBML'}{'test_WriteL3SBML_zip'} =~ s/level-2/level-3/g;

#--------------------------------------------------
$patchClassTop{'csharp'}{'TestL3ModelHistory'} = <<'EOF';

  public bool equals(string s1, string s2)
  {
    return (s1 ==s2);
  }

EOF

}

######################################################################


}
 


