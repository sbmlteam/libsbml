/**
 * @file    local.i
 * @brief   cs-specific SWIG directives for wrapping libSBML API this file 
 *          has been adapted from the SWIG java bindings written by 
 * 	    Ben Bornstein and Akiya Jouraku
 * @author  Frank Bergmann (fbergman@u.washington.edu)
 * @author  Akiya Jouraku
 *
 * $ID:$
 * $HeadURL:$
 */

/*
 *This file is part of libSBML.  Please visit http://sbml.org for more
 *information about SBML, and the latest version of libSBML.
 *
 *Copyright 2008 California Institute of Technology.
 *
 *This library is free software; you can redistribute it and/or modify it
 *under the terms of the GNU Lesser General Public License as published by
 *the Free Software Foundation.  A copy of the license agreement is provided
 *in the file named "LICENSE.txt" included with this software distribution
 *and also available online as http://sbml.org/software/libsbml/license.html
 */

%include "std_string.i"
%include "std_wstring.i"

%include "exception.i"
%include <typemaps.i>



%include "enumsimple.swg"
%csconst(1);


%csconstvalue("'+'") AST_PLUS;
%csconstvalue("'-'") AST_MINUS;
%csconstvalue("'*'") AST_TIMES;
%csconstvalue("'/'") AST_DIVIDE;
%csconstvalue("'^'") AST_POWER;

%{
#include <iostream>
#include <fstream>
#include <sstream>
%}

////////////////////////////////////////////////////////////////////////////////
//
// Changes default behaviour for converting string variables between 
// C# side and C++ side.
//
// -----------------------------------------------------------------------------
// (default)  
// C# string (Unicode (wide char)) -> C++ char* (ANSI CP (multibyte char))
// 
// (changed)  
// C# string (Unicode (wide char)) -> C++ wchar_t* (Unicode (wide char)) 
// -> C++ char* (UTF8 (multibyte char))
// -----------------------------------------------------------------------------
//
// By default, C# Unicode string is converted to C++ ANSI CP string (not UTF8 
// string) and this leads to an invalid encoding error in libSBML API which 
// requires UTF8 string.
// To avoid this problem, the following typemap directive changes the behaviour 
// of string conversion to pass UTF8 string to libSBML C++ API. 
// Since there seems to be no way to directly convert C# Unicode string to C++ 
// UTF8 string, C# Unicode string is converted to C++ Unicode character (wchar_t*) 
// and then converted to UTF8 character (char*). 
//

#ifdef SWIGWIN 

%define SWIGCSHARP_IMTYPE_WSTRING(TYPENAME)
%typemap(imtype, 
         inattributes="[MarshalAs(UnmanagedType.LPWStr)]", 
         outattributes="[return: MarshalAs(UnmanagedType.LPWStr)]" 
        ) TYPENAME "string"
%enddef

SWIGCSHARP_IMTYPE_WSTRING(std::string)
SWIGCSHARP_IMTYPE_WSTRING(std::string&)
SWIGCSHARP_IMTYPE_WSTRING(const std::string&)
SWIGCSHARP_IMTYPE_WSTRING(std::string*)
SWIGCSHARP_IMTYPE_WSTRING(const std::string*)
SWIGCSHARP_IMTYPE_WSTRING(char*)
SWIGCSHARP_IMTYPE_WSTRING(const char*)

//
// In SWIG-1.3.35, a callback function for a returned wide string (implemented in 
// SWIGWStringHelper class) doesn't work when the given Unicode string converted 
// from UTF8 string (the callback function is used in libsbml_wrap.cpp when 
// returning an Unicode character).
// So, currently, the SWIGWStringHelper class is modified as follows.
//
// (NOTICE) 
//  To disable the default SWIGStringHelper class, SWIG_CSHARP_NO_WSTRING_HELPER 
//  needs to be defined by passing  -DSWIG_CSHARP_NO_WSTRING_HELPER  to SWIG command 
//  line.
//

%insert(runtime) %{
/* Callback for returning strings to C# without leaking memory */
typedef void * (SWIGSTDCALL* SWIG_CSharpWStringHelperCallback)(const wchar_t *);
static SWIG_CSharpWStringHelperCallback SWIG_csharp_wstring_callback = NULL;
%}

%pragma(csharp) imclasscode=%{
  protected class SWIGWStringHelper {

    public delegate IntPtr SWIGWStringDelegate(IntPtr message);
    static SWIGWStringDelegate wstringDelegate = new SWIGWStringDelegate(CreateWString);

    [DllImport("$dllimport", EntryPoint="SWIGRegisterWStringCallback_$module")]
    public static extern void SWIGRegisterWStringCallback_$module(SWIGWStringDelegate wstringDelegate);

    static IntPtr CreateWString([MarshalAs(UnmanagedType.LPWStr)]IntPtr cString) {
      string ustr = System.Runtime.InteropServices.Marshal.PtrToStringUni(cString);
      return System.Runtime.InteropServices.Marshal.StringToHGlobalUni(ustr);
    }

    static SWIGWStringHelper() {
      SWIGRegisterWStringCallback_$module(wstringDelegate);
    }
  }

  static protected SWIGWStringHelper swigWStringHelper = new SWIGWStringHelper();
%}

%insert(runtime) %{
#ifdef __cplusplus
extern "C"
#endif
SWIGEXPORT void SWIGSTDCALL SWIGRegisterWStringCallback_$module(SWIG_CSharpWStringHelperCallback callback) {
  SWIG_csharp_wstring_callback = callback;
}
%}

#endif //SWIGWIN

//////////////////////////////////////////////////////////////////////
//
// typemap between "unsigned int (C++)" and "long (C#)"
//
// The following typemap directives for the above conversion have been
// added with CLS-compliant in mind. 
// (uint can not be used in CLS-compliant API)
//
//////////////////////////////////////////////////////////////////////

// mapping for a type of function argument in libsbml_wrap.cpp
%typemap(ctype,  out="unsigned int")        unsigned int        "long long"
%typemap(ctype,  out="const unsigned int&") const unsigned int& "const long long&"

// mapping for a type of function argument in csharp-files/libsbmlPINVOKE.cs
%typemap(imtype, out="uint") unsigned int        "long"
%typemap(imtype, out="uint") const unsigned int& "long"

// mapping for a type of function argument in csharp-files/*.cs (C# proxy classes)
%typemap(cstype) unsigned int        "long"
%typemap(cstype) const unsigned int& "long"

// conversion for a given argument in libsbml_wrap.cpp
%typemap(in)     unsigned int        { $1 = (unsigned int)$input;  }
%typemap(in)     const unsigned int& { $1 = (unsigned int*)$input; }

// conversion for a returned value in csharp-files/*.cs (C# proxy classes)
%typemap(csout)  unsigned int        { return (long)$imcall; }
%typemap(csout)  const unsigned int& { return (long)$imcall; }

//////////////////////////////////////////////////////////////////////

#ifndef USE_LAYOUT

/**
 * @return the most specific c# object possible for the given SBase
 * object.
 */
%pragma(csharp) modulecode =
%{
	
	public static SBase DowncastSBase(IntPtr cPtr, bool owner)
	{
		if (cPtr.Equals(IntPtr.Zero)) return null;
		
		switch( libsbmlPINVOKE.SBase_getTypeCode(new HandleRef(null, cPtr)) )
		{
			case (int) libsbml.SBML_COMPARTMENT:
				return new Compartment(cPtr, owner);
				
			case (int) libsbml.SBML_DOCUMENT:
				return new SBMLDocument(cPtr, owner);
				
			case (int) libsbml.SBML_EVENT:
				return new Event(cPtr, owner);
				
			case (int) libsbml.SBML_EVENT_ASSIGNMENT:
				return new EventAssignment(cPtr, owner);
				
			case (int) libsbml.SBML_FUNCTION_DEFINITION:
				return new FunctionDefinition(cPtr, owner);
				
			case (int) libsbml.SBML_KINETIC_LAW:
				return new KineticLaw(cPtr, owner);
				
			case (int) libsbml.SBML_LIST_OF:
				return new ListOf(cPtr, owner);
				
			case (int) libsbml.SBML_MODEL:
				return new Model(cPtr, owner);
				
			case (int) libsbml.SBML_PARAMETER:
				return new Parameter(cPtr, owner);
				
			case (int) libsbml.SBML_REACTION:
				return new Reaction(cPtr, owner);
				
			case (int) libsbml.SBML_SPECIES:
				return new Species(cPtr, owner);
				
			case (int) libsbml.SBML_SPECIES_REFERENCE:
				return new SpeciesReference(cPtr, owner);
				
			case (int) libsbml.SBML_MODIFIER_SPECIES_REFERENCE:
				return new ModifierSpeciesReference(cPtr, owner);
				
			case (int) libsbml.SBML_UNIT_DEFINITION:
				return new UnitDefinition(cPtr, owner);
				
			case (int) libsbml.SBML_UNIT:
				return new Unit(cPtr, owner);
				
			case (int) libsbml.SBML_ALGEBRAIC_RULE:
				return new AlgebraicRule(cPtr, owner);
				
			case (int) libsbml.SBML_ASSIGNMENT_RULE:
				return new AssignmentRule(cPtr, owner);
				
			case (int) libsbml.SBML_RATE_RULE:
				return new RateRule(cPtr, owner);
				
			default:
				return new SBase(cPtr, owner);
		}
	}
%}

#else

/**
 * @return the most specific c# object possible for the given SBase
 * object.
 */
%pragma(csharp) modulecode =
%{
	
	public static SBase DowncastSBase(IntPtr cPtr, bool owner)
	{
		if (cPtr.Equals(IntPtr.Zero)) return null;
		
                switch( libsbmlPINVOKE.SBase_getTypeCode(new HandleRef(null, cPtr)) )
		{
			case (int) libsbml.SBML_COMPARTMENT:
				return new Compartment(cPtr, owner);
				
			case (int) libsbml.SBML_DOCUMENT:
				return new SBMLDocument(cPtr, owner);
				
			case (int) libsbml.SBML_EVENT:
				return new Event(cPtr, owner);
				
			case (int) libsbml.SBML_EVENT_ASSIGNMENT:
				return new EventAssignment(cPtr, owner);
				
			case (int) libsbml.SBML_FUNCTION_DEFINITION:
				return new FunctionDefinition(cPtr, owner);
				
			case (int) libsbml.SBML_KINETIC_LAW:
				return new KineticLaw(cPtr, owner);
				
			case (int) libsbml.SBML_LIST_OF:
				return new ListOf(cPtr, owner);
				
			case (int) libsbml.SBML_MODEL:
				return new Model(cPtr, owner);
				
			case (int) libsbml.SBML_PARAMETER:
				return new Parameter(cPtr, owner);
				
			case (int) libsbml.SBML_REACTION:
				return new Reaction(cPtr, owner);
				
			case (int) libsbml.SBML_SPECIES:
				return new Species(cPtr, owner);
				
			case (int) libsbml.SBML_SPECIES_REFERENCE:
				return new SpeciesReference(cPtr, owner);
				
			case (int) libsbml.SBML_MODIFIER_SPECIES_REFERENCE:
				return new ModifierSpeciesReference(cPtr, owner);
				
			case (int) libsbml.SBML_UNIT_DEFINITION:
				return new UnitDefinition(cPtr, owner);
				
			case (int) libsbml.SBML_UNIT:
				return new Unit(cPtr, owner);
				
			case (int) libsbml.SBML_ALGEBRAIC_RULE:
				return new AlgebraicRule(cPtr, owner);
				
			case (int) libsbml.SBML_ASSIGNMENT_RULE:
				return new AssignmentRule(cPtr, owner);
				
			case (int) libsbml.SBML_RATE_RULE:
				return new RateRule(cPtr, owner);

			case (int) libsbml.SBML_LAYOUT_BOUNDINGBOX:
				return new BoundingBox(cPtr, owner);
				
			case (int) libsbml.SBML_LAYOUT_COMPARTMENTGLYPH:
				return new CompartmentGlyph(cPtr, owner);
				
			case (int) libsbml.SBML_LAYOUT_CUBICBEZIER:
				return new CubicBezier(cPtr, owner);
				
			case (int) libsbml.SBML_LAYOUT_CURVE:
				return new Curve(cPtr, owner);
				
			case (int) libsbml.SBML_LAYOUT_DIMENSIONS:
				return new Dimensions(cPtr, owner);
				
			case (int) libsbml.SBML_LAYOUT_GRAPHICALOBJECT:
				return new GraphicalObject(cPtr, owner);
				
			case (int) libsbml.SBML_LAYOUT_LAYOUT:
				return new Layout(cPtr, owner);
				
			case (int) libsbml.SBML_LAYOUT_LINESEGMENT:
				return new LineSegment(cPtr, owner);
				
			case (int) libsbml.SBML_LAYOUT_POINT:
				return new Point(cPtr, owner);
				
			case (int) libsbml.SBML_LAYOUT_REACTIONGLYPH:
				return new ReactionGlyph(cPtr, owner);
				
			case (int) libsbml.SBML_LAYOUT_SPECIESGLYPH:
				return new SpeciesGlyph(cPtr, owner);
				
			case (int) libsbml.SBML_LAYOUT_SPECIESREFERENCEGLYPH:
				return new SpeciesReferenceGlyph(cPtr, owner);
				
			case (int) libsbml.SBML_LAYOUT_TEXTGLYPH:
				return new TextGlyph(cPtr, owner);
			default:
				return new SBase(cPtr, owner);
		}
	}
%}

#endif /* ! USE_LAYOUT */


/**
 * Convert SBase objects into the most specific object possible.
 */
%typemap("csout") SBase*
{
	return libsbml.DowncastSBase($imcall, $owner);
}

/**
 * Convert Rule objects into the most specific object possible.
 */
%typemap("csout") Rule*
{
	return (Rule) libsbml.DowncastSBase($imcall, $owner);
}


#ifdef USE_LAYOUT
/**
 * Convert LineSegment objects into the most specific object possible.
 */
%typemap("csout") LineSegment*
{
	return (LineSegment) libsbml.DowncastSBase($imcall, $owner);
}

/**
 * Convert LineSegment objects into the most specific object possible.
 */
%typemap("csout") GraphicalObject*
{
	return (GraphicalObject) libsbml.DowncastSBase($imcall, $owner);
}

#endif /* USE_LAYOUT */

/**
 * getCPtrAndDisown() is like getCPtr() but it also sets the SWIG memory
 * ownsership flag to false.
 *
 * We used to use %typemap(javagetcptr), but this has been deprecated
 * in SWIG 1.3.24.  Instead we add getCPtrAndDisown() from the incantation
 * below (taken from the SWIG 1.3.24 CHANGES file).
 */

/* Utility macro for manipulating the C# body code method attributes */
%define SWIGCSHARP_ATTRIBS(TYPENAME, CTOR_ATTRIB, GETCPTR_ATTRIB)

%typemap(csbody) TYPENAME
%{
	private HandleRef swigCPtr;
	protected bool swigCMemOwn;
	
	CTOR_ATTRIB $csclassname(IntPtr cPtr, bool cMemoryOwn)
	{
		swigCMemOwn = cMemoryOwn;
		swigCPtr    = new HandleRef(this, cPtr);
	}
	
	GETCPTR_ATTRIB static HandleRef getCPtr($csclassname obj)
	{
		return (obj == null) ? new HandleRef(null, IntPtr.Zero) : obj.swigCPtr;
	}
	
	GETCPTR_ATTRIB static HandleRef getCPtrAndDisown ($csclassname obj)
	{
		HandleRef ptr = new HandleRef(null, IntPtr.Zero);
		
		if (obj != null)
		{
			ptr             = obj.swigCPtr;
			obj.swigCMemOwn = false;
		}
		
		return ptr;
	}
%}


%typemap(csbody_derived) TYPENAME
%{
	private HandleRef swigCPtr;
	
	CTOR_ATTRIB $csclassname(IntPtr cPtr, bool cMemoryOwn) : base($modulePINVOKE.$csclassnameUpcast(cPtr), cMemoryOwn)
	{
		//super($modulePINVOKE.$csclassnameUpcast(cPtr), cMemoryOwn);
		swigCPtr = new HandleRef(this, cPtr);
	}
	
	GETCPTR_ATTRIB static HandleRef getCPtr($csclassname obj)
	{
		return (obj == null) ? new HandleRef(null, IntPtr.Zero) : obj.swigCPtr;
	}
	
	GETCPTR_ATTRIB static HandleRef getCPtrAndDisown ($csclassname obj)
	{
		HandleRef ptr = new HandleRef(null, IntPtr.Zero);
		
		if (obj != null)
		{
			ptr             = obj.swigCPtr;
			obj.swigCMemOwn = false;
		}
		
		return ptr;
	}
%}

%enddef

/* The default is INTERNAL getCPtr, INTERNAL constructor */
SWIGCSHARP_ATTRIBS(SWIGTYPE, internal, internal)

/* Public getCPtr method, PUBLIC constructor */
%define PUBLIC_GETCPTR(TYPENAME)
SWIGCSHARP_ATTRIBS(TYPENAME, protected, public)
%enddef

/* Public getCPtr method, public constructor */
%define PUBLIC_BODYMETHODS(TYPENAME)
SWIGCSHARP_ATTRIBS(TYPENAME, public, public)
%enddef


/**
 *
 * Overrides the 'operator==', 'operator!=', 'Equals' and 'GetHashCode' methods 
 * for C# proxy classes of SBase subclasses and classes in libSBML.
 *
 * By default, 'operator==' ( and 'Equals' method) for each wrapped class
 * object returns 'true' if the given two objects refer to the same 
 * *C# proxy object* (not the underlying C++ object). 
 * For example, the following code returns 'true'.
 *
 *   Model m = new Model();
 *   m.createReaction();
 *   Reaction r1  = m.getReaction(0);
 *   Reaction r2 = r1;
 *   return (r1 == r2);  <---- this returns 'true'
 *
 * On the other hand, the following code returns 'false' in spite of
 * the same underlying C++ objects.
 *
 *   Model m = new Model();
 *   m.createReaction();
 *   Reaction r1 = m.getReaction(0);
 *   Reaction r2 = m.getReaction(0);
 *   return (r1 == r2);  <---- this returns 'false'
 *
 * The following override changes the behaviour of the default 'operator==' and
 * 'Equals' method such that returns 'true' if the given two objects refer to 
 * the same underlying C++  object (i.e. 'true' is returned in the both above
 *  examples).
 * 
 */


%define SWIGCS_EQUALS(CLASS)
%typemap("cscode") CLASS
%{
  public static bool operator==(CLASS lhs, CLASS rhs)
  {
    if((Object)lhs == (Object)rhs)
    {
      return true;
    }

    if( ((Object)lhs == null) || ((Object)rhs == null) )
    {
      return false;
    }

    return (getCPtr(lhs).Handle.ToString() == getCPtr(rhs).Handle.ToString());
  }

  public static bool operator!=(CLASS lhs, CLASS rhs)
  {
    return !(lhs == rhs);
  }

  public override bool Equals(Object sb)
  {
    if ( ! (sb is CLASS) )
    {
      return false;
    }

    return this == (CLASS)sb;
  }

  public override int GetHashCode()
  {
    return swigCPtr.Handle.ToInt32();
  }
%}
%enddef

SWIGCS_EQUALS(SBase)
SWIGCS_EQUALS(SBMLReader)
SWIGCS_EQUALS(SBMLWriter)
SWIGCS_EQUALS(ASTNode)
SWIGCS_EQUALS(CVTerm)
SWIGCS_EQUALS(Date)
SWIGCS_EQUALS(ModelHistory)
SWIGCS_EQUALS(ModelCreator)
SWIGCS_EQUALS(XMLNamespaces)
SWIGCS_EQUALS(XMLAttributes)
SWIGCS_EQUALS(XMLToken)
SWIGCS_EQUALS(XMLNode)
SWIGCS_EQUALS(XMLError)
SWIGCS_EQUALS(XMLErrorLog)
SWIGCS_EQUALS(XMLHandler)
SWIGCS_EQUALS(XMLOutputStream)
SWIGCS_EQUALS(XMLInputStream)


/**
 * Most libSBML methods takeover ownership of passed-in objects, so we need
 * to make sure SWIG disowns the object.
 */
%typemap(csin) SWIGTYPE *, SWIGTYPE &
"$csclassname.getCPtrAndDisown($csinput)";


/**
 * takeover ownership
 *
 * - void ListOf::appendAndOwn(SBase* item)
 */
%typemap(csin) SBase*       item "SBase.getCPtrAndDisown($csinput)";
%typemap(csin) const SBase* item "SBase.getCPtr($csinput)";

/**
 * takeover ownership
 *
 * - void ASTNode::addChild (ASTNode* child)
 * - void ASTNode::prependChild (ASTNode* child)
 */
%typemap(csin) ASTNode*       child "ASTNode.getCPtrAndDisown($csinput)";
%typemap(csin) const ASTNode* child "ASTNode.getCPtr($csinput)";

/**
 * Of course, there are some exceptions to the above rule.  These typemaps
 * cover the following functions:
 *
 *  - writeSBML()
 *  - writeSBMLToString()
 *  - writeMathML()
 *  - writeMathMLToString()
 *
 * Which take either an SBMLDocument or MathMLDocument as input.
 */
%typemap(csin) SBMLDocument   * "SBMLDocument.getCPtr($csinput)";
%typemap(csin) MathMLDocument * "MathMLDocument.getCPtr($csinput)";



%typemap(cstype) std::ostream& "OStream"
%typemap(csin) std::ostream& "SWIGTYPE_p_std__ostream.getCPtr($csinput.get_ostream())";


//////////////////////////////////////////////////////////////////////
//
// directives for converting Unicode <-> UTF8 in Windows
//
//////////////////////////////////////////////////////////////////////

#ifdef SWIGWIN

%typemap(ctype) std::string        "wchar_t*"
%typemap(ctype) std::string&       "wchar_t*"
%typemap(ctype) const std::string& "wchar_t*"
%typemap(ctype) std::string*       "wchar_t*"
%typemap(ctype) const std::string* "wchar_t*"
%typemap(ctype) char*              "wchar_t*"
%typemap(ctype) const char*        "wchar_t*"


//
// Unicode -> UTF8 (std::string&, const std::string&)
// (argument variable)
//
%typemap("in") std::string&, const std::string&  (std::string arg_str) {
  char*  mbstr = convertUnicodeToUTF8($input);
  if (!mbstr) return $null;

  arg_str.assign(mbstr);
  $1 = &arg_str;
  delete[] mbstr;
}


//
// UTF8 -> Unicode (std::string&, const std::string&) 
// (return variable)
//
%typemap("out") std::string&, const std::string& {
  $result = convertUTF8ToUnicode(($1)->c_str());
  wchar_t* unistr = convertUTF8ToUnicode(($1)->c_str());
  $result = (wchar_t*) SWIG_csharp_wstring_callback((const wchar_t*)unistr);
  delete[] unistr;
}


//
// Unicode -> UTF8 (std::string)
// (argument variable)
//
%typemap("in") std::string {
  char*  mbstr = convertUnicodeToUTF8($input);
  (&$1)->assign(mbstr);
  delete[] mbstr;
}


//
// UTF8 -> Unicode (std::string)
// (return variable)
//
%typemap("out") std::string {
  $result = convertUTF8ToUnicode( $1.c_str() );
  wchar_t* unistr = convertUTF8ToUnicode( $1.c_str() );
  $result = (wchar_t*) SWIG_csharp_wstring_callback((const wchar_t*)unistr);
  delete[] unistr;
}


//
// Unicode -> UTF8 (char*, const char*)
// (argument variable)
//
%typemap("in")  char*, const char* {
  if ($input)
  {
    $1 = convertUnicodeToUTF8($input);
    if (!$1) return $null;
  }
}

%typemap("freearg")  char*, const char* {
  delete[] $1;
}


//
// UTF8 -> Unicode (char*, const char*)
// (returned variable)
//
%typemap("out")  char*, const char* {
  $result = convertUTF8ToUnicode( $1 );
  wchar_t* unistr = convertUTF8ToUnicode( $1 );
  $result = (wchar_t*) SWIG_csharp_wstring_callback((const wchar_t*)unistr);
  delete[] unistr;
}


//
//  A string for filename should be encoded by ANSI CP not by UTF-8 
//  because file i/o functions used in libSBML requires the ANSI CP 
//  encoded string for a given filename.
//  
//  1) SBMLReader::readSBML(const std::string& filename)
//  2) readSBML(const char* filename)
//  3) SBMLWriter::writeSBML(SBMLDocument*, const std::string& filename)
//  4) writeSBML(SBMLDocument*, const char* filename)
//


//
// Unicode -> ANSI CP (for const std::string& filename)
//
%typemap("in") const std::string& filename (std::string arg_str) {
    char*  mbstr = convertUnicodeToACP($input);
    if (!mbstr) return $null;

    arg_str.assign(mbstr);
    $1 = &arg_str;
    delete[] mbstr;
}


//
// Unicode -> ANSI CP (for const char* filename)
//
%typemap("in")  const char* filename{
    if ($input)
    {
      $1 = convertUnicodeToACP($input);
      if (!$1) return $null;
    }
}

#endif //SWIGWIN

///////////////////////////////////////////////////////////////////////////


%inline 
%{
	
	class LIBLAX_EXTERN OStream 
	{
	protected:
		std::ostream* Stream;
		
	public:
		enum StdOSType {COUT,CERR,CLOG};
		
		/**
		 * Creates a new OStream object with one of standard output stream objects.
		 */
		OStream (StdOSType sot = COUT) 
		{
			switch (sot) {
				case COUT:
					Stream = &std::cout;
					break;
				case CERR:
					Stream = &std::cerr;
					break;
				case CLOG:
					Stream = &std::clog;
					break;
				default:
					Stream = &std::cout;
			}
		}
		
		virtual ~OStream () 
		{
		}
		
		/**
		 * Returns stream object.
		 */
		virtual std::ostream* get_ostream ()  
		{ 
			return Stream;
		}
		
		void endl ()
		{
			std::endl(*Stream);
		}
		
	};
	
	class LIBLAX_EXTERN OFStream : public OStream 
	{
	public:
		/**
		 * Creates a new OFStream object and opens a given file with 
		 * append flag (default is false) and associates its content 
		 * with stream object (Stream).
		 */
		OFStream (const std::string& filename, bool is_append = false) 
		{
			if (is_append) {
				Stream = new std::ofstream(filename.c_str(),ios_base::app);
			}
			else {
				Stream = new std::ofstream(filename.c_str(),ios_base::out);
			}
		}
		
		/**
		 * Opens a given file with append flag (default is false) and associates
		 * its content with existing stream object (Stream).
		 */
		void open (const std::string& filename, bool is_append = false) 
		{ 
			if (is_append) {
				static_cast<std::ofstream*>(Stream)->open(filename.c_str(),ios_base::app);
			}
			else {
				static_cast<std::ofstream*>(Stream)->open(filename.c_str(),ios_base::out);
			}
		}  
		
		/**
		 * Closes the file currently associated with stream object.
		 */
		void close ()
		{
			static_cast<std::ofstream*>(Stream)->close();
		}
		
		/**
		 * Returns true if stream object is currently associated with a file
		 */
		bool is_open () 
		{ 
			return static_cast<std::ofstream*>(Stream)->is_open(); 
		}
		
		virtual ~OFStream ()
		{
			delete Stream;
		}
		
	};
	
	class LIBLAX_EXTERN OStringStream : public OStream 
	{
	public:
		/**
		 * Creates a new OStringStream object
		 */
		OStringStream () 
		{
			Stream = new std::ostringstream();
		}
		
		/**
		 * Returns the copy of the string object currently assosiated 
		 * with the stream buffer.
		 */
		std::string str () 
		{
			return static_cast<std::ostringstream*>(Stream)->str();
		}
		
		/**
		 * Sets string s to the string object currently assosiated with 
		 * the stream buffer.
		 */
		void str (const std::string& s)
		{
			static_cast<std::ostringstream*>(Stream)->str(s.c_str());
		}
		
		virtual ~OStringStream () 
		{
			delete Stream;
		}
		
	};


%}

%pragma(cs) modulecode =
%{
	public static const OStream cout;
	public static const OStream cerr;
	public static const OStream clog;
	
	static {
		cout = new OStream(OStream.COUT); 
		cerr = new OStream(OStream.CERR); 
		clog = new OStream(OStream.CLOG); 
	}
%}
