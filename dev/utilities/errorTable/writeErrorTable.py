#!/usr/bin/env python3
# =============================================================================
# @file   writeErrorTable.py
# @brief  Write documentation for SBML error codes
# @author Sarah Keating
# @author Michael Hucka
# =============================================================================
#
# This Python program interrogates the currently-installed libSBML library to
# generate documentation about the libSBML diagnostic codes, and generate
# output used in several contexts.  The output produced by this program is
# controlled by the first command-line argument given to it:
#
# * Given --doc, it will generate a table suitable for use in the libSBML
#   API documentation as it appears in the header of SBMLError.h.  The file
#   it produces is meant to replace src/sbml/common/common-sbmlerror-codes.h.
#   In other words, do the following:
#
#    writeErrorTable.py --doc ../../../src/sbml/common/common-sbmlerror-codes.h
#
# * Given --enum, it will generate the SBMLErrorCode_t enum for SBMLError.h.
#   (The output needs to be manually copied and pasted into SBMLError.h.)
#
# * Given --web, it will generate output used in the table in the web page at
#   http://sbml.org/Facilities/Documentation/Error_Categories.
#
# Here is a typical way of using this program:
#
# 1) Make any desired updates to the text of the diagnostic messages in
#    src/sbml/SBMLErrorTable.h.
#
# 2) Rebuild and install libSBML on your computer, with the Python bindings
#    enabled.  This is necessary because this program (writeErrorTable.py)
#    uses the libSBML Python bindings to do its work.
#
# 3) Configure your PYTHONPATH environment variable to encompass this newly-
#    installed copy of libSBML.  Double-check that your Python executable
#    is in fact picking up the copy of libSBML you think it is.
#
# 4) Run this program 3 times, as following
#
#      ./writeErrorTable.py --doc  doc-fragment.txt
#      ./writeErrorTable.py --enum enum.txt
#      ./writeErrorTable.py --web  sbmlerror-table.html
#
# 5) Replace sbmlerror-table.html on sbml.org, and edit SBMLError.h to
#    insert the contents of doc-fragment.txt and enum.txt in the appropriate
#    places.
#
#<!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -->

import re, os, sys

from types import *
from imp import *
from string import *
from libsbml import *


# -----------------------------------------------------------------------------
# Globals.
# -----------------------------------------------------------------------------

# The currently-known SBML Levels and Versions.

sbml_levels_versions = [[1, 1], [1, 2],
                        [2, 1], [2, 2], [2, 3], [2, 4],
                        [3, 1], [3, 2]]

# Set of error codes that we ignore for purposes of documentation.

ignored_error_codes = {9999, 10599, 20905, 21112, 29999, 90000, 90501, 99502,
                       99503, 99504, 99994, 99995, 99999}

# Package error code ranges.  The numbers are the low end start values.

package_codes = [['comp',   1000000],
                 ['fbc',    2000000],
                 ['qual',   3000000],
                 ['groups', 4000000],
                 ['layout', 6000000]]

# Our approach to finding error codes starts with numbers and then rummages
# through the list of symbols in the libSBML Python module to find the symbol
# that corresponds to each number.  That works well enough for our purposes
# when the number is above 100, but for lower numbers, we have collisions
# because there are many short enumerations in libSBML.  We don't care about
# numbers lower than 10000 when doing --enum or --doc, but we do for --web,
# where we try to list all error codes.  All of those enumerations turn into
# the same numbers starting from 0, so if you grab a symbol from the Python
# module and examine its value and find that it, say, "3", you can't tell
# which enumeration it came from.  You can only do that if you know the
# symbol names you care about ahead of time.  Since we don't want to
# hard-code all possible libSBML error codes here (avoiding that scenario is
# the point of this program!), we have to find some other way to distinguish
# the cases.  The approach taken here is to only worry about the numbers
# below 1000 using a table that enumerates the symbols we care about; when we
# iterate over all error numbers, we always explicitly map the numbers to the
# following codes.

low_numbered_errors = { 0   : 'XMLUnknownError',
                        1   : 'XMLOutOfMemory',
                        2   : 'XMLFileUnreadable',
                        3   : 'XMLFileUnwritable',
                        4   : 'XMLFileOperationError',
                        5   : 'XMLNetworkAccessError',
                        101 : 'InternalXMLParserError',
                        102 : 'UnrecognizedXMLParserCode',
                        103 : 'XMLTranscoderError' }

# Fragments for the table used in the documentation for SBMLError.h.

doc_table_start_fragment = '''/**
 * @class doc_sbml_error_table
 *
 * @par
<table id="sbmlerror-table"
       class="text-table small-font alt-row-colors"
       width="95%" cellspacing="1" cellpadding="2" border="0">
 <tr style="background: lightgray" class="normal-font">
     <th valign="bottom"><strong>Enumerator</strong></th>
     <th valign="bottom"><strong>Meaning</strong></th>
     <th align="center" width="10">L1 V1</th>
     <th align="center" width="10">L1 V2</th>
     <th align="center" width="10">L2 V1</th>
     <th align="center" width="10">L2 V2</th>
     <th align="center" width="10">L2 V3</th>
     <th align="center" width="10">L2 V4</th>
     <th align="center" width="10">L3 V1</th>
     <th align="center" width="10">L3 V2</th>
 </tr>
'''

doc_table_end_fragment = '''</table>

*/
'''

# Templates and fragments used for the enum.

enum_start_fragment = '''/**
 * @enum SBMLErrorCode_t
 * Codes for all SBML-level errors and warnings from the core specification.
 *
 * @copydetails doc_sbml_error_code_ranges
 */
typedef enum
{
'''

enum_end_fragment = '''} SBMLErrorCode_t;
'''


# Templates and fragments used for the web page.

web_css_fragment = '''<style type='text/css'>
/*<![CDATA[*/
table {
  margin-bottom: 2em;
  border: 1px solid #ccc;
  border-spacing: 0;
}

td, th {
  border: none;
}

th {
  border-bottom: 1px solid #ccc;
}

td {
  border-bottom: 1px solid white;
}

tr:last-child > td {
  border-bottom: none;
}

td.s-fatal {
  font-size: 7pt;
  color: white;
  background-color: darkred;
  font-weight: bold;
  text-align: center;
  letter-spacing: -1px;
  padding: 2px;
}

td.s-fatal:before {
  content: "F";
}

td.s-error {
  font-size: 7pt;
  background-color: #dc143c;
  font-weight: bold;
  text-align: center;
  letter-spacing: -1px;
  padding: 2px;
}

td.s-error:before {
  content: "E";
}

td.s-warning {
  font-size: 7pt;
  background-color: gold;
  font-weight: bold;
  text-align: center;
  letter-spacing: -1px;
  padding: 2px;
}

td.s-warning:before {
  content: "W";
}

td.s-na {
  font-size: 7pt;
  font-weight: bold;
  text-align: center;
  letter-spacing: -1px;
  padding: 2px;
}

td.s-na:before {
  content: "NA";
}

tr.headers {
  background: lightgray;
}

th.levels {
  text-align: center;
  width: 1%;
  padding: 2px;
  letter-spacing: -1px;
}

th.errorid {
  width: 12ex;
  vertical-align: bottom;
}

th.errorid strong {
  color: #333;
}

td.code {
  font-size: 8pt;
  font-family: -webkit-monospace, monospace;
  color: #000;
  text-align: center;
  letter-spacing: -1px;
}

th.errorname {
  width: 20%;
}

td.errorname {
  font-size: 8pt;
  font-family: -webkit-monospace, monospace;
  color: #000;
  height: 26px;   /* to equalize heights of all table rows. */
}

th.meaning {
  font-size: 8pt;
  font-style: italic;
  vertical-align: bottom;
  letter-spacing: -1px;
}

th.meaning strong {
  color: #333;
}

td.meaning code {
  font-size: 8pt;
}

.s-warning
{
  background-color: gold;
  padding: .1em .5em;
  color: black;
}

.s-error
{
  background-color: #D23D24;
  padding: .1em .5em;
  color: white;
}

.s-fatal
{
  background-color: darkred;
  padding: .1em .5em;
  color: white;
}

.table-pkg-separator {
  text-align: center;
  background-color: #ffefd5;
  font-style: italic;
  font-weight: bold;
  line-height: 160%;
}
/*]]>*/
</style>
'''

web_toc_start_fragment = '''
<table style='margin-top: 2pt' id="toc" class="toc" summary="Contents">
<tr><td>
<div style="text-align:left">
<a name="TOC"/></a>
'''

web_toc_end_fragment = '''</div>
</td></tr>
</table>
'''

page_intro_template = '''
<p>This page lists all error, warning and informational diagnostic codes
and messages produced by <a
href="http://sbml.org/Software/libSBML">libSBML</a>, the software
underlying the <a
href="http://sbml.org/Facilities/Validator">Online SBML Validator</a>.
The diagnostics whose code numbers are
between 10000 and 90000 are defined in the <a
href="http://sbml.org/Documents/Specifications">SBML specification documents</a>;
the ones with numbers below 10000 and above 90000 are specific to libSBML or
this validation system and are not defined by the SBML specifications.  The
groups below provide finer-grained divisions in the diagnostics, for better
comprehensibility.</p>

<p>
In the tables below, the right-hand columns titled "L1V1", "L1V2", etc. refer
to Levels and Versions of the SBML specifications, and the entries in
each column refer to the severity of the condition in that
particular Level and Version of SBML.  The codes have the following
meanings:</p>
<p>
<table class="normal-font borderless-table gray-border sm-padding"
       width="200px" cellspacing="1" cellpadding="2" border="0"
       style="margin-left: 2em">
  <tr><th style="text-align: center">Symbol</th><th style="text-align: left">Meaning</th></tr>
  <tr><td class="s-warning gray-border"></td><td>Warning</td></tr>
  <tr><td class="s-error"></td><td>Error</td></tr>
  <tr><td class="s-fatal"></td><td>Fatal</td></tr>
  <tr><td class="s-na" style="border: none"></td><td>Not applicable</td></tr>
</table>
<p>For more information about the meanings of diagnostics numbered between
10000 and 90000, please consult the <a
href="http://sbml.org/Documents/Specifications">SBML specification documents</a>
relevant to the particularl Level and Version combination in question.</p>
'''

web_error_table_start_fragment = '''<center>
<table class="sm-padding sm-font alt-row-colors"
       width="100%" cellspacing="1" cellpadding="2" border="0">
  <tr class="headers">
    <th class="errorid">Error code</th>
    <th class="errorname">LibSBML name</th>
    <th class="errormeaning">Meaning</th>
    <th class="levels">L1 V1</th>
    <th class="levels">L1 V2</th>
    <th class="levels">L2 V1</th>
    <th class="levels">L2 V2</th>
    <th class="levels">L2 V3</th>
    <th class="levels">L2 V4</th>
    <th class="levels">L3 V1</th>
    <th class="levels">L3 V2</th>
  </tr>
'''

web_error_table_end_fragment = '''</table>
</center>
'''

error_groups = [["Internal",                       # HTML anchor
                 [LIBSBML_CAT_INTERNAL],           # category code(s)
                 "Internal errors",                # section title
'''This group of errors concerns problems involving the libSBML software
itself, or the underlying XML parser being used.  This almost certainly
indicates a software defect (i.e., bug) in libSBML.  <strong>Please
<a href='https://sourceforge.net/p/sbml/libsbml/new/'>report</a>
occurrences of these errors to the libSBML developers.</strong>'''
                 ],                                # section intro text

                ["OS",
                 [LIBSBML_CAT_SYSTEM],
                 "Operating system errors",
'''This group of diagnostic codes refers to problems that can be reported
by the operating system where libSBML is running.  This indicates something
that is not a libSBML error, but rather, is outside of the control of libSBML.'''
                 ],

                ["XML",
                 [LIBSBML_CAT_XML],
                 "XML errors",
'''This group of diagnostic codes refers to problems with the XML content of
the SBML file or data stream.  This usually arises from malformed XML, and is
detected at a very low level in the XML parsing system.  Further interpretation
of SBML is aborted if this type of error is encountered.'''
                 ],

                ["Overall-sbml",
                 [LIBSBML_CAT_SBML],
                 "Overall SBML conformance issues",
'''This group of diagnostic codes is concerned with general and overall issues
of conformance to the relevant SBML specifications.'''
                 ],

                ["General",
                 [LIBSBML_CAT_GENERAL_CONSISTENCY],
                 "General SBML consistency issues",
'''This group of diagnostic codes concerns the correct use of SBML constructs
according to the relevant SBML specifications.'''
                 ],

                ["Identifier",
                 [LIBSBML_CAT_IDENTIFIER_CONSISTENCY],
                 "SBML identifier consistency issues",
'''This group of diagnostic codes concerns general SBML issues related to
the identifiers used in SBML documents.  For convenience and greater control
during validation procedures, they are separated from the set of diagnostics 
for general SBML consistency.'''
                 ],

                ["Units",
                 [LIBSBML_CAT_UNITS_CONSISTENCY],
                 "Units consistency issues",
'''This group of diagnostic codes is concerned with the consistency of units
of measurement associated with quantities specified in an SBML model.  For
convenience and greater control during validation procedures, they are 
separated from the set of diagnostics for general SBML consistency.'''
                 ],

                ["MathML",
                 [LIBSBML_CAT_MATHML_CONSISTENCY],
                 "MathML issues",
'''This group of diagnostic codes concerns possible problems that can arise
with the MathML constructs used in an SBML document.'''
                 ],

                ["SBO",
                 [LIBSBML_CAT_SBO_CONSISTENCY],
                 "Systems Biology Ontology (SBO) issues",
'''This group of diagnostic codes concerns possible problems that can arise
with the MathML constructs used in an SBML document.  For
convenience and greater control during validation procedures, they are 
separated from the set of diagnostics for general SBML consistency.'''
                 ],

# The errors previously categorized as LIBSBML_CAT_OVERDETERMINED_MODEL in
# libSBML now seem to be categorized as general SBML spec consistency errors.
#
#                 ["Over",
#                  [LIBSBML_CAT_OVERDETERMINED_MODEL],
#                  "Overdetermined models",
# '''This group of diagnostic codes is concerned with problems encountered
# while testing for overdetermined models; that is, a model where the system is
# overdetermined, therefore violating a tenet of proper SBML.'''
#                  ],

                ["Practice",
                 [LIBSBML_CAT_MODELING_PRACTICE],
                 "Modeling practice issues",
'''This group of diagnostic codes is concerned with matters of good modeling
practices involving SBML and computational modeling.  These are tests
performed by libSBML and do not have equivalent SBML validation rules.
For convenience and greater control during validation procedures, they are
separated from the set of diagnostics for general SBML consistency.'''
                 ],

                ["Internal-consistency",
                 [LIBSBML_CAT_INTERNAL_CONSISTENCY],
                 "Internal SBML consistency issues",
'''This set of diagnostics is concerned with problems that can occur while
validating the libSBML internal representation of SBML constructs. These
are tests performed by libSBML and do not have equivalent SBML validation
rules.  For convenience and greater control during validation procedures, they
are separated from the set of diagnostics for general SBML consistency.'''
                 ],

# 2014-05-14: We decided not to show the conversion codes in the online table.
#
#                 ["conversion",
#                  [LIBSBML_CAT_SBML_L1_COMPAT,
#                   LIBSBML_CAT_SBML_L2V1_COMPAT,
#                   LIBSBML_CAT_SBML_L2V1_COMPAT,
#                   LIBSBML_CAT_SBML_L2V2_COMPAT,
#                   LIBSBML_CAT_SBML_L2V3_COMPAT,
#                   LIBSBML_CAT_SBML_L2V4_COMPAT,
#                   LIBSBML_CAT_SBML_L3V1_COMPAT],
#                  "Problems in converting models",
# '''This group of diagnostic codes is concerned with problems encountered
# during conversion of an SBML document from one SBML Level+Version combination
# to another.'''
#                 ],

]

# -----------------------------------------------------------------------------
# Main code for --doc.
# -----------------------------------------------------------------------------

def write_doc(stream, module):
  stream.write(doc_table_start_fragment)
  for errNum in sorted(get_numeric_constants(module) - ignored_error_codes):
    stream.write(make_doc_row_text(errNum, module))
  stream.write(doc_table_end_fragment)


def make_doc_row_text(errNum, module):
  s = get_symbol(module, errNum)
  output  = '<tr>'
  output += '<td class="code">@sbmlconstant{' + s + ', SBMLErrorCode_t}</td>\n'
  if errNum < 99999:
    e = SBMLError(errNum, 1, 1)
    if not e.isValid():
      return ''
    output += '<td class="meaning">{}</td>\n'.format(to_html(e.getShortMessage()))
    for lv in sbml_levels_versions:
      e = SBMLError(errNum, lv[0], lv[1])
      output += '<td class="{}"></td>\n'.format(get_severity_class(e.getSeverity()))
  else:
    for package in package_codes:
      pkg_name  = package[0]
      pkg_start = package[1]
      pkg_end   = pkg_start + 1000000
      if errNum > pkg_start and errNum < pkg_end:
        # Packages are only possible in SBML Level 3.
        # Check existence in some L/V combination.
        elist = []
        for lv in sbml_levels_versions:
          if lv[0] < 3:
            continue
          elist.append(SBMLError(errNum, lv[0], lv[1], '', 0, 0, 0, 0, pkg_name, 1))
        if all(not e.isValid() for e in elist):
          return ''
        first_e = next(e for e in elist if e.isValid())
        output += '<td class="meaning">{}</td>\n'.format(to_html(first_e.getShortMessage()))
        for lv in sbml_levels_versions:
          if lv[0] < 3:
            output += '<td class="{}"></td>\n'.format(get_severity_class(0))
            continue
          e = SBMLError(errNum, lv[0], lv[1], '', 0, 0, 0, 0, pkg_name, 1)
          output += '<td class="{}"></td>\n'.format(get_severity_class(e.getSeverity()))
        break

  output += '</tr>\n'
  print_progress()
  return output


# -----------------------------------------------------------------------------
# Main code for --web.
# -----------------------------------------------------------------------------

def write_web(stream, module):
  stream.write(web_css_fragment)
  stream.write(make_web_toc())
  stream.write(page_intro_template.format(LIBSBML_DOTTED_VERSION))
  for entry in error_groups:
    anchor     = entry[0]
    categories = entry[1]
    title      = entry[2]
    intro      = entry[3]

    stream.write('<a name="{}"><h3>{}</h3></a>\n'.format(anchor, title))
    stream.write('<p>' + intro + '</p>\n')
    stream.write(web_error_table_start_fragment)
    for errNum in sorted(get_numeric_constants(module, 0, 100000) - ignored_error_codes):
      e = SBMLError(errNum, 1, 1)
      if not e.isValid():
        continue
      if not e.getCategory() in categories:
        continue
      stream.write(make_web_row_text(errNum, module))

    for package in package_codes:
      pkg_name  = package[0]
      pkg_start = package[1]
      pkg_end   = pkg_start + 1000000
      printed_separator = False
      for errNum in sorted(get_numeric_constants(module, pkg_start, pkg_end)):
        e = SBMLError(errNum, 3, 1, '', 0, 0, 0, 0, pkg_name, 1)
        msg = e.getShortMessage()
        if not msg:
          continue
        if not e.getCategory() in categories:
          continue
        if not printed_separator:
          stream.write(make_web_table_pkg_separator(pkg_name))
          printed_separator = True
        stream.write(make_web_row_pkg_text(e, errNum, pkg_name, pkg_start, module))

    print_progress("Writing '" + title + "' \n")
    stream.write(web_error_table_end_fragment)


def make_web_toc():
  output = web_toc_start_fragment
  for entry in error_groups:
    anchor  = entry[0]
    title   = entry[2]
    output += '<a href="#{}">{}</a><br/>\n'.format(anchor, title)
  output += web_toc_end_fragment
  return output


def make_web_table_pkg_separator(pkg_name):
  return '<tr><td colspan="' + str(3 + len(sbml_levels_versions)) \
    + '" class="table-pkg-separator">Codes for SBML Level 3 package "' \
    + pkg_name + '"</td></tr>'


def make_web_row_text(errNum, module):
  e = SBMLError(errNum, 1, 1)
  if not e.isValid():
    return ''

  output  = '<tr>'
  output += '<td class="code">{0:05d}</td>'.format(errNum)
  output += '<td class="errorname">{}</td>'.format(get_symbol(module, errNum))
  output += '<td class="meaning">{}</td>'.format(to_html(e.getShortMessage()))
  for lv in sbml_levels_versions:
    e = SBMLError(errNum, lv[0], lv[1])
    severity = e.getSeverity()
    output += '<td class="{}"></td>'.format(get_severity_class(severity))
  output += '</tr>\n'
  return output


def make_web_row_pkg_text(err, errNum, pkg_name, pkg_start, module):
  output  = '<tr>'
  output += '<td class="code">{0}-{1:05d}</td>'.format(pkg_name, errNum-pkg_start)
  output += '<td class="errorname">{}</td>'.format(get_symbol(module, errNum))
  output += '<td class="meaning">{}</td>'.format(to_html(err.getShortMessage()))
  for lv in range(0, len(sbml_levels_versions) - 1):
    output += '<td class="{}"></td>'.format(get_severity_class(0))
  output += '<td class="{}"></td>'.format(get_severity_class(err.getSeverity()))
  output += '</tr>\n'
  return output


# -----------------------------------------------------------------------------
# Main code for --enum.
# -----------------------------------------------------------------------------

def write_enum(stream, module):
  stream.write(enum_start_fragment)
  for errNum in sorted(get_numeric_constants(module, 0, 100000)):
    e = SBMLError(errNum, 1, 1)
    if e.isValid():
      if errNum == 10000:
        stream.write("  ")
      else:
        stream.write(", ")
      stream.write("%s = %s" %(ljust(get_symbol(module, errNum), 37), str(errNum)))
      msg = e.getShortMessage()
      if msg != '':
        if msg[-1] != '.':
          maybe_period = '.'
        else:
          maybe_period = ''
        stream.write(" /*!< " + to_html(msg) + maybe_period + " */")
      stream.write("\n")
      print_progress()

  stream.write(enum_end_fragment)


# -----------------------------------------------------------------------------
# Helper functions.
# -----------------------------------------------------------------------------

def get_module():
  fp, pathname, description = find_module("libsbml")
  return load_module('_libsbml', fp, pathname, description)


def get_numeric_constants(module, low=0, high=90000000):
  constants = set()
  for symbol in dir(module):
    try:                                # Some symbols don't have a value,
      value = eval(symbol)              # so we have to guard against that.
      if isinstance(value, int) and low <= value and value <= high:
          constants.add(value)
    except:
      continue
  return constants


def get_symbol(module, number):
  # This is an inefficient way of getting the string corresponding to a
  # given error number, but we don't care because this application isn't
  # performance-critical.  The algorithm is also fragile, but if we're
  # careful about the range of values attempted (i.e., numbers from 10000
  # to 100000), it should be okay.  Unfortunately, values below 100 hits
  # a number of small enumerations in libSBML, so we have to special-case
  # that.  Really, this whole thing is just awful.
  #
  if number in low_numbered_errors.keys():
    return low_numbered_errors[number]

  symbols = dir(module)
  attributes = [None]*len(symbols)
  for i in range(0, len(symbols)):
    attributes[i] = getattr(module, symbols[i])
  for i in range(0, len(symbols)):
    if isinstance(attributes[i], int) == True and attributes[i] == number:
      return symbols[i]


def get_severity_class(severity):
  if   (severity == 3): return "s-fatal"
  elif (severity == 2): return "s-error"
  elif (severity == 1): return "s-warning"
  else:                 return "s-na"


def to_html(text):
  text = text.replace('<', '&lt;')
  text = text.replace('>', '&gt;')
  text = text.replace('&lt;', '<code>&lt;')
  text = text.replace('&gt;', '&gt;</code>')
  return text


def print_progress(marker="."):
  # Echo to terminal to show progress.
  sys.stdout.write(marker)
  sys.stdout.flush()


# -----------------------------------------------------------------------------
# Main.
# -----------------------------------------------------------------------------

def main (args):
  """Usage:
            writeErrorTable.py [--doc | --enum | --web]  OUTPUTFILE

  The first argument must one of --doc, --enum or --web.

  * Given --doc, it will generate a table suitable for use in the libSBML
    API documentation as it appears in the header of SBMLError.h.

  * Given --enum, it will generate the SBMLErrorCode_t enum for SBMLError.h.

  * Given --web, it will generate output used in the table in the web page at
    http://sbml.org/Facilities/Documentation/Error_Categories.

  It will write its output to OUTPUTFILE.  Note that the name of the file
  will be used as given; no file name extension will be added.  Callers are
  assumed to supply the full name desired.
  """

  # Start with some sanity-checking.

  if len(args) != 3 or (args[1] not in ['--doc', '--enum', '--web']):
    print(main.__doc__)
    sys.exit(1)

  # OK, let's do this thing.

  module = get_module()

  print('Writing output to ' + args[2])
  stream = open(args[2], 'w')

  if args[1] == '--doc':
    write_doc(stream, module)
  elif args[1] == '--enum':
    write_enum(stream, module)
  else:
    write_web(stream, module)

  stream.close()
  print 'Done.'



if __name__ == '__main__':
  main(sys.argv)
