# Description : cekarticle.perl
# Author(s)   : Michael Hucka <mhucka@@cds.caltech.edu>
# Organization: CDS, California Institute of Technology
# Created     : 2000-09-27 16:57 PDT
# Revision    : $Id$
# $Source$
#
# Originally based on article.perl by Ross Moore <ross@mpce.mq.edu.au> 09-14-97

package main;

&do_require_package('article');
&do_require_package('alltt');
&do_require_package('verbatim');
&do_require_package('color');
&do_require_package('graphicx');
&do_require_package('html');
&do_require_package('hyperref');
&do_require_package('natbib');

#-----------------------------------------------------------------------------
# Stuff from basic article.perl/article.cls.
#-----------------------------------------------------------------------------

sub do_cekarticle_a4paper{}
sub do_cekarticle_a5paper{}
sub do_cekarticle_b5paper{}
sub do_cekarticle_legalpaper{}
sub do_cekarticle_letterpaper{}
sub do_cekarticle_executivepaper{}
sub do_cekarticle_landscape{}
sub do_cekarticle_final{}
sub do_cekarticle_draft{}
sub do_cekarticle_oneside{}
sub do_cekarticle_twoside{}
sub do_cekarticle_openright{}
sub do_cekarticle_openany{}
sub do_cekarticle_onecolumn{}
sub do_cekarticle_twocolumn{}
sub do_cekarticle_twocolumntoc{}
sub do_cekarticle_notitlepage{}
sub do_cekarticle_titlepage{}
sub do_cekarticle_openbib{}

sub do_cekarticle_10pt{ $LATEX_FONT_SIZE = '10pt' unless $LATEX_FONT_SIZE; }
sub do_cekarticle_11pt{ $LATEX_FONT_SIZE = '11pt' unless $LATEX_FONT_SIZE; }
sub do_cekarticle_12pt{ $LATEX_FONT_SIZE = '12pt' unless $LATEX_FONT_SIZE; }

sub do_cekarticle_leqno{ $EQN_TAGS = 'L'; }
sub do_cekarticle_reqno{ $EQN_TAGS = 'R'; }
sub do_cekarticle_fleqn{ $FLUSH_EQN = 1; }

sub do_cmd_thesection {
    join('', &do_cmd_arabic("${O}0${C}section${O}0$C"), @_[0]) }
sub do_cmd_thesubsection {
    join('',&translate_commands("\\thesection")
	,".", &do_cmd_arabic("${O}0${C}subsection${O}0$C"), @_[0]) }
sub do_cmd_thesubsubsection {
    join('',&translate_commands("\\thesubsection")
	,"." , &do_cmd_arabic("${O}0${C}subsubsection${O}0$C"), @_[0]) }
sub do_cmd_theparagraph {
    join('',&translate_commands("\\thesubsubsection")
	,"." , &do_cmd_arabic("${O}0${C}paragraph${O}0$C"), @_[0]) }
sub do_cmd_thesubparagraph {
    join('',&translate_commands("\\theparagraph")
	,"." , &do_cmd_arabic("${O}0${C}subparagraph${O}0$C"), @_[0]) }

sub do_cmd_theequation {
    join('', &do_cmd_arabic("${O}0${C}equation${O}0$C"), @_[0]) }

sub do_cmd_thefootnote {
    join('', &do_cmd_arabic("${O}0${C}footnote${O}0$C"), @_[0]) }

sub do_cmd_thefigure {
    join('', &do_cmd_arabic("${O}0${C}figure${O}0$C"), @_[0]) }

sub do_cmd_thetable {
    join('',  &do_cmd_arabic("${O}0${C}table${O}0$C"), @_[0]) }

#-----------------------------------------------------------------------------
# Additions for cekarticle.cls
#-----------------------------------------------------------------------------

$notoc = 0;

# Run this directly in order to get a white background in latex2html.
&apply_body_options("BGCOLOR", "ffffff");

sub do_cekarticle_toc{}
sub do_cekarticle_notoc{ $notoc++; }

sub do_cmd_attrib { &styled_text_chunk('TT','','font','','', '', @_); }
sub do_cmd_attribtype { &styled_text_chunk('TT','','font','','', '', @_); }
sub do_cmd_attribvalue { &styled_text_chunk('TT','','font','','', '', @_); }
sub do_cmd_class { &styled_text_chunk('TT','','font','','', '', @_); }

# There must be more direct ways of making \toprule etc be essentially
# ignored, but this is the best I could do without spending more time on it.

sub do_cmd_toprule {
    local($_) = @_;
    &ignore_numeric_argument;
    local($pre,$post) = &minimize_open_tags('');
    join('',$pre,$_);
}

sub do_cmd_midrule {
    local($_) = @_;
    &ignore_numeric_argument;
    local($pre,$post) = &minimize_open_tags('');
    join('',$pre,$_);
}
sub do_cmd_bottomrule {
    local($_) = @_;
    &ignore_numeric_argument;
    local($pre,$post) = &minimize_open_tags('');
    join('',$pre,$_);
}

sub do_cmd_tightspacing {
    local($_) = @_;
    &ignore_numeric_argument;
    local($pre,$post) = &minimize_open_tags('<BR>');
    join('',$pre,$_);
}

sub do_cmd_regularspacing {
    local($_) = @_;
    &ignore_numeric_argument;
    local($pre,$post) = &minimize_open_tags('');
    join('',$pre,$_);
}

# Titlepage stuff.
# latex2html base code already defines do_cmd_{title,author,address}.

# A version of the each of the following is defined by the latex2html base
# code, but it produces a mailto: link.  We don't want that because we put
# multiple email addresses in \email{}.

sub do_cmd_authoremail {
    local($_) = @_;
    &get_next_optional_argument;
    local($next);
    $next = &missing_braces unless (
	(s/$next_pair_pr_rx/$next = $2;''/eo)
	||(s/$next_pair_rx/$next = $2;''/eo));
    ($t_email) = &translate_commands($next);
    $_;
}

sub do_cmd_acknowledge {
    local($_) = @_;
    &get_next_optional_argument;
    local($next);
    $next = &missing_braces unless (
	(s/$next_pair_pr_rx/$next = $2;''/eo)
	||(s/$next_pair_rx/$next = $2;''/eo));
    ($t_acknowledge) = &simplify(&translate_commands($next));
    $_;
}

# This redefines the do_cmd_maketitlepage() from the latex2html base code in
# order to handle our specialized constructs and layout.

sub do_cmd_maketitlepage {
    local($_) = @_;
    local($the_title) = '';
    local($alignc, $alignl);
    if ($HTML_VERSION > 2.1) {
	$alignc = " ALIGN=\"CENTER\""; 
	$alignl = " ALIGN=\"LEFT\""; 
    }
    if ($t_title) {
	$the_title .= "<HR NOSHADE>\n<H1$alignc>$t_title</H1>\n<HR NOSHADE>";
    } else { &write_warnings("\nThis document has no title."); }
    if ($t_author) {
	if ($t_authorURL) {
	    local($href) = &translate_commands($t_authorURL);
	    $href = &make_named_href('author'
		         , $href, "<STRONG>${t_author}</STRONG>");
	    $the_title .= "\n<P$alignc>$href</P>";
	} else {
	    $the_title .= "\n<P$alignc><STRONG>$t_author</STRONG></P>";
	}
    } else { &write_warnings("\nThere is no author for this document."); }
    if ($t_email&&!($t_email=~/^\s*(($O|$OP)\d+($C|$CP))\s*\1\s*$/)) {
        $the_title .= "\n<P$alignc><TT>$t_email</TT></P>";}
    if ($t_affil&&!($t_affil=~/^\s*(($O|$OP)\d+($C|$CP))\s*\1\s*$/)) {
	$the_title .= "\n<P$alignc><I>$t_affil</I></P>";}
    if ($t_address&&!($t_address=~/^\s*(($O|$OP)\d+($C|$CP))\s*\1\s*$/)) {
        $the_title .= "\n<P$alignc><SMALL>$t_address</SMALL></P>";}
    if ($t_acknowledge&&!($t_acknowledge=~/^\s*(($O|$OP)\d+($C|$CP))\s*\1\s*$/)) {
        $the_title .= "\n<P$alignc><SMALL>$t_acknowledge</SMALL></P>";}
    if ($t_date&&!($t_date=~/^\s*(($O|$OP)\d+($C|$CP))\s*\1\s*$/)) {
	$the_title .= "\n<P$alignc><STRONG>$t_date</STRONG></P>";}
    # Add the table of contents.
    # FIXME: this should test for $notoc
    $_ .= $the_title;
    $_ .= &do_cmd_tableofcontents();
}

# This redefines the do_cmd_maketitle() from the latex2html base code in
# order to handle our specialized constructs and layout.  This is basically
# identical to do_cmd_maketitlepage, but doesn't insert a table of contents.

sub do_cmd_maketitle {
    local($_) = @_;
    local($the_title) = '';
    local($alignc, $alignl);
    if ($HTML_VERSION > 2.1) {
	$alignc = " ALIGN=\"CENTER\""; 
	$alignl = " ALIGN=\"LEFT\""; 
    }
    if ($t_title) {
	$the_title .= "<HR NOSHADE>\n<H1$alignc>$t_title</H1>\n<HR NOSHADE>";
    } else { &write_warnings("\nThis document has no title."); }
    if ($t_author) {
	if ($t_authorURL) {
	    local($href) = &translate_commands($t_authorURL);
	    $href = &make_named_href('author'
		         , $href, "<STRONG>${t_author}</STRONG>");
	    $the_title .= "\n<P$alignc>$href</P>";
	} else {
	    $the_title .= "\n<P$alignc><STRONG>$t_author</STRONG></P>";
	}
    } else { &write_warnings("\nThere is no author for this document."); }
    if ($t_email&&!($t_email=~/^\s*(($O|$OP)\d+($C|$CP))\s*\1\s*$/)) {
        $the_title .= "\n<P$alignc><TT>$t_email</TT></P>";}
    if ($t_affil&&!($t_affil=~/^\s*(($O|$OP)\d+($C|$CP))\s*\1\s*$/)) {
	$the_title .= "\n<P$alignc><I>$t_affil</I></P>";}
    if ($t_address&&!($t_address=~/^\s*(($O|$OP)\d+($C|$CP))\s*\1\s*$/)) {
        $the_title .= "\n<P$alignc><SMALL>$t_address</SMALL></P>";}
    if ($t_acknowledge&&!($t_acknowledge=~/^\s*(($O|$OP)\d+($C|$CP))\s*\1\s*$/)) {
        $the_title .= "\n<P$alignc><SMALL>$t_acknowledge</SMALL></P>";}
    if ($t_date&&!($t_date=~/^\s*(($O|$OP)\d+($C|$CP))\s*\1\s*$/)) {
	$the_title .= "\n<P$alignc><STRONG>$t_date</STRONG></P>";}
    # Add the table of contents.
    # FIXME: this should test for $notoc
    $_ .= $the_title;
}

# Misc. commands.

sub do_cmd_texttrademark { join('', '&#153;', $_[0]); }

# FIXME: the next one doesn't actually work.

sub do_cmd_textsuperscript {
  local($_) = @_;
  local($next);
  $next = &missing_braces
    unless ( (s/$next_pair_pr_rx/$next = $2;''/eo)
	     || (s/$next_pair_rx/$next = $2;''/eo)
	   );
  ($next) = &translate_commands($next);
  "<SUP>$text</SUP>" . $_;
}

#-----------------------------------------------------------------------------
# End of file.
#-----------------------------------------------------------------------------

1;	# Must be last line
