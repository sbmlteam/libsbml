%% Filename    : sbmlmanual.cls
%% Description : SBML project style for manuals
%% Author(s)   : Michael Hucka <mhucka@@caltech.edu>
%% Organization: CDS, California Institute of Technology
%% Created     : 2004-04-15
%% Revision    : $Id$
%% $Source$
%%
%% Copyright 2004 California Institute of Technology
%% 
%% This file is free software; you can redistribute it and/or modify it
%% under the terms of the GNU Lesser General Public License as published
%% by the Free Software Foundation; either version 2.1 of the License, or
%% any later version.
%% 
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
%% MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
%% documentation provided hereunder is on an "as is" basis, and the
%% California Institute of Technology and Japan Science and Technology
%% Corporation have no obligations to provide maintenance, support,
%% updates, enhancements or modifications.  In no event shall the
%% California Institute of Technology or the Japan Science and Technology
%% Corporation be liable to any party for direct, indirect, special,
%% incidental or consequential damages, including lost profits, arising
%% out of the use of this software and its documentation, even if the
%% California Institute of Technology and/or Japan Science and Technology
%% Corporation have been advised of the possibility of such damage.  See
%% the GNU Lesser General Public License for more details.
%% 
%% You should have received a copy of the GNU Lesser General Public License
%% along with this library; if not, write to the Free Software Foundation,
%% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.

%% ----------------------------------------------------------------------------
%% Identification.
%% ----------------------------------------------------------------------------

%% It would be so nice to be able to put the CVS/RCS 'Date' tag in the
%% \filedate below.  Unfortunately, LaTeX will barf on the resulting string.

\def\filedate{2004/04/15}
\def\fileversion{"$Revision$"}

\NeedsTeXFormat{LaTeX2e}

\ProvidesClass{sbmlmanual}[\filedate\space SBML manual class]

%% ----------------------------------------------------------------------------
%% Definition of package options for this class.
%% ----------------------------------------------------------------------------

\newif\if@notoc
\newif\if@xdvi
\DeclareOption{toc}{\global\let\if@notoc\iffalse}
\DeclareOption{notoc}{\global\let\if@notoc\iftrue}
\DeclareOption{xdvi}{\global\let\if@xdvi\iffalse}
\DeclareOption{oneside}{\@twosidefalse \@mparswitchfalse}
\DeclareOption{twoside}{\@twosidetrue  \@mparswitchtrue}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}

\ExecuteOptions{toc,twoside}
\ProcessOptions
\LoadClass{article}

%% ----------------------------------------------------------------------------
%% Package imports.
%% ----------------------------------------------------------------------------
%% Important: hyperref needs to be the last package loaded, followed by
%% color and colortbl.

% 2002-01-15 <mhucka@caltech.edu> Decided to revert to the default tex font.
% If you use Charter (or just about anything else), then the default
% typewriter font is too hard to distinguish visually.  So then you end up
% trying to find an alternative, like Courier Bold, and the result is way
% too dark on the page.  So until we can get ourselves a professional font
% alternative (e.g., maybe the Y&Y Modern font used in the LaTeX Web
% Companion), I think it's best to use the CM fonts.
%
% Use Charter as the body font.
% \RequirePackage{charter}

% 2002-01-15 <mhucka@caltech.edu>
% This provides a look similar to the Harbison & Steele book on C,
% but I think it's just too heavy when combined with the body
% font we're using here.  Books like those from O'Reilly
% use a non-bold Courier, and it looks a bit more professional
% (at least for the body font we're using).  
% \RequirePackage{courier}
% \renewcommand{\ttdefault}{pcr}

% There seems to be no bold version of the Computer Modern Sans font.
% The following sets things up so that it uses the bold extended, which
% (curiously) *does* exist.  See p. 181 of The LaTeX Companion, 1994.

\def\bfdefault{bx}

% Define a bold typewriter font using the (optional) cmtt.
% You have to download the cmtt metafont files from CTAN.

% 2002-06-17 <mhucka@caltech.edu>
% This really doesn't look very good when translated to PDF.
% Let's stop it for now.

%\DeclareFontShape{OT1}{cmtt}{bx}{n}
%{<5><6><7><8>cmbtt8%
% <9>cmbtt9%
% <10><10.95>cmbtt10%
% <12><14.4><17.28><20.74><24.88>cmbtt10}{}

% The standard latex installation does not have a version of CMS bold
% extended slanted (cmss/bx/sl).  There seems to be at least one 
% implementation of this font available on the net, but it's in Type1
% format and would require work to get running on each developer's
% machine.  So, let's just skip it and set up a substitution.

\DeclareFontShape{OT1}{cmss}{bx}{sl}{<->sub*cmss/bx/n}{}

% Define the reference style.

\RequirePackage[round]{natbib}

% Load up some basic packages we depend on.

\RequirePackage[american]{varioref}
\RequirePackage{ifthen}
\RequirePackage{float}
\RequirePackage{wrapfig}
\RequirePackage{array}
\RequirePackage{alltt}
\RequirePackage{booktabs}
\RequirePackage{html}
\RequirePackage{listings}
% Preload the languages we use.
% Important: do not load "C"; it causes the package to crash on first use.
\lstloadlanguages{Java,[Visual]C++,csh,Delphi,Matlab,make,Perl,Python}

% Get the color package, testing whether pdflatex is being used or not,
% so that we can set the proper switches.
\@ifundefined{pdfoutput}{%
  \if@xdvi
    \RequirePackage[xdvi,usenames]{color}
  \else
    \RequirePackage[dvips,usenames]{color}
  \fi
}{%
  \RequirePackage[pdftex,usenames]{color}
}

% Local color definitions.

\definecolor{sbmlblue}{cmyk}{1.0,0.7,0,0}
\definecolor{sbmlorange}{cmyk}{0,0.75,1,0.24}
\definecolor{verylightgray}{gray}{0.9}
\definecolor{lightgray}{gray}{0.8}
\definecolor{mediumgray}{gray}{0.5}
\definecolor{lightyellow}{rgb}{0.995,0.97,0.93}
\definecolor{darkblue}{rgb}{0.1,0.4,0.55}

% Get the graphicx and hyperref packages, testing whether pdflatex is being
% used or not, so that we can set the proper switches.
\@ifundefined{pdfoutput}{%
  \RequirePackage{graphicx}
  % Test whether we're being run from latex or latex2html
  \latexhtml{
    % For latex.
    \RequirePackage[breaklinks=true,colorlinks=true,
    bookmarks=true,bookmarksopen=true,bookmarksopenlevel=2,
    pdfhighlight=/O,linkcolor={sbmlorange},citecolor={sbmlorange},
    pagecolor={sbmlorange},urlcolor={sbmlorange}]{hyperref}
  }{
    % For latex2html.
    \RequirePackage[latex2html,breaklinks=true,colorlinks=true,
    bookmarks=true,bookmarksopen=true,bookmarksopenlevel=2,
    pdfhighlight=/O,linkcolor={sbmlorange},citecolor={sbmlorange},
    pagecolor={sbmlorange},urlcolor={sbmlorange}]{hyperref}
  }
}{%
  \RequirePackage[pdftex]{graphicx}
  % 2001-05-25 <mhucka@caltech.edu> Note about hyperref: I have spent hours
  % trying to make colorlinks=true work, but failed.  The problem is that there
  % is some interaction between the latex2e color package and dvips, such that
  % if color is used, regular text on the page seems to be printed not as black
  % but as a combination of all RGB colors at 100% density.  The result is 
  % not-quite-black text that is fuzzy and ugly.  I'm giving up and resorting
  % to the default hyperref boxed link style instead of using colored links.
  % I find boxed links more intrusive in the text, but I can't spend any more
  % time debugging this problem.  (Simply avoiding the color package is not
  % an option here because attempting to use colored links requires it.)
  %  \RequirePackage[pdftex,breaklinks=true,
  %  bookmarks=true,bookmarksopen=true,bookmarksopenlevel=2,
  %  pdfhighlight=/O,urlbordercolor={0.8 0.8 1},citebordercolor={0.8 0.8 1},
  %  linkbordercolor={0.8 0.8 1},pagebordercolor={0.8 0.8 1}]{hyperref}

  % 2001-10-22 <mhucka@caltech.edu> Discovered that the problem appears to
  % be only on our NEC SuperScript 4650N printer.  Grr.  So going back to 
  % using colorlinks=true.
  \RequirePackage[pdftex,breaklinks=true,colorlinks=true,
  bookmarks=true,bookmarksopen=true,bookmarksopenlevel=2,
  pdfhighlight=/O,linkcolor={sbmlorange},citecolor={sbmlorange},
  pagecolor={sbmlorange},urlcolor={sbmlorange}]{hyperref}
}

\RequirePackage{colortbl}


%% ----------------------------------------------------------------------------
%% Main code.
%% ----------------------------------------------------------------------------

\raggedbottom

% The default inter-line spacing with 10pt font seems a bit too big.  This
% tightens it ever so slightly.  

\renewcommand{\baselinestretch}{0.98}

% Graphics adjustments.  The path setup is so that the \includegraphics
% in the @puttile definition can find the logo file no matter where the
% document is located (but obviously, it only works for certain path
% combinations -- it's a total hack).

\graphicspath{{./logos/}{../tex/logos/}{../project/tex/logos/}{../../project/tex/logos/}{../../../project/tex/logos/}{../../../../project/tex/logos/}}

% Margin adjustments. I've tried using vmargin.sty, but it interacts badly
% with page numbers at the bottom of the page, so I've resorted to hardcoding
% the dimensions like this.

% I don't understand why the value of the left margin is actually 1 inch +
% \hoffset.  Why bother with the 1 inch, when there are the additional
% parameters \oddsidemargin and \evensidemargin?  Anyway, I'm resetting
% \hoffset to negate the 1 inch so that we start from a clean slate.

\setlength{\hoffset}{-1.0 in}
\setlength{\oddsidemargin}{1.3 in}
\setlength{\evensidemargin}{1.3 in}
\setlength{\textwidth}{5.8 in}
\setlength{\textheight}{9 in}
\setlength{\marginparsep}{9 pt}
\setlength{\marginparwidth}{0.75 in}
\ifx\pdfoutput\undefined
  \setlength{\topmargin}{1 ex}
\else
  \setlength{\topmargin}{-8 ex}
\fi
\setlength{\voffset}{0 in}

% Definition of the paragraph style.

\setlength{\parindent}{0 pt}            % Unindented paragraphs, separated ...
\setlength{\parskip}{1.3 ex}            % ... by roughly one blank line.
\setlength{\partopsep}{-1 ex}

% Fix placement of figures & tables.  This keeps latex from shoving big
% floats to the end of a document when they are somewhat big, which it will
% do even if you put [htb] as the argument.

\setcounter{topnumber}{2}
\renewcommand\topfraction{1.0}
\setcounter{bottomnumber}{2}
\renewcommand\bottomfraction{1.0}
\renewcommand\textfraction{0.0}
\renewcommand\floatpagefraction{0.9}

% Table colors.

\newcommand{\grayrow}{\rowcolor[gray]{0.91}}
\newcommand{\greyrow}{\grayrow}

% Adjustments to package 'booktabs'.

\setlength{\cmidrulewidth}{0.3 pt}
\setlength{\lightrulewidth}{0.3 pt}
\setlength{\heavyrulewidth}{0.9 pt}

% Title style.  This is severely hacked from the orginal article.cls code.

\def\address#1{\gdef\@address{#1}}
\def\@address{\@latex@warning@no@line{No \noexpand\address given}}

\def\authoremail#1{\gdef\@authoremail{#1}}
\def\@authoremail{}

\def\acknowledge#1{\gdef\@acknowledge{#1}}
\def\@acknowledge{}

\def\funding#1{\gdef\@funding{#1}}
\def\@funding{}

\def\@puttitle{%
  \scalebox{1}[0.9]{%
    \fcolorbox{black}{sbmlblue}{%
      \begin{minipage}{\textwidth}%
        \vspace*{9pt}%
        \centering%
        \textcolor{white}{\sffamily\bfseries\Huge \@title}
      \vspace*{8pt}%
    \end{minipage}%
    }}}

\newlength{\bottomgraphicwidth}
\setlength{\bottomgraphicwidth}{\textwidth}
\addtolength{\bottomgraphicwidth}{-26pt}

% IMPORTANT: if you change the following text, make sure to also change
% the corresponding text in sbmlmanual.perl.

\def\@putbottomgraphic{%
  \small{\par\@funding}
  \vskip 10ex%
  \@ifundefined{pdfoutput}{%
    \centerline{\includegraphics[width=2.5in]{sbml-logo}}%
  }{%
    \centerline{\includegraphics[width=2.5in]{sbml-logo.jpg}}%
  }}

\def\@putauthorandaddress{%
  \Large
  \renewcommand{\arraystretch}{0.95}%
  \begin{tabular}[t]{c}%
    \@author
  \end{tabular}%
  \\[0.5 ex]
  \large
  \begin{tabular}[t]{c}%
    {\ttfamily \@authoremail}\\[0.75ex]
    \@address
  \end{tabular}%
  \vskip 1.2 ex%
  \@acknowledge%
  \par}

\def\@puttoc{%
  \if@notoc
  \else
    \begingroup
      \setcounter{tocdepth}{2}%
      \addtolength{\parskip}{-1.5 ex}%
      \tableofcontents%
      \addtolength{\parskip}{1.5 ex}%
    \endgroup
  \fi}

\def\ps@myheadings{%
  \let\@oddfoot\@empty\let\@evenfoot\@empty
  \def\@evenhead{\thepage\hfil\slshape\leftmark}%
  \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
  \let\@mkboth\@gobbletwo
  \let\sectionmark\@gobble
  \let\subsectionmark\@gobble
  }

% Need redefine \tableofcontents to counter effects of hacking \section below.

\renewcommand\tableofcontents{%
  \@startsection{section}{0}{0pt}{-1.8ex \@plus -1ex \@minus -.2ex}%
  {0.8ex}{\normalfont\Large\bfseries\sffamily} *% The star fakes a \section*
  \contentsname%
  \@mkboth{\MakeUppercase\contentsname}{\MakeUppercase\contentsname}%
  \@starttoc{toc}%
  }

\renewcommand\maketitle{\par
  \begingroup
  \renewcommand\thefootnote{\@fnsymbol\c@footnote}%
  \def\@makefnmark{\rlap{\@textsuperscript{\normalfont\@thefnmark}}}%
  \long\def\@makefntext##1{\parindent 1em\noindent
    \hb@xt@1.8em{%
      \hss\@textsuperscript{\normalfont\@thefnmark}}##1}%
  \if@twocolumn
    \ifnum \col@number=\@ne
      \@maketitle
    \else
      \twocolumn[\@maketitle]%
    \fi
  \else
    \global\@topnum\z@   % Prevents figures from going at top of page.
    \@maketitle
  \fi
  \thispagestyle{empty}\@thanks
  \endgroup
  \setcounter{footnote}{0}%
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@author\@empty
  \global\let\@date\@empty
  \global\let\@title\@empty
  \global\let\@authoremail\@empty
  \global\let\@address\@empty
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax
  }

\newcommand\maketitlepage{\par
  \begingroup
  \renewcommand\thefootnote{\@fnsymbol\c@footnote}%
  \def\@makefnmark{\rlap{\@textsuperscript{\normalfont\@thefnmark}}}%
  \long\def\@makefntext##1{\parindent 1em\noindent
    \hb@xt@1.8em{%
      \hss\@textsuperscript{\normalfont\@thefnmark}}##1}%
  \vskip 2em%
  \if@twocolumn
    \ifnum \col@number=\@ne
      \@maketitle
    \else
      \twocolumn[\@maketitle]%
    \fi
  \else
    \newpage
    \global\@topnum\z@   % Prevents figures from going at top of page.
    \@maketitle
  \fi
  \thispagestyle{empty}\@thanks%
%  \vskip 0pt plus 1fil minus 1fil
  \vfill
  \@putbottomgraphic
  \clearpage
  \@puttoc
  \vskip 0pt plus 1fil
  \endgroup
  \clearpage
  \setcounter{footnote}{0}%
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@author\@empty
  \global\let\@date\@empty
  \global\let\@title\@empty
  \global\let\@authoremail\@empty
  \global\let\@address\@empty
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax
  }

\def\@maketitle{%
  \newpage
  \null
  \begin{center}%
    \let \footnote \thanks
    \@puttitle
    \vskip 2em%
    {\lineskip .5em%
      \@putauthorandaddress
      }%
    \vskip 1em%
    {\large \@date}%
  \end{center}%
  \par%
  \vskip 1.5em%
  \ifx\pdfoutput\undefined
  \else
    \hypersetup{pdftitle={\@title},pdfauthor={\@author}}
  \fi%
  }

% Definition of heading style.

\newif\if@mainmatter \@mainmattertrue
\newcommand\@chapapp{\chaptername}
\newcommand\chaptername{Chapter}
\newcommand*\chaptermark[1]{}

\newcommand\chapter{\thispagestyle{plain}%
                    \global\@topnum\z@
                    \@afterindentfalse
                    \secdef\@chapter\@schapter}

\def\@chapter[#1]#2{\ifnum \c@secnumdepth >\m@ne
                       \if@mainmatter
                         \refstepcounter{chapter}%
                         \typeout{\@chapapp\space\thechapter.}%
                         \addcontentsline{toc}{chapter}%
                                   {\protect\numberline{\thechapter}#1}%
                       \else
                         \addcontentsline{toc}{chapter}{#1}%
                       \fi
                    \else
                      \addcontentsline{toc}{chapter}{#1}%
                    \fi
                    \chaptermark{#1}%
                    \addtocontents{lof}{\protect\addvspace{10\p@}}%
                    \addtocontents{lot}{\protect\addvspace{10\p@}}%
                    \if@twocolumn
                      \@topnewpage[\@makechapterhead{#2}]%
                    \else
                      \@makechapterhead{#2}%
                      \@afterheading
                    \fi}

\def\@schapter#1{\if@twocolumn
                   \@topnewpage[\@makeschapterhead{#1}]%
                 \else
                   \@makeschapterhead{#1}%
                   \@afterheading
                 \fi}

\def\@makeschapterhead#1{%
  \vspace*{50\p@}%
  {\parindent \z@ \raggedright
    \normalfont
    \interlinepenalty\@M
    \Huge \bfseries  #1\par\nobreak
    \vskip 40\p@
  }}

\def\@makechapterhead#1{%
  \vspace*{50\p@}%
  {\parindent \z@ \raggedright \normalfont
    \ifnum \c@secnumdepth >\m@ne
      \if@mainmatter
        \hspace*{-20pt}\Huge\sffamily\bfseries \color{sbmlblue}{\@chapapp\space \thechapter}
        \par\nobreak
        \vskip 20\p@
      \fi
    \fi
    \interlinepenalty\@M
    \hspace*{-20pt}\Huge \bfseries #1\par\nobreak
    \vskip 40\p@
  }}

\newlength{\headingwidth}
\setlength{\headingwidth}{\textwidth}
\addtolength{\headingwidth}{33pt}

\renewcommand{\section}{\@startsection%
  {section}{1}{0pt}{-1.8ex \@plus -1ex \@minus -.2ex}%
  {0.8ex}{\hspace*{-35pt}\color{sbmlblue}{\rule{\headingwidth}{0.75pt}}\nopagebreak\\[-2.9pt]%
    \hspace*{-69.6pt}\colorbox{sbmlblue}{\rule{0pt}{12.5pt}\hspace*{10pt}}%
    \hspace*{5pt}\normalfont\Large\bfseries\sffamily}}

\renewcommand{\subsection}{\@startsection%
  {subsection}{2}{0pt}{-2ex \@plus 1ex \@minus -.2ex}%
  {0.8ex}{\large\bfseries\sffamily\color{sbmlblue}}}

\renewcommand{\subsubsection}{\@startsection%
  {subsubsection}{3}{0pt}{-1.5ex \@plus 1ex \@minus -.2ex}%
  {0.5ex}{\color{sbmlblue}\normalsize\bfseries\sffamily\slshape}}

\renewcommand{\paragraph}{\@startsection%
  {paragraph}{4}{0pt}{\baselineskip}%
  {-\fontdimen2\font plus -\fontdimen3\font minus -\fontdimen4\font}
  {\normalsize\bfseries\sffamily\slshape}}

% The References heading needs to be fixed up to account for shift
% we apply to section headings.  These defs are modified from article.cls.

\renewenvironment{thebibliography}[1]%
{\sectionspecial*{\hspace*{24pt}\refname
    \@mkboth{\MakeUppercase\refname}{\MakeUppercase\refname}}%
  \addcontentsline{toc}{section}{\protect\numberline{\refname}}%
  \list{\@biblabel{\@arabic\c@enumiv}}%
  {\settowidth\labelwidth{\@biblabel{#1}}%
    \leftmargin\labelwidth
    \advance\leftmargin\labelsep
    \@openbib@code
    \usecounter{enumiv}%
    \let\p@enumiv\@empty
    \renewcommand\theenumiv{\@arabic\c@enumiv}}%
  \sloppy
  \clubpenalty4000
  \@clubpenalty \clubpenalty
  \widowpenalty4000%
  \sfcode`\.\@m}
{\def\@noitemerr
  {\@latex@warning{Empty `thebibliography' environment}}%
  \endlist}

\newcommand{\sectionspecial}{\@startsection%
  {section}{1}{0pt}{-1.8ex \@plus -1ex \@minus -.2ex}%
  {0.8ex}{\hspace*{-36pt}\color{sbmlblue}{\rule{\headingwidth}{0.75pt}}\\[-2.95pt]%
    \hspace*{-39pt}\colorbox{sbmlblue}{\rule{0pt}{12.5pt}\hspace*{10pt}}%
    \hspace*{5pt}\normalfont\Large\bfseries\sffamily}}


% The following is a modified version of the macro from article.cls.
% It adjusts the vertical spacing in TOC for section lines.

\renewcommand*\l@section[2]{%
  \ifnum \c@tocdepth >\z@
    \addpenalty\@secpenalty
    \addvspace{0.2ex \@plus\p@}%
    \setlength\@tempdima{1.5em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode \bfseries\sffamily
      \advance\leftskip\@tempdima
      \hskip -\leftskip
      #1\nobreak\hfil \nobreak\hb@xt@\@pnumwidth{\hss #2}\par
    \endgroup
  \fi}

\def\@dottedtocline#1#2#3#4#5{%
  \ifnum #1>\c@tocdepth \else
    \vskip \z@ \@plus.2\p@
    {\leftskip #2\relax \rightskip \@tocrmarg \parfillskip -\rightskip
     \parindent #2\relax\@afterindenttrue
     \interlinepenalty\@M
     \leavevmode
     \@tempdima #3\relax
     \advance\leftskip \@tempdima \null\nobreak\hskip -\leftskip
     {\sffamily #4}\nobreak
     \leaders\hbox{$\m@th
        \mkern \@dotsep mu\hbox{.}\mkern \@dotsep
        mu$}\hfill
     \nobreak
     \hb@xt@\@pnumwidth{\hfil\normalfont\sffamily #5}%
     \par}%
  \fi}

% The following was ripped out of caption.sty, version 1.4b.
% Copyright (C) 1994-95 Harald Axel Sommerfeldt
% The first few lines set up the parameters for the layout created
% by this style file.

\newcommand{\captionsize}{\small}
\newcommand{\captionfont}{\captionsize\sffamily\slshape}
\newcommand{\captionlabelfont}{\captionsize\bfseries}

\newlength{\captionmargin}
\newsavebox{\as@captionbox}
\newlength{\as@captionwidth}
\newcommand{\as@normalcaption}[2]{%
  #1 #2\par}
\let\as@caption\as@normalcaption
\newcommand{\as@centercaption}[2]{%
  \parbox[t]{\as@captionwidth}{{\centering#1 #2\par}}}
\let\as@shortcaption\as@centercaption
\newcommand{\as@makecaption}[2]{%
  \setlength{\leftskip}{\captionmargin}%
  \setlength{\rightskip}{\captionmargin}%
  \addtolength{\as@captionwidth}{-2\captionmargin}%
  \captionfont%
  \sbox{\as@captionbox}{{\captionlabelfont #1:} #2}%
  \ifdim \wd\as@captionbox >\as@captionwidth
    \as@caption{{\captionlabelfont #1:}}{#2}%
  \else%
    \as@shortcaption{{\captionlabelfont #1:}}{#2}%
  \fi}
\renewcommand{\@makecaption}[2]{%
  \vskip\abovecaptionskip%
  \setlength{\as@captionwidth}{\linewidth}%
  \as@makecaption{#1}{#2}%
  \vskip\belowcaptionskip}
\ifx\@makerotcaption\undefined
\else
  \typeout{\space\space\space\space\space\space\space\space\space
           `rotating' package detected}
  \renewcommand{\@makerotcaption}[2]{%
    \captionfont%
    \sbox{\as@captionbox}{{\captionlabelfont #1:} #2}%
    \ifdim \wd\as@captionbox > .8\vsize
      \rotatebox{90}{%
        \setlength{\as@captionwidth}{.8\textheight}%
        \begin{minipage}{\as@captionwidth}%
          \as@caption{{\captionlabelfont #1:}}{#2}%
        \end{minipage}}\par
    \else%
      \rotatebox{90}{\usebox{\as@captionbox}}%
    \fi
    \hspace{12pt}}
\fi
\ifx\floatc@plain\undefined
\else
  \typeout{\space\space\space\space\space\space\space\space\space
           `float' package detected}
  \renewcommand\floatc@plain[2]{%
    \setlength{\as@captionwidth}{\linewidth}%
    \as@makecaption{#1}{#2}}
  \ifx\as@ruled\undefined
  \else
    \renewcommand\floatc@ruled[2]{%
      \setlength{\as@captionwidth}{\linewidth}%
      \captionfont%
      \as@caption{{\captionlabelfont #1:}}{#2}}
  \fi
\fi

% Additional new commands.

\newcommand{\class}[1]{\texttt{#1}}
\newcommand{\method}[1]{\texttt{#1}}
\newcommand{\variable}[1]{\texttt{#1}}
\newcommand{\attrib}[1]{\texttt{#1}}
\newcommand{\attribtype}[1]{\texttt{#1}}
\newcommand{\attribvalue}[1]{\texttt{#1}}

% New environment for formatting function/method definitions.

\newlength{\methodboxwidth}
\setlength{\methodboxwidth}{\textwidth}
\addtolength{\methodboxwidth}{-10pt}

\newlength{\methodboxtextwidth}
\setlength{\methodboxtextwidth}{\methodboxwidth}
\addtolength{\methodboxtextwidth}{-20pt}

\newlength{\methodboxrulewidth}
\setlength{\methodboxrulewidth}{\methodboxtextwidth}
\addtolength{\methodboxrulewidth}{4pt}

\newcommand{\methoddefsig}[1]{\gdef\theMethodSig{#1}}
\newsavebox{\theMethodDescription}

\newenvironment{methoddef}[1]{%
  \methoddefsig{#1}%
  \begin{lrbox}{\theMethodDescription}%
    \setlength{\parsep}{1ex}%
    \setlength{\parskip}{1ex}%
    \setlength{\partopsep}{1ex}%
    \begin{minipage}{\methodboxtextwidth}%
  }{%
    \end{minipage}
  \end{lrbox}
  \medskip
  \hspace*{5pt}
  \begin{minipage}{\methodboxwidth}%
    \rule{\methodboxrulewidth}{0.5pt}\\[-1pt]%
    \setlength{\fboxsep}{0pt}
    \setlength{\fboxrule}{0.4pt}
    \fcolorbox{verylightgray}{verylightgray}{%
      \setlength{\fboxrule}{2pt}%
      \fcolorbox{lightyellow}{lightyellow}{%
        \begin{minipage}{\methodboxtextwidth}%
          \small
          \vspace*{2pt}%
          \textsf{\textbf{\theMethodSig}}\\[5pt]
          \usebox{\theMethodDescription}
        \end{minipage}}}%
  \end{minipage}%
  \medskip}

% New figure environment for code examples with line numbers.

\newcommand{\tightspacing}{\renewcommand{\baselinestretch}{0.85}}
\newcommand{\regularspacing}{\renewcommand{\baselinestretch}{1.0}}
\newcommand{\examplespacing}{\renewcommand{\baselinestretch}{0.9}}

\newlength{\codeverbatimwidth}
\setlength{\codeverbatimwidth}{\textwidth}
\addtolength{\codeverbatimwidth}{-2ex}

% The first optional argument must be a language: Java, C, C++, Delphi, etc.
% If any additional arguments needed to be passed to the environment,
% put them after the language name separated by a comma.  Here's an example:
%
% \begin{figure}
%   \begin{boxedCodeVerbatim}[first=20]

\lstnewenvironment{boxedCodeVerbatim}[1][C]{%
  \lstset{escapechar=|,%
    basicstyle=\small\ttfamily,%
    numbers=left,%
    stepnumber=1,%
    numberfirstline=true,%
    numbersep=9pt,%
    numberstyle=\footnotesize,%
    belowskip=5pt,%
    frame=tlrb,%
    framesep=4pt,%
    framerule=0.5pt,%
    frameround=rrrr,%
    rulecolor=\color{sbmlblue},
    flexiblecolumns=true,%
    commentstyle=\footnotesize\itshape,%
    showstringspaces=false,%
    tabsize=2,%x
    keywordstyle=\ttfamily,%
    language=#1}%
  \examplespacing \csname lst@SetFirstLabel\endcsname}
{\regularspacing \csname lst@SaveFirstLabel\endcsname}

\newcommand{\codenote}[1]{\marginpar{\footnotesize\textcolor{sbmlblue}{\emph{#1}}}}

% 2004-09-22 <mhucka@caltech.edu> Turns out that getting verbatim-style
% environments handled properly in latex2html is incredibly hard, because
% latex2html handles 'verbatim' using internal hooks that are not easily
% accessed through the normal latex2html Perl extension mechanism.  I spent
% hours trying to make our original 'example' environment work.  That
% environment used arguments to indicate the language in use.  I've given
% up.  Instead, the following ugly but working solution is to have a
% separate environment for separate languages, and avoid using [csh]
% etc. to indicate the language (or any argument at all).

\newcommand{\code}[1]{\texttt{#1}}
\newcommand{\shell}[1]{\texttt{#1}}

\newcommand{\newVerbatimEnv}[2]{%
  \lstnewenvironment{#1}{%
    \lstset{escapechar=|,%
      basicstyle=\small\ttfamily,%
      aboveskip=0.75\baselineskip,%
      belowskip=0.4\baselineskip,%
      xleftmargin=4ex,%
      commentstyle=\footnotesize\itshape,%
      flexiblecolumns=true,%
      showstringspaces=false,%
      tabsize=2,%x
      keywordstyle=\ttfamily,
      language=#2}%
    \examplespacing \csname lst@SetFirstLabel\endcsname}
  {\regularspacing \csname lst@SaveFirstLabel\endcsname}}

\newVerbatimEnv{cppVerbatim}{[ANSI]C++}
\newVerbatimEnv{cVerbatim}{[ANSI]C}
\newVerbatimEnv{delphiVerbatim}{Delphi}
\newVerbatimEnv{fortranVerbatim}{[90]Fortran}
\newVerbatimEnv{htmlVerbatim}{HTML}
\newVerbatimEnv{javaVerbatim}{Java}
\newVerbatimEnv{latexVerbatim}{LaTeX}
\newVerbatimEnv{lispVerbatim}{Lisp}
\newVerbatimEnv{mathematicaVerbatim}{Mathematica}
\newVerbatimEnv{matlabVerbatim}{Matlab}
\newVerbatimEnv{pascalVerbatim}{Pascal}
\newVerbatimEnv{perlVerbatim}{Perl}
\newVerbatimEnv{pythonVerbatim}{Python}
\newVerbatimEnv{rVerbatim}{R}
\newVerbatimEnv{sqlVerbatim}{SQL}
\newVerbatimEnv{shellVerbatim}{csh}
\newVerbatimEnv{tcltkVerbatim}{[Tk]Tcl}
\newVerbatimEnv{texVerbatim}{[plain]TeX}
\newVerbatimEnv{xmlVerbatim}{XML}

% Lists.

\newlength{\listlabellength}

\newcommand{\methodlistlabel}[1]%
  {\settowidth{\listlabellength}{\texttt{{#1}}}%
    \ifthenelse{\lengthtest{\listlabellength > \labelwidth}}%
      {\parbox[b]{\labelwidth}{\makebox[0pt][l]{\texttt{#1}}\\[2pt]}}%
      {\texttt{#1}}%
   \hfil\relax}

\newenvironment{methodlist}%
  {\begin{list}{}{%
      \renewcommand{\makelabel}{\methodlistlabel}%
      \setlength{\labelwidth}{35pt}%
      \setlength{\leftmargin}{\labelwidth}%
      \addtolength{\leftmargin}{\labelsep}%
      }}%
  {\end{list}}
      

% Macros for use with psfrag.

% The following is for the simplest cases, where we're just interested in
% remapping the font.

\newcommand{\psf}[1]{\psfrag{#1}[Bc][Bc]{\small{#1}}}
\newcommand{\psfsm}[1]{\psfrag{#1}[Bc][Bc]{\footnotesize{#1}}}

% This is for regular cases, with alignment to the center & along the bottom.

\newcommand{\psfmath}[2]{\psfrag{#1}[Bc][Bc]{\small{$#2$}}}
\newcommand{\psfmathsm}[2]{\psfrag{#1}[Bc][Bc]{\footnotesize{$#2$}}}


% Commands for including images.

%begin{latexonly}
\ifx\pdfoutput\undefined
%end{latexonly}
  \newcommand{\incfig}[2][]{\includegraphics[width=#1]{#2.eps}}
  \newcommand{\incfigboxed}[2][]{\def\fboxsep{0pt}\fbox{\includegraphics[width=#1]{#2.eps}}}
  \newcommand{\incimage}[2][]{\includegraphics[width=#1]{#2.eps}}
  \newcommand{\incimageboxed}[2][]{\def\fboxsep{0pt}\fbox{\includegraphics[width=#1]{#2.eps}}}
%begin{latexonly}
\else
  \newcommand{\incfig}[2][]{\includegraphics[width=#1]{#2.pdf}}
  \newcommand{\incfigboxed}[2][]{\def\fboxsep{0pt}\fbox{\includegraphics[width=#1]{#2.pdf}}}
  \newcommand{\incimage}[2][]{\pdfimage width #1 {#2.png}}
  \newcommand{\incimageboxed}[2][]{\def\fboxsep{0pt}\fbox{\pdfimage width #1 {#2.png}}}
\fi
%end{latexonly}

%% -----------------------------------------------------------------------------
%% End of file `sbmlmanual.cls'.
%% -----------------------------------------------------------------------------
