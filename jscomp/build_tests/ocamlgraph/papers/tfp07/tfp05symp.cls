% Style file for the Trends in Functional Programming Symposium Proceedings 2005 <MVE>
%
% Used to be the style file for the Scottish Functional Programming Workshop.
% Mainly a wrapper around article.cls redefining various sizes
%
%% General formating guidelines (not enforced by the style file):
%
% For formating the titlepage use the same macros as in LLNCS-style i.e.
% \title, \author, \institute. Use \and if there are several authors,
% and use the same order for authors and institutes.
%
% Tables and figures:
%
% Center tables and figures. If possible and sensible make them same width
% as text. Use initial caps in the caption of a figure, but initial cap
% only in the caption of a table. Don't put tables or figures immediately
% before or after a section-level heading.
% NB: Following Intellects guidelines, tables are put at the top or the
%     bottom of the page. Hence, the {table} and {figure} environments
%     don't have optional placement parameters.
%
%% Supported options:
%  tfpsymp ... use this for the symposium proceedings; mostly article.cls but with wide text
%            similar to [11,a4paper]{article}
%            NB: article's draft option isn't used any more
%  pagenumbers ... this option adds page numbers of the format
%                  Paper.no.(roman)-Page.no. (e.g. VII-1) to the paper
%  llncs ... gives you LLNCS-style environments (such as example etc) and
%            some LLNCS-style math symbols
%  ams ... gives you amslatex package
%
%% Changelog:
%  $Log: sfp-ws.cls,v $
%  Revision 1.20  2005/07/26 Marko van Eekelen
%  Changed draft to tfpsymp option; renamed file to tfp05symp.cls
%
%  Revision 1.19  2001/08/09 12:55:41  hwloidl
%  Pagenumbering is optional with new option pagenumbers.
%  Fixed a problem with counter reset for theorem-likes at new sections.
%
%  Revision 1.18  2001/08/08 15:20:40  hwloidl
%  Added page numbers for draft option. Format: VII-3 etc
%
%  Revision 1.17  2001/08/06 14:30:55  hwloidl
%  Dropped draft option from article class (draft option to sfp-ws unchanged)
%  Fixed theorem and other llncs envs in non-book setup (untested in book setup)
%
%  Revision 1.16  2001/08/04 01:03:24  hwloidl
%  Changed draft to 11pt
%
%  Revision 1.15  2001/07/31 00:16:04  hwloidl
%  Hacked the version used for the book of SFP'00 to make draft option work
%  agan. Comments starting with %=:-( indicate this hack.
%  This shouldn't break book style but that's untested.
%
%  Revision 1.14  2000/06/30 13:24:13  hwloidl
%  Tiny changes asked for by Intellect:
%  Running header in Bibliography now says `Bibliography'; added to TOC.
%
%  Revision 1.13  2000/06/29 19:34:49  hwloidl
%  Plain pagestyle with draft option.
%
%  Revision 1.12  2000/06/29 19:21:13  hwloidl
%  Fixed a bug in the artice version of the style file.
%  Added the option draft to be used for draft proceedings; it translates to
%  [12pt,draft,a4paper]{article}
%  No pagebreaks after abstact and before bibliography in draft version.
%
%  Revision 1.11  2000/06/15 23:47:36  hwloidl
%  Only change: TOC now only down to section level.
%
%  Revision 1.10  2000/06/15 02:14:11  hwloidl
%  Tiny changes (authors in TOC etc)
%
%  Revision 1.9  2000/06/09 21:57:35  hwloidl
%  Finished typesetting changes for remaining chapters. Started with
%  changes in bibliography but not finished yet.
%
%  Revision 1.8  2000/06/06 14:16:49  hwloidl
%  Typesetting changes. Bib now uses bib-xref for page references in the bib.
%
%  Revision 1.7  2000/05/26 15:54:14  hwloidl
%  Minor typesetting changes
%
%  Revision 1.6  2000/05/24 12:14:02  hwloidl
%  notes on changes for the final versio of the book
%
%  Revision 1.5  2000/05/20 02:27:22  hwloidl
%  clean up
%
%  Revision 1.4  2000/03/06 21:00:36  hwloidl
%  Modifications in sfp-ws.cls style file:
%  changed \@maketitle (better layout and logo), \part and related macros (nuke
%  blank page after part), \chapauthorstoc (new command to add authors into TOC).
%  Changed bodies of chapter, inserting \chapauthorstoc and fixed some figures.
%
%  Revision 1.3  2000/02/10 15:58:45  hwloidl
%  Style file: modified defs of \section, \subsection, \subsubsection, \footnote
%  Body: inserted \addtocounter{footnote}{..} to get numbering of footnotes right.
%
%  Revision 1.2  2000/01/21 14:29:40  hwloidl
%  Modified style file: new headings in text; new titlepage layout; new
%  commands \wstitle, \chaptitle, \chapauthors; modified \bibliography
%  environment in book style.
%
%
%  Modified .tex files to define \chaptitle and \chapauthors in each.
%
%  Revision 1.1.1.1  2000/01/21 02:07:30  hwloidl
%  Latex sources of the SFP'99 proceedings
%
%  Revision 1.3  1999/11/25 14:18:43  hwloidl
%  For final submission
%
%  Revision 1.2  1999/11/24 11:26:16  hwloidl
%  *** empty log message ***
%
%  Revision 1.1  1999/09/27 12:24:17  hwloidl
%  Initial revision
%
% ---------------------------------------------------------------------------

%@menu
%* Load main style file::
%* Sizes::
%* New macros and environments::
%* Modified macros and environments::
%* Misc::
%* Pagestyle etc::
%@end menu

%@node Load main style file, Sizes
%@section Load main style file

\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesFile{sfp-ws} [Scottish Functional Programming Workshop style file $Revision: 1.19 $]

%% Options supported by this style file
\let\if@llncs\iffalse
\DeclareOption{llncs}{\let\if@llncs\iftrue}
\let\if@ams\iffalse
\DeclareOption{ams}{\let\if@ams\iftrue}
\let\if@book\iffalse
\DeclareOption{book}{\let\if@book\iftrue}
\let\if@tfpsymp\iffalse
\DeclareOption{tfpsymp}{\let\if@tfpsymp\iftrue}
\let\if@pagenumbers\iffalse
\DeclareOption{pagenumbers}{\let\if@pagenumbers\iftrue}

\ProcessOptions

% use option titlepage to bring title and abstract to separate pages
\if@book
\LoadClass[10pt,a4paper,titlepage,twoside,openany]{book}
\else
% tfpsymp option for tfpsymp proceedings; basically 11pt article style
\if@tfpsymp
 \LoadClass[11pt,a4paper]{article}
 \else
 \LoadClass[11pt,a4paper,titlepage]{article}
 \fi
\fi

\if@ams
 \usepackage[mathbb]{mathpi}
 \usepackage{amsmath}
 \usepackage{amssymb}
\fi

% inlined from times.sty
\renewcommand{\sfdefault}{phv}
\renewcommand{\rmdefault}{ptm}
\renewcommand{\ttdefault}{pcr}

% inlined from mathptm.sty (jg)
\DeclareSymbolFont{operators}   {OT1}{ptmcm}{m}{n}
\DeclareSymbolFont{letters}     {OML}{ptmcm}{m}{it}
\DeclareSymbolFont{symbols}     {OMS}{pzccm}{m}{n}
\DeclareSymbolFont{largesymbols}{OMX}{psycm}{m}{n}
\DeclareSymbolFont{bold}        {OT1}{ptm}{bx}{n}
\DeclareSymbolFont{italic}      {OT1}{ptm}{m}{it}
\@ifundefined{mathbf}{}{\DeclareMathAlphabet{\mathbf}{OT1}{ptm}{bx}{n}}
\@ifundefined{mathit}{}{\DeclareMathAlphabet{\mathit}{OT1}{ptm}{m}{it}}
\DeclareMathSymbol{\omicron}{0}{operators}{`\o}
\thinmuskip=2mu
\medmuskip=2.5mu plus 1mu minus 1mu
\thickmuskip=4mu plus 1.5mu minus 1mu
  \let\@tempa\version@elt
    \def\version@elt#1{%
      \ifx\mv@bold#1\else\noexpand\version@elt\noexpand#1\fi}
    \edef\version@list{\version@list}
  \let\version@elt\@tempa
  \let\mv@bold\@undefined
\def\boldmath{%
   \PackageWarning{there is no bold Symbol font}%
   \global\let\boldmath=\relax
}
\DeclareMathSizes{5}{5}{5}{5}
\DeclareMathSizes{6}{6}{5}{5}
\DeclareMathSizes{7}{7}{5}{5}
\DeclareMathSizes{8}{8}{6}{5}
\DeclareMathSizes{9}{9}{7}{5}
\DeclareMathSizes{10}{10}{7.4}{6}
\DeclareMathSizes{10.95}{10.95}{8}{6}
\DeclareMathSizes{12}{12}{9}{7}
\DeclareMathSizes{14.4}{14.4}{10.95}{8}
\DeclareMathSizes{17.28}{17.28}{12}{10}
\DeclareMathSizes{20.74}{20.74}{14.4}{12}
\DeclareMathSizes{24.88}{24.88}{17.28}{14.4}

%@node Sizes, New macros and environments, Load main style file
%@section Sizes

% tfpsymp overrides most of the settings in this section
\if@tfpsymp

% all set via option a4paper already

\else

\setlength\textwidth{114mm}
\setlength\textheight{190mm}
\setlength\parskip{0mm}
\setlength\parindent{5mm}
% \setlength\footskip{}
% \setlength\footheight{}

\setlength\topmargin{41mm}
\setlength\oddsidemargin{48mm}
\setlength{\evensidemargin}{\oddsidemargin}
\advance\topmargin by -1in                  % 1in margin is implicit in LaTeX
\advance\oddsidemargin by -1in              % 1in margin is implicit in LaTeX
\advance\evensidemargin by -1in             % 1in margin is implicit in LaTeX

\marginparwidth 0pt         % Margin pars are not allowed.
\marginparsep 11pt          % Horizontal space between outer margin and
                            % marginal note
\marginparpush 5pt       % Minimum vertical separation between two marginal
                         % notes.

                         % indentations for list elements
\leftmargin 1mm
\labelsep 1mm
\labelwidth\leftmargini\advance\labelwidth-\labelsep
% \parsep 4pt plus 2pt minus 1pt (Removed 9 Jun 87)

%\setlength\labelsep{1mm}
\itemsep\baselineskip           % 1 line space between items in the list

% SUBLIST (SL): same as UL, 1m space indent on left side. Lines after first
% line hang under indent
% alas, an indentation of 1m would mean that you have to put your pages
% side by side in order to keep the text on paper ;-) so I choose 1mm instead
% -- HWL
\listparindent 1mm

\leftmargini 5mm
\leftmarginii 5mm
\leftmarginiii 5mm
\leftmarginiv 4mm
\leftmarginv 4mm
\leftmarginvi 4mm

\def\@listI{\leftmargin\leftmargini \parsep 3.6pt plus 2pt minus 1pt%
\topsep 7.2pt plus 2pt minus 4pt%
\itemsep 3.6pt plus 2pt minus 1pt}

\let\@listi\@listI
\@listi

\def\@listii{\leftmargin\leftmarginii
   \labelwidth\leftmarginii\advance\labelwidth-\labelsep
   \topsep 3.6pt plus 2pt minus 1pt
   \parsep 1.8pt plus 0.9pt minus 0.9pt
   \itemsep \parsep}

\def\@listiii{\leftmargin\leftmarginiii
    \labelwidth\leftmarginiii\advance\labelwidth-\labelsep
    \topsep 1.8pt plus 0.9pt minus 0.9pt
    \parsep \z@ \partopsep 1pt plus 0pt minus 1pt
    \itemsep \topsep}

\def\@listiv{\leftmargin\leftmarginiv
     \labelwidth\leftmarginiv\advance\labelwidth-\labelsep}

\def\@listv{\leftmargin\leftmarginv
     \labelwidth\leftmarginv\advance\labelwidth-\labelsep}

\def\@listvi{\leftmargin\leftmarginvi
     \labelwidth\leftmarginvi\advance\labelwidth-\labelsep}


% \setlength\marginparwidth{90\p@}
% \setlength\marginparsep{11\p@}
% \setlength\marginparpush{5\p@}
% \setlength\headheight{0mm}
% \setlength\headsep{0mm}
% \setlength\topskip{0mm}
% \addtolength\footskip{4.23mm}

\fi % tfpsymp

%@node Footnotes, Floating bodies, Sizes, Sizes
%@subsection Footnotes

\footskip 30pt           %    Distance from baseline of box containing foot
                         %    to baseline of last line of text.
\footnotesep 5.6pt    % Height of strut placed at the beginning of every
                      % footnote = height of normal \footnotesize strut,
                      % so no extra space between footnotes.

\skip\footins 8.1pt plus 4pt minus 2pt % Space between last line of text and
                                       % top of first footnote.

%@node Floating bodies, Equations and such, Footnotes, Sizes
%@subsection Floating bodies

\def\topfraction{.7}
\def\bottomfraction{.3}
\def\textfraction{.2}         % HWL: at least 3 lines are required
\def\floatpagefraction{.5}

%% from fpca95.sty

\floatsep 11pt plus 2pt minus 2pt        % Space between adjacent floats moved
                                         % to top or bottom of text page.
\textfloatsep 4mm plus 5pt % HWL         % Space between main text and floats
                                         % at top or bottom of page.
\intextsep 4mm   % HWL                   % Space between in-text figures and
                                         % text.
\@fptop 0pt plus 1fil    % Stretch at top of float page/column. (Must be
                         % 0pt plus ...)
\@fpsep 8pt plus 2fil    % Space between floats on float page/column.
\@fpbot 0pt plus 1fil    % Stretch at bottom of float page/column. (Must be
                         % 0pt plus ... )



%@node Equations and such,  , Floating bodies, Sizes
%@subsection Equations and such

% \mathindent 4mm                      % indentation for equations
\abovedisplayskip \baselineskip      % vert space above a long formula
\belowdisplayskip \baselineskip      % vert space below a long formula
\abovedisplayshortskip \baselineskip      % vert space above a short formula
\belowdisplayshortskip \baselineskip      % vert space below a short formula

%@node New macros and environments, Modified macros and environments, Sizes
%@section New macros and environments

%@node Sizes, Appendix, New macros and environments, New macros and environments
%@subsection Sizes

%% use this for 10pt font on 11pt spacing (default is 10pt on 12pt)
% \renewcommand\normalsize{%
%    \@setfontsize\normalsize\@xpt\@xipt
%    \abovedisplayskip 10\p@ \@plus2\p@ \@minus5\p@
%    \abovedisplayshortskip \z@ \@plus3\p@
%    \belowdisplayshortskip 6\p@ \@plus3\p@ \@minus3\p@
%    \belowdisplayskip \abovedisplayskip
%    \let\@listi\@listI}

\if@tfpsymp

% Take the default sizes from article.cls as defined by e.g. 12pt option
% ToDo: compute sizes based on \@ptsize rather than cut-and-paste

\else

% size in bibliography is basically 9pt
\newcommand\bibsize{%
   \@setfontsize\bibsize\@ixpt{11}%
   \abovedisplayskip 8.5\p@ \@plus3\p@ \@minus4\p@
   \abovedisplayshortskip \z@ \@plus2\p@
   \belowdisplayshortskip 4\p@ \@plus2\p@ \@minus2\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 4\p@ \@plus2\p@ \@minus2\p@
               \parsep 2\p@ \@plus\p@ \@minus\p@
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip
}

% size in contents in tables is  8pt
\newcommand\tablesize{%
   \@setfontsize\tablesize\@viiipt{9.5}%
   \abovedisplayskip 8.5\p@ \@plus3\p@ \@minus4\p@
   \abovedisplayshortskip \z@ \@plus2\p@
   \belowdisplayshortskip 4\p@ \@plus2\p@ \@minus2\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 4\p@ \@plus2\p@ \@minus2\p@
               \parsep 2\p@ \@plus\p@ \@minus\p@
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip
}

% just for debugging   6pt
\newcommand\unreadablesize{%
   \@setfontsize\unreadablesize\@vipt{7.5}%
   \abovedisplayskip 8.5\p@ \@plus3\p@ \@minus4\p@
   \abovedisplayshortskip \z@ \@plus2\p@
   \belowdisplayshortskip 4\p@ \@plus2\p@ \@minus2\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 4\p@ \@plus2\p@ \@minus2\p@
               \parsep 2\p@ \@plus\p@ \@minus\p@
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip
}

\fi % tfpsymp

%@node Appendix, Notes, Sizes, New macros and environments
%@subsection Appendix

\def\appendixname{Appendix}
\def\appendix{\par\c@section\z@ \c@subsection\z@
   \let\sectionname\appendixname
   \normalsize
   \def\thesection{\@Alph\c@section}}

%@node Notes,  , Appendix, New macros and environments
%@subsection Notes

\def\notesname{Notes}
\def\notes{\par\c@section\z@ \c@subsection\z@
   \let\sectionname\notesname
   \if@tfpsymp
     \small
   \else
     \bibsize
   \fi
   \def\thesection{\@Alph\c@section}}

%@node Modified macros and environments, Misc, New macros and environments
%@section Modified macros and environments

%@menu
%* Sectioning commands::
%* Bibliography::
%* Titlepage::
%* Abstract::
%* Quotation::
%* Institute environment::
%* Figures::
%* Tables::
%* Footnotes::
%@end menu

%@node Sectioning commands, Bibliography, Modified macros and environments, Modified macros and environments
%@subsection Sectioning commands

% original taken from article.cls
% spacing taken from IEEEformat
%   URL:http://servo.cogsci.uwo.ca/~elliott/IEEEformat.html

% make section titles bold, 10 point, caps, centered; 2 blank lines before, 1 after
\renewcommand\section{\@startsection {section}{1}{\z@}%
                     % see article.cls
                     {-3.5ex \@plus -1ex \@minus -.2ex}%
                     {2.3ex \@plus.2ex}%
                     %% old sfp-cls
                     %%{-4.5ex \@minus -.2ex}%
                     %%{2.3ex \@plus.2ex}%
                     {\normalfont\normalsize\bfseries\MakeUppercase}} %% HWL dropped: \centering
% make subsection titles bold, underlined, 10 point; 1 blank line before, 1 after
\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
                        % see article.cls
                        {-3.25ex\@plus -1ex \@minus -.2ex}%
                        {1.5ex \@plus .2ex}%
                        %% old sfp-cls
                        %%{-2.8ex \@plus .2ex}%
                        %%{2.8ex \@plus .2ex}%
                        {\normalfont\normalsize\bfseries\hspace{-4.8mm}}} %% HWL dropped: \underline
% make subsubsection titles bold, italics, 10 point; 1 blank line before, 0 after
\renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                           % see article.cls
                           {-3.25ex\@plus -1ex \@minus -.2ex}%
                           {1.5ex \@plus .2ex}%
                           %% old sfp-cls
                           %%{-3.25ex\@plus -1ex \@minus -.2ex}%
                           %%{.5ex \@plus .2ex}%
                           {\normalfont\normalsize\bfseries\itshape}} %% HWL dropped: \raggedright
% make paragraph titles bold, italics, 10 point; 1 blank line before, 0 after
\renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
                       % see article.cls
                       {3.25ex \@plus1ex \@minus.2ex}%
                       {-1em}%
                       %% old sfp-cls
                       %%{3.25ex \@plus1ex \@minus.2ex}%
                       %%{-1em}%
                       {\normalfont\normalsize\bfseries\itshape}}
\renewcommand\subparagraph{\@startsection{subparagraph}{5}{\parindent}%
                          % see article.cls
                          {3.25ex \@plus1ex \@minus .2ex}%
                          {-1em}%
                          %% old sfp-cls
                          %%{3.25ex \@plus1ex \@minus .2ex}%
                          %%{-1em}%
                          {\normalfont\normalsize\bfseries\itshape}}

% same as \section but without uppercase; for use in bibliography only
\newcommand\bibsec{\@startsection {section}{1}{\z@}%
                                   {-4.5ex \@minus -.2ex}%
                                   {2.3ex \@plus.2ex}%
                                   {\normalfont\normalsize\bfseries}} %% HWL dropped: \centering

%@node Bibliography, Titlepage, Sectioning commands, Modified macros and environments
%@subsection Bibliography

\if@book

% chapter env slightly changed to add authors in TOC

% \def\@chapter[#1]#2{\ifnum \c@secnumdepth >\m@ne
%                        \if@mainmatter
%                          \refstepcounter{chapter}%
%                          \typeout{\@chapapp\space\thechapter.}%
%                          \addcontentsline{toc}{chapter}%
%                                    {\protect\numberline{\thechapter}#1}%
%                        \else
%                          \addcontentsline{toc}{chapter}{#1}%
%                        \fi
%                     \else
%                       \addcontentsline{toc}{chapter}{#1}%
%                     \fi
%                     \chaptermark{#1}%
%                     \addtocontents{lof}{\protect\addvspace{10\p@}}%
%                     \addtocontents{lot}{\protect\addvspace{10\p@}}%
%                     \if@twocolumn
%                       \@topnewpage[\@makechapterhead{#2}]%
%                     \else
%                       \@makechapterhead{#2}%
%                       \@afterheading
%                     \fi}

\def\refname{References}
\setlength\bibindent{8mm}

\renewenvironment{thebibliography}[1]
     {\section*{\refname}
        %\@mkboth{\MakeUppercase\refname}{\MakeUppercase\refname}}%
      \chaptitle{\refname}%
      \addcontentsline{toc}{chapter}{\refname}%
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\itemsep 0mm
            \settowidth\labelwidth{\@biblabel{#1}}%
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
      \if@tfpsymp
        \small
      \else
        \bibsize % HWL: 9pt
      \fi
      \sfcode`\.\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}

\else

\def\refname{REFERENCES}
\setlength\bibindent{8mm}

\renewenvironment{thebibliography}[1]
     {\if@tfpsymp
        %\normalsize
        % HWL: no page break in the tfpsymp version
        \small % HWL: 9pt
       \else
         \newpage  % HWL: put bibliography on a separate page
         \bibsize % HWL: 9pt
       \fi
      \bibsec*{\refname
        \@mkboth{\MakeUppercase\refname}{\MakeUppercase\refname}}%
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\itemsep 0mm
            \settowidth\labelwidth{\@biblabel{#1}}%
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

\fi % book

%@node Titlepage, Abstract, Bibliography, Modified macros and environments
%@subsection Titlepage

% Maybe drop support for compatibility flag in sfp-ws.cls (set in
% article.cls) -- HWL

\if@book

% use \titlepage from book.cls without a change

\else

\if@compatibility
\renewenvironment{titlepage}
    {%
      \if@twocolumn
        \@restonecoltrue\onecolumn
      \else
        \@restonecolfalse  % \newpage  % HWL: title and abstract on same page
      \fi
      \thispagestyle{empty}%
      \setcounter{page}\z@
    }%
    {\if@restonecol\twocolumn \else \newpage \fi
    }
\else
\renewenvironment{titlepage}
    {%
      \if@twocolumn
        \@restonecoltrue\onecolumn
      \else
        \@restonecolfalse  % \newpage % HWL: title and abstract on same page
      \fi
      \if@pagenumbers
      \thispagestyle{plain}
      \else
      \thispagestyle{empty}
      \fi
      \setcounter{page}\@ne
    }%
    {\if@restonecol\twocolumn \else \newpage \fi
     \if@twoside\else
        \setcounter{page}\@ne
     \fi
    }
\fi

\fi % book

%@node Abstract, Quotation, Titlepage, Modified macros and environments
%@subsection Abstract

\if@book

% no \abstract environment in a book format

\else

\if@titlepage
  \renewenvironment{abstract}{%
      \titlepage
      \null\vfil
      \@beginparpenalty\@lowpenalty
      \begin{center}%
        \bfseries \abstractname
        \@endparpenalty\@M
      \end{center}
      \noindent % HWL
      \ignorespaces % jg
      %\list{}{\advance\topsep by0.35cm\relax\small
     }%
     {\par\vfil\null\endtitlepage}
\else
  \renewenvironment{abstract}{%
      \if@twocolumn
        \section*{\abstractname}%
      \else
        \small
        \begin{center}%
          {\bfseries \abstractname\vspace{-.5em}\vspace{\z@}}%
        \end{center}%
        \quotation
        \noindent % HWL
        \ignorespaces % jg
        %\list{}{\advance\topsep by0.35cm\relax\small
      \fi}
      {\if@twocolumn\else\endquotation\fi}
\fi % titlepage

\fi % book

%@node Quotation, Institute environment, Abstract, Modified macros and environments
%@subsection Quotation

% HWL: 4mm indent for quotation like environments
\renewenvironment{verse}
               {\let\\\@centercr
                \list{}{\itemsep      \z@
                        \itemindent   0pt%
                        \leftmargin 4mm %
                        \listparindent\itemindent
                        \rightmargin  \leftmargin
                        \advance\leftmargin 1.5em}%
                \item\relax}
               {\endlist}
\renewenvironment{quotation}
               {\list{}{\listparindent 0pt%
                        \itemindent    \listparindent
                        \leftmargin 4mm
                        \rightmargin   \leftmargin
                        \parsep        \z@ \@plus\p@}%
                \item\relax}
               {\endlist}
\renewenvironment{quote}
               {\list{}{\listparindent 0pt
                        \leftmargin \parindent %% HWL: same indentation as para
                        \rightmargin\leftmargin}%
                \item\relax}
               {\endlist}

%@node Institute environment, Figures, Quotation, Modified macros and environments
%@subsection Institute environment

%% Taken from llncs.cls

% LaTeX does not provide a command to enter the authors institute
% addresses. The \institute command is defined here.

\newdimen\headlineindent             % dimension for space between
\headlineindent=1.166cm              % number and text of headings.

\newcounter{@inst}
\newcounter{@auth}
\newcounter{auco}
\def\andname{and}
\def\lastandname{\unskip, and}
\newdimen\instindent
\newbox\authrun
\newtoks\authorrunning
\newtoks\tocauthor
\newbox\titrun
\newtoks\titlerunning
\newtoks\toctitle

\def\clearheadinfo{\gdef\@author{No Author Given}%
                   \gdef\@title{No Title Given}%
                   \gdef\@subtitle{}%
                   \gdef\@institute{No Institute Given}%
                   \gdef\@thanks{}%
                   \global\titlerunning={}\global\authorrunning={}%
                   \global\toctitle={}\global\tocauthor={}}

\def\institute#1{{\gdef\@institute{#1}}}

\def\institutename{\par
 \begingroup
 \parskip=\z@
 \parindent=\z@
 \setcounter{@inst}{1}%
 \def\and{\par\stepcounter{@inst}%
 \noindent$^{\the@inst}$\enspace\ignorespaces}%
 \setbox0=\vbox{\def\thanks##1{}\@institute}%
 \ifnum\c@@inst=1\relax
 \else
   \setcounter{footnote}{\c@@inst}%
   \setcounter{@inst}{1}%
   \noindent$^{\the@inst}$\enspace
 \fi
 \ignorespaces
 \@institute\par
 \endgroup}

\def\@fnsymbol#1{\ensuremath{\ifcase#1\or\star\or{\star\star}\or
   {\star\star\star}\or \dagger\or \ddagger\or
   \mathchar "278\or \mathchar "27B\or \|\or **\or \dagger\dagger
   \or \ddagger\ddagger \else\@ctrerr\fi}}

\def\inst#1{\unskip$^{#1}$}
\def\fnmsep{\unskip$^,$}
\def\email#1{{\tt#1}}
\def\url#1{#1}
\def\homedir{\~{ }}

\def\subtitle#1{\gdef\@subtitle{#1}}
\clearheadinfo

\if@book

% with book option use the titlepage layout from book.cls

\def\logointellect{\LARGE{\bf intellect}$^{\Large \rm \sc tm}$}

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
      \newpage
      \global\@topnum\z@   % Prevents figures from going at top of page.
      \@maketitle
    \fi
      \if@pagenumbers
      \thispagestyle{plain}\@thanks  % HWL: plain pagestyle with funnynumbers
      \else
      \thispagestyle{empty}\@thanks  % HWL: empty pagestyle for titlepage
      \fi
  \endgroup
  \setcounter{footnote}{0}%
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@author\@empty
  \global\let\@date\@empty
  \global\let\@title\@empty
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax
}

\def\@maketitle{%
  \newpage
  \null
  \vskip 2em%
  \begin{center}%
  \let \footnote \thanks
    {\Huge \@title \par}%
    \vskip 1.5em%
    {\Large
      \lineskip .5em%
      \begin{tabular}[t]{c}%
        \@author
      \end{tabular}\par}%
    \vskip 1em%
    {\Large \@date}%
    \par%
    \vfill%
    %STG removed PostScript file inclusion
    %\psfig{width=4cm,file=sfp.ps}%
    %\vskip 1.5em
    \vfill%
    {\centering \logointellect}%
  \end{center}%
  % \begin{tabular}[t]{c} \logointellect \end{tabular}}
}
\else

% without book option (i.e. article style) use an llncs-like layout

\renewcommand\maketitle{\newpage
  \stepcounter{section}%
  \setcounter{section}{0}%
  \setcounter{subsection}{0}%
  \setcounter{figure}{0}
  \setcounter{table}{0}
  \setcounter{equation}{0}
  \setcounter{footnote}{0}%
  \begingroup
    \parindent=\z@
    \renewcommand\thefootnote{\@fnsymbol\c@footnote}%
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
      \if@pagenumbers
      \thispagestyle{plain}\@thanks % HWL: page numbering for titlepage
      \else
      \thispagestyle{empty}\@thanks % HWL: page numbering for titlepage
      \fi
%
    \def\\{\unskip\ \ignorespaces}\def\inst##1{\unskip{}}%
    \def\thanks##1{\unskip{}}\def\fnmsep{\unskip}%
    \instindent=\hsize
    \advance\instindent by-\headlineindent
    \if!\the\toctitle!\addcontentsline{toc}{title}{\@title}\else
       \addcontentsline{toc}{title}{\the\toctitle}\fi
%
    \if!\the\tocauthor!\relax
      {\def\and{\noexpand\protect\noexpand\and}%
      \protected@xdef\toc@uthor{\@author}}%
    \else
      \def\\{\noexpand\protect\noexpand\newline}%
      \protected@xdef\scratch{\the\tocauthor}%
      \protected@xdef\toc@uthor{\scratch}%
    \fi
    \addtocontents{toc}{{\protect\raggedright\protect\leftskip15\p@
    \protect\rightskip\@tocrmarg
    \protect\itshape\toc@uthor\protect\endgraf}}%
  \endgroup
  \setcounter{footnote}{0}%
  \clearheadinfo}
%
\def\@maketitle{\newpage
 \markboth{}{}%
 \def\lastand{\ifnum\value{@inst}=2\relax
                 \unskip{} \andname\
              \else
                 \unskip \lastandname\
              \fi}%
 \def\and{\stepcounter{@auth}\relax
          \ifnum\value{@auth}=\value{@inst}%
             \lastand
          \else
             \unskip,
          \fi}%
 \begin{center}%
 {\Large \bfseries\boldmath
  \pretolerance=10000
  \@title \par}\vskip .8cm
\if!\@subtitle!\else {\large \bfseries\boldmath
  \vskip -.65cm
  \pretolerance=10000
  \@subtitle \par}\vskip .8cm\fi
 \setbox0=\vbox{\setcounter{@auth}{1}\def\and{\stepcounter{@auth}}%
 \def\thanks##1{}\@author}%
 \global\value{@inst}=\value{@auth}%
 \global\value{auco}=\value{@auth}%
 \setcounter{@auth}{1}%
{\lineskip .5em
\noindent\ignorespaces
\@author\vskip.35cm}
 {\small\institutename}
 \end{center}%
 }
\fi % book
%% end llncs.cls

% %% variables normally set in llnc.cls (using default values)
% ToDo: check whether we can use the same defs as below
% HWL: actually theorem-like envs are numbered: <chapter>.<number>
%=:-( \if@book
%=:-( \let\if@envcntreset\iffalse
%=:-( \let\if@envcntsect\iffalse
%=:-( \let\if@envcntsame\iffalse
%=:-( \else
% variables normally set in llnc.cls (using default values)
\let\if@envcntreset\iffalse
\let\if@envcntsect\iffalse
\let\if@envcntsame\iftrue
%=:-(\fi % book

\if@llncs

%% LLNCS style environments

% definition of the "\spnewtheorem" command.
%
% Usage:
%
%     \spnewtheorem{env_nam}{caption}[within]{cap_font}{body_font}
% or  \spnewtheorem{env_nam}[numbered_like]{caption}{cap_font}{body_font}
% or  \spnewtheorem*{env_nam}{caption}{cap_font}{body_font}
%
% New is "cap_font" and "body_font". It stands for
% fontdefinition of the caption and the text itself.
%
% "\spnewtheorem*" gives a theorem without number.
%
% A defined spnewthoerem environment is used as described
% by Lamport.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\def\@thmcountersep{}
\def\@thmcounterend{.}

\def\spnewtheorem{\@ifstar{\@sthm}{\@Sthm}}

% definition of \spnewtheorem with number

\def\@spnthm#1#2{%
  \@ifnextchar[{\@spxnthm{#1}{#2}}{\@spynthm{#1}{#2}}}
\def\@Sthm#1{\@ifnextchar[{\@spothm{#1}}{\@spnthm{#1}}}

\def\@spxnthm#1#2[#3]#4#5{\expandafter\@ifdefinable\csname #1\endcsname
   {\@definecounter{#1}\@addtoreset{#1}{#3}%
   \expandafter\xdef\csname the#1\endcsname{\expandafter\noexpand
     \csname the#3\endcsname \noexpand\@thmcountersep \@thmcounter{#1}}%
   \expandafter\xdef\csname #1name\endcsname{#2}%
   \global\@namedef{#1}{\@spthm{#1}{\csname #1name\endcsname}{#4}{#5}}%
                              \global\@namedef{end#1}{\@endtheorem}}}

\def\@spynthm#1#2#3#4{\expandafter\@ifdefinable\csname #1\endcsname
   {\@definecounter{#1}%
   \expandafter\xdef\csname the#1\endcsname{\@thmcounter{#1}}%
   \expandafter\xdef\csname #1name\endcsname{#2}%
   \global\@namedef{#1}{\@spthm{#1}{\csname #1name\endcsname}{#3}{#4}}%
                               \global\@namedef{end#1}{\@endtheorem}}}

\def\@spothm#1[#2]#3#4#5{%
  \@ifundefined{c@#2}{\@latexerr{No theorem environment `#2' defined}\@eha}%
  {\expandafter\@ifdefinable\csname #1\endcsname
  {\global\@namedef{the#1}{\@nameuse{the#2}}%
  \expandafter\xdef\csname #1name\endcsname{#3}%
  \global\@namedef{#1}{\@spthm{#2}{\csname #1name\endcsname}{#4}{#5}}%
  \global\@namedef{end#1}{\@endtheorem}}}}

\def\@spthm#1#2#3#4{\topsep 7\p@ \@plus2\p@ \@minus4\p@
\refstepcounter{#1}%
\@ifnextchar[{\@spythm{#1}{#2}{#3}{#4}}{\@spxthm{#1}{#2}{#3}{#4}}}

\def\@spxthm#1#2#3#4{\@spbegintheorem{#2}{\csname the#1\endcsname}{#3}{#4}%
                    \ignorespaces}

\def\@spythm#1#2#3#4[#5]{\@spopargbegintheorem{#2}{\csname
       the#1\endcsname}{#5}{#3}{#4}\ignorespaces}

\def\@spbegintheorem#1#2#3#4{\trivlist
                 \item[\hskip\labelsep{#3#1\ #2\@thmcounterend}]#4}

\def\@spopargbegintheorem#1#2#3#4#5{\trivlist
      \item[\hskip\labelsep{#4#1\ #2}]{#4(#3)\@thmcounterend\ }#5}

% definition of \spnewtheorem* without number

\def\@sthm#1#2{\@Ynthm{#1}{#2}}

\def\@Ynthm#1#2#3#4{\expandafter\@ifdefinable\csname #1\endcsname
   {\global\@namedef{#1}{\@Thm{\csname #1name\endcsname}{#3}{#4}}%
    \expandafter\xdef\csname #1name\endcsname{#2}%
    \global\@namedef{end#1}{\@endtheorem}}}

\def\@Thm#1#2#3{\topsep 7\p@ \@plus2\p@ \@minus4\p@
\@ifnextchar[{\@Ythm{#1}{#2}{#3}}{\@Xthm{#1}{#2}{#3}}}

\def\@Xthm#1#2#3{\@Begintheorem{#1}{#2}{#3}\ignorespaces}

\def\@Ythm#1#2#3[#4]{\@Opargbegintheorem{#1}
       {#4}{#2}{#3}\ignorespaces}

\def\@Begintheorem#1#2#3{#3\trivlist
                           \item[\hskip\labelsep{#2#1\@thmcounterend}]}

\def\@Opargbegintheorem#1#2#3#4{#4\trivlist
      \item[\hskip\labelsep{#3#1}]{#3(#2)\@thmcounterend\ }}

\if@envcntsect
   \def\@thmcountersep{.}
   \spnewtheorem{theorem}{Theorem}[section]{\bfseries}{\itshape}
\else
   \spnewtheorem{theorem}{Theorem}{\bfseries}{\itshape}
   \if@envcntreset
      \@addtoreset{theorem}{section}
   \else
      \@addtoreset{theorem}{chapter}
   \fi
\fi

%definition of divers theorem environments
\spnewtheorem*{claim}{Claim}{\itshape}{\rmfamily}
\spnewtheorem*{proof}{Proof}{\itshape}{\rmfamily}
\if@envcntsame % alle Umgebungen wie Theorem.
   \def\spn@wtheorem#1#2#3#4{\@spothm{#1}[theorem]{#2}{#3}{#4}}
\else % alle Umgebungen mit eigenem Zaehler
   \if@envcntsect % mit section numeriert
      \def\spn@wtheorem#1#2#3#4{\@spxnthm{#1}{#2}[chapter]{#3}{#4}} % HWL: was [section]
   \else % nicht mit section numeriert
      \if@envcntreset
         \def\spn@wtheorem#1#2#3#4{\@spynthm{#1}{#2}{#3}{#4}
                                   \@addtoreset{#1}{section}}
      \else
         \def\spn@wtheorem#1#2#3#4{\@spynthm{#1}{#2}{#3}{#4}
                                   \@addtoreset{#1}{chapter}}%
      \fi
   \fi
\fi
\spn@wtheorem{case}{Case}{\itshape}{\rmfamily}
\spn@wtheorem{conjecture}{Conjecture}{\itshape}{\rmfamily}
\spn@wtheorem{corollary}{Corollary}{\bfseries}{\itshape}
\spn@wtheorem{definition}{Definition}{\bfseries}{\itshape}
\spn@wtheorem{example}{Example}{\itshape}{\rmfamily}
\spn@wtheorem{exercise}{Exercise}{\itshape}{\rmfamily}
\spn@wtheorem{lemma}{Lemma}{\bfseries}{\itshape}
\spn@wtheorem{note}{Note}{\itshape}{\rmfamily}
\spn@wtheorem{problem}{Problem}{\itshape}{\rmfamily}
\spn@wtheorem{property}{Property}{\itshape}{\rmfamily}
\spn@wtheorem{proposition}{Proposition}{\bfseries}{\itshape}
\spn@wtheorem{question}{Question}{\itshape}{\rmfamily}
\spn@wtheorem{solution}{Solution}{\itshape}{\rmfamily}
\spn@wtheorem{remark}{Remark}{\itshape}{\rmfamily}

%% LLNCS style math symbols

\DeclareMathSymbol{\Gamma}{\mathalpha}{letters}{"00}
\DeclareMathSymbol{\Delta}{\mathalpha}{letters}{"01}
\DeclareMathSymbol{\Theta}{\mathalpha}{letters}{"02}
\DeclareMathSymbol{\Lambda}{\mathalpha}{letters}{"03}
\DeclareMathSymbol{\Xi}{\mathalpha}{letters}{"04}
\DeclareMathSymbol{\Pi}{\mathalpha}{letters}{"05}
\DeclareMathSymbol{\Sigma}{\mathalpha}{letters}{"06}
\DeclareMathSymbol{\Upsilon}{\mathalpha}{letters}{"07}
\DeclareMathSymbol{\Phi}{\mathalpha}{letters}{"08}
\DeclareMathSymbol{\Psi}{\mathalpha}{letters}{"09}
\DeclareMathSymbol{\Omega}{\mathalpha}{letters}{"0A}

\let\footnotesize\small

\def\vec#1{\mathchoice{\mbox{\boldmath$\displaystyle#1$}}
{\mbox{\boldmath$\textstyle#1$}}
{\mbox{\boldmath$\scriptstyle#1$}}
{\mbox{\boldmath$\scriptscriptstyle#1$}}}

\def\squareforqed{\hbox{\rlap{$\sqcap$}$\sqcup$}}
\def\qed{\ifmmode\squareforqed\else{\unskip\nobreak\hfil
\penalty50\hskip1em\null\nobreak\hfil\squareforqed
\parfillskip=0pt\finalhyphendemerits=0\endgraf}\fi}

\def\getsto{\mathrel{\mathchoice {\vcenter{\offinterlineskip
\halign{\hfil
$\displaystyle##$\hfil\cr\gets\cr\to\cr}}}
{\vcenter{\offinterlineskip\halign{\hfil$\textstyle##$\hfil\cr\gets
\cr\to\cr}}}
{\vcenter{\offinterlineskip\halign{\hfil$\scriptstyle##$\hfil\cr\gets
\cr\to\cr}}}
{\vcenter{\offinterlineskip\halign{\hfil$\scriptscriptstyle##$\hfil\cr
\gets\cr\to\cr}}}}}
\def\lid{\mathrel{\mathchoice {\vcenter{\offinterlineskip\halign{\hfil
$\displaystyle##$\hfil\cr<\cr\noalign{\vskip1.2pt}=\cr}}}
{\vcenter{\offinterlineskip\halign{\hfil$\textstyle##$\hfil\cr<\cr
\noalign{\vskip1.2pt}=\cr}}}
{\vcenter{\offinterlineskip\halign{\hfil$\scriptstyle##$\hfil\cr<\cr
\noalign{\vskip1pt}=\cr}}}
{\vcenter{\offinterlineskip\halign{\hfil$\scriptscriptstyle##$\hfil\cr
<\cr
\noalign{\vskip0.9pt}=\cr}}}}}
\def\gid{\mathrel{\mathchoice {\vcenter{\offinterlineskip\halign{\hfil
$\displaystyle##$\hfil\cr>\cr\noalign{\vskip1.2pt}=\cr}}}
{\vcenter{\offinterlineskip\halign{\hfil$\textstyle##$\hfil\cr>\cr
\noalign{\vskip1.2pt}=\cr}}}
{\vcenter{\offinterlineskip\halign{\hfil$\scriptstyle##$\hfil\cr>\cr
\noalign{\vskip1pt}=\cr}}}
{\vcenter{\offinterlineskip\halign{\hfil$\scriptscriptstyle##$\hfil\cr
>\cr
\noalign{\vskip0.9pt}=\cr}}}}}
\def\grole{\mathrel{\mathchoice {\vcenter{\offinterlineskip
\halign{\hfil
$\displaystyle##$\hfil\cr>\cr\noalign{\vskip-1pt}<\cr}}}
{\vcenter{\offinterlineskip\halign{\hfil$\textstyle##$\hfil\cr
>\cr\noalign{\vskip-1pt}<\cr}}}
{\vcenter{\offinterlineskip\halign{\hfil$\scriptstyle##$\hfil\cr
>\cr\noalign{\vskip-0.8pt}<\cr}}}
{\vcenter{\offinterlineskip\halign{\hfil$\scriptscriptstyle##$\hfil\cr
>\cr\noalign{\vskip-0.3pt}<\cr}}}}}
\def\bbbr{{\rm I\!R}} %reelle Zahlen
\def\bbbm{{\rm I\!M}}
\def\bbbn{{\rm I\!N}} %natuerliche Zahlen
\def\bbbf{{\rm I\!F}}
\def\bbbh{{\rm I\!H}}
\def\bbbk{{\rm I\!K}}
\def\bbbp{{\rm I\!P}}
\def\bbbone{{\mathchoice {\rm 1\mskip-4mu l} {\rm 1\mskip-4mu l}
{\rm 1\mskip-4.5mu l} {\rm 1\mskip-5mu l}}}
\def\bbbc{{\mathchoice {\setbox0=\hbox{$\displaystyle\rm C$}\hbox{\hbox
to0pt{\kern0.4\wd0\vrule height0.9\ht0\hss}\box0}}
{\setbox0=\hbox{$\textstyle\rm C$}\hbox{\hbox
to0pt{\kern0.4\wd0\vrule height0.9\ht0\hss}\box0}}
{\setbox0=\hbox{$\scriptstyle\rm C$}\hbox{\hbox
to0pt{\kern0.4\wd0\vrule height0.9\ht0\hss}\box0}}
{\setbox0=\hbox{$\scriptscriptstyle\rm C$}\hbox{\hbox
to0pt{\kern0.4\wd0\vrule height0.9\ht0\hss}\box0}}}}
\def\bbbq{{\mathchoice {\setbox0=\hbox{$\displaystyle\rm
Q$}\hbox{\raise
0.15\ht0\hbox to0pt{\kern0.4\wd0\vrule height0.8\ht0\hss}\box0}}
{\setbox0=\hbox{$\textstyle\rm Q$}\hbox{\raise
0.15\ht0\hbox to0pt{\kern0.4\wd0\vrule height0.8\ht0\hss}\box0}}
{\setbox0=\hbox{$\scriptstyle\rm Q$}\hbox{\raise
0.15\ht0\hbox to0pt{\kern0.4\wd0\vrule height0.7\ht0\hss}\box0}}
{\setbox0=\hbox{$\scriptscriptstyle\rm Q$}\hbox{\raise
0.15\ht0\hbox to0pt{\kern0.4\wd0\vrule height0.7\ht0\hss}\box0}}}}
\def\bbbt{{\mathchoice {\setbox0=\hbox{$\displaystyle\rm
T$}\hbox{\hbox to0pt{\kern0.3\wd0\vrule height0.9\ht0\hss}\box0}}
{\setbox0=\hbox{$\textstyle\rm T$}\hbox{\hbox
to0pt{\kern0.3\wd0\vrule height0.9\ht0\hss}\box0}}
{\setbox0=\hbox{$\scriptstyle\rm T$}\hbox{\hbox
to0pt{\kern0.3\wd0\vrule height0.9\ht0\hss}\box0}}
{\setbox0=\hbox{$\scriptscriptstyle\rm T$}\hbox{\hbox
to0pt{\kern0.3\wd0\vrule height0.9\ht0\hss}\box0}}}}
\def\bbbs{{\mathchoice
{\setbox0=\hbox{$\displaystyle     \rm S$}\hbox{\raise0.5\ht0\hbox
to0pt{\kern0.35\wd0\vrule height0.45\ht0\hss}\hbox
to0pt{\kern0.55\wd0\vrule height0.5\ht0\hss}\box0}}
{\setbox0=\hbox{$\textstyle        \rm S$}\hbox{\raise0.5\ht0\hbox
to0pt{\kern0.35\wd0\vrule height0.45\ht0\hss}\hbox
to0pt{\kern0.55\wd0\vrule height0.5\ht0\hss}\box0}}
{\setbox0=\hbox{$\scriptstyle      \rm S$}\hbox{\raise0.5\ht0\hbox
to0pt{\kern0.35\wd0\vrule height0.45\ht0\hss}\raise0.05\ht0\hbox
to0pt{\kern0.5\wd0\vrule height0.45\ht0\hss}\box0}}
{\setbox0=\hbox{$\scriptscriptstyle\rm S$}\hbox{\raise0.5\ht0\hbox
to0pt{\kern0.4\wd0\vrule height0.45\ht0\hss}\raise0.05\ht0\hbox
to0pt{\kern0.55\wd0\vrule height0.45\ht0\hss}\box0}}}}
\def\bbbz{{\mathchoice {\hbox{$\mathsf\textstyle Z\kern-0.4em Z$}}
{\hbox{$\mathsf\textstyle Z\kern-0.4em Z$}}
{\hbox{$\mathsf\scriptstyle Z\kern-0.3em Z$}}
{\hbox{$\mathsf\scriptscriptstyle Z\kern-0.2em Z$}}}}

\else % !llncs

% NB: no example etc environments

\fi % llncs

%% HWL final: same numbering as for figure and table
%%            this is a HAAAAAAAAAAAAAACK
%%            ToDo: integrate this into definition of the environments above
%\newcounter{figure}
\if@book
\renewcommand \thetheorem
     {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@theorem}
\else
\def\thetheorem{\@arabic\c@theorem}
\fi

\if@book
\renewcommand \theexample
     {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@example}
\else
\def\theexample{\@arabic\c@example}
\fi

\if@book
\renewcommand \theconjecture
     {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@conjecture}
\else
\def\theconjecture{\@arabic\c@conjecture}
\fi

\if@book
\renewcommand \thecorollary
     {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@corollary}
\else
\def\thecorollary{\@arabic\c@corollary}
\fi

\if@book
\renewcommand \thedefinition
     {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@definition}
\else
\def\thedefinition{\@arabic\c@definition}
\fi

\if@book
\renewcommand \theexercise
     {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@exercise}
\else
\def\theexercise{\@arabic\c@exercise}
\fi

\if@book
\renewcommand \thelemma
     {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@lemma}
\else
\def\thelemma{\@arabic\c@lemma}
\fi

\if@book
\renewcommand \theproblem
     {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@problem}
\else
\def\theproblem{\@arabic\c@problem}
\fi

\if@book
\renewcommand \theproperty
     {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@property}
\else
\def\theproperty{\@arabic\c@property}
\fi

\if@book
\renewcommand \theproposition
     {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@proposition}
\else
\def\theproposition{\@arabic\c@proposition}
\fi

\if@book
\renewcommand \thequestion
     {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@question}
\else
\def\thequestion{\@arabic\c@question}
\fi

\if@book
\renewcommand \thesolution
     {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@solution}
\else
\def\thesolution{\@arabic\c@solution}
\fi

\if@book
\renewcommand \theremark
     {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@remark}
\else
\def\theremark{\@arabic\c@remark}
\fi


%@node Figures, Tables, Institute environment, Modified macros and environments
%@subsection Figures

\def\fps@figure{tb}

\renewcommand{\figurename}{FIGURE~}

% HWL: whole line bold; extra 4mm space before title
\long\def\@makecaption#1#2{%
  \if@tfpsymp
    \small \bfseries   % HWL: 12pt bold in tfpsymp
  \else
    \bibsize \bfseries      % HWL: 9pt bold
  \fi
  \vskip\abovecaptionskip
  \sbox\@tempboxa{#1.\negthinspace\hspace{4mm}#2}
  \ifdim \wd\@tempboxa >\hsize
    #1.\negthinspace\hspace{4mm}#2\par
  \else
    \global \@minipagefalse
    \hb@xt@\hsize{\hfil\box\@tempboxa\hfil}%
  \fi
  \vskip\belowcaptionskip}

%\newcounter{figure}
\if@book
\renewcommand \thefigure
     {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@figure}
\else
\def\thefigure{\@arabic\c@figure}
\fi

\def\fps@figure{tb}
\def\ftype@figure{1}
\def\ext@figure{lof}
\def\fnum@figure{\figurename~\thefigure}
\def\figure{\@float{figure}}
\let\endfigure\end@float
%\@namedef{figure*}{\@dblfloat{figure}}
%\@namedef{endfigure*}{\end@dblfloat}

\renewenvironment{figure}
               {%
                \setlength\abovecaptionskip{\baselineskip}%
                \setlength\belowcaptionskip{\baselineskip}%
                \@float{figure}}
               {\end@float}
\renewenvironment{figure*}
               {%
                \setlength\abovecaptionskip{\baselineskip}%
                \setlength\belowcaptionskip{\baselineskip}%
                \@dblfloat{figure}}
               {\end@dblfloat}

%@node Tables, Footnotes, Figures, Modified macros and environments
%@subsection Tables

\renewcommand{\tablename}{TABLE~}

\def\fps@table{tb}
\def\fnum@table{\tablename\thetable}
% \def\@maketable#1#2{%
%  \setlength\abovecaptionskip{\baselineskip} \setlength\belowcaptionskip{\baselineskip} \@float{table}[#1] #2 \end@float}

% \long\def\@caption#1[#2]#3{\par\addcontentsline{\csname
%   ext@#1\endcsname}{#1}{\protect\numberline{\csname
%   the#1\endcsname}{\ignorespaces #2}}\begingroup
%     \@parboxrestore
%     \@makecaption{\csname fnum@#1\endcsname}{\ignorespaces #3}\par
%   \endgroup}


% \newcounter{table}
% %\renewcommand\thetable{\@arabic\c@table}
\if@book
\renewcommand \thetable
     {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@table}
\else
\def\thetable{\@arabic\c@table}
\fi
\def\fps@table{tb}
\def\ftype@table{2}
\def\ext@table{lot}
\def\fnum@table{\tablename~\thetable}

\renewenvironment{table}
                 { %
                   \setlength\abovecaptionskip{\baselineskip}%
                   \setlength\belowcaptionskip{\baselineskip}%
                   \@float{table}[tb]
                   \if@tfpsymp
                     \normalsize
                   \else
                     \tablesize
                   \fi}
                 {\end@float}
\renewenvironment{table*}
                 {%
                   \setlength\abovecaptionskip{\baselineskip}%
                   \setlength\belowcaptionskip{\baselineskip}%
                   \@dblfloat{table}[tb]
                   \if@tfpsymp
                     \normalsize
                   \else
                     \tablesize
                   \fi}
                 {\end@dblfloat}

%@node Footnotes,  , Tables, Modified macros and environments
%@subsection Footnotes

\renewcommand\@makefntext[1]{%
    %\parindent 1em%
    \noindent
    \raggedright % HWL: no line filling in footnotes
    \hb@xt@1.8em{\hss\@makefnmark}#1}

%@node Misc, Pagestyle etc, Modified macros and environments
%@section Misc

% % from grasp.sty
% \newcommand{\smiley}{%
% \hbox{$\bigcirc\mskip-13.3mu{}^{..}
% \mskip-11mu\scriptscriptstyle\smile\ $}}
% \newcommand{\frowney}{%
% \hbox{$\bigcirc\mskip-13.3mu{}^{..}
% \mskip-11mu\scriptscriptstyle\frown\ $}}


%@node Pagestyle etc,  , Misc
%@section Pagestyle etc

\if@book

\def\chapauthors#1{
    \gdef\@chapauthors{#1}%
    \vspace{-1cm}%
    %% Should add contents line here; currently has to be done inlined in text
    %%\addcontentsline{toc}{}%
    %%                {\protect\numberline{\it #1}}%
    %%\addcontentsline{toc}{}{\@chapauthors}%
    {\large \@chapauthors}%
    }

\def\chapauthorstoc#1{\addtocontents{toc}{\hspace{-0cm}\protect{\it #1}}}

%\def\chapauthorstoc#1{\addtocontents{toc}{\hspace{\bonzowidth}\protect{\it #1}}}

\def\wstitle#1{\gdef\@wstitle{#1}}
\def\chaptitle#1{\gdef\@chaptitle{#1}}

\def\ps@sfpheadings{%
    \let\@oddfoot\@empty\let\@evenfoot\@empty
    \def\@evenhead{\thepage\hfil\@wstitle}%
    %\def\@oddhead{\@chaptitle\hfil\thepage}%
    \def\@oddhead{\@chapapp\ \thechapter. \@chaptitle\hfil\thepage}%
    \let\@mkboth\@gobbletwo
    \let\chaptermark\@gobble
    \let\sectionmark\@gobble
    }

\def\ps@frontheadings{%
    \let\@oddfoot\@empty\let\@evenfoot\@empty
    \def\@evenhead{\thepage\hfil\@wstitle}
    \def\@oddhead{\@chaptitle\hfil\thepage}
    \let\@mkboth\@gobbletwo
    \let\chaptermark\@gobble
    \let\sectionmark\@gobble
    }

\renewcommand\frontmatter{%
%=:-(   \if@openright
%=:-(     \cleardoublepage
%=:-(   \else
%=:-(     \clearpage
%=:-(   \fi
  \@mainmatterfalse
  \pagenumbering{roman}
  \pagestyle{frontheadings}}

\renewcommand\mainmatter{%
%=:-(   \if@openright
%=:-(     \cleardoublepage
%=:-(   \else
%=:-(     \clearpage
%=:-(   \fi
  \@mainmattertrue
  \pagenumbering{arabic}
  \pagestyle{sfpheadings}}

% similar to frontmatter but with arabic page numbers
\renewcommand\backmatter{%
%=:-(   \if@openright
%=:-(     \cleardoublepage
%=:-(   \else
%=:-(     \clearpage
%=:-(   \fi
  \@mainmatterfalse
  \pagenumbering{arabic}
  \pagestyle{frontheadings}}

%% from book.cls:
\renewcommand\part{%
%=:-(   \if@openright
%=:-(     \cleardoublepage
%=:-(   \else
%=:-(     \clearpage
%=:-(   \fi
%=:-(   \thispagestyle{plain}%
  \if@twocolumn
    \onecolumn
    \@tempswatrue
  \else
    \@tempswafalse
  \fi
  \null\vfil
  \secdef\@part\@spart}

\def\@part[#1]#2{%
    \ifnum \c@secnumdepth >-2\relax
      \refstepcounter{part}%
      \addcontentsline{toc}{part}{\thepart\hspace{1em}#1}%
    \else
      \addcontentsline{toc}{part}{#1}%
    \fi
    \markboth{}{}%
    {\centering
     \interlinepenalty \@M
     \normalfont
     \ifnum \c@secnumdepth >-2\relax
       \huge\bfseries \partname~\thepart
       \par
       \vskip 20\p@
     \fi
     \Huge \bfseries #2\par}%
    \@endpart}
\def\@spart#1{%
    {\centering
     \interlinepenalty \@M
     \normalfont
     \Huge \bfseries #1\par}%
    \@endpart}
\def\@endpart{\vfil    %% \newpage -- HWL: no pagebreak with part env
              \if@twoside
                \null
                \thispagestyle{empty}%
                %%\newpage
              \fi
              \if@tempswa
                \twocolumn
              \fi}

\fi  % book

% %   \if@titlepage
% %   \renewcommand\maketitle{\begin{titlepage}%
% %   \let\footnotesize\small
% %   \let\footnoterule\relax
% %   \let \footnote \thanks
% %   \null\vfil
% %   \vskip 60\p@
% %   \begin{center}%
% %     {\LARGE \@title \par}%
% %     \vskip 3em%
% %     {\large
% %      \lineskip .75em%
% %       \begin{tabular}[t]{c}%
% %         \@author
% %       \end{tabular}\par}%
% %       \vskip 1.5em%
% %     {\large \@date \par}%       % Set date in \large size.
% %   \end{center}\par
% %   \@thanks
% %   \vfil\null
% %   \end{titlepage}%
% %   \setcounter{footnote}{0}%
% %   \global\let\thanks\relax
% %   \global\let\maketitle\relax
% %   \global\let\@thanks\@empty
% %   \global\let\@author\@empty
% %   \global\let\@date\@empty
% %   \global\let\@title\@empty
% %   \global\let\title\relax
% %   \global\let\author\relax
% %   \global\let\date\relax
% %   \global\let\and\relax
% % }
% % \else
% % \renewcommand\maketitle{\par
% %   \begingroup
% %     \renewcommand\thefootnote{\@fnsymbol\c@footnote}%
% %     \def\@makefnmark{\rlap{\@textsuperscript{\normalfont\@thefnmark}}}%
% %     \long\def\@makefntext##1{\parindent 1em\noindent
% %             \hb@xt@1.8em{%
% %                 \hss\@textsuperscript{\normalfont\@thefnmark}}##1}%
% %     \if@twocolumn
% %       \ifnum \col@number=\@ne
% %         \@maketitle
% %       \else
% %         \twocolumn[\@maketitle]%
% %       \fi
% %     \else
% %       \newpage
% %       \global\@topnum\z@   % Prevents figures from going at top of page.
% %       \@maketitle
% %     \fi
% %     \thispagestyle{plain}\@thanks
% %   \endgroup
% %   \setcounter{footnote}{0}%
% %   \global\let\thanks\relax
% %   \global\let\maketitle\relax
% %   \global\let\@maketitle\relax
% %   \global\let\@thanks\@empty
% %   \global\let\@author\@empty
% %   \global\let\@date\@empty
% %   \global\let\@title\@empty
% %   \global\let\title\relax
% %   \global\let\author\relax
% %   \global\let\date\relax
% %   \global\let\and\relax
% % }
% % % \def\@maketitle{%
% % %   \newpage
% % %   \null
% % %   \vskip 2em%
% % %   \begin{center}%
% % %   \let \footnote \thanks
% % %     {\LARGE \@title \par}%
% % %     \vskip 1.5em%
% % %     {\large
% % %       \lineskip .5em%
% % %       \begin{tabular}[t]{c}%
% % %         \@author
% % %       \end{tabular}\par}%
% % %     \vskip 1em%
% % %     {\large \@date}%
% % %   \end{center}%
% % %   \par
% % %   \vskip 1.5em}
% % % \fi

\normalsize

% book uses \frontmatter etc to set headings correctly
\if@tfpsymp

\if@pagenumbers

\pagestyle{plain}

% Pagenumbering for tfpsymp papers is: Paper.no.(roman)-Page.no. (e.g. VII-1)
\newcounter{paper}
\def\@FP#1{\@Roman \c@paper--\@arabic #1}
\pagenumbering{FP}

%% NB: The following line has to be inserted at the beginning of each paper!!
%\setcounter{paper}{1009}

\else

\pagestyle{empty}

\fi

\else

\pagestyle{empty}

\fi

%% This is now defined in \mainmatter
% \if@book
% % with the book option we want headings otherwise we don't
% \pagestyle{sfpheadings}
% \else
% \pagestyle{empty}
% \fi
