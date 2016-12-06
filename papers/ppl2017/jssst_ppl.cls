\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{jssst_ppl}
  [2010/11/19 Kiminori MATSUZAKI]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Options
\newif\if@draft
\DeclareOption{draft}{\@drafttrue}
\DeclareOption{final}{\@draftfalse}

\newif\if@english
\DeclareOption{english}{\@englishtrue}
\DeclareOption{japanese}{\@englishfalse}

\ExecuteOptions{final,japanese}
\ProcessOptions

\LoadClass[a4paper,onecolumn,11pt,twoside%
\if@draft ,draft\else ,final\fi%
]{\if@english article\else jarticle\fi}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% page style
\setlength{\textwidth}{462pt}  %% 11pt(1char) * 42
\setlength{\textheight}{698pt} %% 15.5pt(1line) * 45 + 1
\setlength{\headheight}{0pt}
\setlength{\headsep}{0pt}
\@tempdima24mm\advance\@tempdima-1in
\setlength{\topmargin}{\@tempdima} %% 24mm
\@tempdima\paperwidth\advance\@tempdima-\textwidth\@tempdima.5\@tempdima\advance\@tempdima-1in
\setlength{\oddsidemargin}{\@tempdima} %% center
\setlength{\evensidemargin}{\@tempdima} %% center

\if@draft\pagestyle{plain}\else\pagestyle{empty}\fi
\if@english
\else
\def\@listi{\leftmargin\leftmargini
  \topsep 4\p@ \@plus2\p@ \@minus2\p@
  \parsep 2\p@ \@plus\p@ \@minus\p@
  \itemsep \parsep}%
\let\@listI\@listi
\renewcommand{\normalsize}{%
    \@setfontsize\normalsize\@xipt{15.5}%
  \abovedisplayskip 5\p@ \@plus3\p@ \@minus6\p@
  \abovedisplayshortskip \z@ \@plus3\p@
  \belowdisplayshortskip 3.5\p@ \@plus3.5\p@ \@minus3\p@
   \belowdisplayskip \abovedisplayskip
   \let\@listi\@listI}
\renewcommand{\small}{%
  \@setfontsize\small\@xpt\@xiipt
  \abovedisplayskip 5\p@ \@plus2\p@ \@minus5\p@
  \abovedisplayshortskip \z@ \@plus3\p@
  \belowdisplayshortskip 3\p@ \@plus3\p@ \@minus3\p@
  \let\@listi\@listI
  \belowdisplayskip \abovedisplayskip}
\normalsize
\fi

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Title/Authors/Affiliations

\newcommand{\inst}[1]{\gdef\@date{#1}}

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
%%    \thispagestyle{plain}\@thanks
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
  \newpage\null
  \vskip .5em%
  \begin{center}%
  \let \footnote \thanks
    {\LARGE\bfseries \@title \par}%
    \vskip 1.5em%
    {\large \@author \par}%
    \vskip 1em%
    {\normalsize \@date}%
  \end{center}%
  \par
  \vskip .5em}
\renewenvironment{abstract}%
                 {\list{}{\listparindent \parindent%
                          \itemindent    0pt%\listparindent
                          \leftmargin    4em
                          \rightmargin   \leftmargin
                          \parsep        \z@ \@plus\p@}%
                  \item\relax \small{\bfseries \abstractname \quad}}%
                 {\endlist}
\newcommand{\original}[1]{
  \vfill\noindent\rule{5cm}{0.4pt}
  \small\par{#1}
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Captions of Figures/Tables
\long\def\@makecaption#1#2{{%
  \small
  \vskip\abovecaptionskip
  \advance\leftskip2em%
  \advance\rightskip2em%
  \@tempdimb\hsize\advance\@tempdimb-4em
  \sbox\@tempboxa{\mbox{{\bfseries #1.}~}#2}%
  \ifdim \wd\@tempboxa >\@tempdimb
    \list{\bfseries #1.}{%
      \setbox\tw@=\hbox{\hskip2em{\bfseries #1.}~}%
      \renewcommand{\makelabel}[1]{\hskip2em##1\hfil}
      \itemsep    \z@
      \itemindent \z@
      \labelsep   \z@
      \labelwidth \wd\tw@
      \listparindent\z@
      \leftmargin \wd\tw@
      \rightmargin 2em}\item\relax #2\endlist
  \else
    \global \@minipagefalse
    \hb@xt@\hsize{\hfil\box\@tempboxa\hfil}%
  \fi
  \vskip\belowcaptionskip}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section/Subsection
\if@english
\renewcommand\section{\@startsection {section}{1}{\z@}%
                                   {-2.5ex \@plus -1ex \@minus -.2ex}% %% 3.5 -> 2.5
                                   {1.6ex \@plus.2ex}%                 %% 2.3 -> 1.6
                                   {\normalfont\Large\bfseries}}
\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                     {-2.0ex\@plus -1ex \@minus -.2ex}% %% 3.25 -> 2.0
                                     {1.25ex \@plus .2ex}%               %% 1.5  -> 1.25
                                     {\normalfont\large\bfseries}}
\renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                                     {-2.0ex\@plus -1ex \@minus -.2ex}% %% 3.25 -> 2.0
                                     {1.25ex \@plus .2ex}%                %% 1.5 -> 1.25
                                     {\normalfont\normalsize\bfseries}}
\else
\renewcommand{\section}{\@startsection{section}{1}{\z@}%
   {1.5\Cvs \@plus.5\Cvs \@minus.2\Cvs}% %% 1.5
   {.5\Cvs \@plus.3\Cvs}%                %% .5
   {\reset@font\Large\bfseries}}
\renewcommand{\subsection}{\@startsection{subsection}{2}{\z@}%
   {.75\Cvs \@plus.5\Cvs \@minus.2\Cvs}% %% 1.5 -> .75
   {.25\Cvs \@plus.3\Cvs}%               %% .5 -> .25
   {\reset@font\large\bfseries}}
\renewcommand{\subsubsection}{\@startsection{subsubsection}{3}{\z@}%
   {.75\Cvs \@plus.5\Cvs \@minus.2\Cvs}% %% 1.5 -> .75
   {.25\Cvs \@plus.3\Cvs}%               %% .5 -> .25
   {\reset@font\normalsize\bfseries}}
\fi

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reference (Small)

\renewenvironment{thebibliography}[1]
     {\bgroup\small
      \section*{\refname}%
      \@mkboth{\MakeUppercase\refname}{\MakeUppercase\refname}%
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
      \endlist
      \egroup}

\endinput
%%
%% End of file 'jssst_ppl.cls'

