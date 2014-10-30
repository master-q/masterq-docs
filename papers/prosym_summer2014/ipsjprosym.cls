% ipsjprosym.cls  (C) 2014 Prosym Kanjikai
% Copyright (C) 2014 by Kiminori Matsuzaki

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{ipsjprosym}
[2014/09/07 v1.00 IPSJ class]

%%%%%% Document Styles %%%%%%
\newif\ifDSP@withpage \DSP@withpagefalse
\newif\ifDSP@english \DSP@englishfalse

\DeclareOption{withpage}{\DSP@withpagetrue}
\DeclareOption{english}{\DSP@englishtrue}
\ProcessOptions

%%
%% Option
%%
\LoadClass[submit,techreq,noauthor%
\ifDSP@english ,english\fi%
]{ipsj}

%% 提出用にはヘッダフッタをつけない．
\ifDSP@withpage
\let\ps@IPSJTITLEheadings\ps@plain
\pagestyle{plain}
\else
\let\ps@IPSJTITLEheadings\ps@empty
\pagestyle{empty}
\fi

%% 夏のプログラム・シンポジウム 質疑・応答 のため
\newenvironment{QandA}%
{\subsection*{{\normalsize 質疑・応答}}\begin{description}\def\makelabel##1{##1}}
{\end{description}}

%% 版面のみ A4 -> B5 サイズへ縮小する
\addtolength{\textwidth}{-24mm} % 210mm-182mm=28mmだが
\addtolength{\oddsidemargin}{12mm}
\addtolength{\evensidemargin}{12mm}

\addtolength{\textheight}{-36mm} % 297mm-257mm=40mmだが
\addtolength{\topmargin}{18mm}

\endinput

