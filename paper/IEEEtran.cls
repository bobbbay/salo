\ProvidesClass{IEEEtran}[2012/12/27 V1.8 by Michael Shell]
\typeout{-- See the "IEEEtran_HOWTO" manual for usage information.}
\typeout{-- http://www.michaelshell.org/tex/ieeetran/}
\NeedsTeXFormat{LaTeX2e}

\def\IEEEtransversionmajor{1}
\def\IEEEtransversionminor{8}

\newif\if@restonecol
\newif\if@titlepage

\newif\ifCLASSOPTIONonecolumn       \CLASSOPTIONonecolumnfalse
\newif\ifCLASSOPTIONtwocolumn       \CLASSOPTIONtwocolumntrue

\newif\ifCLASSOPTIONoneside         \CLASSOPTIONonesidetrue
\newif\ifCLASSOPTIONtwoside         \CLASSOPTIONtwosidefalse

\newif\ifCLASSOPTIONfinal           \CLASSOPTIONfinaltrue
\newif\ifCLASSOPTIONdraft           \CLASSOPTIONdraftfalse
\newif\ifCLASSOPTIONdraftcls        \CLASSOPTIONdraftclsfalse
\newif\ifCLASSOPTIONdraftclsnofoot  \CLASSOPTIONdraftclsnofootfalse

\newif\ifCLASSOPTIONpeerreview      \CLASSOPTIONpeerreviewfalse
\newif\ifCLASSOPTIONpeerreviewca    \CLASSOPTIONpeerreviewcafalse

\newif\ifCLASSOPTIONjournal         \CLASSOPTIONjournaltrue
\newif\ifCLASSOPTIONconference      \CLASSOPTIONconferencefalse
\newif\ifCLASSOPTIONtechnote        \CLASSOPTIONtechnotefalse

\newif\ifCLASSOPTIONnofonttune      \CLASSOPTIONnofonttunefalse

\newif\ifCLASSOPTIONcaptionsoff     \CLASSOPTIONcaptionsofffalse

\newif\ifCLASSOPTIONcompsoc         \CLASSOPTIONcompsocfalse
\newif\ifCLASSOPTIONtransmag        \CLASSOPTIONtransmagfalse

\newif\ifCLASSOPTIONromanappendices \CLASSOPTIONromanappendicesfalse

\newif\ifCLASSINFOpdf               \CLASSINFOpdffalse

\newif\if@IEEEusingAfourpaper       \@IEEEusingAfourpaperfalse

\newdimen\@IEEEtrantmpdimenA
\newdimen\@IEEEtrantmpdimenB
\newcount\@IEEEtrantmpcountA
\newcount\@IEEEtrantmpcountB
\newtoks\@IEEEtrantmptoksA

\def\@ptsize{0}
\DeclareOption{9pt}{\def\CLASSOPTIONpt{9}\def\@ptsize{0}}
\DeclareOption{10pt}{\def\CLASSOPTIONpt{10}\def\@ptsize{0}}
\DeclareOption{11pt}{\def\CLASSOPTIONpt{11}\def\@ptsize{1}}
\DeclareOption{12pt}{\def\CLASSOPTIONpt{12}\def\@ptsize{2}}

\DeclareOption{letterpaper}{\setlength{\paperheight}{11in}%
                            \setlength{\paperwidth}{8.5in}%
                            \@IEEEusingAfourpaperfalse
                            \def\CLASSOPTIONpaper{letter}%
                            \def\CLASSINFOpaperwidth{8.5in}%
                            \def\CLASSINFOpaperheight{11in}}


\DeclareOption{a4paper}{\setlength{\paperheight}{297mm}%
                        \setlength{\paperwidth}{210mm}%
                        \@IEEEusingAfourpapertrue
                        \def\CLASSOPTIONpaper{a4}%
                        \def\CLASSINFOpaperwidth{210mm}%
                        \def\CLASSINFOpaperheight{297mm}}

\DeclareOption{oneside}{\@twosidefalse\@mparswitchfalse
                        \CLASSOPTIONonesidetrue\CLASSOPTIONtwosidefalse}
\DeclareOption{twoside}{\@twosidetrue\@mparswitchtrue
                        \CLASSOPTIONtwosidetrue\CLASSOPTIONonesidefalse}

\DeclareOption{onecolumn}{\CLASSOPTIONonecolumntrue\CLASSOPTIONtwocolumnfalse}
\DeclareOption{twocolumn}{\CLASSOPTIONtwocolumntrue\CLASSOPTIONonecolumnfalse}

\DeclareOption{draft}{\CLASSOPTIONdrafttrue\CLASSOPTIONdraftclstrue
                      \CLASSOPTIONdraftclsnofootfalse} 
\DeclareOption{draftcls}{\CLASSOPTIONdraftfalse\CLASSOPTIONdraftclstrue
                         \CLASSOPTIONdraftclsnofootfalse} 
\DeclareOption{draftclsnofoot}{\CLASSOPTIONdraftfalse\CLASSOPTIONdraftclstrue
                               \CLASSOPTIONdraftclsnofoottrue} 
\DeclareOption{final}{\CLASSOPTIONdraftfalse\CLASSOPTIONdraftclsfalse
                      \CLASSOPTIONdraftclsnofootfalse}

\DeclareOption{journal}{\CLASSOPTIONpeerreviewfalse\CLASSOPTIONpeerreviewcafalse
                        \CLASSOPTIONjournaltrue\CLASSOPTIONconferencefalse\CLASSOPTIONtechnotefalse}

\DeclareOption{conference}{\CLASSOPTIONpeerreviewfalse\CLASSOPTIONpeerreviewcafalse
                           \CLASSOPTIONjournalfalse\CLASSOPTIONconferencetrue\CLASSOPTIONtechnotefalse}

\DeclareOption{technote}{\CLASSOPTIONpeerreviewfalse\CLASSOPTIONpeerreviewcafalse
                         \CLASSOPTIONjournalfalse\CLASSOPTIONconferencefalse\CLASSOPTIONtechnotetrue}

\DeclareOption{peerreview}{\CLASSOPTIONpeerreviewtrue\CLASSOPTIONpeerreviewcafalse
                           \CLASSOPTIONjournalfalse\CLASSOPTIONconferencefalse\CLASSOPTIONtechnotefalse}

\DeclareOption{peerreviewca}{\CLASSOPTIONpeerreviewtrue\CLASSOPTIONpeerreviewcatrue
                             \CLASSOPTIONjournalfalse\CLASSOPTIONconferencefalse\CLASSOPTIONtechnotefalse}

\DeclareOption{nofonttune}{\CLASSOPTIONnofonttunetrue}

\DeclareOption{captionsoff}{\CLASSOPTIONcaptionsofftrue}

\DeclareOption{compsoc}{\CLASSOPTIONcompsoctrue\CLASSOPTIONtransmagfalse}

\DeclareOption{transmag}{\CLASSOPTIONtransmagtrue\CLASSOPTIONcompsocfalse}

\DeclareOption{romanappendices}{\CLASSOPTIONromanappendicestrue}


\ExecuteOptions{letterpaper,10pt,twocolumn,oneside,final,journal}
\ProcessOptions



\long\def\@IEEEcompsoconly#1{\relax\ifCLASSOPTIONcompsoc\relax#1\relax\fi\relax}
\long\def\@IEEEnotcompsoconly#1{\relax\ifCLASSOPTIONcompsoc\else\relax#1\relax\fi\relax}
\long\def\@IEEEcompsocconfonly#1{\relax\ifCLASSOPTIONcompsoc\ifCLASSOPTIONconference\relax#1\relax\fi\fi\relax}
\long\def\@IEEEcompsocnotconfonly#1{\relax\ifCLASSOPTIONcompsoc\ifCLASSOPTIONconference\else\relax#1\relax\fi\fi\relax}


\renewcommand{\sfdefault}{phv}
\renewcommand{\rmdefault}{ptm}
\renewcommand{\ttdefault}{pcr}

\@IEEEcompsoconly{\typeout{-- Using IEEE Computer Society mode.}}

\@IEEEcompsocnotconfonly{\renewcommand{\rmdefault}{ppl}}

\normalfont\selectfont





\def\@IEEEconsolenoticeconference{\typeout{}%
\typeout{** Conference Paper **}%
\typeout{Before submitting the final camera ready copy, remember to:}%
\typeout{}%
\typeout{ 1. Manually equalize the lengths of two columns on the last page}%
\typeout{ of your paper;}%
\typeout{}%
\typeout{ 2. Ensure that any PostScript and/or PDF output post-processing}%
\typeout{ uses only Type 1 fonts and that every step in the generation}%
\typeout{ process uses the appropriate paper size.}%
\typeout{}}


\AtEndDocument{\ifCLASSOPTIONconference\@IEEEconsolenoticeconference\fi}


\ifCLASSOPTIONtwocolumn\else%
  \ifCLASSOPTIONdraftcls\else%
   \typeout{** ATTENTION: Single column mode is not typically used with IEEE publications.}%
  \fi%
\fi


{\@ifundefined{pdfoutput}{\AtBeginDvi{\special{papersize=\CLASSINFOpaperwidth,\CLASSINFOpaperheight}}}{%
\@ifundefined{pdfpageheight}{\relax}{\global\pdfpagewidth\paperwidth
\global\pdfpageheight\paperheight}%
\ifcase\pdfoutput
\AtBeginDvi{\special{papersize=\CLASSINFOpaperwidth,\CLASSINFOpaperheight}}%
\else
\global\CLASSINFOpdftrue
\fi}}

\typeout{-- Using \CLASSINFOpaperwidth\space x \CLASSINFOpaperheight\space
(\CLASSOPTIONpaper)\space paper.}

\ifCLASSINFOpdf
\typeout{-- Using PDF output.}
\else
\typeout{-- Using DVI output.}
\fi

\def\@IEEEptsizenine{9}
\def\@IEEEptsizeten{10}
\def\@IEEEptsizeeleven{11}
\def\@IEEEptsizetwelve{12}

\newdimen\@IEEEnormalsizeunitybaselineskip
\@IEEEnormalsizeunitybaselineskip\baselineskip

\ifx\CLASSOPTIONpt\@IEEEptsizenine
\typeout{-- This is a 9 point document.}
\def\normalsize{\@setfontsize{\normalsize}{9}{11.0476pt}}%
\setlength{\@IEEEnormalsizeunitybaselineskip}{11.0476pt}%
\normalsize
\abovedisplayskip 1.5ex plus3pt minus1pt%
\belowdisplayskip \abovedisplayskip%
\abovedisplayshortskip 0pt plus3pt%
\belowdisplayshortskip 1.5ex plus3pt minus1pt
\def\small{\@setfontsize{\small}{8.5}{10pt}}
\def\footnotesize{\@setfontsize{\footnotesize}{8}{9pt}}
\def\scriptsize{\@setfontsize{\scriptsize}{7}{8pt}}
\def\tiny{\@setfontsize{\tiny}{5}{6pt}}
\def\sublargesize{\@setfontsize{\sublargesize}{10}{12pt}}
\def\large{\@setfontsize{\large}{10}{12pt}}
\def\Large{\@setfontsize{\Large}{12}{14pt}}
\def\LARGE{\@setfontsize{\LARGE}{14}{17pt}}
\def\huge{\@setfontsize{\huge}{17}{20pt}}
\def\Huge{\@setfontsize{\Huge}{20}{24pt}}
\fi


\ifx\CLASSOPTIONpt\@IEEEptsizeten
\typeout{-- This is a 10 point document.}
\def\normalsize{\@setfontsize{\normalsize}{10}{12.00pt}}%
\setlength{\@IEEEnormalsizeunitybaselineskip}{12pt}%
\normalsize
\abovedisplayskip 1.5ex plus4pt minus2pt%
\belowdisplayskip \abovedisplayskip%
\abovedisplayshortskip 0pt plus4pt%
\belowdisplayshortskip 1.5ex plus4pt minus2pt
\def\small{\@setfontsize{\small}{9}{10pt}}
\def\footnotesize{\@setfontsize{\footnotesize}{8}{9pt}}
\def\scriptsize{\@setfontsize{\scriptsize}{7}{8pt}}
\def\tiny{\@setfontsize{\tiny}{5}{6pt}}
\def\sublargesize{\@setfontsize{\sublargesize}{11}{13.4pt}}
\def\large{\@setfontsize{\large}{12}{14pt}}
\def\Large{\@setfontsize{\Large}{14}{17pt}}
\def\LARGE{\@setfontsize{\LARGE}{17}{20pt}}
\def\huge{\@setfontsize{\huge}{20}{24pt}}
\def\Huge{\@setfontsize{\Huge}{24}{28pt}}
\fi


\ifx\CLASSOPTIONpt\@IEEEptsizeeleven
\typeout{-- This is an 11 point document.}
\def\normalsize{\@setfontsize{\normalsize}{11}{13.3846pt}}%
\setlength{\@IEEEnormalsizeunitybaselineskip}{13.3846pt}%
\normalsize
\abovedisplayskip 1.5ex plus5pt minus3pt%
\belowdisplayskip \abovedisplayskip%
\abovedisplayshortskip 0pt plus5pt%
\belowdisplayshortskip 1.5ex plus5pt minus3pt
\def\small{\@setfontsize{\small}{10}{12pt}}
\def\footnotesize{\@setfontsize{\footnotesize}{9}{10.5pt}}
\def\scriptsize{\@setfontsize{\scriptsize}{8}{9pt}}
\def\tiny{\@setfontsize{\tiny}{6}{7pt}}
\def\sublargesize{\@setfontsize{\sublargesize}{12}{14pt}}
\def\large{\@setfontsize{\large}{12}{14pt}}
\def\Large{\@setfontsize{\Large}{14}{17pt}}
\def\LARGE{\@setfontsize{\LARGE}{17}{20pt}}
\def\huge{\@setfontsize{\huge}{20}{24pt}}
\def\Huge{\@setfontsize{\Huge}{24}{28pt}}
\fi


\ifx\CLASSOPTIONpt\@IEEEptsizetwelve
\typeout{-- This is a 12 point document.}
\def\normalsize{\@setfontsize{\normalsize}{12}{13.92pt}}%
\setlength{\@IEEEnormalsizeunitybaselineskip}{13.92pt}%
\normalsize
\abovedisplayskip 1.5ex plus6pt minus4pt%
\belowdisplayskip \abovedisplayskip%
\abovedisplayshortskip 0pt plus6pt%
\belowdisplayshortskip 1.5ex plus6pt minus4pt
\def\small{\@setfontsize{\small}{10}{12pt}}
\def\footnotesize{\@setfontsize{\footnotesize}{9}{10.5pt}}
\def\scriptsize{\@setfontsize{\scriptsize}{8}{9pt}}
\def\tiny{\@setfontsize{\tiny}{6}{7pt}}
\def\sublargesize{\@setfontsize{\sublargesize}{14}{17pt}}
\def\large{\@setfontsize{\large}{14}{17pt}}
\def\Large{\@setfontsize{\Large}{17}{20pt}}
\def\LARGE{\@setfontsize{\LARGE}{20}{24pt}}
\def\huge{\@setfontsize{\huge}{22}{26pt}}
\def\Huge{\@setfontsize{\Huge}{24}{28pt}}
\fi


\def\fontsubfuzz{.9pt}


\ifCLASSOPTIONtechnote%
 \ifx\CLASSOPTIONpt\@IEEEptsizenine\else%
  \typeout{** ATTENTION: Technotes are normally 9pt documents.}%
 \fi%
\fi


\def\@IEEEstringptm{ptm} % Times Roman family
\def\@IEEEstringppl{ppl} % Palatino Roman family
\def\@IEEEstringphv{phv} % Helvetica Sans Serif family
\def\@IEEEstringpcr{pcr} % Courier typewriter family
\def\@IEEEstringcmtt{cmtt} % Computer Modern typewriter family
\DeclareTextCommandDefault{\textunderscore}{\leavevmode
\ifx\f@family\@IEEEstringpcr\string_\else
\ifx\f@family\@IEEEstringcmtt\string_\else
\ifx\f@family\@IEEEstringptm\kern 0em\vbox{\hrule\@width 0.5em\@height 0.5pt\kern -0.3ex}\else
\ifx\f@family\@IEEEstringppl\kern 0em\vbox{\hrule\@width 0.5em\@height 0.5pt\kern -0.3ex}\else
\ifx\f@family\@IEEEstringphv\kern -0.03em\vbox{\hrule\@width 0.62em\@height 0.52pt\kern -0.33ex}\kern -0.03em\else
\kern 0.09em\vbox{\hrule\@width 0.6em\@height 0.44pt\kern -0.63pt\kern -0.42ex}\kern 0.09em\fi\fi\fi\fi\fi\relax}




\def\baselinestretch{1}
\ifCLASSOPTIONdraftcls
  \def\baselinestretch{1.5}% default baselinestretch for draft modes
\fi 


\ifx\CLASSINPUTbaselinestretch\@IEEEundefined
\else
  \edef\baselinestretch{\CLASSINPUTbaselinestretch} % user CLASSINPUT override
  \typeout{** ATTENTION: Overriding \string\baselinestretch\space to
           \baselinestretch\space via \string\CLASSINPUT.}
\fi

\normalsize % make \baselinestretch take affect




\newdimen\CLASSINFOnormalsizebaselineskip
\CLASSINFOnormalsizebaselineskip=\baselineskip\relax
\newdimen\CLASSINFOnormalsizeunitybaselineskip
\CLASSINFOnormalsizeunitybaselineskip=\@IEEEnormalsizeunitybaselineskip\relax
\newdimen\IEEEnormaljot
\IEEEnormaljot=0.25\baselineskip\relax

\jot=\IEEEnormaljot\relax

\def\@IEEEinterspaceratioM{0.35}
\def\@IEEEinterspaceMINratioM{0.23}
\def\@IEEEinterspaceMAXratioM{0.50}

\def\@IEEEinterspaceratioB{0.375}
\def\@IEEEinterspaceMINratioB{0.23}
\def\@IEEEinterspaceMAXratioB{0.55}


\def\@@@IEEEsetfontdimens#1#2#3{{%
\setlength{\@IEEEtrantmpdimenB}{\f@size pt}% grab the font size in pt, could use 1em instead.
\setlength{\@IEEEtrantmpdimenA}{#1\@IEEEtrantmpdimenB}%
\fontdimen2\font=\@IEEEtrantmpdimenA\relax
\addtolength{\@IEEEtrantmpdimenA}{-#2\@IEEEtrantmpdimenB}%
\fontdimen3\font=-\@IEEEtrantmpdimenA\relax
\setlength{\@IEEEtrantmpdimenA}{#1\@IEEEtrantmpdimenB}%
\addtolength{\@IEEEtrantmpdimenA}{-#3\@IEEEtrantmpdimenB}%
\fontdimen4\font=\@IEEEtrantmpdimenA\relax}}

\def\@@IEEEsetfontdimens{{%
\mdseries
\@@@IEEEsetfontdimens{\@IEEEinterspaceratioM}{\@IEEEinterspaceMAXratioM}{\@IEEEinterspaceMINratioM}%
\bfseries
\@@@IEEEsetfontdimens{\@IEEEinterspaceratioB}{\@IEEEinterspaceMAXratioB}{\@IEEEinterspaceMINratioB}%
}}

\def\@IEEEsetfontdimens{{%
\normalfont
\@@IEEEsetfontdimens
\normalfont\itshape
\@@IEEEsetfontdimens
}}

\def\@IEEEtunefonts{{\selectfont\rmfamily
\tiny\@IEEEsetfontdimens
\scriptsize\@IEEEsetfontdimens
\footnotesize\@IEEEsetfontdimens
\small\@IEEEsetfontdimens
\normalsize\@IEEEsetfontdimens
\sublargesize\@IEEEsetfontdimens
\large\@IEEEsetfontdimens
\LARGE\@IEEEsetfontdimens
\huge\@IEEEsetfontdimens
\Huge\@IEEEsetfontdimens}}

\ifCLASSOPTIONnofonttune\else
\@IEEEtunefonts
\fi

\AtBeginDocument{\ifCLASSOPTIONnofonttune\else\@IEEEtunefonts\fi}



\hyphenpenalty=750
\hbadness=1350
\frenchspacing

\binoppenalty=1000 % default 700
\relpenalty=800     % default 500


\marginparsep      10pt
\marginparwidth    20pt
\marginparpush     25pt


\lineskip            0pt
\normallineskip      0pt
\lineskiplimit       0pt
\normallineskiplimit 0pt

\footskip 0.4in

\parskip 0ex plus 0.2ex minus 0.1ex

\parindent    1.0em

\topmargin    -49.0pt
\headheight   12pt
\headsep      0.25in

\topskip=\@IEEEnormalsizeunitybaselineskip
\maxdepth=0.5\@IEEEnormalsizeunitybaselineskip
\textheight       58pc  % 9.63in, 696pt
\ifx\CLASSOPTIONpt\@IEEEptsizenine\textheight=63\@IEEEnormalsizeunitybaselineskip\fi      % 63 lines/page
\ifx\CLASSOPTIONpt\@IEEEptsizeten\textheight=58\@IEEEnormalsizeunitybaselineskip\fi       % 58 lines/page
\ifx\CLASSOPTIONpt\@IEEEptsizeeleven\textheight=52\@IEEEnormalsizeunitybaselineskip\fi    % 52 lines/page
\ifx\CLASSOPTIONpt\@IEEEptsizetwelve\textheight=50\@IEEEnormalsizeunitybaselineskip\fi    % 50 lines/page


\columnsep         1pc
\textwidth        43pc   % 2 x 21pc + 1pc = 43pc
 

\if@IEEEusingAfourpaper 
\oddsidemargin        14.32mm
\evensidemargin       14.32mm
\else
\oddsidemargin        0.680in
\evensidemargin       0.680in
\fi
\addtolength{\oddsidemargin}{-1in}
\addtolength{\evensidemargin}{-1in}



\ifCLASSOPTIONconference
 \topmargin        -0.25in
 \addtolength{\topmargin}{-\headheight}
 \addtolength{\topmargin}{-\headsep}
 \textheight        9.25in % The standard for conferences (668.4975pt)
 \ifx\CLASSOPTIONpt\@IEEEptsizenine\textheight=61\@IEEEnormalsizeunitybaselineskip\fi      % 61 lines/page
 \ifx\CLASSOPTIONpt\@IEEEptsizeten\textheight=56\@IEEEnormalsizeunitybaselineskip\fi       % 56 lines/page
 \ifx\CLASSOPTIONpt\@IEEEptsizeeleven\textheight=50\@IEEEnormalsizeunitybaselineskip\fi    % 50 lines/page
 \ifx\CLASSOPTIONpt\@IEEEptsizetwelve\textheight=48\@IEEEnormalsizeunitybaselineskip\fi    % 48 lines/page
\fi


\ifCLASSOPTIONcompsoc
\ifCLASSOPTIONconference
 \columnsep 0.375in
 \topmargin        0in
 \addtolength{\topmargin}{-6pt}% we tweak this a tad to better comply with top of line stuff
 \addtolength{\topmargin}{-\headheight}
 \addtolength{\topmargin}{-\headsep}
 \textheight        8.875in % (641.39625pt)
 \ifx\CLASSOPTIONpt\@IEEEptsizenine\textheight=58\@IEEEnormalsizeunitybaselineskip\fi      % 58 lines/page
 \ifx\CLASSOPTIONpt\@IEEEptsizeten\textheight=53\@IEEEnormalsizeunitybaselineskip\fi       % 53 lines/page
 \ifx\CLASSOPTIONpt\@IEEEptsizeeleven\textheight=48\@IEEEnormalsizeunitybaselineskip\fi    % 48 lines/page
 \ifx\CLASSOPTIONpt\@IEEEptsizetwelve\textheight=46\@IEEEnormalsizeunitybaselineskip\fi    % 46 lines/page 
 \textwidth 6.5in
 \if@IEEEusingAfourpaper 
  \oddsidemargin        22.45mm
  \evensidemargin       22.45mm
 \else
  \oddsidemargin        1in
  \evensidemargin       1in
 \fi
 \addtolength{\oddsidemargin}{-1in}
 \addtolength{\evensidemargin}{-1in}
\fi\fi



\ifCLASSOPTIONdraftcls 
  \setlength{\topmargin}{-\headsep}%
  \addtolength{\topmargin}{-\headheight}%
  \oddsidemargin      0in
  \evensidemargin     0in
  \setlength{\textwidth}{\paperwidth}%
  \addtolength{\textwidth}{-2.0in}%
  \setlength{\textheight}{\paperheight}%
  \addtolength{\textheight}{-2.0in}%
  \addtolength{\textheight}{-1\topskip}%
  \divide\textheight  by \baselineskip%
  \multiply\textheight  by \baselineskip%
  \addtolength{\textheight}{\topskip}%
\fi



\ifx\CLASSINPUTinnersidemargin\@IEEEundefined
\else
  \ifx\CLASSINPUToutersidemargin\@IEEEundefined
    \edef\CLASSINPUToutersidemargin{\CLASSINPUTinnersidemargin}
  \fi
\fi

\ifx\CLASSINPUToutersidemargin\@IEEEundefined
\else
  \ifx\CLASSINPUTinnersidemargin\@IEEEundefined
    \edef\CLASSINPUTinnersidemargin{\CLASSINPUToutersidemargin}
  \fi
  \setlength{\oddsidemargin}{\CLASSINPUTinnersidemargin}
  \ifCLASSOPTIONtwoside
    \setlength{\evensidemargin}{\CLASSINPUToutersidemargin}
  \else
    \setlength{\evensidemargin}{\CLASSINPUTinnersidemargin}
  \fi
  \addtolength{\oddsidemargin}{-1in}
  \addtolength{\evensidemargin}{-1in}
  \setlength{\textwidth}{\paperwidth}
  \addtolength{\textwidth}{-\CLASSINPUTinnersidemargin}
  \addtolength{\textwidth}{-\CLASSINPUToutersidemargin}
  \typeout{** ATTENTION: Overriding inner side margin to \CLASSINPUTinnersidemargin\space and 
           outer side margin to \CLASSINPUToutersidemargin\space via \string\CLASSINPUT.}
\fi



\ifx\CLASSINPUTtoptextmargin\@IEEEundefined
\else
  \ifx\CLASSINPUTbottomtextmargin\@IEEEundefined
    \edef\CLASSINPUTbottomtextmargin{\CLASSINPUTtoptextmargin}
  \fi
\fi

\ifx\CLASSINPUTbottomtextmargin\@IEEEundefined
\else
  \ifx\CLASSINPUTtoptextmargin\@IEEEundefined
    \edef\CLASSINPUTtoptextmargin{\CLASSINPUTbottomtextmargin}
  \fi
  \setlength{\topmargin}{\CLASSINPUTtoptextmargin}
  \addtolength{\topmargin}{-1in}
  \addtolength{\topmargin}{-\headheight}
  \addtolength{\topmargin}{-\headsep}
  \setlength{\textheight}{\paperheight}
  \addtolength{\textheight}{-\CLASSINPUTtoptextmargin}
  \addtolength{\textheight}{-\CLASSINPUTbottomtextmargin}
  \addtolength{\topmargin}{-0.3\@IEEEnormalsizeunitybaselineskip}
  \typeout{** ATTENTION: Overriding top text margin to \CLASSINPUTtoptextmargin\space and 
           bottom text margin to \CLASSINPUTbottomtextmargin\space via \string\CLASSINPUT.}
\fi








\topsep           0.5\baselineskip

\partopsep          \z@

\parsep             \z@

\itemsep            \z@

\itemindent         -1em

\leftmargin         2em

\leftmargini        2em
\leftmarginii       1em
\leftmarginiii    1.5em
\leftmarginiv     1.5em
\leftmarginv      1.0em
\leftmarginvi     1.0em
\labelsep         0.5em 
\labelwidth         \z@


\def\@listi{\leftmargin\leftmargini \topsep 2pt plus 1pt minus 1pt}
\let\@listI\@listi
\def\@listii{\leftmargin\leftmarginii\labelwidth\leftmarginii%
    \advance\labelwidth-\labelsep \topsep 2pt}
\def\@listiii{\leftmargin\leftmarginiii\labelwidth\leftmarginiii%
    \advance\labelwidth-\labelsep \topsep 2pt}
\def\@listiv{\leftmargin\leftmarginiv\labelwidth\leftmarginiv%
    \advance\labelwidth-\labelsep \topsep 2pt}
\def\@listv{\leftmargin\leftmarginv\labelwidth\leftmarginv%
    \advance\labelwidth-\labelsep \topsep 2pt}
\def\@listvi{\leftmargin\leftmarginvi\labelwidth\leftmarginvi%
    \advance\labelwidth-\labelsep \topsep 2pt}


\def\labelenumi{\theenumi)}     \def\theenumi{\arabic{enumi}}

\def\labelenumii{\theenumii)}  \def\theenumii{\alph{enumii}}

\def\labelenumiii{\theenumiii)} \def\theenumiii{\roman{enumiii}}

\def\labelenumiv{\theenumiv)}   \def\theenumiv{\Alph{enumiv}}

\def\p@enumii{\theenumi}
\def\p@enumiii{\theenumi(\theenumii)}
\def\p@enumiv{\p@enumiii\theenumiii}

\def\labelitemi{$\scriptstyle\bullet$}
\def\labelitemii{\textbf{--}}
\def\labelitemiii{$\ast$}
\def\labelitemiv{$\cdot$}



\newdimen\IEEEilabelindentA
\IEEEilabelindentA \parindent

\newdimen\IEEEilabelindentB
\IEEEilabelindentB 1.3\parindent
\newdimen\IEEEilabelindent
\IEEEilabelindent \IEEEilabelindentA


\newdimen\IEEEelabelindent
\IEEEelabelindent \parindent

\newdimen\IEEEdlabelindent
\IEEEdlabelindent \parindent

\newdimen\IEEElabelindent
\IEEElabelindent \parindent

\def\IEEElabelindentfactori{1.0}   % almost always one
\def\IEEElabelindentfactorii{0.75} % 0.0 or 1.0 may be used in some cases
\def\IEEElabelindentfactoriii{0.0} % 0.75? 0.5? 0.0?
\def\IEEElabelindentfactoriv{0.0}
\def\IEEElabelindentfactorv{0.0}
\def\IEEElabelindentfactorvi{0.0}

\def\IEEElabelindentfactor{1.0}

\newdimen\IEEEiednormlabelsep
\IEEEiednormlabelsep 0.6em

\newdimen\IEEEiedmathlabelsep
\IEEEiedmathlabelsep 1.2em

\newskip\IEEEiedtopsep
\IEEEiedtopsep 2pt plus 1pt minus 1pt


\def\IEEEiedlistdecl{\relax}

\def\IEEEcalcleftmargin#1{\setlength{\leftmargin}{#1}%
\addtolength{\leftmargin}{\labelwidth}%
\addtolength{\leftmargin}{\labelsep}}

\def\IEEEsetlabelwidth#1{\settowidth{\labelwidth}{#1}}

\def\IEEEusemathlabelsep{\setlength{\labelsep}{\IEEEiedmathlabelsep}}

\newif\ifIEEEnocalcleftmargin
\IEEEnocalcleftmarginfalse

\newif\ifIEEEnolabelindentfactor
\IEEEnolabelindentfactorfalse


\def\@IEEEiedjustify{0}


\def\IEEEiedlabeljustifyl{\def\@IEEEiedjustify{0}}%left
\def\IEEEiedlabeljustifyc{\def\@IEEEiedjustify{1}}%center
\def\IEEEiedlabeljustifyr{\def\@IEEEiedjustify{2}}%right




\def\@IEEEsavelistparams{\edef\@IEEEiedtopsep{\the\topsep}%
\edef\@IEEEiedlabelwidth{\the\labelwidth}%
\edef\@IEEEiedlabelsep{\the\labelsep}%
\edef\@IEEEiedleftmargin{\the\leftmargin}%
\edef\@IEEEiedpartopsep{\the\partopsep}%
\edef\@IEEEiedparsep{\the\parsep}%
\edef\@IEEEieditemsep{\the\itemsep}%
\edef\@IEEEiedrightmargin{\the\rightmargin}%
\edef\@IEEEiedlistparindent{\the\listparindent}%
\edef\@IEEEieditemindent{\the\itemindent}}

\def\@IEEErestorelistparams{\topsep\@IEEEiedtopsep\relax%
\labelwidth\@IEEEiedlabelwidth\relax%
\labelsep\@IEEEiedlabelsep\relax%
\leftmargin\@IEEEiedleftmargin\relax%
\partopsep\@IEEEiedpartopsep\relax%
\parsep\@IEEEiedparsep\relax%
\itemsep\@IEEEieditemsep\relax%
\rightmargin\@IEEEiedrightmargin\relax%
\listparindent\@IEEEiedlistparindent\relax%
\itemindent\@IEEEieditemindent\relax}


\let\LaTeXitemize\itemize
\let\endLaTeXitemize\enditemize
\let\LaTeXenumerate\enumerate
\let\endLaTeXenumerate\endenumerate

\newenvironment{LaTeXdescription}
               {\list{}{\labelwidth\z@ \itemindent-\leftmargin
                        \let\makelabel\descriptionlabel}}
               {\endlist}
\newcommand*\descriptionlabel[1]{\hspace\labelsep
                                 \normalfont\bfseries #1}


\def\itemize{\@IEEEitemize}
\def\enditemize{\@endIEEEitemize}
\def\enumerate{\@IEEEenumerate}
\def\endenumerate{\@endIEEEenumerate}
\def\description{\@IEEEdescription}
\def\enddescription{\@endIEEEdescription}

\def\IEEEitemize{\@IEEEitemize}
\def\endIEEEitemize{\@endIEEEitemize}
\def\IEEEenumerate{\@IEEEenumerate}
\def\endIEEEenumerate{\@endIEEEenumerate}
\def\IEEEdescription{\@IEEEdescription}
\def\endIEEEdescription{\@endIEEEdescription}


\def\@IEEEitemize{\@ifnextchar[{\@@IEEEitemize}{\@@IEEEitemize[\relax]}}
\def\@IEEEenumerate{\@ifnextchar[{\@@IEEEenumerate}{\@@IEEEenumerate[\relax]}}
\def\@IEEEdescription{\@ifnextchar[{\@@IEEEdescription}{\@@IEEEdescription[\relax]}}
\def\@endIEEEitemize{\endlist}
\def\@endIEEEenumerate{\endlist}
\def\@endIEEEdescription{\endlist}


\def\@@IEEEitemize[#1]{%
                \ifnum\@itemdepth>3\relax\@toodeep\else%
                \ifnum\@listdepth>5\relax\@toodeep\else%
                \advance\@itemdepth\@ne%
                \edef\@itemitem{labelitem\romannumeral\the\@itemdepth}%
                \advance\@listdepth\@ne% we need to know what the level WILL be
                \edef\IEEElabelindentfactor{\csname IEEElabelindentfactor\romannumeral\the\@listdepth\endcsname}%
                \advance\@listdepth-\@ne% undo our increment
                \def\@IEEEiedjustify{2}% right justified labels are default
                \IEEEnocalcleftmarginfalse%
                \IEEEnolabelindentfactorfalse%
                \topsep\IEEEiedtopsep%
                \IEEElabelindent\IEEEilabelindent%
                \labelsep\IEEEiednormlabelsep%
                \partopsep 0ex%
                \parsep 0ex%
                \itemsep 0ex%
                \rightmargin 0em%
                \listparindent 0em%
                \itemindent 0em%
                \settowidth{\labelwidth}{\csname labelitem\romannumeral\the\@itemdepth\endcsname}%
                \@IEEEsavelistparams% save our list parameters
                \list{\csname\@itemitem\endcsname}{%
                \@IEEErestorelistparams% override any list{} changes
                \let\makelabel\@IEEEiedmakelabel% v1.6b setup \makelabel
                \IEEEiedlistdecl% let user alter parameters
                #1\relax%
                \ifIEEEnolabelindentfactor\relax%
                \else\IEEElabelindent=\IEEElabelindentfactor\IEEElabelindent%
                \fi%
                \ifIEEEnocalcleftmargin\relax%
                \else\IEEEcalcleftmargin{\IEEElabelindent}%
                \fi}\fi\fi}%


\def\@@IEEEenumerate[#1]{%
                \ifnum\@enumdepth>3\relax\@toodeep\else%
                \ifnum\@listdepth>5\relax\@toodeep\else%
                \advance\@enumdepth\@ne%
                \edef\@enumctr{enum\romannumeral\the\@enumdepth}%
                \advance\@listdepth\@ne% we need to know what the level WILL be
                \edef\IEEElabelindentfactor{\csname IEEElabelindentfactor\romannumeral\the\@listdepth\endcsname}%
                \advance\@listdepth-\@ne% undo our increment
                \def\@IEEEiedjustify{2}% right justified labels are default
                \IEEEnocalcleftmarginfalse%
                \IEEEnolabelindentfactorfalse%
                \topsep\IEEEiedtopsep%
                \IEEElabelindent\IEEEelabelindent%
                \labelsep\IEEEiednormlabelsep%
                \partopsep 0ex%
                \parsep 0ex%
                \itemsep 0ex%
                \rightmargin 0em%
                \listparindent 0em%
                \itemindent 0em%
                \settowidth{\labelwidth}{9)}%
                \@IEEEsavelistparams% save our list parameters
                \list{\csname label\@enumctr\endcsname}{\usecounter{\@enumctr}%
                \@IEEErestorelistparams% override any list{} changes
                \let\makelabel\@IEEEiedmakelabel% v1.6b setup \makelabel
                \IEEEiedlistdecl% let user alter parameters 
                #1\relax%
                \ifIEEEnolabelindentfactor\relax%
                \else\IEEElabelindent=\IEEElabelindentfactor\IEEElabelindent%
                \fi%
                \ifIEEEnocalcleftmargin\relax%
                \else\IEEEcalcleftmargin{\IEEElabelindent}%
                \fi}\fi\fi}%


\def\@@IEEEdescription[#1]{%
                \ifnum\@listdepth>5\relax\@toodeep\else%
                \advance\@listdepth\@ne% we need to know what the level WILL be
                \edef\IEEElabelindentfactor{\csname IEEElabelindentfactor\romannumeral\the\@listdepth\endcsname}%
                \advance\@listdepth-\@ne% undo our increment
                \def\@IEEEiedjustify{0}% left justified labels are default
                \IEEEnocalcleftmarginfalse%
                \IEEEnolabelindentfactorfalse%
                \topsep\IEEEiedtopsep% 
                \IEEElabelindent\IEEEdlabelindent%
                \labelsep\IEEEiednormlabelsep%
                \partopsep 0ex%
                \parsep 0ex%
                \itemsep 0ex%
                \rightmargin 0em%
                \listparindent 0em%
                \itemindent 0em%
                \settowidth{\labelwidth}{Hello}%
                \@IEEEsavelistparams% save our list parameters
                \list{}{\@IEEErestorelistparams% override any list{} changes
                \let\makelabel\@IEEEiedmakelabel% v1.6b setup \makelabel
                \IEEEiedlistdecl% let user alter parameters 
                #1\relax%
                \ifIEEEnolabelindentfactor\relax%
                \else\IEEElabelindent=\IEEElabelindentfactor\IEEElabelindent%
                \fi%
                \ifIEEEnocalcleftmargin\relax%
                \else\IEEEcalcleftmargin{\IEEElabelindent}\relax%
                \fi}\fi}

\def\@IEEEiedmakelabel#1{\relax\if\@IEEEiedjustify 0\relax
\makebox[\labelwidth][l]{\normalfont #1}\else
\if\@IEEEiedjustify 1\relax
\makebox[\labelwidth][c]{\normalfont #1}\else
\makebox[\labelwidth][r]{\normalfont #1}\fi\fi}


\newenvironment{verse}{\let\\=\@centercr
    \list{}{\itemsep\z@ \itemindent -1.5em \listparindent \itemindent
    \rightmargin\leftmargin\advance\leftmargin 1.5em}\item\relax}
    {\endlist}
\newenvironment{quotation}{\list{}{\listparindent 1.5em \itemindent\listparindent
    \rightmargin\leftmargin \parsep 0pt plus 1pt}\item\relax}
    {\endlist}
\newenvironment{quote}{\list{}{\rightmargin\leftmargin}\item\relax}
    {\endlist}


\newif\if@restonecol
\def\titlepage{\@restonecolfalse\if@twocolumn\@restonecoltrue\onecolumn
    \else \newpage \fi \thispagestyle{empty}\c@page\z@}
\def\endtitlepage{\if@restonecol\twocolumn \else \newpage \fi}

\arraycolsep     5pt
\arrayrulewidth .4pt
\doublerulesep   2pt

\tabcolsep       6pt
\tabbingsep      0.5em


\skip\footins 0.9\baselineskip  plus 0.4\baselineskip  minus 0.2\baselineskip

{\footnotesize
\global\footnotesep 0.8\baselineskip}


\skip\@mpfootins = \skip\footins
\fboxsep = 3pt
\fboxrule = .4pt
\long\def\@makefntext#1{\parindent 1em\indent\hbox{\@makefnmark}#1}% V1.6 use 1em
\ifCLASSOPTIONcompsoc
\def\@IEEEcompsocmakefnmark{\hbox{\normalfont\@thefnmark.\ }}
\long\def\@makefntext#1{\parindent 1em\indent\hbox{\@IEEEcompsocmakefnmark}#1}
\fi

\def\footnoterule{}

\newif\if@IEEEenableoneshotfootnoterule
\@IEEEenableoneshotfootnoterulefalse
\ifCLASSOPTIONcompsoc
\def\footnoterule{\relax\if@IEEEenableoneshotfootnoterule
\kern-5pt
\hbox to \columnwidth{\hfill\vrule width 0.5\columnwidth height 0.4pt\hfill}
\kern4.6pt
\global\@IEEEenableoneshotfootnoterulefalse
\else
\relax
\fi}
\fi

\interfootnotelinepenalty=10000

\interdisplaylinepenalty=2500

\setcounter{secnumdepth}{4}

\ifCLASSOPTIONtechnote
   \setcounter{secnumdepth}{3}
\fi
\@IEEEcompsocconfonly{\setcounter{secnumdepth}{3}}


\newcounter{section}
\newcounter{subsection}[section]
\newcounter{subsubsection}[subsection]
\newcounter{paragraph}[subsubsection]

\newcounter{IEEEsubequation}[equation]

\def\theequation{\arabic{equation}}                          % 1
\def\theIEEEsubequation{\theequation\alph{IEEEsubequation}}  % 1a (used only by IEEEtran's IEEEeqnarray)
\ifCLASSOPTIONcompsoc
\def\thesection{\arabic{section}}                
\def\thesubsection{\thesection.\arabic{subsection}}
\def\thesubsubsection{\thesubsection.\arabic{subsubsection}}
\def\theparagraph{\thesubsubsection.\arabic{paragraph}}
\else
\def\thesection{\Roman{section}}                             % I
\def\thesubsection{\mbox{\thesection-\Alph{subsection}}}     % I-A
\def\thesubsubsection{\thesubsection\arabic{subsubsection}}  % I-A1
\def\theparagraph{\thesubsubsection\alph{paragraph}}         % I-A1a
\fi

\@ifundefined{pdfstringdefPreHook}{\let\pdfstringdefPreHook\@empty}{}%
\g@addto@macro\pdfstringdefPreHook{\let\mbox\relax}


\ifCLASSOPTIONcompsoc
  \ifCLASSOPTIONconference% compsoc conference
    \def\thesectiondis{\thesection.}
    \def\thesubsectiondis{\thesectiondis\arabic{subsection}.}
    \def\thesubsubsectiondis{\thesubsectiondis\arabic{subsubsection}.}
    \def\theparagraphdis{\thesubsubsectiondis\arabic{paragraph}.}
  \else% compsoc not conferencs
    \def\thesectiondis{\thesection}
    \def\thesubsectiondis{\thesectiondis.\arabic{subsection}}
    \def\thesubsubsectiondis{\thesubsectiondis.\arabic{subsubsection}}
    \def\theparagraphdis{\thesubsubsectiondis.\arabic{paragraph}}
  \fi
\else% not compsoc
  \def\thesectiondis{\thesection.}                   % I.
  \def\thesubsectiondis{\Alph{subsection}.}          % B.
  \def\thesubsubsectiondis{\arabic{subsubsection})}  % 3)
  \def\theparagraphdis{\alph{paragraph})}            % d)
\fi

\def\theequationdis{{\normalfont \normalcolor (\theequation)}}% (1)
\def\theIEEEsubequationdis{{\normalfont \normalcolor (\theIEEEsubequation)}}% (1a)
\def\@eqnnum{\theequationdis}



\def\contentsname{Contents}
\def\listfigurename{List of Figures}
\def\listtablename{List of Tables}
\def\refname{References}
\def\indexname{Index}
\def\figurename{Fig.}
\def\tablename{TABLE}
\@IEEEcompsocconfonly{\def\figurename{Figure}\def\tablename{Table}}
\def\partname{Part}
\def\appendixname{Appendix}
\def\abstractname{Abstract}
\def\IEEEkeywordsname{Index Terms}
\def\IEEEproofname{Proof}


\def\@pnumwidth{1.55em}
\def\@tocrmarg{2.55em}
\def\@dotsep{4.5}
\setcounter{tocdepth}{3}

\def\tableofcontents{\section*{\contentsname}\@starttoc{toc}}
\def\l@section#1#2{\addpenalty{\@secpenalty}\addvspace{1.0em plus 1pt}%
    \@tempdima 2.75em \begingroup \parindent \z@ \rightskip \@pnumwidth%
    \parfillskip-\@pnumwidth {\bfseries\leavevmode #1}\hfil\hbox to\@pnumwidth{\hss #2}\par%
    \endgroup}
\def\l@subsection{\@dottedtocline{2}{2.75em}{3.75em}}
\def\l@subsubsection{\@dottedtocline{3}{6.5em}{4.5em}}
\def\l@paragraph{\@dottedtocline{4}{6.5em}{5.5em}}
\def\l@subparagraph{\@dottedtocline{5}{6.5em}{6.5em}}
\def\listoffigures{\section*{\listfigurename}\@starttoc{lof}}
\def\l@figure{\@dottedtocline{1}{0em}{2.75em}}
\def\listoftables{\section*{\listtablename}\@starttoc{lot}}
\let\l@table\l@figure


\floatsep 0.85\baselineskip plus  0.2\baselineskip minus  0.2\baselineskip
\textfloatsep 1.55\baselineskip plus  0.2\baselineskip minus  0.4\baselineskip
\@fptop 0pt plus 1fil
\@fpsep 0.75\baselineskip plus 2fil 
\@fpbot 0pt plus 1fil
\def\topfraction{0.9}
\def\bottomfraction{0.4}
\def\floatpagefraction{0.8}
\def\textfraction{0.1}

\dblfloatsep 0.85\baselineskip plus  0.2\baselineskip minus  0.2\baselineskip

\dbltextfloatsep 1.55\baselineskip plus  0.2\baselineskip minus  0.4\baselineskip

\@dblfptop 0pt plus 1fil
\@dblfpsep 0.75\baselineskip plus 2fil
\@dblfpbot 0pt plus 1fil
\def\dbltopfraction{0.8}
\def\dblfloatpagefraction{0.8}
\setcounter{dbltopnumber}{4}

\intextsep 0.85\baselineskip plus 0.2\baselineskip minus  0.2\baselineskip
\setcounter{topnumber}{2}
\setcounter{bottomnumber}{2}
\setcounter{totalnumber}{4}



\newlength\abovecaptionskip
\newlength\belowcaptionskip
\setlength\abovecaptionskip{0.5\baselineskip}
\setlength\belowcaptionskip{0pt}
\def\@IEEEfigurecaptionsepspace{\vskip\abovecaptionskip\relax}%
\def\@IEEEtablecaptionsepspace{\vskip\abovecaptionskip\relax}%


\def\@IEEEtablestring{table}


\def\@IEEEfiguretopskipspace{\ifdim\prevdepth=-1000pt\relax
\setlength{\@IEEEtrantmpdimenA}{1\topskip}\relax
\addtolength{\@IEEEtrantmpdimenA}{-0.7\@IEEEnormalsizeunitybaselineskip}\relax
\vspace*{\@IEEEtrantmpdimenA}\fi}
\def\@IEEEtabletopskipstrut{\ifdim\prevdepth=-1000pt\rule{0pt}{\topskip}\fi}


\ifCLASSOPTIONcompsoc
\ifCLASSOPTIONconference% compsoc conference
\long\def\@makecaption#1#2{%
\ifx\@captype\@IEEEtablestring%
\normalsize\bgroup\par\centering\@IEEEtabletopskipstrut{\normalfont\sffamily\normalsize {#1.}\nobreakspace #2}\par\addvspace{0.5\baselineskip}\egroup%
\@IEEEtablecaptionsepspace
\else
\@IEEEfigurecaptionsepspace
\setbox\@tempboxa\hbox{\normalfont\sffamily\normalsize {#1.}\nobreakspace #2}%
\ifdim \wd\@tempboxa >\hsize%
\setbox\@tempboxa\hbox{\normalfont\sffamily\normalsize {#1.}\nobreakspace}%
\parbox[t]{\hsize}{\normalfont\sffamily\normalsize \noindent\unhbox\@tempboxa#2}%
\else%
\hbox to\hsize{\normalfont\sffamily\normalsize\hfil\box\@tempboxa\hfil}%
\fi\fi}
\else% nonconference compsoc
\long\def\@makecaption#1#2{%
\ifx\@captype\@IEEEtablestring%
\normalsize\bgroup\par\centering\@IEEEtabletopskipstrut{\normalfont\sffamily\normalsize #1}\\{\normalfont\sffamily\normalsize #2}\par\addvspace{0.5\baselineskip}\egroup%
\@IEEEtablecaptionsepspace
\else
\@IEEEfigurecaptionsepspace
\setbox\@tempboxa\hbox{\normalfont\sffamily\normalsize {#1.}\nobreakspace #2}%
\ifdim \wd\@tempboxa >\hsize%
\setbox\@tempboxa\hbox{\normalfont\sffamily\normalsize {#1.}\nobreakspace}%
\parbox[t]{\hsize}{\normalfont\sffamily\normalsize \noindent\unhbox\@tempboxa#2}%
\else%
\hbox to\hsize{\normalfont\sffamily\normalsize\box\@tempboxa\hfil}%
\fi\fi}
\fi
\else% traditional noncompsoc \@makecaption
\long\def\@makecaption#1#2{%
\ifx\@captype\@IEEEtablestring%
\footnotesize\bgroup\par\centering\@IEEEtabletopskipstrut{\normalfont\footnotesize #1}\\{\normalfont\footnotesize\scshape #2}\par\addvspace{0.5\baselineskip}\egroup%
\@IEEEtablecaptionsepspace
\else
\@IEEEfigurecaptionsepspace
\setbox\@tempboxa\hbox{\normalfont\footnotesize {#1.}\nobreakspace\nobreakspace #2}%
\ifdim \wd\@tempboxa >\hsize%
\setbox\@tempboxa\hbox{\normalfont\footnotesize {#1.}\nobreakspace\nobreakspace}%
\parbox[t]{\hsize}{\normalfont\footnotesize\noindent\unhbox\@tempboxa#2}%
\else%
\ifCLASSOPTIONconference \hbox to\hsize{\normalfont\footnotesize\hfil\box\@tempboxa\hfil}%
\else \hbox to\hsize{\normalfont\footnotesize\box\@tempboxa\hfil}%
\fi\fi\fi}
\fi



\ifCLASSOPTIONcaptionsoff
\long\def\@makecaption#1#2{\vspace*{2em}\footnotesize\bgroup\par\addvspace{0.5\baselineskip}\centering{\footnotesize #1}\par\addvspace{0.5\baselineskip}\egroup%
\let\@IEEEtemporiglabeldefsave\label
\let\@IEEEtemplabelargsave\relax
\def\label##1{\gdef\@IEEEtemplabelargsave{##1}}%
\setbox\@tempboxa\hbox{#2}%
\let\label\@IEEEtemporiglabeldefsave
\ifx\@IEEEtemplabelargsave\relax\else\label{\@IEEEtemplabelargsave}\fi}
\fi


\newcounter{figure}
\def\thefigure{\@arabic\c@figure}
\def\fps@figure{tbp}
\def\ftype@figure{1}
\def\ext@figure{lof}
\def\fnum@figure{\figurename\nobreakspace\thefigure}
\def\figure{\def\@floatboxreset{\reset@font\normalsize\@setminipage\@IEEEfiguretopskipspace}\@float{figure}}
\def\endfigure{\end@float}
\@namedef{figure*}{\def\@floatboxreset{\reset@font\normalsize\@setminipage\@IEEEfiguretopskipspace}\@dblfloat{figure}}
\@namedef{endfigure*}{\end@dblfloat}

\newcounter{table}
\ifCLASSOPTIONcompsoc
\def\thetable{\arabic{table}}
\else
\def\thetable{\@Roman\c@table}
\fi
\def\fps@table{tbp}
\def\ftype@table{2}
\def\ext@table{lot}
\def\fnum@table{\tablename\nobreakspace\thetable}
\def\table{\def\@floatboxreset{\reset@font\footnotesize\@setminipage}\@float{table}}
\def\endtable{\end@float}
\@namedef{table*}{\def\@floatboxreset{\reset@font\footnotesize\@setminipage}\@dblfloat{table}}
\@namedef{endtable*}{\end@dblfloat}






\def\@IEEEclspkgerror{\ClassError{IEEEtran}}

\newif\if@IEEEeqnarrayboxnojot% flag to indicate if the environment was called as the star form
\@IEEEeqnarrayboxnojotfalse

\newif\if@advanceIEEEeqncolcnt% tracks if the environment should advance the col counter
\@advanceIEEEeqncolcnttrue

\newcount\@IEEEeqnnumcols % tracks how many IEEEeqnarray cols are defined
\newcount\@IEEEeqncolcnt  % tracks how many IEEEeqnarray cols the user actually used


\def\IEEEeqnarraymathstyle{\displaystyle}
\def\IEEEeqnarraytextstyle{\relax}

\def\IEEEeqnarraydecl{\relax}
\def\IEEEeqnarrayboxdecl{\relax}



\newif\if@IEEEeqnumpersist%
\@IEEEeqnumpersistfalse
\newif\if@IEEEsubeqnumpersist%
\@IEEEsubeqnumpersistfalse
\newif\if@IEEEeqnumpreadv%
\@IEEEeqnumpreadvfalse
\newif\if@IEEEsubeqnumpreadv%
\@IEEEsubeqnumpreadvfalse

\newcount\@IEEEsubeqnnumrollback% saves previous value of IEEEsubequation number in case we need to restore it

\providecommand{\yesnumber}{\global\@eqnswtrue}


\def\IEEEyesnumber{\@ifstar{\global\@IEEEeqnumpersisttrue\global\@IEEEsubeqnumpersistfalse\@IEEEyesnumber}{\@IEEEyesnumber}}

\def\@IEEEyesnumber{\global\@eqnswtrue
\if@IEEEeqnarrayISinner% alter counters and label only inside an IEEEeqnarray
\ifnum\c@IEEEsubequation>0\relax
   \stepcounter{equation}\setcounter{IEEEsubequation}{0}\gdef\@currentlabel{\p@equation\theequation}\relax
   \gdef\@currentHref{\@IEEEtheHrefequation}% setup hyperref label
\fi
\global\@IEEEeqnumpreadvfalse\global\@IEEEsubeqnumpreadvfalse
\fi}

\def\IEEEnonumber{\@ifstar{\global\@IEEEeqnumpersistfalse\global\@IEEEsubeqnumpersistfalse\global\@eqnswfalse}{\global\@eqnswfalse}}


\def\IEEEyessubnumber{\@ifstar{\global\@IEEEsubeqnumpersisttrue\@IEEEyessubnumber}{\@IEEEyessubnumber}}
\def\@IEEEyessubnumber{\if@IEEEeqnarrayISinner% alter counters and label only inside an IEEEeqnarray
  \ifnum\c@IEEEsubequation>0\relax% if it already is a subequation, we are good to go as-is
  \else% if we are a regular equation we have to watch out for two cases
    \if@IEEEeqnumpreadv% if this equation is the result of a preadvance, backout and bump the sub eqnnum
       \global\advance\c@equation\m@ne\global\c@IEEEsubequation=\@IEEEsubeqnnumrollback\addtocounter{IEEEsubequation}{1}\relax
    \else% non-preadvanced equations just need initialization of their sub eqnnum
       \setcounter{IEEEsubequation}{1}\relax
    \fi
  \fi% fi already is subequation
  \gdef\@currentlabel{\p@IEEEsubequation\theIEEEsubequation}\relax
  \gdef\@currentHref{\@IEEEtheHrefsubequation}% setup hyperref label
  \global\@IEEEeqnumpreadvfalse\global\@IEEEsubeqnumpreadvfalse% no longer a preadv anymore
  \global\@eqnswtrue
\fi}


\def\IEEEnosubnumber{\@ifstar{\global\@IEEEsubeqnumpersistfalse\@IEEEnosubnumber}{\@IEEEnosubnumber}}
\def\@IEEEnosubnumber{\if@IEEEeqnarrayISinner% alter counters and label only inside an IEEEeqnarray
  \if@eqnsw % we do nothing unless we know we will display because we play with the counters here
    \ifnum\c@IEEEsubequation>0\relax\addtocounter{equation}{1}\setcounter{IEEEsubequation}{0}\relax
    \fi
    \global\@IEEEeqnumpreadvfalse\global\@IEEEsubeqnumpreadvfalse% no longer a preadv anymore
    \gdef\@currentlabel{\p@equation\theequation}\relax
    \gdef\@currentHref{\@IEEEtheHrefequation}% setup hyperref label
  \fi
\fi}



\def\IEEEeqnarraynumspace{\hphantom{\ifnum\c@IEEEsubequation>0\relax\theIEEEsubequationdis\else\theequationdis\fi}}

\long\def\IEEEeqnarraymulticol#1#2#3{\multispan{#1}\relax
\relax\expandafter\ifx\csname @IEEEeqnarraycolDEF#2\endcsname\@IEEEeqnarraycolisdefined\relax
\csname @IEEEeqnarraycolPRE#2\endcsname
\else% if not, error and use default type
\@IEEEclspkgerror{Invalid column type "#2" in \string\IEEEeqnarraymulticol.\MessageBreak
Using a default centering column instead}%
{You must define IEEEeqnarray column types before use.}%
\csname @IEEEeqnarraycolPRE@IEEEdefault\endcsname
\fi
#3\relax\relax\relax\relax\relax\relax\relax\relax\relax\relax
\expandafter\ifx\csname @IEEEeqnarraycolDEF#2\endcsname\@IEEEeqnarraycolisdefined\relax
\csname @IEEEeqnarraycolPOST#2\endcsname
\else% if not, use the default type
\csname @IEEEeqnarraycolPOST@IEEEdefault\endcsname
\fi
\if@advanceIEEEeqncolcnt\global\advance\@IEEEeqncolcnt by #1\relax\fi}

\def\IEEEeqnarrayomit{\omit\if@advanceIEEEeqncolcnt\global\advance\@IEEEeqncolcnt by 1\relax\fi}


\def\IEEEeqnarraydefcol#1#2#3{\expandafter\def\csname @IEEEeqnarraycolPRE#1\endcsname{#2}%
\expandafter\def\csname @IEEEeqnarraycolPOST#1\endcsname{#3}%
\expandafter\def\csname @IEEEeqnarraycolDEF#1\endcsname{1}}


\def\IEEEeqnarraydefcolsep#1#2{\expandafter\def\csname @IEEEeqnarraycolSEP\romannumeral #1\endcsname{#2}%
\expandafter\def\csname @IEEEeqnarraycolSEPDEF\romannumeral #1\endcsname{1}}


\def\@IEEEeqnarraycolisdefined{1}% just a macro for 1, used for checking undefined column types


\def\@IEEEappendtoksA#1{\edef\@@IEEEappendtoksA{\@IEEEtrantmptoksA={\the\@IEEEtrantmptoksA #1}}%
\@@IEEEappendtoksA}

\def\@IEEEappendNOEXPANDtoksA#1{\toks8={#1}%
\edef\@@IEEEappendNOEXPANDtoksA{\@IEEEtrantmptoksA={\the\@IEEEtrantmptoksA\the\toks8}}%
\@@IEEEappendNOEXPANDtoksA}

\IEEEeqnarraydefcol{l}{$\IEEEeqnarraymathstyle}{$\hfil}
\IEEEeqnarraydefcol{c}{\hfil$\IEEEeqnarraymathstyle}{$\hfil}
\IEEEeqnarraydefcol{r}{\hfil$\IEEEeqnarraymathstyle}{$}
\IEEEeqnarraydefcol{L}{$\IEEEeqnarraymathstyle{}}{{}$\hfil}
\IEEEeqnarraydefcol{C}{\hfil$\IEEEeqnarraymathstyle{}}{{}$\hfil}
\IEEEeqnarraydefcol{R}{\hfil$\IEEEeqnarraymathstyle{}}{{}$}
\IEEEeqnarraydefcol{s}{\IEEEeqnarraytextstyle}{\hfil}
\IEEEeqnarraydefcol{t}{\hfil\IEEEeqnarraytextstyle}{\hfil}
\IEEEeqnarraydefcol{u}{\hfil\IEEEeqnarraytextstyle}{}

\IEEEeqnarraydefcol{v}{}{\vrule width\arrayrulewidth}
\IEEEeqnarraydefcol{vv}{\vrule width\arrayrulewidth\hfil}{\hfil\vrule width\arrayrulewidth}
\IEEEeqnarraydefcol{V}{}{\vrule width\arrayrulewidth\hskip\doublerulesep\vrule width\arrayrulewidth}
\IEEEeqnarraydefcol{VV}{\vrule width\arrayrulewidth\hskip\doublerulesep\vrule width\arrayrulewidth\hfil}%
{\hfil\vrule width\arrayrulewidth\hskip\doublerulesep\vrule width\arrayrulewidth}

\IEEEeqnarraydefcol{h}{}{\leaders\hrule height\arrayrulewidth\hfil}
\IEEEeqnarraydefcol{H}{}{\leaders\vbox{\hrule width\arrayrulewidth\vskip\doublerulesep\hrule width\arrayrulewidth}\hfil}

\IEEEeqnarraydefcol{x}{}{}
\IEEEeqnarraydefcol{X}{$}{$}

\IEEEeqnarraydefcol{@IEEEdefault}{\hfil$\IEEEeqnarraymathstyle}{$\hfil}


\def\@IEEEeqnarraycolSEPzero{0pt plus 0pt minus 0pt}
\def\@IEEEeqnarraycolSEPcenter{1000pt plus 0pt minus 1000pt}

\edef\@IEEEeqnarraycolSEPdefaultstart{\@IEEEeqnarraycolSEPcenter}% default start glue
\edef\@IEEEeqnarraycolSEPdefaultend{\@IEEEeqnarraycolSEPcenter}% default end glue
\edef\@IEEEeqnarraycolSEPdefaultmid{\@IEEEeqnarraycolSEPzero}% default inter-column glue



\newcommand\IEEEeqnarrayvrule[1][\arrayrulewidth]{\vrule\@width#1\relax}

\def\IEEEeqnarrayseprow{\relax\@ifnextchar[{\@IEEEeqnarrayseprow}{\@IEEEeqnarrayseprow[0.25\normalbaselineskip]}}
\def\@IEEEeqnarrayseprow[#1]{\relax\@ifnextchar[{\@@IEEEeqnarrayseprow[#1]}{\@@IEEEeqnarrayseprow[#1][\relax]}}
\def\@@IEEEeqnarrayseprow[#1][#2]{\def\@IEEEeqnarrayseprowARGONE{#1}%
\ifx\@IEEEeqnarrayseprowARGONE\@empty%
{\setbox0=\hbox{#2\relax\global\skip5=0.25\normalbaselineskip}}%
\else%
{\setbox0=\hbox{#2\relax\global\skip5=#1}}%
\fi%
\@IEEEeqnarrayhoptolastcolumn\IEEEeqnarraystrutsize{\skip5}{0pt}[\relax]\relax}

\def\IEEEeqnarrayseprowcut{\multispan{\@IEEEeqnnumcols}\relax% span all the cols
\if@advanceIEEEeqncolcnt\global\advance\@IEEEeqncolcnt by \@IEEEeqnnumcols\relax\fi%
\@ifnextchar[{\@IEEEeqnarrayseprowcut}{\@IEEEeqnarrayseprowcut[0.25\normalbaselineskip]}}
\def\@IEEEeqnarrayseprowcut[#1]{\relax\@ifnextchar[{\@@IEEEeqnarrayseprowcut[#1]}{\@@IEEEeqnarrayseprowcut[#1][\relax]}}
\def\@@IEEEeqnarrayseprowcut[#1][#2]{\def\@IEEEeqnarrayseprowARGONE{#1}%
\ifx\@IEEEeqnarrayseprowARGONE\@empty%
{\setbox0=\hbox{#2\relax\global\skip5=0.25\normalbaselineskip}}%
\else%
{\setbox0=\hbox{#2\relax\global\skip5=#1}}%
\fi%
\IEEEeqnarraystrutsize{\skip5}{0pt}[\relax]\relax}



\def\IEEEeqnarrayrulerow{\multispan{\@IEEEeqnnumcols}\relax% span all the cols
\if@advanceIEEEeqncolcnt\global\advance\@IEEEeqncolcnt by \@IEEEeqnnumcols\relax\fi%
\@ifnextchar[{\@IEEEeqnarrayrulerow}{\@IEEEeqnarrayrulerow[\arrayrulewidth]}}
\def\@IEEEeqnarrayrulerow[#1]{\leaders\hrule height#1\hfil\relax% put in our rule 
\IEEEeqnarraystrutsize{0pt}{0pt}[\relax]\relax}


\def\IEEEeqnarraydblrulerow{\multispan{\@IEEEeqnnumcols}\relax% span all the cols
\if@advanceIEEEeqncolcnt\global\advance\@IEEEeqncolcnt by \@IEEEeqnnumcols\relax\fi%
\@ifnextchar[{\@IEEEeqnarraydblrulerow}{\@IEEEeqnarraydblrulerow[\arrayrulewidth]}}
\def\@IEEEeqnarraydblrulerow[#1]{\relax\@ifnextchar[{\@@IEEEeqnarraydblrulerow[#1]}%
{\@@IEEEeqnarraydblrulerow[#1][\doublerulesep]}}
\def\@@IEEEeqnarraydblrulerow[#1][#2]{\def\@IEEEeqnarraydblrulerowARG{#1}%
\ifx\@IEEEeqnarraydblrulerowARG\@empty%
\@IEEEeqnarrayrulerow[\arrayrulewidth]%
\else%
\@IEEEeqnarrayrulerow[#1]\relax%
\fi%
\def\@IEEEeqnarraydblrulerowARG{#2}%
\ifx\@IEEEeqnarraydblrulerowARG\@empty%
\\\IEEEeqnarrayseprow[\doublerulesep][\relax]%
\else%
\\\IEEEeqnarrayseprow[#2][\relax]%
\fi%
\\\multispan{\@IEEEeqnnumcols}%
\if@advanceIEEEeqncolcnt\global\advance\@IEEEeqncolcnt by \@IEEEeqnnumcols\relax\fi%
\def\@IEEEeqnarraydblrulerowARG{#1}%
\ifx\@IEEEeqnarraydblrulerowARG\@empty%
\@IEEEeqnarrayrulerow[\arrayrulewidth]%
\else%
\@IEEEeqnarrayrulerow[#1]%
\fi%
}

\def\IEEEeqnarraydblrulerowcut{\multispan{\@IEEEeqnnumcols}\relax% span all the cols
\if@advanceIEEEeqncolcnt\global\advance\@IEEEeqncolcnt by \@IEEEeqnnumcols\relax\fi%
\@ifnextchar[{\@IEEEeqnarraydblrulerowcut}{\@IEEEeqnarraydblrulerowcut[\arrayrulewidth]}}
\def\@IEEEeqnarraydblrulerowcut[#1]{\relax\@ifnextchar[{\@@IEEEeqnarraydblrulerowcut[#1]}%
{\@@IEEEeqnarraydblrulerowcut[#1][\doublerulesep]}}
\def\@@IEEEeqnarraydblrulerowcut[#1][#2]{\def\@IEEEeqnarraydblrulerowARG{#1}%
\ifx\@IEEEeqnarraydblrulerowARG\@empty%
\@IEEEeqnarrayrulerow[\arrayrulewidth]%
\else%
\@IEEEeqnarrayrulerow[#1]%
\fi%
\def\@IEEEeqnarraydblrulerowARG{#2}%
\ifx\@IEEEeqnarraydblrulerowARG\@empty%
\\\IEEEeqnarrayseprowcut[\doublerulesep][\relax]%
\else%
\\\IEEEeqnarrayseprowcut[#2][\relax]%
\fi%
\\\multispan{\@IEEEeqnnumcols}%
\if@advanceIEEEeqncolcnt\global\advance\@IEEEeqncolcnt by \@IEEEeqnnumcols\relax\fi%
\def\@IEEEeqnarraydblrulerowARG{#1}%
\ifx\@IEEEeqnarraydblrulerowARG\@empty%
\@IEEEeqnarrayrulerow[\arrayrulewidth]%
\else%
\@IEEEeqnarrayrulerow[#1]%
\fi%
}



\def\@IEEEeqnarrayhoptolastcolumn{\@IEEEtrantmptoksA={}\count0=1\relax%
\loop% add cols if the user did not use them all
\ifnum\count0<\@IEEEeqnnumcols\relax%
\@IEEEappendtoksA{&}%
\advance\count0 by 1\relax% update the col count
\repeat%
\the\@IEEEtrantmptoksA%execute the &'s
}



\newif\if@IEEEeqnarrayISinner % flag to indicate if we are within the lines
\@IEEEeqnarrayISinnerfalse    % of an IEEEeqnarray - after the IEEEeqnarraydecl

\edef\@IEEEeqnarrayTHEstrutheight{0pt} % height and depth of IEEEeqnarray struts
\edef\@IEEEeqnarrayTHEstrutdepth{0pt}

\edef\@IEEEeqnarrayTHEmasterstrutheight{0pt} % default height and depth of
\edef\@IEEEeqnarrayTHEmasterstrutdepth{0pt}  % struts within an IEEEeqnarray

\edef\@IEEEeqnarrayTHEmasterstrutHSAVE{0pt} % saved master strut height
\edef\@IEEEeqnarrayTHEmasterstrutDSAVE{0pt} % and depth

\newif\if@IEEEeqnarrayusemasterstrut % flag to indicate that the master strut value
\@IEEEeqnarrayusemasterstruttrue     % is to be used



\def\@IEEEeqnarraymasterstrutsave{\relax%
\expandafter\skip0=\@IEEEeqnarrayTHEmasterstrutheight\relax%
\expandafter\skip2=\@IEEEeqnarrayTHEmasterstrutdepth\relax%
\dimen0\skip0\relax%
\dimen2\skip2\relax%
\edef\@IEEEeqnarrayTHEmasterstrutHSAVE{\the\dimen0}%
\edef\@IEEEeqnarrayTHEmasterstrutDSAVE{\the\dimen2}}

\def\@IEEEeqnarraymasterstrutrestore{\relax%
\expandafter\skip0=\@IEEEeqnarrayTHEmasterstrutHSAVE\relax%
\expandafter\skip2=\@IEEEeqnarrayTHEmasterstrutDSAVE\relax%
\dimen0\skip0\relax%
\dimen2\skip2\relax%
\edef\@IEEEeqnarrayTHEmasterstrutheight{\the\dimen0}%
\edef\@IEEEeqnarrayTHEmasterstrutdepth{\the\dimen2}}


\def\@IEEEeqnarraystrutreset{\relax%
\expandafter\skip0=\@IEEEeqnarrayTHEmasterstrutheight\relax%
\expandafter\skip2=\@IEEEeqnarrayTHEmasterstrutdepth\relax%
\dimen0\skip0\relax%
\dimen2\skip2\relax%
\xdef\@IEEEeqnarrayTHEstrutheight{\the\dimen0}%
\xdef\@IEEEeqnarrayTHEstrutdepth{\the\dimen2}%
\global\@IEEEeqnarrayusemasterstruttrue}


\def\@IEEEeqnarrayglobalizestrutstatus{\relax%
\if@IEEEeqnarrayusemasterstrut\else%
\xdef\@IEEEeqnarrayTHEstrutheight{\@IEEEeqnarrayTHEstrutheight}%
\xdef\@IEEEeqnarrayTHEstrutdepth{\@IEEEeqnarrayTHEstrutdepth}%
\global\@IEEEeqnarrayusemasterstrutfalse%
\fi}



\def\IEEEeqnarraystrutsize#1#2{\relax\@ifnextchar[{\@IEEEeqnarraystrutsize{#1}{#2}}{\@IEEEeqnarraystrutsize{#1}{#2}[\relax]}}
\def\@IEEEeqnarraystrutsize#1#2[#3]{\def\@IEEEeqnarraystrutsizeARG{#1}%
\ifx\@IEEEeqnarraystrutsizeARG\@empty%
{\setbox0=\hbox{#3\relax\global\skip3=0.7\normalbaselineskip}}%
\skip0=\skip3\relax%
\else% arg one present
{\setbox0=\hbox{#3\relax\global\skip3=#1\relax}}%
\skip0=\skip3\relax%
\fi% if null arg
\def\@IEEEeqnarraystrutsizeARG{#2}%
\ifx\@IEEEeqnarraystrutsizeARG\@empty%
{\setbox0=\hbox{#3\relax\global\skip3=0.3\normalbaselineskip}}%
\skip2=\skip3\relax%
\else% arg two present
{\setbox0=\hbox{#3\relax\global\skip3=#2\relax}}%
\skip2=\skip3\relax%
\fi% if null arg
\dimen0\skip0\relax%
\dimen2\skip2\relax%
\if@IEEEeqnarrayISinner% inner does not touch master strut size
\edef\@IEEEeqnarrayTHEstrutheight{\the\dimen0}%
\edef\@IEEEeqnarrayTHEstrutdepth{\the\dimen2}%
\@IEEEeqnarrayusemasterstrutfalse% do not use master
\else% outer, have to set master strut too
\edef\@IEEEeqnarrayTHEmasterstrutheight{\the\dimen0}%
\edef\@IEEEeqnarrayTHEmasterstrutdepth{\the\dimen2}%
\edef\@IEEEeqnarrayTHEstrutheight{\the\dimen0}%
\edef\@IEEEeqnarrayTHEstrutdepth{\the\dimen2}%
\@IEEEeqnarrayusemasterstruttrue% use master strut
\fi}


\def\IEEEeqnarraystrutsizeadd#1#2{\relax\@ifnextchar[{\@IEEEeqnarraystrutsizeadd{#1}{#2}}{\@IEEEeqnarraystrutsizeadd{#1}{#2}[\relax]}}
\def\@IEEEeqnarraystrutsizeadd#1#2[#3]{\def\@IEEEeqnarraystrutsizearg{#1}%
\ifx\@IEEEeqnarraystrutsizearg\@empty%
\skip0=0pt\relax%
\else% arg one present
{\setbox0=\hbox{#3\relax\global\skip3=#1}}%
\skip0=\skip3\relax%
\fi% if null arg
\def\@IEEEeqnarraystrutsizearg{#2}%
\ifx\@IEEEeqnarraystrutsizearg\@empty%
\skip2=0pt\relax%
\else% arg two present
{\setbox0=\hbox{#3\relax\global\skip3=#2}}%
\skip2=\skip3\relax%
\fi% if null arg
\dimen0\skip0\relax%
\dimen2\skip2\relax%
\if@IEEEeqnarrayISinner% inner does not touch master strut size
\expandafter\skip0=\@IEEEeqnarrayTHEstrutheight\relax%
\expandafter\skip2=\@IEEEeqnarrayTHEstrutdepth\relax%
\advance\dimen0 by \skip0\relax%
\advance\dimen2 by \skip2\relax%
\edef\@IEEEeqnarrayTHEstrutheight{\the\dimen0}%
\edef\@IEEEeqnarrayTHEstrutdepth{\the\dimen2}%
\@IEEEeqnarrayusemasterstrutfalse% do not use master
\else% outer, have to set master strut too
\expandafter\skip0=\@IEEEeqnarrayTHEmasterstrutheight\relax%
\expandafter\skip2=\@IEEEeqnarrayTHEmasterstrutdepth\relax%
\advance\dimen0 by \skip0\relax%
\advance\dimen2 by \skip2\relax%
\edef\@IEEEeqnarrayTHEmasterstrutheight{\the\dimen0}%
\edef\@IEEEeqnarrayTHEmasterstrutdepth{\the\dimen2}%
\edef\@IEEEeqnarrayTHEstrutheight{\the\dimen0}%
\edef\@IEEEeqnarrayTHEstrutdepth{\the\dimen2}%
\@IEEEeqnarrayusemasterstruttrue% use master strut
\fi}


\newif\ifIEEEvisiblestruts
\IEEEvisiblestrutsfalse

\def\@IEEEeqnarrayinsertstrut{\relax%
\if@IEEEeqnarrayusemasterstrut
\expandafter\skip0=\@IEEEeqnarrayTHEmasterstrutheight\relax%
\expandafter\skip2=\@IEEEeqnarrayTHEmasterstrutdepth\relax%
\else%
\expandafter\skip0=\@IEEEeqnarrayTHEstrutheight\relax%
\expandafter\skip2=\@IEEEeqnarrayTHEstrutdepth\relax%
\fi%
\dimen0\skip0\relax%
\dimen2\skip2\relax%
\ifIEEEvisiblestruts%
\vrule width0.2pt height\dimen0 depth\dimen2\relax%
\else%
\vrule width0pt height\dimen0 depth\dimen2\relax\fi}


\def\IEEEstrut{\relax\@ifnextchar[{\@IEEEstrut}{\@IEEEstrut[0.7\normalbaselineskip]}}
\def\@IEEEstrut[#1]{\relax\@ifnextchar[{\@@IEEEstrut[#1]}{\@@IEEEstrut[#1][0.3\normalbaselineskip]}}
\def\@@IEEEstrut[#1][#2]{\relax\@ifnextchar[{\@@@IEEEstrut[#1][#2]}{\@@@IEEEstrut[#1][#2][\relax]}}
\def\@@@IEEEstrut[#1][#2][#3]{\mbox{#3\relax%
\def\@IEEEstrutARG{#1}%
\ifx\@IEEEstrutARG\@empty%
\skip0=0.7\normalbaselineskip\relax%
\else%
\skip0=#1\relax%
\fi%
\def\@IEEEstrutARG{#2}%
\ifx\@IEEEstrutARG\@empty%
\skip2=0.3\normalbaselineskip\relax%
\else%
\skip2=#2\relax%
\fi%
\dimen0\skip0\relax%
\dimen2\skip2\relax%
\ifIEEEvisiblestruts%
\vrule width0.2pt height\dimen0 depth\dimen2\relax%
\else%
\vrule width0.0pt height\dimen0 depth\dimen2\relax\fi}}


\def\IEEEeqnarraystrutmode{\IEEEeqnarraystrutsize{0.7\normalbaselineskip}{0.3\normalbaselineskip}[\relax]%
\baselineskip=0pt\lineskip=0pt\lineskiplimit=0pt\jot=0pt}


\def\@IEEEtheHrefequation{equation.\theHequation}
\def\@IEEEtheHrefsubequation{equation.\theHequation\alph{IEEEsubequation}}


\def\IEEEeqnarray{\@IEEEeqnumpersisttrue\@IEEEsubeqnumpersistfalse\@IEEEeqnarray}
\def\endIEEEeqnarray{\end@IEEEeqnarray}

\@namedef{IEEEeqnarray*}{\@IEEEeqnumpersistfalse\@IEEEsubeqnumpersistfalse\@IEEEeqnarray}
\@namedef{endIEEEeqnarray*}{\end@IEEEeqnarray}


\def\@IEEEeqnarray{\relax\@ifnextchar[{\@@IEEEeqnarray}{\@@IEEEeqnarray[\relax]}}
\def\@@IEEEeqnarray[#1]#2{%
   \if@IEEEeqnumpersist\global\@eqnswtrue
   \else% not the star form
   \global\@eqnswfalse
   \fi% if star form
   \@ifundefined{theHequation}{\def\theHequation{\arabic{equation}}}{}\relax
   \providecommand{\Hy@raisedlink}[1]{}\relax
   \providecommand{\hyper@anchorstart}[1]{}\relax
   \providecommand{\hyper@anchorend}{}\relax
   \providecommand{\@currentHref}{}\relax
   \@IEEEeqnumpreadvfalse% reset eqnpreadv flag
   \@IEEEsubeqnumpreadvfalse% reset subeqnpreadv flag
   \@IEEEeqnarrayISinnerfalse% not yet within the lines of the halign
   \@IEEEeqnarraystrutsize{0pt}{0pt}[\relax]% turn off struts by default
   \@IEEEeqnarrayusemasterstruttrue% use master strut till user asks otherwise
   \IEEEvisiblestrutsfalse% diagnostic mode defaults to off
   \lineskip=0pt\relax
   \lineskiplimit=0pt\relax
   \baselineskip=\normalbaselineskip\relax%
   \jot=\IEEEnormaljot\relax%
   \mathsurround\z@\relax% no extra spacing around math
   \@advanceIEEEeqncolcnttrue% advance the col counter for each col the user uses, 
   \global\@IEEEsubeqnnumrollback=\c@IEEEsubequation%
   \stepcounter{equation}\@IEEEeqnumpreadvtrue% advance equation counter before first line
   \setcounter{IEEEsubequation}{0}% no subequation yet
   \let\@IEEEcurrentlabelsave\@currentlabel% save current label as we later change it globally
   \let\@IEEEcurrentHrefsave\@currentHref% save current href label as we later change it globally
   \def\@currentlabel{\p@equation\theequation}% redefine the ref label
   \def\@currentHref{\@IEEEtheHrefequation}% setup hyperref label
   \IEEEeqnarraydecl\relax% allow a way for the user to make global overrides
   #1\relax% allow user to override defaults
   \let\\\@IEEEeqnarraycr% replace newline with one that can put in eqn. numbers
   \global\@IEEEeqncolcnt\z@% col. count = 0 for first line
   \@IEEEbuildpreamble #2\end\relax% build the preamble and put it into \@IEEEtrantmptoksA 
   \ifnum\@IEEEeqnnumcols>0\relax\@IEEEappendtoksA{&}\fi% col separator for those after the first
   \toks0={##}%
   \@IEEEappendtoksA{\global\advance\@IEEEeqncolcnt by 1\relax}%
   \@IEEEappendtoksA{\tabskip\z@skip\bgroup\the\toks0\egroup}%
   \@IEEEappendtoksA{&\global\advance\@IEEEeqncolcnt by 1\relax}%
   \@IEEEappendtoksA{\tabskip\z@skip\hb@xt@\z@\bgroup\hss\the\toks0\egroup}%
   \tabskip=\@IEEEBPstartglue\relax
   \@IEEEeqnarrayISinnertrue% commands are now within the lines
   $$\everycr{}\halign to\displaywidth\bgroup
   \span\the\@IEEEtrantmptoksA\cr}

\def\end@IEEEeqnarray{\@IEEEeqnarrayglobalizestrutstatus&\@@IEEEeqnarraycr\egroup
\if@IEEEsubeqnumpreadv\global\advance\c@IEEEsubequation\m@ne\fi
\if@IEEEeqnumpreadv\global\advance\c@equation\m@ne\global\c@IEEEsubequation=\@IEEEsubeqnnumrollback\fi
\global\let\@currentlabel\@IEEEcurrentlabelsave% restore current label
\global\let\@currentHref\@IEEEcurrentHrefsave% restore current href label
$$\@ignoretrue}


\def\@IEEEeqnarraycr{\@IEEEeqnarrayglobalizestrutstatus&% save strut status and advance to next column
   {\ifnum0=`}\fi
   \@ifstar{%
      \global\@eqpen\@M\@IEEEeqnarrayYCR
   }{%
      \global\@eqpen\interdisplaylinepenalty \@IEEEeqnarrayYCR
   }%
}

\def\@IEEEeqnarrayYCR{\@testopt\@IEEEeqnarrayXCR\z@skip}

\def\@IEEEeqnarrayXCR[#1]{%
   \ifnum0=`{\fi}%
   \@@IEEEeqnarraycr
   \noalign{\penalty\@eqpen\vskip\jot\vskip #1\relax}}%

\def\@@IEEEeqnarraycr{\@IEEEtrantmptoksA={}% clear token register
    \advance\@IEEEeqncolcnt by -1\relax% adjust col count because of the isolation column
    \ifnum\@IEEEeqncolcnt>\@IEEEeqnnumcols\relax
    \@IEEEclspkgerror{Too many columns within the IEEEeqnarray\MessageBreak
                          environment}%
    {Use fewer \string &'s or put more columns in the IEEEeqnarray column\MessageBreak 
     specifications.}\relax%
    \else
    \loop% add cols if the user did not use them all
    \ifnum\@IEEEeqncolcnt<\@IEEEeqnnumcols\relax
    \@IEEEappendtoksA{&}%
    \advance\@IEEEeqncolcnt by 1\relax% update the col count
    \repeat
    \fi
    \the\@IEEEtrantmptoksA%
    \@IEEEeqnarrayinsertstrut% do the strut if needed
    \@IEEEeqnarraystrutreset% reset the strut system for next line or IEEEeqnarray
    &% and enter the equation number column
    \if@eqnsw% only if we display something
      \Hy@raisedlink{\hyper@anchorstart{\@currentHref}}% start a hyperref anchor
      \global\@IEEEeqnumpreadvfalse\relax% displaying an equation number means
      \global\@IEEEsubeqnumpreadvfalse\relax% the equation counters point to valid equations
      \ifnum\c@IEEEsubequation>0\relax% handle subequation
         \theIEEEsubequationdis\relax
         \if@IEEEsubeqnumpersist% setup for default type of next line
            \stepcounter{IEEEsubequation}\global\@IEEEsubeqnumpreadvtrue\relax
            \gdef\@currentlabel{\p@IEEEsubequation\theIEEEsubequation}\relax
            \gdef\@currentHref{\@IEEEtheHrefsubequation}% setup hyperref label
         \else
             \global\@IEEEsubeqnnumrollback=\c@IEEEsubequation
             \stepcounter{equation}\global\@IEEEeqnumpreadvtrue\relax
             \setcounter{IEEEsubequation}{0}\gdef\@currentlabel{\p@equation\theequation}\relax
             \gdef\@currentHref{\@IEEEtheHrefequation}% setup hyperref label
         \fi
      \else% display a standard equation number
        \theequationdis\relax
        \setcounter{IEEEsubequation}{0}\relax% not really needed
        \if@IEEEsubeqnumpersist% setup for default type of next line
           \stepcounter{IEEEsubequation}\global\@IEEEsubeqnumpreadvtrue\relax
           \gdef\@currentlabel{\p@IEEEsubequation\theIEEEsubequation}\relax
           \gdef\@currentHref{\@IEEEtheHrefsubequation}% setup hyperref label
         \else
             \global\@IEEEsubeqnnumrollback=\c@IEEEsubequation
             \stepcounter{equation}\global\@IEEEeqnumpreadvtrue\relax
             \setcounter{IEEEsubequation}{0}\gdef\@currentlabel{\p@equation\theequation}\relax
             \gdef\@currentHref{\@IEEEtheHrefequation}% setup hyperref label
         \fi
      \fi%
      \Hy@raisedlink{\hyper@anchorend}% end hyperref anchor
    \fi% fi only if we display something
    \if@IEEEeqnumpersist\global\@eqnswtrue\else\global\@eqnswfalse\fi
    \if@IEEEsubeqnumpersist\global\@eqnswtrue\fi% ditto for the subequation flag
    \global\@IEEEeqncolcnt\z@\relax
    \cr}





\def\IEEEeqnarrayboxm{\@IEEEeqnarrayboxnojotfalse\@IEEEeqnarrayboxHBOXSWfalse\@IEEEeqnarraybox}
\def\endIEEEeqnarrayboxm{\end@IEEEeqnarraybox}
\@namedef{IEEEeqnarrayboxm*}{\@IEEEeqnarrayboxnojottrue\@IEEEeqnarrayboxHBOXSWfalse\@IEEEeqnarraybox}
\@namedef{endIEEEeqnarrayboxm*}{\end@IEEEeqnarraybox}

\def\IEEEeqnarrayboxt{\@IEEEeqnarrayboxnojotfalse\@IEEEeqnarrayboxHBOXSWtrue\@IEEEeqnarraybox}
\def\endIEEEeqnarrayboxt{\end@IEEEeqnarraybox}
\@namedef{IEEEeqnarrayboxt*}{\@IEEEeqnarrayboxnojottrue\@IEEEeqnarrayboxHBOXSWtrue\@IEEEeqnarraybox}
\@namedef{endIEEEeqnarrayboxt*}{\end@IEEEeqnarraybox}

\def\IEEEeqnarraybox{\@IEEEeqnarrayboxnojotfalse\ifmmode\@IEEEeqnarrayboxHBOXSWfalse\else\@IEEEeqnarrayboxHBOXSWtrue\fi%
\@IEEEeqnarraybox}
\def\endIEEEeqnarraybox{\end@IEEEeqnarraybox}

\@namedef{IEEEeqnarraybox*}{\@IEEEeqnarrayboxnojottrue\ifmmode\@IEEEeqnarrayboxHBOXSWfalse\else\@IEEEeqnarrayboxHBOXSWtrue\fi%
\@IEEEeqnarraybox}
\@namedef{endIEEEeqnarraybox*}{\end@IEEEeqnarraybox}

\newif\if@IEEEeqnarrayboxHBOXSW%
\@IEEEeqnarrayboxHBOXSWfalse

\def\@IEEEeqnarraybox{\relax\@ifnextchar[{\@@IEEEeqnarraybox}{\@@IEEEeqnarraybox[\relax]}}
\def\@@IEEEeqnarraybox[#1]{\relax\@ifnextchar[{\@@@IEEEeqnarraybox[#1]}{\@@@IEEEeqnarraybox[#1][b]}}
\def\@@@IEEEeqnarraybox[#1][#2]{\relax\@ifnextchar[{\@@@@IEEEeqnarraybox[#1][#2]}{\@@@@IEEEeqnarraybox[#1][#2][\relax]}}

\def\@@@@IEEEeqnarraybox[#1][#2][#3]#4{\@IEEEeqnarrayISinnerfalse % not yet within the lines of the halign
   \@IEEEeqnarraymasterstrutsave% save current master strut values
   \@IEEEeqnarraystrutsize{0pt}{0pt}[\relax]% turn off struts by default
   \@IEEEeqnarrayusemasterstruttrue% use master strut till user asks otherwise
   \IEEEvisiblestrutsfalse% diagnostic mode defaults to off
   \lineskip=0pt\relax%
   \lineskiplimit=0pt\relax%
   \baselineskip=\normalbaselineskip\relax%
   \jot=\IEEEnormaljot\relax%
   \mathsurround\z@\relax% no extra spacing around math
   \edef\@IEEEeqnarraycolSEPdefaultstart{\@IEEEeqnarraycolSEPzero}% default start glue
   \edef\@IEEEeqnarraycolSEPdefaultend{\@IEEEeqnarraycolSEPzero}% default end glue
   \edef\@IEEEeqnarraycolSEPdefaultmid{\@IEEEeqnarraycolSEPzero}% default inter-column glue
   \@advanceIEEEeqncolcntfalse% do not advance the col counter for each col the user uses, 
   \IEEEeqnarrayboxdecl\relax% allow a way for the user to make global overrides
   #1\relax% allow user to override defaults
   \let\\\@IEEEeqnarrayboxcr% replace newline with one that allows optional spacing
   \@IEEEbuildpreamble #4\end\relax% build the preamble and put it into \@IEEEtrantmptoksA
   \ifnum\@IEEEeqnnumcols>0\relax\@IEEEappendtoksA{&}\fi% col separator for those after the first
   \toks0={##}%
   \@IEEEappendtoksA{\tabskip\z@skip\bgroup\the\toks0\egroup}% 
   \tabskip=\@IEEEBPstartglue\relax
   \everycr{}%
   \def\@IEEEgrabfirstoken##1##2\noindent{\let\@IEEEgrabbedfirstoken=##1}%
   \@IEEEgrabfirstoken#2\relax\relax\noindent
   \if@IEEEeqnarrayboxHBOXSW \leavevmode \hbox \bgroup $\fi%
   \if\@IEEEgrabbedfirstoken t\relax\vtop\else\if\@IEEEgrabbedfirstoken c\relax%
   \vcenter\else\vbox\fi\fi\bgroup%
   \@IEEEeqnarrayISinnertrue% commands are now within the lines
   \ifx#3\relax\halign\else\halign to #3\relax\fi%
   \bgroup
   \span\the\@IEEEtrantmptoksA\cr}

\def\end@IEEEeqnarraybox{\@IEEEeqnarrayglobalizestrutstatus% carry strut status
&% enter isolation/strut column
\@IEEEeqnarrayinsertstrut% do strut if needed
\@IEEEeqnarraymasterstrutrestore% restore the previous master strut values
\@IEEEeqnarraystrutreset%
\crcr\egroup\egroup%
\if@IEEEeqnarrayboxHBOXSW $\egroup\fi}



\def\@IEEEeqnarrayboxcr{\@IEEEeqnarrayglobalizestrutstatus% carry strut status
&% enter isolation/strut column
\@IEEEeqnarrayinsertstrut% do strut if needed
\@IEEEeqnarraystrutreset%
{\ifnum0=`}\fi%
\@ifstar{\@IEEEeqnarrayboxYCR}{\@IEEEeqnarrayboxYCR}}

\def\@IEEEeqnarrayboxYCR{\@testopt\@IEEEeqnarrayboxXCR\z@skip}

\def\@IEEEeqnarrayboxXCR[#1]{\ifnum0=`{\fi}%
\cr\noalign{\if@IEEEeqnarrayboxnojot\else\vskip\jot\fi\vskip#1\relax}}



\def\@IEEEbuildpreamble{\@IEEEtrantmptoksA={}% clear token register
\let\@IEEEBPcurtype=u%current column type is not yet known
\let\@IEEEBPprevtype=s%the previous column type was the start
\let\@IEEEBPnexttype=u%next column type is not yet known
\def\@IEEEBPcurglue={0pt plus 0pt minus 0pt}%
\def\@IEEEBPcurcolname{@IEEEdefault}% name of current column definition
\let\@IEEEBPcurnum=\@IEEEtrantmpcountA%
\@IEEEBPcurnum=0%
\@IEEEeqnnumcols=0%
\edef\@IEEEBPstartglue{\@IEEEeqnarraycolSEPdefaultstart}%
\edef\@IEEEBPendglue{\@IEEEeqnarraycolSEPdefaultend}%
\@@IEEEbuildpreamble}


\def\@@IEEEbuildpreamble#1#2{\let\@@nextIEEEbuildpreamble=\@@IEEEbuildpreamble%
\def\@IEEEgrabfirstoken##1##2\noindent{\let\@IEEEgrabbedfirstoken=##1}%
\@IEEEgrabfirstoken#1\relax\relax\noindent
\ifx\@IEEEgrabbedfirstoken\end\let\@@nextIEEEbuildpreamble=\@@IEEEfinishpreamble\else%
\@IEEEgetcoltype{#1}{\@IEEEBPcurtype}{1}% current, error on invalid
\@IEEEgetcoltype{#2}{\@IEEEBPnexttype}{0}% next, no error on invalid next
\if\@IEEEBPcurtype g\@IEEEgetcurglue{#1}{\@IEEEBPcurglue}\fi%
\if\@IEEEBPcurtype c\@IEEEgetcurcol{#1}\fi%
\if\@IEEEBPcurtype n\@IEEEprocessNcol{#1}\fi%
\if\@IEEEBPcurtype g\@IEEEprocessGcol\fi%
\if\@IEEEBPcurtype c\@IEEEprocessCcol\fi%
\let\@IEEEBPprevtype=\@IEEEBPcurtype%
\fi\@@nextIEEEbuildpreamble{#2}}


\def\@@IEEEfinishpreamble#1{\ifnum\@IEEEeqnnumcols<1\relax
\@IEEEclspkgerror{No column specifiers declared for IEEEeqnarray}%
{At least one column type must be declared for each IEEEeqnarray.}%
\fi%num cols less than 1
\if\@IEEEBPprevtype u\@IEEEappendtoksA{\tabskip=\@IEEEBPendglue}\fi}


\def\@IEEEgetcoltype#1#2#3{%
\def\@IEEEgrabfirstoken##1##2\noindent{\let\@IEEEgrabbedfirstoken=##1}%
\@IEEEgrabfirstoken#1\relax\relax\noindent
\let#2=u\relax% assume invalid until know otherwise
\ifx\@IEEEgrabbedfirstoken\end\let#2=e\else
\ifcat\@IEEEgrabbedfirstoken\relax\else% screen out control sequences
\if0\@IEEEgrabbedfirstoken\let#2=n\else
\if1\@IEEEgrabbedfirstoken\let#2=n\else
\if2\@IEEEgrabbedfirstoken\let#2=n\else
\if3\@IEEEgrabbedfirstoken\let#2=n\else
\if4\@IEEEgrabbedfirstoken\let#2=n\else
\if5\@IEEEgrabbedfirstoken\let#2=n\else
\if6\@IEEEgrabbedfirstoken\let#2=n\else
\if7\@IEEEgrabbedfirstoken\let#2=n\else
\if8\@IEEEgrabbedfirstoken\let#2=n\else
\if9\@IEEEgrabbedfirstoken\let#2=n\else
\ifcat,\@IEEEgrabbedfirstoken\let#2=g\relax
\else\ifcat a\@IEEEgrabbedfirstoken\let#2=c\relax\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi
\if#2u\relax
\if0\noexpand#3\relax\else\@IEEEclspkgerror{Invalid character in column specifications}%
{Only letters, numerals and certain other symbols are allowed \MessageBreak
as IEEEeqnarray column specifiers.}\fi\fi}


\def\@IEEEgetcurcol#1{\expandafter\ifx\csname @IEEEeqnarraycolDEF#1\endcsname\@IEEEeqnarraycolisdefined%
\def\@IEEEBPcurcolname{#1}\else% invalid column name
\@IEEEclspkgerror{Invalid column type "#1" in column specifications.\MessageBreak
Using a default centering column instead}%
{You must define IEEEeqnarray column types before use.}%
\def\@IEEEBPcurcolname{@IEEEdefault}\fi}


\def\@IEEEgetcurglue#1#2{%
\def\@IEEEgrabfirstoken##1##2\noindent{\let\@IEEEgrabbedfirstoken=##1}%
\@IEEEgrabfirstoken#1\relax\relax\noindent
{\setbox0=\hbox{$\displaystyle\relax$}}%
\@IEEEtrantmpdimenA=\fontdimen6\textfont2\relax%
\if!\@IEEEgrabbedfirstoken\@IEEEtrantmpdimenA=-0.16667\@IEEEtrantmpdimenA\edef#2{\the\@IEEEtrantmpdimenA}\else
\if,\@IEEEgrabbedfirstoken\@IEEEtrantmpdimenA=0.16667\@IEEEtrantmpdimenA\edef#2{\the\@IEEEtrantmpdimenA}\else
\if:\@IEEEgrabbedfirstoken\@IEEEtrantmpdimenA=0.22222\@IEEEtrantmpdimenA\edef#2{\the\@IEEEtrantmpdimenA}\else
\if;\@IEEEgrabbedfirstoken\@IEEEtrantmpdimenA=0.27778\@IEEEtrantmpdimenA\edef#2{\the\@IEEEtrantmpdimenA}\else
\if'\@IEEEgrabbedfirstoken\@IEEEtrantmpdimenA=1\@IEEEtrantmpdimenA\edef#2{\the\@IEEEtrantmpdimenA}\else
\if"\@IEEEgrabbedfirstoken\@IEEEtrantmpdimenA=2\@IEEEtrantmpdimenA\edef#2{\the\@IEEEtrantmpdimenA}\else
\if.\@IEEEgrabbedfirstoken\@IEEEtrantmpdimenA=0.5\arraycolsep\edef#2{\the\@IEEEtrantmpdimenA}\else
\if/\@IEEEgrabbedfirstoken\edef#2{\the\arraycolsep}\else
\if?\@IEEEgrabbedfirstoken\@IEEEtrantmpdimenA=2\arraycolsep\edef#2{\the\@IEEEtrantmpdimenA}\else
\if *\@IEEEgrabbedfirstoken\edef#2{0pt plus 1fil minus 0pt}\else
\if+\@IEEEgrabbedfirstoken\edef#2{\@IEEEeqnarraycolSEPcenter}\else
\if-\@IEEEgrabbedfirstoken\edef#2{\@IEEEeqnarraycolSEPzero}\else
\edef#2{\@IEEEeqnarraycolSEPzero}%
\@IEEEclspkgerror{Invalid predefined inter-column glue type "#1" in\MessageBreak
column specifications. Using a default value of\MessageBreak
0pt instead}%
{Only !,:;'"./?*+ and - are valid predefined glue types in the\MessageBreak 
IEEEeqnarray column specifications.}\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi\fi}



\def\@IEEEprocessNcol#1{\if\@IEEEBPprevtype g%
\@IEEEclspkgerror{Back-to-back inter-column glue specifiers in column\MessageBreak
specifications. Ignoring consecutive glue specifiers\MessageBreak
after the first}%
{You cannot have two or more glue types next to each other\MessageBreak 
in the IEEEeqnarray column specifications.}%
\let\@IEEEBPcurtype=a% abort this glue, future digits will be discarded
\@IEEEBPcurnum=0\relax%
\else% if we previously aborted a glue
\if\@IEEEBPprevtype a\@IEEEBPcurnum=0\let\@IEEEBPcurtype=a%maintain digit abortion
\else%acquire this number
\if\@IEEEBPprevtype n\else\let\@IEEEBPprevsavedtype=\@IEEEBPprevtype\fi%
\multiply\@IEEEBPcurnum by 10\relax%
\advance\@IEEEBPcurnum by #1\relax% add in number, \relax is needed to stop TeX's number scan
\if\@IEEEBPnexttype n\else%close acquisition
\expandafter\ifx\csname @IEEEeqnarraycolSEPDEF\expandafter\romannumeral\number\@IEEEBPcurnum\endcsname\@IEEEeqnarraycolisdefined%
\edef\@IEEEBPcurglue{\csname @IEEEeqnarraycolSEP\expandafter\romannumeral\number\@IEEEBPcurnum\endcsname}%
\else%user glue not defined
\@IEEEclspkgerror{Invalid user defined inter-column glue type "\number\@IEEEBPcurnum" in\MessageBreak
column specifications. Using a default value of\MessageBreak
0pt instead}%
{You must define all IEEEeqnarray numerical inter-column glue types via\MessageBreak
\string\IEEEeqnarraydefcolsep \space before they are used in column specifications.}%
\edef\@IEEEBPcurglue{\@IEEEeqnarraycolSEPzero}%
\fi% glue defined or not
\let\@IEEEBPcurtype=g% change the type to reflect the acquired glue
\let\@IEEEBPprevtype=\@IEEEBPprevsavedtype% restore the prev type before this number glue
\@IEEEBPcurnum=0\relax%ready for next acquisition
\fi%close acquisition, get glue
\fi%discard or acquire number
\fi%prevtype glue or not
}


\def\@IEEEprocessGcol{\if\@IEEEBPprevtype a\let\@IEEEBPcurtype=a%maintain previous glue abortions
\else
\if\@IEEEBPprevtype s\edef\@IEEEBPstartglue{\@IEEEBPcurglue}%
\else%not the start glue
\if\@IEEEBPprevtype g%ignore if back to back glues
\@IEEEclspkgerror{Back-to-back inter-column glue specifiers in column\MessageBreak
specifications. Ignoring consecutive glue specifiers\MessageBreak
after the first}%
{You cannot have two or more glue types next to each other\MessageBreak 
in the IEEEeqnarray column specifications.}%
\let\@IEEEBPcurtype=a% abort this glue
\else% not a back to back glue
\if\@IEEEBPprevtype c\relax% if the previoustype was a col, add column/glue pair to preamble
\ifnum\@IEEEeqnnumcols>0\relax\@IEEEappendtoksA{&}\fi
\toks0={##}%
\if@advanceIEEEeqncolcnt\@IEEEappendtoksA{\global\advance\@IEEEeqncolcnt by 1\relax}\fi
\@IEEEappendtoksA{\tabskip=\@IEEEBPcurglue}%
\@IEEEappendNOEXPANDtoksA{\begingroup\csname @IEEEeqnarraycolPRE}%
\@IEEEappendtoksA{\@IEEEBPcurcolname}%
\@IEEEappendNOEXPANDtoksA{\endcsname}%
\@IEEEappendtoksA{\the\toks0}%
\@IEEEappendNOEXPANDtoksA{\relax\relax\relax\relax\relax%
\relax\relax\relax\relax\relax\csname @IEEEeqnarraycolPOST}%
\@IEEEappendtoksA{\@IEEEBPcurcolname}%
\@IEEEappendNOEXPANDtoksA{\endcsname\relax\relax\relax\relax\relax%
\relax\relax\relax\relax\relax\endgroup}%
\advance\@IEEEeqnnumcols by 1\relax%one more column in the preamble
\else% error: non-start glue with no pending column
\@IEEEclspkgerror{Inter-column glue specifier without a prior column\MessageBreak
type in the column specifications. Ignoring this glue\MessageBreak 
specifier}%
{Except for the first and last positions, glue can be placed only\MessageBreak
between column types.}%
\let\@IEEEBPcurtype=a% abort this glue
\fi% previous was a column
\fi% back-to-back glues
\fi% is start column glue
\fi% prev type not a
}


\def\@IEEEprocessCcol{\if\@IEEEBPnexttype g\else
\if\@IEEEBPnexttype n\else
\ifnum\@IEEEeqnnumcols>0\relax\@IEEEappendtoksA{&}\fi%col separator for those after the first
\if\@IEEEBPnexttype e\@IEEEappendtoksA{\tabskip=\@IEEEBPendglue\relax}\else%put in end glue
\@IEEEappendtoksA{\tabskip=\@IEEEeqnarraycolSEPdefaultmid\relax}\fi% or default mid glue
\toks0={##}%
\if@advanceIEEEeqncolcnt\@IEEEappendtoksA{\global\advance\@IEEEeqncolcnt by 1\relax}\fi
\@IEEEappendNOEXPANDtoksA{\begingroup\csname @IEEEeqnarraycolPRE}%
\@IEEEappendtoksA{\@IEEEBPcurcolname}%
\@IEEEappendNOEXPANDtoksA{\endcsname}%
\@IEEEappendtoksA{\the\toks0}%
\@IEEEappendNOEXPANDtoksA{\relax\relax\relax\relax\relax%
\relax\relax\relax\relax\relax\csname @IEEEeqnarraycolPOST}%
\@IEEEappendtoksA{\@IEEEBPcurcolname}%
\@IEEEappendNOEXPANDtoksA{\endcsname\relax\relax\relax\relax\relax%
\relax\relax\relax\relax\relax\endgroup}%
\advance\@IEEEeqnnumcols by 1\relax%one more column in the preamble
\fi%next type not numeral
\fi%next type not glue
}






\if@twoside
 \ifCLASSOPTIONtechnote
   \def\ps@headings{%
       \def\@oddhead{\hbox{}\scriptsize\leftmark \hfil \thepage}
       \def\@evenhead{\scriptsize\thepage \hfil \leftmark\hbox{}}
       \ifCLASSOPTIONdraftcls
            \ifCLASSOPTIONdraftclsnofoot
               \def\@oddfoot{}\def\@evenfoot{}%
            \else
               \def\@oddfoot{\scriptsize\@date\hfil DRAFT}
               \def\@evenfoot{\scriptsize DRAFT\hfil\@date}
            \fi
       \else
            \def\@oddfoot{}\def\@evenfoot{}
       \fi}
 \else % not a technote
   \def\ps@headings{%
       \ifCLASSOPTIONconference
        \def\@oddhead{}
        \def\@evenhead{}
       \else
        \def\@oddhead{\hbox{}\scriptsize\rightmark \hfil \thepage}
        \def\@evenhead{\scriptsize\thepage \hfil \leftmark\hbox{}}
       \fi
       \ifCLASSOPTIONdraftcls
            \def\@oddhead{\hbox{}\scriptsize\rightmark \hfil \thepage}
            \def\@evenhead{\scriptsize\thepage \hfil \leftmark\hbox{}}
            \ifCLASSOPTIONdraftclsnofoot
               \def\@oddfoot{}\def\@evenfoot{}%
            \else
               \def\@oddfoot{\scriptsize\@date\hfil DRAFT}
               \def\@evenfoot{\scriptsize DRAFT\hfil\@date}
            \fi
       \else
            \def\@oddfoot{}\def\@evenfoot{}%
       \fi}
 \fi
\else % single side
\def\ps@headings{%
    \ifCLASSOPTIONconference
     \def\@oddhead{}
     \def\@evenhead{}
    \else
     \def\@oddhead{\hbox{}\scriptsize\leftmark \hfil \thepage}
     \def\@evenhead{}
    \fi
    \ifCLASSOPTIONdraftcls
          \def\@oddhead{\hbox{}\scriptsize\leftmark \hfil \thepage}
          \def\@evenhead{}
          \ifCLASSOPTIONdraftclsnofoot
             \def\@oddfoot{}
          \else
             \def\@oddfoot{\scriptsize \@date \hfil DRAFT}
          \fi
    \else
         \def\@oddfoot{}
    \fi
    \def\@evenfoot{}}
\fi


\def\ps@IEEEtitlepagestyle{\def\@oddfoot{}\def\@evenfoot{}%
\ifCLASSOPTIONconference
   \def\@oddhead{}%
   \def\@evenhead{}%
\else
   \def\@oddhead{\hbox{}\scriptsize\leftmark \hfil \thepage}%
   \def\@evenhead{\scriptsize\thepage \hfil \leftmark\hbox{}}%
\fi
\ifCLASSOPTIONdraftcls
   \def\@oddhead{\hbox{}\scriptsize\leftmark \hfil \thepage}%
   \def\@evenhead{\scriptsize\thepage \hfil \leftmark\hbox{}}%
   \ifCLASSOPTIONdraftclsnofoot\else
      \def\@oddfoot{\scriptsize \@date\hfil DRAFT}%
      \def\@evenfoot{\scriptsize DRAFT\hfil \@date}%
   \fi
\else
   \if@IEEEusingpubid
      \ifCLASSOPTIONpeerreview
      \else
         \footskip 0pt%
         \ifCLASSOPTIONcompsoc
           \def\@oddfoot{\hss\normalfont\scriptsize\raisebox{-1.5\@IEEEnormalsizeunitybaselineskip}[0ex][0ex]{\@IEEEpubid}\hss}%
           \def\@evenfoot{\hss\normalfont\scriptsize\raisebox{-1.5\@IEEEnormalsizeunitybaselineskip}[0ex][0ex]{\@IEEEpubid}\hss}%
         \else
           \def\@oddfoot{\hss\normalfont\footnotesize\raisebox{1.5ex}[1.5ex]{\@IEEEpubid}\hss}%
           \def\@evenfoot{\hss\normalfont\footnotesize\raisebox{1.5ex}[1.5ex]{\@IEEEpubid}\hss}%
         \fi
      \fi
   \fi
\fi}


\def\ps@IEEEpeerreviewcoverpagestyle{%
\def\@oddhead{}\def\@evenhead{}%
\def\@oddfoot{}\def\@evenfoot{}%
\ifCLASSOPTIONdraftcls
   \ifCLASSOPTIONdraftclsnofoot\else
      \def\@oddfoot{\scriptsize \@date\hfil DRAFT}%
      \def\@evenfoot{\scriptsize DRAFT\hfil \@date}%
   \fi
\else
   \if@IEEEusingpubid
      \footskip 0pt%
      \ifCLASSOPTIONcompsoc
        \def\@oddfoot{\hss\normalfont\scriptsize\raisebox{-1.5\@IEEEnormalsizeunitybaselineskip}[0ex][0ex]{\@IEEEpubid}\hss}%
        \def\@evenfoot{\hss\normalfont\scriptsize\raisebox{-1.5\@IEEEnormalsizeunitybaselineskip}[0ex][0ex]{\@IEEEpubid}\hss}%
      \else
        \def\@oddfoot{\hss\normalfont\footnotesize\raisebox{1.5ex}[1.5ex]{\@IEEEpubid}\hss}%
        \def\@evenfoot{\hss\normalfont\footnotesize\raisebox{1.5ex}[1.5ex]{\@IEEEpubid}\hss}%
      \fi
   \fi
\fi}


\def\rightmark{}\def\leftmark{}


\def\markboth#1#2{\def\leftmark{\@IEEEcompsoconly{\sffamily}\MakeUppercase{\protect#1}}%
\def\rightmark{\@IEEEcompsoconly{\sffamily}\MakeUppercase{\protect#2}}}
\def\footernote#1{\markboth{#1}{#1}}

\def\today{\ifcase\month\or
    January\or February\or March\or April\or May\or June\or
    July\or August\or September\or October\or November\or December\fi
    \space\number\day, \number\year}




\def\@citex[#1]#2{%
  \let\@citea\@empty
  \@cite{\@for\@citeb:=#2\do
    {\@citea\def\@citea{], [}%
     \edef\@citeb{\expandafter\@firstofone\@citeb\@empty}%
     \if@filesw\immediate\write\@auxout{\string\citation{\@citeb}}\fi
     \@ifundefined{b@\@citeb}{\mbox{\reset@font\bfseries ?}%
       \G@refundefinedtrue
       \@latex@warning
         {Citation `\@citeb' on page \thepage \space undefined}}%
       {\hbox{\csname b@\@citeb\endcsname}}}}{#1}}

\def\citepunct{], [}
\def\citedash{]--[}

\AtBeginDocument{\csname url@samestyle\endcsname}

\def\newblock{\hskip .11em\@plus.33em\@minus.07em}
\let\@openbib@code\@empty


\def\bstctlcite{\@ifnextchar[{\@bstctlcite}{\@bstctlcite[@auxout]}}
\def\@bstctlcite[#1]#2{\@bsphack
  \@for\@citeb:=#2\do{%
    \edef\@citeb{\expandafter\@firstofone\@citeb}%
    \if@filesw\immediate\write\csname #1\endcsname{\string\citation{\@citeb}}\fi}%
  \@esphack}

\edef\@IEEEtriggerrefnum{0}   % the default of zero means that
\def\@IEEEtriggercmd{\newpage}

\long\def\IEEEtriggercmd#1{\long\def\@IEEEtriggercmd{#1}}

\def\IEEEtriggeratref#1{\@IEEEtrantmpcountA=#1%
\edef\@IEEEtriggerrefnum{\the\@IEEEtrantmpcountA}}%

\def\@IEEEbibitemprefix{\@IEEEtrantmpcountA=\@IEEEtriggerrefnum\relax%
\advance\@IEEEtrantmpcountA by -1\relax%
\ifnum\c@enumiv=\@IEEEtrantmpcountA\relax\@IEEEtriggercmd\relax\fi}


\def\@biblabel#1{[#1]}

\@IEEEcompsocnotconfonly{\def\@biblabel#1{[#1]\hfill}}

\def\IEEEbibitemsep{0pt plus .5pt}

\@IEEEcompsocconfonly{\def\IEEEbibitemsep{1\baselineskip plus 0.25\baselineskip minus 0.25\baselineskip}}


\def\thebibliography#1{\section*{\refname}%
    \addcontentsline{toc}{section}{\refname}%
    \footnotesize\@IEEEcompsocconfonly{\small}\vskip 0.3\baselineskip plus 0.1\baselineskip minus 0.1\baselineskip%
    \list{\@biblabel{\@arabic\c@enumiv}}%
    {\settowidth\labelwidth{\@biblabel{#1}}%
    \leftmargin\labelwidth
    \advance\leftmargin\labelsep\relax
    \itemsep \IEEEbibitemsep\relax
    \usecounter{enumiv}%
    \let\p@enumiv\@empty
    \renewcommand\theenumiv{\@arabic\c@enumiv}}%
    \let\@IEEElatexbibitem\bibitem%
    \def\bibitem{\@IEEEbibitemprefix\@IEEElatexbibitem}%
\def\newblock{\hskip .11em plus .33em minus .07em}%
\ifCLASSOPTIONtechnote\sloppy\clubpenalty4000\widowpenalty4000\interlinepenalty100%
\else\sloppy\clubpenalty4000\widowpenalty4000\interlinepenalty500\fi%
    \sfcode`\.=1000\relax}
\let\endthebibliography=\endlist




\def\IEEEmembership#1{{\@IEEEnotcompsoconly{\sublargesize}\normalfont\@IEEEcompsoconly{\sffamily}\textit{#1}}}
 

\ifCLASSOPTIONtransmag
\DeclareRobustCommand*{\IEEEauthorrefmark}[1]{\raisebox{0pt}[0pt][0pt]{\textsuperscript{\footnotesize #1}}}
\else
\DeclareRobustCommand*{\IEEEauthorrefmark}[1]{\raisebox{0pt}[0pt][0pt]{\textsuperscript{\footnotesize\ensuremath{\ifcase#1\or *\or \dagger\or \ddagger\or%
    \mathsection\or \mathparagraph\or \|\or **\or \dagger\dagger%
    \or \ddagger\ddagger \else\textsuperscript{\expandafter\romannumeral#1}\fi}}}}
\fi


\def\@IEEEauthorblockNstyle{\normalfont\@IEEEcompsocnotconfonly{\sffamily}\sublargesize\@IEEEcompsocconfonly{\large}}
\def\@IEEEauthorblockAstyle{\normalfont\@IEEEcompsocnotconfonly{\sffamily}\@IEEEcompsocconfonly{\itshape}\normalsize\@IEEEcompsocconfonly{\large}}
\def\@IEEEauthordefaulttextstyle{\normalfont\@IEEEcompsocnotconfonly{\sffamily}\sublargesize}

\def\@IEEEauthorblockconfadjspace{-0.25em}
\@IEEEcompsocconfonly{\def\@IEEEauthorblockconfadjspace{0.75\@IEEEnormalsizeunitybaselineskip}}

\def\@IEEEauthorblockNtopspace{0.0ex}
\def\@IEEEauthorblockAtopspace{0.0ex}
\ifCLASSOPTIONtransmag
\def\@IEEEauthorblockAtopspace{1\@IEEEnormalsizeunitybaselineskip}
\fi

\def\@IEEEauthorblockNinterlinespace{2.6ex}
\def\@IEEEauthorblockAinterlinespace{2.75ex}

\def\@IEEEauthorblockXinterlinespace{2.7ex}

\gdef\@IEEESAVESTATEfontsize{10}
\gdef\@IEEESAVESTATEfontbaselineskip{12}
\gdef\@IEEESAVESTATEfontencoding{OT1}
\gdef\@IEEESAVESTATEfontfamily{ptm}
\gdef\@IEEESAVESTATEfontseries{m}
\gdef\@IEEESAVESTATEfontshape{n}

\def\@IEEEcurfontSAVE{\global\let\@IEEESAVESTATEfontsize\f@size%
\global\let\@IEEESAVESTATEfontbaselineskip\f@baselineskip%
\global\let\@IEEESAVESTATEfontencoding\f@encoding%
\global\let\@IEEESAVESTATEfontfamily\f@family%
\global\let\@IEEESAVESTATEfontseries\f@series%
\global\let\@IEEESAVESTATEfontshape\f@shape}

\def\@IEEEcurfontRESTORE{\fontsize{\@IEEESAVESTATEfontsize}{\@IEEESAVESTATEfontbaselineskip}%
\fontencoding{\@IEEESAVESTATEfontencoding}%
\fontfamily{\@IEEESAVESTATEfontfamily}%
\fontseries{\@IEEESAVESTATEfontseries}%
\fontshape{\@IEEESAVESTATEfontshape}%
\selectfont}


\newif\if@IEEEprevauthorblockincol   \@IEEEprevauthorblockincolfalse


\def\@IEEEauthorstrutrule{\@IEEEtrantmpdimenA\@IEEEauthorblockXinterlinespace%
\@IEEEtrantmpdimenA=\baselinestretch\@IEEEtrantmpdimenA%
\rule[-0.3\@IEEEtrantmpdimenA]{0pt}{\@IEEEtrantmpdimenA}}


\def\IEEEauthorblockN#1{\relax\@IEEEauthorblockNstyle% set the default text style
\gdef\@IEEEauthorblockXinterlinespace{0pt}% disable strut for spacer row
\if@IEEEprevauthorblockincol\expandafter\@IEEEauthorblockNtopspaceline\fi
\global\@IEEEprevauthorblockincoltrue% we now have a block in this column
\gdef\@IEEEauthorblockXinterlinespace{\@IEEEauthorblockNinterlinespace}%
#1%
\crcr}
\def\@IEEEauthorblockNtopspaceline{\cr\noalign{\vskip\@IEEEauthorblockNtopspace}}
\def\IEEEauthorblockA#1{\relax\@IEEEauthorblockAstyle% set the default text style
\gdef\@IEEEauthorblockXinterlinespace{0pt}%disable strut for spacer row
\if@IEEEprevauthorblockincol\expandafter\@IEEEauthorblockAtopspaceline\fi
\global\@IEEEprevauthorblockincoltrue% we now have a block in this column
\gdef\@IEEEauthorblockXinterlinespace{\@IEEEauthorblockAinterlinespace}%
#1%
\crcr
\ifCLASSOPTIONtransmag\gdef\@IEEEauthorblockAtopspace{0pt}\fi}

\def\@IEEEauthorblockAtopspaceline{\cr\noalign{\vskip\@IEEEauthorblockAtopspace}}


\ifCLASSOPTIONconference
\else
   \ifCLASSOPTIONpeerreviewca\else
      \ifCLASSOPTIONtransmag\else
         \def\IEEEauthorblockN#1{#1}%
         \def\IEEEauthorblockA#1{#1}%
      \fi
   \fi
\fi



\def\@IEEEauthorhalign{\@IEEEauthordefaulttextstyle% default text style
   \lineskip=0pt\relax% disable line spacing
   \lineskiplimit=0pt\relax%
   \baselineskip=0pt\relax%
   \@IEEEcurfontSAVE% save the current font
   \mathsurround\z@\relax% no extra spacing around math
   \let\\\@IEEEauthorhaligncr% replace newline with halign friendly one
   \tabskip=0pt\relax% no column spacing
   \everycr{}% ensure no problems here
   \@IEEEprevauthorblockincolfalse% no author blocks yet
   \def\@IEEEauthorblockXinterlinespace{2.7ex}% default interline space
   \vtop\bgroup%vtop box
   \halign\bgroup&\relax\hfil\@IEEEcurfontRESTORE\relax ##\relax
   \hfil\@IEEEcurfontSAVE\@IEEEauthorstrutrule\cr}

\def\end@IEEEauthorhalign{\crcr\egroup\egroup}

\def\@IEEEauthorhaligncr{{\ifnum0=`}\fi\@ifstar{\@@IEEEauthorhaligncr}{\@@IEEEauthorhaligncr}}

\def\@@IEEEauthorhaligncr{\@testopt\@@@IEEEauthorhaligncr\z@skip}

\def\@@@IEEEauthorhaligncr[#1]{\ifnum0=`{\fi}\cr\noalign{\vskip#1\relax}}



\newif\if@IEEEWARNand
\@IEEEWARNandtrue

\def\and{\relax} % provide a bogus \and that we will then override

\renewcommand{\and}[1][\relax]{\if@IEEEWARNand\typeout{** WARNING: \noexpand\and is valid only
                               when in conference or peerreviewca}\typeout{modes (line \the\inputlineno).}\fi\global\@IEEEWARNandfalse}

\ifCLASSOPTIONconference%
\renewcommand{\and}[1][\hfill]{\end{@IEEEauthorhalign}#1\begin{@IEEEauthorhalign}}%
\fi
\ifCLASSOPTIONpeerreviewca
\renewcommand{\and}[1][\hfill]{\end{@IEEEauthorhalign}#1\begin{@IEEEauthorhalign}}%
\fi
\ifCLASSOPTIONtransmag
\renewcommand{\and}[1][\hfill]{\end{@IEEEauthorhalign}#1\begin{@IEEEauthorhalign}}%
\fi

\def\@IEEEcleardoublepage#1{\clearpage\if@twoside\ifodd\c@page\else
\hbox{}\thispagestyle{#1}\newpage\if@twocolumn\hbox{}\thispagestyle{#1}\newpage\fi\fi\fi}


\def\maketitle{\par%
  \begingroup%
  \normalfont%
  \def\thefootnote{}%  the \thanks{} mark type is empty
  \def\footnotemark{}% and kill space from \thanks within author
  \let\@makefnmark\relax% V1.7, must *really* kill footnotemark to remove all \textsuperscript spacing as well.
  \footnotesize%       equal spacing between thanks lines
  \footnotesep 0.7\baselineskip%see global setting of \footnotesep for more info
  \@IEEEcompsoconly{\long\def\@makefntext##1{\parindent 1em\noindent\hbox{\@makefnmark}##1}}%
  \normalsize%
  \ifCLASSOPTIONpeerreview
     \newpage\global\@topnum\z@ \@maketitle\@IEEEstatictitlevskip\@IEEEaftertitletext%
     \thispagestyle{IEEEpeerreviewcoverpagestyle}\@thanks%
  \else
     \if@twocolumn%
        \ifCLASSOPTIONtechnote%
           \newpage\global\@topnum\z@ \@maketitle\@IEEEstatictitlevskip\@IEEEaftertitletext%
        \else
           \twocolumn[\@maketitle\@IEEEdynamictitlevspace\@IEEEaftertitletext]%
        \fi
     \else
        \newpage\global\@topnum\z@ \@maketitle\@IEEEstatictitlevskip\@IEEEaftertitletext%
     \fi
     \thispagestyle{IEEEtitlepagestyle}\@thanks%
  \fi
  \if@IEEEusingpubid
     \enlargethispage{-\@IEEEpubidpullup}%
  \fi 
  \endgroup
  \setcounter{footnote}{0}\let\maketitle\relax\let\@maketitle\relax
  \gdef\@thanks{}%
  \let\thanks\relax}



\long\def\@IEEEtitleabstractindextextbox#1{\parbox{1\textwidth}{#1}}
\ifCLASSOPTIONcompsoc
\long\def\@IEEEtitleabstractindextextbox#1{\parbox{0.915\textwidth}{#1}}
\fi

\def\@maketitle{\newpage
\bgroup\par\addvspace{0.5\baselineskip}\centering%
\ifCLASSOPTIONtechnote% technotes
   {\bfseries\large\@IEEEcompsoconly{\sffamily}\@title\par}\vskip 1.3em{\lineskip .5em\@IEEEcompsoconly{\sffamily}\@author
   \@IEEEspecialpapernotice\par{\@IEEEcompsoconly{\vskip 1.5em\relax
   \@IEEEtitleabstractindextextbox{\@IEEEtitleabstractindextext}\par
   \hfill\@IEEEcompsocdiamondline\hfill\hbox{}\par}}}\relax
\else% not a technote
   \vskip0.2em{\Huge\ifCLASSOPTIONtransmag\bfseries\LARGE\fi\@IEEEcompsoconly{\sffamily}\@IEEEcompsocconfonly{\normalfont\normalsize\vskip 2\@IEEEnormalsizeunitybaselineskip
   \bfseries\Large}\@title\par}\vskip1.0em\par%
   \ifCLASSOPTIONconference%
      {\@IEEEspecialpapernotice\mbox{}\vskip\@IEEEauthorblockconfadjspace%
       \mbox{}\hfill\begin{@IEEEauthorhalign}\@author\end{@IEEEauthorhalign}\hfill\mbox{}\par}\relax
   \else% peerreviewca, peerreview or journal
      \ifCLASSOPTIONpeerreviewca
         {\@IEEEcompsoconly{\sffamily}\@IEEEspecialpapernotice\mbox{}\vskip\@IEEEauthorblockconfadjspace%
          \mbox{}\hfill\begin{@IEEEauthorhalign}\@author\end{@IEEEauthorhalign}\hfill\mbox{}\par
          {\@IEEEcompsoconly{\vskip 1.5em\relax
           \@IEEEtitleabstractindextextbox{\@IEEEtitleabstractindextext}\par\hfill
           \@IEEEcompsocdiamondline\hfill\hbox{}\par}}}\relax
      \else% journal, peerreview or transmag
         \ifCLASSOPTIONtransmag
           {\@IEEEspecialpapernotice\mbox{}\vskip\@IEEEauthorblockconfadjspace%
            \mbox{}\hfill\begin{@IEEEauthorhalign}\@author\end{@IEEEauthorhalign}\hfill\mbox{}\par
           {\vspace{0.5\baselineskip}\relax\@IEEEtitleabstractindextextbox{\@IEEEtitleabstractindextext}\vspace{-1\baselineskip}\par}}\relax
         \else% journal or peerreview
           {\lineskip.5em\@IEEEcompsoconly{\sffamily}\sublargesize\@author\@IEEEspecialpapernotice\par
           {\@IEEEcompsoconly{\vskip 1.5em\relax
            \@IEEEtitleabstractindextextbox{\@IEEEtitleabstractindextext}\par\hfill
            \@IEEEcompsocdiamondline\hfill\hbox{}\par}}}\relax
         \fi
      \fi
   \fi
\fi\par\addvspace{0.5\baselineskip}\egroup}



\def\@IEEEcompsocdiamondline{\vrule depth 0pt height 0.5pt width 4cm\hspace{7.5pt}%
\raisebox{-3.5pt}{\fontfamily{pzd}\fontencoding{U}\fontseries{m}\fontshape{n}\fontsize{11}{12}\selectfont\char70}%
\hspace{7.5pt}\vrule depth 0pt height 0.5pt width 4cm\relax}

\def\@IEEEtriggeroneshotfootnoterule{\global\@IEEEenableoneshotfootnoteruletrue}


\long\def\thanks#1{\footnotemark
    \protected@xdef\@thanks{\@thanks
        \protect\footnotetext[\the\c@footnote]{\@IEEEcompsoconly{\itshape
        \protect\@IEEEtriggeroneshotfootnoterule\relax}\ignorespaces#1}}}
\let\@thanks\@empty

\long\def\author#1{\gdef\@author{#1}}


\def\@IEEEsetupcompsocitemizelist{\def\labelitemi{$\bullet$}%
\setlength{\IEEElabelindent}{0pt}\setlength{\parskip}{0pt}%
\setlength{\partopsep}{0pt}\setlength{\topsep}{0.5\baselineskip}\vspace{-1\baselineskip}\relax}


\newif\if@IEEEbreakcompsocthanksitem \@IEEEbreakcompsocthanksitemfalse

\ifCLASSOPTIONcompsoc
\long\def\IEEEcompsocitemizethanks#1{\relax\@IEEEbreakcompsocthanksitemfalse\footnotemark
    \protected@xdef\@thanks{\@thanks
        \protect\footnotetext[\the\c@footnote]{\itshape\protect\@IEEEtriggeroneshotfootnoterule
        {\let\IEEEiedlistdecl\relax\protect\begin{IEEEitemize}[\protect\@IEEEsetupcompsocitemizelist]\ignorespaces#1\relax
        \protect\end{IEEEitemize}}\protect\vspace{-1\baselineskip}}}}
\DeclareRobustCommand*{\IEEEcompsocthanksitem}{\item}
\else
\long\def\IEEEcompsocitemizethanks#1{\thanks{#1}}
\DeclareRobustCommand{\IEEEcompsocthanksitem}{\@ifnextchar [{\@IEEEthanksswallowoptionalarg}%
{\@IEEEthanksswallowoptionalarg[\relax]}}
\def\@IEEEthanksswallowoptionalarg[#1]{\relax\if@IEEEbreakcompsocthanksitem\hfil\break
\indent\fi\@IEEEbreakcompsocthanksitemtrue\ignorespaces}
\fi


\ifCLASSOPTIONpeerreview
\def\IEEEpeerreviewmaketitle{\@IEEEcleardoublepage{empty}%
\ifCLASSOPTIONtwocolumn
\twocolumn[\@IEEEpeerreviewmaketitle\@IEEEdynamictitlevspace]
\else
\newpage\@IEEEpeerreviewmaketitle\@IEEEstatictitlevskip
\fi
\thispagestyle{IEEEtitlepagestyle}}
\else
\def\IEEEpeerreviewmaketitle{\relax}
\fi

\def\@IEEEpeerreviewmaketitle{\bgroup\par\addvspace{0.5\baselineskip}\centering\@IEEEcompsoconly{\sffamily}%
\normalfont\normalsize\vskip0.2em{\Huge\@title\par}\vskip1.0em\par
\par\addvspace{0.5\baselineskip}\egroup}



\def\@IEEEstatictitlevskip{{\normalfont\normalsize
\ifCLASSOPTIONpeerreview
   \vskip 1\baselineskip plus 0.375\baselineskip minus 0.1875\baselineskip
\else
   \ifCLASSOPTIONconference% conference
      \vskip 1\baselineskip plus 0.375\baselineskip minus 0.1875\baselineskip%
   \else%
      \ifCLASSOPTIONtechnote% technote
         \vskip 1\baselineskip plus 0.375\baselineskip minus 0.1875\baselineskip%
      \else% journal uses more space
         \vskip 2.5\baselineskip plus 0.75\baselineskip minus 0.375\baselineskip%
      \fi
   \fi
\fi}}


\def\@IEEEdynamictitlevspace{{%
    \long\def\thanks##1{\relax}%don't allow \thanks to run when we evaluate the vbox height
    \long\def\IEEEcompsocitemizethanks##1{\relax}%don't allow \IEEEcompsocitemizethanks to run when we evaluate the vbox height
    \normalfont\normalsize% we declare more descriptive variable names
    \let\@IEEEmaintextheight=\@IEEEtrantmpdimenA%height of the main text columns
    \let\@IEEEINTmaintextheight=\@IEEEtrantmpdimenB%height of the main text columns with integer # lines
    \def\@IEEENORMtitlevspace{2.5\baselineskip}%
    \def\@IEEEMINtitlevspace{2\baselineskip}%
    \ifCLASSOPTIONconference%conference
     \def\@IEEENORMtitlevspace{1\baselineskip}%
     \def\@IEEEMINtitlevspace{0.75\baselineskip}%
    \fi
    \ifCLASSOPTIONtechnote%technote
      \def\@IEEENORMtitlevspace{1\baselineskip}%
      \def\@IEEEMINtitlevspace{0.75\baselineskip}%
    \fi%
    \ifCLASSOPTIONpeerreview
       \settoheight{\@IEEEmaintextheight}{\vbox{\hsize\textwidth \@IEEEpeerreviewmaketitle}}%
    \else
       \settoheight{\@IEEEmaintextheight}{\vbox{\hsize\textwidth \@maketitle}}%
    \fi
    \@IEEEmaintextheight=-\@IEEEmaintextheight% title takes away from maintext, so reverse sign
    \advance\@IEEEmaintextheight by \textheight%
    \ifCLASSOPTIONpeerreview\else
       \if@IEEEusingpubid\advance\@IEEEmaintextheight by -\@IEEEpubidpullup\fi
    \fi%
    \advance\@IEEEmaintextheight by -\@IEEENORMtitlevspace%
    \advance\@IEEEmaintextheight by -\topskip%
    \@IEEEINTmaintextheight=\@IEEEmaintextheight%
    \divide\@IEEEINTmaintextheight  by \baselineskip%
    \multiply\@IEEEINTmaintextheight  by \baselineskip%
    \let\@IEEEREDUCEmaintextheight=\@IEEEINTmaintextheight%
    \advance\@IEEEREDUCEmaintextheight by -\@IEEEmaintextheight%
    \advance\@IEEEREDUCEmaintextheight by \baselineskip%
    \let\@IEEECOMPENSATElen=\@IEEEmaintextheight%
    \@IEEECOMPENSATElen=\@IEEENORMtitlevspace% set the nominal value
    \ifdim\@IEEEREDUCEmaintextheight < 0.5\baselineskip\relax%
     \advance\@IEEECOMPENSATElen by -\@IEEEREDUCEmaintextheight%
     \ifdim\@IEEECOMPENSATElen<\@IEEEMINtitlevspace\relax%
      \advance\@IEEECOMPENSATElen by \baselineskip%
     \fi%
    \else%
     \advance\@IEEECOMPENSATElen by -\@IEEEREDUCEmaintextheight%
     \advance\@IEEECOMPENSATElen by \baselineskip%
    \fi%
    \vspace{\@IEEECOMPENSATElen}}}



\let\@IEEEaftertitletext=\relax
\long\def\IEEEaftertitletext#1{\def\@IEEEaftertitletext{#1}}


\let\@IEEEtitleabstractindextext=\relax
\long\def\IEEEtitleabstractindextext#1{\def\@IEEEtitleabstractindextext{#1}}
\def\IEEEdisplaynontitleabstractindextext{%
\ifCLASSOPTIONconference\@IEEEtitleabstractindextext\relax
\else% non-conferences
  \ifCLASSOPTIONcompsoc% display if not compsoc and not transmag
  \else
    \ifCLASSOPTIONtransmag
    \else% not compsoc journal nor transmag journal
       \@IEEEtitleabstractindextext\relax
    \fi
  \fi
\fi}


\def\@IEEEtweakunitybaselinestretch#1{{\def\baselinestretch{1}\selectfont
\global\@tempskipa\baselineskip}\ifnum\@tempskipa=\baselineskip%
\def\baselinestretch{#1}\selectfont\fi\relax}


\def\@IEEEabskeysecsize{\small}
\ifx\CLASSOPTIONpt\@IEEEptsizenine
 \def\@IEEEabskeysecsize{\footnotesize}
\fi

\@IEEEcompsoconly{\def\@IEEEabskeysecsize{\footnotesize}}
\@IEEEcompsocconfonly{\def\@IEEEabskeysecsize{\normalsize}}




\def\abstract{\normalfont
    \if@twocolumn
      \@IEEEabskeysecsize\bfseries\textit{\abstractname}---\relax
    \else
      \bgroup\par\addvspace{0.5\baselineskip}\centering\vspace{-1.78ex}\@IEEEabskeysecsize\textbf{\abstractname}\par\addvspace{0.5\baselineskip}\egroup\quotation\@IEEEabskeysecsize
    \fi\@IEEEgobbleleadPARNLSP}
\def\endabstract{\relax\ifCLASSOPTIONconference\vspace{0ex}\else\vspace{1.34ex}\fi\par\if@twocolumn\else\endquotation\fi
    \normalfont\normalsize}

\def\IEEEkeywords{\normalfont
    \if@twocolumn
      \@IEEEabskeysecsize\bfseries\textit{\IEEEkeywordsname}---\relax
    \else
      \bgroup\par\addvspace{0.5\baselineskip}\centering\@IEEEabskeysecsize\textbf{\IEEEkeywordsname}\par\addvspace{0.5\baselineskip}\egroup\quotation\@IEEEabskeysecsize
    \fi\@IEEEgobbleleadPARNLSP}
\def\endIEEEkeywords{\relax\ifCLASSOPTIONtechnote\vspace{1.34ex}\else\vspace{0.67ex}\fi
    \par\if@twocolumn\else\endquotation\fi%
    \normalfont\normalsize}

\ifCLASSOPTIONcompsoc
  \ifCLASSOPTIONconference% compsoc conference
\def\abstract{\normalfont
      \bgroup\par\addvspace{0.5\baselineskip}\centering\@IEEEabskeysecsize\textbf{\large\abstractname}\par\addvspace{0.5\baselineskip}\egroup\vskip 0.5\baselineskip plus 0.1\baselineskip minus 0.1\baselineskip
      \if@twocolumn\else\quotation\fi\itshape\@IEEEabskeysecsize%
      \par\@IEEEgobbleleadPARNLSP}
\def\IEEEkeywords{\normalfont\vskip 1.5\baselineskip plus 0.25\baselineskip minus 0.25\baselineskip
      \bgroup\par\addvspace{0.5\baselineskip}\centering\@IEEEabskeysecsize\textbf{\large\IEEEkeywordsname}\par\addvspace{0.5\baselineskip}\egroup\vskip 0.5\baselineskip plus 0.1\baselineskip minus 0.1\baselineskip
      \if@twocolumn\else\quotation\fi\itshape\@IEEEabskeysecsize%
      \par\@IEEEgobbleleadPARNLSP}
  \else% compsoc not conference
\def\abstract{\normalfont\@IEEEtweakunitybaselinestretch{1.15}\sffamily
    \if@twocolumn
      \@IEEEabskeysecsize\noindent\textbf{\abstractname}---\relax
    \else
      \bgroup\par\addvspace{0.5\baselineskip}\centering\vspace{-1.78ex}\@IEEEabskeysecsize\textbf{\abstractname}\par\addvspace{0.5\baselineskip}\egroup\quotation\@IEEEabskeysecsize%
    \fi\@IEEEgobbleleadPARNLSP}
\def\IEEEkeywords{\normalfont\@IEEEtweakunitybaselinestretch{1.15}\sffamily
    \if@twocolumn
      \@IEEEabskeysecsize\vskip 0.5\baselineskip plus 0.25\baselineskip minus 0.25\baselineskip\noindent
      \textbf{\IEEEkeywordsname}---\relax
    \else
      \bgroup\par\addvspace{0.5\baselineskip}\centering\@IEEEabskeysecsize\textbf{\IEEEkeywordsname}\par\addvspace{0.5\baselineskip}\egroup\quotation\@IEEEabskeysecsize%
    \fi\@IEEEgobbleleadPARNLSP}
  \fi
\fi

\ifCLASSOPTIONtransmag
\def\abstract{\normalfont\parindent 1em\relax
    \if@twocolumn
      \@IEEEabskeysecsize\bfseries\indent
    \else
      \bgroup\par\addvspace{0.5\baselineskip}\centering\vspace{-1.78ex}\@IEEEabskeysecsize
      \textbf{\abstractname}\par\addvspace{0.5\baselineskip}\egroup\quotation\@IEEEabskeysecsize
    \fi\@IEEEgobbleleadPARNLSP}

\def\IEEEkeywords{\normalfont\parindent 1em\relax
    \if@twocolumn
      \@IEEEabskeysecsize\vspace{1\baselineskip}\bfseries\indent\textit{\IEEEkeywordsname}---\relax
    \else
      \bgroup\par\vspace{1\baselineskip}\centering\@IEEEabskeysecsize
      \textbf{\IEEEkeywordsname}\par\addvspace{0.5\baselineskip}\egroup\quotation\@IEEEabskeysecsize
    \fi\@IEEEgobbleleadPARNLSP}
\fi



\long\def\@IEEEgobbleleadPARNLSP#1{\let\@IEEEswallowthistoken=0%
\let\@IEEEgobbleleadPARNLSPtoken#1%
\let\@IEEEgobbleleadPARtoken=\par%
\let\@IEEEgobbleleadNLtoken=\\%
\let\@IEEEgobbleleadSPtoken=\ %
\def\@IEEEgobbleleadSPMACRO{\ }%
\ifx\@IEEEgobbleleadPARNLSPtoken\@IEEEgobbleleadPARtoken%
\let\@IEEEswallowthistoken=1%
\fi%
\ifx\@IEEEgobbleleadPARNLSPtoken\@IEEEgobbleleadNLtoken%
\let\@IEEEswallowthistoken=1%
\fi%
\ifx\@IEEEgobbleleadPARNLSPtoken\@IEEEgobbleleadSPtoken%
\let\@IEEEswallowthistoken=1%
\fi%
\ifx\@IEEEgobbleleadPARNLSPtoken\@IEEEgobbleleadSPMACRO%
\let\@IEEEswallowthistoken=1%
\fi%
\ifx\@IEEEswallowthistoken 1\let\@IEEEnextgobbleleadPARNLSP=\@IEEEgobbleleadPARNLSP\else%
\let\@IEEEnextgobbleleadPARNLSP=#1\fi%
\@IEEEnextgobbleleadPARNLSP}%




\def\@IEEEsectpunct{:\ \,}  % Punctuation after run-in section heading  (headings which are
\ifCLASSOPTIONcompsoc
\ifCLASSOPTIONconference
\def\@IEEEsectpunct{.\ }
\fi\fi


\def\@seccntformat#1{\csname the#1dis\endcsname\hskip 0.5em\relax}

\ifCLASSOPTIONcompsoc
\ifCLASSOPTIONconference\else
\def\@seccntformat#1{\csname the#1dis\endcsname\hskip 1em\relax}
\fi\fi

\def\@sect#1#2#3#4#5#6[#7]#8{%
  \ifnum #2>\c@secnumdepth
     \let\@svsec\@empty
  \else
     \refstepcounter{#1}%
     \protected@edef\@svsec{\@seccntformat{#1}\relax}%
  \fi%
  \@tempskipa #5\relax
  \ifdim \@tempskipa>\z@% tempskipa determines whether is treated as a high
     \begingroup #6{\relax% or low level heading
      \noindent % subsections are NOT indented
       {\hskip #3\relax\@svsec}{\interlinepenalty \@M #8\@@par}}%
     \endgroup
     \addcontentsline{toc}{#1}{\ifnum #2>\c@secnumdepth\relax\else
               \protect\numberline{\csname the#1\endcsname}\fi#7}%
  \else % printout low level headings
     \def\@svsechd{#6{\hskip #3\relax\@svsec #8\@IEEEsectpunct\mbox{}}%
     \addcontentsline{toc}{#1}{\ifnum #2>\c@secnumdepth\relax\else
               \protect\numberline{\csname the#1\endcsname}\fi#7}}%
  \fi%skip down
  \@xsect{#5}}


\def\@ssect#1#2#3#4#5{\@tempskipa #3\relax
  \ifdim \@tempskipa>\z@
     \begingroup \noindent #4{\relax{\hskip #1}{\interlinepenalty \@M #5\@@par}}\endgroup
  \else \def\@svsechd{#4{\hskip #1\relax #5\@IEEEsectpunct\mbox{}}}\fi
  \@xsect{#3}}


\ifCLASSOPTIONconference%
\def\section{\@startsection{section}{1}{\z@}{1.5ex plus 1.5ex minus 0.5ex}%
{0.7ex plus 1ex minus 0ex}{\normalfont\normalsize\centering\scshape}}%
\def\subsection{\@startsection{subsection}{2}{\z@}{1.5ex plus 1.5ex minus 0.5ex}%
{0.7ex plus .5ex minus 0ex}{\normalfont\normalsize\itshape}}%
\else % for journals
\def\section{\@startsection{section}{1}{\z@}{3.0ex plus 1.5ex minus 1.5ex}% V1.6 3.0ex from 3.5ex
{0.7ex plus 1ex minus 0ex}{\normalfont\normalsize\centering\scshape}}%
\def\subsection{\@startsection{subsection}{2}{\z@}{3.5ex plus 1.5ex minus 1.5ex}%
{0.7ex plus .5ex minus 0ex}{\normalfont\normalsize\itshape}}%
\fi

\def\subsubsection{\@startsection{subsubsection}{3}{\parindent}{0ex plus 0.1ex minus 0.1ex}%
{0ex}{\normalfont\normalsize\itshape}}%
\def\paragraph{\@startsection{paragraph}{4}{2\parindent}{0ex plus 0.1ex minus 0.1ex}%
{0ex}{\normalfont\normalsize\itshape}}%


\ifCLASSOPTIONcompsoc
\ifCLASSOPTIONconference
\def\section{\@startsection{section}{1}{\z@}{1\baselineskip plus 0.25\baselineskip minus 0.25\baselineskip}%
{1\baselineskip plus 0.25\baselineskip minus 0.25\baselineskip}{\normalfont\large\bfseries}}%
\def\subsection{\@startsection{subsection}{2}{\z@}{1\baselineskip plus 0.25\baselineskip minus 0.25\baselineskip}%
{1\baselineskip plus 0.25\baselineskip minus 0.25\baselineskip}{\normalfont\sublargesize\bfseries}}%
\def\subsubsection{\@startsection{subsubsection}{3}{\z@}{1\baselineskip plus 0.25\baselineskip minus 0.25\baselineskip}%
{0ex}{\normalfont\normalsize\bfseries}}%
\def\paragraph{\@startsection{paragraph}{4}{2\parindent}{0ex plus 0.1ex minus 0.1ex}%
{0ex}{\normalfont\normalsize}}%
\else% compsoc journals
\def\section{\@startsection{section}{1}{\z@}{-3ex plus -2ex minus -1.5ex}%
{0.7ex plus 1ex minus 0ex}{\normalfont\large\sffamily\bfseries\scshape}}%
\def\subsection{\@startsection{subsection}{2}{\z@}{-3.5ex plus -1.5ex minus -1.5ex}%
{0.7ex plus .5ex minus 0ex}{\normalfont\normalsize\sffamily\bfseries}}%
\def\subsubsection{\@startsection{subsubsection}{3}{\z@}{-2.5ex plus -1ex minus -1ex}%
{0.5ex plus 0.5ex minus 0ex}{\normalfont\normalsize\sffamily\itshape}}%
\def\paragraph{\@startsection{paragraph}{4}{2\parindent}{-0ex plus -0.1ex minus -0.1ex}%
{0ex}{\normalfont\normalsize}}%
\fi\fi

\ifCLASSOPTIONtransmag
\def\subsection{\@startsection{subsection}{2}{0.75\parindent}{3.5ex plus 1.5ex minus 1.5ex}%
{0.7ex plus .5ex minus 0ex}{\normalfont\normalsize\itshape}}%
\def\subsubsection{\@startsection{subsubsection}{3}{1.25\parindent}{0.1ex plus 0.1ex minus 0.1ex}%
{0.1ex}{\normalfont\normalsize\itshape}}%
\fi


\def\IEEEQEDclosed{\mbox{\rule[0pt]{1.3ex}{1.3ex}}} % for a filled box
\def\IEEEQEDopen{{\setlength{\fboxsep}{0pt}\setlength{\fboxrule}{0.2pt}\fbox{\rule[0pt]{0pt}{1.3ex}\rule[0pt]{1.3ex}{0pt}}}}
\ifCLASSOPTIONcompsoc
\def\IEEEQED{\IEEEQEDopen}   % default to open for compsoc
\else
\def\IEEEQED{\IEEEQEDclosed} % otherwise default to closed
\fi

\newif\if@IEEEQEDshow  \@IEEEQEDshowtrue
\def\IEEEproofindentspace{2\parindent}% V1.8 allow user to change indentation amount if desired
\def\IEEEproof{\@ifnextchar[{\@IEEEproof}{\@IEEEproof[\IEEEproofname]}}
\def\@IEEEproof[#1]{\@IEEEQEDshowtrue\par\noindent\hspace{\IEEEproofindentspace}{\itshape #1: }}
\def\endIEEEproof{\if@IEEEQEDshow\hspace*{\fill}\nobreakspace\IEEEQED\fi\par}
\def\IEEEQEDhereeqn{\global\@IEEEQEDshowfalse\eqno\let\eqno\relax\let\leqno\relax
                    \let\veqno\relax\hbox{\IEEEQED}}
\def\IEEEQEDhere{\global\@IEEEQEDshowfalse\IEEEQED}
\def\IEEEQEDoff{\global\@IEEEQEDshowfalse}


\newdimen\@IEEEtmpitemindent

\def\@begintheorem#1#2{\@IEEEtmpitemindent\itemindent\relax\topsep 0pt\rmfamily\trivlist%
    \item[]\textit{\indent #1\ #2:} \itemindent\@IEEEtmpitemindent\relax}
\def\@opargbegintheorem#1#2#3{\@IEEEtmpitemindent\itemindent\relax\topsep 0pt\rmfamily \trivlist%
    \item[]\textit{\indent #1\ #2\ (#3):} \itemindent\@IEEEtmpitemindent\relax}
\def\@endtheorem{\endtrivlist}

\gdef\@IEEEthmcounterinsection#1{\arabic{#1}}
\edef\@IEEEstringsection{section}

\def\@xnthm#1#2[#3]{%
  \expandafter\@ifdefinable\csname #1\endcsname
    {\@definecounter{#1}\@newctr{#1}[#3]%
     \edef\@IEEEstringtmp{#3}
     \ifx\@IEEEstringtmp\@IEEEstringsection
     \expandafter\xdef\csname the#1\endcsname{%
     \noexpand\@IEEEthmcounterinsection{#3}\@thmcountersep
          \@thmcounter{#1}}%
     \else
     \expandafter\xdef\csname the#1\endcsname{%
       \expandafter\noexpand\csname the#3\endcsname \@thmcountersep
          \@thmcounter{#1}}%
     \fi
     \global\@namedef{#1}{\@thm{#1}{#2}}%
     \global\@namedef{end#1}{\@endtheorem}}}



\ps@headings
\pagenumbering{arabic}

\setcounter{page}{1}
\ifCLASSOPTIONpeerreview
   \if@twoside
      \setcounter{page}{-1}
   \else
      \setcounter{page}{0}
   \fi
\fi

\ifCLASSOPTIONtwoside\else\raggedbottom\fi
\ifCLASSOPTIONtwocolumn
   \ifCLASSOPTIONpeerreview\else
      \twocolumn
   \fi
\sloppy 
\flushbottom
\fi





\begingroup
\catcode`\Q=3
\long\gdef\@ifmtarg#1{\@xifmtarg#1QQ\@secondoftwo\@firstoftwo\@nil}
\long\gdef\@xifmtarg#1#2Q#3#4#5\@nil{#4}
\endgroup


\def\@IEEEsaveoriginalsectiononce{\let\@IEEEappendixsavesection\section
\let\@IEEEsaveoriginalsectiononce\relax}

\def\@IEEEprocessthesectionargument#1{%
\@ifmtarg{#1}{%
\@IEEEappendixsavesection*{\appendixname\nobreakspace\thesectiondis}%
\addcontentsline{toc}{section}{\appendixname\nobreakspace\thesection}}{%
\@IEEEappendixsavesection*{\appendixname\nobreakspace\thesectiondis\\* #1}%
\addcontentsline{toc}{section}{\appendixname\nobreakspace\thesection: #1}}}

\def\@IEEEdestroythesectionargument#1{\typeout{** WARNING: Ignoring useless
\protect\section\space in Appendix (line \the\inputlineno).}}



\def\appendix{\relax}
\renewcommand{\appendix}[1][]{\@IEEEsaveoriginalsectiononce\par
    \gdef\theHsection{Appendix.A}%
    \xdef\Hy@chapapp{appendix}%
    \setcounter{section}{0}%
    \setcounter{subsection}{0}%
    \setcounter{subsubsection}{0}%
    \setcounter{paragraph}{0}%
    \gdef\thesection{A}%
    \gdef\thesectiondis{}% 
    \gdef\thesubsection{\Alph{subsection}}%
    \gdef\@IEEEthmcounterinsection##1{A}
    \refstepcounter{section}% update the \ref counter
    \@ifmtarg{#1}{\@IEEEappendixsavesection*{\appendixname}%
                  \addcontentsline{toc}{section}{\appendixname}}{%
             \@IEEEappendixsavesection*{\appendixname\nobreakspace\\* #1}%
             \addcontentsline{toc}{section}{\appendixname: #1}}%
    \def\section{\@ifstar{\@IEEEappendixsavesection*}{%
                    \@IEEEdestroythesectionargument}}% throw out the argument
}



\def\appendices{\@IEEEsaveoriginalsectiononce\par
    \gdef\theHsection{Appendix.\Alph{section}}%
    \xdef\Hy@chapapp{appendix}%
    \setcounter{section}{-1}% we want \refstepcounter to use section 0
    \setcounter{subsection}{0}%
    \setcounter{subsubsection}{0}%
    \setcounter{paragraph}{0}%
    \ifCLASSOPTIONromanappendices%
    \gdef\thesection{\Roman{section}}%
    \gdef\thesectiondis{\Roman{section}}%
    \@IEEEcompsocconfonly{\gdef\thesectiondis{\Roman{section}.}}%
    \gdef\@IEEEthmcounterinsection##1{A\arabic{##1}}
    \else%
    \gdef\thesection{\Alph{section}}%
    \gdef\thesectiondis{\Alph{section}}%
    \@IEEEcompsocconfonly{\gdef\thesectiondis{\Alph{section}.}}%
    \gdef\@IEEEthmcounterinsection##1{\Alph{##1}}
    \fi%
    \refstepcounter{section}% update the \ref counter
    \setcounter{section}{0}% NEXT \section will be the FIRST appendix
    \def\section{\@ifstar{\@IEEEappendixsavesection*}{% process the *-form
                    \refstepcounter{section}% or is a new section so,
                    \@IEEEprocessthesectionargument}}% process the argument 
}



\def\@IEEEPARstartDROPLINES{2}
\def\@IEEEPARstartMINPAGELINES{2}
\def\@IEEEPARstartHEIGHTTEXT{T}
\def\@IEEEPARstartDROPDEPTH{1.1\baselineskip}
\def\@IEEEPARstartFONTSTYLE{\bfseries}
\def\@IEEEPARstartCAPSTYLE{\MakeUppercase}
\def\@IEEEPARstartWORDFONTSTYLE{\relax}
\def\@IEEEPARstartWORDCAPSTYLE{\MakeUppercase}
\def\@IEEEPARstartSEP{0.15em}
\def\@IEEEPARstartHOFFSET{0em}
\def\@IEEEPARstartITLCORRECT{\/}

\ifCLASSOPTIONcompsoc
\def\@IEEEPARstartFONTSTYLE{\mdseries}
\def\@IEEEPARstartWORDFONTSTYLE{\scshape}
\def\@IEEEPARstartWORDCAPSTYLE{\relax}
\fi

\def\IEEEPARstart#1#2{\par{%
\@IEEEtranneedspace{\@IEEEPARstartMINPAGELINES\baselineskip}{\relax}%
\noindent
\settoheight{\@IEEEtrantmpdimenA}{\@IEEEPARstartHEIGHTTEXT}%
\addtolength{\@IEEEtrantmpdimenA}{\@IEEEPARstartDROPDEPTH}%
\def\@IEEEPARstartGETFIRSTWORD##1 ##2\relax{##1}%
{\@IEEEPARstartFONTSTYLE{\selectfont\edef\@IEEEPARstartFONTNAMESPACE{\fontname\font\space}%
\xdef\@IEEEPARstartFONTNAME{\expandafter\@IEEEPARstartGETFIRSTWORD\@IEEEPARstartFONTNAMESPACE\relax}}}%
\font\@IEEEPARstartsubfont\@IEEEPARstartFONTNAME\space at \@IEEEtrantmpdimenA\relax%
\@IEEEtrantmpcountA=\@IEEEtrantmpdimenA%
\settoheight{\@IEEEtrantmpdimenB}{\@IEEEPARstartsubfont\@IEEEPARstartCAPSTYLE{#1}}%
\ifdim\@IEEEtrantmpdimenB=0pt\relax%
\typeout{** WARNING: IEEEPARstart drop letter has zero height! (line \the\inputlineno)}%
\typeout{ Forcing the drop letter font size to 10pt.}%
\@IEEEtrantmpdimenB=10pt%
\fi%
\@IEEEtrantmpcountB=\@IEEEtrantmpdimenB%
\divide\@IEEEtrantmpcountB by 200%
\divide\@IEEEtrantmpcountA by \@IEEEtrantmpcountB%
\@IEEEtrantmpdimenB=0.005\@IEEEtrantmpdimenA%
\multiply\@IEEEtrantmpdimenB by \@IEEEtrantmpcountA%
\global\font\@IEEEPARstartfont\@IEEEPARstartFONTNAME\space at \@IEEEtrantmpdimenB%
\settowidth{\global\@IEEEtrantmpdimenA}{\@IEEEPARstartfont
\@IEEEPARstartCAPSTYLE{#1\@IEEEPARstartITLCORRECT}}}%
\advance\@IEEEtrantmpdimenA by \@IEEEPARstartSEP\relax%
\advance\@IEEEtrantmpdimenA by \@IEEEPARstartHOFFSET\relax%
\@IEEEtrantmpdimenB\@IEEEtrantmpdimenA
\ifnum\@IEEEtrantmpdimenB < 0 \@IEEEtrantmpdimenB 0pt\fi
\hangindent\@IEEEtrantmpdimenB\hangafter=-\@IEEEPARstartDROPLINES%
\makebox[0pt][l]{\hspace{-\@IEEEtrantmpdimenA}%
\raisebox{-\@IEEEPARstartDROPDEPTH}[0pt][0pt]{\hspace{\@IEEEPARstartHOFFSET}%
\@IEEEPARstartfont\@IEEEPARstartCAPSTYLE{#1\@IEEEPARstartITLCORRECT}%
\hspace{\@IEEEPARstartSEP}}}%
{\@IEEEPARstartWORDFONTSTYLE{\@IEEEPARstartWORDCAPSTYLE{\selectfont#2}}}}






\def\@IEEEtranneedspace#1#2{\penalty-100\begingroup%shield temp variable
\@IEEEtrantmpdimenB\pagegoal\advance\@IEEEtrantmpdimenB-\pagetotal% space left
\ifdim #1>\@IEEEtrantmpdimenB\relax% not enough space left
\ifdim\@IEEEtrantmpdimenB>\z@\relax #2\fi%
\newpage%
\fi\endgroup}



\newif\if@IEEEbiographyTOCentrynotmade
\global\@IEEEbiographyTOCentrynotmadetrue

\newcounter{IEEEbiography}
\setcounter{IEEEbiography}{0}

\def\@IEEEBIOphotowidth{1.0in}    % width of the biography photo area
\def\@IEEEBIOphotodepth{1.25in}   % depth (height) of the biography photo area
\def\@IEEEBIOhangwidth{1.14in}    % width cleared for the biography photo area
\def\@IEEEBIOhangdepth{1.25in}    % depth cleared for the biography photo area
\def\@IEEEBIOskipN{4\baselineskip}% nominal value of the vskip above the biography

\newenvironment{IEEEbiography}[2][]{\normalfont\@IEEEcompsoconly{\sffamily}\footnotesize%
\unitlength 1in\parskip=0pt\par\parindent 1em\interlinepenalty500%
\@IEEEtrantmpdimenA=\@IEEEBIOhangdepth%
\advance\@IEEEtrantmpdimenA by \@IEEEBIOskipN%
\advance\@IEEEtrantmpdimenA by 1\baselineskip%
\@IEEEtranneedspace{\@IEEEtrantmpdimenA}{\relax}%
\vskip \@IEEEBIOskipN plus 1fil minus 0\baselineskip%
\def\@IEEEtempbiographybox{{\setlength{\fboxsep}{0pt}\framebox{%
\begin{minipage}[b][\@IEEEBIOphotodepth][c]{\@IEEEBIOphotowidth}\centering PLACE\\ PHOTO\\ HERE \end{minipage}}}}%
\@ifmtarg{#1}{\relax}{\def\@IEEEtempbiographybox{\mbox{\begin{minipage}[b][\@IEEEBIOphotodepth][c]{\@IEEEBIOphotowidth}%
\centering%
#1%
\end{minipage}}}}% end if optional argument supplied
\if@IEEEbiographyTOCentrynotmade%
\setcounter{IEEEbiography}{-1}%
\refstepcounter{IEEEbiography}%
\addcontentsline{toc}{section}{Biographies}%
\global\@IEEEbiographyTOCentrynotmadefalse%
\fi%
\refstepcounter{IEEEbiography}%
\addcontentsline{toc}{subsection}{#2}%
\let\@IEEEBIOORGparCMD=\par% save the original \par command
\edef\par{\hfil\break\indent}% the new \par will not be a "real" \par
\settoheight{\@IEEEtrantmpdimenA}{\@IEEEtempbiographybox}% get height of biography box
\@IEEEtrantmpdimenB=\@IEEEBIOhangdepth%
\@IEEEtrantmpcountA=\@IEEEtrantmpdimenB% countA has the hang depth
\divide\@IEEEtrantmpcountA by \baselineskip%  calculates lines needed to produce the hang depth
\advance\@IEEEtrantmpcountA by 1% ensure we overestimate
\hangindent\@IEEEBIOhangwidth%
\hangafter-\@IEEEtrantmpcountA%
\settoheight{\@IEEEtrantmpdimenB}{\mbox{T}}%
\noindent\makebox[0pt][l]{\hspace{-\@IEEEBIOhangwidth}\raisebox{\@IEEEtrantmpdimenB}[0pt][0pt]{%
\raisebox{-\@IEEEBIOphotodepth}[0pt][0pt]{\@IEEEtempbiographybox}}}%
\noindent\textbf{#2\ }\@IEEEgobbleleadPARNLSP}{\relax\let\par=\@IEEEBIOORGparCMD\par%
\ifnum \prevgraf <\@IEEEtrantmpcountA\relax% detect when the biography text is shorter than the photo
    \advance\@IEEEtrantmpcountA by -\prevgraf% calculate how many lines we need to pad
    \advance\@IEEEtrantmpcountA by -1\relax% we compensate for the fact that we indented an extra line
    \@IEEEtrantmpdimenA=\baselineskip% calculate the length of the padding
    \multiply\@IEEEtrantmpdimenA by \@IEEEtrantmpcountA%
    \noindent\rule{0pt}{\@IEEEtrantmpdimenA}% insert an invisible support strut
\fi%
\par\normalfont}



\newenvironment{IEEEbiographynophoto}[1]{%
\if@IEEEbiographyTOCentrynotmade%
\setcounter{IEEEbiography}{-1}%
\refstepcounter{IEEEbiography}%
\addcontentsline{toc}{section}{Biographies}%
\global\@IEEEbiographyTOCentrynotmadefalse%
\fi%
\refstepcounter{IEEEbiography}%
\addcontentsline{toc}{subsection}{#1}%
\normalfont\@IEEEcompsoconly{\sffamily}\footnotesize\interlinepenalty500%
\vskip 4\baselineskip plus 1fil minus 0\baselineskip%
\parskip=0pt\par%
\noindent\textbf{#1\ }\@IEEEgobbleleadPARNLSP}{\relax\par\normalfont}


\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\DeclareRobustCommand*\cal{\@fontswitch\relax\mathcal}
\DeclareRobustCommand*\mit{\@fontswitch\relax\mathnormal}


\def\@IEEEspecialpapernotice{\relax}
 
\def\IEEEspecialpapernotice#1{\ifCLASSOPTIONconference%
\def\@IEEEspecialpapernotice{{\sublargesize\textit{#1}\vspace*{1em}}}%
\else%
\def\@IEEEspecialpapernotice{{\\*[1.5ex]\sublargesize\textit{#1}}\vspace*{-2ex}}%
\fi}




\def\@IEEEpubidpullup{1.3\baselineskip}
\ifCLASSOPTIONtechnote
\def\@IEEEpubidpullup{2\baselineskip}
\fi

\ifCLASSOPTIONcompsoc
\def\@IEEEpubidpullup{0pt}
\fi

\def\@IEEEpubid{\relax}

\newif\if@IEEEusingpubid
\global\@IEEEusingpubidfalse
\def\IEEEpubid#1{\def\@IEEEpubid{#1}\global\@IEEEusingpubidtrue}


\def\IEEEpubidadjcol{\ifCLASSOPTIONcompsoc\else\ifCLASSOPTIONpeerreview\else
\if@twocolumn\if@IEEEusingpubid\enlargethispage{-\@IEEEpubidpullup}\fi\fi\fi\fi}





\newsavebox{\@IEEEtranrubishbin}

\newif\if@IEEEWARNthanks
\newif\if@IEEEWARNIEEEPARstart
\newif\if@IEEEWARNIEEEbiography
\newif\if@IEEEWARNIEEEbiographynophoto
\newif\if@IEEEWARNIEEEpubid
\newif\if@IEEEWARNIEEEpubidadjcol
\newif\if@IEEEWARNIEEEmembership
\newif\if@IEEEWARNIEEEaftertitletext
\@IEEEWARNthankstrue
\@IEEEWARNIEEEPARstarttrue
\@IEEEWARNIEEEbiographytrue
\@IEEEWARNIEEEbiographynophototrue
\@IEEEWARNIEEEpubidtrue
\@IEEEWARNIEEEpubidadjcoltrue
\@IEEEWARNIEEEmembershiptrue
\@IEEEWARNIEEEaftertitletexttrue


\let\@IEEESAVECMDthanks\thanks
\let\@IEEESAVECMDIEEEPARstart\IEEEPARstart
\let\@IEEESAVECMDIEEEbiography\IEEEbiography
\let\@IEEESAVECMDendIEEEbiography\endIEEEbiography
\let\@IEEESAVECMDIEEEbiographynophoto\IEEEbiographynophoto
\let\@IEEESAVECMDendIEEEbiographynophoto\endIEEEbiographynophoto
\let\@IEEESAVECMDIEEEpubid\IEEEpubid
\let\@IEEESAVECMDIEEEpubidadjcol\IEEEpubidadjcol
\let\@IEEESAVECMDIEEEmembership\IEEEmembership
\let\@IEEESAVECMDIEEEaftertitletext\IEEEaftertitletext


\ifCLASSOPTIONdraftcls
\def\IEEEPARstart#1#2{#1#2\if@IEEEWARNIEEEPARstart\typeout{** ATTENTION: \noexpand\IEEEPARstart
 is disabled in draft mode (line \the\inputlineno).}\fi\global\@IEEEWARNIEEEPARstartfalse}
\fi
\ifCLASSOPTIONtechnote
\def\IEEEPARstart#1#2{#1#2\if@IEEEWARNIEEEPARstart\typeout{** WARNING: \noexpand\IEEEPARstart
 is locked out for technotes (line \the\inputlineno).}\fi\global\@IEEEWARNIEEEPARstartfalse}
\fi


\ifCLASSOPTIONconference
\def\thanks#1{\if@IEEEWARNthanks\typeout{** WARNING: \noexpand\thanks
 is locked out when in conference mode (line \the\inputlineno).}\fi\global\@IEEEWARNthanksfalse}
\def\IEEEPARstart#1#2{#1#2\if@IEEEWARNIEEEPARstart\typeout{** WARNING: \noexpand\IEEEPARstart
 is locked out when in conference mode (line \the\inputlineno).}\fi\global\@IEEEWARNIEEEPARstartfalse}


\newenvironment{@IEEEbogusbiography}[2][]{\if@IEEEWARNIEEEbiography\typeout{** WARNING: \noexpand\IEEEbiography
 is locked out when in conference mode (line \the\inputlineno).}\fi\global\@IEEEWARNIEEEbiographyfalse%
\setbox\@IEEEtranrubishbin\vbox\bgroup}{\egroup\relax}
\let\IEEEbiography=\@IEEEbogusbiography
\let\endIEEEbiography=\end@IEEEbogusbiography

\renewenvironment{IEEEbiographynophoto}[1]{\if@IEEEWARNIEEEbiographynophoto\typeout{** WARNING: \noexpand\IEEEbiographynophoto
 is locked out when in conference mode (line \the\inputlineno).}\fi\global\@IEEEWARNIEEEbiographynophotofalse%
\setbox\@IEEEtranrubishbin\vbox\bgroup}{\egroup\relax}

\def\IEEEpubid#1{\if@IEEEWARNIEEEpubid\typeout{** WARNING: \noexpand\IEEEpubid 
 is locked out when in conference mode (line \the\inputlineno).}\fi\global\@IEEEWARNIEEEpubidfalse}
\def\IEEEpubidadjcol{\if@IEEEWARNIEEEpubidadjcol\typeout{** WARNING: \noexpand\IEEEpubidadjcol
 is locked out when in conference mode (line \the\inputlineno).}\fi\global\@IEEEWARNIEEEpubidadjcolfalse}
\def\IEEEmembership#1{\if@IEEEWARNIEEEmembership\typeout{** WARNING: \noexpand\IEEEmembership
 is locked out when in conference mode (line \the\inputlineno).}\fi\global\@IEEEWARNIEEEmembershipfalse}
\def\IEEEaftertitletext#1{\if@IEEEWARNIEEEaftertitletext\typeout{** WARNING: \noexpand\IEEEaftertitletext
 is locked out when in conference mode (line \the\inputlineno).}\fi\global\@IEEEWARNIEEEaftertitletextfalse}
\fi


\def\IEEEoverridecommandlockouts{%
\typeout{** ATTENTION: Overriding command lockouts (line \the\inputlineno).}%
\let\thanks\@IEEESAVECMDthanks%
\let\IEEEPARstart\@IEEESAVECMDIEEEPARstart%
\let\IEEEbiography\@IEEESAVECMDIEEEbiography%
\let\endIEEEbiography\@IEEESAVECMDendIEEEbiography%
\let\IEEEbiographynophoto\@IEEESAVECMDIEEEbiographynophoto%
\let\endIEEEbiographynophoto\@IEEESAVECMDendIEEEbiographynophoto%
\let\IEEEpubid\@IEEESAVECMDIEEEpubid%
\let\IEEEpubidadjcol\@IEEESAVECMDIEEEpubidadjcol%
\let\IEEEmembership\@IEEESAVECMDIEEEmembership%
\let\IEEEaftertitletext\@IEEESAVECMDIEEEaftertitletext}



{\catcode`\|=0 \catcode`\\=12
|xdef|@IEEEbackslash{\}}


\def\@IEEElegacywarn#1#2{\typeout{** ATTENTION: \@IEEEbackslash #1 is deprecated (line \the\inputlineno).
Use \@IEEEbackslash #2 instead.}}


\def\authorblockA{\@IEEElegacywarn{authorblockA}{IEEEauthorblockA}\IEEEauthorblockA}
\def\authorblockN{\@IEEElegacywarn{authorblockN}{IEEEauthorblockN}\IEEEauthorblockN}
\def\authorrefmark{\@IEEElegacywarn{authorrefmark}{IEEEauthorrefmark}\IEEEauthorrefmark}
\def\PARstart{\@IEEElegacywarn{PARstart}{IEEEPARstart}\IEEEPARstart}
\def\pubid{\@IEEElegacywarn{pubid}{IEEEpubid}\IEEEpubid}
\def\pubidadjcol{\@IEEElegacywarn{pubidadjcol}{IEEEpubidadjcol}\IEEEpubidadjcol}
\def\specialpapernotice{\@IEEElegacywarn{specialpapernotice}{IEEEspecialpapernotice}\IEEEspecialpapernotice}
\def\IEEEcompsoctitleabstractindextext{\@IEEElegacywarn{IEEEcompsoctitleabstractindextext}{IEEEtitleabstractindextext}\IEEEtitleabstractindextext}
\def\IEEEdisplaynotcompsoctitleabstractindextext{\@IEEElegacywarn{IEEEdisplaynotcompsoctitleabstractindextext}{IEEEdisplaynontitleabstractindextext}\IEEEdisplaynontitleabstractindextext}
\def\keywords{\@IEEElegacywarn{keywords}{IEEEkeywords}\IEEEkeywords}
\def\endkeywords{\endIEEEkeywords}




\endinput

