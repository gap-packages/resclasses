#############################################################################
##
#W  manstyle.g                                                    Stefan Kohl
##
#H  @(#)$Id$
##
##  This file provides an alternative layout for the LaTeX-/DVI-/Postscript-
##  and PDF-manuals produced by GAPDoc. It mainly consists of code which was
##  taken from the GAPDoc package of Frank Lübeck and adjusted to our needs /
##  taste.
##
Revision.manstyle_g :=
  "@(#)$Id$";

DeclareGlobalVariable( "GAPDoc2LaTeXProcsBuffer" );
InstallValue( GAPDoc2LaTeXProcsBuffer, StructuralCopy( GAPDoc2LaTeXProcs ) );

#############################################################################
##
#F  StefansManualLayout( ) . . . . . . . . . . . . better LaTeX manual layout
##
##  Better layout of LaTeX-/ DVI-/ Postscript-/ PDF-manuals.
##
StefansManualLayout := function ( bookname )

  MakeReadWriteGlobal( "GAPDoc2LaTeXProcs");

  GAPDoc2LaTeXProcs.Head1 :=
  Concatenation(ListWithIdenticalEntries(100,'%'),"\n%%\n%%  ",
                bookname,".tex",
                ListWithIdenticalEntries((100-34-2*Length(bookname))/2,' '),
                bookname," documentation ",
                ListWithIdenticalEntries((100-34-2*Length(bookname))/2,' '),
                "Stefan Kohl\n%%\n\n\\documentclass[");

  GAPDoc2LaTeXProcs.Head1x :=
  Concatenation(
    "10pt,a4paper]{book}\n\n",
    "\\usepackage[latin1]{inputenc}\n",
    "\\usepackage{theorem}\n",
    "\\usepackage{longtable}\n",
    "\\usepackage{verbatim}\n",
    "\\usepackage{fancyhdr}\n",
    "\\usepackage{extramarks}\n",
    "\\usepackage{enumerate}\n",
    "\\usepackage{latexsym}\n",
    "\\usepackage{amsfonts}\n",
    "\\usepackage{amssymb}\n",
    "\\usepackage{amscd}\n",
    "\\usepackage{amsbsy}\n",
    "\\usepackage{amstext}\n",
    "\\usepackage{amsxtra}\n",
    "\\usepackage{makeidx}\n\n",
    "\\pagestyle{fancy}\n",
    "\\oddsidemargin  -0.5cm\n",
    "\\evensidemargin -0.5cm\n",
    "\\textwidth      16.0cm\n" );

  GAPDoc2LaTeXProcs.Head3 := 
  Concatenation(
    "\\newwrite\\pagenrlog\n",
    "\\immediate\\openout\\pagenrlog =\\jobname.pnr\n",
    "\\immediate\\write\\pagenrlog{PAGENRS := [}\n",
    "\\newcommand{\\logpage}[1]",
    "{\\protect\\write\\pagenrlog{#1, \\thepage,}}\n\n",
    "\\newcommand{\\Q}{\\mathbb{Q}}\n",
    "\\newcommand{\\R}{\\mathbb{R}}\n",
    "\\newcommand{\\C}{\\mathbb{C}}\n",
    "\\newcommand{\\Z}{\\mathbb{Z}}\n",
    "\\newcommand{\\N}{\\mathbb{N}}\n",
    "\\newcommand{\\F}{\\mathbb{F}}\n\n",
    "\\newcommand{\\GAP}{\\textsf{GAP}}\n\n",
    "\\newsavebox{\\backslashbox}\n",
    "\\sbox{\\backslashbox}{\\texttt{\\symbol{92}}}\n",
    "\\newcommand{\\bs}{\\usebox{\\backslashbox}}\n\n",
    "\\begin{document}\n\n",
    "  \\headheight     16.0pt\n",
    "  \\headwidth      16.0cm\n",
    "  \\footrulewidth   0.4pt\n",
    "  \\renewcommand{\\chaptermark}[1]{\\markboth{#1}{}}\n",
    "  \\renewcommand{\\sectionmark}[1]{\\markright{\\thesection\\ #1}}\n",
    "  \\fancyhf{}\n",
    "  \\fancyhead[RO]{\\nouppercase{\\bfseries\\rightmark}}\n",
    "  \\fancyhead[LE]{\\nouppercase{\\bfseries\\leftmark}}\n",
    "  \\fancyfoot[CE,CO]{\\bfseries\\thepage}\n",
    "  \\fancypagestyle{plain}{\n",
    "   \\fancyhead{}\n",
    "   \\renewcommand{\\headrulewidth}{0pt}\n",
    "  }\n" );

  GAPDoc2LaTeXProcs.Book := function( r, str, pi )
  
    local   a;
  
    Append(str,GAPDoc2LaTeXProcs.Head1);
    if IsBound(pi.Options) then
      NormalizeWhitespace(pi.Options);
      Append(str,pi.Options);
    fi;
    Append(str,GAPDoc2LaTeXProcs.Head1x);
    if IsBound(pi.ExtraPreamble) then
      Append(str,pi.ExtraPreamble);
    fi;
    Append(str,GAPDoc2LaTeXProcs.Head3);
    GAPDoc2LaTeXContent(r, str);
    Append(str,GAPDoc2LaTeXProcs.Tail);
  end;

  GAPDoc2LaTeXProcs.URL := function( arg )

    local  r, str, pre, s, p;

    r := arg[1]; str := arg[2];
    if Length(arg) > 2 then pre := arg[3]; else pre := ""; fi; s := "";
    GAPDoc2LaTeXContent(r,s);
    s := SubstitutionSublist(s,"{\\textasciitilde}","~");
    p := Position(s,'~');
    while p<>fail do
      s := Concatenation(s{[1..p-1]},"\\~{}",s{[p+1..Length(s)]});
      p := Position(s,'~',p+3);
    od;
    p := Position(s,'\#');
    while p <> fail do
      s := Concatenation(s{[1..p-1]},"\\\#",s{[p+1..Length(s)]});
      p := Position(s,'\#',p+1);
    od;
    Append(str,"\\texttt{");
    Append(str,s);
    Append(str,"}");
  end;

  GAPDoc2LaTeXProcs.Homepage := GAPDoc2LaTeXProcs.URL;

  GAPDoc2LaTeXProcs.Email := GAPDoc2LaTeXProcs.URL;

  GAPDoc2LaTeXProcs.TitlePage := function( r, str )
  
  local  l, a, s, cont;
  
    # page number info for online help
    Append(str, Concatenation("\\logpage{", 
            GAPDoc2LaTeXProcs.StringNrs(r.count{[1..3]}), "}\n"));

    Append(str,"\n  \\mainmatter\n");
    Append(str,"    \\begin{titlepage}\n\n");
    Append(str,"      \\begin{center}\n\n");

    # title
    l := Filtered(r.content,a->a.name="Title");
    Append(str,"        \\textsf{\\textbf{\\Huge{");
    s := "";
    GAPDoc2LaTeXContent(l[1],s);
    Append(str,s);
    Append(str,"}}}\\\\[1cm]\n\n");
  
    # subtitle
    l := Filtered(r.content,a->a.name="Subtitle");
    if Length(l) > 0 then
      Append(str,"        \\Large{");
      GAPDoc2LaTeXContent(l[1],str);
      Append(str,"}\\\\[1cm]\n\n");
    fi;
  
    # version
    l := Filtered(r.content,a->a.name="Version");
    if Length(l) > 0 then
      Append(str,"        \\large{");
      GAPDoc2LaTeXContent(l[1],str);
      Append(str,"}\\\\[1cm]\n\n");
    fi;

    # date
    l := Filtered(r.content,a->a.name="Date");
    if Length(l) > 0 then
      Append(str,"        \\large{");
      GAPDoc2LaTeXContent(l[1],str);
      Append(str,"}\\\\[1cm]\n\n");
    fi;

    # author name(s)
    l := Filtered(r.content,a->a.name="Author");
    for a in l do
      Append(str,"        \\large{");
      GAPDoc2LaTeXContent(rec(content := Filtered(a.content,
                          b->not b.name in ["Email","Homepage","Address"])),
                          str);
      Append(str,"}\\\\\n\n");
    od;

    # extra comment for front page
    l := Filtered(r.content,a->a.name="TitleComment");
    if Length(l) > 0 then
      Append(str,"\\mbox{}\\\\[2cm]\n\\begin{minipage}{12cm}\\noindent\n");
      GAPDoc2LaTeXContent(l[1],str);
      Append(str,"\\end{minipage}\n\n");
    fi;
    Append(str,"      \\end{center}\n      \\vfill\n\n\\mbox{}\\\\\n");
  
    # email, WWW-homepage and address of author(s), if given
    l := Filtered(r.content,a->a.name="Author");
    for a in l do
      cont := List(a.content,b->b.name);
      if "Email" in cont or "Homepage" in cont or "Address" in cont then
        Append(str,"{\\mbox{}\\\\\n\\small \\noindent \\textbf{");
        GAPDoc2LaTeXContent(rec(content := Filtered(a.content,
                                b->not b.name in ["Email","Homepage",
                                                  "Address"])),str);
        Append(str,"}");
        if "Email" in cont then
          Append(str,"\n --- Email: ");
          GAPDoc2LaTeX(a.content[Position(cont,"Email")],str);
        fi;
        if "Homepage" in cont then
          Append(str,"\\\\\n");
          Append(str," --- Homepage: ");
          GAPDoc2LaTeX(a.content[Position(cont,"Homepage")],str);
        fi;
        if "Address" in cont then
          Append(str,"\\\\\n");
          Append(str," --- Address: \\begin{minipage}[t]{8cm}\\noindent\n");
          GAPDoc2LaTeX(a.content[Position(cont,"Address")],str);
          Append(str,"\\end{minipage}\n");
        fi;
        Append(str,"}\\\\\n");
      fi;
    od;

    # Address outside the Author elements
    l := Filtered(r.content,a->a.name="Address");
    if Length(l)>0 then
      Append(str,"\n\\noindent ");
      Append(str,"\\textbf{Address: }\n");
      Append(str,"\\begin{minipage}[t]{8cm}\\noindent\n");
      GAPDoc2LaTeXContent(l[1],str);
      Append(str,"\n\\end{minipage}\n");
    fi;
  
    Append(str,"    \\end{titlepage}\n\n\\newpage");
  
    #  to make physical page numbers same as document page numbers
    Append(str,"\\setcounter{page}{2}\n");

    # abstract
    l := Filtered(r.content,a->a.name="Abstract");
    if Length(l) > 0 then
      Append(str,"{\\small \n\\section*{Abstract}\n");
      Append(str,Concatenation("\\logpage{", 
                 GAPDoc2LaTeXProcs.StringNrs(l[1].count{[1..3]}),"}\n"));
      GAPDoc2LaTeXContent(l[1],str);
      Append(str,"}\\\\[1cm]\n");
    fi;
  
    # copyright page
    l := Filtered(r.content,a->a.name="Copyright");
    if Length(l) > 0 then
      Append(str,"{\\small \n\\section*{Copyright}\n");
      Append(str,Concatenation("\\logpage{", 
                 GAPDoc2LaTeXProcs.StringNrs(l[1].count{[1..3]}),"}\n"));
      GAPDoc2LaTeXContent(l[1],str);
      Append(str,"}\\\\[1cm]\n");
    fi;

    # acknowledgement page
    l := Filtered(r.content,a->a.name="Acknowledgements");
    if Length(l) > 0 then
      Append(str,"{\\small \n\\section*{Acknowledgements}\n");
      Append(str,Concatenation("\\logpage{", 
              GAPDoc2LaTeXProcs.StringNrs(l[1].count{[1..3]}),"}\n"));
      GAPDoc2LaTeXContent(l[1],str);
      Append(str,"}\\\\[1cm]\n");
    fi;

    # colophon page
    l := Filtered(r.content,a->a.name="Colophon");
    if Length(l) > 0 then
      Append(str,"{\\small \n\\section*{Colophon}\n");
      Append(str,Concatenation("\\logpage{", 
                 GAPDoc2LaTeXProcs.StringNrs(l[1].count{[1..3]}),"}\n"));
      GAPDoc2LaTeXContent(l[1],str);
      Append(str,"}\\\\[1cm]\n");
    fi;  
    Append(str,"\\newpage\n\n");
  end;

  GAPDoc2LaTeXProcs.ChapSect := function( r, str, sect )

    local  posh, a, s;

    posh := Position(List(r.content,a->a.name),"Heading");
    while str[Length(str)] in " \n" do Unbind(str[Length(str)]); od;
    Append(str,Concatenation("\n\n",ListWithIdenticalEntries(100,'%'),
                             "\n\n\\",sect,"{"));
    s := "";
    if posh <> fail then      
      GAPDoc2LaTeXProcs.Heading1(r.content[posh],s);
    fi;
    Append(str,s);
    Append(str,"}");
    # label for references
    if IsBound(r.attributes.Label) then
      Append(str," \\label{");
      Append(str,r.attributes.Label);
      Append(str,"}");
      # save heading for "Text" style references to section
      GAPDoc2LaTeXProcs._labeledSections.(r.attributes.Label) := s;
    fi;
    Append(str,"\n");
    Append(str,Concatenation("\\logpage{", 
               GAPDoc2LaTeXProcs.StringNrs(r.count{[1..3]}),"}\n\n"));
    # the actual content
    GAPDoc2LaTeXContent(r,str);
    Append(str,"\n");
  end;

  GAPDoc2LaTeXProcs.TableOfContents := function( r, str )
    Append(str,Concatenation("\\def\\contentsname{Contents\\logpage{", 
            GAPDoc2LaTeXProcs.StringNrs(r.count{[1..3]}),"}}\n"));
    Append(str,"\n\\tableofcontents\n\n");
  end;

  GAPDoc2LaTeXProcs.Bibliography := function( r, str )

    local  st;

    if IsBound(r.attributes.Style)
    then st := r.attributes.Style;
    else st := "alpha"; fi;
    while str[Length(str)] in " \n" do Unbind(str[Length(str)]); od;
    Append(str,Concatenation("\n\n",ListWithIdenticalEntries(100,'%'),
                             "\n\n  \\backmatter\n\n"));
    Append(str,Concatenation("    \\def\\bibname{References\\logpage{", 
               GAPDoc2LaTeXProcs.StringNrs(r.count{[1..3]}),"}}\n"));
    Append(str,"    \\addcontentsline{toc}{chapter}{\\bibname}\n");
    Append(str,"\n    \\bibliographystyle{");
    Append(str,st);
    Append(str,"}\n    \\bibliography{");
    Append(str,r.attributes.Databases);
    Append(str,"}\n\n");
  end;

  ##  setting in typewriter
  GAPDoc2LaTeXProcs.WrapTT := function( r, str )

    local  s, a;

    s := "";
    GAPDoc2LaTeXContent(r,s);
    Append(str,Concatenation("\\texttt{",s,"}"));
  end;

  ##  simple maths
  GAPDoc2LaTeXProcs.M := function( r, str )

    local  a;

    Append(str,"\\(");
    GAPDoc2LaTeXContent(r,str);
    Append(str,"\\)");
  end;

  ##  in LaTeX same as <M>
  GAPDoc2LaTeXProcs.Math := GAPDoc2LaTeXProcs.M;

  ##  displayed maths
  GAPDoc2LaTeXProcs.Display := function( r, str )

    local  a;

    if   Length(str) > 0 and str[Length(str)] <> '\n'
    then Add(str,'\n'); fi;
    Append(str,"\n\\[\n");
    GAPDoc2LaTeXContent(r,str);
    Append(str,"\n\\]\n");
  end;

  ##  verbatim GAP session
  GAPDoc2LaTeXProcs.Verb := function( r, str )

    local  cont, a, s;

    Append(str,"\n\\begin{verbatim}\n");
    cont := "";
    for a in r.content do 
      # here we try to avoid reformatting
      if   IsString(a.content)
      then Append(cont,a.content);
      else s := ""; GAPDoc2LaTeX(a,s); Append(cont,s); fi;
    od;
    cont := SplitString(cont,"","\n");
    cont := Concatenation(List(cont,a->Concatenation("  ",a,"\n")));
    while cont[Length(cont)] in " \n" do
      Unbind(cont[Length(cont)]);
    od;
    Append(str,cont);
    Append(str,"\n\\end{verbatim}\n");
  end;

  GAPDoc2LaTeXProcs.ExampleLike := function( r, str, label )

    local  cont, a, s;

    Append(str,"\n\\begin{verbatim}");
    cont := "";
    for a in r.content do 
      # here we try to avoid reformatting
      if   IsString(a.content)
      then Append(cont,a.content); 
      else s := ""; GAPDoc2LaTeX(a,s); Append(cont,s); fi;
    od;
    cont := SplitString(cont,"\n","");
    # if first line has white space only, we remove it
    if Length(cont) > 0 and ForAll(cont[1],x->x in WHITESPACE) then
      cont := cont{[2..Length(cont)]};
    fi;
    cont := Concatenation(List(cont,a->Concatenation("  ",a,"\n")));
    while cont[Length(cont)] in " \n" do
      Unbind(cont[Length(cont)]);
    od;
    Append(str,cont);
    Append(str,"\n\\end{verbatim}\n");
  end;

  ##  this produces an implicit index entry and a label entry
  GAPDoc2LaTeXProcs.LikeFunc := function( r, str, typ )

    local  nam, namclean, lab;

    Append(str,"\\noindent\\(\\triangleright\\)\\ \\texttt{");
    nam := r.attributes.Name;
    namclean := GAPDoc2LaTeXProcs.DeleteUsBs(nam);
    # we allow _ and \ here
    nam := GAPDoc2LaTeXProcs.EscapeUsBs(nam);
    Append(str,nam);
    if IsBound(r.attributes.Arg) then
      Append(str,"( ");
      Append(str,NormalizedArgList(r.attributes.Arg));
      Append(str," )");
    fi;
    # possible label
    if   IsBound(r.attributes.Label)
    then lab := Concatenation("!",r.attributes.Label);
    else lab := ""; fi;
    # index entry
    Append(str,Concatenation("\\index{",namclean,"@\\texttt{",
            nam,"}",lab,"}\n"));
    # label (if not given, the default is the Name)
    if IsBound(r.attributes.Label) then
      namclean := Concatenation(namclean,":",r.attributes.Label);
    fi;
    Add(GAPDoc2LaTeXProcs._currentSubsection,namclean);
    Append(str,Concatenation("\\label{",namclean,"}\n"));
    # some hint about the type of the variable
    Append(str,"}\\hfill{\\sf (");
    Append(str,typ);
    Append(str,")}\\\\\n");
  end;

  GAPDoc2LaTeXProcs.Returns := function( r, str )
    while str[Length(str)] in " \n" do Unbind(str[Length(str)]); od;
    Append(str,"\n\n\\noindent\\textbf{Returns:\\ }\n");
    GAPDoc2LaTeXContent(r,str); 
    Append(str,"\n\\vspace{2mm}\n");
  end;

  GAPDoc2LaTeXProcs.Description := function( r, str )
    while str[Length(str)] in " \n" do Unbind(str[Length(str)]); od;
    Append(str,"\n\n\\noindent");
    GAPDoc2LaTeXContent(r,str);
  end;

  GAPDoc2LaTeXProcs.ManSection := function( r, str )

    local  funclike, f, lab, i;
  
    # function like elements
    funclike := [ "Func", "Oper", "Meth", "Filt", "Prop", "Attr", "Var", 
                  "Fam", "InfoClass" ];
  
    # heading comes from name of first function like element
    i := 1;
    while not r.content[i].name in funclike do i := i+1; od;
    f := r.content[i];
    if   IsBound(f.attributes.Label)
    then lab := Concatenation(" (",f.attributes.Label,")");
    else lab := ""; fi;
    while str[Length(str)] in " \n" do Unbind(str[Length(str)]); od;
    Append(str,Concatenation("\n\n\\subsection{",
               GAPDoc2LaTeXProcs.EscapeUsBs(f.attributes.Name),lab,"}\n"));
    Append(str,Concatenation("\\logpage{",
            GAPDoc2LaTeXProcs.StringNrs(r.count{[1..3]}),"}\\nobreak\n"));
    # to avoid references to local subsection in description:
    GAPDoc2LaTeXProcs._currentSubsection := r.count{[1..3]};
    Append(str,"{");
    GAPDoc2LaTeXContent(r,str);
    Append(str,"\n"); # Append(str,"}\n\n");
    Unbind(GAPDoc2LaTeXProcs._currentSubsection);
  end;

  GAPDoc2LaTeXProcs.List := function( r, str )

    local  item, type, a;

    if   "Mark" in List(r.content,a->a.name)
    then item := "";          type := "description";
    else item := "\n\\item "; type := "itemize"; fi;
    Append(str,Concatenation("\n\\begin{",type,"}\n"));
    for a in r.content do
      if   a.name = "Mark"
      then GAPDoc2LaTeXProcs.Mark(a,str);
      elif a.name = "Item"
      then Append(str,item); GAPDoc2LaTeXContent(a,str); fi;
    od;
    Append(str,Concatenation("\n\\end{",type,"}\n"));
  end;

  GAPDoc2LaTeXProcs.Enum := function( r, str )
    Append(str,"\n\\begin{enumerate}\n");
    GAPDoc2LaTeXContent(r,str);
    Append(str,"\n\\end{enumerate}\n");
  end;

  MakeReadOnlyGlobal( "GAPDoc2LaTeXProcs" );
end;
MakeReadOnlyGlobal( "StefansManualLayout" );

BindGlobal( "Broken", function( str, lng, breakat, breakbefore, breakafter )

  local  rawlines, rawline, lines, line, result, breakpos, pos;

  rawlines := SplitString(str,"\n"); lines := [ ];
  for rawline in rawlines do
    while Length(rawline) > lng do
      breakpos := lng;
      while not (   rawline[breakpos] in breakat
                 or rawline[breakpos] in breakbefore
                 or rawline[breakpos] in breakafter) and breakpos > 1
      do breakpos := breakpos - 1; od;
      if   breakpos <= 1
      then Error("Line ",rawline," cannot be split into lines of ",
                 "maximal length ",lng,"\n"); fi;
      line := rawline{[1..breakpos]};
      if   line[breakpos] in Concatenation(breakbefore,breakat)
      then Unbind(line[breakpos]); fi;
      Add(lines,line);
      if   rawline[breakpos] in breakbefore
      then rawline := rawline{[breakpos..Length(rawline)]};
      else rawline := rawline{[breakpos+1..Length(rawline)]}; fi;
    od;
    Add(lines,rawline);
  od;
  result := "";
  for line in lines do Append(result,line); Append(result,"\n"); od;
  Unbind(result[Length(result)]);
  return result;
end );

BindGlobal( "MyMakeGAPDocDoc", function( arg )

  local  htmlspecial, path, main, files, bookname, gaproot, 
         str, r, l, latex, null, t, h;
  
  htmlspecial := Filtered(arg,a->a in ["MathML","Tth"]);
  if Length(htmlspecial) > 0 then
    arg := Filtered(arg,a->not a in ["MathML","Tth"]);
  fi;
  path     := arg[1];
  main     := arg[2];
  files    := arg[3];
  bookname := arg[4];
  if IsBound(arg[5]) then gaproot := arg[5]; else gaproot := false; fi;
  # ensure that path is directory object
  if IsString(path) then path := Directory(path); fi; 
  # ensure that .xml is striped from name of main file
  if   Length(main) > 3 and main{[Length(main)-3..Length(main)]} = ".xml"
  then main := main{[1..Length(main)-4]}; fi;
  # compose the XML document
  Print("Composing XML document . . .\n");
  str := ComposedXMLString(path, Concatenation(main, ".xml"), files);
  # parse the XML document
  Print("Parsing XML document . . .\n");
  r := ParseTreeXMLString(str);
  # clean the result
  Print("Checking XML structure . . .\n");
  CheckAndCleanGapDocTree(r);
  # produce LaTeX version
  Print("LaTeX version and calling latex and pdflatex:\n    ");
  l := GAPDoc2LaTeX(r);
  l := Concatenation(Broken(l,100," ","\\",")}],."),"\n\n",
                     ListWithIdenticalEntries(100,'%'));
  Print("writing LaTeX file, \c");
  FileString(Filename(path, Concatenation(main, ".tex")), l);
  # call latex and pdflatex (with bibtex, makeindex and dvips)
  latex := "latex -interaction=nonstopmode ";
  null  := " &> /dev/null";
  Print("3 x latex, bibtex and makeindex, \c"); 
  Exec(Concatenation("sh -c \" cd ",Filename(path,""), 
                     "; ",latex,main,".tex",null,"; bibtex ",main,null,
                     "; ",latex,main,null,"; makeindex ",main,null,
                     "; ",latex,main,null,"; rm -f ",main,".aux\""));
  Print("2 x pdflatex, \c");
  Exec(Concatenation("sh -c \" cd ", Filename(path,""),
                     "; pdf",latex,main,null,"; pdf",latex,main,null,"\""));
  Print("dvips\n");
  Exec(Concatenation("sh -c \" cd ", Filename(path,""),
                     "; dvips -o ",main,".ps ",main,null,"; mv ",main,
                     ".dvi manual.dvi; mv ",main,".pdf manual.pdf; mv ",
                     main,".ps manual.ps; ","\""));
  # produce text version
  Print("Text version . . .\n");
  t := GAPDoc2Text(r, path);
  GAPDoc2TextPrintTextFiles(t,path);
  # read page number information for .six file
  Print("Writing manual.six file . . .\n");
  AddPageNumbersToSix(r,Filename(path,Concatenation(main,".pnr")));
  # print manual.six file
  PrintSixFile(Filename(path,"manual.six"),r,bookname);
  # produce html version
  Print("And finally the HTML version . . .\n");
  h := GAPDoc2HTML(r,path,gaproot);
  GAPDoc2HTMLPrintHTMLFiles(h,path);
  if "Tth" in htmlspecial then
    Print("  - also HTML version with 'tth' translated formulae . . .\n");
    h := GAPDoc2HTML(r,path,gaproot,"Tth");
    GAPDoc2HTMLPrintHTMLFiles(h,path);
  fi;
  if "MathML" in htmlspecial then
    Print("  - also HTML + MathML version with 'ttm' . . .\n");
    h := GAPDoc2HTML(r,path,gaproot,"MathML");
    GAPDoc2HTMLPrintHTMLFiles(h,path);
  fi;
  return r;
end );

#############################################################################
##
#F  ResetManualLayout( ) . . .  reset LaTeX manual layout to GAPDoc's default
##
##  Reset layout of LaTeX-/ DVI-/ Postscript-/ PDF-manuals to GAPDoc's
##  default.
##
ResetManualLayout := function ( )
  MakeReadWriteGlobal( "GAPDoc2LaTeXProcs" );
  GAPDoc2LaTeXProcs := GAPDoc2LaTeXProcsBuffer;
  MakeReadOnlyGlobal( "GAPDoc2LaTeXProcs" );
end;
MakeReadOnlyGlobal( "ResetManualLayout" );

#############################################################################
##
#E  manstyle.g . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
