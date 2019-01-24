type action =
    Print of string
  | Print_arg
  | Skip_arg;;

let cmdtable = (Hashtbl.create 19 : (string, action list) Hashtbl.t);;

let def_macro name action =
  Hashtbl.add cmdtable name action;;

let find_macro name =
  try
    Hashtbl.find cmdtable name
  with Not_found ->
    prerr_string "Unknown macro: "; prerr_endline name; [];;

(* General LaTeX macros *)

def_macro "\\part"
    [Print "<H0>"; Print_arg; Print "</H0>\n"];
def_macro "\\chapter"
    [Print "<H1>"; Print_arg; Print "</H1>\n"];
def_macro "\\chapter*"
    [Print "<H1>"; Print_arg; Print "</H1>\n"];
def_macro "\\section"
    [Print "<H2>"; Print_arg; Print "</H2>\n"];
def_macro "\\section*"
    [Print "<H2>"; Print_arg; Print "</H2>\n"];
def_macro "\\subsection"
    [Print "<H3>"; Print_arg; Print "</H3>\n"];
def_macro "\\subsection*"
    [Print "<H3>"; Print_arg; Print "</H3>\n"];
def_macro "\\subsubsection"
    [Print "<H4>"; Print_arg; Print "</H4>\n"];
def_macro "\\subsubsection*"
    [Print "<H4>"; Print_arg; Print "</H4>\n"];
def_macro "\\paragraph"
    [Print "<B>"; Print_arg; Print "</B>&nbsp;&nbsp;\n"];
def_macro "\\begin{alltt}" [Print "<pre>"];
def_macro "\\end{alltt}" [Print "</pre>"];
def_macro "\\begin{itemize}" [Print "<p><ul>"];
def_macro "\\end{itemize}" [Print "</ul>"];
def_macro "\\begin{enumerate}" [Print "<p><ol>"];
def_macro "\\end{enumerate}" [Print "</ol>"];
def_macro "\\begin{description}" [Print "<p><dl>"];
def_macro "\\end{description}" [Print "</dl>"];
def_macro "\\begin{center}" [Print "<blockquote>"];
def_macro "\\end{center}" [Print "</blockquote>"];
def_macro "\\begin{quote}" [Print "<blockquote>"];
def_macro "\\end{quote}" [Print "</blockquote>"];
def_macro "\\begin{quotation}" [Print "<blockquote>"];
def_macro "\\end{quotation}" [Print "</blockquote>"];
def_macro "\\smallskip" [];
def_macro "\\medskip" [];
def_macro "\\bigskip" [];
def_macro "\\markboth" [Skip_arg; Skip_arg];
def_macro "\\ldots" [Print "..."];
def_macro "\\ " [Print " "];
def_macro "\\{" [Print "{"];
def_macro "\\}" [Print "}"];
def_macro "\\%" [Print "%"];
def_macro "\\$" [Print "$"];
def_macro "\\#" [Print "#"];
def_macro "\\/" [];
def_macro "\\newpage" [];
def_macro "\\label" [Print "<A name=\""; Print_arg; Print "\"></A>"];
def_macro "\\ref" [Print "<A href=\"#"; Print_arg; Print "\">X</A>"];
def_macro "\\pageref" [Print "<A href=\"#"; Print_arg; Print "\">X</A>"];
def_macro "\\index" [Skip_arg];
def_macro "\\oe" [Print "oe"];
def_macro "\\&" [Print "&amp;"];
def_macro "\\_" [Print "_"];
def_macro "\\leq" [Print "&lt;="];
def_macro "\\geq" [Print "&gt;="];
def_macro "\\hbox" [Print_arg];
def_macro "\\copyright" [Print "\169"];
def_macro "\\noindent" [];
def_macro "\\begin{flushleft}" [Print "<blockquote>"];
def_macro "\\end{flushleft}" [Print "</blockquote>"];
def_macro "\\\\" [Print "<br>"];
def_macro "\\begin{htmlonly}" [];
def_macro "\\end{htmlonly}" [];
();;

(* Macros specific to the Caml manual *)

def_macro "\\begin{options}" [Print "<p><dl>"];
def_macro "\\end{options}" [Print "</dl>"];
def_macro "\\var" [Print "<i>"; Print_arg; Print "</i>"];
def_macro "\\optvar" [Print "[<i>"; Print_arg; Print "</i>]"];
def_macro "\\nth" [Print "<i>"; Print_arg;
                   Print "</i><sub>"; Print_arg; Print "</sub>"];
def_macro "\\nmth" [Print "<i>"; Print_arg; 
                    Print "</i><sub>"; Print_arg;
                    Print "</sub><sup>"; Print_arg;
                    Print "</sup>"];
def_macro "\\begin{unix}" [Print "<dl><dt><b>Unix:</b><dd>"];
def_macro "\\end{unix}" [Print "</dl>"];
def_macro "\\begin{macos}" [Print "<dl><dt><b>MacOS:</b><dd>"];
def_macro "\\end{macos}" [Print "</dl>"];
def_macro "\\begin{windows}" [Print "<dl><dt><b>Windows:</b><dd>"];
def_macro "\\end{windows}" [Print "</dl>"];
def_macro "\\begin{requirements}" [Print "<dl><dt><b>Requirements:</b><dd>"];
def_macro "\\end{requirements}" [Print "</dl>"];
def_macro "\\begin{troubleshooting}" [Print "<dl><dt><b>Troubleshooting:</b><dd>"];
def_macro "\\end{troubleshooting}" [Print "</dl>"];
def_macro "\\begin{installation}" [Print "<dl><dt><b>Installation:</b><dd>"];
def_macro "\\end{installation}" [Print "</dl>"];
def_macro "\\index" [Skip_arg];
def_macro "\\ikwd" [Skip_arg];
def_macro "\\th" [Print "-th"];
def_macro "\\begin{library}" [];
def_macro "\\end{library}" [];
def_macro "\\begin{comment}" [Print "<dl><dd>"];
def_macro "\\end{comment}" [Print "</dl>"];
def_macro "\\begin{tableau}"
  [Skip_arg;
   Print "<table border>\n<tr><th>";
   Print_arg;
   Print "</th><th>";
   Print_arg;
   Print "</th></tr>"];
def_macro "\\entree"
  [Print "<tr><td>"; Print_arg;
   Print "</td><td>"; Print_arg; Print "</td></tr>"];
def_macro "\\end{tableau}" [Print "</table>"];
def_macro "\\begin{gcrule}" [Print "<dl><dt><b>Rule:</b><dd>"];
def_macro "\\end{gcrule}" [Print "</dl>"];
def_macro "\\begin{tableauoperateurs}"
  [Print "<table border>\n<tr><th>Operator</th><th>Associated ident</th><th>Behavior in the default environment</th></tr>"];
def_macro "\\end{tableauoperateurs}" [Print "</table>\n"];
def_macro "\\entreeoperateur"
  [Print "<tr><td>"; Print_arg; Print "</td><td>"; Print_arg;
   Print "</td><td>"; Print_arg; Print "</td></tr>"];
def_macro "\\fromoneto"
  [Print "<i>"; Print_arg; Print "</i> = 1, ..., <i>";
   Print_arg; Print "</i>"];
def_macro "\\caml" [Print "<pre>"];
def_macro "\\endcaml" [Print "</pre>"];
def_macro "\\<" [Print "<u>"];
def_macro "\\>" [Print "</u>"];
def_macro "\\rminalltt" [Print_arg];
def_macro "\\event" [Print "<font color=\"red\">*</font>"];
def_macro "\\pdfchapter" [Skip_arg];
def_macro "\\pdfchapterfold" [Skip_arg; Skip_arg];
def_macro "\\pdfsection" [Skip_arg];
def_macro "\\transl" [Print "<"; Print_arg; Print ">"];
();;

