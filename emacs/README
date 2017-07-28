        OCaml emacs mode, snapshot of $Date$

The files in this archive define a caml-mode for emacs, for editing
OCaml and Objective Label programs, as well as an
inferior-caml-mode, to run a toplevel.

Caml-mode supports indentation, compilation and error retrieving,
sending phrases to the toplevel. Moreover support for hilit,
font-lock and imenu was added.

This package is based on the original caml-mode for caml-light by
Xavier Leroy, extended with indentation by Ian Zimmerman. For details
see README.itz, which is the README from Ian Zimmerman's package.

To use it, just put the .el files in your emacs load path, and add the
following lines in your .emacs.

    (add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . caml-mode))
    (autoload 'caml-mode "caml" "Major mode for editing OCaml code." t)
    (autoload 'run-caml "inf-caml" "Run an inferior OCaml process." t)
    (autoload 'camldebug "camldebug" "Run ocamldebug on program." t)
    (add-to-list 'interpreter-mode-alist '("ocamlrun" . caml-mode))
    (add-to-list 'interpreter-mode-alist '("ocaml" . caml-mode))

or put the .el files in, eg. "/usr/share/emacs/site-lisp/caml-mode/"
and add the following line in addtion to the four lines above:

    (add-to-list 'load-path "/usr/share/emacs/site-lisp/caml-mode")

To install the mode itself, edit the Makefile and do

    % make install

To install ocamltags, do

    % make install-ocamltags

To use highlighting capabilities, add ONE of the following two lines
to your .emacs.  The second one works better on recent versions of
emacs.

    (if window-system (require 'caml-hilit))
    (if window-system (require 'caml-font))

caml.el and inf-caml.el can be used collectively, but it might be a
good idea to copy caml-hilit.el or caml-font.el to you own directory,
and edit it to your taste and colors.

Main key bindings:

TAB indent current line
M-C-q   indent phrase
M-C-h   mark phrase
C-c C-a switch between interface and implementation
C-c C-c compile (usually make)
C-x`    goto next error (also mouse button 2 in the compilation log)

Once you have started caml by M-x run-caml:

M-C-x   send phrase to inferior caml process
C-c C-r send region to inferior caml process
C-c C-s show inferior caml process
C-c`    goto error in expression sent by M-C-x

For other bindings, see C-h b.

Changes log:
-----------
Version 3.10.1:
---------------
* use caml-font.el from Olivier Andrieu
  old version is left as caml-font-old.el for compatibility

Version 3.07:
-------------
* support for showing type information <Damien Doligez>

Version 3.05:
-------------
* improved interaction with inferior caml mode

* access help from the source

* fixes in indentation code

Version 3.03:
-------------
* process ;; properly

Version 3.00:
-------------
* adapt to new label syntax

* intelligent indentation of parenthesis

Version 2.02:
-------------
* improved ocamltags <ITZ and JG>

* added support for multibyte characters in emacs 20

Version 2.01+:
--------------
* corrected a bug in caml-font.el <Adam P. Jenkins>

* corrected abbreviations and added ocamltags script <Ian T Zimmerman>

Version 2.01:
------------
* code for interactive errors added by ITZ

Version 2.00:
------------
* changed the algorithm to skip comments

* adapted for the new object syntax

Version 1.07:
------------
* next-error bug fix by John Malecki

* camldebug.el modified by Xavier Leroy

Version 1.06:
------------
* new keywords in Objective Caml 1.06

* compatibility with GNU Emacs 20

* changed from caml-imenu-disable to caml-imenu-enable (off by default)

Version 1.05:
------------
* a few indentation bugs corrected. let, val ... are now indented
  correctly even when you write them at the beginning of a line.

* added a Caml menu, and Imenu support. Imenu menu can be disabled
  by setting the variable caml-imenu-disable to t.
  Xemacs support for the Menu, but no Imenu.

* key bindings closer to lisp-mode.

* O'Labl compatibility (":" is part of words) may be switched off by
  setting caml-olabl-disable to t.

* camldebug.el was updated by Xavier Leroy.

Version 1.03b:
-------------
* many bugs corrected.

* (partial) compatibility with Caml-Light added.
    (setq caml-quote-char "`")
    (setq inferior-caml-program "camllight")
  Literals will be correctly understood and highlighted. However,
  indentation rules are still OCaml's: this just happens to
  work well in most cases, but is only intended for occasional use.

* as many people asked for it, application is now indented. This seems
  to work well: this time differences in indentation between the
  compiler's source and this mode are really exceptionnal. On the
  other hand, you may think that some special cases are strange. No
  miracle.

* nicer behaviour when sending a phrase/region to the inferior caml
  process.

Version 1.03:
------------
* support of OCaml and Objective Label.

* an indentation very close to mine, which happens to be the same as
  Xavier's, since the sources of the OCaml compiler do not
  change if you indent them in this mode.

* highlighting.

Some remarks about the style supported:
--------------------------------------

Since OCaml's syntax is very liberal (more than 100
shift-reduce conflicts with yacc), automatic indentation is far from
easy. Moreover, you expect the indentation to be not purely syntactic,
but also semantic: reflecting the meaning of your program.

This mode tries to be intelligent. For instance some operators are
indented differently in the middle and at the end of a line (thanks to
Ian Zimmerman). Also, we do not indent after if .. then .. else, when
else is on the same line, to reflect that this idiom is equivalent to
a return instruction in a more imperative language, or after the in of
let .. in, since you may see that as an assignment.

However, you may want to use a different indentation style. This is
made partly possible by a number of variables at the beginning of
caml.el. Try to set them. However this only changes the size of
indentations, not really the look of your program. This is enough to
disable the two idioms above, but to do anything more you will have to
edit the code... Enjoy!

This mode does not force you to put ;; in your program. This means
that we had to use a heuristic to decide where a phrase starts and
stops, to speed up the code. A phrase starts when any of the keywords
let, type, class, module, functor, exception, val, external, appears
at the beginning of a line. Using the first column for such keywords
in other cases may confuse the phrase selection function.

Comments and bug reports to

    Jacques Garrigue <garrigue@math.nagoya-u.ac.jp>
