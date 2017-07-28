;(***********************************************************************)
;(*                                                                     *)
;(*                                OCaml                                *)
;(*                                                                     *)
;(*         Jacques Garrigue, Ian T Zimmerman, Damien Doligez           *)
;(*                                                                     *)
;(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
;(*  en Automatique.  All rights reserved.  This file is distributed    *)
;(*  under the terms of the GNU General Public License.                 *)
;(*                                                                     *)
;(***********************************************************************)

;; caml-font: font-lock support for OCaml files
;; now with perfect parsing of comments and strings

(require 'font-lock)

(defvar caml-font-stop-face
  (progn
    (make-face 'caml-font-stop-face)
    (set-face-foreground 'caml-font-stop-face "White")
    (set-face-background 'caml-font-stop-face "Red")
    'caml-font-stop-face))

(defvar caml-font-doccomment-face
  (progn
    (make-face 'caml-font-doccomment-face)
    (set-face-foreground 'caml-font-doccomment-face "Red")
    'caml-font-doccomment-face))

(unless (facep 'font-lock-preprocessor-face)
  (defvar font-lock-preprocessor-face
    (copy-face 'font-lock-builtin-face
               'font-lock-preprocessor-face)))

(defconst caml-font-lock-keywords
  `(
;modules and constructors
   ("`?\\<[A-Z][A-Za-z0-9_']*\\>" . font-lock-function-name-face)
;definition
   (,(regexp-opt '("and" "as" "constraint" "class"
                   "exception" "external" "fun" "function" "functor"
                   "in" "inherit" "initializer" "let"
                   "method" "mutable" "module" "of" "private" "rec"
                   "type" "val" "virtual")
                 'words)
    . font-lock-type-face)
;blocking
   (,(regexp-opt '("begin" "end" "object" "sig" "struct") 'words)
    . font-lock-keyword-face)
;linenums
   ("# *[0-9]+" . font-lock-preprocessor-face)
;infix operators
   (,(regexp-opt '("asr" "land" "lor" "lsl" "lsr" "lxor" "mod") 'words)
    . font-lock-builtin-face)
;control
   (,(concat "[|#&]\\|->\\|"
             (regexp-opt '("do" "done" "downto" "else" "for" "if" "ignore"
                           "lazy" "match" "new" "or" "then" "to" "try"
                           "when" "while" "with")
                         'words))
    . font-lock-constant-face)
   ("\\<raise\\|failwith\\|invalid_arg\\>"
    . font-lock-comment-face)
;labels (and open)
   ("\\(\\([~?]\\|\\<\\)[a-z][a-zA-Z0-9_']*:\\)[^:=]"
    1 font-lock-variable-name-face)
   ("\\<\\(assert\\|open\\|include\\)\\>\\|[~?][ (]*[a-z][a-zA-Z0-9_']*"
    . font-lock-variable-name-face)))


(defun caml-font-syntactic-face (s)
  (let ((in-string  (nth 3 s))
        (in-comment (nth 4 s))
        (start      (nth 8 s)))
    (cond
     (in-string 'font-lock-string-face)
     (in-comment
      (save-excursion
        (goto-char start)
        (cond
         ((looking-at "(\\*\\*/\\*\\*)") 'caml-font-stop-face)
         ((looking-at "(\\*\\*[^*]")     'caml-font-doccomment-face)
         (t                              'font-lock-comment-face)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; In order to correctly fontify an OCaml buffer, it is necessary to
; lex the buffer to tell what is a comment and what is a string.
; We do this incrementally in a hook
; (font-lock-extend-after-change-region-function), which is called
; whenever the buffer changes.  It sets the syntax-table property
; on each beginning and end of chars, strings, and comments.

; This mode handles correctly all the strange cases in the following
; OCaml code.
;
; let l' _ = ();;
; let _' _ = ();;
; let l' = ();;
; let b2_' = ();;
; let a'a' = ();;
; let f2 _ _ = ();;
; let f3 _ _ _ = ();;
; let f' _ _ _ _ _ = ();;
; let hello = ();;
;
; (* ==== easy stuff ==== *)
;
; (* a comment *)
; (* "a string" in a comment *)
; (* "another string *)" in a comment *)
; (* not a string '"' in a comment *)
; "a string";;
; '"';;              (* not a string *)
;
; (* ==== hard stuff ==== *)
;
; l'"' not not a string ";;
; _'"' also not not a string";;
; f2 0l'"';;            (* not not not a string *)
; f2 0_'"';;            (* also not not not a string *)
; f3 0.0l'"' not not not not a string ";;
; f3 0.0_'"';;          (* not not not not not a string *)
; f2 0b01_'"';;         (* not not not a string *)
; f3 0b2_'"'  not not not not a string ";;
; f3 0b02_'"';;         (* not not not not not a string *)
; '\'';;   (* a char *)
; '
; ';;      (* a char *)
; '^M
; ';;      (* also a char [replace ^M with one CR character] *)
; a'a';;   (* not a char *)
; type '
; a' t = X;;   (* also not a char *)
;
; (* ==== far-out stuff ==== *)
;
;    f'"'" "*) print_endline "hello";;(* \"" ;;
; (* f'"'" "*) print_endline "hello";;(* \"" ;; *)


(defconst caml-font-ident-re
  (concat "[A-Za-z_\300-\326\330-\366\370-\377]"
          "[A-Za-z_\300-\326\330-\366\370-\377'0-9]*")
)

(defconst caml-font-int-re
  (concat "\\(0[xX][0-9A-Fa-f][0-9A-Fa-f_]*\\|0[oO][0-7][0-7_]*"
          "\\|0[bB][01][01_]*\\)[lLn]?")
)

; decimal integers are folded into the RE for floats to get longest-match
; without using posix-looking-at
(defconst caml-font-decimal-re
  "[0-9][0-9_]*\\([lLn]\\|\\.[0-9_]*\\)?\\([eE][+-]?[0-9][0-9_]*\\)?"
)

; match any ident or numeral token
(defconst caml-font-ident-or-num-re
  (concat caml-font-ident-re "\\|" caml-font-int-re "\\|" caml-font-decimal-re)
)

; match any char token
(defconst caml-font-char-re
  (concat "'\\(\015\012\\|[^\\']\\|"
          "\\(\\\\\\([\\'\"ntbr ]\\|[0-9][0-9][0-9]"
                    "\\|x[0-9A-Fa-f][0-9A-Fa-f]\\)\\)\\)'")
)

; match a quote followed by a newline
(defconst caml-font-quote-newline-re
  "'\\(\015\012\\|[\012\015]\\)"
)

; match any token or sequence of tokens that cannot contain a
; quote, double quote, a start of comment, or a newline
; note: this is only to go faster than one character at a time
(defconst caml-font-other-re
  "[^A-Za-z_0-9\012\015\300-\326\330-\366\370-\377'\"(]+"
)

; match any sequence of non-special characters in a comment
; note: this is only to go faster than one character at a time
(defconst caml-font-other-comment-re
  "[^(*\"'\012\015]+"
)

; match any sequence of non-special characters in a string
; note: this is only to go faster than one character at a time
(defconst caml-font-other-string-re
  "[^\\\"\012\015]"
)

; match a newline
(defconst caml-font-newline-re
  "\\(\015\012\\|[\012\015]\\)"
)

; Put the 'caml-font-state property with the given state on the
; character before pos.  Return nil if it was already there, t if not.
(defun caml-font-put-state (pos state)
  (if (equal state (get-text-property (1- pos) 'caml-font-state))
      nil
    (put-text-property (1- pos) pos 'caml-font-state state)
    t)
)

; Same as looking-at, but erase properties 'caml-font-state and
; 'syntax-table from the matched range
(defun caml-font-looking-at (re)
  (let ((result (looking-at re)))
    (when result
      (remove-text-properties (match-beginning 0) (match-end 0)
                              '(syntax-table nil caml-font-state nil)))
    result)
)

; Annotate the buffer starting at point in state (st . depth)
; Set the 'syntax-table property on beginnings and ends of:
; - strings
; - chars
; - comments
; Also set the 'caml-font-state property on each LF character that is
; not preceded by a single quote. The property gives the state of the
; lexer (nil or t) after reading that character.

; Leave the point at a point where the pre-existing 'caml-font-state
; property is consistent with the new parse, or at the end of the buffer.

; depth is the depth of nested comments at this point
;   it must be a non-negative integer
; st can be:
;   nil  -- we are in the base state
;   t    -- we are within a string

(defun caml-font-annotate (st depth)
  (let ((continue t))
    (while (and continue (not (eobp)))
      (cond
       ((and (equal st nil) (= depth 0)) ; base state, outside comment
        (cond
         ((caml-font-looking-at caml-font-ident-or-num-re)
          (goto-char (match-end 0)))
         ((caml-font-looking-at caml-font-char-re)
          (put-text-property (point) (1+ (point))
                             'syntax-table (string-to-syntax "|"))
          (put-text-property (1- (match-end 0)) (match-end 0)
                             'syntax-table (string-to-syntax "|"))
          (goto-char (match-end 0)))
         ((caml-font-looking-at caml-font-quote-newline-re)
          (goto-char (match-end 0)))
         ((caml-font-looking-at "\"")
          (put-text-property (point) (1+ (point))
                             'syntax-table (string-to-syntax "|"))
          (goto-char (match-end 0))
          (setq st t))
         ((caml-font-looking-at "(\\*")
          (put-text-property (point) (1+ (point))
                             'syntax-table (string-to-syntax "!"))
          (goto-char (match-end 0))
          (setq depth 1))
         ((looking-at caml-font-newline-re)
          (goto-char (match-end 0))
          (setq continue (caml-font-put-state (match-end 0) '(nil . 0))))
         ((caml-font-looking-at caml-font-other-re)
          (goto-char (match-end 0)))
         (t
          (remove-text-properties (point) (1+ (point))
                                  '(syntax-table nil caml-font-state nil))
          (goto-char (1+ (point))))))
       ((equal st nil)                 ; base state inside comment
        (cond
         ((caml-font-looking-at "(\\*")
          (goto-char (match-end 0))
          (setq depth (1+ depth)))
         ((caml-font-looking-at "\\*)")
          (goto-char (match-end 0))
          (setq depth (1- depth))
          (when (= depth 0)
            (put-text-property (1- (point)) (point)
                               'syntax-table (string-to-syntax "!"))))
         ((caml-font-looking-at "\"")
          (goto-char (match-end 0))
          (setq st t))
         ((caml-font-looking-at caml-font-char-re)
          (goto-char (match-end 0)))
         ((caml-font-looking-at caml-font-quote-newline-re)
          (goto-char (match-end 0)))
         ((caml-font-looking-at "''")
          (goto-char (match-end 0)))
         ((looking-at caml-font-newline-re)
          (goto-char (match-end 0))
          (setq continue (caml-font-put-state (match-end 0) (cons nil depth))))
         ((caml-font-looking-at caml-font-other-comment-re)
          (goto-char (match-end 0)))
         (t
          (remove-text-properties (point) (1+ (point))
                                  '(syntax-table nil caml-font-state nil))
          (goto-char (1+ (point))))))
       (t                     ; string state inside or outside a comment
        (cond
         ((caml-font-looking-at "\"")
          (when (= depth 0)
            (put-text-property (point) (1+ (point))
                               'syntax-table (string-to-syntax "|")))
          (goto-char (1+ (point)))
          (setq st nil))
         ((caml-font-looking-at "\\\\[\"\\]")
          (goto-char (match-end 0)))
         ((looking-at caml-font-newline-re)
          (goto-char (match-end 0))
          (setq continue (caml-font-put-state (match-end 0) (cons t depth))))
         ((caml-font-looking-at caml-font-other-string-re)
          (goto-char (match-end 0)))
         (t
          (remove-text-properties (point) (1+ (point))
                                  '(syntax-table nil caml-font-state nil))
          (goto-char (1+ (point)))))))))
)

; This is the hook function for font-lock-extend-after-change-function
; It finds the nearest saved state at the left of the changed text,
; calls caml-font-annotate to set the 'caml-font-state and 'syntax-table
; properties, then returns the range that was parsed by caml-font-annotate.
(defun caml-font-extend-after-change (beg end &optional old-len)
  (save-excursion
    (save-match-data
      (let ((caml-font-modified (buffer-modified-p))
            start-at
            end-at
            state)
        (remove-text-properties beg end '(syntax-table nil caml-font-state nil))
        (setq start-at
              (or (and (> beg (point-min))
                       (get-text-property (1- beg) 'caml-font-state)
                       beg)
                  (previous-single-property-change beg 'caml-font-state)
                  (point-min)))
        (setq state (or (and (> start-at (point-min))
                             (get-text-property (1- start-at) 'caml-font-state))
                        (cons nil 0)))
        (goto-char start-at)
        (caml-font-annotate (car state) (cdr state))
        (setq end-at (point))
        (restore-buffer-modified-p caml-font-modified)
        (cons start-at end-at))))
)

; We don't use the normal caml-mode syntax table because it contains an
; approximation of strings and comments that interferes with our
; annotations.
(defconst caml-font-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?' "w" tbl)
    (modify-syntax-entry ?_ "w" tbl)
    (modify-syntax-entry ?\" "." tbl)
    (let ((i 192))
      (while (< i 256)
        (or (= i 215) (= i 247) (modify-syntax-entry i "w" tbl))
        (setq i (1+ i))))
    tbl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; font-lock commands are similar for caml-mode and inferior-caml-mode
(defun caml-font-set-font-lock ()
  (setq parse-sexp-lookup-properties t)
  (setq font-lock-defaults
        (list
         'caml-font-lock-keywords  ; keywords
         nil  ; keywords-only
         nil  ; case-fold
         nil  ; syntax-alist
         nil  ; syntax-begin
         (cons 'font-lock-syntax-table caml-font-syntax-table)
         '(font-lock-extend-after-change-region-function
           . caml-font-extend-after-change)
         '(font-lock-syntactic-face-function . caml-font-syntactic-face)
         ))
  (caml-font-extend-after-change (point-min) (point-max) 0)
  (font-lock-mode 1)
)
(add-hook 'caml-mode-hook 'caml-font-set-font-lock)



(defconst inferior-caml-font-lock-keywords
  `(("^[#-]" . font-lock-comment-face)
    ,@caml-font-lock-keywords))

(defun inferior-caml-set-font-lock ()
  (setq parse-sexp-lookup-properties t)
  (setq font-lock-defaults
        (list
         'inferior-caml-font-lock-keywords  ; keywords
         nil  ; keywords-only
         nil  ; case-fold
         nil  ; syntax-alist
         nil  ; syntax-begin
         (cons 'font-lock-syntax-table caml-font-syntax-table)
         '(font-lock-extend-after-change-region-function
           . caml-font-extend-after-change)
         '(font-lock-syntactic-face-function . caml-font-syntactic-face)
         ))
  (caml-font-extend-after-change (point-min) (point-max) 0)
  (font-lock-mode 1)
)
(add-hook 'inferior-caml-mode-hooks 'inferior-caml-set-font-lock)

(provide 'caml-font)
