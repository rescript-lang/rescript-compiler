;**************************************************************************
;*                                                                        *
;*                                 OCaml                                  *
;*                                                                        *
;*             Didier Remy, projet Cristal, INRIA Rocquencourt            *
;*                                                                        *
;*   Copyright 2003 Institut National de Recherche en Informatique et     *
;*     en Automatique.                                                    *
;*                                                                        *
;*   All rights reserved.  This file is distributed under the terms of    *
;*   the GNU General Public License.                                      *
;*                                                                        *
;**************************************************************************

;; for caml-help.el
(defalias 'caml-info-other-window 'info-other-window)

;; for caml-types.el

(defalias 'caml-line-beginning-position 'line-beginning-position)

(defalias 'caml-read-event 'read-event)
(defalias 'caml-window-edges 'window-edges)
(defun caml-mouse-vertical-position ()
  (cddr (mouse-position)))
(defalias 'caml-ignore-event-p 'integer-or-marker-p)
(defalias 'caml-mouse-movement-p 'mouse-movement-p)
(defalias 'caml-sit-for 'sit-for)

(defalias 'caml-track-mouse 'track-mouse)

(defun caml-event-window (e) (posn-window (event-start e)))
(defun caml-event-point-start (e) (posn-point (event-start e)))
(defun caml-event-point-end (e) (posn-point (event-end e)))

(defun caml-release-event-p (original event)
  (and (equal (event-basic-type original) (event-basic-type event))
       (let ((modifiers  (event-modifiers event)))
         (or (member 'drag modifiers)
             (member 'click modifiers)))))

(defalias 'caml-string-to-int (if (fboundp 'string-to-number)
                                  'string-to-number 'string-to-int))

(provide 'caml-emacs)
