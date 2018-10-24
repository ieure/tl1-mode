;;; tl1-mode.el --- Fluke TL/1 mode                  -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018  Ian Eure

;; Author: Ian Eure <ian.eure@gmail.com>
;; Version: 0.6
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This is a major mode for editing Fluke TL/1 source code.

;;; Code:

(defun tl1-mode-word-opt* (&rest words)
  (concat "\\b" (regexp-opt words) "\\b"))

(defconst tl1-builtins
  (tl1-mode-word-opt*
   "getspace"
   "setspace"
   "sysspace"
   "podinfo"
   "sysdata"
   "sysaddr"
   "podsetup"
   "getpod"

   "testbus"
   "testramfast"
   "testramfull"
   "pretestram"
   "diagnoseram"
   "testromfull"
   "getromsig"
   "diagnoserom"

   "loadblock"
   "read"
   "readblock"
   "readstatus"
   "readvirtual"
   "write"
   "writeblock"
   "writecontrol"
   "writefill"
   "writevirtual"

   "rotate"
   "rampdata"
   "toggledata"
   "rampaddr"
   "toggleaddr"
   "togglecontrol"

   "runuut"
   "runuutspecial"
   "runuutvirtual"
   "waituut"
   "haltuut"
   "polluut"

   "counter"
   "edge"
   "enable"
   "reset"
   "stopcount"
   "sync"
   "threshold"

   "probe"
   "assign"
   "assoc"
   "clip"
   "connect"

   "arm"
   "checkstatus"
   "strobeclock"
   "readout"
   "pollbutton"
   "readbutton"

   "count"
   "level"
   "sig"

   "pulser"

   "clearoutputs"
   "clearpatt"
   "storepatt"
   "writepatt"
   "writepin"

   "compare"
   "getoffset"
   "setoffset"
   "restorecal"

   "readword"
   "setword"
   "writeword"

   "clockfreq"
   "drivepoll"
   "edgeoutput"
   "enableoutput"
   "strobeoutclock"
   "syncoutput"
   "vectordrive"
   "vectorload"

   "handle"
   "end handle"
   "exercise"
   "end exercise"
   "fault"
   "refault"

   "gfi accuse"
   "gfi autostart"
   "gfi clear"
   "gfi control"
   "gfi device"
   "gfi fail"
   "gfi hint"
   "gfi pass"
   "gfi ref"
   "gfi status"
   "gfi suggest"
   "gfi test"
   "dbquery"

   "random"
   "cwd"

   "clearpersvars"
   "resetpersvars"

   "wait"

   "passes"
   "fails"

   "readtime"
   "readdate"
   "systime"
   "sysinfo"

   "fabs"
   "sqrt"
   "pow"
   "log"
   "sin"
   "cos"
   "tan"
   "asin"
   "acos"
   "atan"
   "natural"

   "open"
   "close"
   "input"
   "input using"
   "print"
   "print using"
   "poll"
   "delete"
   "filestat"
   "ieee"
   "edisk"

   "define part"
   "remove part"
   "define ref"
   "remove ref"
   "define text"
   "remove text"
   "define mode"
   "remove mode"

   "winctl"
   "draw"
   "draw ref"
   "draw text"

   "define menu"
   "remove menu"
   "readmenu")
  "TL/1 built-in functions.")

(defconst tl1-operators
  (tl1-mode-word-opt* "and" "or" "shr")
  "TL/1 built-in operators.")

(defconst tl1-types
  (tl1-mode-word-opt*
   "numeric" "floating" "string" "array")
  "A list of all types known to TL/1.")

(defconst tl1-keywords
  (tl1-mode-word-opt*
   "if"
   "then"
   "else"
   "else if"
   "end if"
   "global"
   "loop until"
   "loop while"
   "loop for"
   "loop"
   "end loop"
   "for"
   "next"
   "program"
   "end program"
   "function"
   "end function"
   "declare"
   "end declare"
   "global"
   "abort"
   "execute"
   "return"
   "goto"
   "addr"
   "data"
   "mask")
  "TL/1 built-in keywords.")

(defvar tl1-mode-font-lock-keywords
  `(
    ;; Built-ins
    (,tl1-builtins . font-lock-builtin-face)
    (,tl1-operators . font-lock-builtin-face)

    (,tl1-keywords . font-lock-keyword-face)

    ;; Programs & functions
    ("\\b\\(function\\|program\\)\\s-+\\([a-z0-9_]+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))

    ;; Types
    (,tl1-types . font-lock-type-face)

    ;; Variables
    ("\\b\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-+=" (1 font-lock-variable-name-face))

    ;; Constants
    ("\\b\\([0-9]+\\|\$[0-9a-f]+\\)" . font-lock-constant-face))
  "Font-locking defintions for tl1-mode.")

(defvar tl1-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; A `!' is the start of a comment
    (modify-syntax-entry ?! "<" table)
    ;; The comment extends to the end of the line the `!' appears on.
    (modify-syntax-entry ?\n ">" table)
    table))

(defun tl1-mode-skipback ()
  "Move backwards to the first non-blank line, or beginning of buffer."
  (while (progn (forward-line -1)
                (and (not (bobp)) (looking-at "^\\s-*$")))))

(defun tl1-mode-line-type ()
  "Determine the type of the current line.

   Lines may be:
     :START     -- the beginning of an indented block.
     :END       -- the end of an indented block.
     :START-END -- End of the previous block, and the start of a new one.
     NIL        -- Not a block member, a normal statement."
  (cond
   ;; Start of a block
   ((or (looking-at "^\\s-*\\(program\\|function\\|loop\\)")
        (looking-at "^\\s-*declare\\s-*\\(!.*\\)?$")
        (looking-at "^\\s-*if.*then\\s-*\\(!.*\\)?$")
        (looking-at "^\\s-*arm\\s-+device\\s-+"))
    :start)

   ;; End of one block, start of another.
   ((looking-at "^\\s-*else") :start-end)

   ;; End of a block
   ((or (looking-at "^\\s-*end")
        (looking-at "^\\s-*readout\\s-+device\\s-+"))
    :end)
   (t nil)))

(defun tl1-mode-indent ()
  "Indent line for TL/1 mode."
  (interactive)
  (indent-line-to
   (save-excursion
     (save-match-data
       (let* ((case-fold-search t)
              (block (save-excursion (goto-char (line-beginning-position))
                                     (tl1-mode-line-type)))
              (last-line (save-excursion (tl1-mode-skipback)
                                         (cons
                                          (current-indentation)
                                          (tl1-mode-line-type))))
              (last-indent (car last-line))
              (last-line-type (cdr last-line)))
         (cond

          ;; Left-aligned at start of buffer
          ((bobp) 0)

          ((or
            ;; Normal statements align together
            (and (not block) (not last-line-type))

            ;; Blocks align together
            (and (eq :start block)
                 (eq :end last-line-type))

            (and (eq :end block)
                 (eq :start-end last-line-type)))

           last-indent)

          ((eq block :start-end) (- last-indent tab-width))

          ;; Last line was start of a block
          ((or (eq :start last-line-type)
               (eq :start-end last-line-type))
           (+ last-indent tab-width))

          ;; This line is the end of a block
          ((eq block :end) (- last-indent tab-width))

          ;; Catch-all
          (t last-indent)))))))


;;;###autoload
(define-derived-mode tl1-mode prog-mode "TL/1"
  "Major mode for editing Fluke TL/1 source code."
  (setq tab-width 4)
  (setq tab-stop-list '(2 0))
  (setq tab-always-indent t)

  (set-syntax-table tl1-mode-syntax-table)
  (setq comment-start "!")
  (setq comment-use-syntax t)
  (setq fill-prefix nil)
  (setq indent-line-function #'tl1-mode-indent)

  (set (make-local-variable 'font-lock-defaults)
       '(tl1-mode-font-lock-keywords nil t)))

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons "\\.tl1\\'" 'tl1-mode))

(provide 'tl1-mode)
;;; tl1-mode.el ends here
