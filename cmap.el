;;; cmap.el --- Contextual Menu At Point  -*- lexical-binding: t -*-

;; Copyright (C) 2021  Jean-Philippe Bernardy
;; Copyright (C) 2020  Omar Antolín Camarena

;; Author: Jean-Philippe Bernardy <jeanphilippe.bernardy@gmail.com>
;; Version: 0
;; Keywords: extensions, convenience

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Act on relevant object at point.  The main entry point is `cmap-cmap'.
;; The main configuration entry point is `cmap-targets'.


;; Originally a fork of `embark', this is an almost complete rewrite focused on actions in regular buffers, instead of the minibuffer. 

;;; Code:

(require 'dash)
(require 's)
(require 'thingatpt)
(require 'cl)

(defmacro cmap-keymap (&rest bindings)
  "Make keymap with given BINDINGS."
  (declare (indent 0))
  `(let ((map (make-sparse-keymap)))
     ,@(mapcar (pcase-lambda (`(,key . ,fn))
                 `(define-key map ,key ,(if (symbolp fn) `#',fn fn)))
               bindings)
     map))

(defvar cmap-region-map
  (cmap-keymap
   ;; ("h"         . cmap-hi-lock-region)
   ("u"         . upcase-region)
   ("l"         . downcase-region)
   ("c"         . capitalize-region)
   ("\\"        . align)
   ("|"         . align-regexp)
   ("e"         . eval-region)
   ("i"         . indent-rigidly)
   ((kbd "TAB") . indent-region)
   ("f"         . fill-region)
   ("p"         . fill-region-as-paragraph)
   ("r"         . rot13-region)
   ("="         . count-words-region)
   ("s"         . whitespace-cleanup-region)
   ("o"         . org-table-convert-region)
   (";"         . comment-or-uncomment-region)
   ("w"         . write-region)
   ("m"         . apply-macro-to-region-lines)
   ("N"         . narrow-to-region))
  "Actions on the active region.")

(defun cmap-cancel () (interactive))

(defvar cmap-default-map
  (cmap-keymap
   ((kbd "SPC") . cmap-cancel)) "Actions for anything")

(defun cmap-default-target ()
  "Catchall target."
  '(cmap-default-map . cmap-no-arg))

(defvar cmap-mc-map
  (cmap-keymap
   ([up]   . mc/cycle-backward)
   ([down] . mc/cycle-forward)
   ("\\"   . mc/vertical-align-with-space)  ) "Actions when multiple cursors are active")

(defun cmap-mc-target ()
  "Multiple cursors target."
  (when (and (bound-and-true-p multiple-cursors-mode) (> (mc/num-cursors) 1))
    '(cmap-mc-map . cmap-no-arg)))

(defun cmap-region-target ()
  "Region target."
  (when (use-region-p) (cons 'cmap-region-map 'cmap-no-arg)))

(defvar cmap-xref-identifier-map
  (cmap-keymap
   ([return]    . xref-find-definitions)
   ([backspace] . xref-find-references)  ) "Actions for xref identifiers")

(defun cmap-target-identifier ()
  "Identify Xref identifier."
  (when (derived-mode-p 'prog-mode)
    (when-let* ((backend (xref-find-backend))
                (def (xref-backend-identifier-at-point backend)))
      (cons 'cmap-xref-identifier-map def))))

(defvar cmap-url-map
  (cmap-keymap
   ([return] . browse-url)) "Actions for url")

(defun cmap-url-target ()
  "Target the url at point."
  (when-let ((url (thing-at-point 'url)))
    (cons 'cmap-url-map url)))

(defvar cmap-file-map
  (cmap-keymap
   ([return] . find-file)
   ("f"      . find-file))
  "Actions for file")

(defun cmap-file-target ()
  "Target the file at point."
  (when-let ((file (progn (require 'ffap) (ffap-file-at-point))))
    (cons 'cmap-file-map file)))

(defvar cmap-org-link-map
  (cmap-keymap
   ([return] . org-open-link-from-string)) "Keymap for Cmap org link actions.")

(defun cmap-target-org-link ()
  "Org-mode link target."
  (when (and (eq major-mode 'org-mode)
             (org-in-regexp org-any-link-re))
    (cons 'cmap-org-link-map (match-string-no-properties 0))))

(defvar cmap-flymake-diagnostics-map
  (cmap-keymap
   ([return] . attrap-flymake)
   ("a"      . flymake-show-diagnostics-buffer)
   ("t"      . flymake-show-diagnostic)
   ("n"      . flymake-goto-next-error)
   ("p"      . flymake-goto-prev-error)  ) "Keymap for Cmap flymake diagnostics actions.")

(defun cmap-target-flymake-diagnostics ()
  "Identify flymake diagnostics."
  (when (and (fboundp 'flymake-diagnostics) (flymake-diagnostics (point)))
    (cons 'cmap-flymake-diagnostics-map 'cmap-no-arg)))

(defvar cmap-flyspell-map
  (cmap-keymap
   ([return] . ispell-word)
   ("b" . flyspell-buffer)) "Keymap for Flyspell error")

(defun cmap-flyspell-target ()
  "Identify Flyspell error."
  (when (and (bound-and-true-p flyspell-mode)
             (-any #'flyspell-overlay-p (overlays-at (point))))
    (cons 'cmap-flyspell-map 'cmap-no-arg)))

(defvar cmap-flycheck-map
  (cmap-keymap
   ([return] . attrap-flycheck)  ) "Keymap for Flycheck error")

(defun cmap-flycheck-target ()
  "Identify Flycheck message."
  (when (and (bound-and-true-p flycheck-mode)
             (flycheck-overlays-at (point)))
    (cons 'cmap-flycheck-map (point))))


(defvar cmap-boon-hl-map
  (cmap-keymap
   ("h" . boon-hl-remove)
   ("n" . boon-hl-search)
   ([next] . boon-hl-search)
   ("p" . boon-hl-search-backward)
   ([prior] . boon-hl-search-backward)
   ) "Actions for boon-hl-patterns")

(defun cmap-boon-hl-target ()
  (when (bound-and-true-p boon-hl-patterns)
    (when-let* ((patterns (boon-hl-patterns-at-point)))
      (cons 'cmap-boon-hl-map (car patterns)))))

;; (defun cmap-hi-lock-region (beg end)
;;   (interactive "r")
;;   (require 'hi-lock)
;;   (hi-lock-set-pattern
;;    (regexp-quote (buffer-substring-no-properties beg end)) 
;;    (hi-lock-read-face-name)))

(defun cmap-symbol-next (sym)
  (interactive)
  (forward-char)
  (re-search-forward (format "\\_<%s\\_>" (regexp-quote sym))))

(defun cmap-symbol-prev (sym)
  (interactive)
  (backward-char)
  (re-search-backward (format "\\_<%s\\_>" (regexp-quote sym))))

(defvar cmap-symbol-map
  (cmap-keymap
   ("i" . info-lookup-symbol)
   ("p" . cmap-symbol-prev)
   ("n" . cmap-symbol-next)
   ("h" . boon-hl-symbol)  ) "Actions for symbols")

(defun cmap-symbol-target ()
  "Identify symbol."
  (when (derived-mode-p 'prog-mode)
    (when-let* ((name (thing-at-point 'symbol)))
      (cons 'cmap-symbol-map name))))

(defvar cmap-command-map
  (cmap-keymap
   ("k" . where-is)
   ("I" . Info-goto-emacs-command-node)  ) "Actions for commands")

(defun cmap-target-command ()
  "Identify command."
  (when-let* ((name (thing-at-point 'symbol))
              (sym (intern-soft name)))
    (when (commandp sym)
      (cons 'cmap-command-map sym))))

(defvar cmap-face-map
  (cmap-keymap
   ("da" . describe-face)
   ("ca" . customize-face)) "Actions for faces")

(defun cmap-face-target ()
  "Identify a face target."
  (when-let* ((name (thing-at-point 'symbol))
              (sym (intern-soft name)))
    (when (facep sym)
      (cons 'cmap-face-map sym))))

(defvar cmap-function-map
  (cmap-keymap
   ("df" . describe-function)) "Actions for functions or macros")

(defun cmap-target-function ()
  "Identify a function or macro target."
  (when-let* ((name (thing-at-point 'symbol))
              (sym (intern-soft name)))
    (when (or (functionp sym) (macrop sym))
      (cons 'cmap-function-map sym))))

(defvar cmap-option-map
  (cmap-keymap
   ("cv" . customize-option)  ) "Actions for emacs customize options")

(defun cmap-option-target ()
  "Identify a customize option."
  (when-let* ((name (thing-at-point 'symbol))
              (sym (intern-soft name)))
    (when (custom-variable-p sym)
      (cons 'cmap-option-map sym))))

(defvar cmap-variable-map
  (cmap-keymap
   ("dv" . describe-variable)
   ("e" . symbol-value)  ) "Actions for variables")

(defun cmap-variable-target ()
  "Identify a variable target."
  (when-let* ((name (thing-at-point 'symbol))
              (sym (intern-soft name)))
    (when (boundp sym)
      (cons 'cmap-variable-map sym))))

(defvar cmap-org-timestamp-map
  (cmap-keymap
   ([return] . org-time-stamp)
   ("+"      . org-timestamp-up)
   ("="      . org-timestamp-up)
   ("-"      . org-timestamp-down)  ) "Actions for timestamps")

(defun cmap-org-timestamp-target ()
  "Identify a timestamp target."
  (when (and (fboundp 'org-at-timestamp-p) (org-at-timestamp-p 'lax))
    (cons 'cmap-org-timestamp-map 'cmap-no-arg)))

(defvar cmap-org-table-map
  (cmap-keymap
    ([return]        . org-table-recalculate)
    ("s"             . org-table-sort-lines)
    ("c"             . org-table-insert-column)
    ("C"             . org-table-delete-column)
    ("r"             . org-table-insert-row)
    ("R"             . org-table-kill-row)
    ("h"             . org-table-insert-hline)
    ([(shift right)] . org-table-move-column-right)
    ([(shift left)]  . org-table-move-column-left)
    ([(shift up)]    . org-table-move-row-up)
    ([(shift down)]  . org-table-move-row-down)  ) "Actions for org tables")

(defun cmap-org-table-target ()
  "Identify an org-table target."
  (when (and (eq major-mode 'org-mode) (fboundp 'org-at-table-p) (org-at-table-p))
    (cons 'cmap-org-table-map 'cmap-no-arg)))

(defvar cmap-outline-heading-map
  (cmap-keymap
   ([return]       . org-show-subtree)
   ("^"            . org-sort-entries)
   ("<"            . org-promote-subtree)
   (">"            . org-demote-subtree)
   ("n"            . outline-forward-same-level)
   ("p"            . outline-backward-same-level)
   ([(shift up)]   . org-move-subtree-up)
   ([(shift down)] . org-move-subtree-down)
   ("x"            . org-cut-subtree)
   ("c"            . org-copy-subtree)
   ("N"            . org-narrow-to-subtree)
   ("t"            . org-todo)) "Actions for outline headings")

(defun cmap-outline-heading-target ()
  "Identify an outline heading target."
  (when (and (derived-mode-p 'outline-mode)
             (fboundp 'outline-on-heading-p) (outline-on-heading-p))
    (cons 'cmap-outline-heading-map 'cmap-no-arg)))

(defvar cmap-org-item-map
  (cmap-keymap
    ([return]  . org-list-repair)
    ([S-right] . org-cycle-list-bullet)
    ([S-down]  . org-move-item-down)
    ([S-up]    . org-move-item-up)
    ([up]      . org-previous-item)
    ([down]    . org-next-item)
    ("'"       . org-insert-item)
    ("y"       . org-list-make-subtree)
    (","       . org-outdent-item)
    ("."       . org-indent-item)
    ("^"       . org-sort-list)
    ("*"       . org-toggle-heading)
    ("<"       . org-outdent-item-tree)
    (">"       . org-indent-item-tree))
  "Actions for org-items")

(defun cmap-org-item-target ()
  "Identify an org item target."
  (when (and (derived-mode-p 'org-mode) (org-at-item-p))
    (cons 'cmap-org-item-map 'cmap-no-arg)))

(defvar cmap-org-latex-map
  (cmap-keymap
    ([return] . org-latex-preview))
  "Actions for org latex")

(defun cmap-org-latex-target ()
  "Identify an org latex target."
  (when (derived-mode-p 'org-mode)
    (when-let* ((datum (org-element-context)))
      (when (memq (org-element-type datum) '(latex-environment latex-fragment))
        (cons 'cmap-org-latex-map 'cmap-no-arg)))))


(defvar cmap-org-checkbox-map
  (cmap-keymap
    ([return] . org-toggle-checkbox)
    ("r" . org-toggle-radio-button))
  "Actions for org checkboxes")

(defun cmap-org-checkbox-target ()
  "Identify an org item target."
  (when (and (derived-mode-p 'org-mode) (org-at-item-checkbox-p))
    (cons 'cmap-org-checkbox-map 'cmap-no-arg)))

(defvar cmap-eshell-map
  (cmap-keymap
    ([return] . eshell-send-input)
    ([down]   . eshell-next-input)
    ([up]     . eshell-previous-input)
    ([S-up]   . eshell-previous-prompt)
    ([S-down] . eshell-next-prompt))
  "Actions for eshell prompt")

(defun cmap-eshell-target ()
  "Identify an eshell prompt target"
  (when (and (derived-mode-p 'eshell-mode)
             (save-excursion (beginning-of-line) (looking-at eshell-prompt-regexp)))
    (cons 'cmap-eshell-map 'cmap-no-arg)))

(defvar cmap-elisp-sexp-map
  (cmap-keymap
    ([return] . pp-eval-last-sexp)
    ("m"      . pp-macroexpand-last-sexp))
  "Actions for elisp sexp")

(defun cmap-elisp-sexp-target ()
  "Identify an elisp sexp"
  (when (and (derived-mode-p 'emacs-lisp-mode)
             (looking-back ")"))
    (cons 'cmap-elisp-sexp-map 'cmap-no-arg)))

(defvar cmap-alignable-map
  (cmap-keymap
    ("aa"     . align-current))
  "Actions for alignable regions")

(defun cmap-alignable ()
  "Identify an alignable section"
  (require 'align)
  (when (-any
         (pcase-lambda (`(,identifier . ,props))
           (let-alist props
             (and
              (apply #'derived-mode-p (eval .modes)) ;; applicable mode
              (save-excursion
                (beginning-of-line)
                (and
                 ;; (case-fold-search (or .case-fold case-fold-search))
                 (if (functionp .regexp)
                     (funcall .regexp (line-end-position) nil)
                   (search-forward-regexp .regexp (line-end-position) t)))
                 (or (not .valid) ;; no extra validity condition
                     (eval .valid))))))
         align-rules-list)
    (cons 'cmap-alignable-map 'cmap-no-arg)))

(defcustom cmap-targets
  '(cmap-mc-target
    cmap-region-target
    cmap-alignable
    cmap-target-flymake-diagnostics
    cmap-flycheck-target
    cmap-flyspell-target
    cmap-eshell-target
    cmap-target-org-link
    cmap-url-target
    cmap-file-target
    cmap-target-command
    cmap-target-function
    cmap-variable-target
    cmap-face-target
    cmap-option-target
    cmap-boon-hl-target
    cmap-symbol-target
    cmap-org-latex-target
    cmap-org-checkbox-target
    cmap-org-item-target
    cmap-org-timestamp-target
    cmap-org-table-target
    cmap-outline-heading-target
    cmap-elisp-sexp-target
    cmap-target-identifier
    cmap-default-target)
  "List of functions to determine the target in current context.
Each function should take no argument and return either nil to
indicate it found no target or a cons of the form (map-symbol
. target) where map-symbol is a symbol whose value is a keymap
with relevant actions and target is an argument to pass to
functions bound in map-symbol."
  :type 'hook
  :group 'cmap)


(defun cmap-traverse-binding (fct binding)
  "Apply FCT to the tail of BINDING."
  (if (or (keymapp binding) (symbolp binding)) ;; some functions may be not loaded yet, so must use  symbolp
      (funcall fct binding)
    (cons (car binding) (cmap-traverse-binding fct (cdr binding)))))

(defun cmap-traverse-keymap- (fct map)
  "Apply FCT to everything  bound in MAP, not recursively.
MAP is assumed to be in canonical form."
  (if (symbolp map) (cmap-traverse-keymap-shallow fct (symbol-value map))
    (cons 'keymap (-map (apply-partially #'cmap-traverse-binding fct) (cdr map)))))

(defun cmap-traverse-keymap-shallow (map fct)
  (declare (indent 1))
  "Apply FCT to everthing bound in MAP, not recursively."
  (assert (keymapp map))
  (cmap-traverse-keymap- fct (keymap-canonicalize map)))

(defun cmap-traverse-keymap (fct map)
  "Apply FCT to every command bound in MAP, recursively."
  (cmap-traverse-keymap-shallow map
   (lambda (x)
     (if (keymapp x) (cmap-traverse-keymap fct x)
       (funcall fct x)))))

(defun cmap-shorten-keymap (map)
  "Simplify MAP.
Replace branches with a single subnode by that subnode."
  (if (keymapp map)
      (cmap-traverse-keymap-shallow map
        (lambda (x) (pcase (cmap-shorten-keymap x)
                      (`(keymap (,key . ,binding))  binding)
                      (_ x))))
    map))

(defun cmap--compose-maps (maps)
  (cmap-shorten-keymap (make-composed-keymap maps)))


(defun cmap-maps ()
  "Invoke all `cmap-targets' and return the composed maps.
The result is a cons of a composition of the applicable maps in
the current context, applied to the target, and the same actions
not applied to targets."
  (let* ((type-target-pairs (-non-nil (--map (funcall it) cmap-targets)))
         (map-target-pairs
          (-map (pcase-lambda (`(,type . ,target)) (cons (symbol-value type) target))
                type-target-pairs))
         (unapplied-map (cmap--compose-maps (-map 'car map-target-pairs)))
         (maps (-map (pcase-lambda (`(,map . ,target))
                       (if (eq target 'cmap-no-arg) map
                         (cmap-traverse-keymap
                          (lambda (cmd)
                            (lambda () (interactive) (funcall cmd target)))
                          map)))
                     map-target-pairs)))
    (cons (cmap--compose-maps maps) unapplied-map)))


(defcustom cmap-prompter
  (lambda (map) (which-key--show-keymap "Action?" map nil nil 'no-paging))
  "Keymap prompter.
Function taking one keymap argument, called before prompting for
action.  The keymap contains possible actions."
  :group 'cmap
  :type 'symbol)

(defcustom cmap-prompter-done 'which-key--hide-popup
  "Function to call to close the `cmap-prompter'."
  :group 'cmap
  :type 'symbol)

(defcustom cmap-sticky-keys
  '("<" ">" 
    "," "." 
    "+" "-"
    [S-right] [right]
    [S-left] [left]
    [S-up] [up]
    [S-down] [down]
    [prior] [next]
    "n" "p" []) "Keys which won't close the prompt to
    close (commands bound to these keys are expected to be
    repeated)" :group 'cmap :type '(list key-sequence))

(defun cmap--keep-pred ()
  "Should the transient map remain active?"
  (member (this-command-keys) cmap-sticky-keys))

;;;###autoload
(defun cmap-cmap ()
  "Prompt for action on the thing at point.
Use `cmap-targets' to configure what can be done and how."
  (interactive)
  (pcase-let ((`(,map . ,prompt) (cmap-maps)))
    (funcall cmap-prompter prompt)
    (set-transient-map map 'cmap--keep-pred cmap-prompter-done)))

(defun cmap-lucky (KEY)
  "Act on the thing at point using KEY sequence.
Use `cmap-targets' to configure what can be done.  Like `cmap-cmap',
but use KEY directly."
  (interactive)
  (pcase-let ((`(,map . ,_prompt) (cmap-maps)))
    (funcall (lookup-key map KEY))))

;;;###autoload
(defun cmap-default ()
  "Act on the thing at point using the return key."
  (interactive)
  (cmap-lucky [return]))

;;;###autoload
(defun cmap-prior ()
  "Act on the thing at point using the return key."
  (interactive)
  (cmap-lucky [prior]))

;;;###autoload
(defun cmap-next ()
  "Act on the thing at point using the return key."
  (interactive)
  (cmap-lucky [next]))


(provide 'cmap)
;;; cmap.el ends here
