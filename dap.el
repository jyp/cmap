;;; dap.el --- Do At Point  -*- lexical-binding: t -*-

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

;; Act on relevant object at point.  The main entry point is `dap-dap'.
;; The main configuration entry point is `dap-targets'.


;; Originally a fork of `embark', this is an almost complete rewrite focused on actions in regular buffers, instead of the minibuffer. 

;;; Code:

(require 'dash)
(require 's)
(require 'thingatpt)


(defmacro dap-keymap (&rest bindings)
  "Make keymap with given BINDINGS."
  (declare (indent 1))
  `(let ((map (make-sparse-keymap)))
     ,@(mapcar (pcase-lambda (`(,key ,fn))
                 `(define-key map ,key ,(if (symbolp fn) `#',fn fn)))
               bindings)
     map))


(defvar dap-region-map
  (dap-keymap
   ("h"         dap-hi-lock-region)
   ("u"         upcase-region)
   ("l"         downcase-region)
   ("c"         capitalize-region)
   ("|"         shell-command-on-region)
   ("e"         eval-region)
   ("i"         indent-rigidly)
   ((kbd "TAB") indent-region)
   ("f"         fill-region)
   ("p"         fill-region-as-paragraph)
   ("r"         rot13-region)
   ("="         count-words-region)
   ("s"         whitespace-cleanup-region)
   ("o"         org-table-convert-region)
   (";"         comment-or-uncomment-region)
   ("w"         write-region)
   ("m"         apply-macro-to-region-lines)
   ("N"         narrow-to-region))
  "Actions on the active region.")

(defun dap-cancel () (interactive))

(defvar dap-default-map
  (dap-keymap
   ((kbd "SPC") dap-cancel)  ) "Actions for anything")

(defun dap-default-target ()
  "Catchall target."
  (cons 'dap-default-map 'dap-no-arg))

(defvar dap-mc-map
  (dap-keymap
   ([up] mc/cycle-backward)
   ([down] mc/cycle-forward)
   ("\\"  mc/vertical-align-with-space)  ) "Actions when multiple cursors are active")

(defun dap-mc-target ()
  "Multiple cursors target."
  (when (and (bound-and-true-p multiple-cursors-mode) (> (mc/num-cursors) 1))
    (cons 'dap-mc-map 'dap-no-arg)))

(defun dap-region-target ()
  "Region target."
  (when (use-region-p) (cons 'dap-region-map 'dap-no-arg)))

(defvar dap-xref-identifier-map
  (dap-keymap
   ([return] xref-find-definitions)
   ([backspace] xref-find-references)  ) "Actions for xref identifiers")

(defun dap-target-identifier ()
  "Identify Xref identifier."
  (when (derived-mode-p 'prog-mode)
    (when-let* ((backend (xref-find-backend))
                (def (xref-backend-identifier-at-point backend)))
      (cons 'dap-xref-identifier-map def))))

(defvar dap-url-map
  (dap-keymap
   ([return] org-open-link-from-string)  ) "Actions for url")

(defun dap-target-url ()
  "Target the URL at point."
  (when-let ((url (thing-at-point 'url)))
    (cons 'dap-url-map url)))

(defvar dap-org-link-map
  (dap-keymap
   ([return] org-open-link-from-string)  ) "Keymap for Dap org link actions.")

(defun dap-target-org-link ()
  "Org-mode link target."
  (when (and (eq major-mode 'org-mode)
             (org-in-regexp org-any-link-re))
    (cons 'dap-org-link-map (match-string-no-properties 0))))

(defvar dap-flymake-diagnostics-map
  (dap-keymap
   ([return] attrap-flymake)
   ("a" flymake-show-diagnostics-buffer)
   ("t" flymake-show-diagnostic)
   ("n" flymake-goto-next-error)
   ("p" flymake-goto-prev-error)  ) "Keymap for Dap flymake diagnostics actions.")

(defun dap-target-flymake-diagnostics ()
  "Identify flymake diagnostics."
  (when (and (fboundp 'flymake-diagnostics) (flymake-diagnostics (point)))
    (cons 'dap-flymake-diagnostics-map 'dap-no-arg)))

(defvar dap-flyspell-map
  (dap-keymap
   ([return] ispell-word)
   ("b" flyspell-buffer)  ) "Keymap for Flyspell error")

(defun dap-flyspell-target ()
  "Identify Flyspell error."
  (when (and (bound-and-true-p flyspell-mode)
             (-any #'flyspell-overlay-p (overlays-at (point))))
    (cons 'dap-flyspell-map 'dap-no-arg)))

(defvar dap-flycheck-map
  (dap-keymap
   ([return] attrap-flycheck)  ) "Keymap for Flycheck error")

(defun dap-flycheck-target ()
  "Identify Flycheck message."
  (when (and (bound-and-true-p flycheck-mode)
             (flycheck-overlays-at (point)))
    (cons 'dap-flycheck-map (point))))


(defvar dap-hi-lock-regexp-map
  (dap-keymap
   ("h" hi-lock-unface-buffer)
   ("n" re-search-forward)
   ("p" re-search-backward)  ) "Actions for hi-lock regexps")

(defun dap-hi-lock-regexp-target ()
  (when-let* ((name (thing-at-point 'symbol))
	      (patterns (bound-and-true-p hi-lock-interactive-patterns))
	      (pos (point))
	      (matched (--first (thing-at-point-looking-at (car it)) patterns)))
    (cons 'dap-hi-lock-regexp-map (car matched))))

(defun dap-hi-lock-symbol (sym)
  (interactive)
  (require 'hi-lock)
  (hi-lock-set-pattern
   (format "\\_<%s\\_>" (regexp-quote sym))
   (hi-lock-read-face-name)))

(defun dap-hi-lock-region (beg end)
  (interactive "r")
  (require 'hi-lock)
  (hi-lock-set-pattern
   (regexp-quote (buffer-substring-no-properties beg end)) 
   (hi-lock-read-face-name)))

(defun dap-symbol-next (sym)
  (interactive)
  (forward-char)
  (re-search-forward (format "\\_<%s\\_>" (regexp-quote sym))))

(defun dap-symbol-prev (sym)
  (interactive)
  (backward-char)
  (re-search-backward (format "\\_<%s\\_>" (regexp-quote sym))))

(defvar dap-symbol-map
  (dap-keymap
   ("i" info-lookup-symbol)
   ("p" dap-symbol-prev)
   ("n" dap-symbol-next)
   ("h" dap-hi-lock-symbol)  ) "Actions for symbols")

(defun dap-symbol-target ()
  "Identify symbol."
  (when (derived-mode-p 'prog-mode)
    (when-let* ((name (thing-at-point 'symbol)))
      (cons 'dap-symbol-map name))))

(defvar dap-command-map
  (dap-keymap
   ("k" where-is)
   ("I" Info-goto-emacs-command-node)  ) "Actions for commands")

(defun dap-target-command ()
  "Identify command."
  (when-let* ((name (thing-at-point 'symbol))
              (sym (intern-soft name)))
    (when (commandp sym)
      (cons 'dap-command-map sym))))

(defvar dap-face-map
  (dap-keymap
   ("f" describe-face)
   ("c" customize-face)  ) "Actions for faces")

(defun dap-face-target ()
  "Identify a face target."
  (when-let* ((name (thing-at-point 'symbol))
              (sym (intern-soft name)))
    (when (facep sym)
      (cons 'dap-face-map sym))))

(defvar dap-function-map
  (dap-keymap
   ("f" describe-function)  ) "Actions for functions or macros")

(defun dap-target-function ()
  "Identify a function or macro target."
  (when-let* ((name (thing-at-point 'symbol))
              (sym (intern-soft name)))
    (when (or (functionp sym) (macrop sym))
      (cons 'dap-function-map sym))))

(defvar dap-option-map
  (dap-keymap
   ("c" customize-option)  ) "Actions for options")

(defun dap-option-target ()
  "Identify a customize option."
  (when-let* ((name (thing-at-point 'symbol))
              (sym (intern-soft name)))
    (when (custom-variable-p sym)
      (cons 'dap-option-map sym))))

(defvar dap-variable-map
  (dap-keymap
   ("v" describe-variable)
   ("e" symbol-value)  ) "Actions for variables")

(defun dap-variable-target ()
  "Identify a variable target."
  (when-let* ((name (thing-at-point 'symbol))
              (sym (intern-soft name)))
    (when (boundp sym)
      (cons 'dap-variable-map sym))))

(defvar dap-org-timestamp-map
  (dap-keymap
   ([return] org-time-stamp)
   ("+" org-timestamp-up)
   ("=" org-timestamp-up)
   ("-" org-timestamp-down)  ) "Actions for timestamps")

(defun dap-target-org-timestamp ()
  "Identify a timestamp target."
  (when (and (fboundp 'org-at-timestamp-p) (org-at-timestamp-p 'lax))
    (cons 'dap-org-timestamp-map 'dap-no-arg)))

(defvar dap-org-table-map
  (dap-keymap
      ([return] org-table-recalculate)
   ("^" org-table-sort-lines)
   ("c" org-table-insert-column)
   ("C" org-table-delete-column)
   ("r" org-table-insert-row)
   ("R" org-table-kill-row)
   ("h" org-table-insert-hline)
   ([(shift right)] org-table-move-column-right)
   ([(shift left)] org-table-move-column-left)
   ([(shift up)] org-table-move-row-up)
   ([(shift down)] org-table-move-row-down)  ) "Actions for org tables")

(defun dap-org-table-target ()
  "Identify an org-table target."
  (when (and (fboundp 'org-at-table-p) (org-at-table-p))
    (cons 'dap-org-table-map 'dap-no-arg)))

(defvar dap-outline-heading-map
  (dap-keymap
   ([return] org-show-subtree)
   ("^" org-sort-entries)
   ("<" org-promote-subtree)
   (">" org-demote-subtree)
   ("n" outline-forward-same-level)
   ("p" outline-backward-same-level)
   ([(shift up)] org-move-subtree-up)
   ([(shift down)] org-move-subtree-down)
   ("x" org-cut-subtree)
   ("c" org-copy-subtree)
   ("N" org-narrow-to-subtree)
   ("t" org-todo)) "Actions for outline headings")

(defun dap-outline-heading-target ()
  "Identify an outline heading target."
  (when (and (derived-mode-p 'outline-mode)
             (fboundp 'outline-on-heading-p) (outline-on-heading-p))
    (cons 'dap-outline-heading-map 'dap-no-arg)))

(defvar dap-org-item-map
  (dap-keymap
    ([return] org-list-repair)
    ([S-right] org-cycle-list-bullet)
    ([S-down] org-move-item-down)
    ([S-up] org-move-item-up)
    ([up] org-previous-item)
    ([down] org-next-item)
    ("'" org-insert-item)
    ("y" org-list-make-subtree)
    ("," org-outdent-item)
    ("." org-indent-item)
    ("^" org-sort-list)
    ("*" org-toggle-heading)
    ("<" org-outdent-item-tree)
    (">" org-indent-item-tree))
  "Actions for org-items")


(defun dap-org-item-target ()
  "Identify an org item target."
  (when (and (derived-mode-p 'org-mode) (org-at-item-p))
    (cons 'dap-org-item-map 'dap-no-arg)))

(defvar dap-org-checkbox-map
  (dap-keymap
    ([return] org-toggle-checkbox)
    ("r" org-toggle-radio-button))
  "Actions for org checkboxes")

(defun dap-org-checkbox-target ()
  "Identify an org item target."
  (when (and (derived-mode-p 'org-mode) (org-at-item-checkbox-p))
    (cons 'dap-org-checkbox-map 'dap-no-arg)))

(defcustom dap-targets
  '(dap-mc-target
    dap-region-target
    dap-target-flymake-diagnostics
    dap-flycheck-target
    dap-flyspell-target
    dap-target-org-link
    dap-target-url
    dap-target-command
    dap-face-target
    dap-target-function
    dap-variable-target
    dap-option-target
    dap-hi-lock-regexp-target
    dap-symbol-target
    dap-org-checkbox-target
    dap-org-item-target
    dap-target-org-timestamp
    dap-org-table-target
    dap-outline-heading-target
    dap-target-identifier
    dap-default-target)
  "List of functions to determine the target in current context.
Each function should take no argument and return either nil to
indicate it found no target or a cons of the form (map-symbol
. target) where map-symbol is a symbol whose value is a keymap
with relevant actions and target is an argument to pass to
functions bound in map-symbol."
  :type 'hook
  :group 'dap)


(defun dap-traverse-binding (fct binding)
  "Apply FCT to the BINDING."
  (cond ((keymapp binding) (dap-traverse-keymap fct binding))
        ((functionp binding) (funcall fct binding)) ; also accept functions, not just commands
        (t (cons (car binding) (dap-traverse-binding fct (cdr binding))))))

(defun dap-traverse-keymap- (fct map)
  "Apply FCT to every command bound in MAP, which is assumed to be in canonical form."
  (if (symbolp map) (dap-traverse-keymap fct (symbol-value map))
    (cons 'keymap (-map (apply-partially 'dap-traverse-binding fct) (cdr map)))))

(defun dap-traverse-keymap (fct map)
  "Apply FCT to every command bound in MAP."
  (assert (keymapp map))
  (dap-traverse-keymap- fct (keymap-canonicalize map)))

(defun dap-maps ()
  "Invoke all `dap-targets' and return the composed maps.
The result is a cons of a composition of the applicable maps in
the current context, applied to the target, and the same actions
not applied to targets."
  (let* ((type-target-pairs (-non-nil (--map (funcall it) dap-targets)))
         (map-target-pairs
          (-map (pcase-lambda (`(,type . ,target)) (cons (symbol-value type) target))
                type-target-pairs))
         (unapplied-map (make-composed-keymap (-map 'car map-target-pairs)))
         (maps (-map (pcase-lambda (`(,map . ,target))
                       (if (eq target 'dap-no-arg) map
                         (dap-traverse-keymap
                          (lambda (cmd)
                            (lambda () (interactive) (funcall cmd target)))
                          map)))
                     map-target-pairs)))
    (cons (make-composed-keymap maps) unapplied-map)))

(defcustom dap-prompter
  (lambda (map) (which-key--show-keymap "Action?" map nil nil 'no-paging))
  "Keymap prompter.
Function taking one keymap argument, called before prompting for
action.  The keymap contains possible actions."
  :group 'dap
  :type 'symbol)

(defcustom dap-prompter-done 'which-key--hide-popup
  "Function to call to close the `dap-prompter'."
  :group 'dap
  :type 'symbol)

(defcustom dap-sticky-keys
  '("<" ">" 
    "," "." 
    [S-right] [right]
    [S-left] [left]
    [S-up] [up]
    [S-down] [down]
    "n" "p" []) "Keys which won't close the prompt to
    close (commands bound to these keys are expected to be
    repeated)" :group 'dap :type '(list key-sequence))

(defun dap--keep-pred ()
  "Should the transient map remain active?"
  (member (this-command-keys) dap-sticky-keys))

;;;###autoload
(defun dap-dap ()
  "Prompt for action on the thing at point.
Use `dap-targets' to configure what can be done and how."
  (interactive)
  (pcase-let ((`(,map . ,prompt) (dap-maps)))
    (funcall dap-prompter prompt)
    (set-transient-map map 'dap--keep-pred dap-prompter-done)))

;;;###autoload
(defun dap-default ()
  "Act on the thing at point.
Use `dap-targets' to configure what can be done.  Like `dap-dap',
but (use [return])."
  (interactive)
  (pcase-let ((`(,map . ,_prompt) (dap-maps)))
    (funcall (lookup-key map [return]))))

(provide 'dap)
;;; dap.el ends here
