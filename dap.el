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

(defmacro dap-define-keymap (name doc &rest bindings)
  "Define keymap variable NAME.
DOC is the documentation string.
BINDINGS is the list of bindings."
  (declare (indent 1))
  (let* ((parent (if (eq :parent (car bindings)) (cadr bindings)))
         (bindings (if parent (cddr bindings) bindings)))
    `(defvar ,name
       (let ((map (make-sparse-keymap)))
         ,@(mapcar (pcase-lambda (`(,key ,fn))
                     `(define-key map ,key ,(if (symbolp fn) `#',fn fn)))
                   bindings)
         (set-keymap-parent map ,parent)
         map)
       ,doc)))

(defun dap-make-sticky (&rest commands)
  "Make COMMANDS repeatable with `dap-dap'."
  (dolist (cmd commands) (put cmd 'dap-sticky t)))

(dap-define-keymap dap-region-map
  "Actions on the active region."
  ("u" upcase-region)
  ("l" downcase-region)
  ("c" capitalize-region)
  ("|" shell-command-on-region)
  ("e" eval-region)
  ("i" indent-rigidly)
  ((kbd "TAB") indent-region)
  ("f" fill-region)
  ("p" fill-region-as-paragraph)
  ("r" rot13-region)
  ("=" count-words-region)
  ("s" whitespace-cleanup-region)
  ("o" org-table-convert-region)
  (";" comment-or-uncomment-region)
  ("w" write-region)
  ("m" apply-macro-to-region-lines)
  ("N" narrow-to-region))

(defun dap-region-target ()
  "Region target."
  (when (use-region-p) (cons dap-region-map 'dap-no-arg)))

(dap-define-keymap dap-xref-identifier-map
  "Actions for xref identifiers"
  ([return] xref-find-definitions)
  ([backspace] xref-find-references))

(defun dap-target-identifier ()
  "Identify Xref identifier."
  (when (derived-mode-p 'prog-mode)
    (when-let* ((backend (xref-find-backend))
                (def (xref-backend-identifier-at-point backend)))
      (cons 'dap-xref-identifier-map def))))

(dap-define-keymap dap-url-map
  "Actions for url"
  ([return] org-open-link-from-string))

(defun dap-target-url ()
  "Target the URL at point."
  (when-let ((url (thing-at-point 'url)))
    (cons 'dap-url-map url)))

(dap-define-keymap dap-org-link-map
  "Keymap for Dap org link actions."
  ([return] org-open-link-from-string))

(defun dap-target-org-link ()
  "Org-mode link target."
  (when (and (eq major-mode 'org-mode)
             (org-in-regexp org-any-link-re))
    (cons 'dap-org-link-map (match-string-no-properties 0))))

(dap-define-keymap dap-flymake-diagnostics-map
  "Keymap for Dap flymake diagnostics actions."
  ([return] attrap-flymake-diags)
  ("a" flymake-show-diagnostics-buffer)
  ("n" flymake-goto-next-error)
  ("p" flymake-goto-prev-error))

(dap-make-sticky 'flymake-goto-prev-error 'flymake-goto-next-error)

(defun dap-target-flymake-diagnostics ()
  "Identify flymake diagnostics."
  (when-let* ((diags (flymake-diagnostics (point))))
    (cons 'dap-flymake-diagnostics-map diags)))

(dap-define-keymap dap-flyspell-map
  "Keymap for Flyspell error"
  ([return] ispell-word)
  ("b" flyspell-buffer))

(defun dap-flyspell-target ()
  "Identify Flyspell error."
  (when (and (bound-and-true-p flyspell-mode)
             (-any #'flyspell-overlay-p (overlays-at (point))))
    (cons 'dap-flyspell-map 'dap-no-arg)))

(dap-define-keymap dap-flycheck-map
  "Keymap for Flycheck error"
  ([return] attrap-flycheck))

(defun dap-flycheck-target ()
  "Identify Flycheck message."
  (when (and (bound-and-true-p flycheck-mode)
             (flycheck-overlays-at (point)))
    (cons 'dap-flycheck-map (point))))

(dap-define-keymap dap-hi-lock-regexp-map
  "Actions for hi-lock regexps"
  ("h" hi-lock-unface-buffer)
  ("n" re-search-forward)
  ("p" re-search-backward))

(defun dap-hi-lock-regexp-target ()
  (when-let* ((name (thing-at-point 'symbol))
	      (patterns (bound-and-true-p hi-lock-interactive-patterns))
	      (pos (point))
	      (matched (--first (thing-at-point-looking-at (car it)))))
    (cons 'dap-hi-lock-regexp-map (car matched))))

(defun dap-hi-lock-symbol (sym)
  (interactive)
  (require 'hi-lock)
  (hi-lock-set-pattern
   (format "\\_<%s\\_>" (regexp-quote (symbol-name sym)))
   (hi-lock-read-face-name)))

(defun dap-symbol-next (sym)
  (interactive)
  (re-search-forward (regexp-quote (symbol-name sym))))

(defun dap-symbol-prev (sym)
  (interactive)
  (re-search-backward (regexp-quote (symbol-name sym))))

(dap-define-keymap dap-symbol-map
  "Actions for symbols"
  ("i" info-lookup-symbol)
  ("p" dap-symbol-prev)
  ("n" dap-symbol-next)
  ("h" dap-hi-lock-symbol))

(defun dap-symbol-target ()
  "Identify symbol."
  (when (derived-mode-p 'prog-mode)
    (when-let* ((name (thing-at-point 'symbol))
                (sym (intern-soft name)))
      (cons 'dap-symbol-map sym))))

(dap-define-keymap dap-command-map
  "Actions for commands"
  ("k" where-is)
  ("I" Info-goto-emacs-command-node))

(defun dap-target-command ()
  "Identify command."
  (when-let* ((name (thing-at-point 'symbol))
              (sym (intern-soft name)))
    (when (commandp sym)
        (cons 'dap-command-map sym))))

(dap-define-keymap dap-face-map
  "Actions for faces"
  ("f" describe-face)
  ("c" customize-face))

(defun dap-target-face ()
  "Identify a face target."
  (when-let* ((name (thing-at-point 'symbol))
              (sym (intern-soft name)))
    (when (facep sym)
        (cons 'dap-face-map sym))))

(dap-define-keymap dap-function-map
  "Actions for functions or macros"
  ("f" describe-function))

(defun dap-target-function ()
  "Identify a function or macro target."
  (when-let* ((name (thing-at-point 'symbol))
              (sym (intern-soft name)))
    (when (or (functionp sym) (macrop sym))
        (cons 'dap-function-map sym))))

(dap-define-keymap dap-option-map
  "Actions for options"
  ("c" customize-option))

(defun dap-option-target ()
  "Identify a customize option."
  (when-let* ((name (thing-at-point 'symbol))
              (sym (intern-soft name)))
    (when (custom-variable-p sym)
        (cons 'dap-option-map sym))))

(dap-define-keymap dap-variable-map
  "Actions for variables"
  ("v" describe-variable)
  ("e" symbol-value))

(defun dap-variable-target ()
  "Identify a variable target."
  (when-let* ((name (thing-at-point 'symbol))
              (sym (intern-soft name)))
    (when (boundp sym)
        (cons 'dap-variable-map sym))))

(dap-define-keymap dap-org-timestamp-map
  "Actions for timestamps"
  ([return] org-time-stamp)
  ("+" org-timestamp-up)
  ("=" org-timestamp-up)
  ("-" org-timestamp-down))

(dap-make-sticky 'org-timestamp-down 'org-timestamp-up)

(defun dap-target-org-timestamp ()
  "Identify a timestamp target."
    (when (and (fboundp 'org-at-timestamp-p) (org-at-timestamp-p 'lax))
        (cons 'dap-org-timestamp-map 'dap-no-arg)))

(dap-define-keymap dap-org-table-map
  "Actions for org tables"
  ("c" org-table-insert-column)
  ("C" org-table-delete-column)
  ("r" org-table-insert-row)
  ("R" org-table-kill-row)
  ("h" org-table-insert-hline)
  ([shift right] org-table-move-column-right)
  ([shift left] org-table-move-column-left)
  ([shift up] org-table-move-row-up)
  ([shift down] org-table-move-row-down))

(dap-make-sticky
 'org-table-move-row-down
 'org-table-move-row-up
 'org-table-move-column-left
 'org-table-move-column-right)

(defun dap-org-table-target ()
  "Identify an org-table target."
    (when (and (fboundp 'org-at-table-p) (org-at-table-p))
        (cons 'dap-org-table-map 'dap-no-arg)))

(dap-define-keymap dap-outline-heading-map
  "Actions for timestamps"
  ([return] org-show-subtree)
  ("<" org-promote-subtree)
  (">" org-demote-subtree)
  ("n" outline-forward-same-level)
  ("p" outline-backward-same-level)
  ([shift up] org-move-subtree-up)
  ([shift down] org-move-subtree-down)
  ("x" org-cut-subtree)
  ("c" org-copy-subtree)
  ("N" org-narrow-to-subtree)
  ("t" org-todo))

(defun dap-outline-heading-target ()
  "Identify a timestamp target."
    (when (and (derived-mode-p 'outline-mode)
               (fboundp 'outline-on-heading-p) (outline-on-heading-p))
        (cons 'dap-outline-heading-map 'dap-no-arg)))

(defcustom dap-targets
  '(dap-target-flymake-diagnostics
    dap-flycheck-target
    dap-flyspell-target
    dap-target-org-link
    dap-target-url
    dap-target-command
    dap-target-face
    dap-target-function
    dap-variable-target
    dap-option-target
    dap-hi-lock-regexp-target
    dap-symbol-target
    dap-target-org-timestamp
    dap-outline-heading-target
    dap-org-table-target
    dap-target-identifier
    dap-region-target)
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

(defun dap--keep-pred ()
  "Should the transient map remain active?"
  (and (symbolp this-command) (get this-command 'dap-sticky)))

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
