;;; dap.el   -*- lexical-binding: t -*-

;; Copyright (C) 2021  Jean-Philippe Bernardy
;; Copyright (C) 2020  Omar Antolín Camarena

(require 'dash)
(require 'ffap) ; used it to recognize file and url targets

(defmacro dap-define-keymap (name doc &rest bindings)
  "Define keymap variable NAME.
DOC is the documentation string.
BINDINGS is the list of bindings."
  (declare (indent 1))
  (let* ((map (make-symbol "map"))
         (parent (if (eq :parent (car bindings)) (cadr bindings)))
         (bindings (if parent (cddr bindings) bindings)))
    `(defvar ,name
       (let ((,map (make-sparse-keymap)))
         ,@(mapcar (pcase-lambda (`(,key ,fn))
                     (when (stringp key) (setq key (kbd key)))
                     `(define-key ,map ,key ,(if (symbolp fn) `#',fn fn)))
                   bindings)
         ,(if parent `(make-composed-keymap ,map ,parent) map))
       ,doc)))

(dap-define-keymap dap-region-map
  "Actions on the active region."
  ("u" upcase-region)
  ("l" downcase-region)
  ("c" capitalize-region)
  ("|" shell-command-on-region)
  ("e" eval-region)
  ("i" indent-rigidly)
  ("TAB" indent-region)
  ("f" fill-region)
  ("p" fill-region-as-paragraph)
  ("r" rot13-region)
  ("=" count-words-region)
  ("s" whitespace-cleanup-region)
  ("o" org-table-convert-region)
  (";" comment-or-uncomment-region)
  ("w" write-region)
  ("m" apply-macro-to-region-lines)
  ("n" narrow-to-region))


(dap-define-keymap dap-xref-identifier-map
  "Actions for xref identifiers"
  ([return] xref-find-definitions)
  ([backspace] xref-find-references))

(defun dap-target-identifier ()
  "Identify Xref identifier"
  (when-let* ((backend (xref-find-backend))
              (def (xref-backend-identifier-at-point backend)))
    (cons 'dap-xref-identifier-map def)))

(dap-define-keymap dap-url-map
  "Actions for url"
  ([return] org-link-open-from-string))

(defun dap-target-url ()
  "Target the URL at point."
  (when-let ((url (thing-at-point 'url)))
    (cons 'dap-url-map url)))

(dap-define-keymap dap-org-link-map
  "Keymap for Dap org link actions."
  ([return] org-link-open-from-string))

(defun dap-target-org-link ()
  (when (and (eq major-mode 'org-mode)
             (org-in-regexp org-link-any-re))
    (cons 'dap-org-link-map (match-string-no-properties 0))))

(dap-define-keymap dap-flymake-diagnostics-map
  "Keymap for Dap flymake diagnostics actions."
  ([return] attrap-flymake-diags)
  ("a" flymake-show-diagnostics-buffer))

(defun dap-target-flymake-diagnostics ()
  "Identify flymake diagnostics"
  (when-let* ((diags (flymake-diagnostics (point))))
    (cons 'dap-flymake-diagnostics-map diags)))

(dap-define-keymap dap-symbol-map
  "Actions for symbols"
  ("i" info-lookup-symbol))

(defun dap-target-symbol ()
  "Identify symbol"
  (when (derived-mode-p 'prog-mode)
    (cons 'dap-symbol-map (thing-at-point 'symbol))))

(dap-define-keymap dap-command-map
  "Actions for commands"
  ("k" where-is)
  ("I" Info-goto-emacs-command-node))

(defun dap-target-command ()
  "Identify command"
  (when-let* ((name (thing-at-point 'symbol))
              (sym (intern-soft name)))
    (when (commandp sym)
        (cons 'dap-command-map sym))))

(dap-define-keymap dap-face-map
  "Actions for faces"
  ("f" describe-face)
  ("c" customize-face))

(defun dap-target-face ()
  "Identify faces"
  (when-let* ((name (thing-at-point 'symbol))
              (sym (intern-soft name)))
    (when (facep sym)
        (cons 'dap-face-map sym))))

(dap-define-keymap dap-function-map
  "Actions for functions"
  ("f" describe-function))

(defun dap-target-function ()
  "Identify functions"
  (when-let* ((name (thing-at-point 'symbol))
              (sym (intern-soft name)))
    (when (functionp sym)
        (cons 'dap-function-map sym))))

(dap-define-keymap dap-variable-map
  "Actions for variables"
  ("v" describe-variable)
  ("e" eval-symbol))

(defun dap-target-variable ()
  "Identify variables"
  (when-let* ((name (thing-at-point 'symbol))
              (sym (intern-soft name)))
    (when (boundp sym)
        (cons 'dap-variable-map sym))))

(defcustom dap-targets
  '(dap-target-flymake-diagnostics
    dap-target-org-link
    dap-target-url
    dap-target-identifier
    dap-target-command
    dap-target-face
    dap-target-function
    dap-target-variable)
  "List of functions to determine the target in current context.
Each function should take no argument and return either a cons
of the form (map-symbol . target) where map-symbol is a symbol
whose value is a keymap and target is a string, or nil to
indicate it found no target. Finde"
  :type 'hook)

(defun dap-traverse-keymap (fct map)
  (assert (keymapp dap-xref-identifier-map))
  (cons 'keymap
        (-map (pcase-lambda (`(,key . ,binding))
                (cons key (if (keymapp binding) (dap-traverse-keymap fct binding)
                            (funcall fct binding))))
              (cdr map))))

(defun dap-maps ()
  "Invoke all `dap-targets' and return the composed maps.
The result is a cons of the maps to use for looking up the
actions applied to the target, and the actions not applied to
targets."
  (if (use-region-p) (cons dap-region-map dap-region-map)
  (let* ((type-target-pairs (-non-nil (--map (funcall it) dap-targets)))
         (map-target-pairs
          (-map (pcase-lambda (`(,type . ,target)) (cons (symbol-value type) target))
                type-target-pairs))
         (unapplied-map (make-composed-keymap (-map 'car map-target-pairs)))
         (maps (-map (pcase-lambda (`(,map . ,target))
                       (dap-traverse-keymap
                        (lambda (cmd) (lambda () (interactive) (funcall cmd target)))
                         map))
                     map-target-pairs)))
    (cons (make-composed-keymap maps) unapplied-map))))

(defcustom dap-prompter
  (lambda (map) (which-key--show-keymap "Action?" map nil nil 'no-paging))
  "Keymap prompter.
Function taking one keymap argument, called before prompting for
action. The keymap contains possible actions."
  :group 'dap
  :type 'symbol)

(defcustom dap-prompter-done 'which-key--hide-popup
  "Function to call to close the `dap-prompter'."
  :group 'dap
  :type 'symbol)

(defun dap-dap ()
  "Prompt for action on the thing at point.
Use `dap-targets' to configure what can be done and how."
  (interactive)
  (pcase-let ((`(,map . ,prompt) (dap-maps)))
    (funcall dap-prompter prompt)
    (set-transient-map map nil dap-prompter-done)))

(defun dap-default ()
  "Act on the thing at point.
Use `dap-targets' to configure what can be done. Like `dap-dap',
but (use [(return)])."
  (interactive)
  (pcase-let ((`(,map . ,_prompt) (dap-maps)))
    (funcall (lookup-key map [(return)]))))

