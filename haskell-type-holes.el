(defgroup haskell-type-holes nil "Haskell type holes functions.")

(define-minor-mode hs-type-holes-mode
  "Toggle type-hole mode."
  nil
  " ‘_’"
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k C-h") 'th-resolve-hole-hoogle-here)
    (define-key map (kbd "C-c C-k C-b") 'th-resolve-hole-relevant-bindings-here)
    (define-key map (kbd "C-c C-k C-f") 'th-resolve-hole-new-function-here)
    (define-key map (kbd "C-c C-k C-t") 'th-toggle-type-hole-undefined-line)
    (define-key map (kbd "C-c C-k TAB") 'th-resolve-hole-prompt)
    map))

(defun th--get-type-hole-info-flycheck ()
  (flycheck-error-message (car (flycheck-overlay-errors-at (point)))))

(defcustom th-get-type-hole-info-function 'th--get-type-hole-info-flycheck
  "Function to get the information about the type hole at point."
  :group 'haskell-type-holes
  :type '(choice
	  (function-item :tag "Use flycheck" th--get-type-hole-info-flycheck)
	  (function :tag "Custom function")))

(defcustom th-display-type-after-hole-insertion t
  "If t, use `message' to display the inserted binding with type."
  :group 'haskell-type-holes
  :type 'boolean)

(defcustom th-new-function-from-hole-position 'th--below-current-decl
  "Move the position where the new function template will be inserted."
  :group 'haskell-type-holes
  :type '(choice (function-item :tag "End of buffer" :value end-of-buffer)
                 (function-item :tag "Below current decl" :value th--below-current-decl)
                 (function :tag "Custom function")))

(defun th--get-hole-type (str)
  "Get the type of the hole described by STR."
  (string-match "Found hole ‘_’\\(\n\s*\\)? with type: \\([\0-\377[:nonascii:]]*\\)\n\s+Where:" str)
  (s-collapse-whitespace (replace-regexp-in-string "\n" "" (match-string 2 str))))

(defun th-insert-relevant-binding (str &optional add-parens add-arg-holes)
  (let* ((bindings (th-relevant-hole-bindings str))
         (binding (funcall completing-read-function "Which binding? " bindings)))
    (th--replace-type-hole binding add-parens add-arg-holes)))

(defun th-resolve-hole-relevant-bindings-here (arg)
  (interactive "p")
  (let ((add-parens (>= arg 4))
        (add-arg-holes (>= arg 16)))
    (th-insert-relevant-binding (funcall th-get-type-hole-info-function) add-parens add-arg-holes)))

(defun th-resolve-hole-new-function(str &optional add-parens add-arg-holes)
  (let* ((fn-name (read-string "Name for new function: "))
         (fn-with-type (concat fn-name
                               " :: "
                               (th--get-hole-type str)
                               "\n"
                               fn-name
                               " = _")))
    (th--replace-type-hole fn-with-type add-parens add-arg-holes)
    (funcall th-new-function-from-hole-position)
    (insert fn-with-type)
    (backward-char 1)
    (save-excursion
      (end-of-line)
      (newline 1)
      (delete-blank-lines))))

(defun th-resolve-hole-prompt (str)
  (interactive "sEnter replacement for hole: ")
  (th--replace-type-hole str))

(defun th-resolve-hole-new-function-here (arg)
  "With one prefix arg (C-u), insert parentheses, with two (C-u C-u) insert parens and argument holes"
  (interactive "p")
  (let ((add-parens (>= arg 4))
        (add-arg-holes (>= arg 16)))
    (th-resolve-hole-new-function
     (funcall th-get-type-hole-info-function) add-parens add-arg-holes)))

(defun th-relevant-hole-bindings (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (when (search-forward "Relevant bindings include" nil t)
      (forward-line 1)
      (back-to-indentation)
      (forward-char -1)
      (let* ((start-pos (point))
             (end-pos (progn (search-forward "In the" nil nil)
                             (forward-line -1)
                             (line-end-position)))
             (bindings (substring str start-pos end-pos))
             (cleaned-bindings (-filter (lambda (x) (not (s-blank? x)))
                                        (-map 's-trim
                                              (s-lines
                                               (replace-regexp-in-string "
[[:space:]]*(" " (" bindings))))))
        cleaned-bindings))))

(defun th--replace-type-hole (binding &optional add-parens add-arg-holes)
  (if (looking-at "_")
      (progn (delete-char 1)
             (insert (th--maybe-add-parens (if add-arg-holes
                         (th--binding-name-with-arg-holes binding)
                         (th--binding-extract-name binding)) (not add-parens)))
             (if th-display-type-after-hole-insertion
                 (message binding)))
    (message "Not on a type hole.")))

(defun th--maybe-add-parens (str &optional do-nothing)
  (if (or do-nothing
          (and (s-starts-with? "(" str)
               (s-ends-with? ")" str)))
      str
    (concat "(" str ")")))

(defun th--binding-name-with-arg-holes (binding)
  (let ((name (th--binding-extract-name binding)))
    (s-join " " `(,name ,@(-repeat (th--num-of-args binding) "_")))))

(defun th--create-arg-holes (fn-type)
  (s-join " " (-repeat (th--num-of-args fn-type) "_")))

(defun th--binding-extract-name (binding)
  (replace-regexp-in-string " :: .*" "" binding))

(defun th--type-of-hole (str)
  (car (s-lines (replace-regexp-in-string "Found .* type: " "" str))))

(defun th--num-of-args (binding)
  (s-count-matches " -> " (with-temp-buffer
                            (insert binding)
                            (while (progn (beginning-of-line)
                                          (search-forward "(" nil t))
                              (backward-char 1)
                              (kill-sexp))
                            (buffer-string))))

(defun th--below-current-decl ()
  (haskell-ds-forward-decl)
  (backward-word)
  (end-of-line)
  (newline 2))

(defun th-resolve-hole-hoogle (str)
  (let ((hole-type (th--get-hole-type str)))
    (funcall completing-read-function "What to insert: " (th-hoogle-search hole-type))))

(defun th-resolve-hole-hoogle-here ()
  (interactive)
  (th--replace-type-hole
   (th--hoogle-extract-name
    (th-resolve-hole-hoogle
     (funcall th-get-type-hole-info-function)))))

(defun th--hoogle-extract-name (str)
  "Given a hoogle result as STR, extract the function name without module name or type."
  (let ((result  (progn (string-match ".*? \\(.*?\\) ::" str)
                        (match-string 1 str))))
    result))

(defun th-hoogle-search (query)
  (with-temp-buffer
    (call-process "hoogle" nil t nil "search" "-n 30" query)
    (goto-char (point-min))
    (unless (looking-at "^No results found")
      (-filter (lambda (s)
                 (not (or (s-starts-with? "package" s)
                          (s-starts-with? "Warning: Unknown type" s))))
               (s-lines (buffer-string))))))

(defun th-toggle-type-hole-undefined-line ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (re-search-forward
            (rx bow (group (or "undefined" "_")) eow)
            (line-end-position) t)
      (let ((found (match-string 1))
            (toggle-alist '(("undefined" . "_") ("_" . "undefined"))))
        (when found
          (backward-kill-word 1)
          (insert (cdr (assoc found toggle-alist))))))))

(provide 'haskell-type-holes)
