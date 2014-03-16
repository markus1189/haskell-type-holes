(defgroup haskell-type-holes nil "Haskell type holes functions.")

(defcustom th-get-type-hole-info-function
  (lambda ()
    (flycheck-error-message (car (flycheck-overlay-errors-at (point)))))
  "Function to get the information about the type hole at point."
  :group 'haskell-type-holes
  :type 'function)

(defcustom th-insert-argument-holes nil
  "If t, insert holes for the arguments of the binding inserted."
  :group 'haskell-type-holes
  :type 'boolean)

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

(defun th-new-function-from-hole (str &optional add-parens add-arg-holes)
  (let* ((fn-name (read-string "Name for new function: "))
         (fn-with-type (concat fn-name " :: " (th--get-hole-type str) "\n" fn-name " = _"))
         (fn-with-arg-holes (if add-arg-holes
                                (s-join " " (list fn-name
                                                  (th--create-arg-holes fn-with-type)))
                              fn-name)))
    (th--replace-type-hole (if add-parens
                               (concat "(" fn-with-arg-holes ")")
                             fn-with-arg-holes))
    (funcall th-new-function-from-hole-position)
    (insert fn-with-type)
    (backward-char 1)
    (save-excursion
      (end-of-line)
      (newline 1)
      (delete-blank-lines))))

(defun th-new-function-from-hole-here (arg)
  "With one prefix arg (C-u), insert parentheses, with two (C-u C-u) insert parens and argument holes"
  (interactive "p")
  (let ((add-parens (>= arg 4))
        (add-arg-holes (>= arg 16)))
    (th-new-function-from-hole (funcall th-get-type-hole-info-function) add-parens add-arg-holes)))

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


(defun th-insert-relevant-binding (str)
  (let* ((bindings (append '("undefined :: a")
                           (th-relevant-hole-bindings str)
                           '( "_ . _ :: composition" "_ $ _ :: application")))
         (binding (funcall completing-read-function "Which binding?: " bindings)))
    (th--replace-type-hole binding)))

(defun th--replace-type-hole (binding)
  (let ((name (th--binding-extract-name binding)))
    (if (looking-at "_")
        (progn (delete-char 1)
               (insert (if th-insert-argument-holes
                           (s-join " " `(,name
                                         ,@(-repeat (th--num-of-args binding) "_")))
                         name))
               (if th-display-type-after-hole-insertion
                   (message binding)))
      (message "Not on a type hole."))))

(defun th--create-arg-holes (fn-type)
  (s-join " " (-repeat (th--num-of-args fn-type) "_")))

(defun th-insert-relevant-binding-here ()
  (interactive)
  (th-insert-relevant-binding (funcall th-get-type-hole-info-function)))

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

(defun th-resolve-hole-via-hoogle (str)
  (let ((hole-type (th--get-hole-type str)))
    (funcall completing-read-function "What to insert: " (th-hoogle-search hole-type))))

(defun th-resolve-hole-via-hoogle-here ()
  (interactive)
  (th--replace-type-hole
   (th--hoogle-extract-name
    (th-resolve-hole-via-hoogle
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

;; (define-key haskell-mode-map (kbd "C-c C-k C-h") 'th-resolve-hole-via-hoogle-here)
;; (define-key haskell-mode-map (kbd "C-c C-k C-b") 'th-insert-relevant-binding-here)
;; (define-key haskell-mode-map (kbd "C-c C-k C-f") 'th-new-function-from-hole-here)
;; (define-key haskell-mode-map (kbd "C-c C-k C-t") 'th-toggle-type-hole-undefined-line)

(provide 'haskell-type-holes)
