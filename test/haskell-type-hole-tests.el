(defvar haskell-type-holes/test-path
  (f-dirname load-file-name))

(defvar haskell-type-holes/root-path
  (f-parent haskell-type-holes/test-path))

(defvar haskell-type-holes/hole-example1
  (f-read-text
   (concat haskell-type-holes/test-path
           "/hole-example1.txt")))

(defun hth/load-hole (n)
  (f-read-text
   (concat haskell-type-holes/test-path
           (format "%s%d%s" "/hole-example" n ".txt"))))

(defvar haskell-type-holes/hole-example2
  (f-read-text
   (concat haskell-type-holes/test-path
           "/hole-example2.txt")))

(load (f-expand "haskell-type-holes" haskell-type-holes/root-path))

(ert-deftest num-of-args-test-composition ()
  (should (= 3 (th--num-of-args "(.) :: (b -> c) -> (a -> b) -> a -> c"))))

(ert-deftest num-of-args-test-lens-view ()
  (should (= 1 (th--num-of-args "view
  :: Control.Monad.Reader.Class.MonadReader s m =>
     Getting a s a -> m a"))))

(ert-deftest get-type-of-binding-hole-1 ()
  (should (string= "a -> Identity a"
                   (th--get-hole-type (hth/load-hole 1)))))

(ert-deftest get-type-of-binding-hole-2 ()
  (should (string= "(a0 -> a0, Candidate -> Value) -> Candidate -> Candidate"
                   (th--get-hole-type (hth/load-hole 2)))))

(ert-deftest get-type-of-binding-hole-3 ()
  (should (string=
           "(p0 [RTerm] (f0 [RTerm]) -> p0 RExpression (f0 RExpression)) -> RExpression -> [Term]"
           (th--get-hole-type (hth/load-hole 3)))))

(ert-deftest annotate-binding-with-arg-holes ()
  (should (string= "fmap _ _"
             (th--binding-name-with-arg-holes
              "fmap :: Functor f => (a -> b) -> f a -> f b"))))

(ert-deftest add-parens-if-not-present ()
  (should (string= "(fmap)"
                   (th--maybe-add-parens "fmap"))))

(ert-deftest do-not-add-parens-if-present ()
  (should (string= "(fmap)"
                   (th--maybe-add-parens "(fmap)"))))

(defun replace-hole-sandbox (replacement &optional parens arg-holes)
  (with-temp-buffer
    (insert "_")
    (goto-char (point-min))
    (th--replace-type-hole replacement parens arg-holes)
    (buffer-substring (point-min) (point-max))))

(ert-deftest replace-type-hole-no-parens-no-args ()
  (should (string= "fmap"
                   (replace-hole-sandbox "fmap :: (a -> b) -> [a] -> [b]"))))

(ert-deftest replace-type-hole-parens-no-args ()
  (should (string= "(fmap)"
                   (replace-hole-sandbox "fmap :: (a -> b) -> [a] -> [b]" t nil))))

(ert-deftest replace-type-hole-parens-and-args ()
  (should (string= "(fmap _ _)"
                   (replace-hole-sandbox "fmap :: (a -> b) -> [a] -> [b]" t t))))

(ert-deftest replace-type-hole-parens-and-args ()
  (should (string= "fmap _ _"
                   (replace-hole-sandbox "fmap :: (a -> b) -> [a] -> [b]" nil t))))

(provide 'haskell-type-hole-tests)
