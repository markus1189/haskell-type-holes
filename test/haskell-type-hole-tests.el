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

(provide 'haskell-type-hole-tests)
