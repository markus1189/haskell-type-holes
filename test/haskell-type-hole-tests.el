(defvar haskell-type-holes/test-path
  (f-dirname load-file-name))

(defvar haskell-type-holes/root-path
  (f-parent haskell-type-holes/test-path))

(load (f-expand "haskell-type-holes" haskell-type-holes/root-path))

(ert-deftest num-of-args-test ()
  (should (= 3 (th--num-of-args "(.) :: (b -> c) -> (a -> b) -> a -> c"))))

(ert-deftest num-of-args-test ()
  (should (= 1 (th--num-of-args "view
  :: Control.Monad.Reader.Class.MonadReader s m =>
     Getting a s a -> m a"))))

(provide 'haskell-type-hole-tests)
