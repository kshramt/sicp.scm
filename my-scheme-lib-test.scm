(use-modules (srfi srfi-64))

(use-modules (my-scheme-lib))

(test-begin "my-scheme-lib-test")


(test-assert (my-eval))
(test-assert (my-apply))


(test-end "my-scheme-lib-test")
