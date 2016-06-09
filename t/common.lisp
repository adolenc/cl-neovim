(in-package :cl-neovim-tests)
(in-suite neovim-test-suite)


(test sample-test
  (is (= 6 (nvim:strwidth "ngあの"))))
