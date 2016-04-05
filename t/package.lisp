(in-package :cl-user)

(defpackage #:cl-neovim-tests
  (:use #:cl
        #:fiveam
        #:cl-neovim)
  (:shadowing-import-from #:cl #:defun))
