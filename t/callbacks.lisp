(in-package :cl-neovim-tests)
(in-suite neovim-test-suite)


(defmacro capture-reported-spec (spec defform)
  `(let ((nvim::*specs*)
         (nvim::*using-host* T))
     ,defform
     (cond ((and (stringp ,spec) (string= ,spec "opts"))
              (sort (alexandria:hash-table-alist (gethash "opts" (first nvim::*specs*)))
                    #'string< :key #'car))
            (,spec (gethash ,spec (first nvim::*specs*)))
            (T (first nvim::*specs*)))))

(test callback-name-specs
   (is (string= "Test"           (capture-reported-spec "name" (nvim:defun test ()))))
   (is (string= "TestLongName"   (capture-reported-spec "name" (nvim:defun test-long-name ()))))
   (is (string= "TestStringName" (capture-reported-spec "name" (nvim:defun "TestStringName" ())))))

(test callback-sync-specs
   (is (eq :false (capture-reported-spec "sync" (nvim:defcommand test ()))))
   (is (eq T      (capture-reported-spec "sync" (nvim:defcommand/s test ()))))
   (is (eq :false (capture-reported-spec "sync" (nvim:defautocmd test ()))))
   (is (eq T      (capture-reported-spec "sync" (nvim:defautocmd/s test ()))))
   (is (eq :false (capture-reported-spec "sync" (nvim:defun test ()))))
   (is (eq T      (capture-reported-spec "sync" (nvim:defun/s test ())))))

(test callback-type-specs
   (is (string= "command"  (capture-reported-spec "type" (nvim:defcommand test ()))))
   (is (string= "command"  (capture-reported-spec "type" (nvim:defcommand/s test ()))))
   (is (string= "autocmd"  (capture-reported-spec "type" (nvim:defautocmd test ()))))
   (is (string= "autocmd"  (capture-reported-spec "type" (nvim:defautocmd/s test ()))))
   (is (string= "function" (capture-reported-spec "type" (nvim:defun test ()))))
   (is (string= "function" (capture-reported-spec "type" (nvim:defun/s test ())))))

(test callback-opts-specs
   (is (equal '(("nargs" . "*"))
              (capture-reported-spec "opts"
                (nvim:defcommand/s test () (declare (opts nargs))))))
   (is (equal  '(("nargs" . "*"))
               (capture-reported-spec "opts"
                 (nvim:defcommand test () (declare (opts nargs))))))
   (is (equal '(("bang" . "") ("nargs" . "*") ("range" . "%"))
               (capture-reported-spec "opts"
                 (nvim:defcommand test () (declare (opts range bang nargs))))))
   (is (equal '(("bang" . "") ("bar" . "") ("eval" . "eval") ("nargs" . "*") ("range" . "%") ("register" . ""))
               (capture-reported-spec "opts"
                 (nvim:defcommand test () (declare (opts range bang bar nargs (vim-eval "eval") register))))))
   (is (equal '(("bang" . "") ("bar" . "") ("complete" . "file") ("count" . "") ("eval" . "eval") ("nargs" . "?") ("register" . ""))
               (capture-reported-spec "opts"
                 (nvim:defcommand test () (declare (opts count bang bar (nargs "?") (complete "file") (vim-eval "eval") register))))))
   ;; TODO: buffer?

   (is (equal '(("pattern" . "*"))
               (capture-reported-spec "opts"
                 (nvim:defautocmd test ()))))
   (is (equal '(("pattern" . "*.lisp"))
               (capture-reported-spec "opts"
                 (nvim:defautocmd test () (declare (opts (pattern "*.lisp")))))))
   (is (equal '(("eval" . "eval") ("pattern" . "*"))
               (capture-reported-spec "opts"
                 (nvim:defautocmd test (eval-arg) (declare (opts (pattern "*") (vim-eval "eval")))))))

   (is (equal '(("eval" . "eval"))
               (capture-reported-spec "opts"
                 (nvim:defun test-long-name () (declare (opts (vim-eval "eval"))))))))


(nvim:defcommand/s "LispHostTestSameCallbackName" ()
  (declare (opts bang))
  (set-result-in-nvim "first cmd"))

(nvim:defcommand/s "LispHostTestSameCallbackName" ()
  (set-result-in-nvim "second cmd"))

(nvim:defun/s "LispHostTestSameCallbackName" ()
  (set-result-in-nvim "first fun"))

(test registering-duplicate-callbacks
  (is (equal "second cmd" (result-from-nvim/s (nvim:command "LispHostTestSameCallbackName"))))
  (signals mrpc:rpc-error (nvim:command "LispHostTestSameCallbackName!"))
  (is (equal "first fun" (result-from-nvim/s (nvim:call-function "LispHostTestSameCallbackName" #())))))
