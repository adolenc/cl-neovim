(in-package :cl-neovim-tests)
(in-suite neovim-test-suite)


(defmacro capture-reported-spec (spec defform)
  `(let ((nvim::*specs*)
         (nvim::*using-host* T))
     ,defform
     ,(cond ((and (stringp spec) (string= spec "opts"))
             `(sort (alexandria:hash-table-alist (gethash "opts" (first nvim::*specs*)))
                   #'string< :key #'car))
            (spec `(gethash ,spec (first nvim::*specs*)))
            (T `(first nvim::*specs*)))))

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
   (is (equal '(("bang" . "") ("nargs" . "*") ("range" . ""))
               (capture-reported-spec "opts"
                 (nvim:defcommand test () (declare (opts range bang nargs))))))
   (is (equal '(("bang" . "") ("bar" . "") ("eval" . "eval") ("nargs" . "*") ("range" . "") ("register" . ""))
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
                 (nvim:defautocmd test () (declare (opts (pattern "*") (vim-eval "eval")))))))

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
  (is (equal "second cmd" (result-from-nvim/s (nvim:call/s t "vim_command" "LispHostTestSameCallbackName"))))
  (signals mrpc:rpc-error (nvim:call/s t "vim_command" "LispHostTestSameCallbackName!"))
  (is (equal "first fun" (result-from-nvim/s (nvim:call/s t "vim_call_function" "LispHostTestSameCallbackName" #())))))


(defmacro generate-command-callbacks (callback-prefix opts)
  `(progn
     ,@(loop for use-shortnames in '(NIL T) append
             (loop for randomize-opts-order in '(NIL T) collect
                   (let ((opts (if use-shortnames opts (mapcar #'first opts)))
                         (declares (mapcar #'first opts))
                         (callback-name (concatenate 'string callback-prefix
                                                     (if use-shortnames "Short" "Long")
                                                     (if randomize-opts-order "Rand" "Determ")))
                         (return-opts (mapcar (if use-shortnames #'second #'first) opts)))
                     `(nvim:defcommand/s ,callback-name (a b &rest args &opts ,@(if randomize-opts-order
                                                                                  (alexandria:shuffle (alexandria:copy-sequence 'list opts))
                                                                                  opts))
                        (declare (opts nargs ,@(substitute '(vim-eval "line(\".\")") 'vim-eval declares)))
                        (set-result-in-nvim (list (list a b args) ,@return-opts))
                        T))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (generate-command-callbacks "LispHostCommandCount" ((count co) (bang bn) (register re) (vim-eval line-nr)))
  (generate-command-callbacks "LispHostCommandRange" ((range ra) (bang bn) (register re) (vim-eval line-nr))))

(nvim:defcommand/s lisp-host-command-no-arglist-opts (&rest args)
  (declare (opts range bang bar nargs (complete "file") (vim-eval "line(\".\")-1")))
  (set-result-in-nvim args))

(nvim:defcommand/s lisp-host-command-no-arglist ()
  (declare (opts nargs))
  (set-result-in-nvim "called!"))

(test command-callbacks
  (nvim:call/s t "buffer_set_lines" (nvim:call/s t "vim_get_current_buffer") 0 -1 T '("abc" "def" "ghi" "jkl"))
  (is (equal '(("a1" "a2" ("a3" "a4")) (2 3) 0 "r" 1) (result-from-nvim/s (nvim:call/s t "vim_command" "2,3LispHostCommandRangeLongDeterm r a1 a2 a3 a4"))))
  (is (equal '(("a1" "a2" ("a3" "a4")) (2 3) 0 "r" 1) (result-from-nvim/s (nvim:call/s t "vim_command" "2,3LispHostCommandRangeLongRand r a1 a2 a3 a4"))))
  (is (equal '(("a1" "a2" ("a3" "a4")) (2 3) 0 "r" 1) (result-from-nvim/s (nvim:call/s t "vim_command" "2,3LispHostCommandRangeShortDeterm r a1 a2 a3 a4"))))
  (is (equal '(("a1" "a2" ("a3" "a4")) (2 3) 0 "r" 1) (result-from-nvim/s (nvim:call/s t "vim_command" "2,3LispHostCommandRangeShortRand r a1 a2 a3 a4"))))
  (nvim:call/s t "vim_input" "j")
  (is (equal '(("a1" "a2" ("a3" "a4")) (2 4) 1 "r" 2) (result-from-nvim/s (nvim:call/s t "vim_command" ".,$LispHostCommandRangeLongDeterm! r a1 a2 a3 a4"))))
  (is (equal '(("a1" "a2" ("a3" "a4")) (2 4) 1 "r" 2) (result-from-nvim/s (nvim:call/s t "vim_command" ".,$LispHostCommandRangeLongRand! r a1 a2 a3 a4"))))
  (is (equal '(("a1" "a2" ("a3" "a4")) (2 4) 1 "r" 2) (result-from-nvim/s (nvim:call/s t "vim_command" ".,$LispHostCommandRangeShortDeterm! r a1 a2 a3 a4"))))
  (is (equal '(("a1" "a2" ("a3" "a4")) (2 4) 1 "r" 2) (result-from-nvim/s (nvim:call/s t "vim_command" ".,$LispHostCommandRangeShortRand! r a1 a2 a3 a4"))))
  (is (equal '(("a1" "a2" ()) (2 2) 1 "r" 2) (result-from-nvim/s (nvim:call/s t "vim_command" "LispHostCommandRangeLongDeterm! r a1 a2"))))
  (is (equal '(("a1" "a2" ()) (2 2) 1 "r" 2) (result-from-nvim/s (nvim:call/s t "vim_command" "LispHostCommandRangeLongRand! r a1 a2"))))
  (is (equal '(("a1" "a2" ()) (2 2) 1 "r" 2) (result-from-nvim/s (nvim:call/s t "vim_command" "LispHostCommandRangeShortDeterm! r a1 a2"))))
  (is (equal '(("a1" "a2" ()) (2 2) 1 "r" 2) (result-from-nvim/s (nvim:call/s t "vim_command" "LispHostCommandRangeShortRand! r a1 a2"))))
  (nvim:call/s t "vim_input" "gg")
  (is (equal '(("a1" "a2" ("a3" "a4")) 10 0 "r" 1) (result-from-nvim/s (nvim:call/s t "vim_command" "10LispHostCommandCountLongDeterm r a1 a2 a3 a4"))))
  (is (equal '(("a1" "a2" ("a3" "a4")) 10 0 "r" 1) (result-from-nvim/s (nvim:call/s t "vim_command" "10LispHostCommandCountLongRand r a1 a2 a3 a4"))))
  (is (equal '(("a1" "a2" ("a3" "a4")) 10 0 "r" 1) (result-from-nvim/s (nvim:call/s t "vim_command" "10LispHostCommandCountShortDeterm r a1 a2 a3 a4"))))
  (is (equal '(("a1" "a2" ("a3" "a4")) 10 0 "r" 1) (result-from-nvim/s (nvim:call/s t "vim_command" "10LispHostCommandCountShortRand r a1 a2 a3 a4"))))
  (nvim:call/s t "vim_input" "j")
  (is (equal '(("a1" "a2" ("a3" "a4")) 10 1 "r" 2) (result-from-nvim/s (nvim:call/s t "vim_command" "10LispHostCommandCountLongDeterm! r a1 a2 a3 a4"))))
  (is (equal '(("a1" "a2" ("a3" "a4")) 10 1 "r" 2) (result-from-nvim/s (nvim:call/s t "vim_command" "10LispHostCommandCountLongRand! r a1 a2 a3 a4"))))
  (is (equal '(("a1" "a2" ("a3" "a4")) 10 1 "r" 2) (result-from-nvim/s (nvim:call/s t "vim_command" "10LispHostCommandCountShortDeterm! r a1 a2 a3 a4"))))
  (is (equal '(("a1" "a2" ("a3" "a4")) 10 1 "r" 2) (result-from-nvim/s (nvim:call/s t "vim_command" "10LispHostCommandCountShortRand! r a1 a2 a3 a4"))))
  (is (equal '("a1" "a2" "a3" "a4") (result-from-nvim/s (nvim:call/s t "vim_command" "LispHostCommandNoArglistOpts a1 a2 a3 a4"))))
  (is (equal "called!" (result-from-nvim/s (nvim:call/s t "vim_command" "LispHostCommandNoArglist"))))
  (signals mrpc:rpc-error (nvim:call/s t "vim_command" "LispHostCommandNoArglist!"))
  (signals mrpc:rpc-error (nvim:call/s t "vim_command" "3,4LispHostCommandNoArglist"))
  (nvim:call/s t "buffer_set_lines" (nvim:call/s t "vim_get_current_buffer") 0 -1 T '("")))


(nvim:defautocmd buf-enter (filename)
  (declare (opts (pattern "*.lisp_host_testa") (vim-eval "expand(\"<afile>\")")))
  (set-result-in-nvim filename))

(test autocmd-callbacks
  (set-result-in-nvim NIL)
  (is (equal "test.lisp_host_testa" (result-from-nvim/a (nvim:call/s t "vim_command" "e! test.lisp_host_testa"))))
  (set-result-in-nvim NIL)
  (is (eq NIL (result-from-nvim/s (nvim:call/s t "vim_command" "e! test.lisp_host_test1"))))
  (nvim:call/s t "vim_command" "enew!"))


(nvim:defun/s lisp-host-fun-full-opts (&rest args &opts vim-eval)
  (declare (opts (vim-eval "line(\".\")-1")))
  (list args vim-eval))

(nvim:defun/s lisp-host-fun-altname-opts (&rest args &opts (vim-eval line-nr))
  (declare (opts (vim-eval "line(\".\")-1")))
  (list args line-nr))

(nvim:defun/s lisp-host-fun-extra-opts (&rest args)
  (declare (opts (vim-eval "line(\".\")-1")))
  args)

(nvim:defun/s lisp-host-fun-no-opts (&rest args)
  args)

(nvim:defun/s lisp-host-fun-args (a b &optional c d)
  (list a b c d))

(nvim:defun/s lisp-host-fun-sym-returns (when)
  (cond ((= when 0) (return-from lisp-host-fun-sym-returns))
        ((= when 1) (return-from lisp-host-fun-sym-returns 0))
        ((= when 2) (block NIL (return-from lisp-host-fun-sym-returns "returned") "fail")))
  T)

(nvim:defun/s "LispHostFunStrReturns" (when)
  (cond ((= when 0) (return-from lisphostfunstrreturns))
        ((= when 1) (return-from lisphostfunstrreturns 0))
        ((= when 2) (block NIL (return-from lisphostfunstrreturns "returned") "fail")))
  T)

(test function-callbacks
  (is (equal '((1 2 3) 0) (nvim:call/s t "vim_call_function" "LispHostFunFullOpts" '(1 2 3))))
  (is (equal '((1 2 3) 0) (nvim:call/s t "vim_call_function" "LispHostFunAltnameOpts" '(1 2 3))))
  (is (equal '(1 2 3)     (nvim:call/s t "vim_call_function" "LispHostFunExtraOpts" '(1 2 3))))
  (is (equal '(1 2 3)     (nvim:call/s t "vim_call_function" "LispHostFunNoOpts" '(1 2 3))))
  (signals mrpc:rpc-error     (nvim:call/s t "vim_call_function" "LispHostFunArgs" #()))
  (signals mrpc:rpc-error     (nvim:call/s t "vim_call_function" "LispHostFunArgs" '(1)))
  (is (equal '(1 "2" NIL NIL) (nvim:call/s t "vim_call_function" "LispHostFunArgs" '(1 "2"))))
  (is (equal '(1 "2" 3 NIL)   (nvim:call/s t "vim_call_function" "LispHostFunArgs" '(1 "2" 3))))
  (is (equal '(1 "2" 3 (4 5)) (nvim:call/s t "vim_call_function" "LispHostFunArgs" '(1 "2" 3 (4 5)))))
  (is (null (nvim:call/s t "vim_call_function" "LispHostFunSymReturns" '(0))))
  (is (= 0 (nvim:call/s t "vim_call_function" "LispHostFunSymReturns" '(1))))
  (is (string= "returned" (nvim:call/s t "vim_call_function" "LispHostFunSymReturns" '(2))))
  (is-true (nvim:call/s t "vim_call_function" "LispHostFunSymReturns" '(-1)))
  (is (null (nvim:call/s t "vim_call_function" "LispHostFunStrReturns" '(0))))
  (is (= 0 (nvim:call/s t "vim_call_function" "LispHostFunStrReturns" '(1))))
  (is (string= "returned" (nvim:call/s t "vim_call_function" "LispHostFunStrReturns" '(2)))))
