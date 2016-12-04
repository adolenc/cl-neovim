(in-package :cl-neovim-tests)
(def-suite callback-test-suite :description "Test suite for cl-neovim callbacks")
(def-suite api-low-level-test-suite :description "Test suite for low level cl-neovim api")
(def-suite api-vim-test-suite :description "Test suite for general cl-neovim vim api")
(def-suite api-buffer-test-suite :description "Test suite for cl-neovim buffer api")
(def-suite api-window-test-suite :description "Test suite for cl-neovim window api")
(def-suite api-tabpage-test-suite :description "Test suite for cl-neovim tabpage api")
(def-suite api-other-test-suite :description "Test suite for the rest of cl-neovim api")


(unless nvim::*using-host*
  (nvim:connect))

(defun get-result-from-nvim (&optional (var-name "lisp_host_test_tmp_result"))
  (nvim:call/s t "vim_get_var" var-name))

(defun set-result-in-nvim (result &optional (var-name "lisp_host_test_tmp_result"))
  (nvim:call/s t "vim_set_var" var-name result))

(defmacro result-from-nvim/s (&body body)
  `(progn
     ,@body
     (get-result-from-nvim)))

(defmacro result-from-nvim/a (&body body)
  `(progn
     (set-result-in-nvim "result not set")
     ,@body
     (loop for result = (get-result-from-nvim) then (get-result-from-nvim)
           while (and (stringp result) (string= result "result not set"))
           do (sleep 0.5)
           finally (return result))))

(defmacro signals-no-error (&body forms)
  `(handler-bind ((error #'(lambda (c) (fail (princ-to-string c)))))
     ,@forms
     (pass)))

(defparameter *cleanup*
":function BeforeEachTest()
    set all&
    redir => groups
    silent augroup
    redir END
    for group in split(groups)
      exe 'augroup '.group
      autocmd!
      augroup END
    endfor
    autocmd!
    tabnew
    let curbufnum = eval(bufnr('%'))
    redir => buflist
    silent ls!
    redir END
    let bufnums = []
    for buf in split(buflist, '\\n')
      let bufnum = eval(split(buf, '[ u]')[0])
      if bufnum != curbufnum
        call add(bufnums, bufnum)
      endif
    endfor
    if len(bufnums) > 0
      exe 'silent bwipeout! '.join(bufnums, ' ')
    endif
    silent tabonly
    for k in keys(g:)
      exe 'unlet g:'.k
    endfor
    filetype plugin indent off
    mapclear
    mapclear!
    abclear
    comclear
  endfunction
")

(def-fixture cleanup ()
  (when (= 0 (nvim:call/s t "vim_call_function" "exists" '("*BeforeEachTest")))
    (nvim:call/s t "vim_input" *cleanup*))
  (nvim:call/s t "vim_call_function" "BeforeEachTest" #())
  (is (= 1 (length (nvim:call/s t "vim_get_tabpages"))))
  (is (= 1 (length (nvim:call/s t "vim_get_windows"))))
  (is (= 1 (length (nvim:call/s t "vim_get_buffers"))))
  (&body))
