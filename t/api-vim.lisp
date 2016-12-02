(in-package :cl-neovim-tests)
(in-suite api-vim-test-suite)


(test vim-command
  (with-fixture cleanup ()
    (let ((tmpfile (format NIL "/tmp/nvim_lisp_tests_~A.tmp" (random 100000))))
      (nvim:command (format NIL "edit ~A" tmpfile))
      (nvim:input (format NIL "~c" #\Linefeed))
      (nvim:command (format NIL "normal itesting~clisp~capi" #\Newline #\Newline))
      (nvim:command "w")
      (is-true (probe-file tmpfile))
      (or (with-open-file (in tmpfile :direction :input :if-does-not-exist NIL)
            (let ((contents (make-string (file-length in))))
              (read-sequence contents in)
              (is (string= (format NIL "testing~clisp~capi~c" #\Newline #\Newline #\Newline) contents))))
          (fail))
      (delete-file tmpfile))))

(test vim-command-output
  (with-fixture cleanup ()
    (is (string= (format nil "~ctest" #\Newline)
                 (nvim:command-output "echo 'test'")))))

(test vim-eval
  (with-fixture cleanup ()
    (nvim:command "let g:v1 = 'a'")
    (nvim:command "let g:v2 = [1, 2, ['v3', 3]]")
    (let ((result (nvim:eval "g:")))
      (is (equal '(1 2 ("v3" 3)) (gethash "v2" result)))
      (is (equal "a" (gethash "v1" result))))))

(test vim-call
  (with-fixture cleanup ()
    (is (string= "first, last" (nvim:call-function "join" '(("first" "last") ", "))))
    (is (equalp '(0 1 1 0 1) (nvim:call-function "getcurpos" #())))
    (is (equalp '(0 1 1 0 1) (nvim:call-function "getcurpos" '())))))

(test vim-strwidth
  (with-fixture cleanup ()
    (is (= 3 (nvim:strwidth "abc")))
    (is (= 16 (nvim:strwidth "lisphost大好き〜")))))

(test vim-chdir
  (with-fixture cleanup ()
    (let ((pwd (nvim:eval "getcwd()")))
      (nvim:change-directory "/")
      (is (string= "/" (nvim:eval "getcwd()")))
      (nvim:change-directory pwd)
      (is (string= pwd (nvim:eval "getcwd()"))))))

(test vim-current-line
  (with-fixture cleanup ()
    (is (string= "" (nvim:current-line)))
    (setf (nvim:current-line) "abc")
    (is (string= "abc" (nvim:current-line)))
    (nvim:del-current-line)
    (is (string= "" (nvim:current-line)))))

(test vim-vars
  (with-fixture cleanup ()
    (setf (nvim:var "lisp") '((1) "2" (3 4)))
    (is (equal '((1) "2" (3 4)) (nvim:var "lisp")))
    (is (equal '((1) "2" (3 4)) (nvim:eval "g:lisp")))))

(test vim-options
  (with-fixture cleanup ()
    (is (string= "tab:> ,trail:-,nbsp:+" (nvim:option "listchars")))
    (setf (nvim:option "listchars") "tab:xy")
    (is (string= "tab:xy" (nvim:option "listchars")))))

(test vim-runtime-paths
  (with-fixture cleanup ()
    (is (<= 1 (length (nvim:runtime-paths))))
    (is-true (find (nvim:eval "expand('$HOME/.config/nvim')")
                   (nvim:runtime-paths)
                   :test #'string=))))

(test vim-colors
  (with-fixture cleanup ()
    (let* ((color-map (nvim:color-map))
           (colors (alexandria:hash-table-keys color-map)))
      (dotimes (i 15)
        (let ((color (alexandria:random-elt colors)))
          (is (= (gethash color color-map) (nvim:name-to-color color))))))))

(test vim-broadcast
  (with-fixture cleanup ()
    (let ((event1-called)
          (event2-called))
      (flet ((reset-status () (setf event1-called NIL
                                    event2-called NIL)))
        (nvim:subscribe "event2" #'(lambda (&rest args)
                                     (alexandria:appendf event2-called (list args))))
        (nvim:command "call rpcnotify(0, 'event1', 1, 2, 3)")
        (nvim:command "call rpcnotify(0, 'event2', 4, 5, 6)")
        (nvim:command "call rpcnotify(0, 'event2', 7, 8, 9)")
        (is (equal '() event1-called))
        (is (equal '((4 5 6) (7 8 9)) event2-called))
        (reset-status)
        (nvim:unsubscribe "event2")
        (nvim:subscribe "event1" #'(lambda (&rest args)
                                     (alexandria:appendf event1-called (list args))))
        (nvim:command "call rpcnotify(0, 'event2', 10, 11, 12)")
        (nvim:command "call rpcnotify(0, 'event1', 13, 14, 15)")
        (is (equal '((13 14 15)) event1-called))
        (is (equal '() event2-called))))))

(test vim-vvars
  (with-fixture cleanup ()
    (nvim:command "let v:warningmsg='test'")
    (is (string= "test" (nvim:vvar "warningmsg")))
    (nvim:command "let v:warningmsg='test2'")
    (is (string= "test2" (nvim:vvar "warningmsg")))))

(test vim-api-info
  (with-fixture cleanup ()
    (destructuring-bind (channel metadata) (nvim:api-info)
      (is (<= 0 channel))
      (is (equal '("error_types" "functions" "types" "version")
                 (sort (alexandria:hash-table-keys metadata) #'string<))))))

(test vim-buffers
  (with-fixture cleanup ()
    (let ((buffers '()))
      (is (= 1 (length (nvim:buffers))))
      (push (nvim:current-buffer) buffers)
      (nvim:command "new")
      (is (= 2 (length (nvim:buffers))))
      (push (nvim:current-buffer) buffers)
      (is-true (find (first buffers) (nvim:buffers)))
      (is-true (find (second buffers) (nvim:buffers))))))

(test vim-windows
  (with-fixture cleanup ()
    (is (= 1 (length (nvim:tabpages))))
    (is (eq (nvim:current-window) (first (nvim:windows))))
    (nvim:command "vsplit")
    (nvim:command "split")
    (is (= 3 (length (nvim:windows))))
    (is (eq (nvim:current-window) (first (nvim:windows))))
    (setf (nvim:current-window) (second (nvim:windows)))
    (is (eq (nvim:current-window) (second (nvim:windows))))))

(test vim-tabpages
  (with-fixture cleanup ()
    (is (= 1 (length (nvim:tabpages))))
    (is (eq (nvim:current-tabpage) (first (nvim:tabpages))))
    (nvim:command "tabnew")
    (is (= 2 (length (nvim:tabpages))))
    (is (= 2 (length (nvim:windows))))
    (is (eq (nvim:current-window)  (second (nvim:windows))))
    (is (eq (nvim:current-tabpage) (second (nvim:tabpages))))
    (setf (nvim:current-window) (first (nvim:windows)))
    (is (eq (nvim:current-tabpage) (first (nvim:tabpages))))
    (is (eq (nvim:current-window)  (first (nvim:windows))))
    (setf (nvim:current-tabpage) (second (nvim:tabpages)))
    (is (eq (nvim:current-tabpage) (second (nvim:tabpages))))
    (is (eq (nvim:current-window)  (second (nvim:windows))))))
