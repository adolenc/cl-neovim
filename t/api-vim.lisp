(in-package :cl-neovim-tests)
(in-suite api-vim-test-suite)


(test command
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

(test command-output
  (with-fixture cleanup ()
    (is (string= (format nil "~ctest" #\Newline)
                 (nvim:command-output "echo 'test'")))))

(test eval
  (with-fixture cleanup ()
    (nvim:command "let g:v1 = 'a'")
    (nvim:command "let g:v2 = [1, 2, ['v3', 3]]")
    (let ((result (nvim:eval "g:")))
      (is (equal '(1 2 ("v3" 3)) (gethash "v2" result)))
      (is (equal "a" (gethash "v1" result))))))

(test call
  (with-fixture cleanup ()
    (is (string= "first, last" (nvim:call-function "join" '(("first" "last") ", "))))))

(test strwidth
  (with-fixture cleanup ()
    (is (= 3 (nvim:strwidth "abc")))
    (is (= 16 (nvim:strwidth "lisphost大好き〜")))))

(test chdir
  (with-fixture cleanup ()
    (let ((pwd (nvim:eval "getcwd()")))
      (nvim:change-directory "/")
      (is (string= "/" (nvim:eval "getcwd()")))
      (nvim:change-directory pwd)
      (is (string= pwd (nvim:eval "getcwd()"))))))

(test current-line
  (with-fixture cleanup ()
    (is (string= "" (nvim:current-line)))
    (setf (nvim:current-line) "abc")
    (is (string= "abc" (nvim:current-line)))))

(test vars
  (with-fixture cleanup ()
    (setf (nvim:var "lisp") '((1) "2" (3 4)))
    (is (equal '((1) "2" (3 4)) (nvim:var "lisp")))
    (is (equal '((1) "2" (3 4)) (nvim:eval "g:lisp")))))

(test options
  (with-fixture cleanup ()
    (is (string= "tab:> ,trail:-,nbsp:+" (nvim:option "listchars")))
    (setf (nvim:option "listchars") "tab:xy")
    (is (string= "tab:xy" (nvim:option "listchars")))))
