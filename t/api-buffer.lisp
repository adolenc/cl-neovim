(in-package :cl-neovim-tests)
(in-suite api-buffer-test-suite)


(test vars
  (with-fixture cleanup ()
    (let ((b (nvim:current-buffer)))
      (setf (nvim:buffer-var b "lisp") '(1 2 (3 4)))
      (is (equal '(1 2 (3 4)) (nvim:buffer-var b "lisp")))
      (is (equal '(1 2 (3 4)) (nvim:eval "b:lisp"))))))

(test options
  (with-fixture cleanup ()
    (let ((b (nvim:current-buffer)))
      (is (equal 8 (nvim:buffer-option b "shiftwidth")))
      (setf (nvim:buffer-option b "shiftwidth") 4)
      (is (equal 4 (nvim:buffer-option b "shiftwidth")))
      (setf (nvim:buffer-option b "define") "test")
      (is (equal "test" (nvim:buffer-option b "define")))   
      (is (equal "^\\s*#\\s*define" (nvim:option "define"))))))

(test number
  (with-fixture cleanup ()
    (let ((current-number (nvim:buffer-number (nvim:current-buffer))))
      (nvim:command "new")
      (is (= (+ 1 current-number) (nvim:buffer-number (nvim:current-buffer))))
      (nvim:command "new")
      (is (= (+ 2 current-number) (nvim:buffer-number (nvim:current-buffer)))))))

(test name
  (with-fixture cleanup ()
    (nvim:command "new")
    (let ((b (nvim:current-buffer))
          (new-name (nvim:eval "resolve(tempname())")))
      (is (string= "" (nvim:buffer-name b)))
      (setf (nvim:buffer-name b) new-name)
      (is (string= new-name (nvim:buffer-name b)))
      (nvim:command "silent w!")
      (is-true (probe-file new-name)))))

(test valid
  (with-fixture cleanup ()
    (nvim:command "new")
    (let ((b (nvim:current-buffer)))
      (is-true (nvim:buffer-valid-p b))
      (nvim:command "bw!")
      (is-false (nvim:buffer-valid-p b)))))

(test lines
  (with-fixture cleanup ()
    (let ((b (nvim:current-buffer)))
      (is (equal '("") (nvim:buffer-lines b 0 -1 T)))
      (setf (nvim:buffer-lines b 0 -1 T) (cons "a" (nvim:buffer-lines b 0 -1 T)))
      (is (equal '("a" "") (nvim:buffer-lines b 0 -1 T)))
      (push "b" (nvim:buffer-lines b 0 -1 T))
      (is (equal '("b" "a" "") (nvim:buffer-lines b 0 -1 T)))
      (alexandria:appendf (nvim:buffer-lines b 0 -1 T) '("c"))
      (is (equal '("b" "a" "" "c") (nvim:buffer-lines b 0 -1 T)))
      (nvim:buffer-del-line b 2)
      (is (equal '("b" "a" "c") (nvim:buffer-lines b 0 -1 T)))
      (nvim:buffer-del-line b 0)
      (is (equal '("a" "c") (nvim:buffer-lines b 0 -1 T)))
      (nvim:buffer-insert b 1 '("i" "m"))
      (is (equal '("a" "i" "m" "c") (nvim:buffer-lines b 0 -1 T)))
      (dotimes (i 5)
        (nvim:buffer-del-line b 0))
      (is (equal '("") (nvim:buffer-lines b 0 -1 T)))
      (setf (nvim:buffer-lines b 0 -1 T) '("abc" "def" "ghi"))
      (is (equal '("abc" "def" "ghi") (nvim:buffer-lines b 0 -1 T)))   
      (setf (nvim:buffer-lines b 0 -1 T) '("s"))
      (is (equal '("s") (nvim:buffer-lines b 0 -1 T))))))

(test count
  (with-fixture cleanup ()
    (let ((b (nvim:current-buffer)))
      (setf (nvim:buffer-lines b 0 -1 T) '("a" "b" "c" "d"))
      (is (= 4 (length (nvim:buffer-lines b 0 -1 T)))) 
      (is (= 4 (nvim:buffer-line-count b))))))

(test marks
  (with-fixture cleanup ()
    (let ((b (nvim:current-buffer)))
      (setf (nvim:buffer-lines b 0 -1 T) '("abc" "def" "ghi"))
      (setf (nvim:window-cursor (nvim:current-window)) '(2 1))
      (nvim:command "mark V")
      (is (equal '(2 0) (nvim:buffer-mark (nvim:current-buffer) "V"))))))

(test exceptions
  (with-fixture cleanup ()
    (signals error (nvim:buffer-option (nvim:current-buffer) "invalid-option"))))
