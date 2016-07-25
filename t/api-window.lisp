(in-package :cl-neovim-tests)
(in-suite api-window-test-suite)


(test window-buffer
  (with-fixture cleanup ()
    (is (eq (nvim:current-buffer) (window-buffer (first (nvim:windows)))))
    (nvim:command "new")
    (setf (nvim:current-window) (second (nvim:windows)))
    (is (eq (nvim:current-buffer) (window-buffer (second (nvim:windows)))))
    (is (not (eq (nvim:window-buffer (first (nvim:windows)))
                 (nvim:window-buffer (second (nvim:windows))))))))

(test window-cursor
  (with-fixture cleanup ()
    (let ((w (nvim:current-window)))
      (is (equal '(1 0) (nvim:window-cursor w)))
      (nvim:command (format NIL "normal ithis~cis text" #\newline))
      (is (equal '(2 6) (nvim:window-cursor w)))
      (setf (nvim:window-cursor w) '(2 3))
      (nvim:command "normal isome ")
      (setf (nvim:window-cursor w) '(2 6))
      (is (equal '("this" "is some text") (nvim:buffer-lines (nvim:current-buffer) 0 -1 T))))))

(test window-height
  (with-fixture cleanup ()
    (nvim:command "vsplit")
    (is (= (nvim:window-height (first (nvim:windows)))
           (nvim:window-height (second (nvim:windows)))))
    (setf (nvim:current-window) (second (nvim:windows)))
    (nvim:command "split")
    (is (= (nvim:window-height (second (nvim:windows)))
           (floor (nvim:window-height (first (nvim:windows))) 2)))
    (setf (nvim:window-height (second (nvim:windows))) 2)
    (is (= (nvim:window-height (second (nvim:windows)))))))

(test window-width
  (with-fixture cleanup ()
    (nvim:command "split")
    (is (= (nvim:window-width (first (nvim:windows)))
           (nvim:window-width (second (nvim:windows)))))
    (setf (nvim:current-window) (second (nvim:windows)))
    (nvim:command "vsplit")
    (is (= (nvim:window-width (second (nvim:windows)))
           (floor (nvim:window-width (first (nvim:windows))) 2)))
    (setf (nvim:window-width (second (nvim:windows))) 2)
    (is (= (nvim:window-width (second (nvim:windows)))))))

(test window-vars
  (with-fixture cleanup ()
    (let ((w (nvim:current-window)))
      (setf (nvim:window-var w "lisp") '((1) "2" (3 4)))
      (is (equal '((1) "2" (3 4)) (nvim:window-var w "lisp")))
      (is (equal '((1) "2" (3 4)) (nvim:eval "w:lisp"))))))


(test window-options
  (with-fixture cleanup ()
    (let ((w (nvim:current-window)))
      (setf (nvim:window-option w "colorcolumn") "4,3")
      (is (string= "4,3" (nvim:window-option w "colorcolumn")))
      (setf (nvim:window-option w "statusline") "window-status")
      (is (equal "window-status" (nvim:window-option w "statusline")))
      (is (string= "" (nvim:option "statusline"))))))

(test window-position
  (with-fixture cleanup ()
    (flet ((row (window) (first (nvim:window-position window)))
           (col (window) (second (nvim:window-position window))))
      (let ((height (nvim:window-height (first (nvim:windows))))
            (width (nvim:window-width (first (nvim:windows)))))
        (nvim:command "split")
        (nvim:command "vsplit")
        (is (equal '(0 0) (nvim:window-position (first (nvim:windows)))))
        (let ((vsplit-pos (/ width 2))
              (split-pos (/ height 2)))
          (is (= 0 (row (second (nvim:windows)))))
          (is (<= (1- vsplit-pos) (col (second (nvim:windows))) (1+ vsplit-pos)))
          (is (<= (1- split-pos) (row (third (nvim:windows))) (1+ split-pos)))
          (is (= 0 (col (third (nvim:windows))))))))))

(test window-valid
  (with-fixture cleanup ()
    (nvim:command "split")
    (let ((w (second (nvim:windows))))
      (setf (nvim:current-window) w)
      (is-true (nvim:window-valid-p w))
      (nvim:command "q")
      (is-false (nvim:window-valid-p w)))))

(test window-windows
  (with-fixture cleanup ()
    (let ((w (nvim:current-window)))
      (is (= 1 (length (nvim:windows))))
      (dotimes (i 5)
        (nvim:command "split"))
      (is (= 6 (length (nvim:windows))))
      (is-true (find w (nvim:windows)))
      (setf (nvim:current-window) w)
      (nvim:command "q")
      (is (= 5 (length (nvim:windows))))
      (is-false (find w (nvim:windows))))))

(test window-tabpage
  (with-fixture cleanup ()
    (nvim:command "tabnew")
    (nvim:command "vsplit")
    (is (eq (first  (nvim:tabpages)) (nvim:window-tabpage (first (nvim:windows)))))
    (is (eq (second (nvim:tabpages)) (nvim:window-tabpage (second (nvim:windows)))))
    (is (eq (second (nvim:tabpages)) (nvim:window-tabpage (third (nvim:windows)))))))
