(in-package :cl-neovim-tests)
(in-suite api-tabpage-test-suite)


(test tabpage-setting-current
  (with-fixture cleanup ()
    (let ((tp1 (nvim:current-tabpage)))
      (is (eq tp1 (nvim:current-tabpage)))
      (nvim:command "tabnew")
      (let ((tp2 (nvim:current-tabpage)))
        (is (eq tp2 (nvim:current-tabpage)))
        (is (not (eq tp1 (nvim:current-tabpage))))
        (setf (nvim:current-tabpage) tp1)
        (is (eq tp1 (nvim:current-tabpage)))
        (setf (nvim:current-tabpage) tp2)
        (is (eq tp2 (nvim:current-tabpage)))))))

(test tabpage-vars
  (with-fixture cleanup ()
    (let ((tp (nvim:current-tabpage)))
      (setf (nvim:tabpage-var tp "lisp") '((1) "2" (3 4)))
      (is (equal '((1) "2" (3 4)) (nvim:tabpage-var tp "lisp")))
      (is (equal '((1) "2" (3 4)) (nvim:eval "t:lisp"))))))

(test tabpage-valid
  (with-fixture cleanup ()
    (nvim:command "tabnew")
    (let ((tp (second (nvim:tabpages))))
      (is-true (nvim:tabpage-valid-p tp))
      (nvim:command "tabclose")
      (is-false (nvim:tabpage-valid-p tp)))))

(test tabpage-tabpages
  (with-fixture cleanup ()
    (let ((tp (nvim:current-tabpage)))
      (is (= 1 (length (nvim:tabpages))))
      (dotimes (i 5)
        (nvim:command "tabnew"))
      (is (= 6 (length (nvim:tabpages))))
      (is-true (find tp (nvim:tabpages)))
      (setf (nvim:current-tabpage) tp)
      (nvim:command "tabclose")
      (is (= 5 (length (nvim:tabpages))))
      (is-false (find tp (nvim:tabpages))))))

(test tabpage-window
  (with-fixture cleanup ()
    (nvim:command "tabnew")
    (nvim:command "vsplit")
    (is (equal (list (first (nvim:windows)))
               (nvim:tabpage-windows (first (nvim:tabpages)))))
    (is (equal (list (second (nvim:windows)) (third (nvim:windows)))
               (nvim:tabpage-windows (second (nvim:tabpages)))))
    (is (eq (second (nvim:windows)) (nvim:tabpage-window (second (nvim:tabpages)))))
    (setf (nvim:current-window) (third (nvim:windows)))
    (is (eq (third (nvim:windows)) (nvim:tabpage-window (second (nvim:tabpages)))))))
