(in-package :cl-neovim-tests)
(in-suite api-tabpage-test-suite)


(test tabpage-setting-current
  (with-fixture cleanup ()
    (is (= 1 (length (nvim:tabpage-windows (nvim:current-tabpage)))))
    (nvim:command "tabnew")
    (nvim:command "split")
    (nvim:command "split")
    (is (= 3 (length (nvim:tabpage-windows (nvim:current-tabpage)))))
    (is (= 2 (length (nvim:tabpages))))
    (setf (nvim:current-tabpage) (first (nvim:tabpages)))
    (is (= 1 (length (nvim:tabpage-windows (nvim:current-tabpage)))))
    (setf (nvim:current-tabpage) (second (nvim:tabpages)))
    (is (= 3 (length (nvim:tabpage-windows (nvim:current-tabpage)))))))

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
