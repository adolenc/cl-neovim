(in-package #:cl-neovim)


(defparameter *debug-stream* (make-broadcast-stream))
(defvar *using-host* NIL "Variable that host binds to T when it loads plugins.")

(defvar *specs* NIL "A list of all the specs nvim needs.")
(defvar *path* NIL "Path used when loading plugin, which gets prepended to callback name.")
