(asdf:defsystem #:cl-neovim-tests
  :description "Tests for cl-neovim."
  :author "Andrej Dolenc <andrej.dolenc@student.uni-lj.si>"
  :license "MIT"
  :depends-on (#:cl-neovim
               #:fiveam)
  :serial T
  :components ((:module "t"
                :components ((:file "package")
                             (:file "setup")
                             (:file "api-low-level")
                             (:file "callbacks")
                             (:file "api-vim")
                             (:file "api-buffer")
                             (:file "api-window")
                             (:file "api-tabpage")
                             (:file "api-other"))))
  :perform (test-op (op c)
             (uiop:symbol-call '#:fiveam '#:run! (uiop:find-symbol* '#:api-low-level-test-suite :cl-neovim-tests))
             (uiop:symbol-call '#:fiveam '#:run! (uiop:find-symbol* '#:callback-test-suite      :cl-neovim-tests))
             (uiop:symbol-call '#:fiveam '#:run! (uiop:find-symbol* '#:api-vim-test-suite       :cl-neovim-tests))
             (uiop:symbol-call '#:fiveam '#:run! (uiop:find-symbol* '#:api-buffer-test-suite    :cl-neovim-tests))
             (uiop:symbol-call '#:fiveam '#:run! (uiop:find-symbol* '#:api-window-test-suite    :cl-neovim-tests))
             (uiop:symbol-call '#:fiveam '#:run! (uiop:find-symbol* '#:api-tabpage-test-suite   :cl-neovim-tests))
             (uiop:symbol-call '#:fiveam '#:run! (uiop:find-symbol* '#:api-other-test-suite     :cl-neovim-tests))))
