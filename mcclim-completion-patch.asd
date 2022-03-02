;;; Copyright (c) 2022, Andrea De Michele (andrea.demichele@gmail.com)
;;; License: BSD-2-Clause.          

(defsystem "mcclim-completion-patch"
  :depends-on ("mcclim")
  :components ((:file "franz-completion")
               (:file "patch-completion")))
