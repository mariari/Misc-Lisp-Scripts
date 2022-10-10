(asdf:defsystem :yaml-generator
  :version "0.1.0.0"
  :description "Provides a Yaml generator"
  :author "Mariari"
  :license "GPL3"
  :pathname "src/"
  :components
  ((:file "stack-yaml")
   (:file "config" :depends-on ("stack-yaml"))
   (:file "main"   :depends-on ("config"))))

(asdf:defsystem :yaml-generator-gui
  :depends-on (:yaml-generator :mcclim :clim :fset)
  :version "0.1.0.0"
  :description "Provides a Yaml generator gui"
  :author "Mariari"
  :license "GPL3"
  :pathname "gui/"
  :components
  ((:file "draw")))

;; We do this to make the system load for the script
;; (asdf:load-system :yaml-generator)

(defun main ()
  ;; Refactor so this defines a packages that we can call into
  (asdf:load-system :yaml-generator)
  (main))

(defun launch-gui ()
  (asdf:load-system :yaml-generator-gui)
  (defun launch-gui ()
    ;; read time symbol, thus need to find the symbol to get around it
    (funcall (find-symbol "MAIN-GUI" (find-package 'clim-yaml))))
  ;; The function is now the one above, avoiding the extra load-system call
  (launch-gui))
