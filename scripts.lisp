;; #!/usr/bin/sbcl --script


(defun my-command-line ()
  (or 
   #+SBCL *posix-argv*  
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))


(defun main ()
  (write-line "Hello, World!")
  (when (> (length (my-command-line)) 1)
    (write-line (cadr (my-command-line)))))

(sb-ext:save-lisp-and-die "hello-script.sh" :toplevel #'main :executable t)
