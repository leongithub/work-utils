;;;; work-utils.asd

(asdf:defsystem #:work-utils
  :description "Describe work-utils here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-ppcre
               #:com.gigamonkeys.pathnames)
  :serial t
  :components
  ((:file "package")
   (:file "work-utils")
   (:file "compute-number")
   (:file "add-comment-to-duplicate-strings")
   (:file "find-out-lack-strings")
   (:file "copy-string-from-res1-to-res2" :depends-on ("work-utils"))
   (:file "delete-unuse-resource" :depends-on ("work-utils"))))

