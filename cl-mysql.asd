;; $Id$

(asdf:defsystem #:cl-mysql
  :serial t
  :description "Common Lisp MySQL library bindings"
  :version "0.1"
  :author "Steve Knight <stkni@yahoo.com>"
  :licence "LLGPL"
  :components ((:file "mysql"))
  :depends-on (#:cffi))
 
  
