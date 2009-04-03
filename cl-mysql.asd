;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; Author: Steve Knight <stknig@gmail.com>
;;;;

(asdf:defsystem #:cl-mysql
  :description "Common Lisp MySQL library bindings"
  :version "0.1"
  :author "Steve Knight <stknig@gmail.com>"
  :maintainer "Steve Knight <stknig@gmail.com>"
  :licence "LLGPL"
  :in-order-to ((test-op (load-op cl-mysql-test)))
  :perform (test-op :after (op c)
		    (describe
		     (funcall
		      (intern "TEST-BASE" :cl-mysql-test))))
  :serial t
  :components ((:file "mysql")
	       (:file "package"))
  :depends-on (#:cffi))

(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system 'cl-mysql)))))




  
  
