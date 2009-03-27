;; $Id$
;;
(defpackage com.hackinghat.cl-mysql
  (:use :cl)
  (:nicknames "CL-MYSQL")
  (:shadowing-import-from "CL-MYSQL-SYSTEM"
  	   #:connect #:query #:use #:disconnect #:ping #:option
	   #:client-version #:server-version
	   #:list-dbs #:list-tables #:list-processes #:list-fields)
  (:export #:connect #:query #:use #:disconnect #:ping #:option
	   #:client-version #:server-version
	   #:list-dbs #:list-tables #:list-processes #:list-fields))

