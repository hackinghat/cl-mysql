;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; 
;;;; Copyright (c) 2009 Steve Knight <stkni@gmail.com>
;;;; 
;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to
;;;; the following conditions:
;;;; 
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;;
(defpackage com.hackinghat.cl-mysql-system
  (:use :cl :cffi)
  (:nicknames "CL-MYSQL-SYSTEM")
  (:export
   ;; Conditions
   #:cl-mysql-error #:mysql-error
   ;; Classes
   #:connection #:connection-pool #:statement
   ;; Methods
   #:connected #:available #:in-use #:pointer #:connection-equal
   #:aquire #:can-aquire #:release #:count-connections #:contains #:connections
   #:available-connections #:result-set #:max-connections #:min-connections
   #:result-set-fields #:process-result-set #:pool-lock #:bind-arg
   #:configure-bindings #:bound-map #:next-index #:bind #:close
   ;; Special vairalbes
   #:*type-map* #:*last-database*
   ;; Public functions
   #:connect #:query #:use #:disconnect #:ping #:option
   #:client-version #:server-version
   #:list-dbs #:list-tables #:list-processes #:list-fields
   #:escape-string
   #:next-result-set #:next-row #:nth-row #:with-rows
   ;; Thread functions
   #:wait-on-threads #:make-lock #:with-lock #:start-thread-in-nsecs
   ;; Constants
   #:+client-compress+  #:+client-found-rows+    #:+client-ignore-sigpipe+
   #:+client-ignore-space+  #:+client-interactive+ #:+client-local-files+
   #:+client-multi-statements+  #:+client-multi-results+  #:+client-no-schema+
   #:+client-ssl+  #:+client-remember-options+
   ;; Internal functions
   #:string-to-integer #:string-to-float
   #:string-to-date #:string-to-seconds #:string-to-universal-time
   #:string-to-ratio #:extract-field #:cffi-utf8-length
   ;; Enum Options
   #:opt-connect-timeout #:opt-compress #:opt-named-pipe
   #:init-command #:read-default-file #:read-default-group
   #:set-charset-dir #:set-charset-name #:opt-local-infile
   #:opt-protocol #:shared-memory-base-name #:opt-read-timeout
   #:opt-write-timeout #:opt-use-result
   #:opt-use-remote-connection #:opt-use-embedded-connection
   #:opt-guess-connection #:set-client-ip #:secure-auth
   #:report-data-truncation #:opt-reconnect
   #:opt-ssl-verify-server-cert))

(in-package "CL-MYSQL-SYSTEM")

(define-foreign-library libmysqlclient
  (:darwin (:or "libmysqlclient.20.dylib" "libmysqlclient.dylib"))
  ((:not :windows) (:default "libmysqlclient_r"))
  (:windows (:default "libmysql")))

(use-foreign-library libmysqlclient)

;;; Client options
;;;
(defconstant +client-compress+ 32)
(defconstant +client-found-rows+ 2)
(defconstant +client-ignore-sigpipe+ 4096)
(defconstant +client-ignore-space+ 256)
(defconstant +client-interactive+ 1024)
(defconstant +client-local-files+ 128)
(defconstant +client-multi-statements+  (ash 1 16))
(defconstant +client-multi-results+ (ash 1 17))
(defconstant +client-no-schema+ 16)
(defconstant +client-ssl+ (ash 1 11))
(defconstant +client-remember-options+ (ash 1 31))

;;; Field flags
;;;
(defconstant +field-not-null+ 1
  "Field can't be null")
(defconstant +field-primary-key+ 2
  "Field is part of a primary key")
(defconstant +field-unique-key+ 4
  "Field is part of a unique key")
(defconstant +field-multiple-key+ 8
  "Field is part of a key")
(defconstant +field-blob+ 16
  "Field is a blob")
(defconstant +field-unsigned+ 32
  "Field is unsigned")
(defconstant +field-zerofill+ 64
  "Field is zerofill")
(defconstant +field-binary+ 128
  "Field is binary")
(defconstant +field-enum+ 256
  "Field is an enum")
(defconstant +field-auto-increment+ 512
  "Field is auto increment")
(defconstant +field-timestamp+ 1024
  "Field is a timestamp")
(defconstant +field-set+ 2048
  "Field is a set")
(defconstant +field-no-default+ 4096
  "Field doesn't have a default value")
(defconstant +field-num+ 32768
  "Field is num")

;;; Error codes
;;;
(defconstant +error-first+ 2000)
(defconstant +unknown-error+ 2000)
(defconstant +socket-create-error+ 2001)
(defconstant +connection-error+ 2002)
(defconstant +conn-host-error+ 2003)
(defconstant +ipsock-error+ 2004)
(defconstant +unknown-host+ 2005)
(defconstant +server-gone-error+ 2006)
(defconstant +version-error+ 2007)
(defconstant +out-of-memory+ 2008)
(defconstant +wrong-host-info+ 2009)
(defconstant +localhost-connection+ 2010)
(defconstant +tcp-connection+ 2011)
(defconstant +server-handshake-err+ 2012)
(defconstant +server-lost+ 2013)
(defconstant +commands-out-of-sync+ 2014)
(defconstant +namedpipe-connection+ 2015)
(defconstant +namedpipewait-error+ 2016)
(defconstant +namedpipeopen-error+ 2017)
(defconstant +namedpipesetstate-error+ 2018)
(defconstant +cant-read-charset+ 2019)
(defconstant +net-packet-too-large+ 2020)
(defconstant +embedded-connection+ 2021)
(defconstant +probe-slave-status+ 2022)
(defconstant +probe-slave-hosts+ 2023)
(defconstant +probe-slave-connect+ 2024)
(defconstant +probe-master-connect+ 2025)
(defconstant +ssl-connection-error+ 2026)
(defconstant +malformed-packet+ 2027)
(defconstant +wrong-license+ 2028)
(defconstant +null-pointer+ 2029)
(defconstant +no-prepare-stmt+ 2030)
(defconstant +params-not-bound+ 2031)
(defconstant +data-truncated+ 2032)
(defconstant +no-parameters-exists+ 2033)
(defconstant +invalid-parameter-no+ 2034)
(defconstant +invalid-buffer-use+ 2035)
(defconstant +unsupported-param-type+ 2036)
(defconstant +shared-memory-connection+ 2037)
(defconstant +shared-memory-connect-request-error+ 2038)
(defconstant +shared-memory-connect-answer-error+ 2039)
(defconstant +shared-memory-connect-file-map-error+ 2040)
(defconstant +shared-memory-connect-map-error+ 2041)
(defconstant +shared-memory-file-map-error+ 2042)
(defconstant +shared-memory-map-error+ 2043)
(defconstant +shared-memory-event-error+ 2044)
(defconstant +shared-memory-connect-abandoned-error+ 2045)
(defconstant +shared-memory-connect-set-error+ 2046)
(defconstant +conn-unknow-protocol+ 2047)
(defconstant +invalid-conn-handle+ 2048)
(defconstant +secure-auth+ 2049)
(defconstant +fetch-canceled+ 2050)
(defconstant +no-data+ 2051)
(defconstant +no-stmt-metadata+ 2052)
(defconstant +no-result-set+ 2053)
(defconstant +not-implemented+ 2054)
(defconstant +server-lost-extended+ 2055)

(eval-when (:compile-toplevel)
  (defparameter *generate-alt-fns* nil
    "Compile the library with this value equal to T to get the indirection
   facility.   For more performance (because there is no wrapper around
   the CFFI wrapper!) set this value to NIL")
  (defparameter *mysql-dev-api-url* "http://dev.mysql.com/doc/refman/5.0/en/~A.html"
    "MySQL uses a standard page naming convention that matches our function names!"))

(defmacro defmysqlfun ((name internal-name) return-type &body args)
  "Takes a mysql function name as a string and registers the
   appropriate CFFI function as internal-name.  

   If *generate-alt-fns* is T internal-name that denotes T a wrapper function
   that sits around the function lib<internal-name>.

   This function will call the lib<internal-name>, unless there is an 'alt-fn
   property set on the function's symbol.   If such a function exists it is called
   instead.

   e.g.  
   CL-USER> (connect)
   CL-USER> (setf (get 'mysql-close 'alt-fn) (lambda (db)
                                               (print \"Closing! \")
                                               (libmysql-close db)))
   CL-USER> (disconnect)
   Closing!"
  (let* ((n name)
	 (int-name internal-name)
	 (int-libname (intern (string-upcase
			       (format nil "lib~A" int-name))))
	 (docstring (format nil "Library wrapper for MySQL function: ~A" name))
	 (mysql-doc-ref (format nil *mysql-dev-api-url* (string-downcase  int-name)))
	 (arg-names (mapcar #'car args)))
    (if *generate-alt-fns*
	`(progn  (defcfun (,n ,int-libname) ,return-type
		   ,mysql-doc-ref
		   ,@args)
		 (defun ,int-name ,arg-names
		   ,docstring
		   (let ((alt-fn (get ',int-name 'alt-fn)))
		     (if alt-fn
			 (funcall alt-fn ,@arg-names)
			 (,int-libname ,@arg-names)))))
	`(defcfun (,n ,int-name) ,return-type
	   ,mysql-doc-ref
	   ,@args))))
 
(defmysqlfun ("mysql_init" mysql-init) :pointer
  (mysql :pointer))

(defmysqlfun ("mysql_close" mysql-close) :pointer
  (mysql :pointer))

(defmysqlfun ("mysql_error" mysql-error) :string
  (mysql :pointer))

(defmysqlfun ("mysql_errno" mysql-errno) :unsigned-int
  (mysql :pointer))

(defmysqlfun ("mysql_real_connect" mysql-real-connect) :pointer
  (mysql :pointer)
  (host :string)
  (user :string)
  (password :string)
  (database :string)
  (port :int)
  (unix-socket :string)
  (client-flag :unsigned-long))

(defmysqlfun ("mysql_affected_rows" mysql-affected-rows) :unsigned-long
  (mysql :pointer))

(defmysqlfun ("mysql_character_set_name" mysql-character-set-name) :string
  (mysql :pointer))

(defmysqlfun ("mysql_ping" mysql-ping) :int
  (mysql :pointer))

(defmysqlfun ("mysql_query" mysql-query) :int
  (mysql :pointer)
  (statement :string))

(defmysqlfun ("mysql_real_escape_string" mysql-real-escape-string) :unsigned-long
  (mysql :pointer)
  (to    :string)
  (from  :string)
  (length :unsigned-int))

(defmysqlfun ("mysql_escape_string" mysql-escape-string) :unsigned-long
  (to    :string)
  (from  :string)
  (length :unsigned-int))

(defmysqlfun ("mysql_field_count" mysql-field-count) :unsigned-int
  (mysql :pointer))

(defmysqlfun ("mysql_get_client_version" mysql-get-client-version) :unsigned-long)

(defmysqlfun ("mysql_get_character_set_info" mysql-get-character-set-info) :void
  (mysql :pointer)
  (cs :pointer))

(defmysqlfun ("mysql_set_character_set" mysql-set-character-set) :int
  (mysql :pointer)
  (csname :string))

(defmysqlfun ("mysql_get_server_version" mysql-get-server-version) :unsigned-long
  (mysql :pointer))

(defmysqlfun ("mysql_select_db" mysql-select-db) :int
  (mysql :pointer)
  (db :string))

(defmysqlfun ("mysql_store_result" mysql-store-result) :pointer
  (mysql :pointer))

(defmysqlfun ("mysql_use_result" mysql-use-result) :pointer
  (mysql :pointer))

(defmysqlfun ("mysql_num_rows" mysql-num-rows) :unsigned-long
  (mysql-res :pointer))

(defmysqlfun ("mysql_list_dbs" mysql-list-dbs) :pointer
  (mysql :pointer)
  (wild :string))

(defmysqlfun ("mysql_list_tables" mysql-list-tables) :pointer
  (mysql :pointer)
  (wild :string))

(defmysqlfun ("mysql_list_fields" mysql-list-fields) :pointer
  (mysql :pointer)
  (table :string)
  (wild :string))

(defmysqlfun ("mysql_list_processes" mysql-list-processes) :pointer
  (mysql :pointer))

(defmysqlfun ("mysql_fetch_row" mysql-fetch-row) :pointer
  (mysql-res :pointer))

(defmysqlfun ("mysql_free_result" mysql-free-result) :void
  (mysql-res :pointer))

(defmysqlfun ("mysql_fetch_lengths" mysql-fetch-lengths) :pointer
  (mysql-res :pointer))

(defmysqlfun ("mysql_num_fields" mysql-num-fields) :unsigned-int
  (mysql-res :pointer))

(defmysqlfun ("mysql_next_result" mysql-next-result) :int
  (mysql :pointer))

(defmysqlfun ("mysql_fetch_fields" mysql-fetch-fields) :pointer
  (mysql-res :pointer))

(defmysqlfun ("mysql_fetch_field" mysql-fetch-field) :pointer
  (mysql-res :pointer))

(defmysqlfun ("mysql_options" mysql-options) :int
  (mysql :pointer)
  (option :int)
  (arg :pointer))

;; NULL if error
(defmysqlfun ("mysql_stmt_init" mysql-stmt-init) :pointer
  (mysql :pointer))

;; Non-zero if error
(defmysqlfun ("mysql_stmt_prepare" mysql-stmt-prepare) :int
  (stmt :pointer)
  (stmt_str :string)
  (length :unsigned-long))

;; Non-zero if error
(defmysqlfun ("mysql_stmt_bind_param" mysql-stmt-bind-param) :char
  (stmt :pointer)
  (bind :pointer))

(defmysqlfun ("mysql_stmt_sqlstate" mysql-stmt-sqlstate) :string
  (stmt :pointer))

(defmysqlfun ("mysql_stmt_errno" mysql-stmt-errno) :int
  (stmt :pointer))

(defmysqlfun ("mysql_stmt_error" mysql-stmt-error) :string
  (stmt :pointer))

(defmysqlfun ("mysql_stmt_execute" mysql-stmt-execute) :int
  (stmt :pointer))

(defmysqlfun ("mysql_stmt_affected_rows" mysql-stmt-affected-rows) :unsigned-long
  (stmt :pointer))

(defmysqlfun ("mysql_stmt_param_count" mysql-stmt-param-count) :int
  (stmt :pointer))

(defmysqlfun ("mysql_stmt_close" mysql-stmt-close) :char
  (stmt :pointer))

(defcenum enum-field-types
  :decimal :tiny :short :long :float :double :null :timestamp :longlong
  :int24 :date :time :datetime :year :newdate :varchar :bit
  (:unknown 63)
  (:newdecimal 246)
  (:enum 247)
  (:set 248)
  (:tiny-blob 249)
  (:medium-blob 250)
  (:long-blob 251)
  (:blob 252)
  (:var-string 253)
  (:string 254)
  (:geometry 255))

(defcenum enum-options
  :opt-connect-timeout :opt-compress :opt-named-pipe
  :init-command :read-default-file :read-default-group
  :set-charset-dir :set-charset-name :opt-local-infile
  :opt-protocol :shared-memory-base-name :opt-read-timeout
  :opt-write-timeout :opt-use-result
  :opt-use-remote-connection :opt-use-embedded-connection
  :opt-guess-connection :set-client-ip :secure-auth
  :report-data-truncation :opt-reconnect
  :opt-ssl-verify-server-cert)

(defcstruct mysql-field
  (name :string)
  (org-name :string)
  (table :string)
  (org-table :string)
  (db :string)
  (catalog :string)
  (def :string)
  (length :unsigned-long)
  (max-length :unsigned-long)
  (name-length :unsigned-int)
  (org-name-length :unsigned-int)
  (table-length :unsigned-int)
  (org-table-length :unsigned-int)
  (db-length :unsigned-int)
  (catalog-length :unsigned-int)
  (def-length :unsigned-int)
  (flags :unsigned-int)
  (decimals :unsigned-int)
  (charsetnr :unsigned-int)
  (type :int))

(defcstruct character-set
  (number :unsigned-int)
  (state :unsigned-int)
  (csname :string)
  (name :string)
  (comment :string)
  (dir :string)
  (mbminlen :unsigned-int)
  (mbmaxlen :unsigned-int))


;; 5.1
;;(defcstruct mysql-bind
;;  (length :pointer)
;;  (is-null :pointer)
;;  (buffer :pointer)
;;  (error :pointer)
;;  (row-ptr :pointer)
;;  (store-param-func :pointer)
;;  (fetch-result-func :pointer)
;;  (skip-result-func :pointer)
;;  (buffer-length :unsigned-long)
;;  (offset :unsigned-long)
;;  (length-value :unsigned-long)
;;  (param-number :unsigned-int)
;;  (pack-length :unsigned-int)
;;  (buffer-type :int)
;;  (error-value :char)
;;  (is-unsigned :char)
;;  (long-data-used :char)
;;  (is-null-value :char)
;;  (extension :pointer))

;; 5.0
(defcstruct mysql-bind
  (length :pointer)
  (is-null :pointer)
  (buffer :pointer)
  (error :pointer)
  (buffer-type :int)
  (buffer-length :unsigned-long)
  (row-ptr :pointer)
  (offset :unsigned-long)
  (length-value :unsigned-long)
  (param-number :unsigned-int)
  (pack-length :unsigned-int)
  (error-value :char)
  (is-unsigned :char)
  (long-data-used :char)
  (is-null-value :char)
  (store-param-func :pointer)
  (fetch-result-func :pointer)
  (skip-result-func :pointer))

