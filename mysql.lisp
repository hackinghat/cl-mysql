(defpackage com.hackinghat.cl-mysql
  (:use :cffi :cl)
  (:nicknames "CL-MYSQL"))

(in-package cl-mysql)

(define-foreign-library libmysqlclient
  (t (:default "libmysqlclient")))

(use-foreign-library libmysqlclient)

(defconstant +commands-out-of-sync+ 2014)
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

(defcfun ("mysql_init" mysql-init) :pointer
  (mysql :pointer))

(defcfun ("mysql_close" mysql-close) :pointer
  (mysql :pointer))

(defcfun ("mysql_error" mysql-error) :string
  (mysql :pointer))

(defcfun ("mysql_real_connect" mysql-real-connect) :pointer
  (mysql :pointer)
  (host :string)
  (user :string)
  (password :string)
  (database :string)
  (port :int)
  (unix-socket :string)
  (client-flag :unsigned-long))

(defcfun ("mysql_query" mysql-query) :int
  (mysql :pointer)
  (statement :string))

(defcfun ("mysql_field_count" mysql-field-count) :unsigned-int
  (mysql :pointer))

(defcfun ("mysql_select_db" mysql-select-db) :int
  (mysql :pointer)
  (db :string))

(defcfun ("mysql_store_result" mysql-store-result) :pointer
  (mysql :pointer))

(defcfun ("mysql_num_rows" mysql-num-rows) :unsigned-long-long
  (mysql-res :pointer))

(defcfun ("mysql_affected_rows" mysql-affected-rows) :unsigned-long-long
  (mysql :pointer))

(defcfun ("mysql_character_set_name" mysql-character-set-name) :string
  (mysql :pointer))

(defcfun ("mysql_fetch_row" mysql-fetch-row) :pointer
  (mysql-res :pointer))

(defcfun ("mysql_free_result" mysql-free-result) :void
  (mysql-res :pointer))

(defcfun ("mysql_fetch_lengths" mysql-fetch-lengths) :pointer
  (mysql-res :pointer))

(defcfun ("mysql_num_fields" mysql-num-fields) :unsigned-int
  (mysql-res :pointer))

(defcfun ("mysql_next_result" mysql-next-result) :int
  (mysql :pointer))

(defcenum enum-field-types
  :decimal :tiny :short :long :float :double :null :timestamp :longlong
  :date :time :datetime :year :newdate :varchar :bit
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

(defcfun ("mysql_fetch_fields" mysql-fetch-fields) :pointer
  (mysql-res :pointer))


;(defparameter *m* (mysql-init (null-pointer)))
;(mysql-real-connect *m* "localhost" "stkni" (null-pointer) "dpbsdk" 0 (null-pointer) 0)
;(mysql-query *m* "SELECT COUNT(*) FROM price")
;(defparameter *r* (mysql-store-result *m*))
;(mysql-num-fields *r*)
;(defparameter *f* (mysql-fetch-fields *r*))
;(foreign-slot-value *f* 'mysql-field 'name)
;(foreign-enum-keyword 'enum-field-types (foreign-slot-value *f* 'mysql-field 'type))
;(defparameter *row* (mysql-fetch-row *r*))
;(mem-ref *row* :string)
;(mysql-free-result *f*)
;(mysql-next-result *r*)
;