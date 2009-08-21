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
(defpackage com.hackinghat.cl-mysql
  (:use :cl)
  (:nicknames "CL-MYSQL")
  (:shadowing-import-from "CL-MYSQL-SYSTEM"
	  #:connect #:query #:use #:disconnect #:ping #:option
	  #:client-version #:server-version
	  #:list-dbs #:list-tables #:list-processes #:list-fields
	  #:escape-string #:next-result-set #:next-row #:*type-map*
	  #:nth-row #:with-rows #:result-set-fields #:process-result-set
	  #:opt-connect-timeout #:opt-compress #:opt-named-pipe
	  #:init-command #:read-default-file #:read-default-group
          #:+client-compress+  #:+client-found-rows+    #:+client-ignore-sigpipe+
	  #:+client-ignore-space+  #:+client-interactive+ #:+client-local-files+
	  #:+client-multi-statements+  #:+client-multi-results+  #:+client-no-schema+
	  #:+client-ssl+  #:+client-remember-options+
	  #:set-charset-dir #:set-charset-name #:opt-local-infile
	  #:opt-protocol #:shared-memory-base-name #:opt-read-timeout
	  #:opt-write-timeout #:opt-use-result
	  #:opt-use-remote-connection #:opt-use-embedded-connection
	  #:opt-guess-connection #:set-client-ip #:secure-auth
	  #:report-data-truncation #:opt-reconnect
	  #:opt-ssl-verify-server-cert)
  (:export #:connect #:query #:use #:disconnect #:ping #:option
	   #:client-version #:server-version
	   #:list-dbs #:list-tables #:list-processes #:list-fields
	   #:escape-string #:next-result-set #:next-row #:*type-map*
     	   #:nth-row #:with-rows #:result-set-fields #:process-result-set
	   #:+client-compress+  #:+client-found-rows+    #:+client-ignore-sigpipe+
	   #:+client-ignore-space+  #:+client-interactive+ #:+client-local-files+
	   #:+client-multi-statements+  #:+client-multi-results+  #:+client-no-schema+
	   #:+client-ssl+  #:+client-remember-options+
	   #:opt-connect-timeout #:opt-compress #:opt-named-pipe
	   #:init-command #:read-default-file #:read-default-group
	   #:set-charset-dir #:set-charset-name #:opt-local-infile
	   #:opt-protocol #:shared-memory-base-name #:opt-read-timeout
	   #:opt-write-timeout #:opt-use-result
	   #:opt-use-remote-connection #:opt-use-embedded-connection
	   #:opt-guess-connection #:set-client-ip #:secure-auth
	   #:report-data-truncation #:opt-reconnect
	   #:opt-ssl-verify-server-cert))

