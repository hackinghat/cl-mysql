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
(in-package "CL-MYSQL-SYSTEM")

(defconstant *sleep-period* 0.1)

;;; Copied from bordeaux threads
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or (and allegro multiprocessing)
        armedbear
        (and cmu mp)
        scl
        corman
        (and digitool ccl-5.1)
        (and ecl threads)
        lispworks
        (and openmcl openmcl-native-threads)
        (and sbcl sb-thread)
        (and clisp mt))
  (pushnew :thread-support *features*))

(eval-when (:load-toplevel)
  #+allegro (mp:start-scheduler))

(defun make-lock (name)
  #+sb-thread (sb-thread:make-mutex :name name)
  #+ecl (mp:make-lock :name name)
  #+armedbear (ext:make-thread-lock)
  #+ (and clisp mt) (mt:make-mutex :name name)
  #+allegro (mp:make-process-lock :name name))

(defun make-wait-resource ()
  #+sb-thread (sb-thread:make-waitqueue)
  #+(and clisp mt) (mt:make-exemption))

(defun thread-alive-p (thread)
  #+ecl (mp:process-active-p thread)
  #+armedbear (ext:thread-alive-p thread)
  #- (or ecl sb-thread allegro (and clisp mt)) nil)

(defun wait-on-threads (threads)
  #+sb-thread (mapcar #'sb-thread:join-thread threads)
  #+allegro (mapcar (lambda (p)
                      (mp:process-wait "Joining ..." (lambda ()
                                                       (not (mp:process-alive-p p))))) threads)
  #-(or sb-thread allegro) (loop for th in threads
				 do (loop until (not (thread-alive-p th))
				       do (sleep *sleep-period*))))

(defmacro with-lock (lock &body body)
  #+sb-thread `(sb-thread:with-recursive-lock (,lock) ,@body)
  #+allegro `(mp:with-process-lock (,lock) ,@body)
  #+(and clisp mt) `(mt:with-lock  (,lock) ,@body)
  #+ecl `(mp:with-lock (,lock) ,@body)
  #+armedbear `(ext:with-thread-lock (,lock) ,@body)
  #-(or ecl sb-thread allegro) `(progn ,@body))

(defun start-thread-in-nsecs (fn n)
  #+sb-thread (sb-thread:make-thread (lambda ()
                                       (sleep n)
                                       (funcall fn)))
  #+(and clisp mt) (mt:make-thread (lambda ()
			    (sleep n)
			    (funcall fn)) nil)
  #+armedbear (ext:make-thread (lamba ()
				      (sleep n)
				      (funcall fn)) :name nil)
  #+(or allegro ecl) (mp:process-run-function nil (lambda ()
						    (sleep n)
						    (funcall fn))))
    
(defun pool-wait (pool)
  ;; With SBCL threads we can use condition variables to wake us up 
  #+sb-thread (sb-thread:with-mutex ((wait-queue-lock pool))
                                    (sb-thread:condition-wait (wait-queue pool) 
                                                              (wait-queue-lock pool)))
  ;; With Allegro CL we will use the process-wait to run a monitor thread
  ;; on the condition
  #+allegro (mp:process-wait "Waiting for pool" #'can-aquire-lock pool)
  #+(and clisp mt) (mt:exemption-wait (wait-queue pool)
			     (wait-queue-lock pool))
  #-(or allegro sb-thread (and clisp mt)) (sleep *sleep-period*))

(defun pool-notify (pool)
  #+(and clisp mt) (mt:exemption-signal (wait-queue pool))
  #+sb-thread (sb-thread:with-mutex ((wait-queue-lock pool))
                                    (sb-thread:condition-notify (wait-queue pool))))

