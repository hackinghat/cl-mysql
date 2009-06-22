;;;; -*- Mode: Lisp -*-
;;;; $Id$
;;;; Author: Steve Knight <stknig@gmail.com>
;;;;
(in-package "CL-MYSQL-SYSTEM")

(eval-when (:load-toplevel)
  #+allegro (mp:start-scheduler))

(defun make-lock (name)
  #+sb-thread (sb-thread:make-mutex :name name)
  #+allegro (mp:make-process-lock :name name))

(defun release-lock (lock)
  #+sb-thread (sb-thread:release-mutex lock)
  #+allegro (mp:process-unlock lock))

(defun get-lock (lock)
  #+sb-thread (sb-thread:get-mutex lock)
  #+allegro (mp:process-lock lock))

(defun make-wait-resource ()
  #+sb-thread (sb-thread:make-waitqueue))

(defun wait-on-threads (threads)
  #+sb-thread (mapcar #'sb-thread:join-thread threads)
  #+allegro (mapcar (lambda (p)
                      (mp:process-wait "Joining ..." (lambda ()
                                                       (not (mp:process-alive-p p))))) threads))

(defmacro with-lock (lock &body body)
  #+sb-thread `(sb-thread:with-recursive-lock (,lock) ,@body)
  #+allegro `(mp:with-process-lock (,lock) ,@body)
  #-(or sb-thread allegro) `(progn ,@body))

(defun start-thread-in-nsecs (fn n)
  #+sb-thread (sb-thread:make-thread (lambda ()
                                       (sleep n)
                                       (funcall fn)))
  #+allegro (mp:process-run-function nil (lambda ()
                                           (sleep n)
                                           (funcall fn))))
    
(defun pool-wait (pool)
  ;; With SBCL threads we can use condition variables to wake us up 
  #+sb-thread (sb-thread:with-mutex ((wait-queue-lock pool))
                                    (sb-thread:condition-wait (wait-queue pool) 
                                                              (wait-queue-lock pool)))
  ;; With Allegro CL we will use the process-wait to run a monitor thread
  ;; on the condition
  #+allegro (mp:process-wait "Waiting for pool" #'can-aquire-lock pool))

(defun pool-notify (pool)
  #+sb-thread (sb-thread:with-mutex ((wait-queue-lock pool))
                                    (sb-thread:condition-notify (wait-queue pool))))

