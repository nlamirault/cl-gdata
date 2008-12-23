;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          conditions.lisp
;;;; Purpose:       cl-gdata-service conditions.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-gdata, is Copyright (c) 2008, 2009 by Nicolas Lamirault
;;;;
;;;; cl-gdata users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :cl-gdata-service)



(define-condition gdata-error (simple-error)
  ((message :reader message
            :initarg :message
            :documentation "Description of this error.."))
  (:documentation "GData main error."))



(define-condition gdata-service-error (gdata-error)
  ((name :reader name
         :initarg :name))
  (:documentation "Condition raised when the Google web services 
are down.")
  (:report (lambda (condition stream)
	     (format stream "Google authenticate problem: ~A."
                     (message condition)))))


(define-condition gdata-request-error (gdata-error)
  ((code :reader code
         :initarg :code
         :documentation "HTTP code error."))
  (:documentation "Condition raised when an invalide request to the
Google web services is performed.")
  (:report (lambda (condition stream)
	     (format stream "Google Data problem:~%Http error: ~A~%Message: ~A"
                     (code condition)
                     (message condition)))))






