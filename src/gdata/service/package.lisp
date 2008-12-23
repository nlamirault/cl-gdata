;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition file for cl-gdata
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-gdata, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; cl-gdata users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(defpackage :cl-gdata-service
  (:use :cl)
  (:documentation "cl-gdata-service package.")
  (:export #:gdata-service
           #:gdata-service-email
           #:gdata-service-password
           #:gdata-service-account-type
           #:gdata-service-service
           #:gdata-service-source
           #:gdata-service-auth

           #:check-account-type
           #:authenticates
           ;;#:make-request

           ;; conditions
           
           #:gdata-error
           #:gdata-service-error
           #:gdata-request-error

           ;; globals

           #:*source*

           #:+gdata-ok+
           #:+gdata-created+
           #:+gdata-not-modified+
           #:+gdata-bad-request+
           #:+gdata-unauthorized+
           #:+gdata-forbidden+
           #:+gdata-not-found+
           #:+gdata-conflict+
           #:+gdata-internal-error+
           

           #:+gdata-atom+
   ))

