;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition file for cl-gdata-picasa
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-gdata, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; cl-gdata users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(defpackage :cl-gdata-picasa
  (:use :cl
        :cl-gdata-tools
        :cl-gdata-service)
  (:export #:gdata-picasa
           #:make-gdata-picasa

           ;; API

           #:list-albums
           #:add-album
           ;;#:delete-album
           #:list-photos
           #:add-photo
           ;;#:delete-photo

           #:*print-picasa*
           )
   (:documentation "cl-gdata-picasa package.
The Picasa Web Albums Data API allows for websites and programs to integrate 
with Picasa Web Albums."))
