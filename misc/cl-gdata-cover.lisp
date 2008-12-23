;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ut-cover.lisp
;;;; Purpose:       cl-lastfm code coverage.
;;;; Programmer:    Nicolas Lamirault <nlamirault@gmail.com>
;;;;
;;;; This file, part of cl-lastfm, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; cl-lastfm users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************

(require :sb-cover)
     
(declaim (optimize sb-cover:store-coverage-data))

(asdf:oos 'asdf:load-op :cl-lastfm-test)    
(cl-lastfm-test:run-cl-lastfm-test)
(sb-cover:report "/tmp/cl-lastfm-coverage/")
     
(declaim (optimize (sb-cover:store-coverage-data 0)))

