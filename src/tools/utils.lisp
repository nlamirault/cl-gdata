;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sequences.lisp
;;;; Purpose:       cl-gdata utilities
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-gdata, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; cl-gdata users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :cl-gdata-tools)



(defun trim-string (string)
  "Remove Space, Tab and Newline from STRING."
  (string-trim '(#\Space #\Tab #\Newline) string))


(defun debug-mode (debug-mode-p)
  "Enable or disable the debug mode."
  (setf *debug* debug-mode-p
        drakma:*header-stream* (if debug-mode-p
                                   *standard-output*
                                   nil)))

