;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mime.lisp
;;;; Purpose:       MIME tools
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-gdata, is Copyright (c) 2008 by nicolas lamirault
;;;;
;;;; cl-gdata users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :cl-gdata-tools)


;; Stolen from mel-base


(defun make-boundary-tag ()
  (format nil "~A~A"
          (write-to-string (get-universal-time) :base 36 :escape nil)
          (write-to-string (random 9999999) :base 36 :escape nil)))


(defun make-mime-related-stream (text file-data)
  "Creates a MIME multipart/related content.
FILE-DATA is an assoc list of content-type and filename.
Return the content-type and the MIME content."
  (let* ((boundary-tag (make-boundary-tag))
         (content-type
          (format nil "multipart/related; boundary=\"~A\""
                  boundary-tag))
         mime-data)
    (with-output-to-string (stream)
;;;         (format stream "Content-type: multipart/related; boundary=\"~A\"~A~A"
;;;               boundary-tag #\return #\linefeed)
      (format stream "MIME-version: 1.0~A~A" #\return #\linefeed)
      (format stream "This is a MIME Multipart Message.~A~A~A~A"
              #\return #\linefeed #\return #\linefeed)
      (format stream "--~A~A~A~A~A"
              boundary-tag #\return #\linefeed #\return #\linefeed)
      (format stream "Content-Type: ~A~A~A~A~A~A"
              (car text) #\return #\linefeed #\return #\linefeed (cdr text))
      (format stream "~A~A--~A~A~A"
              #\return #\linefeed boundary-tag #\return #\linefeed)
      (format stream "Content-Type: ~A~A~A" (car file-data) #\return #\linefeed)
      ;;(format stream "Content-Transfer-Encoding: base64~A~A" #\return #\linefeed)
      (format stream "Content-Disposition: attachment; filename=\"~A\"~A~A"
              (cdr file-data) #\return #\linefeed)
      (write-char #\return stream)
      (write-char #\linefeed stream)
      (with-open-file (is (cdr file-data)
                          :direction :input
                          :element-type '(unsigned-byte 8))
          (loop with buffer = (make-array 60 :element-type '(unsigned-byte 8))
             for count = (read-sequence buffer is)
             while (> count 0)
             do (write-string (flexi-streams:octets-to-string buffer) stream)))
             ;;(s-base64:encode-base64 is stream)
             ;;(write-sequence is stream)
      (write-char #\return stream)
      (write-char #\linefeed stream)
      (format stream "--~A--~A~A"  boundary-tag #\return #\linefeed)
      (setf mime-data (get-output-stream-string stream)))
    ;;mime-data))
    (values content-type mime-data)))



