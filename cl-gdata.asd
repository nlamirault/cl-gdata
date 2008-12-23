;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cl-gdata.asd
;;;; Purpose:       ASDF definition for cl-gdata.asd
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-gdata, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; cl-gdata users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :asdf)


(defsystem cl-gdata
    :name "cl-gdata"
    :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
    :maintainer "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
    :version "0.1"
    :licence "Lisp Lesser GNU General Public License"
    :description "Common Lisp wrapper for the Google GData APIs."
    :depends-on (:drakma :cl-ppcre)
    :components
    ((:module :src
              :components
              ((:module :tools
                        :components
                        ((:file "package")
                         (:file "specials" :depends-on ("package"))
                         (:file "mime" :depends-on ("package"))
                         (:file "utils" :depends-on ("package"))))
               (:module :gdata
                        :components
                        ((:module :service
                                  :components
                                  ((:file "package")
                                   (:file "specials" :depends-on ("package"))
                                   (:file "conditions" :depends-on ("package"))
                                   (:file "service" :depends-on ("specials" "conditions"))))
;;;                          (:module :spreadsheets
;;;                                   :components
;;;                                   ((:file "package")
;;;                                    (:file "specials" :depends-on ("package"))
;;;                                    (:file "dao" :depends-on ("specials"))
;;;                                    (:file "spreadsheet" :depends-on ("dao")))
;;;                                   :depends-on (:service))
                         (:module :picasa
                                  :components
                                  ((:file "package")
                                   (:file "specials" :depends-on ("package"))
                                   (:file "picasa" :depends-on ("specials")))
                                  :depends-on (:service))
                         )
                        :depends-on (:tools))
	       ))))


