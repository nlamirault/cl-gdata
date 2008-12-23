;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          lastfm-atdoc.lisp
;;;; Purpose:       cl-gdata documentation tool
;;;; Programmer:    Nicolas Lamirault <nlamirault@gmail.com>
;;;;
;;;; This file, part of cl-gdata, is Copyright (c) 2009 by Nicolas Lamirault
;;;;
;;;; cl-gdata users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************


(require :asdf)

(asdf:oos 'asdf:load-op :atdoc)
(asdf:oos 'asdf:load-op :cl-gdata)


(let* ((path (namestring
              (asdf:component-relative-pathname (asdf:find-system :cl-gdata))))
       (dir (concatenate 'string path "/www/api/")))
       (ensure-directories-exist dir)
  (atdoc:generate-html-documentation '(:cl-gdata-service
                                       :cl-gdata-picasa)
                                     dir
                                     :index-title "cl-gdata API reference"
                                     :heading "Google GData for Common Lisp"
                                     ;;:css "orange-sans.css"
                                     :single-page-p t
                                     :include-internal-symbols-p nil)
  (atdoc:generate-latex-documentation '(:cl-gdata-service
                                        :cl-gdata-picasa)
                                      dir
                                      :title "cl-gdata API reference")
  (atdoc:generate-info-documentation '(:cl-gdata-service
                                       :cl-gdata-picasa)
                                     dir
                                     :name "cl-gdata"
                                     :title "cl-gdata API reference"))
