;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          service.lisp
;;;; Purpose:       cl-gdata Service for GData.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-gdata, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; cl-gdata users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************



(in-package :cl-gdata-service)


(defun check-account-type (account-type)
  "Check if the Google account type is available.
@arg[account-type]{The Google account type. Must be one of
@code{:google}, @code{:hosted-or-google} or @code{:hosted}}
@return{True is this Google account type is available.}
@see-condition{gdata-error}"
  (unless (or (equal account-type :google)
              (equal account-type :hosted)
              (equal account-type :hosted-or-google))
    (error 'gdata-error
           :message (format nil "Unknown Account type ~A" account-type))))


(defclass gdata-service ()
  ((email :initform nil
          :initarg :email
          :accessor gdata-service-email)
   (password :initform nil
             :initarg :password
             :accessor gdata-service-password)
   (account-type :initform nil
                 :initarg :account-type
                 :accessor gdata-service-account-type)
   (service :initform nil
            :initarg :service
            :accessor gdata-service-service)
   (source :initform nil
           :initarg :source
           :accessor gdata-service-source)
   (auth :initform nil
         :initarg :auth
         :accessor gdata-service-auth))
  (:documentation "The superclass of all Google service.
Subclasses of @code{gdata-service} represents Google service available using
cl-gdata : Picasa.
@see-slot{gdata-service-email}
@see-slot{gdata-service-password}
@see-slot{gdata-service-account-type}
@see-slot{gdata-service-service}
@see-slot{gdata-service-source}
@see-slot{gdata-service-auth}"))


(setf (documentation 'gdata-service-email 'function)
      "@arg[instance]{a @class{gdata-service}}
       @return{a string}
       Return the email of the Google account.")

(setf (documentation 'gdata-service-password 'function)
      "@arg[instance]{a @class{gdata-service}}
       @return{a string}
       Return the password of the Google account.")

(setf (documentation 'gdata-service-account-type 'function)
      "@arg[instance]{a @class{gdata-service}}
       @return{a keyword}
       Return the type of account to use. Use @code{:google} for
regular Google accounts or @code{:hosted} for Google Apps accounts,
or @code{:hosted-or-google} to try finding a hosted account first and,
if it doesn't exist, try finding a regular Google account.
Default value: @code{:hosted-or-google}.")

(setf (documentation 'gdata-service-service 'function)
      "@arg[instance]{a @class{gdata-service}}
       @return{a string}
       The desired service for which credentials will
be obtained.")


(setf (documentation 'gdata-service-source 'function)
      "@arg[instance]{a @class{gdata-service}}
       @return{a string}
       Return the name of the user's application.")

(setf (documentation 'gdata-service-auth 'function)
      "@arg[instance]{a @class{gdata-service}}
       @return{a string}
       The auth token used for authenticating requests")



(defgeneric authenticates (gdata-service)
  (:documentation "Make a HTTP request to authenticate.
If HTTP return code is 200, update the AUTH token of gdata-service.
@arg[gdata-service]{a @class{gdata-service} instance}
@see-condition{gdata-service-error}
@see-condition{gdata-error}"))


(defmethod authenticates ((gdata-service gdata-service))
  (with-slots (email password service source account-type) gdata-service
    (let ((params (list (cons "Email" email)
                        (cons "Passwd" password)
                        (cons "service" service)
                        (cons "source" source))))
      (cond ((equal account-type :google)
             (push (cons "AccountType" "GOOGLE") params))
            ((equal account-type :hosted)
             (push (cons "Account-Type" "HOSTED") params))
            ((equal account-type :hosted-or-google)
             (push (cons "Account-Type" "HOSTED_OR_GOOGLE") params))
            (t (error 'gdata-error
                      :message (format nil "Unknown Account type ~A" account-type))))
      (when cl-gdata-tools:*debug*
        (format t "~&Google Account : ~A : ~A~%" *login-url* params))
      (multiple-value-bind (body-stream status-code headers uri stream must-close)
          (drakma:http-request *login-url* :method :post :parameters params)       
        (declare (ignore headers uri stream must-close))
        (if (and status-code (= status-code 200))
            (progn
              (when cl-gdata-tools:*debug*
                (format t "Google Authenticate Ok."))
              ;;body-stream)
              (let ((response (cl-gdata-tools:trim-string body-stream))
                    auth)
                (cl-ppcre:do-matches (s e "Auth=" response)
                  (setf auth (subseq response e)))
                (when auth
                  (setf (gdata-service-auth gdata-service) auth)
                  auth)))
            (let* ((error-type
                    (cl-ppcre:regex-replace-all "Error="
                                                (cl-gdata-tools:trim-string body-stream)
                                                ""))
                   (account-error (find error-type *authentication-errors*
                                   :test #'string-equal :key #'car)))
              (format t "Account error ~A ~A~%" error-type account-error)
              (if account-error
                  (error 'gdata-service-error :name (car account-error)
                                              :message (cdr account-error))
                  (error 'gdata-error
                         :message (format nil "Google account authenticate problem ~A"
                                          error-type)))))))))


;; (defmacro perform-request ((body-stream status-code headers uri stream must-close)
;;                            gdata-service request method 
;;                            &body body
;;                            &key additional-headers authentication-p
;;                            )
;;   `(with-slots (auth) gdata-service
;;      (when authentication-p
;;        (authenticates gdata-service)
;;        (setf headers
;;              (append (list (cons "Authorization"
;;                                  (format nil "GoogleLogin auth=~A" auth)))
;;                      headers)))
;;      (when cl-gdata-tools:*debug*
;;        (format t "~&GData :~&Method: ~A~&URL: ~A~&"
;;                method request))
;;      (multiple-value-bind (,body-stream ,status-code ,headers ,uri ,stream ,must-close)
;;          ,@body
;;          (declare (,ignore ,headers ,uri ,stream ,must-close))
;;        (if (and ,status-code (or (= ,status-code cl-gdata-service:+gdata-ok+)
;;                                  (= ,status-code cl-gdata-service:+gdata-created+)))
;;          (flexi-streams:octets-to-string ,body-stream :external-format :utf-8)
;;          (error 'gdata-request-error
;;                 :code ,status-code
;;                 :message ,body-stream)))))


(defgeneric make-request (gdata-service request method authentication-p
                          &rest args)
  (:documentation "Make an HTTP request to Google.
@arg[gdata-service]{a @class{gdata-service} instance}
@arg[request]{The HTTP request to performe}
@arg[method]{The HTTP method. Must be :get, :post, :delete or :put}
@arg[authentication-p]{Specify if the request is done with authentification or not}
@return{a string}
@see-condition{gdata-request-error}
Return a string which contains XML data."))


(defmethod make-request ((gdata-service gdata-service) request method
                         authentication-p &rest args)
                         
  (with-slots (auth) gdata-service
    (let (http-headers)
      (when authentication-p
        (authenticates gdata-service)
        (setf http-headers
              (append (list (cons "Authorization"
                                  (format nil "GoogleLogin auth=~A" auth)))
                      http-headers)))
      (when cl-gdata-tools:*debug*
        (format t "~&GData :~&Method: ~A~&URL: ~A~&Headers: ~A~%"
              method request http-headers))
      (multiple-value-bind (body-stream status-code headers uri stream must-close)
          (apply #'drakma:http-request
                 request
                 :method method
                 :additional-headers http-headers
                 args)
        (declare (ignore headers uri stream must-close))
        (if (and status-code (or (= status-code cl-gdata-service:+gdata-ok+)
                                 (= status-code cl-gdata-service:+gdata-created+)))
            (flexi-streams:octets-to-string body-stream :external-format :utf-8)
            (error 'gdata-request-error
                   :code status-code
                   :message body-stream))))))
  
