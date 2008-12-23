;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          specials.lisp
;;;; Purpose:       cl-gdata-base specials informations.
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


(defparameter *debug* nil "If T, active some logs.")


(unless (boundp '+gdata-atom+)
  (defconstant +gdata-atom+ "application/atom+xml"))


(defparameter *login-url* "https://www.google.com/accounts/ClientLogin"
  "URL to account authentication")


(defparameter *service* "xapi"
  "Name of the Google service for which authorization is requested.")


(defparameter *source* (format nil "cl-gdata-~A" cl-gdata-tools:*version*))


(unless (boundp '+bad-authentication+)
  (defconstant +bad-authentication+
    '("BadAuthentication" . "The login request used a username or
password that is not recognized.")))


(unless (boundp '+not-verified+)
  (defconstant +not-verified+
    '("NotVerified" . "The account email address has not been verified.
The user will need to access their Google account directly to resolve the issue
before logging in using a non-Google application.")))


(unless (boundp '+terms-not-agreed+)
  (defconstant +terms-not-agreed+
    '("TermsNotAgreed" . "The user has not agreed to terms. The user
will need to access their Google account directly to resolve the issue before
logging in using a non-Google application.")))


(unless (boundp '+captcha-required+)
  (defconstant +captcha-required+
    '("CaptchaRequired" . "A CAPTCHA is required.")))


(unless (boundp '+unknown+)
  (defconstant +unknown+
    '("Unknown" . "The error is unknown or unspecified;
the request contained invalid input or was malformed.")))


(unless (boundp '+account-deleted+)
  (defconstant +account-deleted+
    '("AccountDeleted" . "The user account has been deleted.")))


(unless (boundp '+account-disabled+)
  (defconstant +account-disabled+
    '("AccountDisabled" . "The user account has been disabled.")))


(unless (boundp '+service-disabled+)
  (defconstant +service-disabled+
    '("ServiceDisabled" . "The user's access to the specified service has
been disabled. (The user account may still be valid.)")))


(unless (boundp '+service-unavailable+)
  (defconstant +service-unavailable+
    '("ServiceUnavailable" . "The service is not available; try again later.")))


(defparameter *authentication-errors*
  (list +bad-authentication+ +not-verified+ +terms-not-agreed+ +captcha-required+
        +unknown+ +account-deleted+ +account-disabled+ +service-disabled+
        +service-unavailable+))


;; HTTP status codes of GData protocol.

(unless (boundp '+gdata-ok+)
  (defconstant +gdata-ok+ 200 "No error."))


(unless (boundp '+gdata-created+)
  (defconstant +gdata-created+ 201 "Creation of a resource was successful."))


(unless (boundp '+gdata-not-modified+)
  (defconstant +gdata-not-modified+ 304
    "The resource hasn't changed since the time specified in the request's
If-Modified-Since header."))


(unless (boundp '+gdata-bad-request+)
  (defconstant +gdata-bad-request+ 400
    "Invalid request URI or header, or unsupported nonstandard parameter."))


(unless (boundp '+gdata-unauthorized+)
  (defconstant +gdata-unauthorized+ 401
    "Authorization required."))


(unless (boundp '+gdata-forbidden+)
  (defconstant +gdata-forbidden+ 403
    "Unsupported standard parameter, or authentication or authorization failed."))


(unless (boundp '+gdata-not-found+)
  (defconstant +gdata-not-found+ 404
    "Resource (such as a feed or entry) not found."))


(unless (boundp '+gdata-conflict+)
  (defconstant +gdata-conflict+ 409
    "Specified version number doesn't match resource's latest version number."))


(unless (boundp '+gdata-internal-error+)
  (defconstant +gdata-internal-error+ 500
    "Internal error. This is the default code that is used for all unrecognized
errors."))
    
