;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          specials.lisp
;;;; Purpose:       cl-gdata-picasa specials informations.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-gdata, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; cl-gdata users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :cl-gdata-picasa)



(defparameter *print-picasa* nil
    "If T, write the customized presentation of the cl-gdata-picasa objects.")


(defparameter *service* "lh2" "The service name for Picasa.")


(defparameter *list-albums*
  "http://picasaweb.google.com/data/feed/api/user/~A?kind=album"
  "URL to get a feed of albums.")


(defparameter *list-photos*
  "http://picasaweb.google.com/data/feed/api/user/~A/album/~A?kind=photo"
  "URL to get a feed listing all of the photos in an album.")


(defparameter *add-album*
  "http://picasaweb.google.com/data/feed/api/user/~A"
  "URL to add a new album.")


(defparameter *delete-album*
  "http://picasaweb.google.com/data/entry/api/user/~A/albumid/~A/~A"
  "URL to delete an album.")


(defparameter *add-image*
  "http://picasaweb.google.com/data/feed/api/user/~A/album/~A"
  "URL to add a new image to an album.")


(defparameter *delete-image*
  "http://picasaweb.google.com/data/entry/api/user/~A/albumid/~A/photoid/~A/versionNumber"
  "URL to add a new image to an album.")

