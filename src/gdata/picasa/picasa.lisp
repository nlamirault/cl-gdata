;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          picasa.lisp
;;;; Purpose:       cl-gdata Picasa service.
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


(defclass gdata-picasa (cl-gdata-service:gdata-service)
  ()
  (:documentation "A GData Picasa service.
@see-constructor{make-gdata-picasa}"))


(defun make-gdata-picasa (email password &key (account-type :hosted-or-google))
  "Creates a new gdata-picasa object.
@arg[email]{The email of the Google account}
@arg[password]{The password of the Google account}
@see-condition{gdata-error}"
  (cl-gdata-service:check-account-type account-type)
  (make-instance 'gdata-picasa :email email
                               :password password
                               :account-type account-type
                               :service *service*
                               :source cl-gdata-service:*source*))


(defgeneric list-albums (gdata-picasa user &key authentication-p)
  (:documentation "Get a feed of an user's albums.
@arg[gdata-picasa]{a @class{gdata-picasa} instance}
@arg[user]{An username}
@arg[authentication-p]{Specify if the request is done with authentification or not}
@return{A string}
@see-condition{gdata-request-error}
Return a string in XML format which contains the list of albums."))


(defmethod list-albums ((gdata-picasa gdata-picasa) user
                        &key authentication-p)
  (cl-gdata-service::make-request gdata-picasa  
                                  (format nil *list-albums* user)
                                  :get
                                  authentication-p))


(defgeneric list-photos (gdata-picasa user album &key authentication-p)
  (:documentation "Get a feed listing all of the photos in an album belonging
to an user.
@arg[gdata-picasa]{a @class{gdata-picasa} instance}
@arg[user]{An username}
@arg[album]{Name of the photos albun}
@arg[authentication-p]{Specify if the request is done with authentification or not}
@return{A string}
Return a string in XML format which contains the list of photos in this album.
@see-condition{gdata-request-error}"))


(defmethod list-photos ((gdata-picasa gdata-picasa) user album
                        &key authentication-p)
  (cl-gdata-service::make-request gdata-picasa
                                  (format nil *list-photos* user album)
                                  :get
                                  authentication-p))


(defgeneric add-album (gdata-picasa user title summary location access keywords)
  (:documentation "Add a new album for user. Needs authentication.
@arg[gdata-picasa]{a @class{gdata-picasa} instance}
@arg[user]{An username}
@arg[title]{Title of the photos albun}
@arg[summary]{A summary / description for this album}
@arg[location]{Place for a geolocation of this album}
@arg[access]{@code{private} or @code{public}. Public albums are searchable 
by everyone on the internet. Defaults to @code{public}}
@arg[keywords]{A String, keywords separated by coma}
@return{A string}
Return a string which contains XML format representing the new album feed."))


(defmethod add-album ((gdata-picasa gdata-picasa)
                      user title summary location access keywords)
  (let ((request (format nil *add-album* user))
        (album (format nil
            "<entry xmlns='http://www.w3.org/2005/Atom'
    xmlns:media='http://search.yahoo.com/mrss/'
    xmlns:gphoto='http://schemas.google.com/photos/2007'>
  <title type='text'>~A</title>
  <summary type='text'>~A</summary>
  <gphoto:location>~A</gphoto:location>
  <gphoto:access>~A</gphoto:access>
  <gphoto:commentingEnabled>true</gphoto:commentingEnabled>
  <gphoto:timestamp>1152255600000</gphoto:timestamp>
  <media:group>
    <media:keywords>~A</media:keywords>
  </media:group>
  <category scheme='http://schemas.google.com/g/2005#kind'
    term='http://schemas.google.com/photos/2007#album'></category>
</entry>"
            title summary location access keywords)))
    (cl-gdata-service::make-request gdata-picasa
                                    request
                                    :post
                                    t
                                    :content album
                                    :content-type +gdata-atom+)))


(defgeneric delete-album (gdata-picasa user title)
  (:documentation "Delete an new album for user. Needs authentication.
@arg[gdata-picasa]{a @class{gdata-picasa} instance}
@arg[user]{An username}
@arg[title]{Title of the photos albun}
@return{A string}
Return a string which contains XML format representing the deleted album feed."))


(defmethod delete-album ((gdata-picasa gdata-picasa) user title)
  (cl-gdata-service::make-request gdata-picasa
                                  (format nil *delete-album* user title)
                                  :delete
                                  t))


(defgeneric add-photo (gdata-picasa user album filename)
  (:documentation "Add a photo for user's album. Needs authentication.
@arg[gdata-picasa]{a @class{gdata-picasa} instance}
@arg[user]{An username}
@arg[album]{Name of the photos albun}
@arg[filenanem]{The image filename}
@return{A string}
Return a string which contains XML format representing the photo feed."))


(defmethod add-photo ((gdata-picasa gdata-picasa) user album filename)
  (let* ((request (format nil *add-image* user album))
         (photo-feed (format nil
                             "<entry xmlns='http://www.w3.org/2005/Atom'>
  <title>~A</title>
  <summary>~A</summary>
  <category scheme=\"http://schemas.google.com/g/2005#kind\"
    term=\"http://schemas.google.com/photos/2007#photo\"/>
</entry>"
                             (file-namestring filename)
                             "nouvelle photo"))
         headers)
    (multiple-value-bind (content-type content)
        (make-mime-related-stream 
         (cons "application/atom+xml" photo-feed)
         (cons "image/png" filename))
      (when *debug*
        (format t "Post request : ~A~%" content))
      (with-accessors ((auth gdata-service-auth)) gdata-picasa
        (cl-gdata-service:authenticates gdata-picasa)
        (setf headers
              (append (list (cons "Authorization"
                                  (format nil "GoogleLogin auth=~A" auth)))
                      headers)))
      (when cl-gdata-tools:*debug*
        (format t "~&GData :~&Method: ~A~&URL: ~A~&"
                :post request))
      (drakma:http-request request
                           :method :post
                           :content content
                           :content-type content-type
                           :content-length nil
                           :additional-headers headers))))


(defgeneric delete-photo (gdata-picasa user albumid photoid)
  (:documentation ""))


;; (defmethod delete-photo ((gdata-picasa gdata-picasa) user albumid photoid)
;;   (let* ((request (format nil *delete-image* user albumid photoid)))
;;     (make-request gdata-picasa
;;                   request
;;                   :delete
;;                   :authentication-p t)))


