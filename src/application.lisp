(defpackage aliya.application
  (:use :cl :uiop))

(in-package :aliya.application)


(defvar *applications* '())

(defvar *application-definations* '())

(defun register-application (name application))

(defun start-application (name)
  (print (format nil "Starting ~A" name)))

(defun start-application (name)
  (print (format nil "Starting ~A" name)))

(defun install-application (name)
  (print (format nil "Starting ~A" name)))

(defun remove-application (name)
  (print (format nil "Starting ~A" name)))

(defun update-application (name)
  (print (format nil "Starting ~A" name)))

(defun enable-application (name)
  (print (format nil "Starting ~A" name)))

(defun disable-application (name)
  (print (format nil "Starting ~A" name)))


