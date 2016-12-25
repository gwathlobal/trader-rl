;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

(defpackage trader-rl  
  (:use :common-lisp :asdf)
  (:export :trader-rl-main
           :make-exec))  
 
(in-package :trader-rl)  

(defsystem trader-rl  
  :name "Trader Rl"
  :description "A trader-themed roguelike."  
  :version "0.1"  
  :author "Gwathlobal"    
  :depends-on (lispbuilder-sdl cl-store)
  :components
    (
     (:file "globals")
     (:file "data-structures")
     (:file "inventory")
     (:file "world")
     (:file "generate-world")
     (:file "base-window")
     (:file "window-intro" :depends-on ("base-window"))
     (:file "window-settlement" :depends-on ("base-window"))
     (:file "window-leave-settlement" :depends-on ("base-window"))
     (:file "window-marketplace" :depends-on ("base-window"))
     (:file "window-win-game" :depends-on ("base-window"))
     (:file "window-capital" :depends-on ("base-window"))
     (:file "trader-rl")
     ))