;;;; vehicles.asd

(asdf:defsystem #:vehicles
  :description "Lisp version of the simulation program used for developing Transparent Neural Networks at Chalmers University."
  :author "Torbjørn Ludvigsen"
  :license "GPLv2"
  :serial t
  :components ((:file "package")
               (:file "vehicles")))

