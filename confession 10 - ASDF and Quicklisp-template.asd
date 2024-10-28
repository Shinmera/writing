(defpackage #:my-project.asdf
  (:use #:cl #:asdf))
(in-package #:my-project.asdf)

(defsystem my-project
  :name "My Project"
  :version "0.0.0"
  :license "My License"
  :author "Edward Example <some@example.com>"
  :maintainer "Edward Example <some@example.com>"
  :description "Some more lengthy description of the project"
  :serial T
  :components ((:file "lisp-source-file")
               (:module "sub-directory"
                :components (:file "subdir-lisp-source-file")))
  :depends-on (:dependency-a
               :dependency-b))
