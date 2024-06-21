(in-package :asdf-user)

(defparameter *quicklisp-app-depends* (list
                              "uiop" "envy" "log4cl" "clingon" "cl-json"
                              "cl-reexport")
  "The list of dependencies for Quicklisp to load for the cl-versiontool system.")

(defparameter *quicklisp-test-depends* (list
                               "prove" "prove-asdf")
  "The list of dependencies for Quicklisp to load for the cl-versiontool/test system.")

#+quicklisp
(progn
  (ql:quickload *quicklisp-app-depends* :silent t)
  (ql:quickload *quicklisp-test-depends* :silent t))

(asdf:defsystem "cl-versiontool"
  :name "cl-versiontool"
  :description "Version number file management in Common Lisp"
  :long-description #.(with-open-file (stream (merge-pathnames
                                                #p"README.rst"
                                                (or *load-pathname* *compile-file-pathname*))
                                              :if-does-not-exist nil
                                              :direction :input)
                        (when stream
                          (let ((seq (make-array (file-length stream)
                                                 :element-type 'character
                                                 :fill-pointer t)))
                            (setf (fill-pointer seq) (read-sequence seq stream))
                            seq)))
  :version (:read-file-form "version.lisp-expr")

  :author "Tammy Cravit"
  :maintainer "Tammy Cravit"
  :mailto "tammy@tammymakesthings.com"
  :license "MIT"

  :homepage "https://github.com/tammymakesthings/cl-versiontool"
  :bug-tracker "https://github.com/tammymakesthings/cl-versiontool/issues"
  :source-control (:git "https://github.com/tammymakesthings/cl-versiontool.git")

  :defsystem-depends-on (:cl-reexport)
  :depends-on (:uiop :envy :log4cl :clingon :cl-json)

  :build-operation "program-op" ;; leave as is
  :build-pathname #P"../bin/versiontool"
  :entry-point "cl-versiontool/cli:main!"

  :pathname "src"

  :serial t
  :components (
               (:module "core"
                :serial t
                :components ((:file "package")
                             (:file "app-config")
                             (:file "app-context")
                             (:file "version-spec")
                             (:file "version-file")))
               (:module "cli"
                :serial t
                :components ((:file "package")
                             (:file "bump")
                             (:file "get")
                             (:file "set")
                             (:file "toplevel")
                             (:file "main")))
               (:file "package"))

  :in-order-to ((test-op (test-op "cl-versiontool/tests"))))

(asdf:defsystem "cl-versiontool/tests"
  :version (:read-file-form "version.lisp-expr")
  :author "Tammy Cravit"
  :maintainer "Tammy Cravit"
  :mailto "tammy@tammymakesthings.com"
  :license "MIT"

  :defsystem-depends-on (:prove-asdf)
  :depends-on (:uiop :cl-versiontool :prove)

  :pathname "t"
  :serial t

  :components (
               (:file "package")
               (:test-file "test-package")
               (:module "core"
                :serial t
                :components ((:file "package")
                             (:test-file "test-app-config")
                             (:test-file "test-app-context")
                             (:test-file "test-version-spec")
                             (:test-file "test-version-file")))
               (:module "cli"
                :serial t
                :components ((:file "package")
                             (:test-file "test-bump")
                             (:test-file "test-get")
                             (:test-file "test-set")
                             (:test-file "test-toplevel")
                             (:test-file "test-main")))
               )

  :perform (test-op (op c)
                    (progn
                      (setf prove:*default-reporter* :fiveam)
                      (setf prove:*enable-colors* t)
                      (symbol-call :prove-asdf :run-test-system c))))

