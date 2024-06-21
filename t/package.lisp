(in-package :cl-user)

(defpackage :cl-versiontool/tests
  (:use :cl :prove :uiop)
)

(in-package :cl-versiontool/tests)

(setf prove:*default-reporter* :dot)
(setf prove:*enable-colors* t)

