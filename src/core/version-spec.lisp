(in-package :cl-versiontool/core)

(defun version-string-to-vernum (version-string))

(defun vernum-to-version-string (vernum))

(defun bump-vernum-component (vernum component &optional (by 1)))

(defun version-string-valid-p (version-string))

(defun vernum-valid-p (vernum))
