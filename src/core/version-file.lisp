(in-package :cl-versiontool/core)

(defun find-version-file (project-dir &optional (version-file-type :lispexpr)))
(defun read-version-file (version-file-path))
(defun write-version-file (version-file-path))
