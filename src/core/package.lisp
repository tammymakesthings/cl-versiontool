(in-package :cl-user)

(defpackage cl-versiontool/core
  (:use :cl :cl-json :log4cl :envy)
  (:export #:find-version-file
           #:read-version-file
           #:write-version-file
           #:version-string-to-vernum
           #:vernum-to-version-string
           #:bump-vernum-component
           #:version-string-valid-p
           #:vernum-valid-p
           #:read-app-config
           #:setup-logger
           #:make-app-context
           #:read-app-config-file
           #:setup-app-context)
)

(in-package :cl-versiontool/core)
