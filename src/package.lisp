(in-package :cl-user)

(defpackage cl-versiontool
  (:use :cl
        #:cl-versiontool/core
        #:cl-versiontool/cli
        )
  (:local-nicknames (:core :cl-versiontool/core)
                    (:cli :cl-versiontool/cli))
)

(in-package :cl-versiontool)
