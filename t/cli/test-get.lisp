(in-package :cl-versiontool/tests)

(plan 3)

(subtest
  "cli/get/command"
  (is 3 (+ 2 1))
  )

(subtest
  "cli/get/options"
  (is 3 (+ 2 1))
  )

(subtest
  "cli/get/handler"
  (is 3 (+ 2 1))
  )

(finalize)

