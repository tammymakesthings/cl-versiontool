(in-package :cl-versiontool/tests)

(plan 3)

(subtest
  "cli/set/command"
  (is 3 (+ 2 1))
  )

(subtest
  "cli/set/options"
  (is 3 (+ 2 1))
  )

(subtest
  "cli/set/handler"
  (is 3 (+ 2 1))
  )

(finalize)

