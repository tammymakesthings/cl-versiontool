(in-package :cl-versiontool/tests)

(plan 3)

(subtest
  "cli/bump/command"
  (is 3 (+ 2 1))
  )

(subtest
  "cli/bump/options"
  (is 3 (+ 2 1))
  )

(subtest
  "cli/bump/handler"
  (is 3 (+ 2 1))
  )

(finalize)

