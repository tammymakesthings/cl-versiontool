(in-package :cl-versiontool/tests)

(plan 3)

(subtest
  "cli/toplevel/command"
  (is 3 (+ 2 1))
  )

(subtest
  "cli/toplevel/options"
  (is 3 (+ 2 1))
  )

(subtest
  "cli/toplevel/handler"
  (is 3 (+ 2 1))
  )

(finalize)
