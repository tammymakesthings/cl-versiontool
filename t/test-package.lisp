(in-package :cl-versiontool/tests)

(plan 2)

(subtest
  "package/cl-versiontool"
  (ok (asdf:find-system "cl-versiontool"))
  (is-type (asdf:find-system "cl-versiontool") 'ASDF/SYSTEM:SYSTEM)
  )

(subtest
  "package/cl-versiontool/tests"
  (ok (asdf:find-system "cl-versiontool/tests"))
  (is-type (asdf:find-system "cl-versiontool/tests") 'ASDF/SYSTEM:SYSTEM)
  )

(finalize)
