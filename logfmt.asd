(defsystem "logfmt"
  :version "0.0.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :homepage "https://github.com/fosskers/logfmt"
  :depends-on (:parcom)
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "parser"))))
  :description "Parser for the logfmt logging style.")

(defsystem "logfmt/flamegraph"
  :depends-on (:logfmt :transducers :arrow-macros)
  :components ((:module "src" :components ((:file "flamegraph"))))
  :description "Generating flamegraph data from log messages.")

(defsystem "logfmt/tests"
  :depends-on (:logfmt :parachute)
  :components ((:module "tests" :components ((:file "tests"))))
  :perform (test-op (op c) (symbol-call :parachute :test :logfmt/tests)))
