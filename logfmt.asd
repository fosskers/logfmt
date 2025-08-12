(defsystem "logfmt"
  :version "0.0.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :homepage "https://github.com/fosskers/logfmt"
  :depends-on (:parcom :parcom/datetime)
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "parser"))))
  :description "")

(defsystem "logfmt/tests"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :depends-on (:logfmt :parachute)
  :components ((:module "tests" :components ((:file "tests"))))
  :perform (test-op (op c) (symbol-call :parachute :test :logfmt/tests)))
