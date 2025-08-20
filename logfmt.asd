(defsystem "logfmt"
  :version "1.0.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :homepage "https://github.com/fosskers/logfmt"
  :depends-on (:parcom)
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "parser"))))
  :description "Parser for the logfmt logging style.")
