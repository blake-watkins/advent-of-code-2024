(asdf:defsystem "advent-of-code-2024"
  :description "Advent of Code 2024"
  :version "0.0.1"
  :author "Blake Watkins <blakewatkins@gmail.com>"
  :licence "GNU General Public License (GPL) version 3"
  :depends-on ("advent-of-code" "iterate" "fset" "str")
  :components ((:file "package")
               (:file "day1" :depends-on ("package"))))