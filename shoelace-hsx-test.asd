(defsystem "shoelace-hsx-test"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("rove"
               "shoelace-hsx-test/utils"
               "shoelace-hsx-test/element"
               "shoelace-hsx-test/dsl")
  :perform (test-op (o c) (symbol-call :rove :run c :style :dot)))
