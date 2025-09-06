(defpackage #:shoelace-hsx-test/dsl
  (:use #:cl
        #:rove
        #:shoelace-hsx/dsl)
  (:import-from #:shoelace-hsx/builtin)
  (:import-from #:shoelace-hsx/element
                #:element-props
                #:element-children))
(in-package #:shoelace-hsx-test/dsl)

(defcomp ~comp1 (&key children)
  (shoelace-hsx (div children)))

(deftest detect-elements-test
  (testing "detect-tags"
    (ok (expands '(shoelace-hsx (div div div))
                 '(shoelace-hsx/builtin:div div div)))
    (ok (expands '(shoelace-hsx (div (div div (div))))
                 '(shoelace-hsx/builtin:div
                   (shoelace-hsx/builtin:div
                     div
                     (shoelace-hsx/builtin:div))))))

  (testing "detect-components"
    (ok (expands '(shoelace-hsx (~comp1 (div)))
                 '(~comp1 (shoelace-hsx/builtin:div)))))

  (testing "ignore-malformed-form"
    (ok (expands '(shoelace-hsx (div . div))
                 '(div . div)))
    (ok (expands '(shoelace-hsx ((div)))
                 '((div)))))

  (testing "ignore-cl-form"
    (ok (expands '(shoelace-hsx (labels ((div () "div"))
                         (div)))
                 '(labels ((div () "div"))
                   (div))))))

(deftest dsl-test
  (testing "empty-shoelace-hsx"
    (let ((elm (shoelace-hsx (div))))
      (ok (null (element-props elm)))
      (ok (null (element-children elm)))))
 
  (testing "shoelace-hsx-with-static-props"
    (let ((elm (shoelace-hsx (div :prop1 "value1" :prop2 "value2"))))
      (ok (equal '(:prop1 "value1" :prop2 "value2")
                 (element-props elm)))
      (ok (null (element-children elm)))))
  
  (testing "shoelace-hsx-with-dynamic-props"
    (let* ((props '(:prop1 "value1" :prop2 "value2"))
           (elm (shoelace-hsx (div props))))
      (ok (equal props (element-props elm)))
      (ok (null (element-children elm)))))
  
  (testing "shoelace-hsx-with-children"
    (let ((elm (shoelace-hsx (div
                      "child1"
                      "child2"))))
      (ok (null (element-props elm)))
      (ok (equal (list "child1" "child2") (element-children elm)))))
  
  (testing "shoelace-hsx-with-static-props-and-children"
    (let ((elm (shoelace-hsx (div :prop1 "value1" :prop2 "value2"
                      "child1"
                      "child2"))))
      (ok (equal '(:prop1 "value1" :prop2 "value2")
                 (element-props elm)))
      (ok (equal (list "child1" "child2") (element-children elm)))))
  
  (testing "shoelace-hsx-with-dynamic-props-and-children"
    (let* ((props '(:prop1 "value1" :prop2 "value2"))
           (elm (shoelace-hsx (div props
                       "child1"
                       "child2"))))
      (ok (equal props (element-props elm)))
      (ok (equal (list "child1" "child2") (element-children elm))))))
