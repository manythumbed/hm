(ns hm.test.core
  (:use [hm.core])
  (:use [clojure.test])
	(:use [midje.sweet]))

(let [x (make-variable 1)]
	(fact (get-instance x) => nil)
	(fact (is-variable? x) => true)
	(fact (has-instance? x) => false)
	(fact (prune x) => x))

(let [x (make-variable 1)]
	(set-instance x "a")
	(fact (get-instance x) => "a")
	(fact (is-variable? x) => true)
	(fact (has-instance? x) => true)
	(fact (prune x) => "a"))

(let [x (make-variable 1)
			y (make-variable 2)
			z (make-variable 3)
			a t-boolean]
	(set-instance x y)
	(set-instance y z)
	(set-instance z a)
	(fact (prune x) => a)
	(fact (get-instance y) => a))

(let [x (make-variable 1)
			f (make-function t-integer x)]
	(fact (unify x f) => (throws Exception "Recursive unification")))
