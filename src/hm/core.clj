(ns hm.core)

(defn make-variable [id] {:id id :variable true :instance (ref nil)})

(defn set-instance [v value]
	(dosync (ref-set (v :instance) value)))
(defn get-instance [v] @(v :instance))

(defn is-variable? [t] (contains? t :variable))
(defn has-instance? [t] 
	(and (contains? t :instance) (not(nil? (get-instance t)))))

(defn make-operator [name types] {:name name :types types :operator true})
(defn get-name [t] (t :name))
(defn get-types [t] (t :types))

(defn is-operator? [t] (contains? t :operator))

(defn make-function [from to] (make-operator "->" [from to]))

(def t-integer (make-operator "int" []))
(def t-boolean (make-operator "bool" []))

(defn prune [t]
	(cond 
		(and (is-variable? t) (has-instance? t))
			 (set-instance t (prune (get-instance t))) 
		:else t))

(declare occurs-in)
(defn occurs-in-type [v t]
	(let [p (prune t)]
		(cond 
			(= p v) true
			(is-operator? p) (occurs-in v (get-types p))
			:else false)))

(defn occurs-in [t types] 
	(not (nil? (some (fn [t2] (occurs-in-type t t2)) types))))

(defn unify [t1 t2]
	(let [a (prune t1)
				b (prune t2)]
		(cond 	
			(is-variable? a) 
				(if (not (= a b))
					((when (occurs-in-type a b) 
						(throw (Exception. "Recursive unification")))
					(set-instance a b)))
			(and (is-operator? a) (is-variable? b)) (unify b a)
			(and (is-operator? a) (is-operator? b))
				((if 
					(or (not (= (get-name a) (get-name b))) 
						  (not (= (count (get-types a)) (count (get-types b)))))
					(throw (Exception. "Type mismatch")))
				(doall (map unify (get-types a) (get-types b))))
			:else (throw (Exception. "Can't unify"))))) 

