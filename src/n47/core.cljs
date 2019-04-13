;; Solves #47 and #48 in Dummit & Foote section 4.5.
;; If you are currently taking algebra with this book, stop reading immediately.
;;   (my clojure is gibberish anyway)

(ns n47.core)

(defn pfactors_pl [n pl]
  (if (= 0 (count pl))
    '()
    (let [p (first pl)
          pf (pfactors_pl n (rest pl))]
      (if (= 0 (mod n p)) (conj pf p)
        pf)
    )
  )
)


(defn remults [n l] 
  (cond
    (= 0 (count l)) l
    (= 0 (mod (first l) n)) (remults n (rest l))
    :else (conj (remults n (rest l)) (first l))
  )
)

(defn erato [l] ;sthenes
  (cond
    (= 0 (count l)) l
    :else 
      (let [n (first l) 
            lt (rest l)]
        (conj (erato (remults n lt)) n)
      )
  )
)

(defn coprime_divisor [n p] 
  (if (= 0 (mod n p))
    (coprime_divisor (/ n p) p)
    n
  )
)

;; We only check primes up to 100, so any larger prime is found by this.
(defn last_prime_factor [n pl]
  (if (= 0 (count pl))
    n
    (last_prime_factor (coprime_divisor n (first pl)) (rest pl))
  )
)

(defn divisor_list_product [dl1 dl2] (for [d1 dl1 d2 dl2] (* d1 d2)))

(defn addp [p l] (conj (map #(* p %) l) 1))

(defn pdivisors [n p] 
  (if (not (= 0 (mod n p)))
    '(1)
    (addp p (pdivisors (/ n p) p))
  )
)

(defn all_pdivisors [n pf] 
  (if (= 0 (count pf))
    '()
    (conj (all_pdivisors n (rest pf)) (pdivisors n (first pf)))
  )
)

(defn divisors_over [n pf] (reduce divisor_list_product (all_pdivisors n pf)))

(defn each_coprime_divisor [n pf] (for [p pf] [p (coprime_divisor n p)]))

(defn contains_el [l] (reduce #(or %1 (= 0 (count %2))) (conj l false)))

(defn listmodp [l p] (map #(mod % p) l))

(defn each_permissible [n pf] 
  (for [p pf] 
    (filter #(= 1 (mod % p))
            ;we omit 1 from the list of possibile values of n_p
            (rest (divisors_over (coprime_divisor n p) pf))
    )
  )
)

(defn each_max_divisor [n pf] 
  (map
    #(loop [v 1]
      (if (= 0 (mod (/ n v) %))
        (recur (* v %))
        v
      )
    ) 
    pf
  )
)

(def primes (erato (range 2 100)))

(defn nonnil [l] 
  (if (= 0 (count l))
    '()
    (let [nnlt (nonnil (rest l)) 
          lf (first l)]
      (if (nil? lf)
        nnlt
        (conj nnlt lf)
      )
    )
  )
)

(defn pnonnil [l]
  (if (= 0 (count l))
    ()
    (do 
      (if (not (nil? (first l))) 
        (println (first l))
      ) 
      (pnonnil (rest l))
    )
  )
)

(def n47_ans (for [n (range 3 10000 2)]
  (let [pf (pfactors_pl n primes)]
    ; if there is a prime factor > 100, n = pk with p > k, so p > d for all d|k.
    (if (= 1 (last_prime_factor n pf))
      (let [ep (each_permissible n pf) 
            pkf (each_max_divisor n pf)]
        (if (not (contains_el ep)) (conj [n] (map vector pf ep pkf)))
      )
    )
  )
))

(println "Section 4.5 number 47:")
(pnonnil n47_ans)
(println (count (nonnil n47_ans)))

(def n48_ans (for [n (range 2 1001 2)]
  (let [pf (pfactors_pl n primes)]
    (if (= 1 (last_prime_factor n pf))
      (let [ep (each_permissible n pf) 
            pkf (each_max_divisor n pf)]
        (if (not (contains_el ep)) (conj [n] (map vector pf ep pkf)))
      )
    )
  )
))

(println "Section 4.5 number 48:")
(pnonnil n48_ans)
(println (count (nonnil n48_ans)))
