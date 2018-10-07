(defn sum_of_multiples_interatve [r condition] (reduce + (for [x (range 1 r) :when (condition x)] x)))
(defn sum_of_multiples [ra num] (let [r (- ra 1)] (* (quot r num) (/ (+ num (- r (mod r num))) 2))))

(def intr_result (sum_of_multiples_interatve 1000 (fn [x] (or (zero? (mod x 3)) (zero? (mod x 5))))))
(def alg_results (- (+ (sum_of_multiples 1000 5) (sum_of_multiples 1000 3)) (sum_of_multiples 1000 15)))