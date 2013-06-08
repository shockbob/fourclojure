(defn mouse-to-cheese [in]
(let [
  nr (count in)
  nc (count (nth in 0))

  ; a row column pair is okay if it is in the range 0 to nr-1, 0 to nc -1
  ok 
     (fn [[r c]]
        (and (< -1 r nr) (< -1 c nc)))
  ; return true if the input has a C symbol at the given position
  is-cheese? 
     (fn [rc]
       (= \C (get-in in rc)))
  ; return true if the given board has a space at the given position
  is-empty? 
     (fn [bd rc]
        (= \space (get-in bd rc)))
  ; find all the neighbors of the given position
  neighbors 
    (fn [[r c]]
        (filter
           ok
           (map
             (fn [[dr dc]] [(+ dr r)(+ dc c)])
             [[1 0][0 1][-1 0][0 -1]])))
   ; return a set of row column pairs which have the given character at the position given
   find-in-board 
     (fn [board ch]
        (set (for [r (range nr) c (range nc) 
                  :when (= ch (get-in board [r c]))]
                 [r c])))
   ; compute the next board by filling in all the given positions with the M character
   next-board 
     (fn [bd e]
        (vec (for [r (range nr)]
           (apply str (for [c (range nc)]
                            (if (contains? (set e) [r c])
                             \M
                             (get-in bd [r c])))))))
   find-path
      (fn find-path [bd]
         ; 



         (if-let [is (find-in-board bd \M)]
                (let [ns (mapcat neighbors is)
                      es (filter (partial is-empty? bd) ns)]
                  (cond
                    (some is-cheese? ns) true
                    (seq es) (find-path (next-board bd es))))))]
  (true? (find-path in))))


(println (mouse-to-cheese ["MC"]))
(println (mouse-to-cheese ["M#C"]))
