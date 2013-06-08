;Given a pair of numbers, the start and end point, find a path between the two using only three possible operations:
;double
;halve (odd numbers cannot be halved)
;add 2
;Find the shortest path through the "maze". 
;Because there are multiple shortest paths, you must return the length of the shortest path, not the path itself.

(fn [start goal]
  (letfn [
    ; for each element in the given vector, compute up to three elements (e/2,e+2, and e*2), discarding values that
    ; are not integers
    (next-row 
      [row] (mapcat
                 (fn [z]
                    (filter 
                       integer?
                       (map 
                         (fn [f] (f z 2)) 
                         [/ + *]))) 
                  row))] 

; iterate until you find a row that contains the goal value, returning the length of the path
  (inc 
    (count 
      (take-while 
         (fn [v] (nil? ((set v) goal)))
         (iterate 
            next-row
        [start]))))))
