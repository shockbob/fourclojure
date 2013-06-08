; calculate a new Game of Life generation based on the given one
; board is given as a vector of strings, with spaces for dead cells and # 
; characters for living ones

(defn next-life [board]
(let [ 
; replace all the spaces with zeros and all the #'s with ones to make
; it easier to count neighbors
board (vec (map #(vec (replace {\space 0 \# 1} %)) board))
nr (count board)
nc (count (board 0))
deltas (fn [[r c]]
       (for [dr [-1 0 1] 
             dc (if (= 0 dr) 
                  [-1 1] 
                  [-1 0 1]) ] [(+ r dr) (+ c dc)]))

inrange (fn [i b]
  (< -1 i b))

; get a count of neighbors for the given row column pair
neighbors 
   #(reduce +
       (map (fn [[r c]]
                (if (and (inrange r nr) (inrange c nc))
                   (get-in board [r c])
                   0))
         (deltas %)))

; for a given cell, calculate if the new cell is going to be dead or alive
; based on whether it is 1 (alive) or 0 (dead) and how many neighbors, default is dead
new-gen  #({ [1 3] \#
             [1 2] \#
             [0 3] \#} % \space )]

; calculate a new board based on the existing one
  (map (partial apply str)
        (partition nr
          (for [r (range nr)
                c (range nc)]
         (new-gen [(get-in board [r c]) (neighbors [r c])]))))))

(println (take 4 (iterate next-life [" # "
                                " # "
                                " # "])))
