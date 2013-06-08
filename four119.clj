(defn find-wins [pc board]
  (let  [
    who-won (fn [board]
      (some 
        {[:o :o :o] :o [:x :x :x] :x}
        (partition 3
          (map
            board
            [0 1 2 3 4 5 6 7 8
             0 3 6 1 4 7 2 5 8
             0 4 8 2 4 6]))))
    flat-board (vec (flatten board))
    empties (for [i (range 9) 
                :when (= :e (flat-board i))] i)
    wins (for [e empties 
              :when (= pc (who-won (assoc flat-board e pc)))] e)]
    (set 
      (map 
        (fn [win] [(quot win 3) (mod win 3)]) 
         wins))))

(println (find-wins :x [[:x :x :e]
                        [:x :o :o]
                        [:e :x :e]]))
