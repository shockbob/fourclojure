; find if the given word can be put in the crossword puzzle
; the puzzle is given as a vector of strings, where _ is an unsolved grid cell, and # is a delimeter between words
; both horizonal and vertical directions are considered

(defn find-puzzle [word puzzle]
  (letfn [
    ; get rid of all the spaces and return a collection of words in the puzzle
    (split-up [puzzlepart] 
       (mapcat 
         #(partition-by #{\#} (remove #{\ } %))
         puzzlepart))

    ; grab all the vertical puzzle rows
    (verticals [puzzle] 
        (for [c (range (count (puzzle 0)))]
             (for [r (range (count puzzle))]
                  (get-in puzzle [r c]))))
    
    ; a given puzzle cell matches the given word
    ; if it is the same size as the word 
    ; and every character in the puzzle cell matches the corresponding character or is an underscore 
    (match [puzzlecells]
      (and (= (count puzzlecells)(count word))
        (every? 
          (fn [[a b]] (or (= a \_) (= a b)))
          (map 
            vector 
            puzzlecells 
            word))))]
  ; the given word is found in either the vertical or horizontal direction
  (true? (or 
    (some match (split-up puzzle)) 
    (some match (split-up (verticals puzzle)))))))


(println (find-puzzle "abc" ["#_bc"]))
(println (find-puzzle "bc" ["#_bc"]))
