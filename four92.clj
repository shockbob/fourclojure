; convert a roman numeral to an integer
(def roman-to-int
#(last 
    ; keep track of the max digit up to this point and if a
    ; digit greater or equal to that digit is encountered, add it
    ; otherwise subract it from the sum
    (reduce 
      (fn [[max-digit sum] digit]
         (if (<  digit max-digit)
            [max-digit (- sum digit)]
            [digit (+ sum digit)])) 
      [0 0]
      ; convert each digit from its roman character to the equivalent integer
      ; after reversing it 
      (map 
         {\I 1 \V 5 \X 10 \L 50  
          \C 100 \D 500 \M 1000} 
       (reverse %))))
  )

(println (roman-to-int "IIX"))
(println (roman-to-int "XII"))
