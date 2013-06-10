; convert an integer to a roman string
(defn to-roman [value]
(apply str 
  (let [value (str value)
        ; i is 1,10,100,1000, v is 50,500,5000, x is 10,100,1000 
        i "IXCM" 
        v "VLD" 
        x "XCM"
        ; representation of each digit in roman numerals
        ; (for example, 3 is III, 30 is XXX, 300 is CCC, etc.
        ms  {\1 [i] \2 [i i] \3 [i i i]
             \4 [i v] \5 [v] \6 [v i]
             \7 [v i i] \8 [v i i i] \9[i x]}
        ; powers for 100 would be 2,1,0 for example
        powers (reverse (range (count value)))]
        ; for every power digit pair, find the corresponding digit in ms
        (mapcat 
          (fn [power digit]
             (map 
               #((vec %) power) 
               (ms digit))) 
            powers 
            value))))

(println (to-roman 123))
(println (to-roman 345))
