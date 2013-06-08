; convert an integer to a roman string
(fn [value]
(apply str 
  (let [value (str value)
        ; i is 1,10,100,1000, v is 50,500,5000, x is 10,100,1000 
        i "IXCM" 
        v "VLD" 
        x "XCM"
        ; representation of each digit in roman numerals
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