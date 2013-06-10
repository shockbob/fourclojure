

; define some constants
(def blank "_")
(def xPlayer "X")
(def oPlayer "O")
(def corners #{0 2 6 8})
(def center #{4})



(def newBoard {})

(defn printBoard [board]
  "prints a tictac toe board out in three lines"
  (apply str (map (fn [m] (board m blank)) (range 0 9))))

(def r9 [0 1 2 3 4 5 6 7 8])

(defn validMoves [board]
  "given a board, return a vec of valid moves or nil if none exist"
  (if (= 9 (count board))
    nil
    (remove board r9)))

(def validGroups
; the three columns followed by three rows and two diags
  [[0 3 6] [1 4 7] [2 5 8] [0 1 2] [3 4 5] [6 7 8] [0 4 8] [2 4 6]])

(def validSets
  (map #(apply hash-set %) validGroups))

(def subGroups
  (zipmap
    (range 9)
    (for [val (range 9)]
      (map vec (filter #(% val) validSets)))))

(defn won [group]
  "returns xPlayer if all three are xPlayer, ditto for oPlayer, nil otherwise"
    (if (= group [xPlayer xPlayer xPlayer])
       xPlayer
      (if (= group [oPlayer oPlayer oPlayer])
        oPlayer
        nil)))

(defn wonGroup [board inds]
  "calls won for the subset of indices given"
      (won (map board inds)))

(defn move [position player board]
  "add the given player at the given position to the board by calling assoc"
  (assoc board position  (player :value)))

(defn getWinner [board]
  "gets the winner (xPlayer,oPlayer,or nil) for this board"
  (some #(wonGroup board %) validGroups))

(defn getWinnerPos [board pos]
  "getWinner optimized to only check for wins around a given position"
  (some #(wonGroup board %) (subGroups pos)))

(defn isWonPos [board pos]
  "isWon optimized to check for wins around a given position"
  (if (< (count board) 5)
    nil
    (not (nil? (getWinnerPos board pos)))))


(defn isWon [board]
  "return true if the given board has at least one player winning, false otherwise"
  (if (< (count board) 5)
    nil
    (not (nil? (getWinner board)))))

(defn randomMover [l board player opponent]
  "for the given list of moves, return a random member"
  (rand-nth l))

(defn firstMover [l board player opponent]
  "for a given list of moves, return the first member"
  (first l))

(defn lastMover [l board player opponent]
  "for a given list of moves, return the last member"
  (first (reverse l)))

(defn cornerMover [l board player opponent]
  "for a given list of moves, return the first one in the corners set, or nil"
  (some corners l))

(defn winMover [l board player opponent]
  "for a givern list of moves, return the first one where the given player can win, nil otherwise"
  (first (filter #(isWonPos (move % player board) %) l)))

(defn blockerMover [l board player opponent]
  "for a given list of moves, return the first one that would block the opponent"
  (winMover l board opponent player))

(defn centerMover [l board player opponent]
  "for a given list of moves, return the first one that matches the center, nil otherwise"
  (some center l))

(defn multiMover [movers moves board player opponent]
  "for a given list of movers (functions that return a move for a given list of moves, board
  current player, and opponent) let each mover return its suggested move for this input"
  (some identity (map #(% moves board player opponent) movers)))

(defn chooseMove [player opponent moves board]
  (multiMover (player :chooseMove) moves board player opponent))

(defn playOneGame
  ([player1 player2] (playOneGame newBoard player1 player2 []))
  ([board player1 player2 list]
    ; get a list of valid moves for this board
    (let [allMoves (validMoves board)]
      (if (nil? allMoves)
        list
        ; chose a move and create a new board (next) based on that move
        ; see if anybody has won yet
        (let [chosen (chooseMove player1 player2 allMoves board)
              next (move chosen player1 board)
              hasWon (isWonPos next chosen)]
          ; if anybody has won, then return the new board concatted with the current
          ; list, otherwise recur for the next move, switching players
          (if hasWon
            (concat [next] list)
            (recur next player2 player1 (concat [next] list))))))))

(defn playGame
  "play a game with the given moves and players, starting with a blank board,
  returns the last board position"
  ([player1 player2 moves] (playGame  player1 player2 moves newBoard))
  ([player1 player2 moves board]
    (let [chosen (first moves)]
      (if (nil? chosen)
        board
        (let [next (move chosen player1 board)
              hasWon (isWonPos next chosen)]
          (if hasWon
            next
            (recur  player2 player1 (rest moves) next )))))))



(defn playManyGames[player1 player2 max]
  (for [i (range max)]
    (if (even? i)
      (playOneGame player1 player2)
      (playOneGame player2 player1))))

(def player1 {:chooseMove [centerMover  randomMover] :value xPlayer})
(def player2 {:chooseMove [cornerMover  randomMover] :value oPlayer})
(def games (playManyGames player2 player1 1000))
(def winners (map #(getWinner (first %)) games))
(println (frequencies winners))
