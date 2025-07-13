;; Knight's Tour Problem Implementation in Common Lisp
;; Finds a sequence of moves for a knight to visit every square on a chessboard exactly once

(defun knight-tour (board-size &optional (start-row 0) (start-col 0))
  "Solve the knight's tour problem for a board of given size starting from (start-row, start-col)"
  (let ((board (make-array (list board-size board-size) :initial-element 0))
        (move-count 1)
        (total-squares (* board-size board-size)))
    
    ;; Mark starting position
    (setf (aref board start-row start-col) move-count)
    
    ;; Start the recursive search
    (if (knight-tour-recursive board board-size start-row start-col (+ move-count 1) total-squares)
        (progn
          (format t "Knight's tour found!~%")
          (print-board board board-size)
          board)
        (progn
          (format t "No solution found for board size ~a starting from (~a, ~a)~%" 
                  board-size start-row start-col)
          nil))))

(defun knight-tour-recursive (board board-size row col move-num total-squares)
  "Recursive function to find knight's tour using backtracking"
  (if (= move-num (+ total-squares 1))
      t  ; All squares visited successfully
      (let ((moves (get-valid-moves board board-size row col)))
        ;; Sort moves by Warnsdorff's rule (choose move with fewest onward moves)
        (setf moves (sort moves (lambda (move1 move2)
                                  (< (count-valid-moves board board-size (first move1) (second move1))
                                     (count-valid-moves board board-size (first move2) (second move2))))))
        
        ;; Try each valid move
        (dolist (move moves)
          (let ((new-row (first move))
                (new-col (second move)))
            ;; Make the move
            (setf (aref board new-row new-col) move-num)
            
            ;; Recursively try to complete the tour
            (when (knight-tour-recursive board board-size new-row new-col (+ move-num 1) total-squares)
              (return-from knight-tour-recursive t))
            
            ;; Backtrack if the move doesn't lead to a solution
            (setf (aref board new-row new-col) 0)))
        
        ;; No valid move found
        nil)))

(defun get-valid-moves (board board-size row col)
  "Get all valid knight moves from current position"
  (let ((knight-moves '((-2 -1) (-2 1) (-1 -2) (-1 2) (1 -2) (1 2) (2 -1) (2 1)))
        (valid-moves '()))
    
    (dolist (move knight-moves)
      (let ((new-row (+ row (first move)))
            (new-col (+ col (second move))))
        (when (and (>= new-row 0) (< new-row board-size)
                   (>= new-col 0) (< new-col board-size)
                   (= (aref board new-row new-col) 0))
          (push (list new-row new-col) valid-moves))))
    
    valid-moves))

(defun count-valid-moves (board board-size row col)
  "Count the number of valid moves from a given position (used for Warnsdorff's rule)"
  (length (get-valid-moves board board-size row col)))

(defun print-board (board board-size)
  "Print the board with move numbers"
  (format t "~%")
  (dotimes (row board-size)
    (dotimes (col board-size)
      (format t "~3d " (aref board row col)))
    (format t "~%"))
  (format t "~%"))

;; Example usage and test functions
(defun test-knight-tour ()
  "Test the knight's tour with different board sizes"
  (format t "Testing Knight's Tour:~%~%")
  
  ;; Test with 5x5 board
  (format t "5x5 Board:~%")
  (knight-tour 5)
  
  ;; Test with 6x6 board
  (format t "~%6x6 Board:~%")
  (knight-tour 6)
  
  ;; Test with 8x8 board (standard chessboard)
  (format t "~%8x8 Board (this may take a while):~%")
  (knight-tour 8))

(defun knight-tour-warnsdorff (board-size &optional (start-row 0) (start-col 0))
  "Solve knight's tour using Warnsdorff's heuristic without backtracking (faster but not guaranteed)"
  (let ((board (make-array (list board-size board-size) :initial-element 0))
        (move-count 1)
        (current-row start-row)
        (current-col start-col))
    
    ;; Mark starting position
    (setf (aref board current-row current-col) move-count)
    
    ;; Continue until no more moves possible
    (loop while (< move-count (* board-size board-size)) do
      (let ((moves (get-valid-moves board board-size current-row current-col)))
        (if moves
            (progn
              ;; Sort by Warnsdorff's rule
              (setf moves (sort moves (lambda (move1 move2)
                                        (< (count-valid-moves board board-size (first move1) (second move1))
                                           (count-valid-moves board board-size (first move2) (second move2))))))
              ;; Take the best move
              (let ((best-move (first moves)))
                (setf current-row (first best-move))
                (setf current-col (second best-move))
                (incf move-count)
                (setf (aref board current-row current-col) move-count)))
            (return))))
    
    (if (= move-count (* board-size board-size))
        (progn
          (format t "Knight's tour found using Warnsdorff's heuristic!~%")
          (print-board board board-size)
          board)
        (progn
          (format t "Partial tour found (~a/~a moves) using Warnsdorff's heuristic~%" 
                  move-count (* board-size board-size))
          (print-board board board-size)
          nil))))

; Usage examples:
; (knight-tour 5)                    ; Solve 5x5 board starting from (0,0)
; (knight-tour 6 2 3)                ; Solve 6x6 board starting from (2,3)
(knight-tour-warnsdorff 8)         ; Fast heuristic solution for 8x8 board
(test-knight-tour)                 ; Run tests with multiple board sizes