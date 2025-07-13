;;; Crossword Puzzle Implementation in Common Lisp

;; Data structure for crossword grid
(defstruct crossword
  (grid nil)     ; 2D array representing the puzzle grid
  (width 0)      ; grid width
  (height 0)     ; grid height
  (words nil)    ; list of words with their positions and clues
  (clues nil))   ; list of clues

;; Word structure: position, direction, length, answer, clue
(defstruct word-entry
  (number 0)     ; clue number
  (row 0)        ; starting row
  (col 0)        ; starting column
  (direction :across) ; :across or :down
  (length 0)     ; word length
  (answer "")    ; the actual word
  (clue ""))     ; clue text

;; Create a new crossword puzzle
(defun make-crossword-puzzle (width height)
  "Create a new crossword puzzle with given dimensions"
  (let ((grid (make-array (list height width) :initial-element #\Space)))
    (make-crossword :grid grid :width width :height height)))

;; Set a cell in the grid
(defun set-cell (crossword row col char)
  "Set a character at the specified position in the grid"
  (when (and (>= row 0) (< row (crossword-height crossword))
             (>= col 0) (< col (crossword-width crossword)))
    (setf (aref (crossword-grid crossword) row col) char)))

;; Get a cell from the grid
(defun get-cell (crossword row col)
  "Get the character at the specified position in the grid"
  (if (and (>= row 0) (< row (crossword-height crossword))
           (>= col 0) (< col (crossword-width crossword)))
      (aref (crossword-grid crossword) row col)
      #\#))

;; Check if a word can be placed at a given position
(defun can-place-word-p (crossword word row col direction)
  "Check if a word can be placed at the given position and direction"
  (let ((len (length word)))
    (cond
      ((eq direction :across)
       (and (<= (+ col len) (crossword-width crossword))
            (every (lambda (i)
                     (let ((cell (get-cell crossword row (+ col i))))
                       (or (char= cell #\Space)
                           (char= cell (char word i)))))
                   (loop for i from 0 below len collect i))))
      ((eq direction :down)
       (and (<= (+ row len) (crossword-height crossword))
            (every (lambda (i)
                     (let ((cell (get-cell crossword (+ row i) col)))
                       (or (char= cell #\Space)
                           (char= cell (char word i)))))
                   (loop for i from 0 below len collect i))))
      (t nil))))

;; Place a word in the crossword
(defun place-word (crossword word row col direction clue-num clue-text)
  "Place a word in the crossword at the specified position"
  (when (can-place-word-p crossword word row col direction)
    (let ((len (length word)))
      (cond
        ((eq direction :across)
         (loop for i from 0 below len do
           (set-cell crossword row (+ col i) (char word i))))
        ((eq direction :down)
         (loop for i from 0 below len do
           (set-cell crossword (+ row i) col (char word i)))))
      
      ;; Add word entry to the crossword
      (let ((word-entry (make-word-entry
                         :number clue-num
                         :row row
                         :col col
                         :direction direction
                         :length len
                         :answer word
                         :clue clue-text)))
        (push word-entry (crossword-words crossword)))
      t)))

;; Mark black squares
(defun mark-black-square (crossword row col)
  "Mark a cell as a black square"
  (set-cell crossword row col #\#))

;; Display the crossword grid
(defun display-crossword (crossword &optional (show-answers nil))
  "Display the crossword puzzle grid"
  (format t "~%")
  (loop for row from 0 below (crossword-height crossword) do
    (loop for col from 0 below (crossword-width crossword) do
      (let ((cell (get-cell crossword row col)))
        (cond
          ((char= cell #\#) (format t "██"))
          ((char= cell #\Space) (format t "  "))
          (show-answers (format t "~A " cell))
          (t (format t "□ ")))))
    (format t "~%")))

;; Display clues
(defun display-clues (crossword)
  "Display the clues for the crossword"
  (let ((across-clues '())
        (down-clues '()))
    
    ;; Separate across and down clues
    (dolist (word (crossword-words crossword))
      (if (eq (word-entry-direction word) :across)
          (push word across-clues)
          (push word down-clues)))
    
    ;; Sort by clue number
    (setf across-clues (sort across-clues #'< :key #'word-entry-number))
    (setf down-clues (sort down-clues #'< :key #'word-entry-number))
    
    ;; Display across clues
    (format t "~%ACROSS:~%")
    (dolist (word across-clues)
      (format t "~A. ~A~%" 
              (word-entry-number word)
              (word-entry-clue word)))
    
    ;; Display down clues
    (format t "~%DOWN:~%")
    (dolist (word down-clues)
      (format t "~A. ~A~%" 
              (word-entry-number word)
              (word-entry-clue word)))))

;; Check if the puzzle is solved
(defun check-solution (crossword user-grid)
  "Check if the user's solution matches the crossword answer"
  (let ((correct t))
    (loop for row from 0 below (crossword-height crossword) do
      (loop for col from 0 below (crossword-width crossword) do
        (let ((correct-cell (get-cell crossword row col))
              (user-cell (aref user-grid row col)))
          (when (and (not (char= correct-cell #\Space))
                     (not (char= correct-cell #\#))
                     (not (char= (char-upcase correct-cell) 
                                 (char-upcase user-cell))))
            (setf correct nil)))))
    correct))

;; Example usage and test puzzle
(defun create-sample-puzzle ()
  "Create a sample crossword puzzle for testing"
  (let ((puzzle (make-crossword-puzzle 10 10)))
    
    ;; Add some black squares for structure
    (mark-black-square puzzle 0 0)
    (mark-black-square puzzle 0 1)
    (mark-black-square puzzle 1 0)
    (mark-black-square puzzle 8 8)
    (mark-black-square puzzle 8 9)
    (mark-black-square puzzle 9 8)
    (mark-black-square puzzle 9 9)
    
    ;; Place some words
    (place-word puzzle "HELLO" 0 2 :across 1 "Greeting")
    (place-word puzzle "WORLD" 2 0 :across 2 "Earth")
    (place-word puzzle "LISP" 4 3 :across 3 "Programming language")
    (place-word puzzle "CODE" 6 1 :across 4 "Program instructions")
    
    ;; Down words
    (place-word puzzle "HELP" 0 2 :down 1 "Assistance")
    (place-word puzzle "LOOP" 0 5 :down 5 "Iteration construct")
    (place-word puzzle "LIST" 2 4 :down 6 "Data structure")
    
    puzzle))

;; Test the crossword implementation
(defun test-crossword ()
  "Test the crossword puzzle implementation"
  (let ((puzzle (create-sample-puzzle)))
    (format t "Sample Crossword Puzzle:~%")
    (display-crossword puzzle)
    (display-clues puzzle)
    
    (format t "~%~%With answers:~%")
    (display-crossword puzzle t)))

;; Interactive solver helper
(defun solve-interactively (crossword)
  "Interactive crossword solver"
  (let ((user-grid (make-array (list (crossword-height crossword)
                                     (crossword-width crossword))
                               :initial-element #\Space)))
    
    ;; Copy black squares to user grid
    (loop for row from 0 below (crossword-height crossword) do
      (loop for col from 0 below (crossword-width crossword) do
        (when (char= (get-cell crossword row col) #\#)
          (setf (aref user-grid row col) #\#))))
    
    (loop
      (format t "~%Current puzzle state:~%")
      (display-crossword crossword)
      (display-clues crossword)
      
      (format t "~%Enter word number and answer (or 'quit' to exit): ")
      (let ((input (read-line)))
        (if (string= input "quit")
            (return)
            (let ((parts (loop for i = 0 then (1+ j)
                              as j = (position #\Space input :start i)
                              collect (subseq input i j)
                              while j)))
              (when (>= (length parts) 2)
                (let ((word-num (parse-integer (first parts) :junk-allowed t))
                      (answer (string-upcase (second parts))))
                  (when word-num
                    (let ((word-entry (find word-num (crossword-words crossword)
                                           :key #'word-entry-number)))
                      (when word-entry
                        (if (= (length answer) (word-entry-length word-entry))
                            (progn
                              ;; Place the word in user grid
                              (let ((row (word-entry-row word-entry))
                                    (col (word-entry-col word-entry))
                                    (direction (word-entry-direction word-entry)))
                                (loop for i from 0 below (length answer) do
                                  (if (eq direction :across)
                                      (setf (aref user-grid row (+ col i)) (char answer i))
                                      (setf (aref user-grid (+ row i) col) (char answer i)))))
                              (format t "Word placed!~%"))
                            (format t "Wrong length! Expected ~A letters.~%" 
                                    (word-entry-length word-entry))))))))))
      
      ;; Check if solved
      (when (check-solution crossword user-grid)
        (format t "~%Congratulations! You solved the puzzle!~%")
        (return)))))

;; Run the test
(test-crossword)