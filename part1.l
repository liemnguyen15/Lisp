;;;Part 1 - Simple Functions

;;;This is function "cycle", which takes a list 'l' and an integer 'n' as input and returns the
;;;same list, but with the first element cycled to the end of the list 'n' times
(defun cycle (n l)
  (if (null l)
      l
      (do ((i 0 (+ i 1)) ;; append first cons cell car to the end repeatedly
           (ll l (append (cdr ll) (cons (car ll) ()))))
          ((= i n) ll)
        )
      )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;This is function "split-list", which takes a list 'l' and an atom 'a' and splits the list using
;;;the atom as the delimiter. If the array doesn’t contain the delimiter 'a', then it returns
;;;the entire list 'l' as it is.

;; a helper function for split-list, which returns the FIRST part of splitable list 'l' BEFORE delimiter 'a'
(defun atomic-split (a l)
  (if (null (cdr l))
      l
      (if (eq (car l) a)
          ()
          (if (eq (car (cdr l)) a)
              (cons (car l) ())
              (cons (car l) (atomic-split a (cdr l)))
              )
          )
      )
  )

;; a helper function for split-list, which returns the LAST or REST of the list 'l' AFTER delimiter 'a'
(defun split-cdr (a l)
  (if (null (cdr l))
      l
      (if (eq (car l) a)
          (cdr l)
          (split-cdr a (cdr l))
          )
      )
  )

;; the split-list
(defun split-list (a l)
  
  ;; do atomic-split and repeat for any split-cdr, until there is nothing left to split
  (do ((res '() (if (null (atomic-split a ll)) res (append res (cons (atomic-split a ll) ()))))
      (ll l (split-cdr a ll)))
      ((eq ll (split-cdr a ll)) res)
      )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;This is function 'range', which takes a list 'l' of numbers (with at least one element) and
;;;returns a list of length 3 that consists of the smallest number, the mean (reduced to
;;;the simplest fraction) of all numbers, and the largest number.
(defun range (l)
  (if (null l)
      l
      (progn
        (setq min (car l))
        (setq max (car l))
        (setq sum 0)
        (do ((ptr l (cdr ptr)))
            ((null ptr))
          (if (< (car ptr) min) (setq min (car ptr)))
          (if (> (car ptr) max) (setq max (car ptr)))
          (setq sum (+ sum (car ptr)))
          )
        (cons min (cons (/ sum (length l)) (cons max ())))
        )
      )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;This is function 'pivot', which takes a list 'l' and a number 'n' and splits it into two lists,
;;;one containing all the numbers in list 'l' less than 'n' and the other containing all numbers
;;;in list 'l' greater than or equal to 'n'.

;;;Note: preserve the relative order of elements inside the list.
(defun search-count (a l)
  (if (null l)
      0
      ;; evaluate count for head of the list, and repeat it recursively for the rest
      (+ (search-count a (cdr l)) (if (eq (car l) a) 1 0))
      )
  )

(defun pivot (n l)
  (if (null l)
      (cons () (cons () ()))
      (progn
        (do ((ptr l (cdr ptr))
             (head () (if (< (car ptr) n) (append head (cons (car ptr) ())) head))
             (tail () (if (>= (car ptr) n) (append tail (cons (car ptr) ())) tail))
             )
            ((null ptr) (cons head (cons tail ())))
          )
        )
      )
  )

(defun select-n-pivot (l)
  (pivot (car (nthcdr (random (length l)) l)) l)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;This is function 'quicksort' that sorts a list 'l'. 

;;;Here is my quicksort algorithm:
;;;First pick an element and call it the pivot. The head of the list is an easy option for
;;;pivot. Partition the rest of the list into two sublists, one with all the elements less than
;;;the pivot and the other with all the elements not less than the pivot. Recursively sort
;;;the sublists. Combine the two sublists and the pivot into a final sorted list.
(defun quicksort (l)
  (if (or (null l) (null (cdr l)))
      l
      (let ((p (select-n-pivot l)))
        (append
         (quicksort (car p))
         (quicksort (car (cdr p)))
         )
        )
      )
  )