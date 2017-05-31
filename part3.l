;;; Part 3 - Matrix Operations

;;;Let's represent a matrix as a list of lists. For example, ((a b) (c d)) would
;;;represent a 2*2 matrix whose first row contains the elements a and b, and whose second
;;;row contains the elements a and d. You may assume that the matrices are well-formed and
;;;compatible.

;;;This is function 'matrix-add', which takes two matrices as input and outputs the sum
;;;of the two matrices.

;; a helper function for 'matrix-add', which adds two 1-row matrixes
(defun matrix-row-add (a b)
  (do ((res () (append res (cons (+ (car pa) (car pb)) ())))
       (pa a (cdr pa))
       (pb b (cdr pb)))
      ((or (null pa) (null pb)) res)
    )
  )

(defun matrix-add (a b)
  ;; repeatedly perform matrix-row-add for each row of the matrix
  (do ((rres () (append rres (cons (matrix-row-add (car maa) (car mab)) ())))
       (maa a (cdr maa))
       (mab b (cdr mab)))
      ((or (null maa) (null mab)) rres)
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;This is function 'matrix-multiply', which takes two matrices as input and multiplies
;;;them and outputs the resultant.

;; a helper function for 'matrix-transpose' & 'matrix-multiply', which gets
;; first column of the matrix and return it as a row
(defun matrix-get-col (a)
  (do ((ptr a (cdr ptr))
       (res () (append res (cons (car (car ptr)) ()))))
      ((null ptr) res)
    )
  )

;; a helper function for 'matrix-transpose' & 'matrix-multiply", which removes
;; the first column and returns the matrix
(defun matrix-strip-col (a)
  (if (or (null a) (null (car a)))
      ()
      (do ((ptr a (cdr ptr))
           (res () (append res (cons (cdr (car ptr)) ()))))
          ((null ptr) res)
        )
      )
  )

(defun matrix-transpose (a)
  (if (or (null a) (null (car a)))
      ()
      ;; repeatedly get the first column and convert it into a row, then strip the column
      (do ((ptr a (matrix-strip-col ptr))
           (res () (append res (cons (matrix-get-col ptr) ()))))
          ((null (matrix-strip-col ptr)) res)
        )
      )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;This is function 'matrix-transpose', which takes a matrix as input, and outputs its transpose.

;; a helper function for 'matrix-multiply', which takes a row and a column, 
;; and returns an element in a row of the product
(defun matrix-multiply-col (r c)
  (do ((pr r (cdr pr))
       (pc c (cdr pc))
       (sum 0 (+ sum (* (car pr) (car pc))))
       )
      ((or (null pr) (null pc)) (cons sum ()))
    )
  )

;; a helper function for 'matrix-multiply', which takes a row and a column,
;; and returns a row of the product
(defun matrix-multiply-row (r b)
  (do ((pb b (matrix-strip-col pb))
       (rres () (append rres (matrix-multiply-col r (matrix-get-col pb)))))
      ((or (null pb) (null (matrix-strip-col pb))) rres)
    )
  )

(defun matrix-multiply (a b)
  (if (or (null a) (null b))
      ()
      (do ((pa a (cdr pa))
           (res () (append res (cons (matrix-multiply-row (car pa) b) ()))))
          ((null pa) res)
        )
      )
  )