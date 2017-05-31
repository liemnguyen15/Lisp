;;;Part 2 - Assertions and Simple Pattern-Matching

;;;As I build this pattern-matching function, I will first build a set of routines
;;;that will allow us to represent facts, called assertions.

;;;For instance, we can define the following assertions:

;;;(this is an assertion)
;;;(color apple red)
;;;(supports table block1)

;;;Here each assertion is represented as a list. The set of assertions can be maintained
;;;in a database by representing them in a list 'l'. 

;;;For instance, the following list represents an assertion database containing the above assertions:

;;;((this is an assertion) (color apple red) (supports table block1))

;;;Patterns are like assertions, except that they may contain certain special atoms not
;;;allowed in assertions, the single characters ? and !, for instance.

;;;(this ! assertion)
;;;(color ? red)

;;;This is function 'match' that compares a pattern and an assertion. When a pattern
;;;containing no special atoms is compared to an assertion, the two match only if they are
;;;exactly the same, with each corresponding position occupied by the same atom.

(defun exact-match (p a)
  (if (and (atom p) (atom a)) ;;recursion base case
      (eq p a)
      (if (and (listp p) (listp a) (not (null p)) (not (null a)))
          (and (exact-match (car p) (car a)) (exact-match (cdr p) (cdr a)))
          ()
          )
      )
  )

;; a helper function for 'long-any-match', which returns true if the
;; assertion contains any '!'
(defun have-long-match (l)
  (if (null l)
      l
      (or (eq (car l) '!) (have-long-match (cdr l)))
      )
  )

;; a helper function for 'long-any-match', which is a negation wrapper for have-long-match
(defun have-no-long-match (l)
  (not (have-long-match l))
  )

;; a helper function for 'long-any-match', which replaces the first atom by n '?'s
(defun create-any-long (n)
  (if (zerop n)
      ()
      (cons '? (create-any-long (- n 1)))
      )
  )

;; a helper function for 'long-any-match', which returns a list of some assertions
;; with '!' replaced by one to n '?'s
(defun expand-many-long-list (p n)
  (do ((i n (- i 1))
      (res () (append res (cons (append (create-any-long i) (cdr p)) ()))))
      ((or (minusp i) (zerop i)) res))
  )

;; a helper function for 'long-any-match', a wrapper for 'expand-many-long-list', which
;; returns a cons list of the assertion given if first atom isn't '!'
(defun expand-if-have-long (p n)
  (if (eq '! (car p))
      (expand-many-long-list p n)
      (cons p ())
      )
  )

(defun long-any-match (p a)
  (if (and (have-no-long-match p) (have-no-long-match a))
      (any-match p a)
      (let ((lp (expand-if-have-long p (length a)))
            (la (expand-if-have-long a (length p))))

        ;; In O(n^2) to check and see if they match, and perform depth first search (DFS)
        ;; on the tree only if first atom matches or matches '?'
        (do ((res () (or res (do ((res () (or (if (any-match (car (car pla)) (car (car plp)))
                                                  (long-any-match (cdr (car pla)) (cdr (car plp)))
                                                  ()
                                                  )))
                                 (plp lp (cdr plp)))
                                 ((or res (null plp)) res)
                              )
                     ))
            (pla la (cdr pla)))
            ((null pla) res)
          )
        )
      )
  )

(defun any-match (p a)
  (if (and (atom p) (atom a))
      (or (eq p a) (eq p '?) (eq a '?))
      (if (and (listp p) (listp a) (not (null p)) (not (null a)))
          (and (any-match (car p) (car a)) (any-match (cdr p) (cdr a)))
          ()
          )
      )
  )

(defun match (p a)
  (or (exact-match p a) (any-match p a) (long-any-match p a))
  )