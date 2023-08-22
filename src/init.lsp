(setq version 4)

(setq linewidth 31)

(setq defun '(lambda x (set (car x) (cons (quote lambda) (cdr x))) (car x)))

(defun charcount (x left)
  (cond
    ((atom x) (cond ((greaterp left (chars x)) (difference left (chars x)))))
    ((loop
      (until (atom x) (charcount x (difference left (cond (x 4) (t -2)))))
      (while (setq left (charcount (car x) (sub1 left))))
      (setq x (cdr x))))))

(defun edit l
  (sprint (eval (car l)))
  (printc)
  (set (car l) (sed (eval (car l)))))

(defun sprint (x (n . 0))
  (cond
    ((or (atom x) (charcount x (difference linewidth n))) (princ x))
    ((princ lpar)
     (sprint (car x) n)
     (setq n (plus n 3))
     (loop
      (setq x (cdr x))
      (cond ((and x (atom x)) (princ period x)))
      (until (atom x) (princ rpar))
      (xtab n)
      (sprint (car x) n)))))

(defun sed (a (q))
  (loop
   (setq q (princ (getchar)))
   (until (eq q (quote b)) a)
   (setq a (cond
	     ((eq q (quote r)) (printc) (read))
	     ((eq q cr) (sprint a) (printc) a)
	     ((eq q (quote c)) (printc) (cons (read) a))
	     ((atom a) (princ (quote *)) a)
	     ((eq q (quote d)) (cons (car a) (sed (cdr a))))
	     ((eq q (quote a)) (cons (sed (car a)) (cdr a)))
	     ((eq q (quote x)) (cdr a))
	     (t (princ '?) a)))))

(defun xtab (s)
  (printc)
  (loop
   (until (minusp (setq s (difference s 1))))
   (princ blank)))

(save 'image)

(* 'exit)
