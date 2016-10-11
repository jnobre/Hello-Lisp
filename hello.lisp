
(defun mmax(x y)
	(if (> x y)
		x
		y))


(defun max3(x y z)
	(if (> x y)
	 (if (> x z)
	   x
	   z)
	(if (> y z)
	   y
	   z)))


(defun max3(x y z)
  (cond ((and (>= x y) (>= x z)) x)
        ((and (>= y x) (>= y z)) y)
        (T z)))



(defun soma(x y)
   (if (zerop y)
       x
     (soma (1+ x) (1- y))))