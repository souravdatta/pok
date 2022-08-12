(defclass pok-obj () ())

(defclass pok-scalar (pok-obj)
  ((val :initarg :val
	:initform nil
	:accessor pok-scalar-val)))

(defclass pok-list (pok-obj)
  ((val :initarg :val
	:initform '()
	:accessor pok-list-elems)))

(defclass pok-lambda (pok-obj)
  ((body :initarg :defn
	 :initform nil
	 :accessor pok-fn)))

(defun make-pok-list (&rest elems)
  (make-instance 'pok-list
		 :val elems))

(defun make-pok-scalar (x)
  (make-instance 'pok-scalar
		 :val x))

(defun make-pok-lambda (x)
  (if (pok-lambda-p x)
      (make-instance 'pok-lambda :defn (cddr x))
      nil))

(defun make-pok-obj (x)
  (cond
    ((numberp x) (make-pok-scalar x))
    ((listp x) (apply #'make-pok-list
		      (mapcar #'make-pok-obj x)))
    (t nil)))

(defmethod make-cl-obj ((x pok-obj))
  (show x))

(defmethod make-cl-obj ((x pok-scalar))
  (pok-scalar-val x))

(defmethod make-cl-obj ((x pok-list))
  (mapcar #'make-cl-obj (pok-list-elems x)))

(defmethod show (obj)
  (format nil "~A" obj))

(defmethod show ((obj pok-obj))
  (format nil "pok_obj"))

(defmethod show ((obj pok-scalar))
  (format nil "~A"
	  (show (pok-scalar-val obj))))

(defmethod show ((obj pok-list))
  (format nil "~A"
	  (mapcar #'show
		  (pok-list-elems obj))))

(defmethod show ((obj pok-lambda))
  (format nil "(fn _ ~A)" (pok-fn obj)))

(defmethod pok-count ((p pok-obj))
  0)

(defmethod pok-count ((p pok-list))
  (length (pok-list-elems p)))

(defmethod pok-append-lists ((p pok-list)
			     (q pok-list))
  (apply #'make-pok-list (append
			  (pok-list-elems p)
			  (pok-list-elems q))))

(defmethod pok-repeat ((p pok-obj)
		       n)
  (apply #'make-pok-list
	 (loop for i from 1 to n collect p)))

(defmethod pok-sublist ((p pok-obj)
			start
			&optional (end nil))
  (apply #'make-pok-list
	 (subseq (pok-list-elems p)
		 start
		 end)))

(defmethod pok-at ((p pok-list)
		   n)
  (if (>= n 0)
      (nth n (pok-list-elems p))
      (nth (+ (pok-count p) n) (pok-list-elems p))))

(defmethod pok-adjust-obj ((p pok-list)
			   zero
			   rotate
			   repeat
			   fill
			   lenmax)
  (let ((n (- lenmax (pok-count p))))
    (cond
      ((not (null zero)) (pok-append-lists
			  p
			  (pok-repeat (make-pok-scalar 0) n)))
      (rotate (if (< n (pok-count p))
		  (pok-append-lists p (pok-sublist p 0 n))
		  (pok-adjust-obj
		   (pok-append-lists p p)
		   nil
		   t
		   nil
		   nil
		   lenmax)))
      (repeat (pok-append-lists
	       p
	       (pok-repeat (pok-at p -1) n)))
      (fill (pok-append-lists
	     p
	     (pok-repeat fill n)))
      (t p))))		   

(defmethod pok-match-lists ((p pok-list)
			    (q pok-list)
			    &key (zero t) (rotate nil) (repeat nil) (fill nil))
  (let ((l1 (pok-count p))
	(l2 (pok-count q)))
    (cond
      ((= l1 l2) (list p q))
      ((> l1 l2) (list p (pok-adjust-obj q zero rotate repeat fill l1)))
      (t (list (pok-adjust-obj p
			       zero rotate repeat fill l2)
	       q)))))

(defun pok-matched-fn (fn &key (zero t) (rotate nil) (repeat nil) (fill nil))
  #'(lambda (p q) (let ((matched-lists (pok-match-lists p q
							:zero zero
							:rotate rotate
							:repeat repeat
							:fill fill)))
		    (apply fn matched-lists))))

(defmethod pok-plus ((p pok-list)
		     (q pok-list))
  (let ((args (pok-match-lists p q)))
    (apply #'make-pok-list
	   (loop for x in (pok-list-elems (first args))
		 for y in (pok-list-elems (second args))
		 collect (make-pok-obj
			  (+ (pok-scalar-val x)
			     (pok-scalar-val y)))))))

(defmethod pok-plus ((p pok-scalar)
		     (q pok-list))
  (pok-plus (pok-repeat p (pok-count q))
	    q))

(defmethod pok-plus ((p pok-list)
		     (q pok-scalar))
  (pok-plus p
	    (pok-repeat q (pok-count p))))

(defmethod pok-plus ((p pok-scalar)
		     (q pok-scalar))
  (make-pok-scalar
   (+
    (pok-scalar-val p)
    (pok-scalar-val q))))

(defmethod pok-minus ((p pok-list)
		      (q pok-list))
  (let ((args (pok-match-lists p q)))
    (apply #'make-pok-list
	   (loop for x in (pok-list-elems (first args))
		 for y in (pok-list-elems (second args))
		 collect (make-pok-obj
			  (- (pok-scalar-val x)
			     (pok-scalar-val y)))))))

(defmethod pok-minus ((p pok-scalar)
		      (q pok-list))
  (pok-minus (pok-repeat p (pok-count q))
	     q))

(defmethod pok-minus ((p pok-list)
		     (q pok-scalar))
  (pok-minus p
	     (pok-repeat q (pok-count p))))

(defmethod pok-minus ((p pok-scalar)
		      (q pok-scalar))
  (make-pok-scalar
   (-
    (pok-scalar-val p)
    (pok-scalar-val q))))

(defmethod pok-mult ((p pok-list)
		     (q pok-list))
  (let ((args (pok-match-lists p q)))
    (apply #'make-pok-list
	   (loop for x in (pok-list-elems (first args))
		 for y in (pok-list-elems (second args))
		 collect (make-pok-obj
			  (* (pok-scalar-val x)
			     (pok-scalar-val y)))))))

(defmethod pok-mult ((p pok-scalar)
		     (q pok-list))
  (pok-mult (pok-repeat p (pok-count q))
	    q))

(defmethod pok-mult ((p pok-list)
		     (q pok-scalar))
  (pok-mult p
	    (pok-repeat q (pok-count p))))

(defmethod pok-mult ((p pok-scalar)
		     (q pok-scalar))
  (make-pok-scalar
   (*
    (pok-scalar-val p)
    (pok-scalar-val q))))

(defmethod pok-div ((p pok-list)
		    (q pok-list))
  (let ((args (pok-match-lists p q)))
    (apply #'make-pok-list
	   (loop for x in (pok-list-elems (first args))
		 for y in (pok-list-elems (second args))
		 collect (make-pok-obj
			  (/ (pok-scalar-val x)
			     (pok-scalar-val y)))))))

(defmethod pok-div ((p pok-scalar)
		     (q pok-list))
  (pok-div (pok-repeat p (pok-count q))
	   q))

(defmethod pok-div ((p pok-list)
		    (q pok-scalar))
  (pok-div p
	   (pok-repeat q (pok-count p))))

(defmethod pok-div ((p pok-scalar)
		    (q pok-scalar))
  (make-pok-scalar
   (/
    (pok-scalar-val p)
    (pok-scalar-val q))))

(defmethod pok-dup ((o pok-obj))
  (list o o))

(defmethod pok-rotate ((top-2 pok-obj)
		       (top-1 pok-obj)
		       (top pok-obj))
  (list top-1 top-2 top))

(defmethod pok-swap ((top-1 pok-obj)
		     (top pok-obj))
  (list top-1 top))

(defmethod pok-eq ((p pok-scalar)
		    (q pok-list))
  (make-pok-obj
   (mapcar #'(lambda (x)
	       (if (equal (pok-scalar-val p)
			  (pok-scalar-val x))
		   1
		   0))
	   (pok-list-elems q))))

(defmethod pok-eq ((p pok-list)
		    (q pok-scalar))
  (make-pok-obj
   (mapcar #'(lambda (x)
	       (if (equal (pok-scalar-val q)
			  (pok-scalar-val x))
		   1
		   0))
	   (pok-list-elems p))))

(defmethod pok-eq ((p pok-scalar)
		   (q pok-scalar))
  (make-pok-obj
   (if (equal (pok-scalar-val p)
	      (pok-scalar-val q))
       1
       0)))

(defmethod pok-eq ((p pok-list)
		   (q pok-list))
  (if (= (pok-count p) (pok-count q))
      (make-pok-obj
       (mapcar #'(lambda (x y)
		   (if (equal (pok-scalar-val x)
			      (pok-scalar-val y))
		       1
		       0))
	       (pok-list-elems p)
	       (pok-list-elems q)))
      (error "list length mismatched")))

(defun p-and (x y)
  (if
   (and (if (= x 0) nil t)
	(if (= y 0) nil t))
   1
   0))

(defun p-or (x y)
  (if
   (or (if (= x 0) nil t)
       (if (= y 0) nil t))
   1
   0))

(defmethod pok-and ((p pok-scalar)
		    (q pok-list))
  (make-pok-obj
   (mapcar #'(lambda (x)
	       (p-and (pok-scalar-val p)
		      (pok-scalar-val x)))
	   (pok-list-elems q))))

(defmethod pok-and ((p pok-list)
		    (q pok-scalar))
  (make-pok-obj
   (mapcar #'(lambda (x)
	       (p-and (pok-scalar-val q)
		      (pok-scalar-val x)))
	   (pok-list-elems p))))

(defmethod pok-and ((p pok-scalar)
		    (q pok-scalar))
  (make-pok-obj
   (p-and (pok-scalar-val p)
	  (pok-scalar-val q))))

(defmethod pok-and ((p pok-list)
		    (q pok-list))
  (if (= (pok-count p) (pok-count q))
      (make-pok-obj
       (mapcar #'(lambda (x y)
		   (p-and (pok-scalar-val x)
			  (pok-scalar-val y)))
	       (pok-list-elems p)
	       (pok-list-elems q)))
      (error "list length mismatched")))

(defmethod pok-or ((p pok-scalar)
		    (q pok-list))
  (make-pok-obj
   (mapcar #'(lambda (x)
	       (p-or (pok-scalar-val p)
		     (pok-scalar-val x)))
	   (pok-list-elems q))))

(defmethod pok-or ((p pok-list)
		   (q pok-scalar))
  (make-pok-obj
   (mapcar #'(lambda (x)
	       (p-or (pok-scalar-val q)
		     (pok-scalar-val x)))
	   (pok-list-elems p))))

(defmethod pok-or ((p pok-scalar)
		   (q pok-scalar))
  (make-pok-obj
   (p-or (pok-scalar-val p)
	 (pok-scalar-val q))))

(defmethod pok-or ((p pok-list)
		   (q pok-list))
  (if (= (pok-count p) (pok-count q))
      (make-pok-obj
       (mapcar #'(lambda (x y)
		   (p-or (pok-scalar-val x)
			 (pok-scalar-val y)))
	       (pok-list-elems p)
	       (pok-list-elems q)))
      (error "list length mismatched")))

(defmethod pok-not ((p pok-scalar))
  (make-pok-obj
   (if (= (pok-scalar-val p) 0)
       0
       1)))

(defmethod pok-not ((p pok-list))
  (make-pok-list
   (mapcar #'pok-not (pok-list-elems p))))

(defmethod pok-over ((lst pok-list) sym)
  (list 'do
	(append (make-cl-obj lst)
		(loop for i from 1 to (- (pok-count lst) 1)
		      collect sym))))

(defun pok-enlist (&rest args)
  (apply #'make-pok-list args))

(defmethod pok-pair ((q pok-obj)
		     (p pok-obj))
  (make-pok-list q p))

(defmethod pok-pop ((x pok-obj))
  nil)

(defmethod pok-scan ((lst pok-list)
		     sym)
  (if (= (pok-count lst) 0)
      (make-pok-list)
      (let ((combs (loop for i from 1 to (pok-count lst)
			 collect (subseq (pok-list-elems lst)
					 0
					 i))))
	(list 'do-local
	      (append
	       (apply
		#'append
		(loop for c in combs
		      collect (list
			       (mapcar #'make-cl-obj
				       c)
			       (pok-quote sym)
			       'over)))
	       (list 'list))))))

(defmethod pok-map ((lst pok-list)
		    (fn pok-lambda))
  (list 'do-local
	(append
	 (apply
	  #'append
	  (mapcar #'(lambda (x)
		      (cons x (pok-fn fn)))
		  (loop for x in (pok-list-elems lst)
			collect (make-cl-obj x))))
	 (list 'list))))

(defmethod pok-map ((lst pok-list)
		    sym)
  (list 'do-local
	(append
	 (apply
	  #'append
	  (mapcar #'(lambda (x)
		      (list x sym))
		  (loop for x in (pok-list-elems lst)
			collect (make-cl-obj x))))
	 (list 'list))))

(defmethod pok-if ((cnd pok-scalar)
		   (fn pok-lambda))
  (if (= (pok-scalar-val cnd) 1)
      (list 'do
	    (pok-fn fn))
      nil))

(defmethod pok-if ((cnd pok-list)
		   (fn pok-lambda))
  (if (= (reduce #'+ (make-cl-obj cnd)) (pok-count cnd))
      (list 'do
	    (pok-fn fn))
      nil))

(defmethod pok-until ((cnd pok-scalar)
		      (fn pok-lambda))
  (if (= (pok-scalar-val cnd) 0)
      (list 'do
	    (pok-fn fn))
      nil))

(defmethod pok-until ((cnd pok-list)
		      (fn pok-lambda))
  (if (= (reduce #'+ (make-cl-obj cnd)) 0)
      (list 'do
	    (pok-fn fn))
      nil))
	    
(defmethod pok-if-else ((cnd pok-scalar)
			(fn1 pok-lambda)
			(fn2 pok-lambda))
  (if (= (pok-scalar-val cnd) 1)
      (list 'do
	    (pok-fn fn1))
      (list 'do
	    (pok-fn fn2))))

(defmethod pok-if-else ((cnd pok-list)
			(fn1 pok-lambda)
			(fn2 pok-lambda))
  (if (= (reduce #'+ (make-cl-obj cnd)) (pok-count cnd))
      (list 'do
	    (pok-fn fn1))
      (list 'do
	    (pok-fn fn2))))

(defmethod pok-min ((p pok-list)
		    (q pok-list))
  (make-pok-obj
   (mapcar #'min
	   (make-cl-obj q)
	   (make-cl-obj p))))

(defmethod pok-min ((p pok-scalar)
		    (q pok-list))
  (pok-min (pok-repeat p (pok-count q))
	   q))

(defmethod pok-min ((p pok-list)
		    (q pok-scalar))
  (pok-min p
	   (pok-repeat q (pok-count p))))

(defmethod pok-min ((p pok-scalar)
		    (q pok-scalar))
  (make-pok-obj (min (pok-scalar-val p)
		     (pok-scalar-val q))))

(defmethod pok-max ((p pok-list)
		    (q pok-list))
  (make-pok-obj
   (mapcar #'max
	   (make-cl-obj q)
	   (make-cl-obj p))))

(defmethod pok-max ((p pok-scalar)
		    (q pok-list))
  (pok-max (pok-repeat p (pok-count q))
	   q))

(defmethod pok-max ((p pok-list)
		    (q pok-scalar))
  (pok-max p
	   (pok-repeat q (pok-count p))))

(defmethod pok-max ((p pok-scalar)
		    (q pok-scalar))
  (make-pok-obj (max (pok-scalar-val p)
		     (pok-scalar-val q))))

(defmethod pok-elem ((p pok-list)
		     (q pok-scalar))
  (nth (pok-scalar-val q)
       (pok-list-elems p)))

(defmethod pok-elem ((p pok-list)
		     (q pok-list))
  (apply #'make-pok-list
	 (mapcar #'(lambda (i)
		     (pok-elem p i))
		 (pok-list-elems q))))

(defmethod pok-iota ((p pok-scalar))
  (make-pok-obj (loop for i from 0 to (-
				       (pok-scalar-val p)
				       1)
		      collect i)))

(defmethod pok-gather ((p pok-list)
		       (tk pok-scalar)
		       (drp pok-scalar))
  (make-pok-obj
   (scans (pok-scalar-val tk)
	  (pok-scalar-val drp)
	  (make-cl-obj p))))

(defmethod pok-deltas ((p pok-list))
  (let ((lst (make-cl-obj p)))
    (if (not (null p))
	(make-pok-obj
	 (cons
	  (first lst)
	  (mapcar #'(lambda (p) (* -1 (apply #'- p)))
		  (remove-if #'(lambda (x) (< (length x) 2))
			     (scans 2 1 lst)))))
	p)))

;; evaluating

(defun pok-load-file (file stack env)
  (dolist (expr (with-open-file (stream file)
		  (loop for line = (read-line stream nil)
			while line
			collect (with-input-from-string
				    (s (format nil "(~A)" line))
				  (read s)))))
    (let ((result (pok-eval expr stack env)))
      (setf stack (first result))
      (setf env (second result))))
  (list stack env))

(defun pok-scalar-p (x)
  (numberp x))

(defun pok-list-p (x)
  (listp x))

(defun pok-fn-p (x)
  (symbolp x))

(defun pok-fndef-p (x)
  (and (listp x)
       (eq (first x) 'fn)
       (symbolp (second x))))

(defun pok-quoted-p (x)
  (and
   (symbolp x)
   (equal (char (string x) 0) #\?)))

(defun pok-unquote (x)
  (multiple-value-bind (sym x) (intern (subseq (string x) 1))
    sym))

(defun pok-quote (x)
  (intern
   (concatenate 'string "?" (string x))))

(defun drop (n lst)
  (let ((len (length lst)))
    (cond
      ((= len n) '())
      ((> len n) (subseq lst n))
      (t lst))))

(defun take (n lst)
  (subseq lst 0 n))

(defun scans (m n lst &optional (result nil))
  (cond
    ((null lst) (reverse result))
    ((or
      (< (length lst) n)
      (< (length lst) m))
     (scans m n nil (cons lst result)))
    (t (scans m n (drop n lst) (cons (take m lst) result)))))

(defun pok-do-p (x)
  (and
   (listp x)
   (eq (first x) 'do)
   (listp (second x))))

(defun pok-do-local-p (x)
  (and
   (listp x)
   (eq (first x) 'do-local)
   (listp (second x))))

(defun pok-lambda-p (x)
  (and
   (listp x)
   (eq (first x) 'fn)
   (eq (second x) '_)))

(defun pok-apply-n (n fn &optional (fullstack nil))
  #'(lambda (stack env)
      (if (and
	   (not (null stack))
	   (>= (length stack) n))
	  (let ((newstack
		  (if fullstack nil (drop n stack)))
		(result
		   (apply fn
			  (reverse
			   (subseq stack
				   0
				   (if fullstack
				      (length stack)
				      n))))))
	    (cond
	      ((pok-do-p result)
	       (pok-eval (second result)
			 newstack
			 env))
	      ((pok-do-local-p result)
	       (let ((result
		       (pok-eval (second result)
				 nil
				 env)))
		 (list
		  (append (first result) newstack)
		  (second result))))
	      (t (list
		  (append
		   (if (listp result) result (list result))
		   newstack)
		  env))))
	  (error "not enough args for this operation"))))

(defun pok-apply-fn (fn)
  #'(lambda (stack env)
      (pok-eval (cddr fn) stack env)))

(defun pok-user-defn-p (fn env)
  (let ((lookup (assoc fn env)))
    lookup))

(defun pok-fn-lookup (fn &optional (env nil))
  (cond
    ((eq fn '+) (pok-apply-n 2 #'pok-plus))
    ((eq fn '-) (pok-apply-n 2 #'pok-minus))
    ((eq fn '*) (pok-apply-n 2  #'pok-mult))
    ((eq fn '/) (pok-apply-n 2 #'pok-div))
    ((eq fn 'dup) (pok-apply-n 1 #'pok-dup))
    ((eq fn 'rot) (pok-apply-n 3 #'pok-rotate))
    ((eq fn 'swap) (pok-apply-n 2 #'pok-swap))
    ((eq fn 'and) (pok-apply-n 2 #'pok-and))
    ((eq fn 'or) (pok-apply-n 2 #'pok-or))
    ((eq fn '=) (pok-apply-n 2 #'pok-eq))
    ((eq fn 'over) (pok-apply-n 2 #'pok-over))
    ((eq fn 'pop) (pok-apply-n 1 #'pok-pop))
    ((eq fn 'list) (pok-apply-n 0 #'pok-enlist t))
    ((eq fn 'pair) (pok-apply-n 2 #'pok-pair))
    ((eq fn 'scan) (pok-apply-n 2 #'pok-scan))
    ((eq fn 'map) (pok-apply-n 2 #'pok-map))
    ((eq fn 'if) (pok-apply-n 2 #'pok-if))
    ((eq fn 'until) (pok-apply-n 2 #'pok-until))
    ((eq fn 'ifelse) (pok-apply-n 3 #'pok-if-else))
    ((eq fn 'min) (pok-apply-n 2 #'pok-min))
    ((eq fn 'max) (pok-apply-n 2 #'pok-max))
    ((eq fn 'elem) (pok-apply-n 2 #'pok-elem))
    ((eq fn 'iota) (pok-apply-n 1 #'pok-iota))
    ((eq fn 'repeat) (pok-apply-n 2 #'pok-repeat))
    ((eq fn 'at) (pok-apply-n 2 #'pok-at))
    ((eq fn 'append) (pok-apply-n 2 #'pok-append-lists))
    ((eq fn 'sublist) (pok-apply-n 3 #'pok-sublist))
    ((eq fn 'gather) (pok-apply-n 3 #'pok-gather))
    ((eq fn 'deltas) (pok-apply-n 1 #'pok-deltas))
    ((pok-user-defn-p fn env)
     (pok-apply-fn (cdr (assoc fn env))))
    (t (error "no def found for this op"))))
  
(defun pok-eval-stack (stack op env)
  (let ((fn (pok-fn-lookup op env)))
    (funcall fn stack env)))

(defun pok-add-fn (env fndef)
  (cons (cons (second fndef)
	      fndef)
	env))

(defun pok-eval (lst &optional (stack nil) (env nil))
  ;;(format t "eval: ~A [~A , ~A] ~%" lst (mapcar #'show stack) env)
  (if (null lst)
      (list stack env)
      (let ((top (first lst)))
	(cond
	  ((pok-lambda-p top) (pok-eval
			       (rest lst)
			       (cons (make-pok-lambda top) stack)
			       env))
	  ((pok-quoted-p top) (pok-eval
			       (rest lst)
			       (cons (pok-unquote top) stack)
			       env))
	  ((pok-fndef-p top) (pok-eval
			      (rest lst)
			      stack
			      (pok-add-fn env top)))
	  ((or
	    (pok-list-p top)
	    (pok-scalar-p top)) (pok-eval
				 (rest lst)
				 (cons (make-pok-obj top)
				       stack)
				 env))
	  ((pok-fn-p top)
	   (let ((result (pok-eval-stack stack top env)))
	     (pok-eval (rest lst) (first result) (second result))))
	  (t (error (format t "did not understand ~A~%" top)))))))

(defun pok-repl ()
  (format t "pok repl~%")
  (let ((data (pok-load-file "core.pok" nil nil)))
    (loop (handler-case
	      (progn
		(format t "    ")
		(let* ((input (read-line))
		       (line (with-input-from-string
				 (s (format nil "(~A)" input))
			       (read s))))
		  (setf data (pok-eval
			      line
			(first data)
			(second data)))
		  (format t
			  "~A~%"
			  (mapcar #'show
				  (reverse (first data))))))
	    (error (c)
	      (format t "error ~A~%" c))))))
