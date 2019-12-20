;;;*vars* es una variable global de tipo lista en
;;;donde se almacenaran los nombres y valores de
;;;variables instanciadas. Se guardan de la forma
;;;(nombre valor).
(defvar *vars* '())

(defun append1 (lst obj)
  (append lst (list obj)))

;;;check_variable/2: devuelve true si el nombre var
;;;corresponde a una variable previamente instanciada,
;;;es decir, que se encuentra en *vars*
(defun check_variable (var lst)
  (if (not (null lst))
      (if (eql var (caar lst)) t
          (check_variable var (cdr lst)))))

;;;is-constant/1: devuelve true si
;;;el termino term es un atomo
(defun is-constant (term)
  (if (and (not (is-variable term)) (not (is-application term)) (not (is-lambda term))) t nil))

;;;is-variable/1: devuelve true si
;;;el termino term es una variable
(defun is-variable (term)
  (if (check_variable term *vars*) t nil))

;;;is-application/1: devuelve true si
;;;el termino term es una aplicacion
(defun is-application (term)
  (if (listp term)
      (if (not (eql (car term) 'lambda))
          t nil)))

;;;is-lambda/1: devuelve true si el termino
;;;term es una lambda abstraccion
(defun is-lambda (term)
  (if (listp term)
      (if (eql (car term) 'lambda) t nil)))

;;;function-of/1: devuelve el nombre de
;;;la funcion en una aplicacion
(defun function-of (term)
  (car term))

;;;argument-of/1: devuelve los argumentos
;;;de una aplicacion
(defun argument-of (term)
  (cdr term))

;;;mk-aplication/2: ejecuta una
;;;aplicacion
(defmacro mk-application (func arg)
  `(eval (cons ,func ,arg)))

;;;return-value/2: devuelve el valor de una variable,
;;;la cual debio haber sido agregada en *vars* junto con
;;;su valor. La lista lst es *vars* y debe ser enviada
;;;como argumento al llamar a la funcion
(defun return-value (var lst)
  (if (not (null lst))
      (if (eql var (caar lst)) (car (last (car lst)))
          (return-value var (cdr lst)))))

;;;evaluate/1: evalua expresiones lambda, siempre y cuando
;;;sean constantes, variables, aplicaciones o abstracciones
;;;aplicadas. Utilizar sintaxis lisp protegiendo las expresiones
(defun evaluate (term)
  (cond ((is-constant term) term)
        ((is-variable term) (return-value term *vars*))
        ((is-application term) (progn (if (eql (function-of term) 'setq)
                                          (setq *vars* (append1 *vars* (list (first (argument-of term)) (car (last (argument-of term))))))
                                          (mk-application (function-of term) (mapcar 'evaluate (argument-of term))))))
        ((is-lambda term) term)))
