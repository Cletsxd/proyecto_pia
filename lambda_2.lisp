;;;Variable global que contiene
;;;a todos los operadores que el
;;;evaluador permite utilizar
(defvar *oper* '(+ - * / sin cos tan exp
                 log expt sqrt asin acos
                 atan car cdr))

;;;is-variable/1: devuelve true si
;;;el termino term es un atomo
(defun is-variable (term)
  (if (atom term) t nil))

;;;is-application/1: devuelve true si
;;;el termino term es una aplicacion
(defun is-application (term)
  (if (listp term)
      (if (and (not (eql (car term) 'lambda))
               (member (car term) *oper*))
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

;;;evaluate/1: es el evaluador general
;;;de calculo lambda. Puede evaluar cualquier
;;;variable, aplicacion o lambda abstraccion
;;;con los operadores asignados al inicio
(defun evaluate (term)
  (if (is-variable term) term
      (if (is-application term)
          (mk-application (function-of term) (evaluate (argument-of term)))
          (if (is-lambda term)
              (mk-application (function-of (caddr term)) (evaluate (car (last term)))) term))))
