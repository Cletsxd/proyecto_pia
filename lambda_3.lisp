;;;Variable global que contiene
;;;a todos los operadores que el
;;;evaluador permite utilizar
(defvar *oper* '(+ - * / sin cos tan exp
                 log expt sqrt asin acos
                 atan car cdr setq))

(defvar *vars* '())

(defun append1 (lst obj)
  (append lst (list obj)))

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

;;;evaluate/1: es el evaluador general
;;;de calculo lambda. Puede evaluar cualquier
;;;variable, aplicacion o lambda abstraccion
;;;con los operadores asignados al inicio
(defun evaluate (term)
  (if (is-constant term) term
      (if (is-application term)
          (progn (if (eql (function-of term) 'setq)
                     (setq *vars* (append1 *vars* '((first (argument-of term)) (last (argument-of term))))))
                 (mk-application (function-of term) (evaluate (argument-of term))))
          (if (is-lambda term)
              (mk-application (function-of (caddr term)) (evaluate (car (last term))))
              (if (is-variable term) 'term)))))

(defun return-value (var lst)
  (if (not (null lst))
      (if (eql var (caar lst)) (car (last (car lst)))
          (check_variable var (cdr lst)))))

(defun evaluate2 (term)
  (cond ((is-constant term) term)
        ((is-variable term) (return-value term *vars*))
        ((is-application term) (progn (if (eql (function-of term) 'setq)
                                          (setq *vars* (append1 *vars* (list (first (argument-of term)) (car (last (argument-of term))))))
                                          (mk-application (function-of term) (evaluate2 (argument-of term))))))
        ((is-lambda term) (mk-application (function-of (caddr term)) (evaluate2 (car (last term)))))))
