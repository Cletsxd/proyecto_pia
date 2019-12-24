%%%%%%%%%%%%% BASADO EN DIAPOSITIVA 19

%%%%% Operadores
%%% oper/1. oper(?Symbol).
%%% Representación de los operadores alfanuméricos, lambda y lógicos.
%%% Ejemplo de salida:
%%% ?- oper(cdr).
%%% true.
oper(esp(lambda)).
oper(suma). %% Implementada.
oper(resta). %% Implementada.
oper(car). %% Implementada.
oper(cdr). %% Implementada.
oper(esp(if)). %% Implementada.
oper(+). %% Implementada.
oper(/). %% Implementada.
oper(-). %% Implementada.
oper(*). %% Implementada.
oper(**). %% implementada.
oper(>). %% Implementada.
oper(<). %% Implementada.
oper(=). %% Implementada.
oper(not). %% Implementada.
oper(and). %% Implementada.
oper(or). %% Implementada.
oper(impl). %% Implementada.
oper(equiv). %% Implementada.
oper(abs). %% Implementada.
oper(sin). %% Implementada.
oper(cos). %% Implementada.
oper(tan). %% Implementada.
oper(print). %% Implementada.
oper(println). % Implementada.
oper(esp(let)). %% Implementada.
oper(esp(setq)). %% Implementada.
oper(esp(new)). %% Implementada.
oper(esp(defun)). %% Implementada.
oper(esp(progn)).

%%%%% Evaluadores
%%% Átomos.
%%% eval/2. eval(+Atom, -Atom).
%%% Devuelve el átomo si Atom es un átomo.
%%% Ejemplo de salida:
%%% ?- eval("atomo", R).
%%% R = "atomo".
eval([], []).

eval(Var, Value) :-
    variable(Var/Value).

eval(Atom, Atom) :-
    oper(Atom).

eval(Atom, Atom) :-
    atom(Atom).

eval(Atom, Atom) :-
    number(Atom).

eval(Atom, Atom) :-
    string(Atom).

%%% Aplicación sobre una abstracción lambda.
%%% Evaluador especial por si el argumento OpVar es una variable que contiene una abstracción lambda.
%%% Ejemplo de salida:
%%% ?- eval([setq, [w, [lambda, [x, y], [+, [x, y]]]]], R).
%%% R = [lambda, [x, y], [+, [x, y]]] .
%%% ?- eval(w, R).
%%% R = [lambda, [x, y], [+, [x, y]]] .
%%% ?- eval([w, [4, 5]], R).
%%% R = 9 .
eval([OpVar | [Args]], R) :-
    eval(OpVar, [Op | [Vars | [Body]]]),
    Op == lambda,
    eval([lambda, Vars, Body, Args], R).

%%% Aplicación.
%%% eval/2. eval(+List, -Result).
%%% Devuelve el resultado Result del cómputo de List.
%%% Ejemplo de salida:
%%% ?- eval([+, [[+, [15, 34]], [-, [23, 43]]]], R).
%%% R = 29 .
eval([Op | R], Res) :-
    oper(Op),
    eval(R, [HV | _]),
    execute(Op, HV, Res).

%%% NO IMPLEMENTADA.
eval([let | R], Res) :-
    oper(esp(let)),
    execute(let, R, Res).

eval([setq | R], Res) :-
    oper(esp(setq)),
    execute(setq, R, Res).

%%% Abstracción Lambda.
%%% eval/2. eval(+List, -Result).
%%% Devuelve el resultado del cómputo de alguna abstracción lambda.
%%% Una abstracción lambda se compone de lo siguiente:
%%% [lambda, [argumentos], [cuerpo], [valores para los argumentos (pede tenerlos o no)]]
%%% Ejemplo de salida:
%%% ?- eval([lambda, [x, y], [+, [[car, x], [car, y]]], [[1, 2, 3, 4], [3, 4, 5]]], R).
%%% R = 4 .
%%% Ejemplo de salida:
%%% No tiene argumentos numéricos.
%%% ?- eval([lambda, [x, y], [+, [[car, x], [car, y]]]], R).
%%% R = [lambda, [x, y], [+, [[car, x], [car, y]]]] .
eval([lambda | R], Res) :-
    oper(esp(lambda)),
    execute(lambda, R, Res).

%%% Abstracción Lógica.
%%% eval/2. eval(+List, -Result).
%%% Devuelve el resultado del cómputo de alguna abstracción lógica.
%%% Una abstracción lógica se compone de lo siguiente:
%%% [if, [condición], [entonces], [si no]]
%%% Ejemplo de salida:
%%% ?- eval([if, [>, [5, 1]], [print, true], [print, false]], R).
%%% true
%%% R = true .
%%% Ejemplo de salida:
%%% ?- eval([if, [<, [5, 1]], [print, true], [print, false]], R).
%%% false
%%% R = false .
eval([if | [Cond | R]], Res) :-
    oper(esp(if)),
    eval(Cond, HR),
    execute(if, [HR | R], Res).

eval([defun | R], Res) :-
    oper(esp(defun)),
    execute(defun, R, Res).

eval([new], Res) :-
    oper(esp(new)),
    execute(new, _, Res).

eval([progn | RestEx], Res) :-
    oper(esp(progn)),
    execute(progn, RestEx, Res).

%%% Lista de átomos.
%%% eval/2. eval(+List, +List).
%%% Ejemplo de salida:
%%% ?- eval([ad, d, df, 23, 4, "atom", "yeah"], R).
%%% R = [ad, d, df, 23, 4, "atom", "yeah"] .
eval([H | R], [Val | Res]) :-
    eval(H, Val),
    eval(R, Res).

%%%%% Operaciones de ejecución.
%%% execute/3. execute(+Operator, +Args, -Result).
%%% Suma
%%% Calcula la suma de los argumentos en la lista Args.
%%% Ejemplo de salida:
%%% ?- eval([suma, [[**, [2, 4]], [-, [5, 6]]]], R).
%%% R = 15 .
%%% Ejemplo de salida:
%%% eval([+, [3, 4]], R).
%%% R = 7 .
execute(suma, [], 0).
execute(suma, [H | R], H1) :-
    execute(suma, R, Res),
    H1 is H + Res.
execute(+, Args, Res) :-
    execute(suma, Args, Res).

%%% Resta
%%% Calcula la resta de los argumentos en la lista Args.
%%% Ejemplo de salida:
%%% ?- eval([resta, [[/, [20, 5]], [+, [1, 5]]]], R).
%%% R = -2 .
%%% Ejemplo de salida:
%%% ?- eval([-, [1234, 21]], R).
%%% R = 1213 .
execute(resta, [], 0).
execute(resta, [H | R], H1) :-
    execute(resta, R, Res),
    H1 is H - Res.
execute(-, Args, Res) :-
    execute(resta, Args, Res).

%%% Multiplicación.
%%% Devuelve la multiplicacion de P1 por P2.
%%% Ejemplo de salida:
%%% ?- eval([*, [[-, [34, 35]], [**, [2, 2]]]], R).
%%% R = -4 .
execute(*,[P1 | P2], Res):-
    Res is P1 * P2.
execute(*, N, N):-
    \+ is_list(N).
execute(*, [], 0).

%%% División.
%%% execute(/, +List, -Res).
%%% Devuelve Head de List a división de Head/Tail
%%% Ejemplo de salida:
%%% ?- eval([/, [1, 4]], R).
%%% R = 0.25 .
%%% ?- eval([/, [1, 0]], R).
%%% PENDIENTE.
execute(/, [_ | 0], _):-
    write("Error: se encontró denominador 0").
execute(/, N, N):-
    \+ is_list(N).
execute(/, [], 0).
execute(/, [Num | Den], Res):-
    Res is Num / Den.

%%% execute(**, +List, -Res).
%%% Devuelve Head de List a la potencia Tail.
%%% Ejemplo de salida:
%%% ?- eval([**, [1, 10]], R).
%%% R = 1 .
execute(**, [Base | Potencia], Res):-
    Res is Base ** Potencia.
execute(**, N, N):-
    \+ is_list(N).
execute(**, [], 0).

%%% execute(abs, +Numero, -Res).
%%% Devuelve el valor absoluto del número.
%%% Ejemplo de salida:
%%% ?- eval([abs, -1], R).
%%% R = 1 .
execute(abs, N, Res):-
    Res is abs(N).
execute(abs, [], _):-
    write("Error: se encontro una lista vacia").

%%% execute(sin, +Numero, -Res).
%%% Devuelve el seno del número.
execute(sin, N, Res):-
    Res is sin(N).

%%% execute(cos, +Numero, -Res)-
%%% Devuelve el coseno del número.
execute(cos, N, Res):-
    Res is cos(N).

%%% execute(tan, +Numero, -Res)-
%%% Devuelve la tangente del número.
execute(tan, N, Res):-
    Res is tan(N).

%%% IF
%                vvvvvvvvv List vvvvvvvv
%%% execute(if, +[Cond, [Then | [Else]]], -Res).
% Realiza el cómputo de una operación lógica.
%%% Ejemplo de salida:
% ?- eval([if, [>, [5, 2]], [print, "verdadero"], [false]], R).
% verdadero
% R = "verdadero" .
%%% Ejemplo de salida:
% ?- eval([if, [>, [1, 2]], [print, "verdadero"], [false]], R).
% R = [false] .
%%% Ejemplo de salida:
% 30 ?- eval([if, true, [print, "verdadero"], [false]], R).
% verdadero
% R = "verdadero" .
%%% IF TRUE THEN
execute(if, [true | [Then | [_]]], Res) :-
    eval(Then, Res).

%%% IF FALSE ELSE
execute(if, [false | [_ | [Else]]], Res) :-
    eval(Else, Res).

%%% Mayor que.
%%% execute(>, +[Oper1, Oper2], -Res).
%%% Devuelve true, si la operación es verdadera.
%%% Ejemplo de salida:
%%% ?- eval([>, [5, 1]], R).
%%% R = true .
%%% ?- eval([>, [1, 5]], R).
%%% R = false .
execute(>, [C1 | C2], true) :-
    eval(C1, R1),
    eval(C2, [Res2 | _]),
    R1 > Res2.
execute(>, _, false).

%%% Menor que.
%%% execute(<, +[Oper1, Oper2], -Res).
%%% Devuelve true, si la operación es verdadera.
%%% Ejemplo de salida:
%%% ?- eval([<, [[+, [53, 45]], [-, [3, 4]]]], R).
%%% R = false .
%%% ?- eval([<, [0, 1]], R).
%%% R = true .
execute(<, [C1 | C2], true) :-
    eval(C1, R1),
    eval(C2, [Res2 | _]),
    R1 < Res2.
execute(<, _, false).

%%% Igual a.
%%% execute(=, +[Oper1, Oper2], -Res).
%%% Devuelve true, si la operación es verdadera.
execute(=, [C1 | C2], true) :-
    eval(C1, R1),
    eval(C2, [Res2 | _]),
    R1 == Res2.
execute(=, _, false).

%%% NEGACIÓN.
execute(not, OP, false) :-
    OP == true, !.
execute(not, OP, true) :-
    OP == false, !.

%%% AND.
execute(and, [C1 | C2], true) :-
    eval(C1, Rc1),
    eval(C2, [Rc2 | _]),
    Rc1 == true,
    Rc2 == true.
execute(and, _, false).

%%% OR.
execute(or, [C1 | C2], false) :-
    eval(C1, Rc1),
    eval(C2, [Rc2 | _]),
    Rc1 == false,
    Rc2 == false.
execute(or, _, true).

%%% IMPLICACIÓN.
execute(impl, [C1 | C2], false) :-
    eval(C1, Rc1),
    eval(C2, [Rc2 | _]),
    Rc1 == true,
    Rc2 == false.
execute(impl, _, true).

%%% EQUIVALENCIA.
execute(equiv, [C1 | C2], true) :-
    eval(C1, Rc1),
    eval(C2, [Rc2 | _]),
    Rc1 == Rc2.
execute(equiv, _, false).

%%% Cabeza de una lista.
% Retorna la cabeza de una lista.
execute(car, [H | _], H).

%%% Resto de una lista.
% Retorna el resto de una lista.
execute(cdr, [_ | R], R).

%%% Print.
%%% execute(print, +Args, -Args).
% Escribe y devuelve la evaluación de Args.
execute(print, Args, Res) :-
    eval(Args, Res),
    write(Res).

execute(println, Args, Res) :-
    eval(Args, Res),
    write(Res), nl.

execute(let, [Subs | [Func]], Res) :-
    div_vars_vals(Subs, Variables, Values),
    subs_main(Variables, Func, Values, ResSub),
    eval(ResSub, Res).

%%% SETQ, para variables globales.
%%% Ejemplo de salida:
%%% ?- eval([setq, [a, [lambda, [x, y], [+, [x, y]], [5, 6]]], [z, 4]], R).
%%% R = 4 .
%%% ?- ?- eval(a, R).
%%% R = 11 .
%%% ?- eval([+, [a, z]], R).
%%% R = 15 .
%%% ?- eval(z, R).
%%% R = 4 .
%%% Ejemplo de salida:
%%% ?- eval([setq, [w, [lambda, [x, y], [+, [x, y]]]]], R).
%%% R = [lambda, [x, y], [+, [x, y]]] .
%%% ?- eval(w, R).
%%% R = [lambda, [x, y], [+, [x, y]]] .
%%% ?- eval([w, [4, 5]], R).
%%% R = 9 .
%%% ?- eval([w, [[*, [3, 4]], [/, [4, 5]]]], R).
%%% R = 12.8 .
%%% Ejemplo de salida:
%%% ?- eval([w, [z, [+, [z, z]]]], R).
%%% R = 12 .
%%% Ejemplo de salida:
%%% ?- eval([setq, [var, [w, [z, a]]]], R).
%%% R = 15 .
%%% ?- ?- eval([print, var], R).
%%% 15
%%% R = 15 .
%%% Ejemplo de salida:
%%% ?- variable(X).
%%% X = var/15 ;
%%% X = w/[lambda, [x, y], [+, [x, y]]] ;
%%% X = z/4 ;
%%% X = a/11.
execute(setq, [], R) :-
    variable(_ / R).
execute(setq, [[Var | [Value | _]] | R], Res2) :-
    eval(Value, Res1),
    asserta(variable(Var / Res1)),
    execute(setq, R, Res2).

%%% Ejecutable lambda sin valores de entrada.
%%% Ejemplo de entrada:
%%% ?- eval([lambda, [x, y], [+, [[car, x], [car, y]]]],R).
%%% R = [lambda, [x, y], [+, [[car, x], [car, y]]]] .
execute(lambda, [Args |[Body]], [lambda, Args, Body]).

%%% Ejecutable lambda como una aplicación.
%%% Ejemplo de salida:
%%% ?- eval([lambda, [x, y], [+, [[car, x], [car, y]]], [[3, 4, 5], [1, 3]]],R).
%%% R = 4 .
execute(lambda, [Args |[Body | [Values]]], Res) :-
    subs_main(Args, Body, Values, R),
    eval(R, Res).

execute(defun, [Name | [Vars | [Body]]], Res) :-
    eval([setq, [Name, [lambda, Vars, Body]]], Res).

execute(progn, [F | []], Res) :-
    eval(F, Res).
execute(progn, [F | R], Res) :-
    eval(F, _),
    execute(progn, R, Res).

execute(new, _, "Running new program...") :-
    retractall(variable(_/_)).

%%%%% OPERACIONES AUXILIARES.
%%% subs_main/4. subs_main(+Args, +Body, +Values, -Res).
% Devuelve la substitución de las variables de Body por los valores declarados en Value.
% Ejemplo de salida:
% ?- subs_main([x, y], [+, [x, y]], [5, 6], R).
% R = [+, [5, 6]].
subs_main(Args, Body, Vals, R) :-
    pre_sub(Args, Vals, PSubs),
    check_subs(Body, PSubs, R), !.

%%% pre_sub/3. pre_sub(+Args, +Values, -Res).
% Devuelve la pre-substitución de las variables de Args con los valores en Values.
pre_sub([], [], []).
pre_sub([Var1 | R1], [Vl1 | R2], [Var1/RealVl1 | Res]) :-
    eval(Vl1, RealVl1),
    pre_sub(R1, R2, Res).

%%% check_subs/3. check_subs(+Body, +PreSubs, -Result).
% Realiza la substitución de las variables pre-subsituidas de PreSubs en Body.
check_subs([], _, []).
check_subs([H | R], Subs, [HR | RR]) :-
    is_list(H),
    check_subs(H, Subs, HR),
    check_subs(R, Subs, RR),
    RR \== [].
check_subs([H | _], Subs, [HR]) :-
    is_list(H),
    check_subs(H, Subs, HR).
check_subs([H | R], Subs, [Res | RR]) :-
    substitution(H, Subs, Res),
    check_subs(R, Subs, RR),
    RR \== [].
check_subs([H | _], Subs, [Res]) :-
    substitution(H, Subs, Res).

%%% substitution/3. substitution(+Symb, +PreSubs, -Value).
% Devuelve la substitución del símbolo Symb por alguna pre-substitución PreSubs.
substitution(Var, [], Var).
substitution(Var, [Vr/Value | _], Value) :-
    Var == Vr.
substitution(Var, [_ | R], Res) :-
    substitution(Var, R, Res).

div_vars_vals([], [], []).
div_vars_vals([[Var | [Val]] | R], [Var | ResVar], [ValEv | ResVal]) :-
    eval(Val, ValEv),
    div_vars_vals(R, ResVar, ResVal).

:- retractall(variable(_/_)).