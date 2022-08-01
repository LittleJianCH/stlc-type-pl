% Definitions start here.

%% Here's the definition of the abstract syntax of the language.
%% e ::= x
%%     | λx.e
%%     | (e e)
%%     | zero
%%     | succ e
%%     | e \in T

expr(var(Name)) :- atom(Name).
expr(lam(Name, Body)) :- atom(Name), expr(Body).
expr(app(Fun, Arg)) :- expr(Fun), expr(Arg).
expr(zero).
expr(succ(Num)) :- expr(Num).
expr(ann(Expr, Type)) :- expr(Expr), type(Type).


%% Here's the definition of the type of the language.
%% t ::= nat | arrow(t, t)

type(nat).
type(arrow(Arg, Ret)) :- type(Arg), type(Ret).

% Definitions end.

type_of(Expr, Type) :- 
    expr(Expr),
    type_of_helper([], Expr, Type),
    type(Type). % I have to put type avaliable check here, 
                % otherwise it will cause some segfault.

type_of_helper(Context, zero, nat).
type_of_helper(Context, succ(Num), nat) :- type_of_helper(Context, Num, nat).

type_of_helper(Context, var(Name), Type) :- member([Name, Type], Context).

type_of_helper(Context, app(Fun, Arg), TypeRes) :-
    type_of_helper(Context, Arg, TypeArg),
    type_of_helper(Context, Fun, arrow(TypeArg, TypeRes)).

type_of_helper(Context, lam(Name, Body), arrow(TypeArg, TypeRes)) :-
    append([[Name, TypeArg]], Context, NewContext),
    type_of_helper(NewContext, Body, TypeRes).

type_of_helper(Context, ann(Expr, Type), Type) :- 
    type_of_helper(Context, Expr, Type).