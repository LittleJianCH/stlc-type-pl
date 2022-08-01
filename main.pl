% Definitions start here.

%% Here's the definition of the abstract syntax of the language.
%% e ::= x
%%     | λx.e
%%     | (e e)
%%     | zero
%%     | succ e
%%     | e \in T

make_var(Name, var(Name)).
make_lam(Name, Body, lam(Name, Body)).
make_app(Fun, Arg, app(Fun, Arg)).
make_zero(zero).
make_succ(Num, succ(Num)).
make_ann(Expr, Type, ann(Expr, Type)).


%% Here's the definition of the type of the language.
%% t ::= nat | arrow(t, t)

make_nat(nat).
make_arrow(Arg, Ret, arrow(Arg, Ret)).

% Definitions end.

type_of(Context, zero, nat).
type_of(Context, succ(Num), nat) :- type_of(Context, Num, nat).

type_of(Context, var(Name), Type) :- member([Name, Type], Context).

type_of(Context, app(Fun, Arg), TypeRes) :-
    type_of(Context, Arg, TypeArg),
    type_of(Context, Fun, arrow(TypeArg, TypeRes)).

type_of(Context, lam(Name, Body), arrow(TypeArg, TypeRes)) :-
    append([[Name, TypeArg]], Context, NewContext),
    type_of(NewContext, Body, TypeRes).

type_of(Context, ann(Expr, Type), Type) :- type_of(Context, Expr, Type).