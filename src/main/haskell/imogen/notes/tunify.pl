
t_string_unify([]).

t_string_unify([S=T|G]) :- 
        flatten(S,S1,[]),
        flatten(T,T1,[]),
        tunify(S1,[],T1), 
        t_string_unify(G).

tunify([],[],[]).

tunify([],[],[X|T]) :- tunify([X|T],[],[]).

tunify([X1|S],[],[X2|T]) :- 
        X1==X2, 
        !, 
        tunify(S,[],T).

tunify([C|S],[],[V|T]) :- 
        atom(C),
        !, 
        var(V), 
        tunify([V|T],[],[C|S]).

tunify([V|S],Z,[]) :- 
        V=Z,
        tunify(S,[],[]).

tunify([V|S],[],[C1|T]) :- 
        atom(C1), 
        V=[],
        tunify(S,[],[C1|T]).

tunify([V|S],Z,[C1,C2|T]) :-
        atom(C1),
        atom(C2),
        append(Z,[C1],V),

tunify(S,[],[C2|T]).

tunify([V,X|S],[],[V1|T]) :-
        var(V1), 
        tunify([V1|T],[V],[X|S]).

tunify([V,X|S],[Z1|Z],[V1|T]):- 
        var(V1),
        append([Z1|Z],[Vnew],V),
        tunify([V1|T],[Vnew],[X|S]).

tunify([V|S],Z,[X|T]) :- (S=[]; t\=[]; atom(X)) ->
        append(Z,[X],Z1), 
        tunify([V|S],Z1,T).

flatten([],A,A).

flatten(A,[A|B],B) :-
        (var(A); atom(A)) -> 
            A \== [],
            !.

flatten([A|B],C,D) :-
        flatten(A,C,E),
        flatten(B,E,D).

%-------------------------------------------------------------------------------
% Junk                                                                              
%-------------------------------------------------------------------------------


app([], Ys, Ys).
app([X|Xs], Ys, [X|Zs]) :-
        app(Xs, Ys, Zs).
   
flat([], []).
flat(.(X, Xs), Ys) :-
        flat(Xs, Ys1),
        app(X, Ys1, Ys).

plus(N, M, K) :- K is N + M.
times(N, M, K) :- K is N * M.

polish_eval(Input, R) :- polish_eval([], Input, R).
 
polish_eval([X], [], X).

polish_eval(S, [Op|Rest], R) :- member(Op,[plus,times]),
                                push(S1, Y, S),
                                push(S2, X, S1),
                                call(Op, X, Y, R1),
                                push(S2, R1, S3),
                                polish_eval(S3, Rest, R).

%% polish_eval(S, [X, Y, Op | Rest], R) :- 
%%         call(Op, X, Y, R1),
%%         push(S, R1, S1),
%%         polish_eval(S1, Rest, R).

%% polish_eval(S, [+|Rest], R) :- 
%%         push(S1, Y, S),
%%         push(S2, X, S1),
%%         R1 is X+Y,
%%         push(S2, R1, S3),
%%         polish_eval(S3, Rest, R).

%% polish_eval(S, [-|Rest], R) :- push(S1, Y, S),
%%                                push(S2, X, S1),
%%                                R1 is X-Y,
%%                                push(S2, R1, S3),
%%                                polish_eval(S3, Rest, R).

%% polish_eval(S, [*|Rest], R) :- push(S1, Y, S),
%%                                push(S2, X, S1),
%%                                R1 is X*Y,
%%                                push(S2, R1, S3),
%%                                polish_eval(S3, Rest, R).

%% polish_eval(S, [/|Rest], R) :- push(S1, Y, S),
%%                                push(S2, X, S1),
%%                                R1 is X/Y,
%%                                push(S2, R1, S3),
%%                                polish_eval(S3, Rest, R).

polish_eval(S, [X|Rest], R) :- 
        push(S, X, S1),
        polish_eval(S1, Rest, R).
 
%% polish_eval([X], [], X).

%% polish_eval(S, [Op|Rest], R) :- push(S1, Y, S),
%%                                push(S2, X, S1),
%%                                R1 is call(Op,(X,Y)),
%%                                push(S2, R1, S3),
%%                                polish_eval(S3, Rest, R).

push(S, X, [X|S]).

