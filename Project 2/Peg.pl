
/* Gurnoor Aujla 10102069 Peg Solitaire: Solves 5 peg solitaire problems. 




printBoard, is the main board printing predicate, it takes in hardcoded lists of the non moveable spots, the valid spots, the spots for the problem, and the goal spot
it also take a counter, and sees if that counter is a memeber of a combination of the lists. This decides what to print out into the console. */ 

printBoard(70, Em, SB, EB, GB).
printBoard(N, Em, SB, EB, GB):- 
(emptyPrint(N, Em, SB, EB, GB);pegPrint(N, Em, SB, EB, GB);noPegPrint(N, Em, SB, EB, GB);newLinePrint(N, Em, SB, EB, GB);goalPrint(N, Em, SB, EB, GB);overlapPrint(N, Em, SB, EB, GB)).




%this preidcate decides if the counter is a member of the list of non-moveable spaces, if so it prints a space

emptyPrint(N, Em, SB, EB,GB) :-
member(N,Em), 
write(' '),
N1 is N+1,
printBoard(N1, Em, SB, EB,GB).




%this preidcate decides if the counter is a member of the list of moveable spaces, and the problem board if so it prints a X

pegPrint(N, Em, SB, EB, GB) :-
member(N,SB), 
member(N,EB),
not(member(N,GB)),  
write('X'),
N1 is N+1,
printBoard(N1, Em, SB, EB, GB).


%this preidcate decides if the counter is a member of the list of moveable spaces, and not a member of either the goal or problem board if so it prints a -

noPegPrint(N, Em, SB, EB, GB) :-
member(N,SB), 
not(member(N,EB)), 
not(member(N,GB)),
write('-'),
N1 is N+1,
printBoard(N1, Em, SB, EB, GB).

%this preidcate decides if the counter is a member of the list of moveable spaces, and  a member of the goal but not the problem board if so it prints a O

goalPrint(N, Em, SB, EB, GB) :-
member(N,GB),  
member(N,SB),
not(member(N,EB)), 
write('O'),
N1 is N+1,
printBoard(N1, Em, SB, EB, GB).

%this preidcate decides if the counter is a member of the list of moveable spaces, and  a member of the goal and the problem board if so it prints a X

overlapPrint(N, Em, SB, EB, GB) :-
member(N,EB),
member(N,GB),
member(N,SB),  
write('X'),
N1 is N+1,
printBoard(N1, Em, SB, EB, GB).

%this preidcate decides if the counter is not a member of any of the lists, presumes it must be out of bounds, and starts a new line and increments the counter by 3.

newLinePrint(N, Em, SB, EB, GB) :-
not(member(N,SB)),
not(member(N,EB)),
not(member(N,Em)),
nl,
N1 is N+3,
printBoard(N1, Em, SB, EB, GB).


%all of the possible problem boards, the board itself, and the spaces that need to be empty.

empty([0,1,5,6,10,11,15,16,50,51,55,56,60,61,65,66]).
board([2,3,4,12,13,14,20,21,22,23,24,25,26,30,31,32,33,34,35,36,40,41,42,43,44,45,46,52,53,54,62,63,64]).
crossbow([31,32,34,35,41,42,43,44,45,53]). 
longbow([20, 26, 30, 31, 33, 35, 36, 41, 43, 45, 52, 53, 54, 63]). 
standard([2,3,4,12,13,14,20,21,22,23,24,25,26,30,31,32,34,35,36,40,41,42,43,44,45,46,52,53,54,62,63,64]).
notquite([2,3,4,12,14,20,21,22,23,24,25,26,30,32,35,36,40,41,42,43,44,45,46,52,54,62,64]).
half([20,22,23,24,30,34,35,40,41,42,43,44,45,52,54,62,64]).


%defining in the onboard space. 
onboard(Pos) :- 2 =< Pos, Pos =< 4.
onboard(Pos) :- 12 =< Pos, Pos =< 14.
onboard(Pos) :- 20 =< Pos, Pos =< 26.
onboard(Pos) :- 30 =< Pos, Pos =< 36.
onboard(Pos) :- 40 =< Pos, Pos =< 46.
onboard(Pos) :- 52 =< Pos, Pos =< 54.
onboard(Pos) :- 62 =< Pos, Pos =< 64.

%the board is defined so that a move to the left and right is -1 and +1, a move up and down is +10, and -10. A jump would be those values multiplied by 2. 
%the predicate also checks if the jump is onboard or not. 

jump(Start, Jumped, End) :-
Jumped is Start + 1, 
End is Start + 2,
onboard(Start),
onboard(Jumped), 
onboard(End).

jump(Start, Jumped, End) :-
Jumped is Start - 1, 
End is Start - 2,
onboard(Start),
onboard(Jumped), 
onboard(End).

jump(Start, Jumped, End) :-
Jumped is Start + 10, 
End is Start + 20,
onboard(Start),
onboard(Jumped), 
onboard(End).

jump(Start, Jumped, End) :-
Jumped is Start - 10, 
End is Start - 20,
onboard(Start),
onboard(Jumped), 
onboard(End).

%peg is the predicate that prints out the board for each respective problem, it takes in solitaire steps to generate the moves, and printboard along with recursiveprint
%to print the moves. It is very similar along all of the problems. 
	
peg(crossbow) :- 
board(X), 
empty(Q),
crossbow(P), 
printBoard(0, Q, X, P, [3]),
nl,nl,nl,
solitaire_steps(crossbow,P,[],[H|T]),  
recursivePrint(X,P,Q,[3],[H|T]). 

peg(full) :- 
board(X), 
empty(Q),
standard(P),
printBoard(0, Q, X, P, [33]),
nl,nl,nl,
solitaire_steps(full,P,[],[H|T]),  
recursivePrint(X,P,Q,[33],[H|T]). 

peg(halfdead) :- 
board(X), 
empty(Q),
half(P),
printBoard(0, Q, X, P, [33]),
nl,nl,nl,
solitaire_steps(half,P,[],[H|T]),  
recursivePrint(X,P,Q,[33],[H|T]). 

peg(notquitedead) :- 
board(X), 
empty(Q),
notquite(P),
printBoard(0, Q, X, P, [33]),
nl,nl,nl,
solitaire_steps(notquite,P,[],[H|T]),  
recursivePrint(X,P,Q,[33],[H|T]). 

peg(longbow) :- 
board(X),
empty(Q),
longbow(P),
printBoard(0, Q, X, P,[3]),
nl,nl,nl,
solitaire_steps(longbow,P,[],[H|T]),  
recursivePrint(X,P,Q,[3],[H|T]). 


%the recursive print predicate takes in the list of moves, and applys them to the board, and prints out the output board after each application up until the goal board. 

recursivePrint(X, GB, Q, GB, []).
recursivePrint(X,Y,Q,GB,[H|T]) :- 
solitaire_move(Y, H, M), 
printBoard(0, Q, X, M,GB),
nl,nl,nl,
recursivePrint(X, M, Q, GB, T),!.

%solitaire_steps recursively provides a list of moves for the game inputted, utilizing both independence checking and pagoda functions. 

solitaire_steps(Game,GB,_,[]) :- final_board(Game,GB). 
solitaire_steps(Game,Board,Hist,[Mv|Moves]) :-  
    solitaire_move(Board,Mv,NewBoard),
    independent(Mv,Hist),
	findall((P,W),(member(P,[assym, assymrot, center, simple]), wgt(P,NewBoard,W)), Wgts),check_wgts(Game,Wgts),
	solitaire_steps(Game,NewBoard,[Mv | Hist], Moves).

%solitaire_move applys the moves to create an end board.	
	
solitaire_move(SB, (Start, End), [End|SB2]) :-
    select(Start, SB, SB1),
    jump(Start, Jumped, End),
    select(Jumped, SB1, SB2),
    not(member(End,SB2)).
	

	

%predicate for checking of the moves independence
independent(Mv, []).
independent(Mv, [H|T]) :-
  overlap(Mv, H), !.
independent(Mv, [H|T]) :-
  lexorder(Mv, H),
  independent(Mv, T).
  
 %checks if the moves overlap by comparing every point of the move. 
overlap((Start1, End1), (Start2, End2)) :- 
    jump(Start1, Jumped1, End1), 
	jump(Start2, Jumped2, End2), 
	(Start1 = End2 ; Start1 = Jumped2 ; Start1 = Start2 ; 
	Jumped1 = End2 ; Jumped1 = Jumped2 ; Jumped1 = Start2 ; 
	End1 = End2 ; End1 = Jumped2 ; End1 = Start2). 

%checks if the moves are in lexical order from left to right or top to bottom. 	
lexorder((Start1, _),(Start2, _)):- Start1 =< Start2. 


%the ending state for each game used in tandem with solitaire move. 
final_board(crossbow, [3]).
final_board(longbow, [3]).
final_board(notquite, [33]).
final_board(full, [33]).
final_board(half, [33]).

	
%used to check if the wgt is greater than or equal to the goal wgt	
check_wgts(G,[]).
check_wgts(G,[(P,WgtP) | Rest]):-
    goal_wgt(G,P,WgtGoal),
    WgtP >= WgtGoal,
    check_wgts(G,Rest). 
	
%used to retrieve weight. 
wgt(_,[], 0).
wgt(P,[H | T], Wgt) :-
    (pagoda(P,H,PWgt) ; PWgt = 0), !,
	wgt(P,T,WgtRest),
	Wgt is WgtRest + PWgt. 
	

% all of the goal weights for each problem and respective pagoda function 
goal_wgt(crossbow, assymrot,0).
goal_wgt(crossbow,assym,0).
goal_wgt(crossbow,simple,0).
goal_wgt(crossbow,center,0).
goal_wgt(longbow,assymrot,0).
goal_wgt(longbow,assym,0).
goal_wgt(longbow,simple,0).
goal_wgt(longbow,center,0).
goal_wgt(full,assym,2).
goal_wgt(full,assymrot,2).
goal_wgt(full, simple, 1). 
goal_wgt(full, center, 0). 
goal_wgt(notquite,assym,2).
goal_wgt(notquite,assymrot,2).
goal_wgt(notquite, simple, 1).
goal_wgt(notquite, center, 0). 
goal_wgt(half,assym,2).
goal_wgt(half,assymrot,2).
goal_wgt(half, simple, 1).
goal_wgt(half, center, 0). 


%definition of all of the pagoda functions.
pagoda(assym, 13, 1).
pagoda(assym, 20, (-1)).
pagoda(assym, 21, 1).
pagoda(assym, 23, 1).
pagoda(assym, 25, 1).
pagoda(assym, 26, (-1)).
pagoda(assym, 31, 2).
pagoda(assym, 33, 2).
pagoda(assym, 35, 2).
pagoda(assym, 40, (-1)).
pagoda(assym, 41, 1).
pagoda(assym, 43, 1).
pagoda(assym, 45, 1).
pagoda(assym, 46, (-1)).
pagoda(assym, 53, 1).
pagoda(assymrot, 2, (-1)). 
pagoda(assymrot, 4, (-1)).
pagoda(assymrot, 12, 1).
pagoda(assymrot, 13, 2).
pagoda(assymrot, 14, 1).
pagoda(assymrot, 31, 1).
pagoda(assymrot, 32, 1).
pagoda(assymrot, 33, 2).
pagoda(assymrot, 34, 1).
pagoda(assymrot, 35, 1).
pagoda(assymrot, 52, 1).
pagoda(assymrot, 53, 2).
pagoda(assymrot, 54, 1).
pagoda(assymrot, 62, (-1)).
pagoda(assymrot, 64, (-1)).
pagoda(simple, 13, 1). 
pagoda(simple, 31, 1).
pagoda(simple, 33, 1).
pagoda(simple, 35, 1).
pagoda(simple, 53, 1).
pagoda(center, 2, (-1)). 
pagoda(center, 4, (-1)). 
pagoda(center, 20, (-1)). 
pagoda(center, 40, (-1)).
pagoda(center, 62, (-1)). 
pagoda(center, 64, (-1)).
pagoda(center, 26, (-1)). 
pagoda(center, 46, (-1)).
pagoda(center, 12, 1).
pagoda(center, 22, 1). 
pagoda(center, 21, 1).
pagoda(center, 41, 1).
pagoda(center, 42, 1). 
pagoda(center, 52, 1).
pagoda(center, 54, 1).
pagoda(center, 44, 1). 
pagoda(center, 45, 1).
pagoda(center, 14, 1).
pagoda(center, 24, 1). 
pagoda(center, 25, 1).



