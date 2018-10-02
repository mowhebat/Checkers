% play
% Start the game.
play :-
    nl,
    write('********************'), nl,
	  write('* Prolog Checkers *'), nl,
	  write('********************'), nl, nl,
	  write('Rem : x starts the game'), nl,
	  playAskColor.
	
	
	
% playAskColor
% Ask the color for the human player and start the game with it.
playAskColor :-
	  nl, write('Color for human player ? (x or o)'), nl,
	  read(Player), nl,
          nl, write('Select Difficulty ?'), nl,
          read(Depth), nl,
	  (
	    Player \= o, Player \= x, !,     % If not x or o -> not a valid color
	    write('Error : not a valid color !'), nl,
	    playAskColor                     % Ask again
	    ;
	    InitBoard =  [[x,0,x,0,x,0,x,0]
                   	 ,[0,x,0,x,0,x,0,x]
                   	 ,[x,0,x,0,x,0,x,0]
                  	 ,[0,0,0,0,0,0,0,0]
                  	 ,[0,0,0,0,0,0,0,0]
                 	 ,[0,o,0,o,0,o,0,o]
                 	 ,[o,0,o,0,o,0,o,0]
                 	 ,[0,o,0,o,0,o,0,o]],

	    show(InitBoard), nl,
	
	    % Start the game with color and emptyBoard
	    play([x, play, InitBoard], Depth, Player)
	  ).


% play(+Position, +HumanPlayer)
% If next player to play in position is equal to HumanPlayer -> Human must play
% Ask to human what to do.
play([Player, play, Board], Depth, Player) :- !,
    (
      humanMove([Player, play, Board], [NextPlayer, State, NextBoard]), !,
     % show(NextBoard),
      opp2(Player, NextPlayer),
      (
        State = win, !,                             % If Player win -> stop
        nl, write('End of game : '),
        write(Player), write(' win !'), nl, nl
        ;
        State = draw, !,                            % If draw -> stop
        nl, write('End of game : '),
        write(' draw !'), nl, nl
        ;
        play([NextPlayer, play, NextBoard],Depth, Player) % Else -> continue the game
        )
     % ;
   %   write('-> Bad Move !'), nl,                % If humanMove fail -> bad move
    %  play([Player, play, Board],Depth, Player)        % Ask again
    ).




% play(+Position, +HumanPlayer)
% If it is not human who must play -> Computer must play
% Compute the best move for computer with minimax or alpha-beta.
play([Player, play, Board], Depth, HumanPlayer) :-
    nl, write('Computer play : '), nl, nl,
    % Compute the best move
    findall([Pre_I, Pre_J],game(Board,[Pre_I, Pre_J],[Post_I, Post_J], Depth,Player), AllPre),
    findall([Post_I, Post_J],game(Board,[Pre_I, Pre_J],[Post_I, Post_J], Depth,Player), AllPost),
    AllPre = [[I_Pre, J_Pre]|_],
    is_king(Board, I_Pre, J_Pre, Player, RightPlayer),
    set_piece2(Board,AllPre,AllPost, RightPlayer, FinalBoard),
    opp2(Player, NextPlayer),
    (
      winningPos(NextPlayer, FinalBoard), !, State = win ;
      drawPos(Player,FinalBoard), !, State = draw ;
      State = play
    ),
    %show(FinalBoard),
    (
      State = win, !,                                 % If Player win -> stop
      nl, write('End of game : '),
      write(Player), write(' win !'), nl, nl
      ;
      State = draw, !,                                % If draw -> stop
      nl, write('End of game : '), write(' draw !'), nl, nl
      ;
      % Else -> continue the game
      play([NextPlayer, play, FinalBoard], Depth, HumanPlayer)
    ).

is_king( Board, I, J, Color, King ) :-
    king(Color, King),
    index(Board, I, J, King), !.

is_king( _, _, _, Color, Color ).


become_king( P, I, _ , KP ) :-
    can_n(P),
    I is 8,
    king(P,KP), !.

become_king( P, I, _ , KP ) :-
    can_s(P),
    I is 1,
    king(P,KP), !.

become_king( P, _, _ , P ).
    

/*game2( Board, AllPre, AllPost, Depth, Color ) :-
    findall([Pre_I, Pre_J],game(Board,[Pre_I, Pre_J],[Post_I, Post_J], Depth,Color), AllPre),
    findall([Post_I, Post_J],game(Board,[Pre_I, Pre_J],[Post_I, Post_J], Depth,Color), AllPost),
    set_piece2(Board,AllPre,AllPost, Color, FinalBoard),
    show(FinalBoard).*/

game( Board, [Pre_I, Pre_J], [Post_I, Post_J], Depth, Color ) :-
    % move([Color,_,Board],[_,_,NextBoard]),
     minimax([Color,_,Board],20,-20,Depth,_,[_,_,NextBoard]),
    % show(NextBoard),
     find( NextBoard, [Pre_I, Pre_J], [Post_I, Post_J],1, Color ).

set_piece2(Board,[A],[[I_Post,J_Post]], Color, FinalBoard) :-
    become_king(Color,I_Post,J_Post, King_Color),
    set_piece(Board,A, [I_Post,J_Post] , King_Color, FinalBoard),
    show(FinalBoard), nl,
    !.
    
set_piece2(Board,[A|A1],[B|B1], Color, FinalBoard) :-
    set_piece2(Board,[A], [B], Color, Board2),
    set_piece2(Board2,A1, B1, Color, FinalBoard).

set_piece(Board,[P1,P2], [P3,P4], Color, FinalBoard) :-
    is_neighber([P1 , P2 ] , [P3,P4]) ,
    P_3 is P3 - 1,
    P_4 is P4 - 1,
    P_1 is P1-1,
    P_2 is P2-1,
    replace(Board , P_1 , P_2 ,0, B1),
    replace(B1 , P_3 , P_4 ,Color, FinalBoard).

set_piece(Board,[P1,P2], [P3,P4], Color, FinalBoard) :-
    P_m1 is ((P1 + P3) / 2) ,
    P_m2 is ((P2 + P4)/2) ,
    P_m_1 is P_m1 - 1,
    P_m_2 is P_m2 - 1,
    P_1 is P1-1,
    P_2 is P2-1,
    replace(Board , P_1 , P_2 ,0, B1),
    replace(B1 , P_m_1 , P_m_2 , 0 , B),
    P_3 is P3 -1 ,
    P_4 is P4 - 1,
    replace(B , P_3 , P_4 ,Color, FinalBoard).


is_neighber([P1,P2] , [P3,P4]) :-
  P1 is (P3 - 1),
    P2 is (P4 -1 ).

is_neighber([P1,P2] , [P3,P4]) :-
  P1 is P3 - 1,
    P2 is (P4 + 1 ).

is_neighber([P1,P2] , [P3,P4]) :-
  P1 is (P3 + 1),
    P2 is (P4 -1 ).
 
is_neighber([P1,P2] , [P3,P4]) :-
  P1 is (P3 + 1),
  P2 is (P4 + 1 ).

find( Board, [Pre_I, Pre_J], [Post_I, Post_J], Count, _ ) :-
    index(Board, Pre_I, Pre_J, Count ),   
    Count1 is Count + 1,
    index(Board, Post_I, Post_J, Count1).
  
find( Board, [Pre_I, Pre_J], [Post_I, Post_J], Count, Color ) :-
    Count \= 8,
    Count1 is Count+1,
    find(Board,[Pre_I, Pre_J], [Post_I, Post_J], Count1, Color ).

/*find( Board,[Pre_I, Pre_J],[Post_I, Post_J],Count, Color ) :-
    index(Board, Pre_I, Pre_J , Count ), 
    jump( Color, J_Color),
    index( Board, Post_I, Post_J, J_Color ).*/


minimax(Pos, Min, Max, Depth, Val, BestNextPos) :-
    Depth \= 0,
    % Legal moves in Pos produce NextPosList
    bagof(NextPos, move(Pos, NextPos), NextPosList),
    Depth_1 is Depth-1,
    best(NextPosList, Min, Max, Depth_1, Val, BestNextPos ), !
    ;
    evalPos(Pos, Val).     % Pos has no successors -> evaluate the position


best([Pos], Min, Max, Depth, Val, Pos) :-
    minimax(Pos, Min, Max, Depth, Val, _), !.

best([Pos1 | PosList], Min, Max, Depth, BestVal, BestPos) :-
    Pos1 = [Color,_,Board1] ,
    BestPos = [_,_,BestBoard],
    minimax(Pos1, Min, Max, Depth, Val1, _),
    (
    ( 
    alpha_beta_prune(Color, Max, Min, Val1), !, 
    BestBoard = Board1, 
    BestVal = Val1
    );
    (  
    update_alpha_beta(Color, Max, Min, Val1, NewMax, NewMin),
    best(PosList, NewMin, NewMax, Depth, Val2, Pos2),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal)
    )
    )
    .



betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    min_to_move(Pos0),                         % MIN to move in Pos0
    Val0 > Val1, !                             % MAX prefers the greater value
    ;
    max_to_move(Pos0),                         % MAX to move in Pos0
    Val0 < Val1, !.                            % MIN prefers the lesser value

betterOf(_, _, Pos1, Val1, Pos1, Val1).        % Otherwise Pos1 better than Pos0
% min_to_move(+Pos)
% True if the next player to play is the MIN player.
min_to_move([o, _, _]).

% max_to_move(+Pos)
% True if the next player to play is the MAX player.
max_to_move([x, _, _]).



% Checks if specified value prune other nodes.
alpha_beta_prune(x, _, Beta, Value)  :-
    Value > Beta.
alpha_beta_prune(o, Alpha, _, Value) :-
    Value < Alpha.

% Update Alpha or Beta value based on new 'Value'
update_alpha_beta(x, Alpha, Beta, Value, NewAlpha, Beta) :- 
    (Value > Alpha, !, NewAlpha = Value) ; (NewAlpha = Alpha).
update_alpha_beta(o, Alpha, Beta, Value, Alpha, NewBeta) :- 
    (Value < Beta,  !, NewBeta = Value) ; (NewBeta = Beta).


index(Matrix, Row, Col, Value):-
  nth1(Row, Matrix, MatrixRow),
  nth1(Col, MatrixRow, Value).




replace( [L|Ls] , 0 , Y , Z , [R|Ls] ) :- % once we find the desired row,
  replace_column(L,Y,Z,R)                 % - we replace specified column, and we're done.
  .                                       %
replace( [L|Ls] , X , Y , Z , [L|Rs] ) :- % if we haven't found the desired row yet
  X > 0 ,                                 % - and the row offset is positive,
  X1 is X-1 ,                             % - we decrement the row offset
  replace( Ls , X1 , Y , Z , Rs )         % - and recurse down
  .                                       %

replace_column( [_|Cs] , 0 , Z , [Z|Cs] ) .  % once we find the specified offset, just make the substitution and finish up.
replace_column( [C|Cs] , Y , Z , [C|Rs] ) :- % otherwise,
  Y > 0 ,                                    % - assuming that the column offset is positive,
  Y1 is Y-1 ,                                % - we decrement it
  replace_column( Cs , Y1 , Z , Rs )         % - and recurse down.
  .

can_n(x).
can_n(ko).
can_n(kx).
can_n(j_x).
can_n(j_ko).
can_n(j_kx).

can_s(o).
can_s(ko).
can_s(kx).
can_s(j_o).
can_s(j_ko).
can_s(j_kx).

opp2(x,o).
opp2(o,x).
opp2(j_x,o).
opp2(j_o,x).

opp(x,o).
opp(o,x).
opp(x,ko).
opp(o,kx).
opp(kx,o).
opp(kx,ko).
opp(ko,x).
opp(ko,kx).

opp(j_x,o).
opp(j_o,x).
opp(j_x,ko).
opp(j_o,kx).
opp(j_kx,o).
opp(j_kx,ko).
opp(j_ko,x).
opp(j_ko,kx).


jump(o,j_o).
jump(x,j_x).
jump(ko,j_ko).
jump(kx,j_kx).
jump(j_x,j_x).
jump(j_kx,j_kx).


j_val(j_o).
j_val(j_x).
j_val(j_ko).
j_val(j_kx).

move_aux( P, Board, NextBoard ) :-
    can_n(P),
    index( Board, I , J , P ),
    I1 is I+1,
    J1 is J+1,
    index( Board, I1 , J1 , 0 ), 
    I_1 is I-1,
    J_1 is J-1,
    replace( Board, I_1 , J_1 , 1, B ),
    replace( B , I , J , 2 , NextBoard ) .

move_aux( P, Board, NextBoard) :-
    can_n(P),
    index( Board, I , J , P ),
    I1 is I+1,
    J_1 is J-1,
    index( Board, I1 , J_1 , 0 ), 
    I_1 is I-1,
    replace( Board, I_1 , J_1 , 1, B ) ,
    J_2 is J-2 ,
    replace( B , I , J_2 , 2 , NextBoard ).

move_aux( P, Board, NextBoard) :-
    can_s(P),
    index( Board, I , J , P ),
    I_1 is I-1,
    J1 is J+1,
    index( Board, I_1 , J1 , 0 ), 
    I_1 is I-1,
    J_1 is J-1,
    replace( Board, I_1 , J_1 , 1, B ) ,
    I_2 is I-2,
    replace( B , I_2 , J , 2 , NextBoard ) .


move_aux( P, Board, NextBoard) :-
    can_s(P),
    index( Board, I , J , P ),
    I_1 is I-1,
    J_1 is J-1,
    index( Board, I_1 , J_1 , 0 ), 
    replace( Board, I_1 , J_1 , 1, B ) ,
    I_2 is I-2,
    J_2 is J-2,
    replace( B , I_2 , J_2 , 2 , NextBoard ) .

move_aux_j( P, Board, NextBoard, J_count) :-
    can_n(P),
    index( Board, I , J , P ),
    I1 is I+1,
    J1 is J+1,
    opp(P,OPP_P),
    index( Board, I1 , J1 , OPP_P ),
    I2 is I+2,
    J2 is J+2,
    index(Board, I2 , J2 , 0 ),
    I_1 is I-1,
    J_1 is J-1,
    J_count1 is J_count+1,
    replace( Board, I_1 , J_1 , J_count1 , B ) ,
    replace( B , I , J , 0 , B1 ), 
    jump(P, JP),
    replace( B1 , I1 , J1 , JP , B2 ),
    move_aux_j( JP, B2, NextBoard, J_count1).
    


move_aux_j( P, Board, NextBoard, J_count) :-
    can_n(P),
    index( Board, I , J , P ),
    I1 is I+1,
    J_1 is J-1,
    index( Board, I1 , J_1 , OPP_P ), 
    I2 is I+2,
    J_2 is J-2,
    opp(P,OPP_P),
    index( Board, I2 , J_2 , 0 ), 
    I_1 is I-1,
    J_count1 is J_count+1,
    replace( Board, I_1 , J_1 , J_count1, B ) ,   
    replace( B , I , J_2 , 0 , B1 ),
    J_3 is J-3,  
    jump(P, JP),
    replace( B1 , I1 , J_3 , JP , B2 ),
    move_aux_j( JP, B2, NextBoard, J_count1).


move_aux_j( P, Board, NextBoard, J_count) :-
    can_s(P),
    index( Board, I , J , P ),
    I_1 is I-1,
    J1 is J+1,
    index( Board, I_1 , J1 , OPP_P ),
    I_2 is I-2,
    J2 is J+2,
    opp(P,OPP_P),
    index( Board, I_2 , J2 , 0 ), 
    I_1 is I-1,
    J_1 is J-1,
    J_count1 is J_count+1,
    replace( Board, I_1 , J_1 , J_count1, B ) ,
    replace( B , I_2 , J , 0 , B1 ) ,
    I_3 is I-3,
    jump(P, JP),
    replace( B1 , I_3 , J1 , JP , B2),
    move_aux_j( JP, B2, NextBoard, J_count1).

move_aux_j( P, Board, NextBoard, J_count) :- 
    can_s(P),
    index( Board, I , J , P ),
    I_1 is I-1,
    J_1 is J-1,
    index( Board, I_1 , J_1 , OPP_P ), 
    I_2 is I-2,
    J_2 is J-2,
    opp(P,OPP_P),
    index( Board, I_2 , J_2 , 0 ), 
    J_count1 is J_count+1,
    replace( Board, I_1 , J_1 , J_count1, B ) ,
    replace( B , I_2 , J_2 , 0 , B1 ),
    I_3 is I-3,
    J_3 is J-3,
    jump(P, JP),
    replace( B1 , I_3 , J_3 , JP , B2 ),
    move_aux_j( JP, B2, NextBoard, J_count1).

    move_aux_j( P, Board, NextBoard, J_count) :-
     (P = j_x;
      P = j_kx;
      P = j_o;
      P = j_ko),
      index( Board, I , J , P ),
      J_count1 is J_count+1,
      I_1 is I-1,
      J_1 is J-1,
      replace( Board, I_1 , J_1 , J_count1, NextBoard ).


move_aux_2( P, Board, NextBoard) :-
    move_aux( P, Board, NextBoard).

move_aux_2( P, Board, NextBoard) :-
    move_aux_j( P, Board, NextBoard, 0).

move([X1, play, Board], [X2, _ , NextBoard]) :-
    opp2(X1, X2),
    move_aux_2(X1, Board, NextBoard).
    %show(NextBoard).

move([X1, play, Board], [X2, _ , NextBoard]) :-
    opp2(X1, X2),
    king(X1,King_X1),
    move_aux_2(King_X1, Board, NextBoard).

winningPos( P, Board ) :-
    eval1(Board, P, 0, 0).

drawPos( P, Board ) :-
    eval1(Board, P, 0, 0).


evalPos([P,_,Board], Val) :-
    eval1(Board, x, 0, Val_x1),
    eval1(Board, kx, Val_x1, Val_x2),
    eval1(Board, j_x, Val_x2, Val_x3),
    eval1(Board, j_kx, Val_x3, Val_x),
    eval1(Board, o, 0, Val_o1),
    eval1(Board, ko, Val_o1, Val_o2),
    eval1(Board, j_o, Val_o2, Val_o3),
    eval1(Board, j_ko, Val_o3, Val_o),
    Val1 is Val_x-Val_o,
    (   
    (P = o, !, Val is Val1 + 1); 
    (Val is Val1-1)).

eval1([[]],_,P_count,N_count) :-
    P_count=N_count. 
    eval1([[]|T],P,P_count,N_count):-
              eval1(T,P,P_count,N_count).
    eval1([[H|T]|TP],P,P_count,N_count) :-
      H=P,inc(P_count,V),eval1([T|TP],P,V,N_count);
      H\= P ,eval1([T|TP],P,P_count,N_count).

inc(I1 , I) :-
    I is I1 + 1.

% show(+Board)
% Show the board to current output.
show([X0, X1, X2, X3, X4, X5, X6, X7]) :-
    write( '    0   1   2   3   4   5   6   7  ' ), nl,
    write( '  --------------------------------' ), nl,
    write( '7' ), 
    show1(X7),
    write( '7' ), nl,
    write( '  --------------------------------' ), nl,
    write( '6' ), 
    show1(X6),
    write( '6' ), nl,
    write( '  --------------------------------' ), nl,
    write( '5' ), 
    show1(X5),
    write( '5' ), nl,
    write( '  --------------------------------' ), nl,
    write( '4'), 
    show1(X4),
    write( '4' ), nl,
    write( '  --------------------------------' ), nl,
    write( '3' ), 
    show1(X3),
    write( '3' ), nl,
    write( '  --------------------------------' ), nl,
    write( '2'), 
    show1(X2),
    write( '2' ), nl,
    write( '  --------------------------------' ), nl,
    write( '1' ), 
    show1(X1),
    write( '1' ), nl,
    write( '  --------------------------------' ), nl,
    write( '0' ), 
    show1(X0),
    write( '0' ), nl,
    write( '  --------------------------------' ), nl,
    write( '    0   1   2   3   4   5   6   7  ' ), nl.
     
%show(+Row)
show1([X1, X2, X3, X4, X5, X6, X7, X8]) :-
    write(' | '), show2(X1),
    write(' | '), show2(X2),
    write(' | '), show2(X3),
    write(' | '), show2(X4),
    write(' | '), show2(X5),
    write(' | '), show2(X6),
    write(' | '), show2(X7),
    write(' | '), show2(X8), 
    write(' | ').
     
% show2(+Term)
% Write the term to current outupt
% Replace 0 by ' '.
show2(X) :-
    X = 0, !,
    write('.').

show2(X) :-
    write(X).

checker3(Board , P , P1 , P2 ,P1 , P2) :-
    index(Board , P1 , P2 , P), !.

checker3(Board , P , P1 , P2 ,P1 , P2) :-
    king(P,KP),
    index(Board , P1 , P2 , KP), !.

checker3(Board , P , _ , _ , P3 , P4) :-
    write('your preposition isnot right, enter again'), nl ,
    read([P3,P4]),
	P_3 is P3 + 1,
	P_4 is P4 + 1,
    checker3(Board , P , P_3 , P_4 , _ , _).

check_direction(P , P1 ,_ , P3 ,_ , I1 , J1 , I2 , J2) :-
    can_n(P) ,
    P3 < P1,
    write('invalid direction for jump, enter your prepositon again') , nl,
    read([P5,P6]),
	P_5 is P5 + 1,
	P_6 is P6 + 1,
    read([P7,P8]),
	P_7 is P7 + 1,
	P_8 is P8 + 1,
    check_direction(P,P_5,P_6,P_7,P_8,I1, J1,I2,J2), !.

check_direction(P , P1 ,_ , P3 , _ , I1 , J1 , I2 , J2) :-
    can_s(P),
    P3 > P1 ,
    write('invalid direction for jump, enter your prepositon again'), nl,
    read([P5,P6]),
	P_5 is P5 + 1,
	P_6 is P6 + 1,
    read([P7,P8]),
	P_7 is P7 + 1,
	P_8 is P8 + 1,
    check_direction(P,P_5,P_6,P_7,P_8,I1, J1,I2,J2), !.

check_direction(_ , I1 , J1 , I2 , J2 , I1 , J1 , I2 , J2) .


humanMove([P, play, Board] , [OPP, State, NextBoard]) :-
    opp(P,OPP),
    write('please enter your preposition'), nl ,
    read([P1 , P2]),
	P11 is P1 + 1,
	P22 is P2 + 1,
    checker3(Board , P , P11 , P22 , P5, P6),
    P_1 is P5 - 1 ,
    P_2 is P6 - 1,
    replace(Board , P_1 , P_2 , 0 , B2),
    write('please enter your postposition'),nl,
    read([P3,P4]),
	P33 is P3 + 1,
	P44 is P4 + 1,
    check_direction(P , P5 , P6 , P33 , P44 , I1 , J1 , I2 , J2),
    (   
    (   move_human(P , B2 , [I1,J1] , [I2,J2] , NextBoard),
        show(NextBoard), !);
    (   move_human_j(P , B2 , [I1,J1] , [I2,J2] , B1),
        show(B1),
        can_move_again(B1 , P , [I2 , J2] ,NextBoard )
    )
    ),
    (
      winningPos(OPP, NextBoard), !, State = win ;
      drawPos(P, NextBoard), !, State = draw ;
      State = play
    ).

can_move_again(Board , P , [P3 , P4], NextBoard ) :-
    can_n(P),
    P_3 is P3 + 1 ,
    P_4 is P4 + 1,
    P_1 is P3 - 1,
    P_2 is P4 - 1,
    opp(P , OPP),
    index(Board , P_3 , P_4 , OPP ),
    P7 is P3 + 2,
    P8 is P4 + 2,
    check_boarder(P , P7 , P8 ),
    index(Board , P7 , P8 , 0),
    write('you can move again please enter the next position') , nl,
    read([P5,P6]),
	P_5 is P5 + 1,
	P_6 is P6 + 1,
    replace(Board , P_1 , P_2 , 0 , B),
    move_human_j(P ,B , [P3,P4] , [P_5,P_6] , B1),
    show(B1),
    can_move_again(B1 , P , [P_5,P_6], NextBoard).

can_move_again(Board , P , [P3 , P4],  NextBoard) :-
    can_s(P),
    P_3 is P3 - 1 ,
    P_4 is P4 + 1,
    P_2 is P4 - 1,    
    opp(P , OPP),
    index(Board , P_3 , P_4 , OPP ),
    P7 is P3 - 2,
    P8 is P4 + 2,
    check_boarder(P , P7 , P8 ),
    index(Board , P7 , P8 , 0),
    write('you can move again please enter the next position') , nl,
    read([P5,P6]),
	P_5 is P5 + 1,
	P_6 is P6 + 1,
    replace(Board , P_3 , P_2 , 0 , B ),
    move_human_j(P ,B , [P3,P4] , [P_5,P_6] , B1),
    show(B1),
    can_move_again(B1 , P , [P_5,P_6], NextBoard).

can_move_again(Board , P , [P3 , P4], NextBoard ) :-
    can_n(P),
    P_3 is P3 + 1 ,
    P_4 is P4 - 1,
    P_2 is P3 - 1,    
    opp(P , OPP),
    index(Board , P_3 , P_4 , OPP ),
    P7 is P3 + 2,
    P8 is P4 - 2,
    check_boarder(P , P7 , P8 ),
    index(Board , P7 , P8 , 0),
    write('you can move again please enter the next position') , nl,
    read([P5,P6]),
	P_5 is P5 + 1,
	P_6 is P6 + 1,
    replace(Board , P_2 , P_4 , 0 , B ),   
    move_human_j(P , B , [P3,P4] , [P_5,P_6] , B1),
    show(B1),
    can_move_again(B1 , P , [P_5,P_6], NextBoard).

can_move_again(Board , P , [P3 , P4], NextBoard ) :-
    can_s(P),
    P_3 is P3 - 1 ,
    P_4 is P4 - 1,
    opp(P , OPP),
    index(Board , P_3 , P_4 , OPP ),
    P7 is P3 - 2,
    P8 is P4 - 2,
    index(Board , P7 , P8 , 0),
    check_boarder(P , P7 , P8 ),
    write('you can move again please enter the next position') , nl,
    read([P5,P6]),
	P_5 is P5 + 1,
	P_6 is P6 + 1,
    replace(Board , P_3 , P_4 , 0 , B),
    move_human_j(P , B , [P3,P4] , [P_5,P_6] , B1),
    show(B1),
    can_move_again(B1 , P , [P_5,P_6], NextBoard).

can_move_again(Board, _ , _, Board ):- true.

check_boarder(P , P3 , _ ) :-
    can_n(P),
    P3 < 9.

check_boarder(P , P3 , _ ) :-
    can_s(P),
    P3 > -1.

check_boarder(_ , _ , P4 ) :-
    P4 < 9.

check_boarder(_ , _ , P4 ) :-
    P4 > -1.

check_boarder(_ , _ , _ ).

check_empty(Board , P3 , P4 , P3 , P4) :-
    index(Board , P3 , P4 , 0 ).

check_empty(_ , _ , _ , P_5 , P_6) :-
    write('this position isnot empty, enter again') , nl ,
    read([P5 , P6]),
	P_5 is P5 + 1,
	P_6 is P6 + 1.

move_human(P , Board , [P1 , P2 ] , [P3,P4] , NextBoard) :-
    %check_boarder(P , [P3 , P4 ] , P5 , P6 ),
    is_neighber([P1 , P2 ] , [P3,P4]) ,
    check_empty(Board , P3 , P4 , P5 , P6),
    P_3 is P5 - 1,
    P_4 is P6 - 1,
    replace(Board , P_3 , P_4 ,P , B2),
    be_king(B2 , P , P3 , P4 ,NextBoard).

move_human_j(P , Board , [P1 , P2 ] , [P3,P4] , NextBoard) :-
    %check_boarder(P , [P3 , P4 ] , P5 , P6 ),
    P_m1 is ((P1 + P3) / 2) ,
    P_m2 is ((P2 + P4)/2) ,
    P_m_1 is P_m1 - 1,
    P_m_2 is P_m2 - 1,
    replace(Board , P_m_1 , P_m_2 , 0 , B),
    P_3 is P3 -1 ,
    P_4 is P4 - 1,
    replace(B , P_3 , P_4 , P ,B2),
    be_king(B2 , P , P3 , P4 ,NextBoard)
    .

not_king(x).
not_king(o).
king(x,kx).
king(o,ko).


be_king(Board , P , P1 , P2 ,NextBoard):-
    can_n(P),
    not_king(P),
    P1 is 8 ,
    king(P,KP),
    P_1 is P1 - 1,
    P_2 is P2 - 1,
    replace(Board , P_1 , P_2 , KP , NextBoard).

be_king(Board , P , P1 , P2 ,NextBoard):-
    can_s(P),
    not_king(P),
    P1 is 1 ,
    king(P,KP),
    P_1 is P1 - 1,
    P_2 is P2 - 1,
    replace(Board , P_1 , P_2 , KP , NextBoard).

be_king(Board , _ , _ , _ ,Board).

