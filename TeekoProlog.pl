:- use_module(library(lists)).

% =============================================================================
% État du jeu courant
% =============================================================================

:- dynamic current_game_state/1.

% =============================================================================
% STRUCTURE DE L'ÉTAT DU JEU
% =============================================================================

% game_state(
%     board(Board),           % Plateau 5x5
%     phase(Phase),           % placement ou movement
%     turn(Player),           % player1 ou player2
%     pieces_placed(P1, P2),  % Compteurs de pièces placées
%     history(History),       % Liste des coups joués
%     move_count(Count)       % Nombre total de coups
% )

% =============================================================================
% INITIALISATION
% =============================================================================

init_game(
    game_state(
        board([
            [empty, empty, empty, empty, empty],
            [empty, empty, empty, empty, empty],
            [empty, empty, empty, empty, empty],
            [empty, empty, empty, empty, empty],
            [empty, empty, empty, empty, empty]
        ]),
        phase(placement),
        turn(player1),
        pieces_placed(0, 0),
        history([]),
        move_count(0)
    )
).

% =============================================================================
% ACCESSEURS DE L'ÉTAT DU JEU
% =============================================================================

get_board(game_state(board(Board), _, _, _, _, _), Board).

get_phase(game_state(_, phase(Phase), _, _, _, _), Phase).

get_turn(game_state(_, _, turn(Player), _, _, _), Player).

get_pieces_placed(game_state(_, _, _, pieces_placed(P1, P2), _, _), P1, P2).

get_history(game_state(_, _, _, _, history(History), _), History).

get_move_count(game_state(_, _, _, _, _, move_count(Count)), Count).

get_opponent(player1, player2).
get_opponent(player2, player1).

% =============================================================================
% MANIPULATION DU PLATEAU
% =============================================================================

% piece_at(+GameState, +Row, +Col, ?Piece)
% Accès à une cellule du plateau (coordonnées 1-5)
piece_at(GameState, Row, Col, Piece) :-
    get_board(GameState, Board),
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Piece).

% piece_at avec position
piece_at(GameState, pos(R, C), Piece) :-
    piece_at(GameState, R, C, Piece).

% get_all_pieces(+GameState, +Player, -Positions)
get_all_pieces(GameState, Player, Positions) :-
    get_board(GameState, Board),
    findall(
        pos(R, C),
        (   nth1(R, Board, Row),
            nth1(C, Row, Player)
        ),
        Positions
    ).

% Utilitaire: remplacer le Nième élément d'une liste
replace_nth([H|T], I, X, [H|R]) :- 
    I > 1,
    I1 is I - 1,
    replace_nth(T, I1, X, R).
replace_nth([_|T], 1, X, [X|T]).

% set_cell(+Board, +Row, +Col, +Value, -NewBoard)
% Modifie une cellule du plateau
set_cell(Board, R, C, Val, NewBoard) :-
    nth1(R, Board, Row), 
    replace_nth(Row, C, Val, NewRow),
    replace_nth(Board, R, NewRow, NewBoard).

% =============================================================================
% GESTION DES PHASES ET TOURS
% =============================================================================

current_phase(GameState, Phase) :-
    get_phase(GameState, Phase).

current_player(GameState, Player) :-
    get_turn(GameState, Player).

pieces_placed_count(GameState, Player, Count) :-
    get_pieces_placed(GameState, P1, P2),
    (Player = player1 -> Count = P1 ; Count = P2).

get_remaining_placements(GameState, Player, Count) :-
    pieces_placed_count(GameState, Player, Placed),
    Count is 4 - Placed.

switch_turn(
    game_state(B, P, turn(Player), PC, H, C),
    game_state(B, P, turn(Next), PC, H, C)
) :-
    get_opponent(Player, Next).

% =============================================================================
% VALIDATION DES POSITIONS ET MOUVEMENTS
% =============================================================================

% in_bounds(+Row, +Col)
% Vérifie si une position est dans les limites du plateau
in_bounds(R, C) :- 
    R >= 1, R =< 5, 
    C >= 1, C =< 5.

valid_position(pos(R, C)) :-
    in_bounds(R, C).

% is_valid_placement(+GameState, +Row, +Col)
is_valid_placement(GameState, R, C) :-
    integer(R), integer(C), 
    in_bounds(R, C),
    piece_at(GameState, R, C, empty).

% is_valid_placement avec position
is_valid_placement(GameState, pos(R, C)) :-
    is_valid_placement(GameState, R, C).

% is_adjacent(+(R1,C1), +(R2,C2))
% Vérifie si deux positions sont adjacentes (8 voisins)
is_adjacent((R1, C1), (R2, C2)) :-
    integer(R1), integer(R2), integer(C1), integer(C2),
    DR is abs(R1 - R2), 
    DC is abs(C1 - C2),
    DR =< 1, DC =< 1,
    (DR > 0 ; DC > 0),
    in_bounds(R2, C2).

% is_adjacent_move avec positions
is_adjacent_move(pos(R1, C1), pos(R2, C2)) :-
    DR is abs(R1 - R2),
    DC is abs(C1 - C2),
    DR =< 1,
    DC =< 1,
    (DR + DC) > 0.

% can_move_to(+GameState, +FromPos, +ToPos)
can_move_to(GameState, From, To) :-
    is_adjacent_move(From, To),
    piece_at(GameState, To, empty).

% is_valid_movement(+GameState, +FromPos, +ToPos)
is_valid_movement(GameState, From, To) :-
    current_phase(GameState, movement),
    can_move_to(GameState, From, To).

% =============================================================================
% GÉNÉRATION DES COUPS LÉGAUX
% =============================================================================

% get_moves_for_piece(+GameState, +(R,C), -Moves)
% Retourne tous les mouvements possibles pour une pièce
get_moves_for_piece(GameState, (R, C), Moves) :-
    findall(move(R, C, R2, C2),
        (   between(1, 5, R2), 
            between(1, 5, C2),
            is_adjacent((R, C), (R2, C2)),
            piece_at(GameState, R2, C2, empty)
        ),
        Moves
    ).

% get_movement_moves(+GameState, +Player, -AllMoves)
% Retourne tous les mouvements possibles pour un joueur en phase movement
get_movement_moves(GameState, Player, AllMoves) :-
    findall(move(Player, R, C, TR, TC),
        (   between(1, 5, R), 
            between(1, 5, C),
            piece_at(GameState, R, C, Player),
            get_moves_for_piece(GameState, (R, C), Ms),
            member(move(R, C, TR, TC), Ms)
        ),
        AllMoves
    ).

% get_placement_moves(+GameState, +Player, -Moves)
% Retourne tous les placements possibles en phase placement
get_placement_moves(GameState, Player, Moves) :-
    findall(place(Player, R, C),
        (   between(1, 5, R), 
            between(1, 5, C),
            piece_at(GameState, R, C, empty)
        ),
        Moves
    ).

% get_all_legal_moves(+GameState, +Player, -MovesList)
% Retourne tous les coups légaux pour un joueur
get_all_legal_moves(GameState, Player, Moves) :-
    get_phase(GameState, Phase),
    get_turn(GameState, Player),
    (   Phase = placement ->
        get_placement_moves(GameState, Player, Moves)
    ;   Phase = movement ->
        get_movement_moves(GameState, Player, Moves)
    ;   Moves = []
    ).

% =============================================================================
% EXÉCUTION DES COUPS
% =============================================================================

% place_piece(+GameState, +Player, +Row, +Col, -NewState)
% Place une pièce pendant la phase de placement
place_piece(
    game_state(board(Board), phase(placement), turn(Player), 
               pieces_placed(P1, P2), history(H), move_count(N)),
    Player, R, C,
    game_state(board(NBoard), phase(NewPhase), turn(NextPlayer), 
               pieces_placed(NewP1, NewP2), history(NH), move_count(NN))
) :-
    is_valid_placement(game_state(board(Board), _, _, _, _, _), R, C),
    set_cell(Board, R, C, Player, NBoard),
    NN is N + 1,
    append(H, [place(Player, R, C)], NH),
    (   Player == player1 -> 
        NewP1 is P1 + 1, NewP2 = P2, NextPlayer = player2
    ;   NewP2 is P2 + 1, NewP1 = P1, NextPlayer = player1
    ),
    % Transition vers phase movement si les deux joueurs ont 4 pièces
    (   NewP1 >= 4, NewP2 >= 4 -> 
        NewPhase = movement 
    ;   NewPhase = placement
    ).

% move_piece(+GameState, +Player, +FromR, +FromC, +ToR, +ToC, -NewState)
% Déplace une pièce pendant la phase de mouvement
move_piece(
    game_state(board(Board), phase(movement), turn(Player), 
               pieces_placed(P1, P2), history(H), move_count(N)),
    Player, FromR, FromC, ToR, ToC,
    game_state(board(NB2), phase(movement), turn(NextPlayer), 
               pieces_placed(P1, P2), history(NH), move_count(NN))
) :-
    piece_at(game_state(board(Board), _, _, _, _, _), FromR, FromC, Player),
    piece_at(game_state(board(Board), _, _, _, _, _), ToR, ToC, empty),
    is_adjacent((FromR, FromC), (ToR, ToC)),
    set_cell(Board, FromR, FromC, empty, NB1),
    set_cell(NB1, ToR, ToC, Player, NB2),
    NN is N + 1,
    append(H, [move(Player, FromR, FromC, ToR, ToC)], NH),
    (Player == player1 -> NextPlayer = player2 ; NextPlayer = player1).

% make_move(+GameState, +Move, -NewState)
make_move(GameState, place(Player, R, C), NewState) :-
    place_piece(GameState, Player, R, C, NewState).

make_move(GameState, move(Player, FR, FC, TR, TC), NewState) :-
    move_piece(GameState, Player, FR, FC, TR, TC, NewState).

% =============================================================================
% DÉTECTION DE VICTOIRE
% =============================================================================

% Helper: vérifier un carré 2x2 à partir de (R,C)
check_square(Board, Player, R, C) :-
    R2 is R + 1, C2 is C + 1,
    R >= 1, R2 =< 5, C >= 1, C2 =< 5,
    nth1(R, Board, Row1), 
    nth1(C, Row1, A), nth1(C2, Row1, B),
    nth1(R2, Board, Row2), 
    nth1(C, Row2, C1), nth1(C2, Row2, D),
    A == Player, B == Player, C1 == Player, D == Player.

% check_four_in_square(+GameState, +Player)
% Vérifie si le joueur a formé un carré 2x2
check_four_in_square(game_state(board(Board), _, _, _, _, _), Player) :-
    between(1, 4, R), 
    between(1, 4, C),
    check_square(Board, Player, R, C), !.

% Helper: compter les pièces consécutives dans une direction
count_dir(Board, Player, R, C, DR, DC, Count) :-
    (   R < 1 ; R > 5 ; C < 1 ; C > 5 
    ) -> 
        Count = 0 
    ;   nth1(R, Board, Row), 
        nth1(C, Row, Cell),
        (   Cell == Player ->
            R2 is R + DR, 
            C2 is C + DC,
            count_dir(Board, Player, R2, C2, DR, DC, C2Count),
            Count is C2Count + 1
        ;   Count = 0
        ).

% check_four_in_line(+GameState, +Player)
% Vérifie si le joueur a aligné 4 pièces (horizontal, vertical, diagonal)
check_four_in_line(game_state(board(Board), _, _, _, _, _), Player) :-
    between(1, 5, R), 
    between(1, 5, C),
    (   count_dir(Board, Player, R, C, 0, 1, N1), N1 >= 4
    ;   count_dir(Board, Player, R, C, 1, 0, N2), N2 >= 4
    ;   count_dir(Board, Player, R, C, 1, 1, N3), N3 >= 4
    ;   count_dir(Board, Player, R, C, 1, -1, N4), N4 >= 4
    ), !.

check_win(GameState, Player) :-
    check_four_in_line(GameState, Player), !.
check_win(GameState, Player) :-
    check_four_in_square(GameState, Player), !.

game_status(GameState, winner(Player)) :-
    check_win(GameState, Player), !.
game_status(_, playing).

% =============================================================================
% HEURISTIQUE D'ÉVALUATION
% =============================================================================
% Convention: score positif = favorable à player2 (IA)
%             score négatif = favorable à player1 (humain)
% =============================================================================

% Poids pour l'heuristique
% IMPORTANT: Les menaces adverses ont un poids PLUS ÉLEVÉ (défense > attaque)
weight_win(100000).

% Poids OFFENSIFS (pour l'IA)
weight_three_in_line_attack(500).
weight_two_in_line_attack(20).
weight_three_in_square_attack(400).
weight_two_in_square_attack(15).

% Poids DÉFENSIFS (menaces adverses) - PLUS ÉLEVÉS !
weight_three_in_line_defense(2000).    % Menace immédiate = très dangereux
weight_two_in_line_defense(50).
weight_three_in_square_defense(1500).
weight_two_in_square_defense(40).

% Poids positionnels (contrôle du centre)
weight_center(20).              % Case centrale (3,3)
weight_adjacent_center(10).     % Cases adjacentes au centre

% evaluer_plateau(+GameState, -Score)
% Fonction principale d'évaluation
evaluer_plateau(GameState, Score) :-
    % Vérifier victoire
    (   check_win(GameState, player2) 
    ->  weight_win(W), Score = W
    ;   check_win(GameState, player1)
    ->  weight_win(W), Score is -W
    ;   % D'abord vérifier les menaces IMMÉDIATES (priorité absolue)
        detect_immediate_threats(GameState, player1, ThreatCount1),
        detect_immediate_threats(GameState, player2, ThreatCount2),
        
        % Calculer score heuristique avec poids asymétriques
        eval_lignes_attack(GameState, player2, ScoreLignesP2),
        eval_lignes_defense(GameState, player1, ScoreLignesP1),
        eval_carres_attack(GameState, player2, ScoreCarresP2),
        eval_carres_defense(GameState, player1, ScoreCarresP1),
        eval_position(GameState, player2, ScorePosP2),
        eval_position(GameState, player1, ScorePosP1),
        
        % Les menaces immédiates ont un impact ÉNORME
        ThreatPenalty is ThreatCount1 * 5000,  % Pénalité si adversaire menace
        ThreatBonus is ThreatCount2 * 3000,    % Bonus si on menace
        
        Score is (ScoreLignesP2 - ScoreLignesP1) + 
                 (ScoreCarresP2 - ScoreCarresP1) + 
                 (ScorePosP2 - ScorePosP1) +
                 ThreatBonus - ThreatPenalty
    ).

% =============================================================================
% DÉTECTION DES MENACES IMMÉDIATES
% =============================================================================
% Une menace immédiate = 3 pièces alignées (ou en carré) avec 1 case vide
% Si l'adversaire a ça, il peut gagner au prochain coup !

detect_immediate_threats(GameState, Player, Count) :-
    all_lines(Lines),
    all_squares(Squares),
    append(Lines, Squares, AllFormations),
    findall(1, (
        member(Formation, AllFormations),
        is_immediate_threat(GameState, Formation, Player)
    ), Threats),
    length(Threats, Count).

% Vérifie si une formation est une menace immédiate pour Player
is_immediate_threat(GameState, Formation, Player) :-
    count_in_line(GameState, Formation, Player, 3, 1),  % Exactement 3 pièces + 1 vide
    get_opponent(Player, Opponent),
    count_in_line(GameState, Formation, Opponent, 0, _).  % Pas bloqué par l'adversaire

% =============================================================================
% Évaluation des lignes (horizontales, verticales, diagonales)
% =============================================================================

% Toutes les lignes gagnantes possibles (4 cases consécutives)
all_lines(Lines) :-
    % Lignes horizontales
    findall(Line, 
        (between(1, 5, R), between(1, 2, C), 
         C2 is C+1, C3 is C+2, C4 is C+3,
         Line = [pos(R,C), pos(R,C2), pos(R,C3), pos(R,C4)]),
        HLines),
    % Lignes verticales
    findall(Line,
        (between(1, 2, R), between(1, 5, C),
         R2 is R+1, R3 is R+2, R4 is R+3,
         Line = [pos(R,C), pos(R2,C), pos(R3,C), pos(R4,C)]),
        VLines),
    % Diagonales descendantes (\)
    findall(Line,
        (between(1, 2, R), between(1, 2, C),
         R2 is R+1, C2 is C+1, R3 is R+2, C3 is C+2, R4 is R+3, C4 is C+3,
         Line = [pos(R,C), pos(R2,C2), pos(R3,C3), pos(R4,C4)]),
        D1Lines),
    % Diagonales montantes (/)
    findall(Line,
        (between(4, 5, R), between(1, 2, C),
         R2 is R-1, C2 is C+1, R3 is R-2, C3 is C+2, R4 is R-3, C4 is C+3,
         Line = [pos(R,C), pos(R2,C2), pos(R3,C3), pos(R4,C4)]),
        D2Lines),
    append([HLines, VLines, D1Lines, D2Lines], Lines).

% Compter les pièces d'un joueur et les cases vides dans une ligne
count_in_line(_, [], _, 0, 0).
count_in_line(GameState, [pos(R,C)|Rest], Player, PlayerCount, EmptyCount) :-
    count_in_line(GameState, Rest, Player, RestPlayer, RestEmpty),
    piece_at(GameState, R, C, Piece),
    (   Piece == Player 
    ->  PlayerCount is RestPlayer + 1, EmptyCount = RestEmpty
    ;   Piece == empty
    ->  EmptyCount is RestEmpty + 1, PlayerCount = RestPlayer
    ;   PlayerCount = RestPlayer, EmptyCount = RestEmpty
    ).

% Évaluer une ligne pour un joueur (MODE ATTAQUE - nos pièces)
eval_single_line_attack(GameState, Line, Player, Score) :-
    count_in_line(GameState, Line, Player, PCount, ECount),
    get_opponent(Player, Opponent),
    count_in_line(GameState, Line, Opponent, OppCount, _),
    (   OppCount > 0 
    ->  Score = 0  % Ligne bloquée par l'adversaire
    ;   PCount = 3, ECount >= 1
    ->  weight_three_in_line_attack(W), Score = W
    ;   PCount = 2, ECount >= 2
    ->  weight_two_in_line_attack(W), Score = W
    ;   Score = 0
    ).

% Évaluer une ligne pour un joueur (MODE DÉFENSE - pièces adverses = menace)
eval_single_line_defense(GameState, Line, Player, Score) :-
    count_in_line(GameState, Line, Player, PCount, ECount),
    get_opponent(Player, Opponent),
    count_in_line(GameState, Line, Opponent, OppCount, _),
    (   OppCount > 0 
    ->  Score = 0  % Ligne bloquée, pas de menace
    ;   PCount = 3, ECount >= 1
    ->  weight_three_in_line_defense(W), Score = W  % MENACE CRITIQUE !
    ;   PCount = 2, ECount >= 2
    ->  weight_two_in_line_defense(W), Score = W
    ;   Score = 0
    ).

% Évaluer toutes les lignes (attaque)
eval_lignes_attack(GameState, Player, TotalScore) :-
    all_lines(Lines),
    findall(S, (member(Line, Lines), eval_single_line_attack(GameState, Line, Player, S)), Scores),
    sum_list(Scores, TotalScore).

% Évaluer toutes les lignes (défense)
eval_lignes_defense(GameState, Player, TotalScore) :-
    all_lines(Lines),
    findall(S, (member(Line, Lines), eval_single_line_defense(GameState, Line, Player, S)), Scores),
    sum_list(Scores, TotalScore).

% =============================================================================
% Évaluation des carrés 2x2
% =============================================================================

% Tous les carrés 2x2 possibles
all_squares(Squares) :-
    findall(Square,
        (between(1, 4, R), between(1, 4, C),
         R2 is R+1, C2 is C+1,
         Square = [pos(R,C), pos(R,C2), pos(R2,C), pos(R2,C2)]),
        Squares).

% Évaluer un carré pour un joueur (MODE ATTAQUE)
eval_single_square_attack(GameState, Square, Player, Score) :-
    count_in_line(GameState, Square, Player, PCount, ECount),
    get_opponent(Player, Opponent),
    count_in_line(GameState, Square, Opponent, OppCount, _),
    (   OppCount > 0
    ->  Score = 0  % Carré bloqué
    ;   PCount = 3, ECount >= 1
    ->  weight_three_in_square_attack(W), Score = W
    ;   PCount = 2, ECount >= 2
    ->  weight_two_in_square_attack(W), Score = W
    ;   Score = 0
    ).

% Évaluer un carré pour un joueur (MODE DÉFENSE)
eval_single_square_defense(GameState, Square, Player, Score) :-
    count_in_line(GameState, Square, Player, PCount, ECount),
    get_opponent(Player, Opponent),
    count_in_line(GameState, Square, Opponent, OppCount, _),
    (   OppCount > 0
    ->  Score = 0  % Carré bloqué, pas de menace
    ;   PCount = 3, ECount >= 1
    ->  weight_three_in_square_defense(W), Score = W  % MENACE CRITIQUE !
    ;   PCount = 2, ECount >= 2
    ->  weight_two_in_square_defense(W), Score = W
    ;   Score = 0
    ).

% Évaluer tous les carrés (attaque)
eval_carres_attack(GameState, Player, TotalScore) :-
    all_squares(Squares),
    findall(S, (member(Sq, Squares), eval_single_square_attack(GameState, Sq, Player, S)), Scores),
    sum_list(Scores, TotalScore).

% Évaluer tous les carrés (défense)
eval_carres_defense(GameState, Player, TotalScore) :-
    all_squares(Squares),
    findall(S, (member(Sq, Squares), eval_single_square_defense(GameState, Sq, Player, S)), Scores),
    sum_list(Scores, TotalScore).

% =============================================================================
% Évaluation positionnelle (contrôle du centre)
% =============================================================================

% Cases centrales et leur valeur
center_value(3, 3, V) :- weight_center(V).
center_value(2, 2, V) :- weight_adjacent_center(V).
center_value(2, 3, V) :- weight_adjacent_center(V).
center_value(2, 4, V) :- weight_adjacent_center(V).
center_value(3, 2, V) :- weight_adjacent_center(V).
center_value(3, 4, V) :- weight_adjacent_center(V).
center_value(4, 2, V) :- weight_adjacent_center(V).
center_value(4, 3, V) :- weight_adjacent_center(V).
center_value(4, 4, V) :- weight_adjacent_center(V).
center_value(_, _, 0).

% Évaluer la position des pièces d'un joueur
eval_position(GameState, Player, TotalScore) :-
    get_all_pieces(GameState, Player, Pieces),
    findall(V, 
        (member(pos(R,C), Pieces), center_value(R, C, V)), 
        Values),
    sum_list(Values, TotalScore).

% =============================================================================
% ALGORITHME MINIMAX AVEC ALPHA-BETA
% =============================================================================

% minimax(+GameState, +Profondeur, +Alpha, +Beta, +Maximisant, -MeilleurCoup, -MeilleureValeur)
%
% Maximisant = true  -> on maximise (tour de player2/IA)
% Maximisant = false -> on minimise (tour de player1/humain)

% Cas terminal : victoire player2 (IA)
minimax(GameState, _, _, _, _, nil, Score) :-
    check_win(GameState, player2), !,
    weight_win(Score).

% Cas terminal : victoire player1 (humain)  
minimax(GameState, _, _, _, _, nil, Score) :-
    check_win(GameState, player1), !,
    weight_win(W),
    Score is -W.

% Cas terminal : profondeur 0
minimax(GameState, 0, _, _, _, nil, Score) :- !,
    evaluer_plateau(GameState, Score).

% Cas récursif : maximisant (player2/IA joue)
minimax(GameState, Prof, Alpha, Beta, true, MeilleurCoup, MeilleureVal) :-
    Prof > 0,
    get_turn(GameState, Player),
    get_all_legal_moves(GameState, Player, Moves),
    Moves \= [], !,
    Prof1 is Prof - 1,
    maximize(GameState, Moves, Prof1, Alpha, Beta, nil, -100000, MeilleurCoup, MeilleureVal).

% Cas récursif : minimisant (player1/humain joue)
minimax(GameState, Prof, Alpha, Beta, false, MeilleurCoup, MeilleureVal) :-
    Prof > 0,
    get_turn(GameState, Player),
    get_all_legal_moves(GameState, Player, Moves),
    Moves \= [], !,
    Prof1 is Prof - 1,
    minimize(GameState, Moves, Prof1, Alpha, Beta, nil, 100000, MeilleurCoup, MeilleureVal).

% Pas de coups disponibles -> évaluer la position
minimax(GameState, _, _, _, _, nil, Score) :-
    evaluer_plateau(GameState, Score).

% =============================================================================
% Maximisation (tour de l'IA - player2)
% =============================================================================

% Plus de coups à évaluer
maximize(_, [], _, _, _, BestMove, BestVal, BestMove, BestVal) :- !.

% Élagage beta
maximize(_, _, _, Alpha, Beta, BestMove, BestVal, BestMove, BestVal) :-
    BestVal >= Beta, !.

% Évaluer le prochain coup
maximize(GameState, [Coup|Reste], Prof, Alpha, Beta, CoupAcc, ValAcc, MeilleurCoup, MeilleureVal) :-
    make_move(GameState, Coup, NouvelEtat),
    minimax(NouvelEtat, Prof, Alpha, Beta, false, _, Val),
    (   Val > ValAcc
    ->  NewCoup = Coup, NewVal = Val
    ;   NewCoup = CoupAcc, NewVal = ValAcc
    ),
    NewAlpha is max(Alpha, NewVal),
    maximize(GameState, Reste, Prof, NewAlpha, Beta, NewCoup, NewVal, MeilleurCoup, MeilleureVal).

% =============================================================================
% Minimisation (tour du joueur humain - player1)
% =============================================================================

% Plus de coups à évaluer
minimize(_, [], _, _, _, BestMove, BestVal, BestMove, BestVal) :- !.

% Élagage alpha
minimize(_, _, _, Alpha, _, BestMove, BestVal, BestMove, BestVal) :-
    BestVal =< Alpha, !.

% Évaluer le prochain coup
minimize(GameState, [Coup|Reste], Prof, Alpha, Beta, CoupAcc, ValAcc, MeilleurCoup, MeilleureVal) :-
    make_move(GameState, Coup, NouvelEtat),
    minimax(NouvelEtat, Prof, Alpha, Beta, true, _, Val),
    (   Val < ValAcc
    ->  NewCoup = Coup, NewVal = Val
    ;   NewCoup = CoupAcc, NewVal = ValAcc
    ),
    NewBeta is min(Beta, NewVal),
    minimize(GameState, Reste, Prof, Alpha, NewBeta, NewCoup, NewVal, MeilleurCoup, MeilleureVal).

% ==========================================
===================================
% INTERFACE PRINCIPALE POUR L'IA
% =============================================================================
% helper : ajuste la profondeur en fonction de la phase du jeu (pour réduire le temps de calcul)
get_adaptive_depth(GameState, Depth) :-
    get_phase(GameState, Phase),
    get_pieces_placed(GameState, P1, P2),
    Total is P1 + P2,
    (   Phase = placement, Total < 4 
    ->  Depth = 2    % Début de partie : peu profond
    ;   Phase = placement 
    ->  Depth = 4    % Milieu placement
    ;   Depth = 5    % Phase mouvement : plus profond
    ).

% get_best_move(+GameState, -Move)
% Retourne le meilleur coup pour le joueur courant
get_best_move(GameState, Move) :-
    get_turn(GameState, Player),
    get_all_legal_moves(GameState, Player, Moves),
    Moves \= [],
    get_adaptive_depth(GameState, Profondeur),
    
    % D'abord, vérifier s'il y a un coup gagnant immédiat
    (   find_winning_move(GameState, Player, Moves, WinMove)
    ->  Move = WinMove
    ;   % Sinon, vérifier s'il faut bloquer une menace immédiate
        get_opponent(Player, Opponent),
        (   find_blocking_move(GameState, Opponent, Moves, BlockMove)
        ->  Move = BlockMove
        ;   % Sinon, utiliser minimax avec coups triés
            order_moves(GameState, Player, Moves, OrderedMoves),
            (Player = player2 -> Maximisant = true ; Maximisant = false),
            minimax(GameState, Profondeur, -200000, 200000, Maximisant, Move, _Score),
            Move \= nil
        )
    ), !.

% =============================================================================
% DÉTECTION DES COUPS CRITIQUES
% =============================================================================

% Trouve un coup gagnant immédiat s'il existe
find_winning_move(GameState, Player, Moves, WinMove) :-
    member(WinMove, Moves),
    make_move(GameState, WinMove, NewState),
    check_win(NewState, Player), !.

% Trouve un coup qui bloque une menace immédiate de l'adversaire
find_blocking_move(GameState, Opponent, Moves, BlockMove) :-
    % Trouver les cases où l'adversaire pourrait gagner
    get_all_legal_moves(GameState, Opponent, OppMoves),
    member(OppWinMove, OppMoves),
    make_move(GameState, OppWinMove, TestState),
    check_win(TestState, Opponent), !,
    % Trouver notre coup qui bloque cette case
    get_blocking_position(OppWinMove, BlockPos),
    member(BlockMove, Moves),
    get_move_target(BlockMove, BlockPos), !.

% Extraire la position cible d'un coup adverse
get_blocking_position(place(_, R, C), pos(R, C)).
get_blocking_position(move(_, _, _, R, C), pos(R, C)).

% Vérifier si notre coup cible une position donnée
get_move_target(place(_, R, C), pos(R, C)).
get_move_target(move(_, _, _, R, C), pos(R, C)).

% =============================================================================
% TRI DES COUPS (MOVE ORDERING)
% =============================================================================
% Trier les coups pour améliorer l'élagage alpha-beta :
% 1. Coups au centre (plus de potentiel)
% 2. Coups adjacents à nos pièces
% 3. Autres coups

order_moves(GameState, Player, Moves, OrderedMoves) :-
    map_move_scores(GameState, Player, Moves, ScoredMoves),
    sort(1, @>=, ScoredMoves, SortedScored),
    pairs_values(SortedScored, OrderedMoves).

map_move_scores(_, _, [], []).
map_move_scores(GameState, Player, [Move|Rest], [Score-Move|RestScored]) :-
    score_move_priority(GameState, Player, Move, Score),
    map_move_scores(GameState, Player, Rest, RestScored).

% Calculer la priorité d'un coup pour le tri
score_move_priority(GameState, Player, Move, Score) :-
    get_move_target(Move, pos(R, C)),
    % Bonus centre
    (   (R = 3, C = 3) -> CenterScore = 50
    ;   (R >= 2, R =< 4, C >= 2, C =< 4) -> CenterScore = 20
    ;   CenterScore = 0
    ),
    % Bonus si le coup crée une menace
    (   make_move(GameState, Move, NewState),
        detect_immediate_threats(NewState, Player, TC),
        TC > 0
    ->  ThreatScore = 100
    ;   ThreatScore = 0
    ),
    % Bonus si adjacent à nos pièces
    (   is_adjacent_to_own_piece(GameState, Player, pos(R,C))
    ->  AdjScore = 30
    ;   AdjScore = 0
    ),
    Score is CenterScore + ThreatScore + AdjScore.

% Vérifier si une position est adjacente à une de nos pièces
is_adjacent_to_own_piece(GameState, Player, pos(R, C)) :-
    get_all_pieces(GameState, Player, Pieces),
    member(pos(PR, PC), Pieces),
    DR is abs(R - PR),
    DC is abs(C - PC),
    DR =< 1, DC =< 1,
    (DR > 0 ; DC > 0), !.

% =============================================================================
% UTILITAIRES D'AFFICHAGE
% =============================================================================

print_cell(empty) :- write('. ').
print_cell(player1) :- write('X ').
print_cell(player2) :- write('O ').

print_row([]) :- nl.
print_row([C|R]) :- 
    print_cell(C), 
    print_row(R).

print_board(game_state(board(B), phase(P), turn(T), pieces_placed(P1, P2), _, _)) :-
    format("Phase: ~w  Turn: ~w  Pieces: p1=~w p2=~w~n", [P, T, P1, P2]),
    write("  1 2 3 4 5"), nl,
    print_board_rows(B, 1).

print_board_rows([], _).
print_board_rows([Row|Rest], N) :-
    write(N), write(' '),
    print_row(Row),
    N2 is N + 1,
    print_board_rows(Rest, N2).

board_to_string(GameState, String) :-
    get_board(GameState, Board),
    with_output_to(string(String),
        forall(member(Row, Board),
            (write(Row), nl)
        )
    ).

% =============================================================================
% VALIDATION DE L'ÉTAT
% =============================================================================

is_valid_game_state(GameState) :-
    get_board(GameState, Board),
    length(Board, 5),
    maplist(length_is_5, Board),
    get_pieces_placed(GameState, P1Count, P2Count),
    P1Count =< 4, 
    P2Count =< 4.

length_is_5(Row) :- length(Row, 5).

print_game_state(GameState) :-
    print_board(GameState),
    get_phase(GameState, Phase),
    get_turn(GameState, Turn),
    get_move_count(GameState, MoveCount),
    format("Phase: ~w, Turn: ~w, Move count: ~w~n", [Phase, Turn, MoveCount]).

% =============================================================================
% COMMUNICATION IPC
% =============================================================================
% Format: COMMANDE [args...]
% Réponses: OK TYPE [args...] ou ERROR message
% Note: Les coordonnées sont converties 0-4 (Qt) <-> 1-5 (Prolog)
% =============================================================================

main :-
    retractall(current_game_state(_)),
    set_stream(user_output, buffer(line)),
    communication_loop.

communication_loop :-
    read_line_to_string(user_input, Line),
    (   Line = end_of_file ->
        true
    ;   catch(
            process_line(Line),
            Error,
            send_error(Error)
        ),
        communication_loop
    ).

process_line(Line) :-
    normalize_space(atom(Normalized), Line),
    atom_string(Normalized, NormStr),
    split_string(NormStr, " ", "", Parts),
    Parts = [CmdStr|Args],
    string_upper(CmdStr, CmdUpper),
    handle_command(CmdUpper, Args).

% =============================================================================
% GESTIONNAIRES DE COMMANDES
% =============================================================================

% INIT - Initialiser
handle_command("INIT", _) :- !,
    format("Initializing new game...~n", []),
    init_game(GameState),
    retractall(current_game_state(_)),
    assertz(current_game_state(GameState)),
    send_response("OK READY").

% NEWGAME - Nouvelle partie
handle_command("NEWGAME", _) :- !,
    init_game(GameState),
    retractall(current_game_state(_)),
    assertz(current_game_state(GameState)),
    send_response("OK ACK").

% DROP row col - Le joueur place une pièce (coordonnées 0-4 depuis Qt)
handle_command("DROP", [RowStr, ColStr]) :- !,
    number_string(Row0, RowStr),
    number_string(Col0, ColStr),
    Row is Row0 + 1,
    Col is Col0 + 1,
    current_game_state(GameState),
    get_turn(GameState, Player),
    Move = place(Player, Row, Col),
    (   make_move(GameState, Move, NewState)->
        retractall(current_game_state(_)),
        assertz(current_game_state(NewState)),
        send_response("OK ACK")
    ;   send_response("ERROR Invalid drop position")
    )
    
    .

% MOVE fromRow fromCol toRow toCol - Le joueur déplace (coordonnées 0-4)
handle_command("MOVE", [FromRowStr, FromColStr, ToRowStr, ToColStr]) :- !,
    number_string(FromRow0, FromRowStr),
    number_string(FromCol0, FromColStr),
    number_string(ToRow0, ToRowStr),
    number_string(ToCol0, ToColStr),
    FromRow is FromRow0 + 1,
    FromCol is FromCol0 + 1,
    ToRow is ToRow0 + 1,
    ToCol is ToCol0 + 1,
    current_game_state(GameState),
    get_turn(GameState, Player),
    Move = move(Player, FromRow, FromCol, ToRow, ToCol),
    (   make_move(GameState, Move, NewState) ->
        retractall(current_game_state(_)),
        assertz(current_game_state(NewState)),
        send_response("OK ACK")
    ;   send_response("ERROR Invalid move")
    ).

handle_command("GETMOVE", _) :- !,
    current_game_state(GameState),
    get_phase(GameState, Phase),
    (   get_best_move(GameState, Move) ->
        (   Phase = placement ->
            Move = place(_, Row, Col),
            make_move(GameState, Move, NewState),
            retractall(current_game_state(_)),
            assertz(current_game_state(NewState)),
            Row0 is Row - 1,
            Col0 is Col - 1,
            format(string(Response), "OK DROP ~d ~d", [Row0, Col0]),
            send_response(Response)
        ;   Move = move(_, FromRow, FromCol, ToRow, ToCol),
            make_move(GameState, Move, NewState),
            retractall(current_game_state(_)),
            assertz(current_game_state(NewState)),
            FromRow0 is FromRow - 1,
            FromCol0 is FromCol - 1,
            ToRow0 is ToRow - 1,
            ToCol0 is ToCol - 1,
            format(string(Response), "OK MOVE ~d ~d ~d ~d", 
                   [FromRow0, FromCol0, ToRow0, ToCol0]),
            send_response(Response)
        )
    ;   send_response("ERROR No valid move found")
    ).

% STATUS - Obtenir le statut
handle_command("STATUS", _) :- !,
    current_game_state(GameState),
    get_phase(GameState, Phase),
    get_move_count(GameState, Count),
    game_status(GameState, Status),
    format(string(Response), "OK STATUS ~w ~d ~w", [Phase, Count, Status]),
    send_response(Response).

% QUIT - Quitter
handle_command("QUIT", _) :- !,
    send_response("OK BYE"),
    halt(0).

% Commande inconnue
handle_command(Cmd, _) :-
    format(string(Msg), "ERROR Unknown command: ~w", [Cmd]),
    send_response(Msg).

% =============================================================================
% ENVOI DES RÉPONSES
% =============================================================================

send_response(Response) :-
    writeln(Response),
    flush_output.

send_error(Error) :-
    term_string(Error, ErrorStr),
    format(string(Msg), "ERROR ~w", [ErrorStr]),
    send_response(Msg).

% =============================================================================
% TESTS ET DEBUG
% =============================================================================

% test_ia/0 - Tester l'IA sur une position initiale
test_ia :-
    init_game(G),
    writeln("=== Test IA ==="),
    print_board(G),
    writeln("Recherche du meilleur coup..."),
    get_best_move(G, Move),
    format("Meilleur coup trouvé: ~w~n", [Move]).

% test_eval/0 - Tester l'évaluation sur différentes positions
test_eval :-
    init_game(G),
    evaluer_plateau(G, Score),
    format("Score position initiale: ~w~n", [Score]).

% test_blocking/0 - Tester si l'IA bloque bien les menaces
test_blocking :-
    writeln("=== Test Blocage de Menace ==="),
    % Créer une position où player1 a 3 pièces alignées
    init_game(G0),
    make_move(G0, place(player1, 1, 1), G1),
    make_move(G1, place(player2, 3, 3), G2),
    make_move(G2, place(player1, 1, 2), G3),
    make_move(G3, place(player2, 4, 4), G4),
    make_move(G4, place(player1, 1, 3), G5),  % player1 a 3 en ligne !
    make_move(G5, place(player2, 5, 5), G6),
    % Maintenant c'est à player1 de placer sa 4ème pièce
    % Après, player2 devrait bloquer (1,4) pour éviter la victoire
    make_move(G6, place(player1, 2, 2), G7),  % player1 place ailleurs
    
    writeln("Position avec menace de player1 (ligne 1):"),
    print_board(G7),
    
    % Vérifier les menaces détectées
    detect_immediate_threats(G7, player1, Threats),
    format("Menaces immédiates de player1: ~w~n", [Threats]),
    
    evaluer_plateau(G7, Score),
    format("Score du plateau: ~w~n", [Score]),
    
    writeln("IA (player2) devrait bloquer en (1,4)..."),
    get_best_move(G7, Move),
    format("Coup choisi par IA: ~w~n", [Move]),
    
    % Vérifier si c'est le bon coup
    (   Move = place(player2, 1, 4)
    ->  writeln("SUCCES: menace bloquée!")
    ;   writeln("ECHEC: menace non bloquée!")).

% test_winning/0 - Tester si l'IA prend un coup gagnant
test_winning :-
    writeln("=== Test Coup Gagnant ==="),
    init_game(G0),
    make_move(G0, place(player1, 5, 5), G1),
    make_move(G1, place(player2, 1, 1), G2),
    make_move(G2, place(player1, 5, 4), G3),
    make_move(G3, place(player2, 1, 2), G4),
    make_move(G4, place(player1, 5, 3), G5),
    make_move(G5, place(player2, 1, 3), G6),  % player2 a 3 en ligne !
    make_move(G6, place(player1, 4, 4), G7),
    % player2 devrait placer sa 4ème pièce en (1,4) pour gagner
    make_move(G7, place(player2, 2, 2), G8),  % player2 place sa 4ème ailleurs
    
    % Maintenant c'est à player1, puis player2 devrait chercher à gagner
    make_move(G8, place(player1, 3, 3), G9),
    
    writeln("Position où player2 peut aligner:"),
    print_board(G9),
    
    % En phase mouvement, tester si l'IA trouve le coup gagnant
    writeln("IA cherche le meilleur coup..."),
    get_best_move(G9, Move),
    format("Coup choisi: ~w~n", [Move]).


% simulate_game/0 - Simuler quelques coups
simulate_game :-
    init_game(G0),
    writeln("Position initiale:"),
    print_board(G0),
    
    % Player1 place en (3,3)
    make_move(G0, place(player1, 3, 3), G1),
    writeln("\nAprès player1 place (3,3):"),
    print_board(G1),
    evaluer_plateau(G1, S1),
    format("Score: ~w~n", [S1]),
    
    % IA joue
    get_best_move(G1, Move2),
    format("\nIA choisit: ~w~n", [Move2]),
    make_move(G1, Move2, G2),
    print_board(G2),
    evaluer_plateau(G2, S2),
    format("Score: ~w~n", [S2]).