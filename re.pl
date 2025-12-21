:- use_module(library(lists)).

% =============================================================================
% État du jeu courant
% =============================================================================

:- dynamic current_game_state/1.

% =============================================================================
% STRUCTURE DE LTAT DU JEU
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

% board_to_string(+GameState, -String)
% Convertit le plateau en chaîne pour affichage
board_to_string(GameState, String) :-
    get_board(GameState, Board),
    with_output_to(string(String),
        forall(member(Row, Board),
            (write(Row), nl)
        )
    ).

get_board_as_matrix(GameState, Matrix) :-
    get_board(GameState, Matrix).

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
    GameState = game_state(_, phase(Phase), turn(Player), _, _, _),
    (   Phase = placement ->
        get_placement_moves(GameState, Player, Moves)
    ;   Phase = movement ->
        get_movement_moves(GameState, Player, Moves)
    ;   Moves = []
    ).

% =============================================================================
% VALIDATION DÉTAILLÉE DES COUPS
% =============================================================================

% validate_move_detailed(+GameState, +Move, -Result)
% Result = ok(Message) ou error(Message)
validate_move_detailed(GameState, place(Player, R, C), ok('placement ok')) :-
    GameState = game_state(_, phase(placement), turn(Player), _, _, _),
    is_valid_placement(GameState, R, C), !.

validate_move_detailed(game_state(_, phase(Phase), turn(Turn), _, _, _), 
                       place(Player, _, _), 
                       error('not in placement phase or not your turn')) :-
    (Phase \== placement ; Player \== Turn), !.

validate_move_detailed(_, place(_, _, _), error('invalid placement: cell occupied or out of bounds')).

validate_move_detailed(GameState, move(Player, FR, FC, TR, TC), ok('movement ok')) :-
    GameState = game_state(_, phase(movement), turn(Player), _, _, _),
    piece_at(GameState, FR, FC, Player),
    piece_at(GameState, TR, TC, empty),
    is_adjacent((FR, FC), (TR, TC)), !.

validate_move_detailed(game_state(_, phase(Phase), turn(Turn), _, _, _), 
                       move(Player, _, _, _, _), 
                       error('not in movement phase or not your turn')) :-
    (Phase \== movement ; Player \== Turn), !.

validate_move_detailed(_, move(_, _, _, _, _), 
                       error('invalid movement (source not yours / dest not empty / not adjacent)')).

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
    (   count_dir(Board, Player, R, C, 0, 1, N1), N1 >= 4    % horizontal
    ;   count_dir(Board, Player, R, C, 1, 0, N2), N2 >= 4    % vertical
    ;   count_dir(Board, Player, R, C, 1, 1, N3), N3 >= 4    % diag down-right
    ;   count_dir(Board, Player, R, C, 1, -1, N4), N4 >= 4   % diag down-left
    ), !.

% game_status(+GameState, ?Status)
% Retourne winner(Player) si victoire, sinon playing
game_status(GameState, winner(Player)) :-
    check_four_in_line(GameState, Player), !.
game_status(GameState, winner(Player)) :-
    check_four_in_square(GameState, Player), !.
game_status(_, playing).

% evalPlateau(+GameState,+Joueur, -Val)
%retour 1 si le plateau est une victoire pour l'IA
%retour -1 si le plateau est une victoire pour le joueur
evalPlateau(GameState, player1,-1) :- !,
    check_four_in_line(GameState, player1); check_four_in_square(GameState, player1).
evalPlateau(GameState, player2,1):-!, check_four_in_line(GameState, player2); check_four_in_square(GameState, player2).
     

%comparerCoups(+Coup1,+Val1,+Coup2,+Val2,+Joueur,-MCoup,MVal)
%retour le coup avec le meilleur valeur
%p1 j, p2 IA

comparerCoups(C1,V1,_,V2,player2,C1,V1):- V1>=V2.
comparerCoups(_,V1,C2,V2,player2,C2,V2):- V2>V1. 
comparerCoups(C1,V1,_,V2,player1,C1,V1):- V1=<V2.
comparerCoups(_,V1,C2,V2,player1,C2,V2):- V2<V1.



% =============================================================================
% UTILITAIRES DAFFICHAGE
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

% =============================================================================
% SÉRIALISATION
% =============================================================================

% serialize_state(+GameState, -String)
serialize_state(GameState, String) :-
    term_string(GameState, String).

% deserialize_state(+String, -GameState)
deserialize_state(String, GameState) :-
    term_string(GameState, String).

% save_game_to_file(+GameState, +Filename)
save_game_to_file(GameState, Filename) :-
    serialize_state(GameState, String),
    open(Filename, write, Stream),
    write(Stream, String),
    close(Stream).

% load_game_from_file(+Filename, -GameState)
load_game_from_file(Filename, GameState) :-
    open(Filename, read, Stream),
    read_string(Stream, _, String),
    close(Stream),
    deserialize_state(String, GameState).

% =============================================================================
% INFORMATIONS DU JEU
% =============================================================================

get_board_dimensions(5, 5).

get_pieces_per_player(4).

get_winning_formation_size(4).

get_game_rules(Rules) :-
    Rules = "Teeko est un jeu à deux joueurs sur un plateau 5x5. 
             Chaque joueur dispose de 4 pièces. 
             Le jeu comporte deux phases : placement et déplacement. 
             Un joueur gagne en alignant 4 pièces en ligne droite (horizontale, verticale ou diagonale) 

             ou en formant un carré 2x2. 
             Les joueurs jouent à tour de rôle, et aucun coup ne peut placer une pièce sur une case occupée.".

get_game_configuration(Config) :-
    get_board_dimensions(R, C),
    get_pieces_per_player(Pieces),
    get_winning_formation_size(Size),
    Config = config{
        board_rows: R,
        board_cols: C,
        pieces_per_player: Pieces,
        winning_formation_size: Size
    }.

% =============================================================================
% SUPPORT PROTOCOLE DE COMMUNICATION
% =============================================================================

parse_command(CommandString, Command, Arguments) :-
    split_string(CommandString, " ", "", [CmdStr|Args]),
    atom_string(Command, CmdStr),
    Arguments = Args.

format_response(ok, Data, Response) :-
    format(string(Response), "OK ~w", [Data]).

format_response(error, Message, Response) :-
    format(string(Response), "ERROR ~w", [Message]).

create_error_response(Code, Message, Response) :-
    format(string(Response), "ERROR ~w: ~w", [Code, Message]).

create_success_response(Data, Response) :-
    format(string(Response), "OK ~w", [Data]).

get_game_status(GameState, Status) :-
    get_phase(GameState, Phase),
    get_turn(GameState, Turn),
    get_move_count(GameState, MoveCount),
    game_status(GameState, GameResult),
    Status = status(phase(Phase), turn(Turn), moves(MoveCount), result(GameResult)).

format_board_for_gui(GameState, GUIFormat) :-
    get_board(GameState, Board),
    GUIFormat = Board.

% =============================================================================
% DEBUG ET VALIDATION
% =============================================================================

validate_game_state(GameState) :-
    is_valid_game_state(GameState) ->
        writeln("Game state is valid.")
    ;   (detect_state_anomalies(GameState, Anomalies),
         format("Invalid game state detected: ~w~n", [Anomalies])).

print_game_state(GameState) :-
    get_board(GameState, Board),
    writeln("Current board:"),
    board_to_string(GameState, BoardStr),
    writeln(BoardStr),
    get_phase(GameState, Phase),
    get_turn(GameState, Turn),
    get_move_count(GameState, MoveCount),
    format("Phase: ~w, Turn: ~w, Move count: ~w~n", [Phase, Turn, MoveCount]).

get_state_statistics(GameState, Stats) :-
    get_all_pieces(GameState, player1, P1Pieces),
    get_all_pieces(GameState, player2, P2Pieces),
    length(P1Pieces, P1Count),
    length(P2Pieces, P2Count),
    get_move_count(GameState, MoveCount),
    Stats = stats{
        player1_pieces: P1Count,
        player2_pieces: P2Count,
        move_count: MoveCount
    }.

is_valid_game_state(GameState) :-
    get_board(GameState, Board),
    length(Board, 5),
    maplist(length_is_5, Board),
    get_pieces_placed(GameState, P1Count, P2Count),
    P1Count =< 4, 
    P2Count =< 4.

length_is_5(Row) :- length(Row, 5).

detect_state_anomalies(GameState, Anomalies) :-
    findall(Issue,
        (   (\+ is_valid_game_state(GameState), 
             Issue = "Invalid board size or pieces count")
        ;   (get_phase(GameState, Phase),
             \+ member(Phase, [placement, movement]), 
             Issue = "Invalid phase")
        ;   (get_turn(GameState, Turn),
             \+ member(Turn, [player1, player2]), 
             Issue = "Invalid turn")
        ),
        Anomalies
    ).

% =============================================================================
% COMMUNICATION IPC
% =============================================================================
% Format: COMMANDE [args...]
% Réponses: OK TYPE [args...] ou ERROR message
% Note: Les coordonnées sont converties 0-4 (Qt) <-> 1-5 (Prolog)
% =============================================================================

main :-
    init_game(GameState),
    retractall(current_game_state(_)),
    assertz(current_game_state(GameState)),
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
    Row is Row0 + 1,  % Convertir 0-4 -> 1-5
    Col is Col0 + 1,
    current_game_state(GameState),
    get_turn(GameState, Player),
    Move = place(Player, Row, Col),
    (   make_move(GameState, Move, NewState), print_board(NewState)->
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
    (   make_move(GameState, Move, NewState),print_board(NewState)->
        retractall(current_game_state(_)),
        assertz(current_game_state(NewState)),
        send_response("OK ACK")
    ;   send_response("ERROR Invalid move")
    )
    
.

handle_command("GETMOVE", _) :- !,
    current_game_state(GameState),
    get_phase(GameState, Phase),
    (   get_best_move(GameState, Move),print(Move) ->
        (   Phase = placement ->
            Move = place(_, Row, Col),
            make_move(GameState, Move, NewState),
            retractall(current_game_state(_)),
            assertz(current_game_state(NewState)),
            Row0 is Row - 1,  % Convertir 1-5 -> 0-4
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
% IA SIM'PLE (À IMPLÉMENTER)
% =============================================================================

%place2board(+Gamestate,+Coups,-Plateaus)
% transformer la liste de place() en une liste de plateaus
place2board(_,[],[]).
place2board(Gamestate,[Coup|Coups],[NewPlateau|Plateaus]):- make_move(Gamestate,Coup,NewPlateau),
place2board(Gamestate,Coups,Plateaus).

/*permuterP(+Player, -Autre player)
échanger atome player
*/
permuterP(player1,player2).
permuterP(player2,player1).

/*minimax(+Player,+GameState,+Profondité,-Meilleur Coup, -Meilleur Valeur)
prédicat non récursif du minimax: géneration de touts les coups légaux à partir d'un état GameState et exploration de toutes les possibilités.
*/
minimax(_,[GameState|_],0,GameState, 1):-!.
minimax(Player,GameState,Prof,MCoup,MVal):- !,
get_all_legal_moves(GameState, Player, Moves),place2board(GameState,Moves,Plateaus), 
minimaxMCoup(Player,Plateaus,Prof,MCoup,MVal).

/*minimaxMCoup(+Player,+Liste de Gamestates Coups, + Profondité,-MeilleurCoup,-MeilleurValeur)
prédicat loop récursif du minimax, avec 3 cas en evaluant le gamestate actuel.
Comparation de coups est fait au retour
*/
minimaxMCoup(player2,[],_,_,-2):-!.
minimaxMCoup(player1,[],_,_,2):-!.
minimaxMCoup(Player,[GameState|GameStates],P,MCoup,MVal):- (P>0->!,
( evalPlateau(GameState,Player,V)-> Prof is P-1, !, minimaxMCoup(Player,GameStates,Prof,MCoupAc,MValAc), 
comparerCoups(GameState,V,MCoupAc,MValAc,Player,MCoup,MVal),!; /*cas GameState est une victoire*/

!,Prof is P-1, minimaxMCoup(Player, GameStates, Prof,MCoupAc, MValAc),permuterP(Player,AutreP),!,
 minimax(AutreP,GameState,Prof,_,V1),!,comparerCoups(GameState,V1, MCoupAc, MValAc, Player, MCoup,MVal)/*cas gamestate n'est pas une victoire*/
)
;MCoup = GameState,MVal is 1). /*cas prof max atteint*/

% get_best_move(+GameState, -Move)
% TODO: Implémenter une IA (minimax, alpha-beta)
get_best_move(GameState, MCoup2) :-
    get_turn(GameState, Player),
    findall(MCoup,minimax(Player,GameState,10,MCoup,_),MCL1),
    MCL1 = [MCoup2] ,
    print_board(MCoup2)
    .