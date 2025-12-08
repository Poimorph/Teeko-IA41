:- use_module(library(lists)).

% =============================================================================
% État du jeu courant
% =============================================================================

:- dynamic current_game_state/1.


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
    (Line = end_of_file ->
        true
    ;
        catch(
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
    Move = move(place, Player, row(Row), col(Col)),
    (make_move(GameState, Move, NewState) ->
        retractall(current_game_state(_)),
        assertz(current_game_state(NewState)),
        send_response("OK ACK")
    ;
        send_response("ERROR Invalid drop position")
    ).

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
    Move = move(move, Player, from(FromRow, FromCol), to(ToRow, ToCol)),
    (make_move(GameState, Move, NewState) ->
        retractall(current_game_state(_)),
        assertz(current_game_state(NewState)),
        send_response("OK ACK")
    ;
        send_response("ERROR Invalid move")
    ).

% GETMOVE - Demander le coup de l'IA
handle_command("GETMOVE", _) :- !,
    current_game_state(GameState),
    get_phase(GameState, Phase),
    (get_best_move(GameState, Move) ->
        (Phase = placement ->
            Move = move(place, _, row(Row), col(Col)),
            make_move(GameState, Move, NewState),
            retractall(current_game_state(_)),
            assertz(current_game_state(NewState)),
            Row0 is Row - 1,  % Convertir 1-5 -> 0-4
            Col0 is Col - 1,
            format(string(Response), "OK DROP ~d ~d", [Row0, Col0]),
            send_response(Response)
        ;
            Move = move(move, _, from(FromRow, FromCol), to(ToRow, ToCol)),
            make_move(GameState, Move, NewState),
            retractall(current_game_state(_)),
            assertz(current_game_state(NewState)),
            FromRow0 is FromRow - 1,
            FromCol0 is FromCol - 1,
            ToRow0 is ToRow - 1,
            ToCol0 is ToCol - 1,
            format(string(Response), "OK MOVE ~d ~d ~d ~d", [FromRow0, FromCol0, ToRow0, ToCol0]),
            send_response(Response)
        )
    ;
        send_response("ERROR No valid move found")
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
% SUPPORT DU PROTOCOLE DE COMMUNICATION
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
