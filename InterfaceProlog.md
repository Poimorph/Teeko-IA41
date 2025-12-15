# Interface de la Logique de Jeu pour Teeko (Sans IA)

## Proposition des differentes représentations

```prolog
% Plateau sous forme de matrice 5x5
Board = [
    [empty, empty, empty, empty, empty],
    [empty, red,   empty, empty, empty],
    [empty, empty, black, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, red,   empty]
]

% Accès aux positions par coordonnées (Row, Col) où les deux sont indexés à partir de 1
% Exemple: piece_at(Board, 2, 2, red) 


% État de jeu complet sous forme de terme composé
game_state(
    board(Board),           % Le plateau matrice 5x5
    phase(Phase),           % placement ou movement
    turn(Player),           % player1 ou player2  
    pieces_placed(P1Count, P2Count),  % (0-4, 0-4)
    history(MoveList),      % Liste des coups joués
    move_count(Count)       % Nombre total de coups
)

% Exemple d'état instancié:
game_state(
    board([
        [empty, empty, black, empty, empty],
        [empty, red,   empty, empty, empty],
        [empty, empty, black, empty, red],
        [empty, empty, empty, empty, empty],
        [red,   empty, empty, black, red]
    ]),
    phase(placement),
    turn(player1),
    pieces_placed(3, 4),
    history([place(player2,5,4), place(player1,5,1), ...]),
    move_count(7)
)


% Représenter les coups
% Coup de placement
move(place, Player, row(R), col(C))

% Coup de déplacement  
move(move, Player, from(R1,C1), to(R2,C2))


```

## Gestion de l'État de Jeu Principal

```prolog
% Initialiser/Réinitialiser l'état du jeu
init_game(-GameState)
% Crée un nouvel état de jeu avec un plateau vide et le joueur 1 qui commence

% Représentation de l'état actuel du jeu
current_state(?GameState)
% Représentation unifiée du plateau, tour du joueur, phase, et pièces placées

% Mettre à jour l'état du jeu
update_state(+OldState, +Move, -NewState)
% Applique un coup pour produire un nouvel état, gérant les transitions de phase
```

## Représentation et Requêtes du Plateau

```prolog
% Requêtes sur les positions du plateau
get_board(+GameState, -Board)
% Extrait la matrice 5x5 du plateau depuis l'état de jeu

piece_at(+GameState, +Position, ?Piece)
% Interroge quelle pièce (black/red/empty) est à une position donnée

get_all_pieces(+GameState, +Player, -Positions)
% Retourne la liste de toutes les positions occupées par les pièces d'un joueur

board_to_string(+GameState, -StringRepresentation)
% Convertit le plateau en format chaîne affichable pour l'interface graphique

get_board_as_matrix(+GameState, -Matrix)
% Retourne le plateau sous forme de structure matricielle 5x5
```

## Gestion des Phases de Jeu

```prolog
% Détermination de la phase
current_phase(+GameState, ?Phase)
% Vrai si Phase est la phase actuelle (la retourne si Phase n'est pas donnée)

pieces_placed_count(+GameState, +Player, -Count)
% Nombre de pièces qu'un joueur a placées (0-4)

get_remaining_placements(+GameState, +Player, -Count)
% Combien de pièces le joueur doit encore placer
```

## Gestion des Tours

```prolog
% Contrôle du tour des joueurs
current_player(+GameState, ?Player)
% Vrai si c'est ke Tour de Player (retourne a qui est le tour si aucun Player spécifié)

switch_turn(+GameState, -NewState)
% Alterne le joueur actif

get_opponent(+Player, -Opponent)
% Retourne l'autre joueur
```

## Génération et Validation des Coups

```prolog
% Génération des coups légaux
get_all_legal_moves(+GameState, +Player, -MovesList)
% Retourne tous les coups possibles légaux pour l'état et le joueur actuels

get_placement_moves(+GameState, +Player, -MovesList)
% Retourne toutes les positions de placement valides pendant la phase de placement

get_movement_moves(+GameState, +Player, -MovesList)  
% Retourne tous les mouvements de pièces valides pendant la phase de mouvement

get_moves_for_piece(+GameState, +Position, -MovesList)
% Retourne les coups possibles pour une pièce spécifique à une position

% Validation des coups
is_valid_move(+GameState, +Move)
% Valide si un coup est légal dans l'état actuel

is_valid_placement(+GameState, +Position)
% Vérifie si le placement à une position est légal

is_valid_movement(+GameState, +FromPos, +ToPos)
% Vérifie si déplacer une pièce de->vers est légal

validate_move_detailed(+GameState, +Move, -ErrorCode, -ErrorMessage)
% Fournit une validation détaillée avec explication de l'erreur

can_move_to(+GameState, +FromPos, +ToPos)
% Vérifie si une pièce peut se déplacer d'une position à une autre

is_adjacent_move(+FromPos, +ToPos)
% Vérifie si le mouvement est vers une cellule adjacente (orthogonale ou diagonale)
```

## Exécution des Coups

```prolog
% Appliquer les coups à l'état du jeu
make_move(+GameState, +Move, -NewState)
% Exécute un coup validé et retourne l'état résultant

place_piece(+GameState, +Player, +Position, -NewState)
% Place une nouvelle pièce pendant la phase de placement

move_piece(+GameState, +FromPos, +ToPos, -NewState)
% Déplace une pièce existante pendant la phase de mouvement

undo_move(+GameState, -PreviousState, -Move)
% Annule le dernier coup si l'historique est maintenu

apply_move_sequence(+InitialState, +MovesList, -FinalState)
% Applique plusieurs coups en séquence

## Vérification des Conditions de Victoire

```prolog
% Status de la partie
game_status(+GameState, ?Status)
% Verifie ou retourne l'État actuelle du jeu
% Status = playing/winner(Player)/draw

check_four_in_line(+GameState, +Player)
% Vérifie la victoire par ligne horizontale/verticale/diagonale

check_four_in_square(+GameState, +Player)
% Vérifie la victoire par formation carrée 2x2

get_winning_positions(+GameState, +Player, -Positions)
% Retourne les positions formant la configuration gagnante

is_draw(+GameState)
% Vérifie les conditions d'égalité/pat

get_winning_formation_type(+GameState, +Player, -Type)
% Retourne 'line' ou 'square' pour la formation gagnante
```

## Analyse du Jeu

```prolog
% Évaluation de position
count_lines_of_length(+GameState, +Player, +Length, -Count)
% Compte combien de lignes de longueur spécifiée le joueur possède

find_potential_wins(+GameState, +Player, -Positions)
% Trouve les positions qui créeraient une formation gagnante

is_blocking_move(+GameState, +Move, +Player)
% Vérifie si le coup bloque une opportunité de victoire de l'adversaire

get_threatened_positions(+GameState, +Player, -Positions)
% Retourne les positions menacées par l'adversaire

count_formations(+GameState, +Player, -Lines, -Squares)
% Compte les formations partielles de lignes et carrés

find_critical_positions(+GameState, -Positions)
% Identifie les positions critiques pour les deux joueurs
```

## Notation et Historique des Coups

```prolog
% Représentation des coups
move_to_notation(+Move, -Notation)
% Convertit un coup interne en notation lisible (ex: "A1", "B2-C3")

notation_to_move(+Notation, -Move)
% Analyse une chaîne de notation vers le format de coup interne

format_move_for_display(+Move, +Phase, -DisplayString)
% Formate le coup pour l'affichage GUI selon la phase de jeu

% Historique du jeu
get_move_history(+GameState, -History)
% Retourne la liste de tous les coups effectués

add_to_history(+GameState, +Move, -NewState)
% Ajoute un coup à l'historique

replay_moves(+InitialState, +MovesList, -FinalState)
% Rejoue une séquence de coups depuis l'état initial

get_last_move(+GameState, -Move)
% Retourne le coup le plus récent effectué

clear_history(+GameState, -NewState)
% Supprime l'historique des coups de l'état
```

## Prédicats Utilitaires

```prolog
% Système de coordonnées
valid_position(+Position)
% Vérifie si la position est dans les limites du plateau 5x5

adjacent_positions(+Position, -AdjacentList)
% Retourne toutes les positions adjacentes orthogonalement

diagonal_positions(+Position, -DiagonalList)
% Retourne toutes les positions adjacentes diagonalement

all_neighbor_positions(+Position, -NeighborsList)
% Retourne les 8 positions voisines (orthogonales + diagonales)

distance(+Pos1, +Pos2, -Distance)
% Calcule la distance entre deux positions

position_to_coordinates(+Position, -Row, -Col)
% Convertit la notation de position en indices ligne/colonne

coordinates_to_position(+Row, +Col, -Position)
% Convertit ligne/colonne en notation de position

% Vérification des lignes et formations
positions_form_line(+PositionsList)
% Vérifie si les positions forment une ligne droite

positions_form_square(+PositionsList)
% Vérifie si les positions forment un carré 2x2

get_line_direction(+Pos1, +Pos2, -Direction)
% Retourne la direction (horizontale/verticale/diagonale) entre positions

extend_line(+GameState, +Position, +Direction, -Positions)
% Retourne toutes les positions s'étendant depuis une position donnée dans une direction
```

## Sérialisation et Persistance du Jeu

```prolog
% Sérialisation de l'état du jeu
serialize_state(+GameState, -String)
% Convertit l'état du jeu en chaîne pour stockage/transmission

deserialize_state(+String, -GameState)
% Reconstruit l'état du jeu depuis une chaîne sérialisée

export_game(+GameState, -ExportFormat)
% Exporte le jeu dans un format standard (JSON/XML)

import_game(+ExportFormat, -GameState)
% Importe un jeu depuis un format externe

save_game_to_file(+GameState, +Filename)
% Persiste l'état du jeu dans un fichier

load_game_from_file(+Filename, -GameState)
% Charge l'état du jeu depuis un fichier
```

## Informations et Configuration du Jeu

```prolog
% Règles et paramètres du jeu
get_board_dimensions(-Rows, -Cols)
% Retourne la taille du plateau (5x5 pour Teeko)

get_pieces_per_player(-Count)
% Retourne le nombre de pièces par joueur (4)

get_winning_formation_size(-Size)
% Retourne la taille de la formation gagnante (4)

get_game_rules(-RulesDescription)
% Retourne les règles du jeu lisibles par l'humain

get_game_configuration(-Config)
% Retourne les paramètres de configuration actuels du jeu
```

## Support du Protocole de Communication

```prolog
% Gestion des messages IPC
parse_command(+CommandString, -Command, -Arguments)
% Analyse les commandes entrantes depuis l'interface graphique

format_response(+ResponseType, +Data, -ResponseString)
% Formate les réponses pour consommation par l'interface graphique

create_error_response(+ErrorCode, +ErrorMessage, -Response)
% Crée une réponse d'erreur standardisée

create_success_response(+Data, -Response)
% Crée une réponse de succès standardisée

% Requêtes de statut pour l'interface graphique
get_game_status(+GameState, -Status)
% Retourne un statut complet (phase, tour, scores, etc.)

get_display_data(+GameState, -DisplayData)
% Retourne toutes les données nécessaires pour la mise à jour de l'affichage GUI

format_board_for_gui(+GameState, -GUIFormat)
% Convertit le plateau au format spécifique de l'interface graphique
```

## Support de Débogage et de Test

```prolog
% Validation et débogage de l'état
validate_game_state(+GameState)
% Vérifie la cohérence et la validité de l'état

print_game_state(+GameState)
% Affichage formaté de l'état du jeu pour le débogage

get_state_statistics(+GameState, -Stats)
% Retourne des informations statistiques sur l'état actuel

is_valid_game_state(+GameState)
% Retourne vrai si l'état est cohérent en interne

detect_state_anomalies(+GameState, -Anomalies)
% Identifie toute incohérence dans l'état du jeu
```