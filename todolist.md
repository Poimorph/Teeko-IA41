# ToDoList — Teeko (C++ / SFML)

## Initialisation & Structure du Projet
- [ ] Créer le squelette **CMake** (`teeko`, Debug/Release, warnings).
- [ ] Ajouter dépendances : **SFML** (graphics, window, audio), éventuellement **fmt**(formattage des textes).
- [ ] Arborescence :  
```
/src
/include
/assets (fonts, sprites, sons)
/build
/release
```
- [ ] Boucle de jeu.
- [ ] Gestion centralisée des ressources (fonts/textures/sounds) + cache.
- [ ] Implémentation du parallèlisme

---

## Jeu et IA
-Jeu:
    -implementer verifications de victoire
    -arreter un joueur de jouer dans une intersection non vide
    -implementer les 2 phases du jeu: placement de 4 pions et aprés, phase avec bougements de pions
    -implementer mode facile: sans IA, coups aleatoaires

    
-IA:Rechercher l'algo minimax
    -definir l'Heuristique de chaque état
    -implementer en prolog
    -implementer l'algo complét

---
## Modèle d’Interface
- [ ] Définir le GameState UI :  
- phase = `Placement` | `Déplacement` | `Fin`  
- tour = `Rouge` | `Bleu`  
- sélection courante, dernier coup, message d’état.
- [ ] Créer un BoardViewModel (5×5) avec mapping grille <-> pixels (clics surlignement etc).

---

## Rendu du Plateau
- [ ] Dessiner le plateau 5×5 (grille ou nœuds + lignes).
- [ ] Afficher les pions (rouge/Bleu) avec cercles ou sprites.
- [ ] Gérer les états visuels :  
    - surbrillance case sous la souris  
    - pion sélectionné  
    - coups jouables  
    - dernier coup joué
- [ ] Animations :  
    - apparition (placement)  
    - glissement (déplacement)

---

## Entrées & Ergonomie
- [ ] Clic gauche : sélection / dépôt.
- [ ] Support drag & drop (optionnel).
- [ ] Gestion des états d’interaction :  
    - `Placement` -> clic sur case vide  
    - `Déplacement` -> clic sur pion du joueur, puis destination
- [ ] Raccourcis clavier : 
    - `Esc` -> quitter 
    - `F11` -> plein écran
    - `F3`  -> overlay debug
    - **à determiner**
- [ ] Curseur personnalisé selon contexte. (optionnel)

---

## Scènes & UI Hors Plateau
- [ ] Scene Manager : `MainMenu`, `Game`, `Settings`, `EndScreen`.
- [ ] MainMenu : Jouer, Options, Quitter.
- [ ] HUD en jeu :  
    - Texte “Au tour de Rouge/Bleu”  
    - Phase actuelle  
    - Timer (affichage seulement)  
    - Bouton “Recommencer”
- [ ] EndScreen : gagnant + boutons Rejouer/Menu.
- [ ] Settings : volume, plein écran, thème, vitesse animations. (optionnel)

---

## Layout & Adaptation
- [ ] Plateau centré et responsive.
- [ ] Support HiDPI et redimensionnement.
- [ ] Police lisible (Roboto ou équivalent).

---

## Effets & Feedback
- [ ] Particules ou halo sur pion joué. (optionnel)
- [ ] Sons (clic, pose, déplacement, victoire).
- [ ] Bandeau d’info ou toast pour erreurs (destination invalide).

---

## Validation Visuelle
- [ ] Afficher pattern de victoire (ligne de 4 ou carré 2×2).
- [ ] Afficher les cases adjacentes au pion sélectionné.

---

## Journal & Debug
- [ ] Overlay `F3` : FPS, frame time, phase, tour, sélection, taille fenêtre.
- [ ] Mode “hitboxes” (affiche zones cliquables).
- [ ] Logs stdout : clics, commandes envoyées/reçues.

---

## Persistance (UI)
- [ ] Sauvegarde des options (JSON) : volume, plein écran, thème. (si options intégrés)
- [ ] Replay local (liste des coups joués).

---

## Pont d’Intégration/ communication cpp-IA(Prolog)
- [ ] API C++ côté UI :  
    - `requestPlace(x, y)`  
    - `requestMove(x1, y1, x2, y2)`
- [ ] Événements entrants :  
    - `onBoardUpdated(boardState)`  
    - `onTurnChanged()`  
    - `onPhaseChanged()`  
    - `onWin(pattern)`  
    - `onError(message)`
- [ ] File d’ordres UI-safe (préparer pour async plus tard).
- [ ] Spinner ou overlay pendant le traitement d’un coup.

---

## Tests
- [ ] Test manuel clics aléatoires -> pas de crash.
- [ ] Redimensionnement agressif -> OK.
- [ ] 50 placements/déplacements -> pas de fuite.
- [ ] FPS stable (60 FPS).
- [ ] Vérifier contraste, lisibilité, accessibilité.




