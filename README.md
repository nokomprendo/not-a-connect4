# Not a Connect4

Puissance 4 en réseau pour comparer des IA.

## Protocole réseau

- websockets
- messages textes utf-8 finissant par " \n"
- player/pool sans whitespaces (' ', \t, \n, \r)
- BOARD: (. | R | Y)x42                     ; ligne0, ligne1...
- COLOR: R | Y
- MOVE: 0-6                                 ; numéro colonne
- RESULT: WinR | WinY | Draw
- move invalide -> annulation partie courante et fin
- déconnexion -> annulation partie courante et fin
- TODO: gestion du temps (par coup ? par partie ? ...)
- s2c/c2s: server-to-client/client-to-server

```
c2s: connect <player-name> <pool>\n
s2c: connected [message]\n                  ; message d'accueil
s2c: not-connected [message]\n              ; message d'erreur

s2c: newgame <playerR> <playerY>\n
s2c: genmove BOARD COLOR\n                 ; board, 1er joueur, joueur courant
c2s: playmove MOVE\n
s2c: endgame BOARD RESULT\n
```

## Monitoring serveur

```
- pour chaque pool: nom, description
    - pour chaque partie terminée: <playerR> <playerY> RESULT HISTORY TIME TIME
    - pour chaque joueur: nbparties, ratio win/lose/draw, durée moyenne
```

- HISTORY: ab...        ; coup a, puis coup b puis, ...
- TIME: t               ; temps de calcul cumulé du joueur, en secondes
- API JSON ?
- page web ?

## Roadmap

- 2 joueurs clients via server
- déploiement docker/heroku
- n joueurs via server
- page web SSE
- channels
- stocker les résultats dans une BD

## Changelog

- puissance 4
- bots
- serveur http basique
- serveur json basique
- connexion basique client-server WS
- types pour le protocole
- sérialisation protocole/game
- le serveur envoie un jeu et fin
- le serveur stocke les clients dans une map
- le serveur affiche les clients dans la page web
- implémentation client
- runner basique

