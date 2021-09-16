# Implementation des grammaires, verification de preuve d'appartenance d'un mot au langage d'une grammaire et prouveur d'appartenance d'un mot au langage d'une grammaire.


## Idée pour le prouveur :
pour une grammaire G, et son language L,
On peut essayer de définir une distance entre deux mots de L.
Ainsi, en appliquant un algorithme du type A* ou Dijkstra, il serait peut être possible de construire la suite de dérivations permettant de passer d'un mot à l'autre.

