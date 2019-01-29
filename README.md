# Plan

## Références

* 8 Fallacies of distributed computing



* un cas simple de programmation répartie : le RPC point à point. Le but est que les étudiants codent un serveur HTTP qui fasse des appels (HTTP) à un autre serveur (codé par un autre binôme/groupe d'étudiants). On utilisera le langage principal qu'ils maîtrisent
  * PetStore -> Un serveur applicatif + un serveur de paiement + K/V store
  * le serveur
* le message passing asynchrone et le modèle d'acteurs: implémenter la même fonctionnalité mais avec des messages asynchrones
* le problème du consensus: mon idée c'est qu'ils codent un système complètement répartie entre tous les serveurs qu'ils auront codé chacun, en les laissant libre de discuter de la meilleure manière de le faire
* enfin dernière étape, j'introduis un algorithme "bien connu", en l'occurence Raft, pour résoudre ce problème et on essaye de l'implémenter.
