# Plan

## Références

* [8 Fallacies of distributed computing]()
* [Programming Models for Distributed Computing](http://dist-prog-book.com/)

## Plan

### PetStore : Une micro-boutique en ligne distribuée

On va programmer 2 services qui vont communiquer ensemble:

* le  _backend_ d'une boutique en ligne de vente d'animaux
  * On peut ajouter des animaux avec une espèce, un prix et un nom
  * Un utilisateur peut se "logger"
  * On peut consulter la liste des animaux disponibles
  * On peut ajouter un animal au _panier_ d'un utilisateur loggé
  * L'utilisateur peut _commander_ son panier en fournissant un numéro de carte de crédit
* un service de _paiement en ligne_:
  * il stocke un _solde disponible_ par numéro de carte
  * lorsqu'il reçoit une demande de paiement, il doit valider le numéro de carte et vérifier que le montant de la transaction correspond bien au disponible

Ces deux services vont utiliser un troisième service de stockage de type clé/valeur qui sera lui aussi distribué mais partagé par toutes les boutiques, et qui permet de _stocker_ des valeurs en générant une _clé_ et de retrouver la valeur à partir de la clé.

* un cas simple de programmation répartie : le RPC point à point. Le but est que les étudiants codent un serveur HTTP qui fasse des appels (HTTP) à un autre serveur (codé par un autre binôme/groupe d'étudiants). On utilisera le langage principal qu'ils maîtrisent
  * PetStore -> Un serveur applicatif + un serveur de paiement + K/V store
  * le serveur
* le message passing asynchrone et le modèle d'acteurs: implémenter la même fonctionnalité mais avec des messages asynchrones
* le problème du consensus: mon idée c'est qu'ils codent un système complètement répartie entre tous les serveurs qu'ils auront codé chacun, en les laissant libre de discuter de la meilleure manière de le faire
* enfin dernière étape, j'introduis un algorithme "bien connu", en l'occurence Raft, pour résoudre ce problème et on essaye de l'implémenter.

## Développement d'une base clé-valeur

* 1 serveur local simple GET/PUT
* 1 serveur local fonctionnant en mode master/slave
* serveur local avec cache
* serveurs répartis cohérents
