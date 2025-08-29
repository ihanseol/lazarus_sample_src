Adaptation du programme "Substantifs2".
=======================================

Si le féminin d'un substantif, tel qu'il est généré par "Substantifs2" vous semble incorrect, pas utilisé dans la pratique ou différent
de ce qu'il est d'usage dans votre pays ou région, vous pouvez assez facilement y remédier, en forçant l'application de générer le (ou les
deux)  féminin(s), que vous souhaitez. En effet, si les règles avec leurs exceptions sont décrites dans le programme (et qu'il faudrait
donc changer le code source pour les modifier), il est tout autrement des substantifs à féminin irrégulier. Ceux-ci sont décrits dans un
fichier texte, appelé "irreguliers.txt", qui n'est autre chose qu'une liste de noms au masculin avec le (ou les deux) féminins. Comme,
pour déterminer le féminin d'un nom, "Substantifs2" consulte la liste des féminins irréguliers avant toute autre analyse du nom, suggérer
à l'application, que votre substantif est irrégulier, permettra de modifier/corriger le féminin du nom en question.

Voici, comment procéder:

1. Dans la liste des substantifs (fichier "substantifs.txt"), trouvez le nom en question et remplacer le code de formation du féminin indiqué
par "I" (ou par "i", s'il y 2 formes du féminin).

2. Dans la liste des substantifs irréguliers (fichier "irreguliers.txt"), ajoutez une ligne, comportant les éléments suivants:
    - le nom au masculin;
	- un tiret, encadré d'espaces (" - "), comme séparateur entre le masculin et le(s) féminin(s);
	- s'il y a une seule forme du féminin, ce féminin,
	  s'il y a deux formes du féminin, celles-ci, séparées par une virgule (",").
	  
Exemple:
--------
Ne voulant pas de "un maire", "une femme maire" comme féminin de "un maire", mais plutôt souhaitant avoir le féminin "une mairesse", comme on
dit au Québec, après avoir fait la modification dans "substantifs.txt", ajoutez la ligne suivante au fichier "irreguliers.txt":

maire - mairesse
 
That's it!
