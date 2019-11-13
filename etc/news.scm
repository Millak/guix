;; GNU Guix news, for use by 'guix pull'.
;;
;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
;;
;; Copying and distribution of this file, with or without modification, are
;; permitted in any medium without royalty provided the copyright notice and
;; this notice are preserved.

(channel-news
 (version 0)

 (entry (commit "3e962e59d849e4300e447d94487684102d9d412e")
        (title (en "@command{guix graph} now supports package
transformations")
               (de "@command{guix graph} unterstützt nun Paketumwandlungen"))
        (body
         (en "The @command{guix graph} command now supports the common package
transformation options (see @command{info \"(guix) Package Transformation
Options\"}).  This is useful in particular to see the effect of the
@option{--with-input} dependency graph rewriting option.")
         (de "Der Befehl @command{guix graph} unterstützt nun die mit anderen
Befehlen gemeinsamen Umwandlungsoptionen (siehe @command{info \"(guix.de)
Paketumwandlungsoptionen\"}).  Sie helfen insbesondere dabei, die Wirkung der
Befehlszeilenoption @option{--with-input} zum Umschreiben des
Abhängigkeitsgraphen zu sehen.")
         (es "La orden @command{guix graph} ahora implementa las opciones
comunes de transformación de paquetes (véase @command{info \"(guix.es)
Opciones de transformación de paquetes\"}). Esto es particularmente
útil para comprobar el efecto de la opción de reescritura del grafo
de dependencias @option{--with-input}.")))

 (entry (commit "49af34cfac89d384c46269bfd9388b2c73b1220a")
        (title (en "@command{guix pull} now honors
@file{/etc/guix/channels.scm}")
               (de "@command{guix pull} berücksichtigt nun
@file{/etc/guix/channels.scm}")
               (es "Ahora @command{guix pull} tiene en cuenta
@file{/etc/guix/channels.scm}")
               (fr "@command{guix pull} lit maintenant
@file{/etc/guix/channels.scm}"))
        (body
         (en "The @command{guix pull} command will now read the
@file{/etc/guix/channels.scm} file if it exists and if the per-user
@file{~/.config/guix/channels.scm} is not present.  This allows administrators
of multi-user systems to define site-wide defaults.")
         (de "Der Befehl @command{guix pull} liest nun die Datei
@file{/etc/guix/channels.scm}, wenn sie existiert und es für den jeweiligen
Benutzer keine @file{~/.config/guix/channels.scm} gibt.  Dadurch können
Administratoren von Mehrbenutzersystemen systemweite Voreinstellungen
vornehmen.")
         (es "Ahora la orden @command{guix pull} lee el fichero
@file{/etc/guix/channels.scm} si existe y el fichero personalizable
@file{~/.config/guix/channels.scm} no está presente. Esto permite a quienes
administran sistemas con múltiples usuarias definir valores predeterminados
en el sistema.")
         (fr "La commande @command{guix pull} lira maintenant le fichier
@file{/etc/guix/channels.scm} s'il existe et si le fichier
@file{~/.config/guix/channels.scm} par utilisateur·rice n'est pas présent.
Cela permet aux personnes administrant des systèmes multi-utilisateurs de
définir les canaux par défaut.")))

 (entry (commit "81c580c8664bfeeb767e2c47ea343004e88223c7")
        (title (en "Insecure @file{/var/guix/profiles/per-user} permissions (CVE-2019-18192)")
               (de "Sicherheitslücke in @file{/var/guix/profiles/per-user}-Berechtigungen (CVE-2019-18192)")
               (es "Vulnerabilidad en los permisos de @file{/var/guix/profiles/per-user} (CVE-2019-18192)")
               (fr "Permissions laxistes pour @file{/var/guix/profiles/per-user} (CVE-2019-18192)")
               (nl "Onveilige @file{/var/guix/profiles/per-user}-rechten (CVE-2019-18192)"))
        (body
         (en "The default user profile, @file{~/.guix-profile}, points to
@file{/var/guix/profiles/per-user/$USER}.  Until now,
@file{/var/guix/profiles/per-user} was world-writable, allowing the
@command{guix} command to create the @code{$USER} sub-directory.

On a multi-user system, this allowed a malicious user to create and populate
that @code{$USER} sub-directory for another user that had not yet logged in.
Since @code{/var/@dots{}/$USER} is in @code{$PATH}, the target user could end
up running attacker-provided code.  See
@uref{https://issues.guix.gnu.org/issue/37744} for more information.

This is now fixed by letting @command{guix-daemon} create these directories on
behalf of users and removing the world-writable permissions on
@code{per-user}.  On multi-user systems, we recommend updating the daemon now.
To do that, run @code{sudo guix pull} if you're on a foreign distro, or run
@code{guix pull && sudo guix system reconfigure @dots{}} on Guix System.  In
both cases, make sure to restart the service afterwards, with @code{herd} or
@code{systemctl}.")
         (de "Das voreingestellte Benutzerprofil, @file{~/.guix-profile},
verweist auf @file{/var/guix/profiles/per-user/$USER}.  Bisher hatte jeder
Benutzer Schreibzugriff auf @file{/var/guix/profiles/per-user}, wodurch der
@command{guix}-Befehl berechtigt war, das Unterverzeichnis @code{$USER}
anzulegen.

Wenn mehrere Benutzer dasselbe System benutzen, kann ein böswilliger Benutzer
so das Unterverzeichnis @code{$USER} und Dateien darin für einen anderen
Benutzer anlegen, wenn sich dieser noch nie angemeldet hat.  Weil
@code{/var/…/$USER} auch in @code{$PATH} aufgeführt ist, kann der betroffene
Nutzer dazu gebracht werden, vom Angreifer vorgegebenen Code auszuführen.
Siehe @uref{https://issues.guix.gnu.org/issue/37744} für weitere
Informationen.

Der Fehler wurde nun behoben, indem @command{guix-daemon} diese Verzeichnisse
jetzt selbst anlegt statt das dem jeweiligen Benutzerkonto zu überlassen.  Der
Schreibzugriff auf @code{per-user} wird den Benutzern entzogen.  Für Systeme
mit mehreren Benutzern empfehlen wir, den Daemon jetzt zu aktualisieren.  Auf
einer Fremddistribution führen Sie dazu @code{sudo guix pull} aus; auf einem
Guix-System führen Sie @code{guix pull && sudo guix system reconfigure …}
aus.  Achten Sie in beiden Fällen darauf, den Dienst mit @code{herd} oder
@code{systemctl} neuzustarten.")
         (es "El perfil predeterminado de la usuaria, @file{~/.guix-profile},
apunta a @file{/var/guix/profiles/per-user/$USUARIA}.  Hasta ahora cualquiera
podía escribir en @file{/var/guix/profiles/per-user}, lo cual permitía
a la orden @command{guix} crear el subdirectorio @code{$USUARIA}.

En un sistema con múltiples usuarias, esto permitiría a cualquiera con
intención de causar daño crear ese subdirectorio @code{$USUARIA} con el nombre
de alguien que no hubiese ingresado en el sistema. Puesto que ese
subdirectorio @code{/var/@dots{}/$USUARIA} se encuentra en la ruta de binarios
predeterminada @code{$PATH}, el objetivo del ataque podría ejecutar código
proporcionado por la parte atacante. Véase
@uref{https://issues.guix.gnu.org/issue/37744} para obtener más información.

Se ha solucionando delegando en @command{guix-daemon} la creación de esos
directorios y eliminando los permisos de escritura para todo el mundo en
@code{per-user}. En sistemas con múltiples usuarias recomendamos actualizar
cuanto antes el daemon. Para hacerlo ejecute @code{sudo guix pull} si se
encuentra en una distribución distinta, o ejecute @code{guix pull && sudo guix system reconfigure @dots{}} en el sistema Guix. En ambos casos, asegurese de
reiniciar el servicio tras ello, con @code{herd} o @code{systemctl}.")
         (fr "Le profil utilisateur par défaut, @file{~/.guix-profile},
pointe vers @file{/var/guix/profiles/per-user/$USER}.  Jusqu'à
maintenant, @file{/var/guix/profiles/per-user} était disponible en
écriture pour tout le monde, ce qui permettait à la commande
@command{guix} de créér le sous-répertoire @code{$USER}.

Sur un système multi-utilisateur, cela permet à un utilisateur
malveillant de créer et de remplir le sous-répertoire @code{USER} pour
n'importe quel utilisateur qui ne s'est jamais connecté.  Comme
@code{/var/@dots{}/$USER} fait partie de @code{$PATH}, l'utilisateur
ciblé pouvait exécuter des programmes fournis par l'attaquant.  Voir
@uref{https://issues.guix.gnu.org/issue/37744} pour plus de détails.

Cela est maintenant corrigé en laissant à @command{guix-daemon} le soin
de créer ces répertoire pour le compte des utilisateurs et en
supprimant les permissions en écriture pour tout le monde sur
@code{per-user}.  Nous te recommandons de mettre à jour le démon
immédiatement.  Pour cela, lance @code{sudo guix pull} si tu es sur
une distro externe ou @code{guix pull && sudo guix system reconfigure
@dots{}} sur le système Guix.  Dans tous les cas, assure-toi ensuite de
redémarrer le service avec @code{herd} ou @code{systemctl}.")
         (nl "Het standaard gebruikersprofiel, @file{~/.guix-profile}, verwijst
naar @file{/var/guix/profiles/per-user/$USER}.  Tot op heden kon om het even wie
in @file{/var/guix/profiles/per-user} schrijven, wat het @command{guix}-commando
toestond de @code{$USER} submap aan te maken.

Op systemen met meerdere gebruikers kon hierdoor een kwaadaardige gebruiker een
@code{$USER} submap met inhoud aanmaken voor een andere gebruiker die nog niet
was ingelogd.  Omdat @code{/var/@dots{}/$USER} zich in @code{$PATH} bevindt,
kon het doelwit zo code uitvoeren die door de aanvaller zelf werd aangeleverd.
Zie @uref{https://issues.guix.gnu.org/issue/37744} voor meer informatie.

Dit probleem is nu verholpen: schrijven door iedereen in @code{per-user} is niet
meer toegestaan en @command{guix-daemon} maakt zelf submappen aan namens de
gebruiker.  Op systemen met meerdere gebruikers raden we aan om
@code{guix-daemon} nu bij te werken.  Op Guix System kan dit met
@code{guix pull && sudo guix system reconfigure @dots{}}, op andere distributies
met @code{sudo guix pull}.  Herstart vervolgens in beide gevallen
@code{guix-daemon} met @code{herd} of @code{systemctl}.")))

 (entry (commit "5f3f70391809f8791c55c05bd1646bc58508fa2c")
        (title (en "GNU C Library upgraded")
               (de "GNU-C-Bibliothek aktualisiert")
               (es "Actualización de la biblioteca C de GNU")
               (fr "Mise à jour de la bibliothèque C de GNU")
               (nl "GNU C-bibliotheek bijgewerkt"))
        (body
         (en "The GNU C Library (glibc) has been upgraded to version 2.29.  To
run previously-installed programs linked against glibc 2.28, you need to
install locale data for version 2.28 in addition to locale data for 2.29:

@example
guix install glibc-locales glibc-locales-2.28
@end example

On Guix System, you can adjust the @code{locale-libcs} field of your
@code{operating-system} form.  Run @code{info \"(guix) Locales\"}, for more
info.")
         (de "Die GNU-C-Bibliothek (glibc) wurde auf Version 2.29
aktualisiert. Um zuvor installierte Programme, die an glibc 2.28 gebunden
worden sind, weiter benutzen zu können, müssen Sie Locale-Daten für Version
2.28 zusätzlich zu den Locale-Daten für 2.29 installieren:

@example
guix install glibc-locales glibc-locales-2.28
@end example

Auf Guix System genügt es, das @code{locale-libcs}-Feld Ihrer
@code{operating-system}-Form anzupassen. Führen Sie @code{info \"(guix.de)
Locales\"} aus, um weitere Informationen dazu zu erhalten.")
         (es "Se ha actualizado la biblioteca de C de GNU (glibc) a la versión
2.29. Para ejecutar programas instalados previamente que se encuentren
enlazados con glibc 2.28, es necesario que instale los datos de localización
de la versión 2.28 junto a los datos de localización de la versión 2.29:

@example
guix install glibc-locales glibc-locales-2.28
@end example

En el sistema Guix, puede ajustar el campo @code{locale-libcs} de su
declaración @code{operating-system}. Ejecute @code{info \"(guix.es)
Localizaciones\"} para obtener más información.")
         (fr "La bibliothèque C de GNU (glibc) a été mise à jour en version
2.29.  Pour pouvoir lancer tes programmes déjà installés et liés à glibc 2.28,
tu dois installer les données pour la version 2.28 en plus des données de
régionalisation pour la version 2.29 :

@example
guix install glibc-locales glibc-locales-2.28
@end example

Sur le système Guix, tu peux ajuster le champ @code{locale-libcs} de ta forme
@code{operating-system}.  Lance @code{info \"(guix.fr) Régionalisation\"} pour
plus de détails.")
         (nl "De GNU C-bibliotheek (glibc) werd bijgewerkt naar versie 2.29.
Om gebruik te maken van reeds geïnstalleerde programma's die aan glibc 2.28
gebonden zijn, moet u de regionale informatie van versie 2.28 naast die van
versie 2.29 installeren:

@example
guix install glibc-locales glibc-locales-2.28
@end example

Op Guix System kunt u het @code{locale-libcs}-veld van uw
@code{operating-system}-vorm aanpassen.   Voer @code{info \"(guix) Locales\"}
uit voor verdere uitleg.")))
 (entry (commit "cdd3bcf03883d129581a79e6d6611b2afd3b277b")
        (title (en "New reduced binary seed bootstrap")
               (de "Neues Bootstrapping mit kleinerem Seed")
               (es "Nueva reducción de la semilla binaria para el lanzamiento inicial")
               (fr "Nouvel ensemble de binaires de bootstrap réduit")
               (nl "Nieuwe bootstrap met verkleinde binaire kiem"))
        (body
         (en "The package graph on x86_64 and i686 is now rooted in a
@dfn{reduced set of binary seeds}.  The initial set of binaries from which
packages are built now weighs in at approximately 130 MiB, half of what it
used to be.  Run @code{info \"(guix) Bootstrapping\"} to learn more, or watch
the talk at @uref{https://archive.fosdem.org/2019/schedule/event/gnumes/}.")
         (de "Der Paketgraph auf x86_64 und i686 hat jetzt eine @dfn{kleinere
Menge an binären Seeds} als Wurzel. Das heißt, die ursprüngliche Menge an
Binärdateien, aus denen heraus Pakete erstellt werden, machen nun ungefähr
130 MiB aus, halb so viel wie früher. Führen Sie @code{info \"(guix.de)
Bootstrapping\"} aus, um mehr zu erfahren, oder schauen Sie sich den Vortrag
auf @uref{https://archive.fosdem.org/2019/schedule/event/gnumes/} an.")
         (fr "Le graphe des paquets sur x86_64 et i686 prend maintenant sa
source dans un @dfn{ensemble réduit de binaires}.  L'ensemble initial des
binaires à partir desquels les paquets sont construits pèse maintenant environ
130 Mio, soit la moitié par rapport à l'ensemble précédent.  Tu peux lancer
@code{info \"(guix) Bootstrapping\"} pour plus de détails, ou regarder la
présentation sur @uref{https://archive.fosdem.org/2019/schedule/event/gnumes/}.")
         (es "El grafo de paquetes en x86_64 y i686 ahora tiene su raíz en un
@dfn{conjunto reducido de semillas binarias}. El conjunto inicial de binarios
desde el que se construyen los paquetes ahora tiene un tamaño aproximado de
130_MiB , la mitad de su tamaño anterior. Ejecute @code{info \"(guix.es)
Lanzamiento inicial\"} para aprender más, o puede ver la charla en inglés
en@uref{https://archive.fosdem.org/2019/schedule/event/gnumes/}.")
         (nl "Het netwerk van pakketten voor x86_64 en i686 is nu geworteld in
een @dfn{verkleinde verzameling van binaire kiemen}.  Die beginverzameling
van binaire bestanden waaruit pakketten gebouwd worden is nu zo'n 130 MiB
groot; nog maar half zo groot als voorheen.  Voer @code{info \"(guix)
Bootstrapping\"} uit voor meer details, of bekijk de presentatie op
@uref{https://archive.fosdem.org/2019/schedule/event/gnumes/}.")))

 (entry (commit "dcc90d15581189dbc30e201db2b807273d6484f0")
        (title (en "New channel news mechanism")
               (de "Neuer Mechanismus, um Neuigkeiten über Kanäle anzuzeigen.")
               (es "Nuevo mecanismo de noticias de los canales")
               (fr "Nouveau mécanisme d'information sur les canaux")
               (nl "Nieuw mechanisme voor nieuwsberichten per kanaal"))
        (body
         (en "You are reading this message through the new channel news
mechanism, congratulations!  This mechanism allows channel authors to provide
@dfn{news entries} that their users can view with @command{guix pull --news}.
Run @command{info \"(guix) Invoking guix pull\"} for more info.")
         (de "Sie lesen diese Meldung mit Hilfe des neuen Mechanismus, um
Neuigkeiten über Kanäle anzuzeigen — Glückwunsch! Mit diesem
Mechanismus können Kanalautoren Ihren Nutzern @dfn{Einträge zu
Neuigkeiten} mitteilen, die diese sich mit @command{guix pull --news}
anzeigen lassen können. Führen Sie @command{info \"(guix.de) Aufruf
von guix pull\"} aus, um weitere Informationen zu erhalten.")
         (es "Está leyendo este mensaje a través del mecanismo de noticias del
canal, ¡enhorabuena! Este mecanismo permite a las autoras de canales
proporcionar @dfn{entradas de noticias} que las usuarias pueden ver con
@command{guix pull --news}. Ejecute @command{info \"(guix.es) Invocación de
guix pull\"} para obtener más información.")
         (fr "Ce message t'arrive à travers le nouveau mécanisme d'information
des canaux, bravo !  Ce mécanisme permet aux auteur·rice·s de canaux de
fournir des informations qu'on peut visualiser avec @command{guix pull
--news}.  Tape @command{info \"(guix.fr) Invoquer guix pull\"} pour plus de
détails.")
         (nl "U leest dit bericht door een nieuw mechanisme om per kanaal
@dfn{nieuwsberichten} te verspreiden.  Proficiat!  Hiermee kunnen kanaalauteurs
mededelingen uitzenden die hun gebruikers met @command{guix pull --news} kunnen
lezen.  Voer @command{info \"(guix) Invoking guix pull\"} uit voor meer
informatie."))))
