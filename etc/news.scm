;; GNU Guix news, for use by 'guix pull'.
;;
;; Copyright © 2019-2026 Ludovic Courtès <ludo@gnu.org>
;; Copyright © 2019–2021, 2024 Tobias Geerinckx-Rice <me@tobias.gr>
;; Copyright © 2019, 2020 Miguel Ángel Arruga Vivas <rosen644835@gmail.com>
;; Copyright © 2019, 2020 Konrad Hinsen <konrad.hinsen@fastmail.net>
;; Copyright © 2019, 2020, 2021, 2023 Julien Lepiller <julien@lepiller.eu>
;; Copyright © 2019–2026 Florian Pelz <pelzflorian@pelzflorian.de>
;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;; Copyright © 2020, 2021 Mathieu Othacehe <m.othacehe@gmail.com>
;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;; Copyright © 2020-2025 Maxim Cournoyer <maxim@guixotic.coop>
;; Copyright © 2021–2023 Leo Famulari <leo@famulari.name>
;; Copyright © 2021 Zhu Zihao <all_but_last@163.com>
;; Copyright © 2021 Chris Marusich <cmmarusich@gmail.com>
;; Copyright © 2021, 2022 Maxime Devos <maximedevos@telenet.be>
;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;; Copyright © 2021 Andrew Tropin <andrew@trop.in>
;; Copyright © 2021, 2023 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;; Copyright © 2022 Thiago Jung Bauermann <bauermann@kolabnow.com>
;; Copyright © 2024 Oleg Pykhalov <go.wigust@gmail.com>
;; Copyright © 2024 Wilko Meyer <w@wmeyer.eu>
;; Copyright © 2024 Hilton Chain <hako@ultrarare.space>
;; Copyright © 2024 Liliana Marie Prikler <liliana.prikler@gmail.com>
;; Copyright © 2024 Vivien Kraus <vivien@planete-kraus.eu>
;; Copyright © 2024 Guillaume Le Vaillant <glv@posteo.net>
;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;; Copyright © 2024 Sebastian Dümcke <code@sam-d.com>
;; Copyright © 2025-2026 Anderson Torres <anderson.torres.8519@gmail.com>
;; Copyright © 2025 Roman Scherer <roman@burningswell.com>
;; Copyright © 2025 Jelle Licht <jlicht@fsfe.org>
;; Copyright © 2025 Rivulet Cedar <rivulet_cedar@yeah.net>
;; Copyright © 2025 jgart <jgart@dismail.de>
;; Copyright © 2025 Steve George <steve@futurile.net>
;; Copyright © 2026 Noé Lopez <noelopez@free.fr>

;;
;; Copying and distribution of this file, with or without modification, are
;; permitted in any medium without royalty provided the copyright notice and
;; this notice are preserved.

(channel-news
 (version 0)

 (entry (commit "a7c8e68dc51144a6d3981b770aca9c4897fc7c0c")
        (title
         (en "Records can refer to inherited values of thunked fields")
         (de "Vererbung in Verbundsobjekten erlaubt Verweise auf geerbte Werte verzögert ausgewerteter Felder")
         (pt "Registros podem referir-se a valores herdados de campos ainda não evaluados (thunks)"))
        (body
         (en "A new feature has been added to records—the facility used when
defining packages, services, operating systems, and many other things—that,
when inheriting from another record, allows you to refer to inherited field
values.  When defining package variants, you may have seen this pattern:

@lisp
(package
  (inherit gdb)
  (inputs (modify-inputs (package-inputs gdb)
            (delete \"guile\"))))
@end lisp

This can now be written like this:

@lisp
(package
  (inherit gdb)
  (inputs (modify-inputs inputs ;<- notice the change
            (delete \"guile\"))))
@end lisp

Within the @code{inputs} body, the @code{inputs} identifier is now bound to
the @dfn{inherited value}—i.e., the value this field inherits from @code{gdb}.
The same mechanism can be used when changing other input fields or the
@code{arguments} field.  It is less verbose but also
@uref{https://issues.guix.gnu.org/50335, more accurate}.

This change applies to \"thunked\" fields of all record types produced by
@code{define-record-type*}: @code{package}, @code{origin},
@code{operating-system}, @code{home-environment}, and so on.

Note that this is an incompatible change: the newly introduced
bindings—@code{inputs} in the example above—could shadow same-named bindings.")
         (de "Als neue Funktionalität kann man in Verbundsobjekten – also beim
Definieren von Paketen, Diensten, Betriebssystemen und vielem anderen – beim
Erben von einem anderen Verbundsobjekt auf die geerbten Werte verweisen. Zum
Definieren von Paketvarianten wurde früher etwas geschrieben wie:

@lisp
(package
  (inherit gdb)
  (inputs (modify-inputs (package-inputs gdb)
            (delete \"guile\"))))
@end lisp

Das lässt sich jetzt schreiben als:

@lisp
(package
  (inherit gdb)
  (inputs (modify-inputs inputs ;<- hier ändert sich was
            (delete \"guile\"))))
@end lisp

Innerhalb des @code{inputs}-Rumpfes ist der Bezeichner @code{inputs} jetzt
an den @dfn{geerbten Wert} gebunden, d.h. an den Wert, den das Feld von
@code{gdb} erbt. Derselbe Mechanismus kann beim Ändern anderer Felder mit
Eingaben oder im Feld @code{arguments} benutzt werden. Das spart Platz und
@uref{https://issues.guix.gnu.org/50335, verhält sich richtiger}.

Die Änderung wirkt sich auf verzögert ausgewertete \"thunked\"-Felder aller
Verbundstypen aus, die mit @code{define-record-type*} erstellt wurden:
@code{package}, @code{origin}, @code{operating-system}, @code{home-environment}
und so weiter.

Hinweis: das ist eine inkompatible Änderung. Die neu eingeführte Bindung –
@code{inputs} im obigen Beispiel – kann gleichnamige Bindungen überschatten.")
         (pt "Um novo recurso foi adicionado aos registros - a ferramenta
usada quando se definem pacotes, serviços, sistemas operacionais, e muitas
outras coisas - que, quando herdados de outro registro, te permitem referir
aos valores de campos herdados.  Durante a definição de variantes de pacotes,
você pode encontrar este padrão:

@lisp
(package
  (inherit gdb)
  (inputs (modify-inputs (package-inputs gdb)
            (delete \"guile\"))))
@end lisp

Agora isto pode ser escrito desta forma:

@lisp
(package
  (inherit gdb)
  (inputs (modify-inputs inputs ;<- Note a modificação
            (delete \"guile\"))))
@end lisp

Dentro do corpo de @code{inputs}, o identificador @code{inputs} agora está
atrelado ao @dfn{valor herdado} - id est, ao valor que este campo herda de
@code{gdb}.  O mesmo mecanismo pode ser empregado ao modificar outros campos
da entrada ou ao campo @code{arguments}.  É menos verboso mas também
@uref{https://issues.guix.gnu.org/50335, é mais preciso}.

Esta mudança se aplica aos campos \"thunk-ados\" de todos os tipos de registro
produzidos por @code{define-record-type*}: @code{package}, @code{origin},
@code{operating-system}, @code{home-environment}, e assim por diante.

Note que esta é uma modificação incompatível: as ligações introduzidas
recentemente - @code{inputs} no exemplo acima - podem sombrear ligações de
mesmo nome.")))

 (entry (commit "b52ce9041ad58aeababd2d50d3e72bc23dffff60")
        (title
         (en "GNOME 48 is now available")
         (fr "GNOME 48 maintenant disponible")
         (pt "GNOME 48 já está disponível")
         (de "GNOME 48 jetzt verfügbar"))
        (body
         (en "The GNOME desktop was updated from version 46 to 48, bringing
accent colors, an improved Files (Nautilus) application, a new image
viewer (Loupe), and more:

@itemize
@item @url{https://release.gnome.org/47/, GNOME 47 release notes}
@item @url{https://release.gnome.org/48/, GNOME 48 release notes}
@end itemize")
         (fr "La version 48 de l’environnement de bureau GNOME est maintenant
disponible. Les nouvelles fonctionnalitées incluent: les couleurs d’accent,
l’application Fichiers (Nautilus) améliorée, un nouveau visionneur
d’images (Loupe), et plus:

@itemize
@item @url{https://release.gnome.org/47/, GNOME 47 release notes}
@item @url{https://release.gnome.org/48/, GNOME 48 release notes}
@end itemize")
         (pt "GNOME desktop foi atualizado da versão 46 para a 48, trazendo
cores acentuadas, um aplicativo de arquivos (Nautilus) melhorado, um novo
visualizador de imagens (Loupe), e mais:

@itemize
@item @url{https://release.gnome.org/47/, Notas de lançamento do GNOME 47}
@item @url{https://release.gnome.org/48/, Notas de lançamento do GNOME 48}
@end itemize")
         (de "Die GNOME-Desktopumgebung wurde von Version 46 auf 48
aktualisiert. Neue Funktionen in dieser Version sind die Möglichkeit, eine
Akzentfarbe zu wählen, Verbesserungen in der Dateien-Anwendung (Nautilus), ein
neuer Bildbetrachter (Loupe), und mehr. Für eine ausführlichere Liste an
Änderungen, siehe:

@itemize
@item @url{https://release.gnome.org/47/, GNOME 47 release notes}
@item @url{https://release.gnome.org/48/, GNOME 48 release notes}
@end itemize")))

 (entry (commit "d659fe8666c4bc38fcbdbe7b7a35101f2d7cc41b")
        (title
         (en "Potential security vulnerability in glibc")
         (de "Mögliche Sicherheitslücke in glibc")
         (pt "Potencial vulnerabilidade de segurança em glibc"))
        (body
         (en "Guix adds the environment variable @code{GUIX_LOCPATH} to glibc,
however it was not added to potentially unsafe variables to be unset in
privileged environments.  A CVE number is pending for this issue.  This has
been fixed with a graft to glibc and users should update all profiles,
reconfigure their system, and reboot.")
         (de "Guix fügt die Umgebungsvariable @code{GUIX_LOCPATH} zu glibc
hinzu, aber sie wurde nicht zu den möglicherweise unsicheren Variablen
hinzugefügt, die in privilegierten Umgebungen zurückgesetzt werden.  Eine
CVE-Nummer hierfür wird noch zugewiesen.  Um den Fehler zu beheben, stellen
wir eine Veredelung für glibc bereit; Nutzerinnen und Nutzer sollten all ihre
Profile aktualisieren, ihr System rekonfigurieren und neustarten.")
         (pt "Guix adiciona a variável de ambiente @code{GUIX_LOCPATH} à
glibc, todavia ela não foi adicionada às variáveis potencialmente inseguras
a serem desabilitadas em ambientes privilegiados.  Este incidente está
pendente de um identificador CVE.  Isto foi resolvido com um enxerto à glibc,
e os usuários devem atualizar todos os seus perfis, reconfigurando seu
sistema e reiniciando-o.")))

 (entry (commit "6d4cb99a15da7f4fd55f956c55f4f4aacfcc7742")
        (title
         (en "@code{%desktop-services} now includes GDM on AArch64")
         (de "@code{%desktop-services} enthält jetzt GDM auf AArch64")
         (pt "@code{%desktop-services} agora inclui GDM no AArch64"))
        (body
         (en "On Guix System in an operating system configuration, your value for the
@code{services} field is based on @code{%desktop-services} for desktop
machines.

The @code{%desktop-services} contains a login manager that is
architecture-dependent.  Previously, GDM has been used on x86_64 and SDDM on
other architectures.

Now, GDM is used on all 64-bit architectures.  This affects only AArch64 in
the end, as other 64-bit architectures aren't mature enough to use
%desktop-services, yet.

In case you're on AArch64 and you've changed the login manager, you will have
to adapt your config to remove @code{gdm-service-type} instead of
@code{sddm-service-type}.")
         (de "Auf Guix System liegen in einer Betriebssystemkonfiguration Ihrem
Wert des @code{services}-Feldes auf Desktop-Maschinen die
@code{%desktop-services} zugrunde.

Zu den @code{%desktop-services} gehört eine Anmeldeverwaltung, die sich je nach
Architektur unterscheidet. Bisher wurde GDM auf x86_64 benutzt und SDDM auf
anderen Architekturen.

Ab jetzt wird GDM auf allen 64-Bit-Architekturen benutzt. Das betrifft
letztlich nur AArch64, weil andere 64-Bit-Architecturen bisher noch nicht weit
genug fortgeschritten sind, um die %desktop-services zu benutzen.

Falls Sie auf AArch64 sind und die Anmeldeverwaltung geändert haben, müssen Sie
Ihre Konfiguration ändern und dort @code{gdm-service-type} entfernen statt
@code{sddm-service-type}.")
         (pt "Em uma configuração de sistema operacional do Guix System, seu
valor para o campo @code{services} é baseado em @code{%desktop-services} para
máquinas desktop.

@code{%desktop-services} contém um gerenciador de login dependente de
arquitetura.  Anteriormente, GDM fora usado em x86_64 e SDDM nas demais.

Agora, GDM é usado em todas as arquiteturas de 64 bits.  Esta mudança afeta
apenas a AArch64 no frigir dos ovos, dado que as demais arquiteturas de 64
bits não são maduras o bastante para usar @code{%desktop-services}, por
enquanto.

No caso de você estar em um AArch64 e ter modificado o gerenciador de login,
você terá que adaptar sua configuração para remover @code{gdm-service-type} em
vez de @code{sddm-service-type}.")))

 (entry (commit "d3b79beaa806452dceaffb8b211b69dc4c346aba")
        (title
         (en "Linux-libre 5.4 no longer supported")
         (de "Linux-libre 5.4 nicht mehr unterstützt")
         (pt "Linux-libre 5.4 não é mais suportado"))
        (body
         (en "The linux-libre 5.4 kernel series is no longer supported
upstream and will be removed from GNU Guix soon:

https://www.kernel.org/category/releases.html

This kernel series was first added to Guix in 2019 by Mark Weaver, at version
5.4.5.  After ~297 updates delivered to Guix users, we have deployed the final
update, 5.4.302.

The next kernel series removals are scheduled for December 2026, when the Linux
long-term support team is planning to stop supporting four different kernel
series: 6.12, 6.6, 5.15, and 5.10.")
         (de "Die Versionsreihe 5.4 des Linux-libre-Kernels wird von dessen
Anbieter nicht mehr unterstützt und wird bald aus GNU Guix entfernt werden:

https://www.kernel.org/category/releases.html

Diese Kernel-Reihe wurde ursprünglich 2019 zu Guix hinzugefügt von Mark Weaver
mit Version 5.4.5.  Nach ~297 Aktualisierungen, die Guix seinen Nutzern
bereitgestellt hat, haben wir nun die letzte Aktualisierung
ausgeliefert, 5.4.302.

Der weitere Plan von Linux’ Team für Langzeitunterstützung sieht vor, als
Nächstes die Unterstützung für vier verschiedene Kernel-Reihen im Dezember 2026
zu beenden: 6.12, 6.6, 5.15 und 5.10.")
         (pt "A série 5.4 do kernel linux-libre não é mais suportada pelo
fornecedor e será removida do GNU Guix em breve:

https://www.kernel.org/category/releases.html

Esta série de kernels foi adicionada inicialmente ao Guix em 2019 por Mark
Weaver, na versão 5.4.5.  Após ~297 atualizações entregues aos usuários do
Guix, implantamos a atualização final, 5.4.302.

As próximas remoções de séries do kernel estão agendadas para dezembro de
2026, quando o time de suporte de longo prazo planeja interromper o suporte a
quatro séries diferentes do kernel: 6.12, 6.6, 5.15 e 5.10.")))

 (entry (commit "14bb0192fde79909999c3880b6b66aa22499eb11")
        (title
         (en "Linux-libre 6.16 removed from Guix")
         (de "Linux-libre 6.16 wurde aus Guix entfernt")
         (pt "Linux-libre 6.16 removido do Guix")
         (zh "Linux-libre 6.16 已从 Guix 中移除"))
        (body
         (en "The linux-libre 6.16 kernel series has been removed from Guix, as
it is no longer supported upstream.  The 6.17 kernel series is now the
default.")
         (de "Die Versionsreihe 6.16 des Linux-libre-Kernels wurde aus GNU Guix
entfernt, weil sie von dessen Anbieter nicht mehr unterstützt wird.
Kernel-Versionsreihe 6.17 ist jetzt vorgegeben.")
         (pt "A série 6.16 do kernel linux-libre foi removida do Guix, dado
que não é mais suportada pelo fornecedor.  A série 6.17 é o padrão atual.")
         (zh "由于上游不再支持，GNU Guix 已移除 6.16 系列的 linux-libre 内核。6.17
内核系列现已成为默认版本。")))

 (entry (commit "8daae75de3670594dfcb63b6ae6f1603ca6f7971")
        (title
         (en "Fundraising to sustain and strengthen Guix")
         (de "Spendensammlung zur Sicherung und Stärkung von Guix")
         (pt "Campanha de arrecadação para sustentar e fortalecer o Guix"))
        (body
         (en "We've launched a fundraising campaign for Guix to fund the
project's expenses.  The goal is for Guix to be sustainable, meaning that
donations pay for shared resources like infrastructure and to promote the
project.  To find out more see the blog post
@url{https://guix.gnu.org/blog/2025/fundraising-campaign-to-sustain-gnu-guix/}.

You can give a @emph{recurring} or @emph{one-time} donation through either the
FSF or Guix Foundation.  Recurring donations are ideal because they align with
the project's ongoing costs, but we value and appreciate any support.  Every
donation gets us a step closer to being sustainable.

To take part please go to the Donate page:
@url{https://guix.gnu.org/donate/}.

Thanks to everyone who takes part!")
         (de "Wir haben eine Spendenaktion für Guix eröffnet, um die Ausgaben
des Projekts zu finanzieren.  Das Ziel ist, dass Guix nachhaltig wirtschaftet,
also gemeinschaftlich genutzte Ressourcen wie Infrastruktur und das Voranbringen
des Projekts durch Spenden finanziert werden.  Genaueres lesen Sie im
Blog-Eintrag
@url{https://guix.gnu.org/blog/2025/fundraising-campaign-to-sustain-gnu-guix/}.

Sie können eine @emph{regelmäßige} oder @emph{einmalige} Spende an entweder die
FSF oder die Guix Foundation geben.  Regelmäßige Spenden bevorzugen wir, weil
sie mit den laufenden Kosten des Projekts abgestimmt werden, aber jede
Unterstützung wird wertgeschätzt.  Mit jeder Spende kommen wir näher an die
dauerhafte Finanzierbarkeit.

Um mitzumachen, besuchen Sie unsere Spendenseite:
@url{https://guix.gnu.org/donate/}.

Wir danken allen Teilnehmern!")
         (pt "Lançamos uma campanha de arrecadação para o Guix a fim de
custear as expensas do projeto.  O objetivo é que Guix seja sustentável, o que
significa que as doações pagam pelos recursos compartilhados como a
infraestrutura e para a promoção do projeto.  Para mais informações acesse a
postagem no blog:
@url{https://guix.gnu.org/blog/2025/fundraising-campaign-to-sustain-gnu-guix/}.

Você pode dar uma doação @emph{recorrente} ou @emph{unitária} mediante tanto à
FSF quanto à Fundação Guix.  Doações recorrentes são ideais porque elas se
alinham com os custos contínuos do Guix, todavia valorizamos e apreciamos
qualquer suporte.  Qualquer doação nos aproxima de sermos sustentáveis.

Para fazer parte, por favor vá à página de doações:
@url{https://guix.gnu.org/donate/}.

Obrigado a todos que participarem!")))

 (entry (commit "61081db5d11dca328c8b2c8d1d5d4ef9ba662059")
        (title
         (en "Linux-libre updated to 6.16")
         (de "Linux-libre wird auf 6.16 aktualisiert")
         (pt "Linux-libre atualizado para 6.16"))
        (body
         (en "The default linux-libre package has been updated to the 6.16
release series.  The 6.15 kernel series will be removed soon because it is no
longer supported upstream.")
         (de "Das standardmäßig verwendete @code{linux-libre}-Paket wurde auf
die Versionsreihe 6.16 aktualisiert.  Die Kernel-Reihe 6.15 wird bald entfernt
werden, weil sie von dessen Anbieter nicht mehr unterstützt wird.")
         (pt "O pacote @code{linux-libre} padrão foi atualizado para a série de
lançamentos 6.16.  A série de kernels 6.15 será removida em breve porque não é
mais suportada pelo fornecedor.")))

 (entry (commit "6e57701013a69a20df6983f5e05aa3598073282f")
        (title
         (en "Linux-libre 6.16 now available")
         (de "Linux-libre 6.16 jetzt verfügbar")
         (pt "Linux-libre 6.16 já disponível"))
        (body
         (en "The Linux-libre 6.16 kernel series is now available in GNU Guix.
This kernel series was added to Guix without a custom kernel configuration for
the armhf-linux system (32-bit ARM with floating point support, also called
'armv7+'), due to lack of interest.  The generic armhf-linux kernel packages are
still available.

Soon, linux-libre 6.16 will be made the default linux-libre package because 6.15
is no longer supported upstream.")
         (de "Die Kernel-Versionsreihe 6.16 von Linux-libre ist ab jetzt in
GNU Guix verfügbar.  Diese Kernel-Versionsreihe wurde zu Guix ohne eine
angepasste Kernel-Konfiguration für das armhf-linux-System hinzugefügt (d.h.
32-Bit-ARM mit Gleitkommaunterstützung, auch bekannt als „armv7+“), weil sich
zu wenige Menschen dafür interessieren.  Die generischen
armhf-Linuxkernelpakete bleiben verfügbar.

Bald wird Linux-libre 6.16 zum vorgegebenen Linux-libre-Paket, weil 6.15 vom
Anbieter nicht mehr unterstützt wird.")
         (pt "A série 6.16 do kernel Linux-libre já está disponível no
GNU Guix.  Esta série foi adicionada ao Guix sem uma configuração
personalizada para o sistema armhf-linux (ARM de 32 bits com suporte a
ponto flutuante, também chamada 'armv7+'), em razão da falta de
interesse.  Os pacotes de kernel para o armhf-linux ainda estão
disponíveis.

Em breve, linux-libre 6.16 será feita o pacote padrão linux-libre, já
que 6.15 não é mais suportada pelo fornecedor.")))

 (entry (commit "1618ca7aa2ee8b6519ee9fd0b965e15eca2bfe45")
        (title
         (en "New @command{guix-daemon} privilege escalation vulnerability
fixed")
         (de "Weitere Schwachstelle in @command{guix-daemon} zur
Rechteausweitung behoben")
         (pt "Nova vulnerabilidade de escalação de privilégio em @command{guix-daemon} consertada"))
        (body
         (en "A new vulnerability was identified and fixed in the build
daemon, @command{guix-daemon} (CVE ID assignment pending).  Everyone is
strongly advised to upgrade @command{guix-daemon}.  Guix System users can do
this with commands along these lines:

@example
sudo guix system reconfigure /run/current-system/configuration.scm
sudo herd restart guix-daemon
@end example

If you are using Guix on another distro, run @command{info \"(guix) Upgrading
Guix\"} or visit
@uref{https://guix.gnu.org/manual/devel/en/html_node/Upgrading-Guix.html} to
learn how to upgrade Guix.

This vulnerability lies in the @code{builtin:download} derivation builder:
anyone with access to the daemon can craft a @code{content-addressed-mirrors}
Scheme procedure that the daemon will execute as a build user (or as the
daemon user, when running @command{guix-daemon} unprivileged).  An attacker
could use this to gain build user privileges and thereafter compromise builds
performed on the system.  See @uref{https://codeberg.org/guix/guix/pulls/2419}
for more information.")
         (de "Eine weitere Schwachstelle im Erstellungsdaemon,
@command{guix-daemon}, wurde erkannt und behoben (CVE-ID-Zuweisung steht noch
aus).  Wir raten allen deutlich dazu, @command{guix-daemon} zu aktualisieren.
Auf Guix System können Nutzer dazu Befehle verwenden wie:

@example
sudo guix system reconfigure /run/current-system/configuration.scm
sudo herd restart guix-daemon
@end example

Wenn Sie Guix auf einer anderen Distribution verwenden, erfahren Sie mit dem
Befehl @command{info \"(guix.de) Aktualisieren von Guix\"} oder auf
@uref{https://guix.gnu.org/manual/devel/de/html_node/Aktualisieren-von-Guix.html},
wie Sie Guix aktualisieren.

Die Schwachstelle besteht im Ableitungsersteller @code{builtin:download}:
Jeder, der Zugriff auf den Daemon hat, kann eine Scheme-Prozedur
@code{content-addressed-mirrors} anfertigen, die der Daemon als
Erstellungsbenutzer ausführen wird (oder als der Daemon-Benutzer, wenn
@command{guix-daemon} unprivilegiert ausgeführt wird).  Ein Angreifer kann das
ausnutzen, um sich die Berechtigungen eines Erstellungsbenutzers anzueignen, und
kann von da an auf dem System durchgeführte Erstellungen kompromittieren.  Siehe
@uref{https://codeberg.org/guix/guix/pulls/2419} für mehr Informationen.")
         (pt "Uma nova vulnerabilidade foi identificada e consertada no daemon
de construção, @command{guix-daemon} (pendente de atribuição de identificador
CVE).  Aconselhamos fortemente todos a atualizar o @command{guix-daemon}.
Usuários do Sistema Guix podem fazer isso com comandos nestas linhas:

@example
sudo guix system reconfigure /run/current-system/configuration.scm
sudo herd restart guix-daemon
@end example

Se você estiver usando Guix em outra distro, execute @command{info
\"(guix.pt_BR) Atualizando o Guix\"} ou visite
@uref{https://guix.gnu.org/manual/devel/pt-br/html_node/Atualizando-o-Guix.html}
a fim de aprender como atualizar o Guix.

Esta vulnerabilidade está situada no construtor de derivação
@code{builtin:download}: qualquer um com acesso ao daemon pode engenhar um
procedimento Scheme @code{content-addressed-mirrors} que o daemon executará
como um usuário construtor (ou como o usuário daemon, quando executando
@command{guix-daemon} sem privilégios).  Um atacante poderia usar isto para
obter privilégios de usuário construtor e por conseguinte comprometer
construções executadas no sistema.  Veja
@uref{https://codeberg.org/guix/guix/pulls/2419} para mais informação.")))

 (entry (commit "3e45fc0f37d027516ac3d112ca7768d698eeac74")
        (title
         (en "All Rust applications repackaged")
         (de "Alle Rust-Anwendungen wurden neu verpackt")
         (pt "Reempacotamento de todas as aplicações Rust"))
        (body
         (en "We have switched to
@url{https://guix.gnu.org/blog/2025/a-new-rust-packaging-model/, a new Rust
packaging model} and repackaged all Rust applications.  Rust library
packages (those previously in @code{(gnu packages crates-*)} modules) have been
moved to @url{https://codeberg.org/guix/guix-rust-past-crates, the
@code{guix-rust-past-crates} channel}, compatible with Rust 1.82.  If your
packages depend on them, please add this additional channel before migrating to
the new packaging model.")
         (de "Wir sind auf
@url{https://guix.gnu.org/blog/2025/a-new-rust-packaging-model/, ein neues
Paketmodell für Rust} umgestiegen und haben alle Rust-Anwendungen neu verpackt.
Rust-Bibliothekspakete (die bisher in @code{(gnu packages crates-*)}-Modulen
verpackt waren) wurden in @url{https://codeberg.org/guix/guix-rust-past-crates,
den Kanal @code{guix-rust-past-crates}} verschoben, der mit Rust 1.82 kompatibel
ist.  Wenn Sie Pakete haben, die von diesen abhängen, fügen Sie bitte diesen
weiteren Kanal hinzu, ehe Sie auf das neue Paketmodell wechseln.")
         (pt "Mudamos para
@url{https://guix.gnu.org/blog/2025/a-new-rust-packaging-model/, um novo
modelo de empacotamento para Rust} e reempacotamos todas as aplicações Rust.
Pacotes de bibliotecas Rust (aqueles previamente em módulos @code{(gnu
packages crates-*)}) foram movidos para
@url{https://codeberg.org/guix/guix-rust-past-crates, o canal
@code{guix-rust-past-crates}}, compatível com Rust 1.82.  Se seus pacotes
dependem deles, adicione este canal antes de migrar para o novo modelo de
empacotamento.")))

 (entry (commit "8b7208cb8e6b3de3d26c9cc5129a0024d3900642")
        (title
         (en "Translations moved to Codeberg")
         (de "Übersetzungen sind auf Codeberg umgezogen")
         (pt "Traduções movidas para o Codeberg"))
        (body
         (en "GNU Guix can now be translated @i{via} Weblate on Codeberg at
@url{https://translate.codeberg.org/projects/guix/#components} next to where
the Guix repositories and issue tracker are located.  Reviewing that existing
translations are useful is of great help as well; please modify where needed.
We are grateful to Fedora for providing the Weblate translation platform to us
so far.  Furthermore, Framagit had been hosting the intermediate repository
where Weblate places translations before maintainers verify their syntax.  The
intermediate repository is now hosted at
@url{https://codeberg.org/guix/translations}.  If you had cloned a copy to
watch the repository previously, please change the repository's upstream URL
accordingly in your clone's @file{.git/config} file.

See @samp{info \"(guix)Translating Guix\"} for more information on how
translations are organized.")
         (de "GNU Guix kann jetzt über Weblate auf Codeberg unter
@url{https://translate.codeberg.org/projects/guix/#components} übersetzt werden,
wo auch die Guix-Repositorys und Fehlerberichte abrufbar sind.  Auch zu
überprüfen, ob bestehende Übersetzungen nützlich sind, hilft uns ebenfalls sehr;
ändern Sie sie ruhig.  Wir danken Fedora dafür, dass sie uns die
Weblate-Übersetzungsplattform bisher bereitgestellt haben.  Des Weiteren hatte
Framagit das zwischengeschaltete Repository gehostet, wo Weblate Übersetzungen
ablegt, solange Betreuer ihre Syntax noch nicht geprüft haben.  Das
zwischengeschaltete Repository wird jetzt auf
@url{https://codeberg.org/guix/translations} gehostet.  Wenn Sie zuvor eine
Kopie davon geklont hatten, um Änderungen zu verfolgen, ändern Sie bitte die
Anbieter-URL des Repository in der @file{.git/config}-Datei Ihres Klons.

Siehe @samp{info \"(guix.de)Guix übersetzen\"} für weitere Informationen, wie
Übersetzungen organisiert sind.")
         (pt "GNU Guix agora pode ser traduzido @i{via} Weblate no Codeberg em
@url{https://translate.codeberg.org/projects/guix/#components} próximo a onde
os repositórios e sinalizadores de problemas do Guix estão localizados.
Revisar se as traduções existentes são úteis é de grande ajuda também; por
favor modifique-as quando necessário.  Somos gratos ao projeto Fedora por
fornecer a plataforma Weblate de traduções para nós até aqui.  Adicionalmente,
Framagit tem hospedado o repositŕoio intermediário onde o Weblate insere as
traduções antes de os mantenedores verificarem sua sintaxe.  As traduções
intermediárias agora estão hospedadas em
@url{https://codeberg.org/guix/translations}.  Se você clonou uma cópia para
acompanhar o repositório anteriormente, modifique a URL do repositório
fornecedor adequadamente no arquivo @file{.git/config} do repositório clone.

Veja @samp{info \"(guix.pt_BR) Traduzindo o Guix\"} para mais informação sobre
como as traduções são organizadas.")))

 (entry (commit "4aff8591836e3b4125dcbed651efc8607de53280")
        (title
         (en "New build system added for @code{clasp-cl}")
         (de "Neues Erstellungssystem für @code{clasp-cl} hinzugefügt")
         (es "Se agregó un nuevo sistema de construcción para @code{clasp-cl}")
         (pt "Novo sistema de construção adicionado para @code{clasp-cl}"))
        (body
         (en "A new build system has been introduced for @code{clasp-cl}.
             Common Lisp devs using Guix can start using this new build
             system similarly to others such as @code{asdf-build-system/ecl}.")
         (de "Ein neues Erstellungssystem wurde für @code{clasp-cl} eingeführt.
             Entwickler für Common Lisp, die Guix nutzen, können dazu jetzt
             dieses Erstellungssystem verwenden ähnlich wie die anderen, etwa
             @code{asdf-build-system/ecl}.")
         (es "Se ha introducido un nuevo sistema de construcción para
             @code{clasp-cl}. Los devs de Common Lisp que usan Guix pueden
             empezar a usar este nuevo sistema de construcción de manera
             similar a otros como @code{asdf-build-system/ecl}.")
         (pt "Um novo sistema de construção foi introduzido para
@code{clasp-cl}.  Desenvolvedores Common Lisp usando Guix podem usar este novo
sistema de construção semelhantemente a outros como @code{asdf-build-system/ecl}.")))

 (entry (commit "30a5d140aa5a789a362749d057754783fea83dde")
        (title
         (en "@command{guix-daemon} privilege escalation vulnerabilities
fixed (CVE-2025-46415, CVE-2025-46416)")
         (de "Schwachstellen in @command{guix-daemon} zur Rechteausweitung
behoben (CVE-2025-46415, CVE-2025-46416)")
         (pt "Vulnerabilidades de escalação de privilégios no
@command{guix-daemon} consertadas (CVE-2025-46415, CVE-2025-46416)"))
        (body
         (en "Vulnerabilities in the build daemon, @command{guix-daemon}, were
identified and fixed.  One vulnerability would allow any user on the system
that can interact with the daemon to potentially corrupt new packages built
locally (CVE-2025-46416).  With the other vulnerability (CVE-2025-46415), if
@command{guix-daemon} is running as root, it is also possible to escalate to
root privileges.  CVE-2025-52991, CVE-2025-52992, and CVE-2025-52993 were
identified as additional opportunities that could have prevented the proposed
exploits.

Everyone is strongly advised to upgrade @command{guix-daemon}.  Guix System
users can do this with commands along these lines:

@example
sudo guix system reconfigure /run/current-system/configuration.scm
sudo herd restart guix-daemon
@end example

If you are using Guix on another distro, run @command{info \"(guix) Upgrading
Guix\"} or visit
@uref{https://guix.gnu.org/manual/devel/en/html_node/Upgrading-Guix.html} to
learn how to upgrade Guix.

The root cause of the vulnerability was the ability of a @dfn{fixed-output
derivation} build process to smuggle a file descriptor to the store or to a
setuid program to an outside process @i{via} an abstract Unix-domain socket.
See @uref{https://codeberg.org/guix/guix/pulls/788} for more information.")
         (de "Schwachstellen im Erstellungs-Daemon @command{guix-daemon} wurden
erkannt und behoben.  Mit einer der Schwachstellen konnte jeder Benutzer des
Systems, dem eine Interaktion mit dem Daemon möglich ist, neue lokal erstellte
Pakete in vielen Fällen manipulieren (CVE-2025-46416).  Mit der anderen
Schwachstelle (CVE-2025-46415) konnten, sofern @command{guix-daemon} als
Administratornutzer root läuft, sogar alle Rechte des root-Benutzers erlangt
werden.  Dabei wurden CVE-2025-52991, CVE-2025-52992 und CVE-2025-52993 als
zusätzliche Gelegenheiten erkannt, durch die der vorgeschlagene Angriff
vereitelt hätte werden können.

Wir raten allen deutlich dazu, @command{guix-daemon} zu aktualisieren. Auf
Guix System können Nutzer dazu Befehle verwenden wie:

@example
sudo guix system reconfigure /run/current-system/configuration.scm
sudo herd restart guix-daemon
@end example

Wenn Sie Guix auf einer anderen Distribution verwenden, erfahren Sie mit dem
Befehl @command{info \"(guix.de) Aktualisieren von Guix\"} oder auf
@uref{https://guix.gnu.org/manual/devel/de/html_node/Aktualisieren-von-Guix.html},
wie Sie Guix aktualisieren.

Die eigentliche Ursache war, dass der Erstellungsprozess einer @dfn{Ableitung
mit fester Ausgabe} die Möglichkeit hatte, über einen abstrakten
Unix-Domain-Socket einen Dateideskriptor in den Store
hinein oder auf ein setuid-Programm zu einem Prozess außerhalb heraus zu
schmuggeln.  Siehe @uref{https://codeberg.org/guix/guix/pulls/788} für mehr
Informationen.")
         (pt "Vulnerabilidades no daemon de construção, @command{guix-daemon},
foram identificadas e consertadas.  Uma vulnerabilidade permitiria a qualquer
usuário no sistema que possa interagir com o daemon potencialmente corromper
novos pacotes construídos localmente (CVE-2025-46416).  Com a outra
vulnerabilidade (CVE-2025-46415), se @command{guix-daemon} é executado como
superusuário, é possível também escalar privilégios de superusuário.
CVE-2025-52991, CVE-2025-52992 e CVE-2025-52993 foram identificadas como
oportunidades adicionais que poderiam prevenir as explorações propostas.

Aconselhamos fortemente todos a atualizar o @command{guix-daemon}.  Usuários
do Sistema Guix podem fazer isso com comandos nestas linhas:

@example
sudo guix system reconfigure /run/current-system/configuration.scm
sudo herd restart guix-daemon
@end example

Se você estiver usando Guix em outra distro, execute @command{info
\"(guix.pt_BR) Atualizando o Guix\"} ou visite
@uref{https://guix.gnu.org/manual/devel/pt-br/html_node/Atualizando-o-Guix.html}
a fim de aprender como atualizar o Guix.

A raiz da vulnerabilidade estava na capacidade de um processo de construção de
uma @dfn{derivação de ponto fixo} contrabandear um descritor de arquivo para o
armazém ou para o programa setuid para um processo externo @i{via} um socket
abstrato de domínio Unix.  Veja
@uref{https://codeberg.org/guix/guix/pulls/788} para mais informação.")))

 (entry (commit "78d4b1e52c731502b29288ab6975bd9efa91392a")
        (title
         (en "New services for /etc/profile.d and /etc/bashrc.d")
         (de "Neue Dienste für /etc/profile.d und /etc/bashrc.d")
         (pt "Novos serviços para /etc/profile.d e /etc/bashrc.d")
         (zh "为 /etc/profile.d 和 /etc/bashrc.d 提供新服务"))
        (body
         (en "Two new services, @code{etc-profile-d-service-type} and
@code{etc-bashrc-d-service-type}, can now be used to configure and extend your
shell.  The services accept a list of file-like objects that must have the
@file{.sh} file name extension.  @code{etc-bashrc-d-service-type} is now used
instead of skeleton files to provision the Guix-provided Bash
aliases (@file{/etc/bashrc.d/aliases.sh}).  It is also used to configure the
@code{bash-completion} package, by provisioning
@file{/etc/bashrc.d/bash_completion.sh}.  If your @code{operating-system}
configuration does not include the @code{%base-services} or
@code{%desktop-services}, you may want to add
@code{(service etc-bashrc-d-service-type)} to your services, so Bash
completion will continue working for new users.  See @samp{info \"(guix) Service
Reference\"} for extra information and examples.")
         (de "Zwei neue Dienste, @code{etc-profile-d-service-type} und
@code{etc-bashrc-d-service-type}, können jetzt benutzt werden, um Ihre Shell zu
konfigurieren und zu erweitern.  Die Dienste nehmen eine Liste dateiartiger
Objekte entgegen, deren Dateinamen auf die Dateinamenserweiterung @file{.sh}
enden müssen.  @code{etc-bashrc-d-service-type} wird jetzt anstelle von
Skeleton-Dateien benutzt, um von Guix ausgelieferte Bash-Aliasse bereitzustellen
(@file{/etc/bashrc.d/aliases.sh}). Mit ihm wird auch das
@code{bash-completion}-Paket eingerichtet, indem
@file{/etc/bashrc.d/bash_completion.sh} bereitgestellt wird.  Wenn Ihre
Betriebssystemkonfiguration @code{%base-services} oder @code{%desktop-services}
nicht beinhaltet, möchten Sie vielleicht
@code{(service etc-bashrc-d-service-type)} in Ihre Dienstliste aufnehmen, damit
Bash-Komplettierung weiterhin für neue Benutzer Befehle vervollständigt.  Siehe
@samp{info \"(guix.de) Service-Referenz\"} für weitere Informationen und
Beispiele.")
         (pt "Dois novos serviços, @code{etc-profile-d-service-type} e
@code{etc-bashrc-d-service-type}, podem agora ser usados para configurar e
estender sua shell.  Os serviços aceitam uma lista de objetos similares a
arquivos que devem ter a extensão de arquivo @file{.sh}.
@code{etc-bashrc-d-service-type} é agora usado em vez de arquivos-esqueleto
para provisionar os aliases Bash providos pelo
Guix (@file{/etc/bashrc.d/aliases.sh}).  Também é usado para configurar o
pacote provisionando @file{/etc/bashrc.d/bash_completion.sh}.  Se sua
configuração @code{operating-system} não inclui @code{%base-services} e
@code{%desktop-services}, você pode adicionar @code{(service
etc-bashrc-d-service-type)} a seus serviços, de forma que a completação do
Bash continue funcionando para novos usuários.  Veja @samp{info \"(guix.pt_BR)
Referência de Service\"} para informação e exemplos adicionais.")
         (zh "您现在可在 Guix 中使用新服务 @code{etc-profile-d-service-type}
和 @code{etc-bashrc-d-service-type} 配置和扩展 shell。这些服务接受一个必须具有
@file{.sh} 文件扩展名的类文件对象列表。

Guix 现在用 @code{etc-bashrc-d-service-type} 取代了 skeleton
文件（skeleton file），以提供 Bash
别名（@file{/etc/bashrc.d/aliases.sh}）。该服务也能通过提供
@file{/etc/bashrc.d/bash_completion.sh} 来配置
@code{bash-completion} 包。

如果您的 @code{operating-system} 配置不包含 @code{%base-services} 或
@code{%desktop-services}，则可能需要将
@code{(service etc-bashrc-d-service-type)} 添加到 @code{services}
列表中，以确保 Bash 补全功能对新用户继续有效。更多信息和示例请参阅
@samp{info \"(guix.zh_CN) 服务参考\"}。")))

 (entry (commit "3b6e499d5e635e2189f1bf60279dbf5175c404bd")
        (title
         (en "Guix source code repository moved to Codeberg")
         (de "Repository mit Guix’ Quellcode ist auf Codeberg umgezogen")
         (fr "Dépôt de code source de Guix déplacé sur Codeberg")
         (pt "O repositório código-fonte do Guix foi movido para o Codeberg")
         (zh "Guix 源代码仓库已迁移至 Codeberg"))
        (body
         (en "The canonical URL of the Guix repository is now
@url{https://git.guix.gnu.org/guix.git}.

No action is needed on your side: @command{guix pull} will now automatically
fetch from the new URL, unless your @file{channels.scm} files refer to the
former URL; in that case, you are encouraged to update it, though the former
URL at @code{git.savannah.gnu.org} will remain valid for at least one
year.

It is now possible to report bugs and to propose changes using the issue and
pull-request mechanisms at @url{https://codeberg.org/guix/guix}.  The
@email{bug-guix@@gnu.org} and @email{guix-patches@@gnu.org} mailing lists
remain available, including for new bugs and patches, until December 31st,
2025.

Guix Consensus Document (GCD) 002, which is the result of community
consensus-building, documents the motivation, choices, and open issues of this
migration to Codeberg:
@url{https://codeberg.org/guix/guix-consensus-documents/src/branch/main/002-codeberg.md}.")
         (de "Die kanonische URL von Guix’ Repository ist jetzt
@url{https://git.guix.gnu.org/guix.git}.

Sie müssen dazu nichts tun: @command{guix pull} wird jetzt automatisch Code von
der neuen URL beziehen, außer wenn Ihre @file{channels.scm}-Dateien auf die alte
URL verweisen.  In diesem Fall rufen wir Sie dazu auf, sie zu aktualisieren,
obwohl die alte URL unter @code{git.savannah.gnu.org} noch mindestens ein Jahr
gültig bleiben wird.

Sie haben jetzt die Möglichkeit, den Pull-Request-Mechanismus unter
@url{https://codeberg.org/guix/guix} zu nutzen, um Fehler zu melden und
Änderungen vorzuschlagen.  Die Mailing-Listen @email{bug-guix@@gnu.org} und
@email{guix-patches@@gnu.org} bleiben erhalten und Sie können bis zum 31.
Dezember 2025 auch neue Fehlerberichte und Patches dorthin schicken.

Im Guix Consensus Document (GCD) 002, das die Gemeinde im Konsens entschieden
hat, ist dokumentiert, welche Motivation, Wahlmöglichkeiten und ungelöste
Probleme den Umzug auf Codeberg begleiten:
@url{https://codeberg.org/guix/guix-consensus-documents/src/branch/main/002-codeberg.md}.")
         (fr "L'URL canonique du dépôt Guix est désormais
@url{https://git.guix.gnu.org/guix.git}.

Aucune action n'est nécessaire de votre part : @command{guix pull} va
automatiquement télécharger depuis la nouvelle URL, à moins que vos fichiers
@file{channels.scm} ne fassent référence à l'ancienne URL ; dans ce cas, vous
êtes encouragé·e à les mettre à jour, bien que l'ancienne adresse
@code{git.savannah.gnu.org} reste valide pour au moins un an.

Il est maintenant possible de faire remonter des bugs et de proposer des
changements @i{via} les mécanismes de bugs et de requête de fusion à
@url{https://codeberg.org/guix/guix}.  Les listes de diffusion
@email{bug-guix@@gnu.org} et @email{guix-patches@@gnu.org} restent
disponibles, y compris pour des nouveaux bugs ou changements, jusqu'au 31
décembre 2025.

Le Guix Consensus Document (GCD) 002, qui est le résultat du consensus
construit par la communauté, documente les motivations, les choix et les
questions ouvertes relatives à cette migration sur Codeberg :
@url{https://codeberg.org/guix/guix-consensus-documents/src/branch/main/002-codeberg.md}.")
         (pt "A URL canônica do repositório do Guix agora é
@url{https://git.guix.gnu.org/guix.git}.

Nenhuma ação é necessária do teu lado: @command{guix pull} já buscará
automaticamente a partir da nova URL, a não ser que seus arquivos
@file{channels.scm} façam referência à antiga URL; neste caso, você é
encorajado a atualizá-la, ainda que a antiga URL em
@code{git.savannah.gnu.org} permanecerá válida por pelo menos um ano.

Já é possível reportar bugs e propor mudanças usando os mecanismos de
problemas e requisições de inclusão em @url{https://codeberg.org/guix/guix}.
As listas de e-mail @email{bug-guix@@gnu.org} e @email{guix-patches@@gnu.org}
permanecem disponíveis, inclusive para novos bugs e patches, até 31 de
dezembro de 2025.

O Guix Consensus Document (GCD) 002, resultado de uma construção de consenso
da comunidade, documenta a motivação, escolhas, e questões em aberto desta
migração para o Codeberg:
@url{https://codeberg.org/guix/guix-consensus-documents/src/branch/main/002-codeberg.md}.")
         (zh "Guix 仓库的规范 URL 现在是 @url{https://git.guix.gnu.org/guix.git}。

您无需进行任何操作：@command{guix pull} 现在会自动从新 URL 获取（fetch）代码，除非您的 @file{channels.scm} 文件引用了之前的 URL；这种情况下，建议更新文件，不过位于 @code{git.savannah.gnu.org} 的原 URL 将至少一年内继续有效。

现在可以通过 @url{https://codeberg.org/guix/guix} 上的工单（Issue）和合并请求（Pull Request） 机制报告缺陷和提出修改建议。邮件列表 @email{bug-guix@@gnu.org} 和 @email{guix-patches@@gnu.org} 在 2025 年 12 月 31 日之前仍可使用，包括用于提交新的缺陷报告和补丁。

Guix 共识文档 (Guix Consensus Document，GCD) 002 记录了此次迁移至 Codeberg 的动机、选择以及待解决的问题，是社区共识的成果：
@url{https://codeberg.org/guix/guix-consensus-documents/src/branch/main/002-codeberg.md}。")))

 (entry (commit "271a8fc2499135c3f0198bf69c9f2a60f1961bf1")
        (title
         (en "@samp{guix refresh} can now target partial versions")
         (de "@samp{guix refresh} kann jetzt unvollständige Versionen als Ziel
nehmen")
         (pt "@samp{guix refresh} pode agora alvejar versões parciais"))
        (body
         (en "While it had been possible for some time to use the
@option{--target-version} to update to a partially defined version, this can
now be more conveniently expressed via the package version specification
directly, by prefixing it with the tilde (@samp{~}) character:

@example
$ guix refresh bash=~5.2
gnu/packages/bash.scm:150:15: bash would be upgraded from 5.1.16 to 5.2.37
@end example

For more information, see (info \"(guix) Invoking guix refresh\").")
         (de "Es war schon einige Zeit möglich, unter Angabe von
@option{--target-version} auf eine unvollständig definierte Version zu
aktualisieren; das lässt sich jetzt angenehmer ausdrücken, direkt in der
Paketversionsangabe, wenn Sie ihr das Tildezeichen (@samp{~}) voranstellen:

@example
$ guix refresh bash=~5.2
gnu/packages/bash.scm:150:15: bash würde von 5.1.16 auf 5.2.37 aktualisiert
@end example

Für mehr Informationen siehe (info \"(guix.de)Aufruf von guix
refresh\").")
         (pt "Embora tenha sempre sido possível há um certo tempo usar
@option{--target-version} a fim de atualizar para uma versão definida em
particular, isto agora pode ser feito de maneira mais conveniente por meio da
especificação de versão do pacote diretamente, prefixando-o com o
caractere (@samp{~}):

@example
$ guix refresh bash=~5.2
gnu/packages/bash.scm:150:15: bash would be upgraded from 5.1.16 to 5.2.37
@end example

Para mais informação, veja (info \"(guix.pt_BR) Invocando guix refresh\").")))

 (entry (commit "6e8ffdf3c5afac265e540027c2332573b25461ae")
        (title
         (en "Linux-libre 6.13 removed from Guix")
         (de "Linux-libre 6.13 wurde aus Guix entfernt")
         (pt "Linux-libre 6.13 removido do Guix")
         (zh "Linux-libre 6.13 已从 Guix 中移除"))
        (body
         (en "The 6.13 linux-libre kernel series has been removed from GNU Guix,
because it is no longer supported upstream.  The 6.14 kernel series is now the
default.")
         (de "Die Versionsreihe 6.13 des Linux-libre-Kernels wurde aus GNU Guix
entfernt, weil sie von dessen Anbieter nicht mehr unterstützt wird.
Kernel-Versionsreihe 6.14 ist jetzt vorgegeben.")
         (pt "O kernel linux-libre 6.13 foi removido do GNU Guix, porque não é
mais suportado pelo fornecedor.  A série do kernel 6.14 agora é a padrão.")
         (zh "由于上游不再支持，GNU Guix 已移除 6.13 系列的 linux-libre 内核。6.14 内核系列现已成为默认版本。")))

 (entry (commit "ce363c1dc7bd63a74dcf7788d340819f6d5db89f")
        (title
         (en "@command{guix shell --container} provides a read-only root by
default")
         (de "@command{guix shell --container} stellt als Vorgabe ein
nur lesbares Wurzeldateisystem bereit")
         (pt "@command{guix shell --container} fornece uma raiz
somente-leitura por padrão"))
        (body
         (en "The @command{guix shell --container} command now provides a
read-only root file system by default.  You can restore the previous behavior
by passing the @option{--writable-root} flag.

Run @command{info \"(guix) Invoking guix shell\"} for more information.")
         (de "Über den Befehl @command{guix shell --container} bekommt man in
der Vorgabeeinstellung jetzt ein nur lesbares Wurzeldateisystem zur Verfügung
gestellt.  Wenn Sie das vorherige Verhalten möchten, können Sie die
Befehlszeilenoption @option{--writable-root} übergeben.

Führen Sie @command{info \"(guix.de) Aufruf von guix shell\"} aus, um mehr
zu erfahren.")
         (pt "O comando @command{guix shell --container} agora fornece uma
raiz somente-leitura por padrão.  Você pode restaurar o comportamento anterior
passando a opção @option{--writable-root}.

Execute @command{info \"(guix.pt_BR) Invocando guix shell\"} para mais
informação.")))

 (entry (commit "7e5913f90df916d8d9f5c509354d62324f54f481")
        (title
         (en "Linux-libre 6.14 now available")
         (de "Linux-libre 6.14 jetzt verfügbar")
         (pt "Linux-libre 6.14 já disponível"))
        (body
         (en "The 6.14 linux-libre kernel series is now available in GNU Guix.
Soon, it will be made the default linux-libre package because the 6.13 is no
longer supported upstream.")
         (de "Die Kernel-Versionsreihe 6.14 von Linux-libre ist ab jetzt in
GNU Guix verfügbar.  Bald wird sie zum vorgegebenen Linux-libre-Paket, weil
6.13 vom Anbieter nicht mehr unterstützt wird.")
         (pt "A série 6.14 do kernel linux-libre já está disponível no GNU
Guix.  Em breve, ele será feito o pacote linux-libre padrão porque o 6.13 não é
mais suportado pelo fornecedor.")))

 (entry (commit "e2583b5a17bfdedc1d24b3bab2d752fbf8fa6db6")
        (title
         (en "Guix System can run @command{guix-daemon} without root
privileges")
         (de "Guix System kann @command{guix-daemon} ohne root-Berechtigungen
ausführen")
         (fr "Guix System peut faire tourner @command{guix-daemon} sans
privilèges")
         (pt "O Sistema Guix System pode executar @command{guix-daemon} sem
privilégios de superusuário"))
        (body
         (en "On Guix System, @code{guix-service-type} can now be configured
to run the build daemon, @command{guix-daemon}, without root privileges.  In
that configuration, the daemon runs with the authority of the
@code{guix-daemon} user, which we think can reduce the impact of some classes
of vulnerabilities that could affect it.

For now, this is opt-in: you have to change @code{guix-configuration} to set
the @code{privileged?} field to @code{#f}.  When you do this, all the files in
@file{/gnu/store}, @file{/var/guix}, etc. will have their ownership changed to
the @code{guix-daemon} user (instead of @code{root}); this can take a while,
especially if the store is big.  To learn more about it, run:

@example
info guix --index-search=guix-service-type
@end example

Running @command{guix-daemon} without root privileges will likely become the
default in the future.

Users of Guix on other distributions can find information on how to migrate in
the manual:

@example
info guix --index-search=migration
@end example")
         (de "Auf Guix System kann @code{guix-service-type} jetzt so
konfiguriert werden, dass der Erstellungs-Daemon @command{guix-daemon} ohne
root-Berechtigungen ausgeführt wird.  In dieser Konfiguration läuft der Daemon
mit den Berechtigungen des Benutzers @code{guix-daemon}, wovon wir glauben,
dass es die Auswirkungen mancher Schwachstellen-Kategorien verringert, die ihn
betreffen könnten.

Fürs Erste bleibt es Ihnen überlassen: Sie müssen @code{guix-configuration}
anpassen und dort das Feld @code{privileged?} auf @code{#f} setzen.  Wenn Sie
das tun, wird der Besitzer aller Dateien in @file{/gnu/store},
@file{/var/guix}, usw. auf den Benutzer @code{guix-daemon} geändert (anstelle
von @code{root}); das kann eine Weile dauern, besonders wenn der Store groß
ist.  Um mehr zu erfahren, führen Sie aus:

@example
info guix --index-search=guix-service-type
@end example

Schließlich wird das Ausführen von @command{guix-daemon} ohne
root-Berechtigungen wahrscheinlich die Vorgabe.

Wer Guix auf anderen Distributionen benutzt, kann sich mit dem Handbuch
informieren, wie man umsteigt:

@example
info guix --index-search=migration
@end example")
         (fr "Sur Guix System, @code{guix-service-type} peut maintenant être
configuré pour faire tourner le démon de compilation, @command{guix-daemon},
sans privilèges ``root''.  Dans cette configuration, le démon s'exécute avec
l'autorité du compte @code{guix-daemon}, ce qui selon nous réduit l'impact de
certaines classes de vulnérabilités qui pourraient l'affecter.

Pour le moment, c'est à activer explicitement : il faut changer
@code{guix-configuration} pour mettre le champ @code{privileged?} à @code{#f}.
Tous les fichiers de @file{/gnu/store}, @file{/var/guix}, etc. voient alors
leur propriétaire changé pour @code{guix-daemon} (au lieu de @code{root}) ;
cette opération peut prendre un moment, particulièrement si le dépôt est gros.
Pour en savoir plus, lancer :

@example
info guix --index-search=guix-service-type
@end example

L'exécution de @command{guix-daemon} sans privilèges se fera probablement par
défaut à l'avenir.

Pour l'utilisation de Guix sur d'autres distributions, des informations sur
comment migrer se trouver dans le manuel :

@example
info guix --index-search=migration
@end example")
         (pt "No sistema Guix, @code{guix-service-type} já pode ser
configurado para executar o daemon de construção, @command{guix-daemon}, sem
privilégios de superusuário.  Nesta configuração, o daemon roda com a
autoridade do usuário @code{guix-daemon}, o que pensamos que pode reduzir o
impacto de algumas classes de vulnerabilidades que podem afetá-lo.

Por ora, isto é opcional e desabilitado por padrão: você tem que modificar
@code{guix-configuration} para definir o valor do campo @code{privileged?}
para @code{#f}.  Quando você fizer isso, todos os arquivos em
@file{/gnu/store}, @file{/var/guix}, etc. terá sua posse mudada para o usuário
@code{guix-daemon} (em vez de @code{root}); isto pode levar um tempo,
especialmente se o armazém for grande.  Para aprender mais sobre isso, execute:

@example
info guix --index-search=guix-service-type
@end example

Executar @command{guix-daemon} sem privilégios de superusuário provavelmente
será o padrão no futuro.

Usuários do Guix em outras distribuições podem encontrar informação sobre como
migrar no manual:

@example
info guix --index-search=migration
@end example")))

 (entry (commit "3e9e164154af6245389af5a1781473b36263ad20")
        (title
         (en "Incompatible changes in the @code{node-build-system}")
         (de "Inkompatible Änderungen im @code{node-build-system}")
         (pt "Modificações incompatíveis em @code{node-build-system}"))
        (body
         (en "The @code{node-build-system} has been modified to work with the
new @code{modify-json} API.

If you used @code{delete-dependencies} or
@code{with-atomic-json-file-replacement} in your own packages previously, you
now have to wrap these in @code{modify-json}.  For example:

@lisp
(modify-json (delete-dependencies '(\"tslint\")))
@end lisp

The @code{npm-binary} importer generates package expressions using the new
@code{modify-json} API.")
         (de "Das Erstellungssystem @code{node-build-system} wurde verändert und
benutzt jetzt die neue @code{modify-json}-API.

Wenn Sie bisher @code{delete-dependencies} oder
@code{with-atomic-json-file-replacement} in Ihren eigenen Paketen benutzt haben,
müssen Sie diese jetzt mit @code{modify-json} umhüllen.  Zum Beispiel:

@lisp
(modify-json (delete-dependencies '(\"tslint\")))
@end lisp

Der @code{npm-binary}-Importer verwendet die neue @code{modify-json}-API und
erzeugt damit Paketausdrücke.")
         (pt "O @code{node-build-system} foi modificado para funcionar com a
nova API @code{modify-json}.

Se você usava @code{delete-dependencies} ou
@code{with-atomic-json-file-replacement} nos seus próprios pacotes
anteriormente, você agora deve envelopá-los em @code{modify-json}.  Por
exemplo:

@lisp
(modify-json (delete-dependencies '(\"tslint\")))
@end lisp

O importador @code{npm-binary} gera expressões para pacotes usando a nova API
@code{modify-json}.")))

 (entry (commit "0e51c6547ffdaf91777f7383da4a52a1a07b7286")
        (title
         (en "Incompatible upgrade of the Syncthing service")
         (pt "Atualização incompatível do serviço Syncthing"))
        (body
         (en "The @code{syncthing-service-type} has received an incompatible
upgrade.  This means that if you are already using the Syncthing service, you
will have to manually adjust your service declaration.

If your @code{syncthing-configuration} declares a value for @code{config-file},
then you need to remove @file{~/.config/syncthing/config.xml}, so that
it does not exist after reconfiguring.  For example:

@example
mv ~/.config/syncthing/config.xml ~/.config/syncthing/config.xml.bak 2> /dev/null
@end example

Additionally, you should ensure the @file{*.pem} files in
@file{~/.local/state/syncthing/} refer to the desired Syncthing device ID.
Early adopters of the @code{config-file} field of
@code{syncthing-configuration}, and people that previously configured Syncthing
from @file{~/.config} will have their Syncthing device ID change if they do not
move the @file{*.pem} files from @file{~/.config/syncthing} to
@file{~/.local/state/syncthing}.  You can preserve your device ID by running the
following command:

@example
cp ~/.config/syncthing/*.pem  ~/.local/state/syncthing/
@end example

Note that if you specified @code{config-file} previously and had your Syncthing
device ID change, and would like your original device ID back, then do NOT run
the above.

Finally, there are some obscure fields of the various Syncthing records
that were renamed (notably, booleans now end in '?').  Refer to the
documentation of @code{syncthing-config-file} in the Networking Services of the
Guix manual to see the new names.")
         (pt "O serviço @code{syncthing-service-type} recebeu uma atualização
incompatível.  Isto significa que se você já estiver usando o serviço
Syncthing, você terá que ajustar sua declaração de serviço manualmente.

Se sua @code{syncthing-configuration} declara um valor para
@code{config-file}, você então precisa remover
@file{~/.config/syncthing/config.xml}, de forma que ele não existe após a
reconfiguração.  Por exemplo:

@example
mv ~/.config/syncthing/config.xml ~/.config/syncthing/config.xml.bak 2> /dev/null
@end example

Adicionalmente, você deve garantir que os arquivos @file{*.pem} em
@file{~/.local/state/syncthing/} se refiram ao ID de dispositivo Syncthing
desejado.  Adotantes precoces do campo @code{config-file} de
@code{syncthing-configuration}, e pessoas que anteriormente configuraram
Syncthing a partir de @file{~/.config} terão seus IDs de dispositivo Syncthing
modificados se não moverem seus arquivos @file{*.pem} de
@file{~/.config/syncthing} para @file{~/.local/state/syncthing}.  Você pode
preservar seu ID de dispositivo executando o comando a seguir:

@example
cp ~/.config/syncthing/*.pem  ~/.local/state/syncthing/
@end example

Note que se você especificou @code{config-file} anteriormente e teve seu ID de
dispositivo Syncthing modificado, e gostaria de reaver seu ID de dispositivo,
então NÃO EXECUTE o comando acima.

Finalmente, há alguns campos obscuros de vários registros Syncthing que foram
renomeadas (notoriamete, booleanos agora terminam em '?').  Refira à
documentação de @code{syncthing-config-file} nos serviços de rede do manual
Guix para ver seus nomes.")))

 (entry (commit "41e62cb10c3049610dc854f1d3e9b91aebd73aed")
        (title
         (en "Removable devices now mount under @file{/run/media/$USER} instead of @file{/media}")
         (de "Wechseldatenträger werden jetzt unter @file{/run/media/$USER} statt @file{/media} eingebunden")
         (pt "Dispositivos removíveis agora são montados a partir de @file{/run/media/$USER} em vez de @file{/media}"))
        (body
         (en "UDisks, the daemon responsible for allowing unprivileged users
mounting removable drives in GNOME and other desktop environments, will now
make them available at mount points under the @file{/run/media/$USER}
directory instead of directly under @file{/media}.  This will ensure that
mount points are not erroneously persisted across reboots, after @file{/run}
is made volatile in a future update.  Users are advised to ensure any backup
script or solution they use are updated to reflect the new location of the
mounted removable devices.")
         (de "UDisks, der Daemon, der dafür sorgt, dass unprivilegierte Nutzer
externe Laufwerke in GNOME und anderen Arbeitsumgebungen einbinden können,
wird diese ab sofort an Einhängepunkten im Verzeichnis @file{/run/media/$USER}
verfügbar machen und nicht mehr direkt in @file{/media}.  Dadurch werden
Einhängepunkte nach Neustarts nicht mehr erhalten bleiben, sobald @file{/run}
durch eine kommende Aktualisierung als flüchtig vorgegeben wird.  Wir raten
unseren Nutzerinnen und Nutzern, Scripts oder Programme anzupassen, mit denen
sie Backups anlegen, damit diese den neuen Ort für eingebundene
Wechseldatenträger verwenden.")
         (pt "UDisks, o daemon responsável por permitir que usuários
não-privilegiados montem dispositivos removíveis no GNOME e em outros
ambientes de trabalho desktop, agora os fará disponíveis em pontos de montagem
dentro do diretório @file{/run/media/$USER} em vez de diretórios dentro de
@file{/media}.  Isto assegurará que os pontos de montagem não persistirão
erroneamente após reinícios do sistema, após @file{/run} ser tornado volátil
em uma atualização futura.  Usuários são recomendados a assegurar que
quaisquer scripts ou soluções de backup que eles usem sejam atualizadas a fim
de refletir as novas localizações dos dispositivos removíveis montados.")))

 (entry (commit "8492a3c8962664db4bd0e7475f63be0ef59db87a")
        (title
         (en "Guix System switches to the Shepherd's system log")
         (de "Guix System wechselt zu Shepherds Systemprotokoll")
         (fr "Guix System passe au journal système de Shepherd")
         (pt "Sistema Guix muda para o sistema de registros (@i{log}) do Shepherd"))
        (body
         (en "The default system log used on Guix System and part of
@code{%base-services} has been switched from the @command{syslogd} command of
GNU@tie{}Inetutils to the new built-in @code{system-log} service found in
version 1.0 of the Shepherd.

The advantages of this change are the fact that @code{system-log} can start
logging earlier after boot and stop shortly before shutdown, along with full
integration with the rest of @command{shepherd}, in particular with the new
@code{log-rotation} service.")
         (de "Das vorgegebene Systemprotokoll, das auf Guix System benutzt
wird und Teil von @code{%base-services} ist, wurde ausgetauscht und anstelle
des Befehls @command{syslogd} aus den GNU@tie{}Inetutils wird der neue, in
Version 1.0 von Shepherd eingebaute @code{system-log}-Dienst benutzt.

Die Vorteile dieser Änderung sind, dass @code{system-log} schon früher nach
dem Start Protokolle aufnehmen kann, kurz vor dem Herunterfahren damit aufhört
und außerdem mit dem Rest von @command{shepherd} voll integriert ist,
insbesondere mit dem neuen Dienst @code{log-rotation} zur
Protokollrotation.")
         (fr "Le journal système par défaut sur Guix System, qui fait partie
de @code{%base-services}, est passé de la commande @command{syslogd} de
GNU@tie{}Inetutils au nouveau service @code{system-log} fourni par la version
1.0 du Shepherd.

Les avantages de ce changement sont le fait que @code{system-log} peut
commencer à enregistrer les journaux plus tôt pendant la séquence de
démarrage et s'arrêter plus tard au moment de l'arrêt, ainsi que son
intégration complète avec le reste de @command{shepherd}, en particulier avec
le nouveau service @code{log-rotation}.")
         (pt "O sistema padrão de registro usado no Guix System e parte do
@code{%base-services} foi modificado do comando @command{syslogd} do
GNU@tie{}Inetutils para o novo serviço embutido @code{system-log}  encontrado
na versão 1.0 do Shepherd.

As vantagens dessa mudança são o fato que @code{system-log} pode começar a
registrar mais cedo após o início do sistema e parar brevemente antes do
desligamento, junto a uma integração completa com o restante do
@command{shepherd}, em particular com o novo serviço @code{log-rotation}.")))

 (entry (commit "b4cc3e50187bd20a9479df52022c8228d3af49ab")
        (title
         (en "Linux-libre updated to 6.13")
         (de "Linux-libre wird auf 6.13 aktualisiert")
         (pt "Linux-libre atualizado para 6.13"))
        (body
         (en "The default linux-libre package has been updated to the
6.13 release series.")
         (de "Das standardmäßig verwendete @code{linux-libre}-Paket basiert
              jetzt auf der 6.13-Versionsreihe.")
         (pt "O pacote @code{linux-libre} padrão foi atualizado para a série de
lançamentos 6.13.")))

 (entry (commit "0753a17ddf6f4fab98b93c25f1a93b97ff9e46bb")
        (title
         (en "The @command{guix deploy} command now supports the Hetzner Cloud
service")
         (de "Der Befehl @command{guix deploy} unterstützt jetzt den
Hetzner-Clouddienst")
         (pt "O comando @command{guix deploy} agora suporta o serviço de nuvem Hetzner Cloud"))
        (body
         (en "In addition to deploying machines over SSH and on the Digital
Ocean cloud service, the @command{guix deploy} command now supports deployment
on the Hetzner Cloud service as well.  When deploying a machine with the new
@code{hetzner-environment-type}, a @acronym{VPS, virtual private server} will
be provisioned on the Hetzner Cloud, and the machine configuration's operating
system will be installed on it.  Provisioning happens through the Hetzner
Cloud API and you need to set the @code{GUIX_HETZNER_API_TOKEN} environment
variable to a Hetzner Cloud API token.  Additionally, you can use the
@code{hetzner-configuration} record to customize the deployment, such as the
system architecture, type of VPS, etc.")
         (de "Der Befehl @command{guix deploy} kann jetzt zusätzlich zum
Bereitstellen von Maschinen über SSH und auf den Digital-Ocean-Clouddienst auch
benutzt werden, um ein System auf den Hetzner-Clouddienst einzuspielen.  Bei
einer Bereitstellung mit dem neuen @code{hetzner-environment-type} wird ein
@acronym{VPS, Virtual Private Server} auf der Hetzner-Cloud bereitgestellt und
das in der Konfigurationsdatei der Maschine deklarierte Betriebssystem wird
darauf installiert.  Zum Bereitstellen wird Hetzners Cloud-API benutzt und Sie
müssen die Umgebungsvariable @code{GUIX_HETZNER_API_TOKEN} auf einen API-Token
der Hetzner-Cloud-API festlegen.  Außerdem können Sie mit einem
@code{hetzner-configuration}-Verbundsobjekt die Bereitstellung anpassen und
etwa die Systemarchitektur, den VPS-Typ usw.@: wählen.")
         (pt "Além de implantar máquinas via SSH e no serviço de nuvem Digital
Ocean, o comando @command{guix deploy} agora suporta a implantação no serviço
Hetzner Cloud também.  Na implantação em uma máquina com o novo
@code{hetzner-environment-type}, um @acronym{VPS, virtual private server} será
providenciado na Hetzner Cloud, e o sistema operacional da configuração da
máquina será instalado nele.  O providenciamento ocorre mediante a API da
Hetzner Cloud e você terá que definir a variável de ambiente @code{GUIX_HETZNER_API_TOKEN}
para um token de API da Hetzner Cloud.  Adicionalmente, você pode usar o
registro @code{hetzner-configuration} para customizar a implantação, como por
exemplo a arquitetura do sistema, tipo de VPS, etc.")))

 (entry (commit "616ae36e0f557cecb4abe58c5b0973b9428d25e0")
        (title
         (en "Kernel persistent storage in UEFI disabled")
         (de "Im Kernel wurde persistenter Speicher in UEFI abgeschaltet")
         (pt "Armazenamento persistente do kernel em UEFI desabilitado"))
        (body
         (en "The linux-libre kernel's persistent storage (pstore) mechanism
can use UEFI non-volative memory to store information that would otherwise be
lost, such as kernel panic logs.  However, this can permanently fill the
non-volatile memory on some hardware implementations, breaking the ability to
reconfigure Guix System.  Therefore, this mechanism has been disabled by default
in the build-time kernel configuration.  Users can re-enable it if desired.")
         (de "Im Kernel Linux-libre kann nicht flüchtiger Speicher eines UEFI
als persistenter Speicher (pstore) benutzt werden, um Informationen zu
behalten, die andernfalls nicht mehr einsehbar wären, wie Protokolle einer
Kernel Panic.  Jedoch kann dadurch der nicht flüchtige Speicher auf manchen
Hardware-Implementierungen dauerhaft gefüllt werden, wodurch weiteres
Rekonfigurieren eines Guix System unmöglich wird.  Daher wurde dieser
Mechanismus in der Vorgabeeinstellung neu erstellter Kernels abgeschaltet.
Interessierte Benutzer können ihn wieder aktivieren.")
         (pt "O mecanismo de armazenamento persistente do kernel linux-libre
(@i{pstore}) pode usar memória não-volátil do UEFI para armazenar informação
que de outra forma seria perdida, como registros de pânico do kernel.
Todavia, isto pode lotar permanentemente a memória não-volátil de algumas
implementações de hardware, danificando a capacidade de reconfigurar o Sistema
Guix.  Portanto, este mecanismo foi desabilitado por padrão na configuração de
kernel em tempo de construção.  Usuários podem reabilitá-la se desejarem.")))

 (entry (commit "f40eff02413c20cdb6200d90cbb7f674cea475fd")
        (title
         (en "Linux-libre 6.13 now available")
         (de "Linux-libre 6.13 jetzt verfügbar")
         (pt "Linux-libre 6.13 já disponível"))
        (body
         (en "The 6.13 linux-libre series is now available in GNU Guix.")
         (de "Die Versionsreihe 6.13 von Linux-libre ist ab jetzt in GNU Guix
verfügbar.")
         (pt "A série 6.13 do linux-libre já está disponível no GNU Guix.")))

 (entry (commit "0aa45f18543552f2396414ab130dab40f8969d27")
        (title
         (en "New @code{%base-home-services} variable for Guix Home")
         (de "Neue Variable @code{%base-home-services} für Guix Home")
         (fr "Nouvelle variable @code{%base-home-services} pour Guix Home")
         (pt "Nova variável @code{%base-home-services} para Guix Home"))
        (body
         (en "If you are using Guix Home, we recommend you update your
configuration to include @code{%base-home-services}, a list of non-essential
services deemed generally useful (this is comparable to @code{%base-services}
in Guix System).  That is, your Home configuration should now look like this:

@lisp
(home-environment
  ;; fields omitted @dots{}
  (services (append (list @dots{}) %base-home-services)))
@end lisp

The dots above should be replaced by your own list of services, as it
currently appears in your @code{home-environment} declaration.

Currently, @code{%base-home-services} contains only one service: the new log
rotation service provided by version 1.0 of the Shepherd.")
         (de "Wenn Sie Guix Home benutzen, empfehlen wir, dass Sie Ihre
Konfiguration anpassen und @code{%base-home-services} eintragen, eine Liste von
nicht essenziellen Diensten, die aber in den meisten Fällen nützlich sind
(vergleichbar mit @code{%base-services} in Guix System).  Das heißt, Ihre
Persönliche Konfiguration sollte nun diese Form haben:

@lisp
(home-environment
  ;; hier stehen andere Felder@dots{}
  (services (append (list @dots{}) %base-home-services)))
@end lisp

Statt der drei Auslassungspunkte schreiben Sie Ihre Dienste auf, die bisher in
der @code{home-environment}-Deklaration Ihrer Persönlichen Umgebung aufgetaucht
sind.

Derzeit enthält @code{%base-home-services} nur einen Dienst: den neuen Dienst
zur Protokollrotation, den Version 1.0 von Shepherd bringt.")
         (fr "Il est recommandé aux utilisateurices de Guix Home de mettre à
jour leur configuration pour include @code{%base-home-services}, une liste de
services non-essentiels mais considérés généralement utiles (c'est comparable
à @code{%base-services} pour Guix System).  Concrètement, la configuration
Home devrait maintenant ressembler à ça :

@lisp
(home-environment
  ;; champs omis @dots{}
  (services (append (list @dots{}) %base-home-services)))
@end lisp

Les points de suspension ci-dessus doivent être remplacés par sa propre liste
de services, celle qui est déjà visible dans sa déclaration
@code{home-environment}.

Pour le moment @code{%base-home-services} ne contient qu'un seul service : le
nouveau service de rotation des journaux fourni par la version 1.0 de
Shepherd.")
         (pt "Se você está utilizando Guix Home, recomendamos que você
atualize sua configuração para incluir @code{%base-home-services}, uma lista
de serviços não-essenciais tida como geralmente útil (isto é comparável a
@code{%base-services} no Sistema Guix).  Isto é, sua configuração Home deve
agora parecer com isso:

@lisp
(home-environment
  ;; campos omitidos @dots{}
  (services (append (list @dots{}) %base-home-services)))
@end lisp

As reticências acima devem ser substituídas pela sua própria lista de
serviços, como correntemente aparece em sua declaração de
@code{home-environment}.

Atualmente, @code{%base-home-services} contém apenas um serviço: o novo
serviço de rotação de registros fornecido pela versão 1.0 do Shepherd.")))

 (entry (commit "a9f21036e43ffe4eeda2ae51b86e563c14509225")
        (title
         (en "Rottlog service replaced by new log rotation service")
         (de "Rottlog-Dienst ersetzt durch neuen Dienst zur Protokollrotation")
         (fr "Service Rottlog remplacé par un nouveau service de rotation des
journaux")
         (pt "Serviço rottlog substituído pelo novo serviço de rotação de registro"))
        (body
         (en "A noticeable change was made that impacts all Guix System users:
the Rottlog service was replaced by the new log rotation service.
Additionally, @code{rottlog-service-type} is now deprecated in favor of
@code{log-rotation-service-type} and will be removed in six months, in
accordance with Guix's deprecation policy.  Authors of service types in
custom Guix channels should therefore no longer extend
@code{rottlog-service-type}.

The new @code{log-rotation-service-type} builds upon the log rotation service
provided by version 1.0 of the Shepherd.  It is more flexible and easier to
use.  Run @command{info \"(guix) Log Rotation\"}, for more info.

Because the new log rotation service depends on Shepherd 1.0 functionality,
you will need to reboot after reconfiguring if you are not running Shepherd
1.0 yet.")
         (de "Eine merkliche Änderung betrifft alle Nutzer von Guix System:
Anstelle des Rottlog-Dienstes wird jetzt ein neuer Dienst zur Protokollrotation
benutzt.  Des Weiteren gilt @code{rottlog-service-type} jetzt als veraltet und
ersetzt durch @code{log-rotation-service-type}; er wird in sechs Monaten
entfernt werden, wie es Guix’ Richtlinie zu Veraltetem vorsieht.  Autoren
von Diensttypen in eigenen Guix-Kanälen sollten daher nicht mehr
@code{rottlog-service-type} erweitern.

Der neue Diensttyp @code{log-rotation-service-type} baut auf auf dem in Version
1.0 von Shepherd bereitgestellten Protokollrotationsdienst.  Er ist
vielseitiger und leichter nutzbar.  Führen Sie @command{info \"(guix.de)
Log-Rotation\"} aus, um mehr zu erfahren.

Weil der neue Protokollrotationsdienst die Funktionalitäten der Version 1.0 von
Shepherd voraussetzt, werden Sie nach dem Rekonfigurieren Ihren Rechner neu
starten müssen, falls auf ihm noch nicht Shepherd 1.0 läuft.")
         (fr "Un changement important a eu lieu impactant toute personne
utilisant Guix System : le service Rottlog a été remplacé par le nouveau
service de rotation des journaux.  De plus, @code{rottlog-service-type} est
maintenant obsolète, remplacé par @code{log-rotation-service-type}, et sera
retiré d'ici six mois, conformément à la politique d'obsolescence de Guix.
Les auteurices de services dans des canaux Guix tiers sont donc invité·es à ne
plus étendre @code{rottlog-service-type}.

Le nouveau @code{log-rotation-service-type} repose sur le service de rotation
des journaux fourni par la version 1.0 du Shepherd.  Il est plus flexible et
facile à utiliser.  Lancer @command{info \"(guix.fr) Rotation des journaux\"}
pour en savoir plus.

Comme ce nouveau service dépend d'une fonctionnalité de Shepherd 1.0, il
faudra redémarrer après reconfiguration si tu ne fais pas encore tourner
Shepherd 1.0.")
         (pt "Foi feita uma mudança notória que impacta todos os usuários do
Sistema Guix: o serviço Rottlog foi substituído pelo novo serviço de rotação.
Adicionalmente, @code{rottlog-service-type} já está depreciado em favor de
@code{log-rotation-service-type} e será removido em seis meses, de acordo com
a política de depreciação do Guix.  Autores dos tipos de serviço em canais
Guix personalizados devem, portanto, não mais estender
@code{rottlog-service-type}.

O novo @code{log-rotation-service-type} é construído em cima do serviço de
rotação de registro fornecido pela versão 1.0 do Shepherd.  Este é mais
flexível e fácil de usar.  Execute @command{info \"(guix.pt_BR) Rotação de
log\"}, para mais info.

Como o novo serviço de rotação depende da funcionalidade do Shepherd 1.0, você
precisará reiniciar após a reconfiguração se você não estiver rodando o
Shepherd 1.0 ainda.")))

 (entry (commit "5c2bcafd281fdd31b3dfec5a67ba85084c58cf60")
        (title
         (en "Linux-libre 6.11 removed from GNU Guix")
         (de "Linux-libre 6.11 wurde aus GNU Guix entfernt")
         (pt "Linux-libre 6.11 removido do GNU Guix"))
        (body
         (en "The linux-libre 6.11 kernel series is no longer supported
             upstream, so it has been removed from GNU Guix.")
         (de "Die Versionsreihe 6.11 des Linux-libre-Kernels wurde aus GNU Guix
entfernt, weil sie von dessen Anbieter nicht mehr unterstützt wird.")
         (pt "O kernel linux-libre 6.11 não é mais suportado pelo fornecedor,
portanto será removido do GNU Guix.")))

 (entry (commit "3a4209224e7e3121189390307295fccdc0612db4")
        (title
         (en "Linux-libre updated to 6.12")
         (de "Linux-libre wird auf 6.12 aktualisiert")
         (pt "Linux-libre atualizado para 6.12"))
        (body
         (en "The default linux-libre package has been updated to the
             6.12 series.  The 6.11 series will be removed soon, due to
             end of upstream support.")
         (de "Das standardmäßig verwendete @code{linux-libre}-Paket basiert
              jetzt auf der 6.12-Versionsreihe.  Die Versionsreihe 6.11 wird
bald entfernt werden, weil sie von dessen Anbieter nicht mehr unterstützt
wird.")
         (pt "O pacote padrão linux-libre foi atualizado para a série 6.12.  A
série 6.11 será removida em breve, em razão do fim do suporte do fornecedor.")))

 (entry (commit "1dcd0ded86e341cbfd0567cefde1e71684c0cdba")
        (title
         (en "Linux-libre 6.12 now available")
         (de "Linux-libre 6.12 jetzt verfügbar")
         (pt "Linux-libre 6.12 já disponível"))
        (body
         (en "The 6.12 linux-libre series is now available in GNU Guix.  This
kernel will receive upstream long-term support, so the @code{linux-libre-lts}
package has been updated to 6.12.")
         (de "Die Versionsreihe 6.12 von Linux-libre ist ab jetzt in GNU Guix
verfügbar.  Dieser Kernel wird langfristig vom Anbieter mit Aktualisierungen
versorgt werden, daher wurde das Paket @code{linux-libre-lts} auf 6.12
aktualisiert.")
         (pt "A série 6.12 do linux-libre já está disponível no GNU Guix.
Este kernel receberá suporte de longa duração do fornecedor, então o pacote
@code{linux-libre-lts} será atualizado para 6.12.")))

 (entry (commit "7b40b9d2d2ddafd6945f18f19f5e621086d57169")
        (title
         (en "Linux-libre 4.19 removed due to end of upstream support")
         (de "Linux-libre 4.19 wurde entfernt")
         (pt "Linux-libre 4.19 removido em razão do fim do suporte do fornecedor"))
        (body
         (en "The linux-libre 4.19 kernel series has reached the end of its
             life, and is no longer supported upstream.  For this reason, it has
             been removed from GNU Guix.")
         (de "Die linux-libre 4.19-Versionsreihe hat ihr Supportende erreicht
             und wird nicht mehr unterstützt („end of life“). Daher ist die
             Versionsreihe aus GNU Guix entfernt worden.")
         (pt "A série 4.19 do kernel linux-libre alcançou o fim de sua vida, e
não é mais suportada pelo fornecedor.  Por esta razão, ela foi removida do GNU
Guix.")))

 (entry (commit "1305f78d05f4e0027162c1b7e783fc127a49fb8e")
        (title
         (en "@command{guix system reconfigure} now supports kexec")
         (de "@command{guix system reconfigure} unterstützt jetzt kexec")
         (pt "@command{guix system reconfigure} agora suporta kexec"))
        (body
         (en "If you are using Guix System, you'll be delighted to know that
@command{guix system reconfigure} now loads the new system for fast reboot
@i{via} Linux's kexec mechanism---unless given the @option{--no-kexec} option.
The same goes for @command{guix deploy}.

Kexec allows Linux to reboot straight into a new kernel (and operating
system), bypassing the BIOS and the bootloader.  The @command{reboot} command,
starting from Shepherd 1.0.0, has a new @option{--kexec} (or @option{-k})
option that lets you reboot into a previously-loaded system; use
@command{reboot --kexec} to take advantage of this new @command{guix system
reconfigure} feature.

Run @command{info \"(guix) Invoking guix system\"}, for more info.")
         (de "Wenn bei Ihnen Guix System läuft, wird Sie die Nachricht
erfreuen, dass @command{guix system reconfigure} von nun an das neue System für
ein schnelles Neustarten des Rechners mit Linux’ kexec-Mechanismus
vorbereitet – außer wenn Sie die Option @option{--no-kexec} angeben.  Genauso
gilt das für @command{guix deploy}.

Mit kexec kann Linux sofort einen neuen Kernel (und Betriebssystem) starten,
ohne nochmal das BIOS und den Bootloader zu durchlaufen.  Seit
Shepherd 1.0.0 kann der Befehl @command{reboot} eine neue Option
@option{--kexec} (oder @option{-k}) entgegennehmen, womit Sie direkt in ein
vorher geladenes System neu starten können.  Rufen Sie dazu @command{reboot
--kexec} auf, um sich die neue Funktion von @command{guix system reconfigure}
zunutze zu machen.

Führen Sie @command{info \"(guix.de) Aufruf von guix system\"} aus für mehr
Informationen.")
         (pt "Se você está usando o Sistema Guix, você se encantará em saber
que @command{guix system reconfigure} agora carrega o novo sistema para
reinício rápido @i{via} mecanismo de kexec do Linux---a menos que seja passada
a opção @option{--no-kexec}.  O mesmo vale para @command{guix deploy}.

Kexec permite ao Linux to reiniciar direto em um novo kernel (e sistema
operacional), ignorando a BIOS e o carregador de boot.  O comando
@command{reboot}, desde o Shepherd 1.0.0, tem uma nova opção @option{--kexec}
(ou @option{-k}) que te permite reiniciar em um sistema previamente carregado;
use @command{reboot --kexec} para tirar vantagem desta nova característica do
@command{guix system reconfigure}.

Execute @command{info \"(guix.pt_BR) Invocando guix system\"}, para mais
info.")))

 (entry (commit "ccf72d5074b0c5ba793e686cbb1d6eaad39824bf")
        (title
         (de "Neues Format @samp{appimage} für den Befehl @command{guix pack}")
         (en "New @samp{AppImage} format for the @command{guix pack} command")
         (fr "Nouveau format @samp{AppImage} pour la commande @command{guix pack}")
         (pt "Novo formato @samp{AppImage} para o comando @command{guix pack}"))
        (body
         (de "@command{guix pack} kann nun AppImage-Dateien erstellen. Das
AppImage-Dateiformat erlaubt es, in einer einzelnen Datei Software zu verteilen. Die
AppImage-Datei lässt sich ohne besondere Benutzerrechte ausführen. Hier ist
ein Beispiel:

@example
guix pack --format=appimage --entry-point=bin/hello hello
@end example

Siehe @command{info \"(guix.de) Aufruf von guix pack\"} für mehr
Informationen.")
         (en "@command{guix pack} can now produce AppImage a single file,
self-contained software archive. AppImage files are easily distributed and can
be run with user privileges.  Here is an example for the @code{hello} package:

@example
guix pack --format=appimage --entry-point=bin/hello hello
@end example

See @command{info \"(guix) Invoking guix pack\"} for more information.")
         (fr "@command{guix pack} peut désormais produire un fichier
AppImage. AppImage est une manière de distribuer les logiciels en un seul
fichier, qui peut être executé avec des droits d’utilisateur. Voici un
exemple pour le paquet @code{hello} :

@example
guix pack --format=appimage --entry-point=bin/hello hello
@end example

Consultez @command{info \"(guix.fr) Invoquer guix pack\"} pour plus
d’informations.")
         (pt "@command{guix pack} pode agora produzir uma AppImage, um arquivo
de software autocontido.  Arquivos AppImage são facilmente distribuíveis e
podem rodar com privilégios de usuário comum.  Eis um exemplo para o pacote
@code{hello}:

@example
guix pack --format=appimage --entry-point=bin/hello hello
@end example

Veja @command{info \"(guix.pt_BR) Invocando guix pack\"} para mais
informação.")))

 (entry (commit "b93434e656eba4260df82158a96c295000d3ff44")
        (title (en "PostgreSQL service upgrade")
               (de "PostgreSQL-Dienst aktualisiert")
               (fr "Mise à jour du service PostgreSQL")
               (pt "Atualização do serviço PostgreSQL"))
        (body
         (en "\
This news entry concerns users of the @code{postgresql-service-type}.

The default PostgreSQL used in @code{postgresql-configuration} has been
deprecated, and will be removed in a few months.  However, the value of the
default @code{postgresql-configuration-postgresql} can't be directly changed
to a newer major version of PostgreSQL, because switching to a major version
currently requires a manual update of the database.

Because of this, the default value of postgresql-configuration-postgresql has
been unset.  Current users of the service will have to set it manually.  If it
was unset, use @code{postgresql-10} and plan an upgrade in the next month to a
supported version of PostgreSQL.  To upgrade, you will need to either dump
your database using your previous version of PostgreSQL and reload it in the
new version or use the @command{pg_upgrade} application.  See
@url{https://www.postgresql.org/docs/current/upgrading.html} for more info.")
         (de "\
Diese Neuigkeit betrifft Nutzer des @code{postgresql-service-type}.

Die in @code{postgresql-configuration} bisher vorgegebene Version von
PostgreSQL gilt als veraltet und wird in ein paar Monaten entfernt werden.
Allerdings können wir den Vorgabewert von
@code{postgresql-configuration-postgresql} nicht einfach für Sie auf eine
neuere große Version ändern, weil bei so einem Wechsel die Datenbank derzeit
manuell aktualisiert werden muss.

Aus diesem Grund wurde der Vorgabewert von
@code{postgresql-configuration-postgresql} von nun an entfernt.  Aktuelle
Nutzer des Dienstes müssen sie selbst setzen.  Wenn kein Wert gesetzt war,
sollten Sie @code{postgresql-10} eintragen und dabei einplanen, dass Sie es
kommenden Monat auf eine unterstützte Version von PostgreSQL aktualisieren
müssen.  Zum Aktualisieren werden Sie entweder mit der vorherigen Version von
PostgreSQL ein Dump Ihrer Datenbank exportieren, das Sie dann in der neuen
Version laden, oder Sie benutzen dafür die Anwendung @command{pg_upgrade}.
Siehe @url{https://www.postgresql.org/docs/current/upgrading.html} für weitere
Informationen.")
         (fr "\
Ce message concerne les utilisateurs du service-type PostgreSQL.

La précédente version par défaut de PostgreSQL utilisée dans
@code{postgresqsl-configuration} est obsolète et sera supprimée dans quelques
mois.  Cependant, la valeur de la configuration par défaut
@code{postgresql-configuration-postgresql} ne peut pas être changée
directement pour une version majeure prise en charge de PostgreSQL, car le
passage à une nouvelle version majeure nécessite actuellement une mise à jour
manuelle de la base de données.

Pour cette raison, la valeur par défaut de
@code{postgresql-configuration-postgresql} a été supprimée et les utilisateurs
actuels du service doivent la configurer manuellement. Si elle n’était pas
définie, utilisez @code{postgresql-10} et prévoyez une mise à jour dans le
mois à venir vers une version plus récente de PostgreSQL.  Pour effectuer la
mise à jour, vous devrez soit exporter votre base de données sous une version
de PostgreSQL puis la réimporter sous une nouvelle version, soit utiliser
l'application @command{pg_upgrade}. Pour plus d'informations, consultez
@url{https://www.postgresql.org/docs/current/upgrading.html}.")
         (pt "\
Esta entrada concerne usuários do @code{postgresql-service-type}.

O PostgreSQL padrão usado em @code{postgresql-configuration} foi depreciado, e
será removido em alguns meses.  Porém, o valor do
@code{postgresql-configuration-postgresql} padão não pode ser modificado
diretamente para uma versão major mais recente do PostgreSQL, porque modificar
uma para uma versão major correntemente exige uma atualização manual da base
de dados.

Por causa disso, o valor padrão de postgresql-configuration-postgresql foi
mantido como não definido.  Usuários correntes do serviço terão que defini-lo
manualmente.  Se ele não foi definido, use @code{postgresql-10} e planeje uma
atualização no próximo mês para uma versão suportada do PostgreSQL.  Para
atualizar, você precisa ou despejar sua base de dados usando sua versão
anterior do PostgreSQL e recarregá-la na nova versão, ou usar a aplicação
@command{pg_upgrade}.  Veja
@url{https://www.postgresql.org/docs/current/upgrading.html} para mais
info.")))

 (entry (commit "cfc85eb0c67a5cf10a3fbe2531b926cbb8c62489")
        (title
         (en "Linux-libre 6.10 removed due to end of upstream support")
         (de "Linux-libre 6.10 wurde entfernt")
         (pt "Linux-libre 6.10 removido em razão do fim do suporte do fornecedor"))
        (body
         (en "The linux-libre 6.10 kernel series has reached the end of
             its life, and is no longer supported upstream.  For this
             reason, it has been removed from GNU Guix.")
         (de "Die @code{linux-libre} 6.10-Versionsreihe hat ihr
Supportende erreicht und wird nicht mehr unterstützt („end of life“). Daher ist die
Versionsreihe aus GNU Guix entfernt worden.")
         (en "A série 6.10 do kernel linux-libre alcançou seu fim de vida, e
não é mais suportada pelo fornecedor.  Por esta razão, ela foi removida do GNU
Guix.")))

 (entry (commit "5966e0fdc78771c562e0f484a22f381a77908be0")
        (title
         (en "Daemon vulnerability allowing takeover of build users fixed")
         (de "Schwachstelle im Daemon behoben, durch die Übernahme von Erstellungsbenutzern möglich ist")
         (pt "Vulnerabilidade do daemon permitindo captura de usuários de construção consertada"))
        (body
         (en "A vulnerability allowing a local user to execute arbitrary code
as any of the build users has been identified and fixed.  Most notably, this
allows any local user to alter the result of any local build, even if it
happens inside a container.  The only requirements to exploit this
vulnerability are the ability to start a derivation build and the ability to
run arbitrary code with access to the store in the root PID namespace on the
machine that build occurs on.  This largely limits the vulnerability to
multi-user systems.

This vulnerability is caused by the fact that @command{guix-daemon} does not
change ownership and permissions on the outputs of failed builds when it moves
them to the store, and is also caused by there being a window of time between
when it moves outputs of successful builds to the store and when it changes
their ownership and permissions.  Because of this, a build can create a binary
with both setuid and setgid bits set and have it become visible to the outside
world once the build ends.  At that point any process that can access the
store can execute it and gain the build user's privileges.  From there any
process owned by that build user can be manipulated via procfs and signals at
will, allowing the attacker to control the output of its builds.

You are advised to upgrade @command{guix-daemon}.  Run @command{info \"(guix)
Upgrading Guix\"}, for info on how to do that.  Additionally, if there is any
risk that a builder may have already created these setuid binaries (for
example on accident), run @command{guix gc} to remove all failed build
outputs.

See @uref{https://issues.guix.gnu.org/73919} for more information on this
vulnerability.")
         (de "Eine Sicherheitslücke, durch die ein lokaler Benutzer beliebigen
Code als jeder der Erstellungsbenutzer ausführen kann, wurde gefunden und
behoben.  Diese hat zur Folge, dass jeder lokale Benutzer das Ergebnis jeder
lokalen Erstellung verändern kann, selbst wenn sie in einem Container isoliert
stattfindet.  Um die Lücke auszunutzen, wird nur vorausgesetzt, dass er
Ableitungen erstellen lassen kann und beliebigen Code mit Store-Zugriff im
Wurzel-PID-Namensraum auf der Maschine laufen lassen kann, wo die Erstellung
abläuft.  Somit sind vor allem Mehrbenutzersysteme betroffen.

Ursache der Lücke ist, dass @command{guix-daemon} Besitzer und Berechtigungen
der Ausgaben einer fehlgeschlagenen Erstellung nicht ändert, wenn er sie in den
Store verschiebt.  Auch bei erfolgreichen Erstellungen gibt es ein Zeitfenster
nachdem Ausgaben in den Store gelangen und bevor ihr Besitzer und
Berechtigungen angeglichen werden.  So kann eine Erstellung eine Binärdatei
erzeugen, bei der die Bits für setuid und setgid gesetzt sind, die dann für die
Außenwelt sichtbar wird, wenn die Erstellung fertig ist.  Ab dann kann jeder
Prozess mit Zugriff auf den Store diese ausführen und die Berechtigungen des
Erstellungsbenutzers erlangen, so dass jeder Prozess im Besitz des
Erstellungsbenutzers über procfs und Signale beeinflussbar ist und der
Angreifer Kontrolle darüber hat, welche Ausgabe Erstellungen haben.

Wir raten Ihnen, @command{guix-daemon} zu aktualisieren.  Führen Sie
@command{info \"(guix.de) Aktualisieren von Guix\"} aus für Erklärungen, wie
Sie ihn aktualisieren können.  Wenn zudem Gefahr besteht, dass ein
Erstellungsprogramm bereits setuid-gesetzte Binärdateien angelegt hat (etwa
versehentlich), führen Sie @command{guix gc} aus, um alle fehlgeschlagenen
Erstellungsausgaben zu entfernen.

Siehe @uref{https://issues.guix.gnu.org/73919} für weitere Details zu dieser
Sicherheitslücke.")
         (pt "Uma vulnerabilidade que permitiria a um usuário local executar
código arbitrário como qualquer dos usuários construtores foi identificada e
consertada.  Mais notoriamente, ela permitia a qualquer usuário alterar o
resultado de qualquer construção local, mesmo se ela ocorrer dentro de um
contêiner.  As únicas exigências para explorar essa vulnerabilidade são a
capacidade de iniciar uma derivação e a capacidade de executar código
arbitrário com acesso ao armazém no espaço de nomes do PID raiz na na máquina
onde a construção acontece.  Isto limita largamente a vulnerabilidade para
sistemas multiusuário.

Esta vulnerabilidade é causada pelo fato que @command{guix-daemon} não
modifica o pertencimento e as permissões nas saídas de construções
mal-sucedidas quando as move para o armazém, e é também causada por haver uma
janela de tempo entre quando se move a saída de construções bem-sucedidas para
o armazém e quando se mudam seus pertencimentos e permissões.  Por causa
disso, uma construção pode criar um binário com ambos os bits setuid e setgid
definidos e torná-lo visível ao mundo exterior uma vez que a construção se
encerra.  Neste ponto quaisquer processos que podem acessar o armazém podem
executá-lo e obter privilégios de usuário de construção.  Daqui, qualquer
processo pertencente a este usuário de construção pode ser manipulado vai
procfs e sinais à vontade, permitindo ao atancante controlar a saída de suas
construções.

Te aconselhamos a atualizar o @command{guix-daemon}.  Execute @command{info
\"(guix.pt_BR) Atualizando o Guix\"} para mais info sobre como fazer isso.
Adicionalmente, se existir qualquer risco de um construtor já ter criado estes
binários setuid (por exemplo, por acicente), execute @command{guix gc} para
remover todas as saídas de construções fracassadas.

Veja @uref{https://issues.guix.gnu.org/73919} para mais informação sobre esta
vulnerabilidade.")))

 (entry (commit "2fae63df2138b74d30e120364f0f272871595862")
        (title
         (en "Core packages updated")
         (de "Kernpakete aktualisiert")
         (fr "Logiciels fondamentaux mis à jour")
         (pt "Pacotes essenciais atualizados"))
        (body
         (en "Core packages have been updated, in particular those that
are used to build every other package in the distribution.  Noteworthy
upgrades include:

@itemize
@item @code{glibc} 2.39 (was 2.35);
@item @code{gcc} 11.4.0 as the default compiler (was 11.3.0);
@item @code{binutils} 2.41 (was 2.38);
@item @code{make} 4.4.1 (was 4.3);
@item TeX@tie{}Live 2024.2 (was 20230313; note that due to the new
versioning scheme, @command{guix upgrade} will consider the new packages
as ``older'' than the previous ones so you may need to use
@command{guix install} to upgrade them).
@end itemize

Additional improvements were made to build systems and related packages and
tools:

@itemize
@item
the @code{glibc} package now includes the @code{C.UTF-8} locale,
suitable for use when a UTF-8 locale is necessary regardless of
any language or regional convention;
@item
origins that include patches are now repacked with zstd instead of xz,
which uses less CPU power and memory, both when compressing and when
decompressing;
@item
performance issues with the modular TeX@tie{}Live package set have
been fixed.
@end itemize

If you encounter any problem, please check
@url{https://issues.guix.gnu.org} for existing reports and resolutions;
email @email{bug-guix@@gnu.org} to report new bugs.")
         (de "Pakete, die den Kern der Distribution ausmachen, wurden
aktualisiert, insbesondere solche Pakete, aus denen heraus alle anderen Pakete
der Distribution erstellt werden.  Zu den nennenswerten Neuerungen gehören:

@itemize
@item @code{glibc} 2.39 (war 2.35),
@item @code{gcc} 11.4.0 ist der voreingestellte Compiler (war 11.3.0),
@item @code{binutils} 2.41 (war 2.38),
@item @code{make} 4.4.1 (war 4.3),
@item TeX@tie{}Live 2024.2 (war 20230313; das bedeutet, dass wegen der neuen
Versionsbezeichnungen @command{guix upgrade} die neuen Pakete für „älter“ als
die vorherigen hält und eine Aktualisierung unter Umständen nur zulässt, indem
Sie @command{guix install} benutzen).
@end itemize

Weitere Verbesserungen wurden an Erstellungssystemen und zugehörigen Paketen
und Werkzeugen vorgenommen:

@itemize
@item
Zu dem Paket @code{glibc} gehört jetzt die Locale für @code{C.UTF-8}, die
geeignet ist, wenn eine UTF-8-Locale gebraucht wird, aber Sprache und regionale
Konventionen unwichtig sind.
@item
Paketursprünge mit Patches werden jetzt in Archive mit zstd anstelle von xz
neu gepackt, wodurch weniger Rechenzeit und Speicher beim Komprimieren und
Dekomprimieren nötig sind.
@item
Performance-Probleme mit den modularen Paketsatz von TeX@tie{}Live wurden
behoben.
@end itemize

Wenn Sie Probleme feststellen, schauen Sie bitte auf
@url{https://issues.guix.gnu.org} nach bisherigen Fehlerberichten und Lösungen
und schicken Sie eine E-Mail an @email{bug-guix@@gnu.org}, um neue Fehler zu
melden.")
         (fr "Les logiciels fondamentaux on été mis à jour, en particulier
ceux qui servent à construire tous les autres logiciels de la distribution.
Les mises à jour notables sont :

@itemize
@item @code{glibc} 2.39 (au lieu de 2.35) ;
@item @code{gcc} 11.4.0 comme compilateur par défaut (au lieu de 11.3.0) ;
@item @code{binutils} 2.41 (au lieu de 2.38) ;
@item @code{make} 4.4.1 (au lieu de 4.3) ;
@item TeX@tie{}Live 2024.2 (au lieu de 20230313 ; compte tenu du changement de
numérotation des versions, @command{guix upgrade} va croire que les nouveaux
paquets sont « plus vieux » que les précédents et il faudra donc utiliser
@command{guix install} pour les mettre à jour).
@end itemize

Les systèmes de construction ainsi que les paquets et outils connexes ont été
améliorés :

@itemize
@item
le paquet @code{glibc} inclut dorénavant la locale @code{C.UTF-8} qui convient
chaque fois qu'on a besoin d'une locale UTF-8 indépendemment d'un langage ou
de conventions régionales ;
@item
les origines qui incluent des @i{patches} sont maintenant recompressées avec
zstd au lieu de xz, ce qui demande moins de temps de calcul et de mémoire, à
la fois en compression et en décompression ;
@item
des problèmes de performance avec les paquets TeX@tie{}Live modulaires ont été
résolus.
@end itemize

En cas de difficultés, merci de jeter un œil à
@url{https://issues.guix.gnu.org} pour voir la liste des problèmes qui ont été
remontés et les éventuelles solutions ; envoyer un courrier à
@email{bug-guix@@gnu.org} pour faire remonter de nouveaux bogues.")
         (pt "Pascotes essenciais foram atualizados, em especial aqueles que
são usados para construir cada um dos outros pacotes na distribuição.
Atualizações notórias incluem:

@itemize
@item @code{glibc} 2.39 (era 2.35);
@item @code{gcc} 11.4.0 as the default compiler (era 11.3.0);
@item @code{binutils} 2.41 (era 2.38);
@item @code{make} 4.4.1 (era 4.3);
@item TeX@tie{}Live 2024.2 (era 20230313; note que devido ao novo sistema de
versionamento, @command{guix upgrade} considerará os novos pacotes como ``mais
antigos'' que os anteriores, então você pode ter que usar @command{guix
install} para atualizá-lo).
@end itemize

Foram feitas melhorias adicionais para sistemas de construçao e pacotes e
ferramentas relacionadas:

@itemize
@item
o pacote @code{glibc} agora inclui a localidade @code{C.UTF-8}, adeuado para o
uso quando uma localidade UTF-8 é necessária independente de qualquer
convenção regional ou de linguagem;
@item
origens que incluem patches são agora reempacotados com zstd em vez de xz, que
usa menos poder de processamento e memória, tanto durante a compressão quanto
durante a descompressão;
@item
questões de desempenho com o conjunto modular de pacotes TeX@tie{}Live foram
consertadas.
@end itemize

Se você encontrar algum problema, por favor confira
@url{https://issues.guix.gnu.org} para reportes e soluções existentes;
email @email{bug-guix@@gnu.org} para reportar novas falhas.")))

 (entry (commit "fc35b9fa6d6ed3583d4f3fc9214f657022d49678")
        (title
         (en "Linux-libre 6.9 removed due to end of upstream support")
         (de "Linux-libre 6.9 wurde entfernt")
         (pt "Linux-libre 6.9 reovido em razão do fim do suporte ao fornecedor"))
        (body
         (en "The linux-libre 6.9 kernel series has reached the end of
             its life, and is no longer supported upstream.  For this
             reason, it has been removed from GNU Guix.")
         (de "Die @code{linux-libre} 6.9-Versionsreihe hat ihr
Supportende erreicht und wird nicht mehr unterstützt („end of life“). Daher ist die
Versionsreihe aus GNU Guix entfernt worden.")
         (pt "A série 6.9 do kernel linux-libre alcançou o fim de sua vida, e
não é mais suportada pelo fornecedor.  Por esta razão, ela foi removida do GNU
Guix.")))

 (entry (commit "4e58dfee6c7456d1e662f66041b8a157efe8710a")
        (title
         (en "More capable @code{privileged-programs} replace @code{setuid-programs}")
         (de "Befähigtere @code{privileged-programs} ersetzen @code{setuid-programs}")
         (nl "Capabelere @code{privileged-programs} vervangen @code{setuid-programs}")
         (pt "O mais capacitado @code{privileged-programs} substitui @code{setuid-programs}"))
        (body
         (en "Where the kernel supports it, Guix System can now assign
POSIX@tie{}@dfn{capabilities} to trusted executables.  Capabilities offer a
more granular alternative to the traditional setuid and setgid permissions,
which remain available.

To reflect this, @code{(gnu system setuid)} has been renamed to @code{(gnu
system privilege)}.  @code{privileged-programs} replaces @code{setuid-programs}
as @code{operating-system} field and defaults to
@code{%default-privileged-programs}.  The executables themselves have moved from
@file{/run/setuid-programs} to @file{/run/privileged/bin}.")
         (de "Wo der Kernel dies unterstützt, kann Guix System nun
POSIX-@dfn{Capabilities} an die Anwendungen vergeben, denen besonders vertraut
wird.  Capabilities bieten eine feinmaschigere Alternative zu den klassischen
setuid- und setgid-Berechtigungen, die auch verfügbar bleiben.

Um dies deutlich zu machen, heißt @code{(gnu system setuid)} nun @code{(gnu
system privilege)}.  Das Feld @code{privileged-programs} ersetzt
@code{setuid-programs} in jedem @code{operating-system}-Objekt und sein
Vorgabewert ist @code{%default-privileged-programs}.  Die ausführbaren Dateien
wurden von @file{/run/setuid-programs} nach @file{/run/privileged/bin}
verschoben.")
         (nl "Waar de kernel dit toelaat kan Guix System nu
POSIX@tie{}@dfn{capabilities} toewijzen aan vertrouwde uitvoerbare bestanden.
``Capabilities'' zijn een fijnmaziger alternatief voor de klassieke setuid- en
setgid-rechten, die ook beschikbaar blijven.

Om dit duidelijk te maken heet @code{(gnu system setuid)} nu @code{(gnu system
privilege)}.  @code{privileged-programs} vervangt @code{setuid-programs} als
veld in het @code{operating-system} en heeft @code{%default-privileged-programs}
als standaardwaarde.  De uitvoerbare bestanden verhuizen van
@file{/run/setuid-programs} naar @file{/run/privileged/bin}.")
         (pt "Onde houver suporte do kernel, o Sistema Guix pode agora atribuir
POSIX@tie{}@dfn{capacidades} a executáveis confiáveis.  Capacidades oferecem
uma alternativa mais granular às permissões setuid e setgid, que premanecem
disponíveis.

Para refletir isto, @code{(gnu system setuid)} foi renomeado para @code{(gnu
system privilege)}.  @code{privileged-programs} substitui
@code{setuid-programs} como campo em @code{operating-system} e por padrão é
@code{%default-privileged-programs}.  Os executáveis em su foram movidos de
@file{/run/setuid-programs} para @file{/run/privileged/bin}.")))

 (entry (commit "26638b8e8129aa755586d017677b4cf076bafda6")
        (title
         (en "The containerd service is separated from @code{docker-service-type}")
         (pt "O serviço containerd está separado de @code{docker-service-type}")
         (ru "Сервис containerd отделен от @code{docker-service-type}")
         (de "containerd-Dienst wurde vom @code{docker-service-type} getrennt"))
        (body
         (en "containerd service has been decoupled from the
@code{docker-service-type}.  Moving forward, users are required to specify
containerd settings manually for their configurations.  The
@code{containerd-service-type} service need to be added to a system
configuration, otherwise a message about not any service provides
@code{containerd} will be displayed during @code{guix system reconfigure}.

Run @command{info \"(guix) Miscellaneous Services\"} for more info.")
         (pt "O serviço containerd foi desacoplado do
@code{docker-service-type}.  Daqui por diante, usuários são exigidos a
especificar definições containerd manualmente para suas configurações.
O serviço @code{containerd-service-type} precisa ser adicionado a uma
configuração de sistema, caso contrário uma mensagem sobre nenhum serviço
fornecer @code{containerd} será exibida durante @code{guix system
reconfigure}.

Execute @command{info \"(guix.pt_BR) Serviços diversos\"} para mais info.")
         (ru "Сервис containerd был отделен от @code{docker-service-type}.
Впредь пользователям потребуется указывать параметры containerd вручную для
своих конфигураций.  Сервис @code{containerd-service-type} должен быть
добавлен в конфигурацию системы, в противном случае будет отображено сообщение
о том, что ни один сервис не предоставляет поддержку для @code{containerd} во
время выполнения команды @code{guix system reconfigure}.

Смотрите @command{info \"(guix.ru) Разнообразные службы\"} для получения более
детальных сведений.")
         (de "Es gibt einen eigenen containerd-Dienst losgelöst von
@code{docker-service-type}.  In Zukunft müssen Nutzer dort manuelle
Einstellungen für containerd vornehmen.  Der Dienst
@code{containerd-service-type} muss zur Systemkonfiguration hinzugefügt
werden, sonst wird durch @code{guix system reconfigure} eine Meldung
gezeigt, dass kein Dienst @code{containerd} zur Verfügung stellt.

Siehe @command{info \"(guix.de) Verschiedene Dienste\"} für genauere
Informationen.")))

 (entry (commit "ee7e5e00bf2b9257e67d785b37efddb008c5da37")
        (title
         (en "Plasma updated to version 6.1.2")
         (de "Plasma auf Version 6.1.2 aktualisiert")
         (fr "Plasma passe à la version 6.1.2")
         (pt "Plasma atualizado para a versão 6.1.2")
         (zh "Plasma 更新到 6.1.2 版本"))
        (body
         (en "Plasma updated to 6.1.2, KDE Frameworks updated to 6.3.0, and
other KDE package updates.

With Plasma updates, SDDM has QT6 enabled by default. If you want to still use a
Qt5 theme, you need to set the field @code{sddm} in @code{sddm-configuration} to
@code{sddm-qt5}.")
         (de "Plasma wurde auf 6.1.2 aktualisiert, KDE Frameworks wurde
auf 6.3.0 aktualisiert und andere KDE-Pakete wurden aktualisiert.

Mit der Aktualisierung von Plasma ist in SDDM die Version qt6
vorgegeben.  Wenn Sie ein Qt5-Thema benutzen möchten, müssen Sie
in der @code{sddm-configuration} das Feld @code{sddm} auf
@code{sddm-qt5} setzen.")
         (fr "Plasma passe à la version 6.1.2, KDE Frameworks à la 6.3.0, et
  d’autres logiciels KDE sont mis à jour.

  Avec ces mises à jour de Plasma, sddm utilise Qt 6 par défaut.  Pour
  utiliser un thème Qt 5, il faut mettre le champ @code{sddm} de
  @code{sddm-configuration} à @code{sddm-qt5}.")
         (pt "Plasma atualizado para 6.1.2, KDE Frameworks atualizado para
6.3.0, e outras atualizações dos pacotes KDE.

Com as atualizações do Plasma, SDDM tem Qt6 habilitado por padrão.  Se você
ainda quer usar umm tema Qt5, você precisa definir o campo @code{sddm} em
@code{sddm-configuration} para @code{sddm-qt5}.")
         (zh "Plasma 更新到 6.1.2, KDE Frameworks 更新到 6.3.0, 及其他 KDE 软件包更新。

随着 Plasma 更新, SDDM 默认启用 Qt6, 如果您还想使用 Qt5 主题, 需要将 @code{sddm-configuration} 中的
@code{sddm} 字段设置为 @code{sddm-qt5}.")))

 (entry (commit "a46908620fac09bd8ccd0f587a27e86035d3b1d7")
        (title
         (en "@code{stumpwm:lib} removed")
         (de "@code{stumpwm:lib} wurde entfernt")
         (fr "@code{stumpwm:lib} supprimé")
         (pt "@code{stumpwm:lib} removido"))
        (body
         (en "The @code{lib} output of the @code{stumpwm} package has been
removed.  If you have some personal package definitions depending on
@code{stumpwm:lib}, they should be updated to depend on @code{stumpwm}
instead.")
         (de "Die Ausgabe @code{lib} des Pakets @code{stumpwm} gibt es nicht
mehr.  Wenn Sie eigene Paketdefinitionen haben, die von @code{stumpwm:lib}
abhängen, müssen sie angepasst werden, um stattdessen von @code{stumpwm}
abzuhängen.")
         (fr "La sortie @code{lib} du paquet @code{stumpwm} a été supprimée.
Si vous avez des définitions de paquets personnels dépendantes de
@code{stumpwm:lib}, elle doivent être modifiées pour dépendre de
@code{stumpwm} à la place.")
         (pt "A saída @code{lib} do pacote @code{stumpwm} foi removida.  Se
você tem alguma definição de pacote particular que dependa de
@code{stumpwm:lib}, ela deve ser atualizada para depender de @code{stumpwm} em
vez disso.")))

 (entry (commit "6fad0fd1c32db2cb25447b694f08d5c7836536ad")
        (title
         (en "Linux-libre 6.8 removed due to end of upstream support")
         (de "Linux-libre 6.8 wurde entfernt")
         (pt "Linux-libre 6.8 removido em razão do fim do suporte do fornecedor"))
        (body
         (en "The linux-libre 6.8 kernel series has reached the end of
             its life, and is no longer supported upstream.  For this
             reason, it has been removed from GNU Guix.")
         (de "Die @code{linux-libre} 6.8-Versionsreihe hat ihr
Supportende erreicht und wird nicht mehr unterstützt („end of life“). Daher ist die
Versionsreihe aus GNU Guix entfernt worden.")
         (pt "A série 6.8 do kernel linux-libre alcançou o fim de sua vida, e
não é mais suportada pelo fornecedor.  Por esta razão, ela foi removida do GNU
Guix.")))

 (entry (commit "8d1d98a3aa3448b9d983e4bd64243a938b96e8ab")
        (title
         (en "@command{guix git authenticate} usage simplified")
         (de "@command{guix git authenticate} ist leichter nutzbar")
         (fr "@command{guix git authenticate} simplifiée")
         (pt "Uso de @command{guix git authenticate} simplificado"))
        (body
         (en "Usage of the @command{guix git authenticate} command has been
simplified.  The command is useful to channel authors and to developers
willing to validate the provenance of their code.

On your first use, @command{guix git authenticate} will now record the commit
and signer (the @dfn{introduction}) in the @file{.git/config} file of your
repository so that you don't have to pass them on the command line in
subsequent runs.  It will also install pre-push and post-merge hooks,
unless preexisting hooks are found.

Run @command{info \"(guix) Invoking guix authenticate\"} for more info.")
         (de "Der Befehl @command{guix git authenticate} kann jetzt einfacher
benutzt werden.  Mit dem Befehl können Kanalautoren und Entwickler die
Provenienz ihres Codes überprüfen.

Beim ersten Gebrauch speichert @command{guix git authenticate} Commit und
Unterzeichner (wie in der @dfn{Kanaleinführung}) in der Datei
@file{.git/config} Ihres Repositorys, so dass Sie sie bei späteren
Ausführungen nicht mehr auf der Befehlszeile angeben müssen.  Auch werden
Git-Hooks für pre-push und post-merge installiert, wenn es bisher keine
Hooks dieser Art gibt.

Führen Sie @command{info \"(guix.de) Aufruf von guix git authenticate\"}
aus, wenn Sie mehr wissen wollen.")
         (fr "L'utilisation de la commande @command{guix git authenticate} a
été simplifiée.  Cette commande est utile aux auteur·rices de canaux et aux
développeur·euses souhaitant pouvoir valider l'origine de leur code.

À la première utilisation, @command{guix git authenticate} enregistre
désormais le commit et signataire (l'@dfn{introduction}) dans le fichier
@file{.git/config} du dépôt, ce qui permet de ne pas avoir à les spécifier sur
la ligne de commande les fois suivantes.  La commande installe aussi des
crochets « pre-push » et « post-merge », sauf si des crochets préexistants
sont trouvés.

Lancer @command{info \"(guix.fr) Invoquer guix git authenticate\"} pour en
savoir plus.")
         (pt "O uso do comando @command{guix git authenticate} foi
simplificado.  O comando é útil para autores de canais e para desenvolvedores
interessados em validar a procedência de seus códigos.

No seu primeiro uso, @command{guix git authenticate} agora anotará o commit e
o assinante (a @dfn{introduction}) no arquivo @file{.git/config} de seu
repositório de forma que você não tenha que passá-los pela linha de comando em
execuções seguintes.  Ele também instalará ganchos (@i{hook}) de
pré-instalação e pós-instalação, a não ser que sejam encontrados ganchos
pré-existentes.

Execute @command{info \"(guix.pt_BR) Invocando guix git authenticate\"} para
mais info.")))

 (entry (commit "238a74c7dfd1469af064b445abcee38fd7408d5b")
        (title
         (en "Linux-libre 6.7 removed due to end of upstream support")
         (de "Linux-libre 6.7 wurde entfernt")
         (pt "Linux-libre 6.7 removido em razão do fim do suporte do fornecedor"))
        (body
         (en "The linux-libre 6.7 kernel series has reached the end of
             its life, and is no longer supported upstream.  For this
             reason, it has been removed from GNU Guix.")
         (de "Die @code{linux-libre} 6.7-Versionsreihe hat ihr
Supportende erreicht und wird nicht mehr unterstützt („end of life“). Daher ist die
Versionsreihe aus GNU Guix entfernt worden.")
         (pt "A série 6.7 do kernel linux-libre alcançou o fim de sua vida, e
não é mais suportada pelo fornecedor.  Por esta razão, ela foi removida do GNU
Guix.")))

 (entry (commit "67a3a83170c038d2eb084d3f53a7ea7b033aea74")
        (title
         (en "@code{nss-certs} is now included in @code{%base-packages}")
         (de "@code{nss-certs} ist jetzt Teil von @code{%base-packages}")
         (fr "@code{nss-certs} est maintenant inclus dans @code{%base-packages}")
         (pt "@code{nss-certs} agora está incluso em @code{%base-packages}"))
        (body
         (en "The @code{nss-certs} package is now included in the
@code{%base-packages}, the default value for the @code{packages} field of the
@code{operating-system} record.

If you are a Guix System user, in your operating system configuration file,
please no longer add @code{(specification->package \"nss-certs\")} to
@code{%base-packages}.")
         (de "Das Paket @code{nss-certs} ist jetzt in @code{%base-packages}
enthalten, dem Vorgabewert des @code{packages}-Felds im
@code{operating-system}-Verbundstyp.

Wenn Sie ein Nutzer von Guix System sind, fügen Sie bitte in Ihrer
Betriebssystemkonfigurationsdatei nicht mehr
@code{(specification->package \"nss-certs\")} zu @code{%base-packages} hinzu.")
         (fr "Le paquet @code{nss-certs} est maintenant inclus dans
@code{%base-packages}, la valeur par défaut du champ @code{packages} de
l'enregistrement @code{operating-system}.

Si vous êtes une utilisatrice ou un utilisateur du système Guix, dans le
fichier de configuration du système d'exploitation, veuillez ne plus ajouter
@code{(specification->package \"nss-certs\")} à @code{%base-packages}.")
         (pt "O pacote @code{nss-certs} agora está incluso em
@code{%base-packages}, o valor padrão para o campo @code{packages} do registro
@code{operating-system}.

Se você é usuário do Sistema Guix, em seu arquivo de configuração de sistema
operacional, favor não inclua @code{(specification->package \"nss-certs\")} ao
@code{%base-packages}.")))

 (entry (commit "b4aed68e960116b2b60f68ea1468d7a526149823")
        (title
         (en "Linux-libre LTS kernel updated to 6.6")
         (de "Linux-libre LTS Kernel wird auf 6.6 aktualisiert")
         (pt "Kernel linux-libre atualizado para 6.6"))
        (body
         (en "The default version of the @code{linux-libre-lts} kernel has been
             updated to the 6.6 longterm release series.")
         (de "Der standardmäßig verwendete @code{linux-libre-lts}-Kernel basiert
              jetzt auf der 6.6-Versionsreihe (Langzeitunterstützung).")
         (pt "A versão padrão do kernel @code{linux-libre-lts} foi atualizada
para a série de lançamentos de longo prazo 6.6.")))

 (entry (commit "523f3def65ab061a87f4fc9e6f9008e6a78fafb5")
        (title
         (en "GNOME updated to version 44 with a more modular desktop service")
         (de "GNOME auf Version 44 aktualisiert mit modularem Dienst")
         (fr "Mise à jour de GNOME en version 44 avec un service plus modulaire")
         (pt "GNOME atualizado para a versÃo 44 com um serviço de desktop mais modular")
         (zh "GNOME 44 更新，帶來更加模塊化的桌面服務"))
        (body
         (en "The @code{gnome-desktop-service-type} now differentiates between
shell, utilities, and extra-packages among other fields to bring more structure
in its configuration.

With the update to GNOME 44, some shell extensions have been deprecated and
others removed.  If any @code{gnome-shell-extension-@dots{}} package causes
an error while running your usual update routine, make sure to remove it from
your profile.")
         (de "Der Dienst @code{gnome-desktop-service-type} unterscheidet nun
unter anderem zwischen den Feldern shell, utilities und extra-packages, und
bringt so etwas mehr Struktur in die Konfiguration.

Mit dem Update zu GNOME 44 wurden einige Erweiterungen als obsolet deklariert
und andere entfernt.  Falls ein Paket, dessen Name mit
@code{gnome-shell-extension-} beginnt, zu einem Fehler während Ihrer
Update-Routine führt, entfernen Sie es von Ihrem Profil.")
         (fr "Le service @code{gnome-desktop-service-type} sépare
maintenant les champs @code{shell}, @code{utilities} et @code{extra-
packages} (entre autres) pour donner plus de structure à sa
configuration.

Pendant la mise à jour vers GNOME 44, certaines extensions du shell ont
été dépréciées et d’autres supprimées. Si un paquet nommé
@code{gnome-shell-extension-@dots{}} émet une erreur quand vous
effectuez la mise à jour, vous devriez l’enlever de votre profil.")
         (pt "O @code{gnome-desktop-service-type} agora diferencia entre
shell, utilities, e extra-packages entre outros campos para trazer mais
estrutura em sua configuração.

Com a atualização para o GNOME 44, algumas extensões de shell foram
depreciadsas e outras removidas.  Se qualquer pacote
@code{gnome-shell-extension-@dots{}} causar um erro durante a execução de sua
rotina habitual de atualização, certifique-se de removê-la do seu perfil.")
         (zh "@code{gnome-desktop-service-type} 設置新增 @code{shell}、
@code{utilities}、@code{extra-packages} 等字段，使得 GNOME 桌面配置更加模塊化。

隨着 GNOME 44 更新，一些 GNOME Shell 拓展已被棄用或刪除。更新中若有關於
@code{gnome-shell-extension-@dots{}} 軟件包的錯誤，請將對應軟件包從 profile 中
刪除。")))

 (entry (commit "06d01c610e3bee61e38a177aecda5982d5b338ae")
        (title
         (en "The GNOME Display Manager uses Wayland by default")
         (de "GNOME Display Manager nutzt nun Wayland als Vorgabe")
         (fr "GDM utilise Wayland par défaut")
         (pt "O GNOME Display Manager usa Wayland por padrão")
         (zh "GNOME 顯示管理器（GDM）服務默認啓用 Wayland 支持"))
        (body
         (en "The @code{gdm-service-type} is configured to use Wayland instead
of Xorg by default.")
         (de "Der Dienst @code{gdm-service-type} verwendet nun Wayland
als Vorgabe anstelle von Xorg.")
         (fr "Le service @code{gdm-service-type} est configuré par défaut pour
utiliser Wayland au lieu de Xorg.")
         (pt "O @code{gdm-service-type} está configurado para usar Wayland por
padrão em vez de Xorg.")
         (zh "@code{gdm-service-type} 預設已由 Xorg 改爲 Wayland。")))

 (entry (commit "498db4de1f09414adf68a3a383f0178434035179")
        (title
         (en "The udev service also manages hardware configuration files")
         (de "Udev verwaltet nun auch Hardwarekonfigurationen")
         (fr "Le service udev gère maintenant les configurations de matériel")
         (pt "O serviço udev também gerencia arquivos de configuração de hardware")
         (zh "udev 服務現可管理硬件配置文件"))
        (body
         (en "The @code{udev-service-type} can now be configured and extended
with eudev hardware configuration files (named @dfn{hwdb} by the eudev
project).")
         (de "Der Udev-Dienst kann nun mit Hardwaredatenbanken (auch als
@dfn{hwdb} bekannt) konfiguriert und erweitert werden.")
         (fr "Le type de service @code{udev-service-type} peut maintenant être
configuré et étendu avec des fichiers de configuration de matériel (appelés
@dfn{hwdb} par le projet eudev).")
         (pt "O @code{udev-service-type} pode agora ser configurado e
estendido com arquivos de configuração de hardware para eudev (nomeados
@dfn{hwdb} pelo projeto eudev).")
         (zh "現可使用 eudev 的硬件配置文件（@dfn{hwdb}）設置及拓展
@code{udev-service-type}。")))

 (entry (commit "ff1251de0bc327ec478fc66a562430fbf35aef42")
        (title
         (en "Daemon vulnerability allowing store corruption has been fixed")
         (de "Schwachstelle im Daemon behoben, durch die der Store verfälscht werden konnte")
         (fr "Une faille du démon permettant de corrompre le dépôt a été corrigée")
         (pt "Vulnerabilidade no daemon permitindo corrupção do armazém foi consertada"))
        (body
         (en "A vulnerability in the build daemon, @command{guix-daemon}, was
identified and fixed.  The vulnerability would allow unprivileged users to
corrupt the result of @dfn{fixed-output derivations} such as source code
tarballs and Git checkouts, which in turn could lead to local privilege
escalation.

This bug is fixed and Guix System users are advised to upgrade their system,
with a command along the lines of:

@example
sudo guix system reconfigure /run/current-system/configuration.scm
sudo herd restart guix-daemon
@end example

If you are using Guix on another distro, run @command{info \"(guix) Upgrading
Guix\"} or visit
@uref{https://guix.gnu.org/manual/devel/en/html_node/Upgrading-Guix.html} to
learn how to upgrade Guix.

See @uref{https://issues.guix.gnu.org/69728} for more information on this
issue.")
         (de "Eine Sicherheitslücke im Erstellungs-Daemon,
@command{guix-daemon}, wurde gefunden und geschlossen.  Sie hatte es
unprivilegierten Nutzern ermöglicht, das Ergebnis einer @dfn{Ableitung mit
fester Ausgabe}, wie Quellcode-Tarballs und Git-Checkouts, zu manipulieren.
So war eine lokale Rechteausweitung möglich.

Der Fehler ist behoben und wir raten Nutzern von Guix System, ihr System zu
aktualisieren mit einem Befehl wie:

@example
sudo guix system reconfigure /run/current-system/configuration.scm
sudo herd restart guix-daemon
@end example

Wenn Sie Guix auf einer anderen Distribution verwenden, erfahren Sie mit dem
Befehl @command{info \"(guix.de) Aktualisieren von Guix\"} oder auf
@uref{https://guix.gnu.org/manual/devel/de/html_node/Aktualisieren-von-Guix.html},
wie Sie Guix aktualisieren.

Siehe @uref{https://issues.guix.gnu.org/69728} für mehr Informationen zu dem
Fehler.")
         (fr "Une faille de sécurité du démon de compilation,
@command{guix-daemon}, a été identifiée et corrigée.  La faille permettait à
un·e utilisateur·rice sans privilège de corrompre le résultat d'une
@dfn{dérivation à sortie fixe} telle qu'une archive ou un @i{checkout} Git, ce
qui peut ensuite permettre une élévation locale de privilèges.

Ce problème est corrigé et les utilisateur·rices de Guix System sont invité·es
à mettre à jour leur système avec une commande telle que :

@example
sudo guix system reconfigure /run/current-system/configuration.scm
sudo herd restart guix-daemon
@end example

Pour voir comment mettre à jour Guix sur une autre distribution, lancer
@command{info \"(guix.fr) Mettre à niveau Guix\"} ou visiter
@uref{https://guix.gnu.org/manual/devel/fr/html_node/Mettre-a-niveau-Guix.html}.

Voir @uref{https://issues.guix.gnu.org/69728} pour plus d'informations sur
cette anomalie.")
         (pt "Uma vulnerabilidade no daemon de construção,
@command{guix-daemon}, foi identificada e consertada.  A vulnerabilidade
permitiria que usuários não privilegiados corrompessem o resultado de uma
@dfn{derivação de saída fixa} como tarballs de código-fonte e @i{checkouts}
de Git, o que por sua vez levaria a escalação local de privilégios.

Este erro está consertado e aconselhamos os usuários do Sistema Guix a
atualizar seus sistemas, com um comando nas linhas de:

@example
sudo guix system reconfigure /run/current-system/configuration.scm
sudo herd restart guix-daemon
@end example

Se você usa Guix em outra distro, execute @command{info \"(guix.pt_BR)
Atualizando o Guix\"} ou visite
@uref{https://guix.gnu.org/manual/devel/pt-br/html_node/Atualizando-o-Guix.html}
para aprender como atualizar o Guix.

Veja @uref{https://issues.guix.gnu.org/69728} para mais informação sobre essa
questão.")))

 (entry (commit "10a193596368443f441077525ebbddf787d91e4b")
        (title
         (en "Linux-libre 4.14 removed due to end of upstream support")
         (de "Linux-libre 4.14 wurde entfernt")
         (pt "Linux-libre 4.14 removido em razão do fim do suporte do fornecedor"))
        (body
         (en "The linux-libre 4.14 kernel series has reached the end of
             its life, and is no longer supported upstream.  For this
             reason, it has been removed from GNU Guix.")
         (de "Die @code{linux-libre} 4.14-Versionsreihe hat ihr
Supportende erreicht und wird nicht mehr unterstützt („end of life“). Daher ist die
Versionsreihe aus GNU Guix entfernt worden.")
         (pt "A série 4.14 do kernel linux-libre alcançou o fim de sua vida, e
não é mais suportada pelo fornecedor.  Por esta razão, ela foi removida do GNU
Guix.")))

 (entry (commit "519e1e3eb88ec532fc83ebb742d9919269b57c87")
        (title
         (de "Neue Option @samp{--max-layers=N} für den Befehl @command{guix pack}")
         (en "New @samp{--max-layers=N} option for the @command{guix pack} command")
         (ru "Новая опция @samp{--max-layers=N} для @command{guix pack} команды")
         (en "Nova opção @samp{--max-layers=N} para o comando @command{guix pack}"))
        (body
         (de "Sie können jetzt auch mehrschichtige Docker-Abbilder mit dem Befehl
@command{guix pack --format=docker --max-layers=N} erzeugen. Damit bekommen Sie ein
Docker-Abbild, bei dem Store-Pfade auf getrennten Schichten („Layer“)
untergebracht sind, die sich mehrere Abbilder teilen können.  Das Abbild wird
im Store als gzip-komprimierter Tarball erzeugt.  Hier ist ein einfaches
Beispiel, wo ein mehrschichtiges Docker-Abbild für das Paket @code{hello}
angelegt wird:

@example
guix pack --format=docker --max-layers=N --symlink=/usr/bin/hello=bin/hello hello
@end example

@command{guix system image} kann jetzt geschichtete Docker-Abbilder erzeugen,
indem Sie @code{--max-layers=N} übergeben.

Siehe @command{info \"(guix.de) Aufruf von guix pack\"} und
@command{info \"(guix.de) Systemabbilder\"} für weitere Informationen.")
         (en "Docker layered images can now be produced via the @command{guix
pack --format=docker --max-layers=N} command, providing a Docker image with
many of the store paths being on their own layer to improve sharing between
images.  The image is realized into the GNU store as a gzipped tarball.  Here
is a simple example that generates a layered Docker image for the @code{hello}
package:

@example
guix pack --format=docker --max-layers=N --symlink=/usr/bin/hello=bin/hello hello
@end example

The @command{guix system image} can now produce layered Docker image by passing
@code{--max-layers=N}.

See @command{info \"(guix) Invoking guix pack\"} and
@command{info \"(guix) System Images\"} for more information.")
         (ru "Появилась команда создания многослойных Docker образов с помощью
@command{guix pack --format=docker --max-layers=N}, которая соберет Docker образ с
путями в store расположенными на отдельных слоях, ускоряя таким образом
передачу образов.  Образ будет создан в GNU store в качестве gzipped tarball.

Пример создания Docker layered образ с @code{hello} пакетом:
@example
guix pack --format=docker --max-layers=N --symlink=/usr/bin/hello=bin/hello hello
@end example

@command{guix system image} теперь может создавать layered Docker образ путем
указания опции @option{--max-layers=N}.

Смотрите @command{info \"(guix) Invoking guix pack\"} и
@command{info \"(guix) System Images\"} для получения более детальных
сведений.")
         (pt "Imagens Docker empilhadas já podem ser produzidas pelo comando
@command{guix pack --format=docker --max-layers=N}, fornecendo uma imagem
Docker com boa parte dos caminhos do armazém em sua própria camada a fim de
melhorar o compartilhamento entre imagens.  A imagem é realizada no armazém
GNU como um tarball compactado com gzip.  Eis um exemplo simples que gera uma
imagem Docker empilhada para o pacote @code{hello}:

@example
guix pack --format=docker --max-layers=N --symlink=/usr/bin/hello=bin/hello hello
@end example

O comando @command{guix system image} agora pode produzir uma imagem Docker
empilhada passando @code{--max-layers=N}.

Veja @command{info \"(guix.pt_BR) Invocando guix pack\"} e @command{info
\"(guix.pt_BR) Imagens do sistema\"} para mais informação.")))

 (entry (commit "953c65ffdd43c02c934518fb7a1c68542584b223")
        (title
         (en "Declarative offloading on Guix System and childhurds")
         (de "Auslagern kann deklariert werden auf Guix System und Childhurds")
         (fr "Déclaration du déchargement dans Guix System et childhurds")
         (pt "Descarga declarative no Sistema Guix e hurds-filhos"))
        (body
         (en "When configuring Guix System, it is now possible to declare
build machines to offload to directly from the @code{operating-system}
declaration by specifying the @code{build-machines} field of
@code{guix-configuration}.  When you do this, @command{guix system} generates
a @file{/etc/guix/machines.scm} file by concatenating the @code{(build-machine
@dots{})} expressions you specified.

This mechanism is used by @code{hurd-vm-service-type}, also known as
``childhurd'', to create virtual machines running the GNU/Hurd operating
system one can offload to, without additional configuration steps.

Run @command{info \"(guix) Daemon Offload Setup\"} for more info on
offloading; run @command{info \"(guix) Virtualization Services\"} for info on
@code{hurd-vm-service-type}.")
         (de "In der Betriebssystemkonfiguration von Guix System können jetzt
auch die Erstellungsmaschinen deklariert werden, an die Erstellungen ausgelagert
werden.  Diese geben Sie im Feld @code{build-machines} einer
@code{guix-configuration} an.  In diesem Fall wird @command{guix system} eine
Datei @file{/etc/guix/machines.scm} anlegen, die der Aneinanderreihung der
angegebenen @code{(build-machine @dots{})}-Ausdrücke entspricht.

Der Mechanismus wird für @code{hurd-vm-service-type} eingesetzt, auch bekannt
als „Childhurd“, um virtuelle Maschinen anzulegen, auf denen das
GNU/Hurd-Betriebssystem läuft und auf die man auslagern kann, ohne weitere
Einstellungen vorzunehmen.

Führen Sie @command{info \"(guix.de) Auslagern des Daemons einrichten\"} aus, um
mehr Informationen über das Auslagern zu bekommen; führen Sie
@command{info \"(guix.de) Virtualisierungsdienste\"} aus für Informationen zu
@code{hurd-vm-service-type}.")
         (fr "Dans sa configuration Guix System, il est désormais possible de
déclarer les machines de construction vers lesquelles se décharger directement
dans la déclaration @code{operating-system} en spécifiant le champ
@code{build-machines} de @code{guix-configuration}.  Quand on fait cela,
@command{guix system} produit un fichier @file{/etc/guix/machines.scm} en
concaténant les expressions @code{(build-machine @dots{})} fournies.

Ce mécanisme est utilisé par @code{hurd-vm-service-type}, également appelé
« childhurd », pour créer des machines virtuelles faisant tourner le système
d'exploitation GNU/Hurd vers lesquelles se décharger, sans étape
supplémentaire de configuration.

Lancer @command{info \"(guix.fr) Réglages du déchargement du démon\"} pour
plus d'informations sur le délestage ; lancer @command{info \"(guix.fr)
Services de virtualisation\"} pour en apprendre sur
@code{hurd-vm-service-type}.")
         (pt "Ao configurar o Sistema Guix, agora é possível declarar máquinas
de construção para descarregar diretamente da declaração de
@code{operating-system} especificando o campo @code{build-machines} de
@code{guix-configuration}.  Quando fizer isso, @command{guix system} gera um
arquivo @file{/etc/guix/machines.scm} concatenando as expressões
@code{(build-machine @dots{})} que você especificou.

Este mecanismo é utilizado por @code{hurd-vm-service-type}, também conhecido por
``hurd-filho'', para criar máquinas virtuais executando o sistema operacional
GNU/Hurd operating para as quais se pode descarregar, sem configurações
adicionais.

Execute @command{info \"(guix.pt_BR) Configuração de descarregamento de
daemon\"} para mais info sobre descarga; execute @command{info \"(guix.pt_BR)
Serviços de virtualização\"} para info sobre @code{hurd-vm-service-type}.")))

 (entry (commit "db775e7367e8deffb513aad94f4afb875d796d0b")
        (title
         (en "Linux-libre 6.3 removed due to end of upstream support")
         (de "Linux-libre 6.3 wurde entfernt")
         (pt "Linux-libre 6.3 removido em razão do fim do suporte do fornecedor"))
        (body
         (en "The linux-libre 6.3 kernel series has reached the end of
             its life, and is no longer supported upstream.  For this
             reason, it has been removed from GNU Guix.")
         (de "Vom Kernel @code{linux-libre} wird die 6.3-Versionsreihe keine
Unterstützung von dessen Anbieter mehr erfahren („end of life“).  Daher ist es
aus GNU Guix entfernt worden.")
         (pt "A série 6.3 do kernel linux-libre alcançou o fim de sua vida, e
não é mais suportada pelo fornecedor.  Por esta razão, ela foi removida do GNU
Guix.")))

 (entry (commit "bff1f2d4d07e934ea296f9c724b5337996a27c44")
        (title
         (en "Linux-libre kernel updated to 6.4")
         (de "Linux-libre-Kernel wird auf 6.4 aktualisiert")
         (fr "Le noyau linux-libre est mis à jour vers la 6.4")
         (pt "Kernel linux-libre atualizado para 6.4"))
        (body
         (en "The default version of the linux-libre kernel has been updated to
              the 6.4 release series.")
         (de "Der standardmäßig verwendete @code{linux-libre}-Kernel basiert
              jetzt auf der 6.4-Versionsreihe.")
         (fr "La version par défaut du noyau linux-libre est mise à jour
              vers la série des 6.4.")
         (pt "A versão padrão do kernel linux-libre foi atualizada para a
              série do kernel 6.4.")))

 (entry (commit "1b7aabbc79969a89141aadd3d41d7a5329a3462e")
        (title
         (en "New @command{guix locate} command")
         (de "Neuer Befehl @command{guix locate}")
         (fr "Nouvelle commande @command{guix locate}")
         (pt "Novo comando @command{guix locate}"))
        (body
         (en "The new @command{guix locate} command lets you search for
packages containing a given file---at long last!  For instance, to find which
package(s) provide a file named @file{ls}, run:

@example
guix locate ls
@end example

Currently the command relies on purely local information.  It is thus unable
to find packages that have not reached your store.  This limitation will be
lifted in a future revision.

Run @command{info \"(guix) Invoking guix locate\"} for more info.")
         (de "Mit dem neuen Befehl @command{guix locate} können Sie nach
Paketen suchen, die eine angegebene Datei enthalten — endlich ist es
soweit!  Um zum Beispiel das Paket bzw.@: die Pakete zu finden, die eine
Datei namens @file{ls} bereitstellen, führen Sie aus:

@example
guix locate ls
@end example

Derzeit benutzt der Befehl ausschließlich lokal vorliegende
Informationen.  Daher können Sie damit nur Pakete finden, die sich in
Ihrem Store-Verzeichnis befinden.  Diese Einschränkung werden wir in
einer zukünftigen Version aufheben.

Führen Sie @command{info \"(guix) Invoking guix locate\"} aus, um mehr zu
erfahren.")
         (fr "La nouvelle commande @command{guix locate} permet de chercher le
ou les paquets contenant un fichier donné---enfin !  Par exemple, pour trouver
quel paquet fournit un fichier nommé @file{ls}, on lance :

@example
guix locate ls
@end example

Pour le moment la commande se base uniquement sur des informations locales.
Elle ne peut donc pas trouver des paquets absents de votre dépôt.  Cette limitation
sera levée dans une prochaine version.

Lancer @command{info \"(guix) Invoking guix locate\"} pour plus d'informations.")
         (pt "O novo comando @command{guix locate} te permite pesquisar por
pacotes contendo um dado arquivo --- finalmente!  Por exemplo, para encontrar
que pacotes fornecem um arquivo de nome @file{ls}, execute:

@example
guix locate ls
@end example

Correntemente o comando se apóia em informação puramente local.  Portanto, é
incapaz de encontrar pacotes que ainda não alcançaram teu armazém.  Esta
limitação será superada em uma revisão futura.

Execute @command{info \"(guix.pt_BR) Invocando guix locate\"} para mais
info.")))

 (entry (commit "ba5da5125a81307500982517e2f458d57b024668")
        (title
         (en "New @code{arguments} rule for @command{guix style}")
         (de "Neue Stilregel @code{arguments} für @command{guix style}")
         (fr "Nouvelle règle @code{arguments} pour @command{guix style}")
         (pt "Nova regra @code{arguments} para @command{guix style}"))
        (body
         (en "The @command{guix style} command has a new @dfn{styling rule}
for package definitions.  Package writers may now run the following command:

@example
guix style -L /path/to/channel -S arguments @var{package}
@end example

This command rewrites the @code{arguments} field of @var{package} so that it
uses G-expressions instead of classical quasiquotation.

Run @command{info \"(guix) Invoking guix style\"} for more info.")
         (de "Der Befehl @command{guix style} verfügt über eine neue @dfn{Stilregel}
für Paketdefinitionen.  Paketautoren können jetzt folgenden Befehl benutzen:

@example
guix style -L /pfad/zum/kanal -S arguments @var{Paket}
@end example

Dadurch wird das Feld @code{arguments} in @var{Paket} so umgeschrieben, dass
G-Ausdrücke (gexps) anstelle von klassischer Quasiquotierung verwendet
werden.

Führen Sie @command{info \"(guix.de) Aufruf von guix style\"} aus, um
mehr Informationen zu erhalten.")
         (fr "La commande @command{guix style} a une nouvelle @dfn{règle de
style} pour les définitions de paquets.  Les auteurices de paquets peuvent
maintenant lancer la commande suivante :

@example
guix style -L /chemin/vers/canal -S arguments @var{paquet}
@end example

Cette commande réécrit le champ @code{arguments} de @var{paquet} pour qu'il
utilise des G-expressions plutôt que des quasicitations classiques.

Lancer @command{info \"(guix.fr) Invoquer guix style\"} pour plus
d'informations.")
         (pt "O comando @command{guix style} tem uma nova @dfn{regra
estilística} para definições de pacote.  Escritores de pacotes podem agora
executar o comando a seguir:

@example
guix style -L /path/to/channel -S arguments @var{package}
@end example

Este comando reescreve o campo @code{arguments} de @var{package} de forma que
ele use G-expressões em vez da quasi-quotação clássica.

Execute @command{info \"(guix.pt_BR) Invocando guix style\"} para mais
info.")))

 (entry (commit "ae11fcb84ac478dfa56d322ef08890645183a087")
        (title
         (en "New @option{--with-configure-flag} transformation option")
         (de "Neue Paketumwandlungsoption @option{--with-configure-flag}")
         (fr "Nouvelle option de transformation @option{--with-configure-flag}")
         (pt "Nova opção de transformação @option{--with-configure-flag}"))
        (body
         (en "The new @option{--with-configure-flag} package transformation
option lets you pass an additional configure flag to the build system of a
package.  For instance, here is how you would pass a flag to @command{cmake},
the build system of the @code{lapack} package:

@example
guix build lapack \\
  --with-configure-flag=lapack=-DBUILD_SHARED_LIBS=OFF
@end example

Run @command{info \"(guix) Package Transformation Options\"} for more info.")
         (de "Die neue Paketumwandlungsoption @option{--with-configure-flag}
macht es möglich, dem Erstellungssystem eines Pakets eine zusätzliche
Befehlszeilenoption für configure mitzugeben.  Zum Beispiel können Sie dem
@command{cmake}-Erstellungssystem des @code{lapack}-Pakets eine Option mitgeben:

@example
guix build lapack \\
  --with-configure-flag=lapack=-DBUILD_SHARED_LIBS=OFF
@end example

Führen Sie für mehr Informationen @command{info \"(guix.de)
Paketumwandlungsoptionen\"} aus.")
         (fr "La nouvelle option de transformation de paquets
@option{--with-configure-flag} permet de passer un drapeau supplémentaire au
système de construction d'un paquet.  Par exemple, voici comment on passerait
un drapeau à @command{cmake}, le système de construction du logiciel
@code{lapack} :

@example
guix build lapack \\
  --with-configure-flag=lapack=-DBUILD_SHARED_LIBS=OFF
@end example

Voir @command{info \"(guix.fr) Options de transformation de paquets\"} pour
plus de détails.")
         (pt "A nova opção de transformação de pacote
@option{--with-configure-flag} te permite passar uma sinalização de
configuração (configure flag) ao sistema de construção de um pacote.
Por exemplo, eis como passar um sinal ao @command{cmake}, o sistema de
construção do pacote @code{lapack}:

@example
guix build lapack \\
  --with-configure-flag=lapack=-DBUILD_SHARED_LIBS=OFF
@end example

Execute @command{info \"(guix.pt_BR) Opções de transformação de pacote\"} para
mais info.")))

 (entry (commit "c919bfefd98bf2e29549539b4e28e6dc2a8a6f32")
        (title
         (en "Core packages updated")
         (de "Kern-Pakete aktualisiert")
         (fr "Mise à jour des logiciels importants")
         (pt "Atualização dos pacotes essenciais"))
        (body
         (en "Core packages have been updated, following months of hard work
by contributors.  Noteworthy package upgrades include:

@itemize
@item glibc 2.35;
@item Python 3.10;
@item Perl 5.36;
@item Mesa 22;
@item GCC 11 is now used as the default compiler.
@end itemize

A major highlight is the introduction of the so-called @dfn{full-source
bootstrap}: packages are all built starting from a 500-byte program called
stage0, which is then used to build a higher-level interpreter, a basic Scheme
interpreter and C compiler (GNU Mes), and so on, until @acronym{GCC, the GNU
Compiler Collection} is finally built.  This is a premiere and a huge step
forward in terms of transparency of auditability.")
         (de "Die Pakete, die den Kern der Distribution ausmachen, haben eine
Aktualisierung bekommen.  Dies ist das Ergebnis monatelanger harter Arbeit
unserer Mitwirkenden.  Zu den wichtigen Paketaktualisierungen gehören:

@itemize
@item glibc 2.35,
@item Python 3.10,
@item Perl 5.36,
@item Mesa 22,
@item GCC 11 wird jetzt standardmäßig als Compiler benutzt.
@end itemize

Eine große Neuigkeit ist, dass Guix jetzt auf „Bootstrapping aus dem Quellcode
allein“ basiert: Grundlage der Pakete ist ein 500 Byte großes Programm namens
stage0, womit ein weiter abstrahierter Interpretierer kompiliert wird, dann ein
grundlegender Scheme-Interpretierer und C-Compiler (GNU Mes), und so geht es
weiter, bis endlich @acronym{GCC, die GNU Compiler Collection} erstellt wird. Es
ist eine Premiere und ein großer Schritt nach vorne, was Transparenz und
Auditierbarkeit angeht.")
         (fr "Les logiciels de base ont été mis à jour, après des mois de dur
labeur par les contributeur·rices du projet.  Parmi les mises à jour notables,
il y a :

@itemize
@item glibc 2.35 ;
@item Python 3.10 ;
@item Perl 5.36 ;
@item Mesa 22 ;
@item GCC 11 est dorénavant le compilateur par défaut.
@end itemize

Une autre nouveauté importante est l'introduction de @dfn{l'amorçage intégral
depuis le source} (@i{full-source bootstrap} en anglais) : les logiciels sont
compilés en partant d'un programme de 500 octets appelé stage0, à partir
duquel on compile un interprète de plus haut niveau, un interprète Scheme et
un compilateur C de base (GNU Mes), et ainsi de suite, jusqu'à ce que
@acronym{GCC, the GNU Compiler Collection} soit enfin compilé.  C'est une
première et un grand pas en avant en termes de transparence et
d'auditabilité.")
         (pt "Os pacotes essenciais foram atualizados, após meses de trabalho
duro dos contribuintes.  Atualizações de pacotes dignas de nota incluem:

@itemize
@item glibc 2.35;
@item Python 3.10;
@item Perl 5.36;
@item Mesa 22;
@item GCC 11 é agora usado como o compilador padrão.
@end itemize

Um grande destaque é a introdução da assim-chamada @dfn{iniciação completa a
partir das fontes} (@dfn{full-source bootstrap}): os pacotes são todos
construídos a prtir de um programa de 500 bytes chamado stage0, que é então
usado para construir um interpretador de nível mais alto, um interpretador
Scheme básico e um compilador C (GNU Mes), e assim por diante, até a coleção
de compiladores GNU (@acronym{GCC, the GNU Compiler Collection}) ser
finalmente construída.  Este é um passo imenso e fundamental em termos de
transparência da auditabilidade.")))

 (entry (commit "21564fada141bfba25d471518b293b6004244c3a")
        (title
         (en "Linux-libre LTS kernel updated to 6.1")
         (de "Linux-libre LTS Kernel wird auf 6.1 aktualisiert")
         (pt "Kernel linux-libre LTS atualizado para 6.1"))
        (body
         (en "The default version of the @code{linux-libre-lts} kernel has been
             updated to the 6.1 longterm release series.")
         (de "Der standardmäßig verwendete @code{linux-libre-lts}-Kernel basiert
              jetzt auf der 6.1-Versionsreihe (Langzeitunterstützung).")
         (pt "A versão padrão do kernel @code{linux-libre-lts} foi atualizada
para a série de lançamentos de longo prazo 6.1.")))

 (entry (commit "57db09aae73e3713a10c5253758d84e1046f80dc")
        (title
         (en "Using Guix within @command{guix shell --container}")
         (de "Guix innerhalb @command{guix shell --container} nutzen")
         (fr "Utilisation de Guix dans @command{guix shell --container}")
         (pt "Usando Guix dentro de @command{guix shell --container}"))
        (body
         (en "The @option{--container} (or @option{-C}) option lets you spawn
a container---an isolated software environment.  In some cases, it is useful
to use Guix from within the container, something that is normally not
possible.

The new @option{--nesting} (or @option{-W}) option lets you do exactly that: a
container created with that option will let you use @command{guix} commands,
including @command{guix shell -C}, @emph{inside} of it.

The example below shows how to evaluate a @file{guix.scm} file to build a
package from within an isolated container, which is useful if @file{guix.scm}
is untrusted:

@example
guix shell -CW -- guix build -f guix.scm
@end example

Run @command{info \"(guix) Invoking guix shell\"} for more information.")
         (de "Mit der Befehlszeilenoption @option{--container} (oder
@option{-C}) können Sie Container anlegen, also isolierte
Software-Umgebungen.  Allerdings will man manchmal Guix innerhalb eines
Containers nutzen, was unmöglich ist, wenn Sie es normal aufrufen.

Die neue Befehlszeilenoption @option{--nesting} (oder @option{-W}) ergänzt
genau das: In einem damit angelegten Container lassen sich
@command{guix}-Befehle, einschließlich @command{guix shell -C}, @emph{im
Container} benutzen.

Folgendes Beispiel zeigt, wie Sie eine Datei @file{guix.scm} auswerten lassen,
um ein Paket aus dem isolierten Container heraus erstellen zu lassen. Das
können Sie gebrauchen, wenn Sie @file{guix.scm} @emph{nicht} vertrauen:

@example
guix shell -CW -- guix build -f guix.scm
@end example

Führen Sie @command{info \"(guix.de) Aufruf von guix shell\"} aus, um mehr
zu erfahren.")
         (fr "L'option @option{--container} (ou @option{-C}) permet de
démarrer un conteneur---un environnement logiciel isolé.  Dans certains cas,
il peut être utile d'utiliser Guix à l'intérieur du conteneur, ce qui n'est
normalement pas possible.

La nouvelle option @option{--nesting} (ou @option{-W}) résoud ce problème : un
conteneur créé avec cette option permet d'utiliser des commandes
@command{guix} à l'intérieur, y compris @command{guix shell -C}.

L'exemple ci-dessous montre comment évaluer un fichier @file{guix.scm} pour
construire un paquet depuis un conteneur isolé, ce qui est utile si
@file{guix.scm} n'est pas de confiance :

@example
guix shell -CW -- guix build -f guix.scm
@end example

Lancer @command{info \"(guix.fr) Invoquer guix shell\"} pour plus
d'informations.")
         (pt "A opção @option{--container} (or @option{-C}) te permite gerar
um contêiner --- um ambiente de software isolado.  Em alguns casos, é útil
usar Guix dentro de um contêiner, algo que não é normalmente possível.

A nova opção @option{--nesting} (or @option{-W}) te permite fazer exatamente
isso: um contêiner criado com esta opção te permitirá usar comandos
@command{guix}, incluindo @command{guix shell -C}, @emph{dentro} dele.

O exemplo abaixo mostra como evaluar um arquivo @file{guix.scm} para construir
um pacote em um contêiner isolado, o que é útil se @file{guix.scm} não é
confiável:

@example
guix shell -CW -- guix build -f guix.scm
@end example

Execute @command{info \"(guix.pt_BR) Invocando guix shell\"} para mais
informação.")))

 (entry (commit "0e18c5e5bcb9204c278cfc75493d3b02b746d5c3")
        (title
         (en "Linux-libre kernel updated to 6.2")
         (de "Linux-libre-Kernel wird auf 6.2 aktualisiert")
         (fr "Le noyau linux-libre est mis à jour vers la 6.2")
         (pt "Kernel linux-libre atualizado para 6.2"))
        (body
         (en "The default version of the linux-libre kernel has been updated to
              the 6.2 release series.")
         (de "Der standardmäßig verwendete @code{linux-libre}-Kernel basiert
              jetzt auf der 6.2-Versionsreihe.")
         (fr "La version par défaut du noyau linux-libre est mise à jour
              vers la série des 6.2.")
         (pt "A versão padrão do kernel linux-libre foi atualizada para a
              série do kernel 6.2.")))

 (entry (commit "598f4c509bbfec2b983a8ee246cce0a0fe45ec7f")
        (title
         (de "Neues Format @samp{rpm} für den Befehl @command{guix pack}")
         (en "New @samp{rpm} format for the @command{guix pack} command")
         (fr "Nouveau format @samp{rpm} pour la commande @command{guix pack}")
         (pt "Novo formato @samp{rpm} para o comando @command{guix pack} "))
        (body
         (de "Sie können jetzt auch RPM-Archive (mit der Dateinamenserweiterung
.rpm) erzeugen mit dem Befehl @command{guix pack --format=rpm}.  Damit
haben Sie einen alternativen Distributionsweg für mit Guix erstellte
Software.  Hier sehen Sie ein einfaches Beispiel, wie Sie ein
RPM-Archiv für das Paket @code{hello} erzeugen:

@example
guix pack --format=rpm --symlink=/usr/bin/hello=bin/hello hello
@end example

Siehe @command{info \"(guix.de) Aufruf von guix pack\"} für mehr
Informationen.")
         (en "RPM archives (with the .rpm file extension) can now be produced
via the @command{guix pack --format=rpm} command, providing an alternative
distribution path for software built with Guix.  Here is a simple example that
generates an RPM archive for the @code{hello} package:

@example
guix pack --format=rpm --symlink=/usr/bin/hello=bin/hello hello
@end example

See @command{info \"(guix) Invoking guix pack\"} for more information.")
         (fr "Vous pouvez désormais produire une archive RPM (avec l'extension
.rpm) avec la commande @command{guix pack --format=rpm} qui propose
donc une nouvelle manière de distribuer les logiciels construits avec
Guix.  Voici un exemple permettant de générer une archive RPM pour le
paquet @code{hello} :

@example
guix pack --format=rpm --symlink=/usr/bin/hello=bin/hello hello
@end example

Consultez @command{info \"(guix.fr) Invoquer guix pack\"} pour plus
d'informations.")
         (pt "Arquivos RPM (com a extensão .rpm) já podem ser produzidos
mediante o comando @command{guix pack --format=rpm}, fornecendo uma via de
distribuição alternativa para softwares feitos com Guix.  Eis um exemplo
simples que gera um arquivo RPM para o pacote @code{hello}:

@example
guix pack --format=rpm --symlink=/usr/bin/hello=bin/hello hello
@end example

Veja @command{info \"(guix) Invoking guix pack\"} para mais informação.")))

 (entry (commit "137b91f03bbb7f1df71cf10c4f79ae57fbcea400")
        (title
         (en "New @option{--with-version} package transformation option")
         (de "Neue Paketumwandlungsoption @option{--with-version}")
         (fr "Nouvelle option de transformation @option{--with-version}")
         (pt "Nova opção para transformação de pacote @option{--with-version}"))
        (body
         (en "The new @option{--with-version} package transformation option
generalizes @option{--with-latest}: it gets the specified upstream release of
a package and uses it instead of the currently-packaged version.

For example, the command below would spawn GNOME Clocks built against GTK
4.7.0, skipping its test suite:

@example
guix shell gnome-clocks --with-version=gtk=4.7.0 \\
  --without-tests=gtk -- gnome-clocks
@end example

Run @command{info \"(guix) Package Transformation Options\"} for more info.")
         (de "Die neue Paketumwandlungsoption @option{--with-version}
verallgemeinert @option{--with-latest}: Mit ihr kann man angeben, welche
vom Anbieter veröffentlichte Version man anstelle der derzeit im Paket
vorgegebenen haben möchte.

Zum Beispiel kann mit folgendem Befehl ein für die GTK-Version 4.7.0
erstelltes GNOME Clocks aufgerufen werden, wobei der Testkatalog dafür
übersprungen wird.

@example
guix shell gnome-clocks --with-version=gtk=4.7.0 \\
  --without-tests=gtk -- gnome-clocks
@end example

Führen Sie für mehr Informationen @command{info \"(guix.de)
Paketumwandlungsoptionen\"} aus.")
         (fr "La nouvelle option de transformation de paquets
@option{--with-version} généralise @option{--with-latest} : elle permet de
spécifier quelle version amont d'un logiciel utiliser à la place de celle
actuellement fournie.

Par exemple, la commande ci-dessous démarre GNOME Clocks construit avec GTK
4.7.0, sans lancer sa suite de tests :

@example
guix shell gnome-clocks --with-version=gtk=4.7.0 \\
  --without-tests=gtk -- gnome-clocks
@end example

Voir @command{info \"(guix.fr) Options de transformation de paquets\"} pour
plus de détails.")
         (pt "A nova opção de transformação de pacote @option{--with-version}
generaliza @option{--with-latest}: ela pega o lançamento especificado do
fornecedor do pacote e o utiliza em vez da versão empacotada atualmente.

Por exemplo, o comando abaixo gera um GNOME Clocks construído contra a GTK
4.7.0, pulando a bateria de testes:

@example
guix shell gnome-clocks --with-version=gtk=4.7.0 \\
  --without-tests=gtk -- gnome-clocks
@end example

Execute @command{info \"(guix.pt_BR) Opções de transformação de pacote\"} para mais
info.")))

 (entry (commit "9ea37eb9f5329c213757bbfe5d9241cde8433858")
        (title
         (en "Linux-libre 6.0 removed due to end of upstream support")
         (de "Linux-libre 6.0 wurde entfernt")
         (pt "Linux-libre 6.0 removido em razão do fim do suporte do fornecedor"))
        (body
         (en "The linux-libre 6.0 kernel series has reached the end of
             its life, and no longer supported upstream.  For this
             reason, it has been removed from GNU Guix.")
         (de "Vom Kernel @code{linux-libre} wird die 6.0-Versionsreihe keine
Unterstützung von dessen Anbieter mehr erfahren („end of life“).  Daher ist es
aus GNU Guix entfernt worden.")
         (pt "A série 6.0 do kernel linux-libre alcançou o fim de sua vida, e
não é mais suportada pelo fornecedor.  Por esta razão, ela foi removida do GNU
Guix.")))

 (entry (commit "ce8a34bc9ab89f31f107383ba791954864aed372")
        (title
         (en "Linux-libre kernel updated to 6.1")
         (de "Linux-libre-Kernel wird auf 6.1 aktualisiert")
         (fr "Le noyau linux-libre est mis à jour vers la 6.1")
         (pt "Kernel linux-libre atualizado para 6.1"))
        (body
         (en "The default version of the linux-libre kernel has been updated to
              the 6.1 release series.")
         (de "Der standardmäßig verwendete @code{linux-libre}-Kernel basiert
              jetzt auf der 6.1-Versionsreihe.")
         (fr "La version par défaut du noyau linux-libre est mise à jour
              vers la série des 6.1.")
         (pt "A versão padrão do kernel linux-libre foi atualizada para a
              série do kernel 6.1.")))

 (entry (commit "064c5b7e450f9f6d55cfcd0ec2bc9e96ee0b2958")
        (title
         (en "Linux-libre 4.9 removed due to end of upstream support")
         (de "Linux-libre 4.9 wurde entfernt")
         (pt "Linux-libre 4.9 removido em razão do fim do suporte do fornecedor"))
        (body
         (en "The linux-libre 4.9 kernel series has reach the end of its life,
and is no longer supported upstream.  For this reason, it has been removed from
GNU Guix.")
         (de "Vom Kernel @code{linux-libre} wird die 4.9-Versionsreihe keine
Unterstützung von dessen Anbieter mehr erfahren („end of life“).  Daher ist es
aus GNU Guix entfernt worden.")
         (pt "A série 4.9 do kernel linux-libre alcançou o fim de sua vida, e
não é mais suportada pelo fornecedor.  Por esta razão, ela foi removida do GNU
Guix.")))

 (entry (commit "dfc6957a5af7d179d4618eb19d4f555c519bc6f2")
        (title
         (en "New @code{customize-linux} procedure")
         (de "Neue Prozedur @code{customize-linux}")
         (fr "Nouvelle procédure @code{customize-linux}")
         (pt "Novo procedimento @code{customize-linux}"))
        (body
         (en "The @code{(gnu packages linux)} module includes a new
@code{customize-linux} procedure, which should now be used instead of
replacing the @samp{\"kconfig\"} native input of a @code{linux-libre}-derived
package, as the kernel config file is no longer provided as a native
input.")
         (de "Das Modul @code{(gnu packages linux)} enthält eine neue Prozedur
@code{customize-linux}, die von nun an für angepasste Linux-Pakete benutzt
werden sollte.  Die native Eingabe @samp{\"kconfig\"} eines von
@code{linux-libre} abgeleiteten Pakets zu ersetzen, funktioniert nicht mehr,
weil die Kernel-Konfigurationsdatei nicht mehr als native Eingabe vorliegt.")
         (fr "Le module @code{(gnu packages linux)} inclut une nouvelle
procédure @code{customize-linux}, qui devrait maintenant être utilisée au lieu
de remplacer l'entrée native @samp{\"kconfig\"} d'un paquet dérivé de
@code{linux-libre}, car le fichier de configuration du noyau n'est plus fourni
en tant qu'entrée native.")
         (pt "O módulo @code{(gnu packages linux)} inclui um novo procedimento
@code{customize-linux}, que agora deve ser usada em vez de substituir a
entrada nativa (native input) @samp{\"kconfig\"} de um pacote derivado de
@code{linux-libre}, já que o arquivo de configuração do kernel não é mais
fornecido como entrada nativa.")))

 (entry (commit "788602b37ff42f730d4b7b569b0fb51465f147da")
        (title
         (en "New @option{--symlink} option for @command{guix shell}")
         (de "Neue Option @option{--symlink} für @command{guix shell}")
         (fr "Nouvelle option @option{--symlink} pour @command{guix shell}")
         (pt "Nova opção @option{--symlink} para @command{guix shell}"))
        (body
         (en "The @command{guix shell} command has a new
@option{--symlink} (or @option{-S}) option, to be used in conjunction with the
@option{--container} (or @option{-C}) option to create a symbolic link inside
the container.  Run @command{info \"(guix) Invoking guix shell\"} for more
information.")
         (de "Der Befehl @command{guix shell} verfügt jetzt über eine neue
Befehlszeilenoption @option{--symlink} (oder @option{-S}), die zusammen mit der
Option @option{--container} (oder @option{-C}) benutzt werden kann, um eine
symbolische Verknüpfung im Container anzulegen.  Führen Sie
@command{info \"(guix.de) Aufruf von guix shell\"} aus, um mehr zu erfahren.")
         (fr "La commande @command{guix shell} dispose d'une nouvelle option,
@option{--symlink} (ou @option{-S}), qui doit être utilisée en conjonction
avec l'option @option{--container} (ou @option{-C}) pour créer un lien
symbolique dans le conteneur.  Lancer @command{info \"(guix.fr) Invoquer guix
shell\"} pour plus d'informations.")
         (pt "O comando @command{guix shell} command tem uma nova opção
@option{--symlink} (ou @option{-S}), para ser usada em conjunção com a opção
@option{--container} (ou @option{-C}) para criar uma ligação simbólica dentro
do contênier.  Execute @command{info \"(guix.pt_BR) Invocando guix shell\"}
para mais informação.")))

 (entry (commit "82a0a395d7051eab7b9f15ec4740d58c86413604")
        (title
         (en "Linux-libre kernel updated to 6.0")
         (de "Linux-libre-Kernel wird auf 6.0 aktualisiert")
         (fr "Le noyau linux-libre est mis à jour vers la 6.0")
         (pt "Kernel linux-libre atualizado para 6.0"))
        (body
         (en "The default version of the linux-libre kernel has been
              updated to the 6.0 release series.")
         (de "Der standardmäßig verwendete @code{linux-libre}-Kernel basiert
              jetzt auf der 6.0-Versionsreihe.")
         (fr "La version par défaut du noyau linux-libre est mise à jour
              vers la série des 6.0.")
         (pt "A versão padrão do kernel linux-libre foi atualizada para a
              série do kernel 6.0.")))

 (entry (commit "400a7a4c80efbde1905ae98a298bbb5882d46a0d")
        (title
         (en "New build system for Python packages")
         (de "Neues Erstellungssystem für Python-Pakete")
         (fr "Nouveau système de construction pour les paquets Python")
         (pt "Novo sistema de compilação para pacotes Python"))
        (body
         (en "A new @var{pyproject-build-system} has been added.  This
is a redesign of @var{python-build-system} with support for @dfn{PEP 517}
and @file{pyproject.toml} files.  It also has built-in support for various
test frameworks such as @command{pytest} and @code{nosetests}.

There is a complementary @code{python-toolchain} package that comes with
updated versions of @command{pip}, @command{setuptools} and others.

The build system will eventually be merged into @var{python-build-system}
but you are encouraged to use it for packages in the @code{guix} channel.
Third party channels may want to wait until the API is stable (see the
Guix manual for caveats).

Despite the name, @var{pyproject-build-system} also works with the
``legacy'' @file{setup.py} format.")
         (de "Ein neues Erstellungssystem @var{pyproject-build-system} ist
verfügbar.  Es ist eine Neuauflage des @var{python-build-system}, die
@dfn{PEP 517} und @file{pyproject.toml}-Dateien unterstützt.  Auch wurde
Unterstützung für Testrahmen wie @command{pytest} und @code{nosetests}
eingebaut.

Ergänzend gibt es ein Paket @code{python-toolchain} mit aktualisierten Versionen
von @command{pip}, @command{setuptools} und mehr.

Das Erstellungssystem wird in Zukunft Teil von @var{python-build-system} werden,
aber wir würden es begrüßen, wenn Sie es für Pakete auf dem @code{guix}-Kanal
verwenden würden.  Drittanbieterkanäle warten vielleicht lieber auf eine
stabile Programmierschnittstelle (siehe die im Guix-Handbuch genannten
Einschränkungen).

Trotz dem Namen funktioniert @var{pyproject-build-system} auch mit dem „alten“
@file{setup.py}-Format.")
         (fr "Un nouveau système de construction, @var{pyproject-build-system},
a été ajouté.  Il s'agit d'une refonte du @var{python-build-system} qui rajoute
la prise en charge de @dfn{PEP 517} et des fichiers @file{pyproject.toml}.
Il intègre aussi la prise en charge de divers cadriciels de test comme
@command{pytest} ou @code{nosetests}.

Un paquet supplémentaire @code{python-toolchain} fournit des versions à jour
de @command{pip}, @command{setuptools} et autres.

Le système de construction finira par être intégré au @var{python-build-system}
mais nous vous encourageons à l'utiliser pour les paquets du canal @code{guix}.
Les canaux tiers devraient attendre que l'API se stabilise (voir le manuel de
Guix pour les mises en garde).

Contrairement à ce qu'indique son nom, @var{pyproject-build-system} fonctionne
aussi avec « l'ancien » format @file{setup.py}.")
         (pt "Um novo sistema de compilação chamado @var{pyproject-build-system}
foi adicionado.  É um redesign do @var{python-build-system} com suporte à
@dfn{PEP 517} e a arquivos @file{pyproject.toml}.  Ele também inclui suporte a
vários frameworks de teste tais como @command{pytest} e @code{nosetests}.

Há um pacote complementar @code{python-toolchain} que contém versões
atualizadas do @command{pip}, @command{setuptools} e outros.

O sistema de compilação será eventualmente incorporado ao
@var{python-build-system}, mas encorajamos você a usá-lo para pacotes no canal
@code{guix}.  Canais de terceiros podem querer esperar até a API se
estabilizar (veja o manual do Guix para ressalvas).

Apesar do nome, o @var{pyproject-build-system} também funciona com o formato
“legado” do @file{setup.py}.")))

 (entry (commit "c7ba5f38b80433b040d3946b8fc0b1e8621ba30a")
        (title
         (en "New @option{--emulate-fhs} option for @command{guix shell}")
         (de "Neue Option @option{--emulate-fhs} für @command{guix shell}")
         (fr "Nouvelle option @option{--emulate-fhs} pour @command{guix shell}")
         (pt "Nova opção @option{--emulate-fhs} para o @command{guix shell}"))
        (body
         (en "The @command{guix shell} command has a new
@option{--emulate-fhs} (or @option{-F}) option.  Combined with
@option{--container} (or @option{-C}), it emulates the file and directory
layout specified by the Filesystem Hierarchy Standard (FHS), providing
@file{/bin}, @file{/lib}, etc. within the container.

For example, the following command runs @file{/bin/ls} within such a
container:

@example
guix shell -CF coreutils -- /bin/ls
@end example

Run @command{info \"(guix) Invoking guix shell\"} for more information.")
         (de "Der Befehl @command{guix shell} verfügt jetzt über eine neue
Befehlszeilenoption @option{--emulate-fhs} (oder @option{-F}).  Zusammen mit
@option{--container} (oder @option{-C}) kann so die Datei- und
Verzeichnisstruktur, die im @i{Filesystem Hierarchy Standard} (FHS) vorgegeben
wird, nachgebildet werden. Das heißt, in der Container-Umgebung gibt es
@file{/bin}, @file{/lib} und so weiter.

Zum Beispiel wird folgender Befehl @file{/bin/ls} in einem solchen Container
ausführen:

@example
guix shell -CF coreutils -- /bin/ls
@end example

Führen Sie @command{info \"(guix.de) Aufruf von guix shell\"} aus, um mehr
zu erfahren.")
         (fr "La commande @command{guix shell} dispose d'une nouvelle option,
@option{--emulate-fhs} (ou @option{-F}).  Avec @option{--container} (ou
@option{-C}), elle permet d'imiter la disposition des fichiers et répertoires
spécifiée par le @i{Filesystem Hierarchy Standard} (FHS) en fournissant
@file{/bin}, @file{/lib}, etc. dans le conteneur.

Par exemple, la commande ci-dessous lance @file{/bin/ls} dans un tel
conteneur :

@example
guix shell -CF coreutils -- /bin/ls
@end example

Lancer @command{info \"(guix.fr) Invoquer guix shell\"} pour plus
d'informations.")
         (pt "O comando @command{guix shell} tem uma nova opção
@option{--emulate-fhs} (ou @option{-F}).  Combinada com
@option{--container} (ou @option{-C}), ela emula o layout de arquivos e
diretórios especificado pelo Padrão de Hierarquia do Sistema de
Arquivos (Filesystem Hierarchy Standard — FHS), provendo @file{/bin},
@file{/lib}, etc. dentro do contêiner.

Por exemplo, o comando seguinte executa @file{/bin/ls} dentro de um contêiner
desse tipo:

@example
guix shell -CF coreutils -- /bin/ls
@end example

Execute @command{info \"(guix.pt_BR) Invocando guix shell\"} para mais
informações.")))

 (entry (commit "28ade1bab207974cce6a014e7187968511fc5526")
        (title
         (en "@option{--with-source} is now recursive")
         (de "@option{--with-source} ist jetzt rekursiv")
         (fr "@option{--with-source} est dorénavant récursive")
         (pt "@option{--with-source} agora é recursiva"))
        (body
         (en "The @option{--with-source} package transformation option now
uses the specified source for all matching packages, including dependencies.
This option is useful for all package maintainers, developers, and, in
general, all users who want Guix to facilitate their rights to modify their
software and share their changes.

Run @command{info \"(guix) Package Transformation Options\"} for more
info.")
         (de "Die Paketumwandlungsoption @option{--with-source} wird jetzt den
angegebenen Quellcode für sämtliche passende Pakete benutzen, Abhängigkeiten
eingeschlossen. Die Option hilft Paketbetreuern, Entwicklern und allgemein allen
Nutzern, die Guix benutzen, das Recht, ihre Software anzupassen und
Änderungen zu teilen, leichter auszuüben.

Führen Sie für mehr Informationen @command{info \"(guix.de)
Paketumwandlungsoptionen\"} aus.")
         (fr "L'option de transformation de paquet @option{--with-source}
s'applique désormais à tous les paquets correspondant, y compris les
dépendances.  Cette option est utile pour les personnes qui maintiennent un
logiciel, en développent un ou, plus généralement, pour toute personne qui
souhaite que Guix facilite l'exercice de ses droits à modifier le logiciel et
à partager ses changements.

Lancer @command{info \"(guix.fr) Options de transformation de paquets\"} pour
plus d'informations.")
         (pt "A opção de transformação de pacote @option{--with-source} agora
usa a fonte especificada para todos os pacotes correspondentes, incluindo
dependências.  Essa opção é útil para todos os mantenedores de pacotes,
desenvolvedores e usuários em geral que querem que o Guix facilite seu direito
de modificar seu software e compartilhar suas mudanças.

Execute @command{info \"(guix.pt_BR) Opções de transformação de pacote\"} para
mais informações.")))

 (entry (commit "a13f5ead0265cf0fe11e60150547c09dfc8c45b0")
        (title
         (en "Guix System image creation is now documented")
         (de "Es gibt eine Dokumentation, wie Sie Guix-System-Abbilder („Images“) erzeugen")
         (fr "La création d'images pour Guix System est à présent documentée")
         (pt "A criação de imagens do Guix System agora está documentada"))
        (body
         (en "The Guix System image API that allows you to create customized
system images and turn them into actual bootable images is now documented in
the @code{Creating System Images} chapter of the Guix documentation.  This
should be particularly useful for people trying to port Guix System to new
hardware.")
         (de "Sie können nun im Kapitel @code{Creating System Images} der
Dokumentation nachlesen, wie Sie mit der Schnittstelle für Guix-System-Abbilder
anpassbare, bootfähige Systemabbilder erzeugen können.  Das sollte vor allem
Personen unterstützen, die versuchen, Guix System auf neuer Hardware zum Laufen
zu bringen.")
         (fr "L'interface de programmation d'images pour Guix System,
permettant de créer des images personnalisées et de les transformer en images
amorçables est désormais documentée dans le chapitre @code{Création d'images
système}.  Cette interface devrait être particulièrement utile aux personnes
qui essaient de faire fonctionner Guix sur de nouvelles machines.")
         (pt "A @acronym{API, Application Public Interface} de imagens do Guix
System que permite criar imagens de sistema customizadas e torná-las
inicializáveis agora está documentada no capítulo @code{Creating System
Images} da documentação do Guix.  Isso é particularmente útil para pessoas
tentando portar o Guix System para um novo hardware.")))

 (entry (commit "c8112f3bd95269ce4aca12dedbfe61bb6b37acae")
        (title
         (en "WSL system images support")
         (de "WSL-Systemabbilder werden unterstützt")
         (fr "Support pour les images système WSL")
         (pt "Suporte a imagens de sistema WSL"))
        (body
         (en "The @command{guix system image} command can now generate system
images for the Windows Subsystem for Linux.  To get started, you can for
instance run from a Guix checkout:

@command{guix system image gnu/system/images/wsl2.scm},

and import the resulting image this way:

@command{wsl --import Guix ./guix ./wsl2-image.tar.gz}
@command{wsl -d Guix}.")
         (de "Mit dem Befehl @command{guix system image} können Sie
Systemabbilder erzeugen, die auf dem Windows-Subsystem für Linux laufen.  Wenn
Sie das ausprobieren möchten, führen Sie zum Beispiel Folgendes aus einem
Guix-Checkout heraus aus:

@command{guix system image gnu/system/images/wsl2.scm},

Das resultierende Abbild können Sie so importieren:

@command{wsl --import Guix ./guix ./wsl2-image.tar.gz}
@command{wsl -d Guix}.")
         (fr "La commande @command{guix system image} peut désormais générer
des images système pour le Windows Subystem for Linux.  Vous pouvez par
exemple lancer la commande suivante depuis un répertoire de sources Guix :

@command{guix system image gnu/system/images/wsl2.scm},

et importer l'image obtenue de cette manière :

@command{wsl --import Guix ./guix ./wsl2-image.tar.gz}
@command{wsl -d Guix}.")
         (pt "Agora o comando @command{guix system image} pode gerar imagens
de sistema para o Subsistema do Windows para Linux.  Para começar, você pode
por exemplo rodar a partir de um checkout do repositório do Guix:

@command{guix system image gnu/system/images/wsl2.scm},

e importar a imagem resultante da seguinte maneira:

@command{wsl --import Guix ./guix ./wsl2-image.tar.gz}
@command{wsl -d Guix}.")))

 (entry (commit "11a06d1e49f4d50d6789e05bbf35e2e145ff7838")
        (title
         (en "Emacs now supports native compilation")
         (de "Emacs kann Pakete nun nativ kompilieren")
         (pt "O Emacs agora suporta compilação nativa"))
        (body
         (en "Emacs can now compile packages natively.  Under the default
configuration, this means that Emacs packages will now be just-in-time (JIT)
compiled as you use them, and the results stored in a subdirectory of your
@code{user-emacs-directory}.

Furthermore, the build system for Emacs packages transparently supports native
compilation, but note, that @code{emacs-minimal}---the default Emacs for
building packages---has been configured without native compilation.
To natively compile your emacs packages ahead of time, use a transformation
like @option{--with-input=emacs-minimal=emacs}.")
         (de "Emacs kann nun native Maschinenbefehle erzeugen.  Standardgemäß
kompiliert es nun Pakete „just in time“, während Sie diese laden, und platziert
die so erzeugten nativen Bibliotheken in einem Unterverzeichnis Ihres
@code{user-emacs-directory}.

Darüber hinaus unterstützt das Erstellungssystem für Emacs-Pakete die Erzeugung
nativer Maschinenbefehle.  Beachten Sie jedoch, dass @code{emacs-minimal} –
die Emacs-Variante, mit der normalerweise Emacs-Pakete erstellt werden –
weiterhin keine nativen Befehle generiert.  Um native Befehle für Ihre
Emacs-Pakete schon im Voraus zu erzeugen, nutzen Sie eine Transformation, z.B.
@option{--with-input=emacs-minimal=emacs}.")
         (pt "Agora o Emacs pode compilar pacotes nativamente.  Na
configuração padrão os pacotes do Emacs serão compilados “just-in-time” (JIT)
conforme forem usados, e os resultados armazenados em um subdiretório de
@code{user-emacs-directory}.

Além disso, o sistema de compilação para pacotes do Emacs suporta compilação
nativa de forma transparente.  Note porém que o @code{emacs-minimal} --- a
variante padrão do Emacs para compilar pacotes --- foi configurado sem
compilação nativa.  Para pré-compilar nativamente seus pacotes do Emacs use
uma transformação, como por exemplo
@code{--with-input=emacs-minimal=emacs}.")))

 (entry (commit "c188cf57f161c0c26e2d7c8516bd1ddd1492d686")
        (title
         (en "Linux-libre kernel updated to 5.19")
         (de "Linux-libre-Kernel wird auf 5.19 aktualisiert")
         (fr "Le noyau linux-libre est mis à jour vers la 5.19")
         (pt "Kernel linux-libre atualizado para 5.19"))
        (body
         (en "The default version of the linux-libre kernel has been
              updated to the 5.19 release series.")
         (de "Der standardmäßig verwendete @code{linux-libre}-Kernel basiert
              jetzt auf der 5.19-Versionsreihe.")
         (fr "La version par défaut du noyau linux-libre est mise à jour
              vers la série des 5.19.")
         (pt "A versão padrão do kernel linux-libre foi atualizada para a
              série do kernel 5.19.")))

 (entry (commit "a15542d26df42dabdb5e2f76d150ae200230c3b0")
        (title
         (en "New @option{--whole-file} option for @command{guix style}")
         (de "Neue Option @option{--whole-file} für @command{guix style}")
         (fr "Nouvelle option @option{--whole-file} pour @command{guix style}")
         (pt "Nova opção @option{--whole-file} para @command{guix style}"))
        (body
         (en "The @command{guix style} command has a new @option{--whole-file}
option: instead of formatting individual package definitions, this option lets
you reformat entire Scheme files.  You might want to use it to format your
operating system configuration file, for instance.

Run @command{info \"(guix) Invoking guix style\"} for more info.")
         (de "Der Befehl @command{guix style} verfügt über eine neue
Befehlszeilenoption @option{--whole-file}: Mit ihr werden keine einzelnen
Paketdefinitionen umformatiert, sondern ganze Scheme-Dateien werden in die
richtige Darstellungsform gebracht.  Sie können damit zum Beispiel die
Konfigurationsdatei für Ihr Betriebssystem formatieren lassen.

Führen Sie @command{info \"(guix.de) Aufruf von guix style\"} aus, um mehr
Informationen zu erhalten.")
         (fr "La commande @command{guix style} a désormais une nouvelle option
@option{--whole-file} : au lieu de mettre en forme des définitions de paquets,
cette option permet de mettre en forme des fichiers Scheme entiers.  Ça peut
s'avérer utile par exemple pour mettre en forme son fichier de configuration
du système d'exploitation.

Lancer @command{info \"(guix.fr) Invoquer guix style\"} pour plus
d'informations.")
         (pt "O comando @command{guix style} tem uma nova opção
@option{--whole-file}: ao invés de formatar definições de pacote
individualmente, esta opção permite a reformatação de arquivos Scheme por
inteiro.  Você pode querer usá-la para formatar seu arquivo de configuração do
sistema operacional, por exemplo.

Execute @command{info \"(guix.pt_BR) Invocando guix style\"} para mais
informações.")))

 (entry (commit "2ec7ab2610eb67e26dab52b671eb29e46f64ea0f")
        (title
         (en "Linux-libre kernel updated to 5.18")
         (de "Linux-libre-Kernel wird auf 5.18 aktualisiert")
         (fr "Le noyau linux-libre est mis à jour vers la 5.18")
         (pt "Kernel linux-libre atualizado para to 5.18"))
        (body
         (en "The default version of the linux-libre kernel has been
              updated to the 5.18 release series.")
         (de "Der standardmäßig verwendete @code{linux-libre}-Kernel basiert
jetzt auf der 5.18-Versionsreihe.")
         (fr "La version par défaut du noyau linux-libre est mise à jour
              vers la série des 5.18.")
         (pt "A versão padrão do kernel linux-libre foi atualizada para
a série de lançamentos 5.18.")))

 (entry (commit "bdf422176739b473add66eb8cac9fdd8c654f794")
        (title
         (en "@option{-L} option of @command{guix refresh} repurposed")
         (de "Option @option{-L} von @command{guix refresh} wechselt Bedeutung")
         (fr "Option @option{-L} de @command{guix refresh} réutilisée")
         (pt "A opção @option{-L} de @command{guix refresh} retrabalhada"))
        (body
         (en "The @option{-L} option of @command{guix refresh} has been
repurposed: it used to be synonymous with @option{--list-updaters}; it is now
synonymous with @option{--load-path} as is the case with most other commands.

Run @command{info \"(guix) Invoking guix refresh\"} for more info.")
         (de "Die Befehlszeilenoption @option{-L} von @command{guix refresh} hat
einen anderen Zweck bekommen: Früher war sie gleichbedeutend mit
@option{--list-updaters}; jetzt ist sie gleichbedeutend mit
@option{--load-path}, wie bereits beim Großteil der anderen Befehle.

Führen Sie @command{info \"(guix.de) Aufruf von guix refresh\"} aus, wenn Sie
mehr wissen möchten.")
         (fr "L'option @option{-L} de @command{guix refresh} a changé de
signification : elle était auparavant synonyme de @option{--list-updaters} ;
elle est maintenant synonyme de @option{--load-path} comme c'est le cas pour
la plupart des autres commandes.

Lancer @command{info \"(guix.fr) Invoquer guix refresh\"} pour plus
d'informations.")
         (pt "A opção @option{-L} de @command{guix refresh} foi
retrabalhada: ela costumava ser sinônimo de @option{--list-updaters};
agora ela é sinônimo de @option{--load-path}, como é o caso com a
maioria dos outroscomandos.

Execute @command{info \"(guix.pt_BR) Invocando guix refresh\"} para mais
info.")))

 (entry (commit "35c1edb20ad07250728d3bdcd0296bd0cedaf6bb")
        (title
         (en "New @command{edit} sub-commands for services")
         (de "Neue @command{edit}-Unterbefehle für Dienste")
         (fr "Nouvelles commandes @command{edit} pour les services")
         (nl "Nieuwe deelopdracht @command{edit} voor diensten")
         (pt "Novo subcomando @command{edit} para serviços"))
        (body
         (en "The new @command{guix system edit} and @command{guix home edit} commands
allow you to view or edit service types defined for Guix System or Guix Home.
For example, here is how you would open the definition of the OpenSSH system
service:

@example
guix system edit openssh
@end example

Run @command{info \"(guix) Invoking guix system\"} or @command{info \"(guix)
Invoking guix home\"} for more info.")
         (de "Mit den neuen Befehlen @command{guix system edit} und
@command{guix home edit} können Sie Diensttypen für Guix System oder Guix Home
betrachten und bearbeiten.  Zum Beispiel würden Sie die Definition des
OpenSSH-Systemdienstes wie folgt öffnen:

@example
guix system edit openssh
@end example

Führen Sie @command{info \"(guix.de) Aufruf von guix system\"} oder
@command{info \"(guix.de) Aufruf von guix home\"} aus, um mehr zu erfahren.")
         (fr "Les nouvelles commandes @command{guix system edit} et
@command{guix home edit} permettent de visualiser ou d'éditer les types de
services définis pour Guix System ou Guix Home.  Par exemple, voici comment
ouvrir la définition du service système OpenSSH :

@example
guix system edit openssh
@end example

Lancer @command{info \"(guix.fr) Invoquer guix system\"} ou @command{info
\"(guix.fr) Invoquer guix home\"} pour plus d'informations.")
         ;; TODO: pas verwijzingen naar de handleiding aan wanneer ze vertaald is
         (nl "Met de nieuwe bewerkingen @command{guix system edit} en
@command{guix home edit} kan je dienstsoorten van Guix System en Guix
Home bekijken en bewerken.  Je kan bijvoorbeeld de definitie van de
systeemdienst OpenSSH als volgt openen:

@example
guix system edit openssh
@end example

Voer @command{info \"(guix) Invoking guix system\"} of @command{info
\"(guix)Invoking guix home\"} uit voor meer informatie.")
         (pt "Os comandos @command{guix system edit} e @command{guix
home edit} te permitem ver ou editar tipos de serviço definidos para o
Sistema Guix ou Guix Home.  Por exemplo, eis como alguém abriria a
definição do serviço de sistema OpenSSH:

@example
guix system edit openssh
@end example

Execute @command{info \"(guix.pt_BR) Invocando guix system\"} ou @command{info
\"(guix.pt_BR) Invocando guix home\"} para mais info.")))

 (entry (commit "903c82583e1cec4c9ff09d5895c5cc646c37b661")
        (title
         (en "New @command{guix import elm} command")
         (de "Neuer Befehl @command{guix import elm}")
         (fr "Nouvelle commande @command{guix import elm}")
         (pt "Novo comando @command{guix import elm}"))
        (body
         (en "The new @command{guix import elm} command allows packagers to
generate a package definition or given the name of a package for Elm, a
functional programming language for the Web:

@example
guix import elm elm/bytes
@end example

Run @command{info \"(guix) Invoking guix import\"} for more info.

This comes with a new build system for Elm packages---run @command{info
\"(guix) Build Systems\"} for details.")
         (de "Mit dem neuen Befehl @command{guix import elm} können Paketautoren
eine Paketdefinition anhand des Namens eines Pakets für Elm, einer funktionalen
Programmiersprache für das Web, erzeugen:

@example
guix import elm elm/bytes
@end example

Führen Sie @command{info \"(guix.de) Aufruf von guix import\"} aus, um mehr
Informationen zu bekommen.

Dazu kommt ein neues Erstellungssystem für Elm-Pakete.  Führen Sie
@command{info \"(guix.de) Erstellungssysteme\"} aus, um mehr zu erfahren.")
         (fr "La nouvelle commande @command{guix import elm} permet de générer
une définition de paquet reposant sur Elm, un langage de programmation
fonctionnelle pour le Web:

@example
guix import elm elm/bytes
@end example

Lancer @command{info \"(guix.fr) Invoquer guix import\"} pour plus
d'informations.

Cela vient avec un nouveau système de construction pour paquets Elm---lancer
@command{info \"(guix.fr) Systèmes de construction\"} pour plus de
détails.")
         (pt "O novo comando new @command{guix import elm} permite
empacotadores gerar uma definição de pacote para um dado nome de pacote
para Elm, uma linguagem de programação funcional para a Web:

@example
guix import elm elm/bytes
@end example

Execute @command{info \"(guix.pt_BR) Invocando guix import\"} para mais info.

Isto vem com um novo sistema de construção para pacotes Elm --- execute
@command{info \"(guix) Build Systems\"} para detalhes.")))

 (entry (commit "b6b2de2a0d52530bc1ee128c61580bed662ee15c")
        (title (en "Linux-libre kernel updated to 5.17")
               (de "Linux-libre-Kernel wird auf 5.17 aktualisiert")
               (fr "Le noyau linux-libre est mis à jour vers la 5.17")
               (pt "Kernel linux-libre atualizado para 5.17"))
        (body
         (en "The default version of the linux-libre kernel has been
              updated to the 5.17 release series.")
         (de "Der standardmäßig verwendete @code{linux-libre}-Kernel basiert
jetzt auf der 5.17-Versionsreihe.")
         (fr "La version par défaut du noyau linux-libre est mise à jour
              vers la série des 5.17.")
         (pt "A versão padrão do kernel linux-libre foi atualizada para a
série de lançamentos 5.17.")))

 (entry (commit "c42b7baf13c7633b4512e94da7445299c57b247d")
        (title
         (en "New @option{--export-manifest} option for @command{guix shell}")
         (de "Neue Option @option{--export-manifest} für @command{guix shell}")
         (fr "Nouvelle option @option{--export-manifest} de @command{guix shell}")
         (pt "Nova opção @option{--export-manifest} para @command{guix shell}"))
        (body
         (en "If you use @command{guix shell}, you might wonder how to
``translate'' a command line into a manifest file that you can keep under
version control, share with others, and pass to @command{guix shell -m} and in
fact to most @command{guix} commands.  This is what the new
@option{--export-manifest} option does.

For example, the command below prints a manifest for the given packages:

@lisp
guix shell --export-manifest \\
   -D guile git emacs emacs-geiser emacs-geiser-guile
@end lisp

Run @code{info \"(guix) Invoking guix shell\"} for more information.")
         (de "Wenn Sie @command{guix shell} benutzen, haben Sie sich vielleicht
einmal gefragt, wie Sie eine Befehlszeile in eine Manifest-Datei „übersetzen“
können, die Sie unter Versionskontrolle stellen können, mit anderen teilen
können und an @command{guix shell -m} oder tatsächlich die meisten anderen
@command{guix}-Befehle übergeben können.  Die Antwort ist die neue
Befehlszeilenoption @option{--export-manifest}.

Zum Beispiel gibt der folgende Befehl ein Manifest mit den genannten Paketen
aus:

@lisp
guix shell --export-manifest \\
   -D guile git emacs emacs-geiser emacs-geiser-guile
@end lisp

Führen Sie @command{info \"(guix.de) Aufruf von guix shell\"} aus, um mehr
zu erfahren.")
         (fr "Si tu utilises @command{guix shell}, tu t'es peut-être déjà
demandé comment « traduire » une ligne de commande en un fichier manifeste que
tu puisse garder en gestion de version, partager et passer à @command{guix
shell -m} et autres commandes @command{guix}.  C'est ce que la nouvelle option
@option{--export-manifest} fait.

Par exemple, la commande ci-dessous affiche un manifeste pour les paquets
donnés :

@lisp
guix shell --export-manifest \\
   -D guile git emacs emacs-geiser emacs-geiser-guile
@end lisp

Lancer @code{info \"(guix.fr) Invoquer guix shell\"} pour plus
d'informations.")
         (pt "Se você usa @command{guix shell}, você pode se indagar como
``traduzir'' uma linha de comando em um arquivo manifesto que você pode manter
sob controle de versão, compartilhar com outras pessoaso, e passar para
@command{guix shell -m} e de fato para a maioria dos comandos @command{guix}.
É isto que a nova opção @option{--export-manifest} faz.

Por exemplo, o comando abaixo imprime um manifesto para os pacotes dados:

@lisp
guix shell --export-manifest \\
   -D guile git emacs emacs-geiser emacs-geiser-guile
@end lisp

Execute @code{info \"(guix.pt_BR) Invocando guix shell\"} para mais
informação.")))

 (entry (commit "094a2cfbe45c104d0da30ff9d975d052ca0c118c")
        (title
         (en "New @command{guix home container} command")
         (de "Neuer Befehl @command{guix home container}")
         (fr "Nouvelle commande @command{guix home container}")
         (pt "Novo comando @command{guix home container}"))
        (body
         (en "The new @command{guix home} tool, which lets you to manage
entire \"home environments\" in a declarative fashion, has gained a
@command{container} sub-command.  The new @command{guix home container}
command allows you to test your configuration in an isolated @dfn{container},
without touching your home directory:

@example
guix home container config.scm
@end example

This provides a simple and safe way to test your configuration before
deploying it with @command{guix home reconfigure}.  Run @code{info \"(guix)
Invoking guix home\"} for more information.")
         (de "Das neue Werkzeug @command{guix home}, womit Sie vollständige
„Persönliche Umgebungen“ deklarativ verwalten können, hat einen neuen
Unterbefehl @command{container} hinzubekommen.  Mit dem neuen Befehl
@command{guix home container} können Sie Ihre Konfiguration in einem isolierten
@dfn{Container} ausprobieren, ohne Ihr Persönliches Verzeichnis anzutasten.

@example
guix home container config.scm
@end example

So ist es ein Leichtes, Ihre Konfiguration in einer sicheren Umgebung zu testen,
bevor Sie mit @command{guix home reconfigure} auf sie umsteigen.  Führen Sie
@code{info \"(guix.de) Aufruf von guix home\"} aus, um mehr zu erfahren.")
         (fr "La nouvelle commande @command{guix home}, qui sert à gérer son
« environnement d'accueil » de manière déclarative, dispose maintenant d'une
sous-commande @command{container}.  La nouvelle commande @command{guix home
container} permet de tester sa configuration dans un @dfn{conteneur} isolé,
sans toucher à son répertoire d'accueil :

@example
guix home container config.scm
@end example

C'est un moyen simple et sûr de tester sa configuration avant de la déployer
avec @command{guix home reconfigure}.  Lancer @code{info \"(guix.fr) Invoquer
guix home\"} pour plus d'informations.")
         (pt "A nova ferramenta @command{guix home}, que te permite gerenciar
\"ambientes de usuário\" (@i{home}) inteiros em um estilo declarativo, ganhou
um subcomando @command{container}.  O novo comando @command{guix home
container} te permite testar sua configuração em um @dfn{contêiner} isolado,
sem tocar teu diretório de usuário:

@example
guix home container config.scm
@end example

Isto fornece uma maneira simples e segura de testar sua configuração antes de
implantá-la com @command{guix home reconfigure}.

Execute @code{info \"(guix.pt_BR) Invocando guix home\"} para mais
informação.")))

 (entry (commit "f1d18adbed39a3bacae93be29346fd4c86b480ef")
        (title
         (en "More compact @samp{guix pull --news}")
         (de "@samp{guix pull --news} wird knapper")
         (nl "Meer beknopte @samp{guix pull --news}")
         (pt "@samp{guix pull --news} mais compacto"))
        (body
          (en "The output of @samp{guix pull --news} has been shortened to
display only fresh news items such as this one.  It no longer includes the
partial selection of new and updated packages, which was often long enough to
be distracting whilst being too short to be useful.

The complete list of new and updated packages can now be obtained separately
using @samp{guix pull --news --details}.")
         (de "Die Ausgabe von @samp{guix pull --news} wurde gekürzt
und informiert nur mehr über ungezeigte Neuigkeiten wie diese. Es
fehlt der unvollständige Bericht über neue und aktualisierte Pakete,
der oft so lang war, dass er gestört hat, doch zu kurz war, um
nützlich zu sein.

Die vollständige Liste neuer und aktualisierter Pakete bekommen Sie
jetzt mit @samp{guix pull --news --details}.")
         (nl "De uitvoer van @samp{guix pull --news} is vanaf nu beperkt tot
verse nieuwsberichten zoals dit, zonder de onvolledige bloemlezing van nieuwe
en bijgewerkte pakketten.  Die was vaak lang genoeg om de lezer af te leiden
maar te kort om nuttig te zijn.

De volledige list van nieuwe en bijgewerkte pakketten is nu afzonderlijk
beschikbaar met @samp{guix pull --news --details}.")
         (pt "A saída de @samp{guix pull --news} foi encurtada para exibir
somente os itens de notícias frescas como este.  Ela não mais inclui a seleção
parcial de pacotes novos e atualizados, o que geralmente era longa o bastante
para ser uma distração ao mesmo tempo que era curta demais para ser útil.

A lista completa de novidades e atualizações agora pode ser obtida
separadamente usando @samp{guix pull --news --details}.")))

 (entry (commit "96d7535b030c65b2d8cb0bea52c4bd96cbdefaf0")
        (title
         (en "ci.guix.gnu.org to stop offering Gzip substitutes")
         (de "ci.guix.gnu.org wird keine Substitute mit Gzip mehr anbieten")
         (pt "ci.guix.gnu.org interrompe o fornecimento de substitutos Gzip"))
        (body
         (en "This is a notice to let you know that starting next
month (2022/03/01), Gzip-compressed substitutes will no longer be available,
which means that Guix daemons from a revision older than commit
@samp{3092f1b835d79655eecb2f8a79dda20ad9ba6bd6} (2019/06/02) will loose the
ability to download binary substitutes.  Starting next month, only lzip and
zstd substitutes will be offered.  Dropping Gzip substitutes will free about
6.5 TiB of storage space from the build farm.")
         (de "Hiermit weisen wir Sie darauf hin, dass ab nächstem
Monat (2022/03/01) keine Gzip-komprimierten Substitute mehr zur Verfügung
stehen.  Dadurch können Guix-Daemons, deren Version älter ist als Commit
@samp{3092f1b835d79655eecb2f8a79dda20ad9ba6bd6} (2019/06/02), keine binären
Substitute mehr beziehen.  Ab kommendem Monat werden nur Substitute mit lzip
und zstd angeboten.  Indem wir auf Gzip-Substitute verzichten, sparen wir 6.5
TiB Speicherplatz auf der Erstellungsfarm.")
         (pt "Esta é uma notícia para que você saiba que a partir do próximo
mês (2022/03/01), substitutos comprimidos com Gzip não mais estarão
disponíveis, o que significa que daemons Guix de uma revisão anterior ao
commit @samp{3092f1b835d79655eecb2f8a79dda20ad9ba6bd6} (2019/06/02) perderão a
capacidade de baixar substitutos binários.  A partir do próximo mês, serão
fornecidos somente substitutos lzip e zstd. A remoção de substitutos Gzip
liberará cerca de 6.5 TiB de espaço de armazenamento das fazendas de
construção.")))

 (entry (commit "5c13484646069064c834bbd3cd02c3bc80d94cb6")
        (title
         (en "New @option{--execute} option to @command{guix deploy}")
         (de "Neue Option @option{--execute} für @command{guix deploy}")
         (fr "Nouvelle option @option{--execute} pour @command{guix deploy}")
         (pt "Nova opção @option{--execute} para @command{guix deploy}"))
        (body
         (en "The @command{guix deploy} command has a new @option{--execute}
or @option{-x} option, which allows you to execute a command on all the
machines that your configuration file specifies, as in this example:

@example
guix deploy deploy.scm -x -- herd restart guix-daemon
@end example

This is no substitute for full-featured tools such as pdsh but it is a useful
helper.")
         (de "Der Befehl @command{guix deploy} verfügt über eine neue Option
@option{--execute} oder @option{-x}, mit der Sie einen Befehl auf allen in der
Konfigurationsdatei angegebenen Maschinen ausführen können.  Zum Beispiel:

@example
guix deploy deploy.scm -x -- herd restart guix-daemon
@end example

Dies ist kein Ersatz für vollumfängliche Werkzeuge wie pdsh, aber es kann doch
von Nutzen sein.")
         (fr "La commande @command{guix deploy} a une nouvelle option
@option{--execute} ou @option{-x} qui permet d'exécuter une commande sur
toutes les machines spécifiées dans son fichier de configuration, comme dans
cet exemple :

@example
guix deploy deploy.scm -x -- herd restart guix-daemon
@end example

Ça ne remplace pas les outils sophistiqués comme pdsh mais c'est bien
pratique.")
         (pt "O comando @command{guix deploy} tem uma nova opção
@option{--execute} (ou @option{-x}), que permite executar um comando em todas
as máquinas que seu arquivo de configuração especificar, como no exemplo:

@example
guix deploy deploy.scm -x -- herd restart guix-daemon
@end example

Este não é um substituto para ferramentas completas como @command{pdsh}, mas é
um ajudante útil.")))

 (entry (commit "c4fe13c294cc1e31dd8a49ce3981f603fb169e0a")
        (title
         (en "@command{guix style} can format package definitions")
         (de "@command{guix style} kann Paketdefinitionen formatieren")
         (fr "@command{guix style} peut mettre en forme les définitions de paquets")
         (pt "@command{guix style} pode formatar definições de pacotes"))
        (body
         (en "The recently-introduced @command{guix style} command can now be
used to automatically format package definitions according to the Guix
project's formatting guidelines.  If you contribute packages to Guix or to a
third-party channel, you may find it useful.

The new @option{--styling} option can currently be passed one of the following
@dfn{styling rules}: @code{format}, to format package definitions, or
@code{inputs}, to remove labels from package inputs.  Omitting
@option{--styling} is equivalent to passing @samp{--styling=format};
previously it was equivalent to @samp{--styling=inputs}.

Run @code{info \"(guix) Invoking guix style\"}, for more info.")
         (de "Der kürzlich eingeführte Befehl @command{guix style} kann jetzt
benutzt werden, um Paketdefinitionen automatisch nach den
Formatierungsrichtlinien des Guix-Projekts zu formatieren. Wenn Sie Pakete zu
Guix oder zu einem Drittanbieterkanal beitragen, könnte Ihnen das helfen.

Für die neue Befehlszeilenoption @option{--styling} können Sie derzeit eine
der folgenden @dfn{Stilregeln} angeben: @code{format}, wodurch
Paketdefinitionen formatiert werden, oder @code{inputs}, wodurch die
Bezeichnungen aus Paketeingaben entfernt werden. Wenn Sie {--styling}
weglassen, passiert das Gleiche wie wenn Sie @samp{--styling=format} angeben;
früher war es das Gleiche wie @samp{--styling=inputs}.

Führen Sie @command{info \"(guix.de) Aufruf von guix style\"} aus, um mehr
Informationen zu erhalten.")
         (fr "La commande @command{guix style}, récemment introduite, peut
désormais être utilisée pour mettre en forme des définitions de paquets
suivant les règles de style du projet Guix.  Si vous contribuez des paquets à
Guix ou à un canal tiers, cela peut vous être utile.

La nouvelle option @option{--style} peut pour le moment recevoir une des deux
@dfn{règles de style} suivantes : @code{format}, pour mettre en forme les
définitions de paquet, ou @code{inputs}, pour retirer les étiquettes des
champs @code{inputs} des paquets.  Omettre @option{--styling} revient à passer
@samp{--styling=format} ; auparavant c'était équivalent à
@samp{--styling=inputs}.

Lancer @command{info \"(guix.fr) Invoquer guix style\"}, pour plus
d'informations.")
         (pr "O comando recém-introduzido @command{guix style} agora pode ser
utilizado para formatar definições de pacotes automaticamente de acordo com as
guias de formatação do projeto Guix.  Se você contribui com pacotes para o
Guix ou algum canal de terceiros, você pode achar este comando útil.

À nova opção @option{--styling} atualmente pode ser pasada uma das seguintes
@dfn{regras de estilo}: @code{format}, para formatar definições de pacote, ou
@code{inputs}, para remover rótulos das entradas do pacote.  Omitir a opção
@option{--styling} é equivalente a passar @samp{--styling=format};
anteriormente era equivalente a passar @samp{--styling=inputs}.

Execute @code{info \"(guix.pt_BR) Invocando guix style\"} para mais info.")))

 (entry (commit "d090e9c37d693f5a0f381482c17fb03462cb6a48")
        (title
         (en "New @option{--tune} option for CPU micro-architecture tuning")
         (de "Neue Option @option{--tune} ermöglicht mikroarchitekturspezifische Optimierungen")
         (fr "Nouvelle option @option{--tune} pour optimiser pour une
micro-architecture")
         (pt "Nova opção @option{--tune} para ajuste de microarquitetura de CPU"))
        (body
         (en "The new @option{--tune} package transformation option instructs
Guix to tune relevant packages for the micro-architecture of the host CPU.
This lets the compiler use single-instruction/multiple-data (SIMD)
instructions beyond the baseline instruction set architecture (ISA), which can
noticeably improve performance in some cases such as linear algebra code.

As an example, here is how you would install the GNU Astronomy Utilities
against an optimized variant of the GNU Scientific Library (GSL):

@example
guix install gnuastro --tune
@end example

Run @command{info \"(guix) Package Transformation Options\"} for more
information.")
         (de "Die neue Paketumwandlungsoption @option{--tune} lässt Guix die
betroffenen Pakete an die im Prozessor dieses Rechners benutzte
Mikroarchitektur anpassen.  Dadurch kann der Compiler Befehle für
Single-Instruction/Multiple-Data (SIMD) einsetzen, die über den gemeinsamen
Befehlssatz hinausgehen. Das kann in manchen Fällen die Leistung beträchtlich
steigern, etwa für Berechnungen der linearen Algebra.

Zum Beispiel würden Sie so die GNU-Astronomieprogramme unter Nutzung einer
optimierten Variante der GNU Scientific Library (GSL) installieren:

@example
guix install gnuastro --tune
@end example

Führen Sie für mehr Informationen @command{info \"(guix.de)
Paketumwandlungsoptionen\"} aus.")
         (fr "La nouvelle option de transformation de paquets @option{--tune}
demande à Guix d'optimiser les paquets pour lesquels c'est pertinent pour la
micro-architecture du processeur hôte.  Cela permet au compilateur d'utiliser
des instructions vectorielles (SIMD) en plus des instructions de base de
l'architecture, ce qui peut sensiblement améliorer les performance dans
certains cas tels que pour du code d'algèbre linéaire.

Par exemple, voici comment installer les Utilitaires d'astronomie GNU de
manière à ce qu'ils utilisent une variante optimisée de la Bibliothèque
scientifique GNU (GSL) :

@example
guix install gnuastro --tune
@end example

Lancer @command{info \"(guix.fr) Options de transformation de paquets\"} pour
plus d'informations.")
         (pt "A nova opção de transformação de pacote @option{--tune} instrui
Guix a ajustar os pacotes relevantes para a microarquitetura da CPU anfitriã.
Isto leva o compilador a usar instruções do tipo \"instrução simples para
dados múltiplos\" (@acronym{SIMD, Single Instruction Multiple Data}) além da
linha basal do conjunto de instruções da arquitetura (@acronym{ISA,
Instruction Set Architecture}), o que pode melhorar sensivelmente o desempenho
em alguns casos como código de álgebra linear.

Como um exemplo, eis como você instalaria as Utilidades de Astronomia GNU
contra uma variante otimizada da Biblioteca Científica GNU:

@example
guix install gnuastro --tune
@end example

Execute @command{info \"(guix.pt_BR) Opções de transformação de pacote\"} para
mais informação.")))

 (entry (commit "ea2fd313d52dc62593b478acf5c3e7ea052c45de")
        (title
         (en "@samp{integer expected from stream}?  Update your Guix daemon")
         (de "@samp{integer expected from stream}? Aktualisieren Sie Ihren Guix-Daemon")
         (nl "@samp{integer expected from stream}?  Werk je Guix daemon bij")
         (pt "@samp{integer expected from stream}?  Atualiza teu daemon Guix"))
        (body
         (en "We recently fixed a bug where substitution would fail with
@samp{error: integer expected from stream}.  Be sure to update your system's
Guix package that provides the @command{guix-daemon} if you haven't done so
recently.  Run @command{info \"(guix) Upgrading Guix\"} for instructions.")
         (de "Unlängst haben wir einen Fehler behoben, dass die Substitution
mit der Fehlermeldung @samp{error: integer expected from stream}
fehlschlägt. Bitte aktualisieren Sie daher Ihr systemweites Guix-Paket, das
@command{guix-daemon} zur Verfügung stellt, wenn es noch nicht geschehen
ist. Führen Sie @command{info \"(guix.de) Aktualisieren von Guix\"} aus für
genaue Anweisungen.")
         (nl "Onlangs herstelden we een fout waarbij substitutie mislukt met
een @samp{error: integer expected from stream}.  Werk zeker je systeemwijde
Guix-pakket bij, dat de @command{guix-daemon} levert, als dat nog niet is
gebeurd.  Voer @command{info \"(guix) Upgrading Guix\"} uit voor meer uitleg.")
         (pt "Recentemente consertamos uma falha onde uma substituição
falharia com @samp{error: integer expected from stream}.  Certifique-se de
atualizar o pacote Guix do seu sistema que fornece o @command{guix-daemon} se
você ainda não o fez recentemente.  Execute @command{info \"(guix.pt_BR)
Atualizando o Guix\"} para instruções.")))

 (entry (commit "223f1b1eb3707f1d3ef91200dd616ee6c8b77db0")
        (title
         (en "Improved static networking support on Guix System")
         (de "Bessere Unterstützung für statische Netzwerkanbindungen auf Guix System")
         (fr "Meilleure prise en charge des réseaux statiques sur Guix System")
         (pt "Suporte a rede estática no Sistema Guix"))
        (body
         (en "Support for declarative static networking setup on Guix System
has been improved.  It now allows you to list IPv4 and IPv6 addresses in
routes in a flexible way, similar to what you would do with the @command{ip}
command, but in a declarative fashion, as in this example:

@lisp
;; Static networking for one NIC, IPv4-only.
(service static-networking-service-type
         (list (static-networking
                (addresses
                 (list (network-address
                        (device \"eno1\")
                        (value \"10.0.2.15/24\"))))
                (routes
                 (list (network-route
                        (destination \"default\")
                        (gateway \"10.0.2.2\"))))
                (name-servers '(\"10.0.2.3\")))))
@end lisp

The @code{static-networking-service} procedure remains available but is
deprecated.  Run @command{info \"(guix) Networking Setup\"} for more
information.")
         (de "Die deklarative Konfiguration für statische Netzwerkanbindungen
auf Guix System wurde verbessert.  Sie können jetzt die IPv4- und
IPv6-Adressen in Routen flexibel auflisten, ähnlich wie Sie es mit dem
@command{ip}-Befehl tun würden, aber auf deklarative Weise wie in diesem
Beispiel:

@lisp
;; Statische Netzwerkkonfiguration mit einer Netzwerkkarte, nur IPv4.
(service static-networking-service-type
         (list (static-networking
                (addresses
                 (list (network-address
                        (device \"eno1\")
                        (value \"10.0.2.15/24\"))))
                (routes
                 (list (network-route
                        (destination \"default\")
                        (gateway \"10.0.2.2\"))))
                (name-servers '(\"10.0.2.3\")))))
@end lisp

Die Prozedur @code{static-networking-service} gibt es noch, aber sie gilt als
veraltet.  Führen Sie @command{info \"(guix) Networking Setup\"} aus für
weitere Informationen.")
         (fr "La configuration déclarative et statique du réseau est mieux
prise en charge sur Guix System.  Il est maintenant possible d'énumérer des
adresses IPv6 et IPv4 et les chemins avec plus de flexibilité, un peu comme ce
qu'on peut faire avec la commande @command{ip} mais de manière déclarative,
comme dans cet exemple :

@lisp
;; Réseau statique à une seule interface, IPv4 seulement.
(service static-networking-service-type
         (list (static-networking
                (addresses
                 (list (network-address
                        (device \"eno1\")
                        (value \"10.0.2.15/24\"))))
                (routes
                 (list (network-route
                        (destination \"default\")
                        (gateway \"10.0.2.2\"))))
                (name-servers '(\"10.0.2.3\")))))
@end lisp

La procédure @code{static-networking-service} reste disponible mais elle est
obsolète.  Lancer @command{info \"(guix) Networking Setup\"} pour plus
d'informations.")
         (pt "Suporte à configuração declarativa de rede estática no Sistema
Guix foi aperfeiçoada.  Agora ela permite listas endereços IPv4 e IPv6 em
rotas de maneira flexível, semelhante ao que você fatia com o comando
@command{ip}, mas em um estilo declarativo, como neste exemplo:

@lisp
;; Rede estática para um NIC, IPv4-apenas.
(service static-networking-service-type
         (list (static-networking
                (addresses
                 (list (network-address
                        (device \"eno1\")
                        (value \"10.0.2.15/24\"))))
                (routes
                 (list (network-route
                        (destination \"default\")
                        (gateway \"10.0.2.2\"))))
                (name-servers '(\"10.0.2.3\")))))
@end lisp

O procedimento @code{static-networking-service} permanece disponível mas está
depreciado.  Execute @command{info \"(guix) Networking Setup\"} para mais
informação.")))

 (entry (commit "52cb5cf5b852117b5151a67af187d80764849ad3")
        (title
         (en "Icedove 91: profile folder moved to @file{~/.thunderbird}")
         (de "Icedove 91: Profilordner jetzt unter @file{~/.thunderbird}")
         (pt "Icedove 91: diretório de perfil movido para @file{~/.thunderbird}"))
        (body
         (en "Icedove 91 expects your profile folder under @file{~/.thunderbird}.
You need to manually copy your Icedove profiles from @file{~/.icedove} to
@file{~./thunderbird}.  It may be required to start Icedove with
@option{--ProfileManager} for the first time after the migration.")
         (de "Icedove 91 erwartet Ihren Profilordner unter @file{~/.thunderbird}.
Dafür müssen sie Ihre Icedove-Profile von @file{~/.icedove} nach
@file{~/.thunderbird} kopieren.  Eventuell muss Icedove das erste Mal nach der
Migration mit @option{--ProfileManager} gestartet werden.")
         (pt "Icedove 91 espera sua pasta de perfil dentro de
@file{~/.thunderbird}.  Você precisa copiar manualmente seus perfis Icedove de
@file{~/.icedove} para @file{~./thunderbird}.  Pode ser necessário iniciar
Icedove com @option{--ProfileManager} pela primeira vez após a migração.")))

 (entry (commit "746584e0ca200e7bf51b139ceb36c19ea81d6ef1")
        (title
         (en "New @command{guix shell} command supersedes @command{guix
environment}")
         (de "Neuer Befehl @command{guix shell} löst @command{guix
environment} ab")
         (fr "Nouvelle commande @command{guix shell} en remplacement de
@command{guix environment}")
         (pt "Novo comando @command{guix shell} substitui @command{guix environment}"))
        (body
         (en "A new @command{guix shell} command is now available.  It is
similar to @command{guix environment}, but with a more convenient interface
(@command{guix environment} is deprecated but will remain available until May,
1st 2023).  The main difference compared to @command{guix environment} is that
the \"ad hoc\" mode is the default.  Thus, to create an interactive
environment containing Python, NumPy, and SciPy, you would run:

@example
guix shell python python-numpy python-scipy
@end example

To get a development environment for, say, Inkscape, pass the @option{-D}
flag:

@example
guix shell -D inkscape
@end example

Another difference is that running @command{guix shell} without arguments
loads @file{manifest.scm} or @file{guix.scm} for the current directory or an
ancestor, provided you allowed it.  The command maintains a cache to speed up
access to such environments.

Run @command{info \"(guix) Invoking guix shell\"} for more information.")
         (de "Ein neuer Befehl @command{guix shell} ist ab jetzt
verfügbar. Er ähnelt @command{guix environment}, ist aber leichter zu
benutzen (@command{guix environment} gilt als veraltet, bleibt aber
bis zum 1.@: Mai 2023 verfügbar). Der größte Unterschied ist, dass das
Verhalten mit @option{--ad-hoc} nun der Normalfall ist. D.h.@: um eine
interaktive Umgebung mit Python, NumPy und SciPy zu bekommen, lautet
der Befehl:

@example
guix shell python python-numpy python-scipy
@end example

Wenn Sie eine Entwicklungsumgebung für, sagen wir, Inkscape schaffen
wollen, übergeben Sie die Option @option{-D}:

@example
guix shell -D inkscape
@end example

Noch ein Unterschied ist, dass wenn Sie @command{guix shell} ohne
Argumente ausführen, @file{manifest.scm} oder @file{guix.scm} aus dem
aktuellen Arbeitsverzeichnis oder einem übergeordneten Verzeichnis
geladen wird, wenn Sie die Berechtigung dazu erteilt haben. Für den
Befehl wird ein Zwischenspeicher vorgehalten, damit Sie schneller auf
solche Umgebungen zugreifen können.

Führen Sie @command{info \"(guix) Invoking guix shell\"} aus, um mehr
zu erfahren.")
         (fr "Une nouvelle commande, @command{guix shell}, est maintenant
disponible.  Elle est similaire à @command{guix environment}, mais avec une
interface plus pratique (@command{guix environment} est désuet mais restera
disponible jusqu'au 1er mai 2023).  La principale différence par rapport à
@command{guix environment} est que le mode par défaut est le mode \"ad hoc\".
Pour créer un environnement interactif contenant Python, NumPy et SciPy, il
faut donc lancer :

@example
guix shell python python-numpy python-scipy
@end example

Pour obtenir un environnement de développement pour Inkscape, par exemple,
passer l'option @option{-D} :

@example
guix shell -D inkscape
@end example

Une autre différence est qu'en lançant @command{guix shell} sans argument, le
fichier @file{manifest.scm} ou @file{guix.scm} du répertoire courant ou d'un
parent est automatiquement chargé, à condition de l'avoir autorisé.  La
commande garde un cache pour accélérer l'accès à ces environnements.

Lancer @command{info \"(guix.fr) Invoquer guix shell\"} pour plus
d'informations.")
         (pt "O novo comando @command{guix shell} já está disponível.  Ele é
similar a @command{guix environment}, mas com uma interface mais conveniente
(@command{guix environment} está depreciado mas permanecerá disponível até
01/05/2023).  A principal diferença comparada a @command{guix environment} é
que o modo \"ad hoc\" é o padrão.  Portanto, para criar um ambiente interativo
contendo Python, NumPy e SciPy, você executaria:

@example
guix shell python python-numpy python-scipy
@end example

Para obter um ambiente de desenvolvimento para, digamos, o Inkscape, passe a
opção sinalizadora @option{-D}:

@example
guix shell -D inkscape
@end example

Outra diferença é que executar @command{guix shell} sem argumentos carrega
@file{manifest.scm} ou @file{guix.scm} do diretório corrente ou um ancestral,
dado que você permita.  O comando mantém um cache para acelerar o acesso a
tais ambientes.

Execute @command{info \"(guix.pt_BR) Invocando guix shell\"} para mais
informação.")))

 (entry (commit "a2324d8b56eabf8117bca220a507cc791edffd2e")
        (title
         (en "Guix Home is a part of GNU Guix")
         (de "Guix Home ist jetzt Teil von GNU Guix")
         (pt "Guix Home é parte do GNU Guix")
         (ru "Guix Home теперь поставляется в составе GNU Guix"))
        (body
         (en "Guix Home split out from rde project and now is a part of
Guix proper.  It is available as a @emph{technology preview} and thus subject
to change.

The new @command{guix home} command with its actions allows users to
manage their packages and configurations (aka. dotfiles) in a declarative way,
similar to how many people manage their system with @command{guix system}.

Take a look at available actions and arguments:
@example
guix home --help
@end example

See @command{info \"(guix) Home Configuration\"} for more information.")
         (de "Guix Home ist aus dem rde-Projekt ins offizielle Guix übernommen
worden. Es ist als @emph{Technologievorschau} bereits verfügbar, aber die
Schnittstelle kann sich in Zukunft noch ändern.

Der neue Befehl @command{guix home} ermöglicht es, die Pakete und
Konfigurationsdateien (Dotfiles) für ein Benutzerkonto im deklarativen Stil zu
verwalten. Es ist analog dazu, wie man @command{guix system} benutzen kann, um
sein System zu verwalten.

Werfen Sie einen Blick auf die verfügbaren Aktionen und Argumente:
@example
guix home --help
@end example

Führen Sie für mehr Informationen @command{info \"(guix) Home Configuration\"}
aus.")
         (pt "Guix Home foi extraído do projeto rde e agora é parte do Guix
propriamente.  Ele está disponível como uma @emph{prévia da tecnologia} e
portanto sujeito a mudanças.

O novo comando @command{guix home} com suas ações permite aos usuários
gerenciar seus pacotes e configurações (também conhecidos como @i{dotfiles})
de maneira declarativa, semelhante a como muitas pessoas gerenciam seus
sistemas com @command{guix system}.

Dê uma olhada nas ações e argumentos disponíveis:
@example
guix home --help
@end example

Veja @command{info \"(guix) Home Configuration\"} para mais informação.")
         (ru "Guix Home отделился от проекта rde и теперь является частью
Guix.  Новая команда @command{guix home} даёт возможность пользователям
управлять их пакетами и конфигурациями (дотфайлами) для них в декларативном
стиле, аналогично тому, как многие люди управляют своими системами с помощью
@command{guix system}.

Чтобы получить список доступных действий и аргументов:
@example
guix home --help
@end example

Смотрите @command{info \"(guix) Home Configuration\"} для получения более
детальных сведений.")))

 (entry (commit "5b32ad4f6f555d305659cee825879df075b06331")
        (title
         (en "New @option{--max-depth} option for @command{guix graph}")
         (de "Neue Option @option{--max-depth} für @command{guix graph}")
         (fr "Nouvelle option @option{--max-depth} pour @command{guix graph}")
         (pt "Nova opção @option{--max-depth} para @command{guix graph}"))
        (body
         (en "The @command{guix graph} command has a new @option{--max-depth}
(or @option{-M}) option, which allows you to restrict a graph to the given
depth---very useful when visualizing large graphs.  For example, the command
below displays, using the @code{xdot} package, the dependency graph of
LibreOffice, including only nodes that are at most at distance 2 of
LibreOffice itself:

@example
guix graph -M 2 libreoffice | xdot -
@end example

See @command{info \"(guix) Invoking guix graph\"} for more information.")
         (de "Der Befehl @command{guix graph} verfügt über eine neue
Befehlszeilenoption @option{--max-depth} (oder @option{-M}), mit der
Sie einen Graphen auf die angegebene Tiefe einschränken. Das ist vor
allem bei großen Graphen nützlich; zum Beispiel zeigt der folgende
Befehl, unter Verwendung des Pakets @code{xdot}, den
Abhängigkeitsgraphen von LibreOffice unter Ausschluss der Knoten, die
eine Distanz größer als 2 von LibreOffice selbst haben:

@example
guix graph -M 2 libreoffice | xdot -
@end example

Führen Sie @code{info \"(guix.de) Aufruf von guix graph\"} aus, um mehr zu
erfahren.")
         (fr "La commande @command{guix graph} dispose d'une nouvelle option
@option{--max-depth} (ou @option{-M}) pour restreindre la profondeur d'un
graphe---très utile pour visualiser des gros graphes.  Par exemple, la
commande ci-dessous affiche, en utilisant @code{xdot}, le graphe de dépendance
de LibreOffice en n'incluant que les nœuds qui sont au plus à distance 2 de
LibreOffice soi-même :

@example
guix graph -M 2 libreoffice | xdot -
@end example

Voir @command{info \"(guix.fr) Invoquer guix graph\"} pour plus
d'informations.")
         (pt "O comando @command{guix graph} tem uma opção nova
@option{--max-depth} (or @option{-M}), que te permite restringir o grafo até a
prfundidade dada---muito útil quando se visualizam grafos grandes.  Por
exemplo, o comando abaixo exibe, usando o pacote @code{xdot}, o grafo de
dependência do LibreOffice, incluindo somente os nós que estão a uma distância
de no máximo 2 do próprio LibreOffice:

@example
guix graph -M 2 libreoffice | xdot -
@end example

Veja @command{info \"(guix.pt_BR) Invocando guix graph\"} para mais
informação.")))

 (entry (commit "05f44c2d858a1e7b13c90362c35fa86bdc4d5a24")
        (title
         (en "Channel clones fall back to Software Heritage")
         (de "Zum Klonen von Kanälen wird notfalls auf Software Heritage zurückgegriffen")
         (fr "Les clones de canaux peuvent recourir à Software Heritage")
         (pt "Clones de canais recorrem ao Software Heritage"))
        (body
         (en "When @command{guix time-machine} or @command{guix pull} fetches
a channel pinned to a specific commit, it now automatically falls back to
cloning it from the Software Heritage archive if the original URL is
unreachable.  This contributes to long-term reproducibility.  See
@command{info \"(guix) Replicating Guix\"}.

Automatic fallback also works for other Git clones made on your behalf, such
as when using @option{--with-commit} and related package transformation
options.")
         (de "Wenn bei @command{guix time-machine} oder @command{guix
pull} ein bestimmter Commit eines Kanals bezogen werden soll, wird
jetzt für den Fall, dass die ursprüngliche URL unerreichbar ist,
automatisch vom Software-Heritage-Archiv geklont. Das trägt zur
langfristigen Reproduzierbarkeit bei. Siehe @command{info \"(guix.de)
Guix nachbilden\"}.

Der automatische Rückgriff auf Software Heritage findet auch
Verwendung bei anderen Arten von Git-Klon, die Guix durchführt, z.B.@:
wenn Sie @option{--with-commit} und ähnliche Paketumwandlungsoptionen
einsetzen.")
         (fr "Quand la commande @command{guix time-machine} ou @command{guix
pull} récupère un canal fixé à une révision spécifique, elle est maintenant
capable de le cloner depuis l'archive Software Heritage si l'URL initiale
n'est plus disponible.  Cela contribue à la reproductibilité à long terme.
Voir @command{info \"(guix.fr) Répliquer Guix\"}.

Ce recours à Software Heritage fonctionne aussi pour les autres clones Git que
Guix peut faire, comme lorsqu'on utilise @option{--with-commit} et les options
de transformation de paquet similaires.")
         (pt "Quando @command{guix time-machine} ou @command{guix pull} busca
um canal fixado em um commit específico, agora ele recorre automaticamente a
cloná-lo do arquivo do Software Heritage se a URL original não for acessível.
Isto contribui para a reprodutibilidade de longo prazo.  Veja @command{info
\"(guix.pt_BR) Replicando Guix\"}.

Recurso automático também funciona para outros clones Git feitos em seu nome,
tais quando usando a opção @option{--with-commit} e outras relacionadas à
transformação de pacotes.")))

 (entry (commit "db4681a4c17d282a661552f2f57e5c453d02e414")
        (title
         (en "@code{gdm-service-type} now supports Wayland")
         (de "@code{gdm-service-type} bietet nun Unterstützung für Wayland")
         (fr "@code{gdm-service-type} prend maintenant en charge Wayland")
         (pt "@code{gdm-service-type} agora suporta Wayland"))
        (body
         (en "@code{gdm-service-type} has been updated to support being launched
as a Wayland client, and to launch Wayland sessions. The @code{wayland?} boolean
field in @code{gdm-configuration} controls whether GDM starts in Wayland or X
mode. See @command{info \"(guix) X Window\"} for more information.

Wayland mode for GDM will soon become the default in Guix, so if your
hardware doesn't support Wayland (Nvidia users are the most concerned here),
please consider disabling it now.")
         (de "@code{gdm-service-type} wurde um Unterstützung dafür
aktualisiert, als Wayland-Client gestartet zu werden und Wayland-Sitzungen zu
starten.  Der Boolesche Wert im Feld @code{wayland?} in
@code{gdm-configuration} bestimmt, ob GDM im Wayland- oder X-Modus gestartet
wird.  Siehe @command{info \"(guix.de) X Window\"} für weitere Informationen.

Bald wird der Wayland-Modus für GDM die Vorgabeeinstellung in Guix werden,
daher sollten Sie, wenn Ihre Hardware kein Wayland unterstützt (Nvidia-Nutzer
betrifft dies am ehesten), ihn jetzt ausdrücklich abschalten.")
         (fr "@code{gdm-service-type} a été mis à jour et peut maintenant être
lancé comme client Wayland, ainsi que lancer des sessions Wayland. Le champ
booléen @code{wayland?} de @code{gdm-configuration} contrôle le mode dans lequel
GDM est lancé (Wayland ou X). Pour plus d'informations, voir
@command{info \"(guix) X Window\"} (en anglais).

GDM sera bientôt lancé en mode Wayland par défaut sur Guix, donc si votre matériel
ne le prend pas en charge (les utilisateur·ices de cartes Nvidia sont les plus
concerné·es), merci de le désactiver dès maintenant.")
         (pt "@code{gdm-service-type} foi atualizado para suportar ser
disparado como um cliente Wayland, e para disparar sessões Wayland.  O campo
booleano @code{wayland?} em @code{gdm-configuration} controla se o GDM inicia
em modo Wayland ou X. Veja @command{info \"(guix.pt_BR) X Window\"} para mais
informação.

O modo Wayland para o GDM em breve se tornará o padrão no Guix, então se seu
hardware não suporta Wayland (usuários de Nvidia são os mais interessados
aqui), favor considere desabilitá-lo agora.")))

 (entry (commit "f23803af2018a148fb088f2516d79c20d6bf95f0")
        (title
         (en "Input labels can now be omitted in package definitions")
         (de "Eingaben in Paketdefinitionen brauchen keine Bezeichnungen mehr")
         (pt "Rótulos de entrada agora podem ser omitidos nas definições de pacotes"))
        (body
         (en "If you have written package definitions before, you may know
that package inputs required a bit of boilerplate: each input needs to have an
associated label (a string), which you can refer to in ``build-side code''.

Input labels are now unnecessary, meaning that you can write code like:

@lisp
(package
  ;; @dots{}
  (inputs (list libunistring libffi libgc)))
@end lisp

Notice that the @code{inputs} field is simplified compared to the ``old
style''.  When needed, you can now use g-expressions (gexps) to refer to
another package in build-side code.  Additionally, the new
@code{modify-inputs} macro facilitates common operations on inputs---deleting,
replacing, adding inputs.

To ease transition to the ``new style'', a new @command{guix style} command is
provided.  Run @command{info \"(guix) Invoking guix style\"} for more info.")
         (de "Wenn Sie bereits Paketdefinitionen verfasst haben,
erinnern Sie sich vielleicht, dass Sie für Paketeingaben manches
doppelt schreiben mussten: Jede Eingabe wird assoziiert mit einer
Bezeichnung (als Zeichenkette), auf die Sie sich in
„erstellungsseitigem Code“ beziehen können.

Diese Eingabebezeichnungen sind @emph{nicht} mehr nötig.  Sie können
jetzt solchen Code schreiben:

@lisp
(package
  ;; …
  (inputs (list libunistring libffi libgc)))
@end lisp

Achten Sie auf das gegenüber früher vereinfachte @code{inputs}-Feld.
Wenn nötig können Sie in erstellungsseitigem Code G-Ausdrücke (gexps)
benutzen, um andere Pakete zu referenzieren.  Des Weiteren erleichtert
das Makro @code{modify-inputs} geläufige Operationen auf Eingaben —
das Löschen, Ersetzen, Hinzufügen von Eingaben.

Um den Übergang zum „neuen Stil“ zu erleichtern, steht ein neuer
Befehl @command{guix style} zur Verfügung.  Führen Sie @command{info
\"(guix.de) Aufruf von guix style\"} aus, um mehr Informationen zu
erhalten.")
         (pt "Se você já escreveu definições de pacote antes, você
provavelmente sabe que as entradas do pacote exigem um pouco de código
padronizado (@i{boilerplate}): cada entrada precisa de um rótulo associado
(uma string), ao qual você pode referir no ``código do lado da construção''.

Rótulos de entrada agora são desnecessários, implicando que você pode escrever
código assim:

@lisp
(package
  ;; @dots{}
  (inputs (list libunistring libffi libgc)))
@end lisp

Note que o campo @code{inputs} é simplificado comparado ao ``velho estilo''.
Quando necessário, agora você pode usar expressões G (gexps) para referir a
outro pacote no lado da construção.  Adicionalmente, a nova macro
@code{modify-inputs} facilita operações comuns nas entradas---remoção,
substituição, adição etc.

Para suavizar a transição para o ``novo estilo'', um novo comando
@command{guix style} é fornecido.  Execute @command{info \"(guix.pt_BR)
Invocando guix style\"} para mais info.")))

 (entry (commit "82daab42811a2e3c7684ebdf12af75ff0fa67b99")
        (title
         (en "New @samp{deb} format for the @command{guix pack} command")
         (de "Neues Format @samp{deb} für den Befehl @command{guix pack}")
         (pt "Novo formato @samp{deb} para o comando @command{guix pack}"))
        (body
         (en "Debian archives (with the .deb file extension) can now be
produced via the @command{guix pack --format=deb} command, providing an
alternative distribution path for software built with Guix.  Here is a simple
example that generates a Debian archive for the @code{hello} package:

@example
guix pack --format=deb --symlink=/usr/bin/hello=bin/hello hello
@end example

See @command{info \"(guix) Invoking guix pack\"} for more information.")
         (de "Debian-Archive (mit der Dateinamenserweiterung .deb) können
jetzt auch mit dem Befehl @command{guix pack --format=deb} erzeugt werden, um
mit Guix erstellte Software auf andere Art anzubieten.  Hier sehen Sie ein
einfaches Beispiel, wie ein Debian-Archiv für das Paket @code{hello} angelegt
wird:

@example
guix pack --format=deb --symlink=/usr/bin/hello=bin/hello hello
@end example

Siehe @command{info \"(guix.de) Aufruf von guix pack\"} für mehr
Informationen.")
         (pt "Arquivos Debian (com a extensão de arquivo .deb) agora podem ser
produzidos mediante o comando @command{guix pack --format=deb}, fornecendo um
caminho alternativo de distribuição para software feito com Guix.  Eis um
exemplo simples que gera um arquivo Debian para o pacote @code{hello}:

@example
guix pack --format=deb --symlink=/usr/bin/hello=bin/hello hello
@end example

Veja @command{info \"(guix.pt_BR) Invocando guix pack\"} para mais
informação.")))

 (entry (commit "bdc298ecee15283451d3aa20a849dd7bb22c8538")
        (title
         (en "New @command{guix import egg} command")
         (de "Neuer Befehl @command{guix import egg}")
         (pt "Novo comando @command{guix import egg}")
         (zh "新的 @command{guix import egg} 命令"))
        (body
         (en "The new @command{guix import egg} command allows packagers to
generate a package definition or a template thereof given the name of a
CHICKEN egg package, like so:

@example
guix import egg sourcehut
@end example

Run @command{info \"(guix) Invoking guix import\"} for more info.")
         (de "Mit dem neuen Befehl @command{guix import egg} können
Paketautoren eine Paketdefinition oder eine Vorlage dafür anhand des Namens
eines „Egg“-Pakets für CHICKEN erzeugen, etwa so:

@example
guix import egg sourcehut
@end example

Führen Sie @command{info \"(guix.de) Aufruf von guix import\"} aus, um mehr
Informationen zu bekommen.")
         (pt "O novo comando @command{guix import egg} permite aos
empacotadores gerar uma definição de pacote ou um gabarito dado o nome de um
pacote egg do CHICKEN, assim:

@example
guix import egg sourcehut
@end example

Executex @command{info \"(guix.pt_BR) Invoking guix import\"} para mais
info.")
         (zh "新的 @command{guix import egg} 命令能让贡献者从一个CHICKEN egg生
成一个包装或包装样板。

@example
guix import egg sourcehut
@end example

想了解更多可以运行 @command{info \"(guix) Invoking guix import\"}。")))

 (entry (commit "2161820ebbbab62a5ce76c9101ebaec54dc61586")
        (title
         (en "Risk of local privilege escalation during user account creation")
         (de "Risiko lokaler Rechteausweitung während der Erstellung von Benutzerkonten")
         (pt "Risco de escalação local de privilégio durante a criação de conta de usuário"))
        (body
         (en "A security vulnerability that can lead to local privilege
escalation has been found in the code that creates user accounts on Guix
System---Guix on other distros is unaffected.  The system is only vulnerable
during the activation of user accounts that do not already exist.

This bug is fixed and Guix System users are advised to upgrade their system,
with a command along the lines of:

@example
guix system reconfigure /run/current-system/configuration.scm
@end example

The attack can happen when @command{guix system reconfigure} is running.
Running @command{guix system reconfigure} can trigger the creation of new user
accounts if the configuration specifies new accounts.  If a user whose account
is being created manages to log in after the account has been created but
before ``skeleton files'' copied to its home directory have the right
ownership, they may, by creating an appropriately-named symbolic link in the
home directory pointing to a sensitive file, such as @file{/etc/shadow}, get
root privileges.

See @uref{https://issues.guix.gnu.org/47584} for more information on this
bug.")
         (de "Eine Sicherheitslücke, die eine lokale Rechteausweitung zur
Folge haben kann, wurde in dem Code gefunden, mit dem Benutzerkonten auf Guix
System angelegt werden — Guix auf anderen Distributionen ist nicht betroffen.
Das System kann nur während der Aktivierung noch nicht existierender
Benutzerkonten angegriffen werden.

Der Fehler wurde behoben und wir empfehlen Nutzern von Guix System, ihre
Systeme zu aktualisieren, mit einem Befehl wie:

@example
guix system reconfigure /run/current-system/configuration.scm
@end example

Der Angriff kann erfolgen, während @command{guix system reconfigure} läuft.
Wenn @command{guix system reconfigure} ausgeführt wird, kann das die Erzeugung
neuer Benutzerkonten auslösen, wenn in der Konfiguration neue Konten angegeben
wurden.  Wenn ein Benutzer, dessen Konto gerade angelegt wird, es
fertigbringt, sich anzumelden, bevor „Skeleton-Dateien“ in seinem Persönlichen
Verzeichnis den richtigen Besitzer haben, kann er durch Anlegen einer gezielt
benannten symbolischen Verknüpfung in seinem Persönlichen Verzeichnis auf eine
sensible Datei wie @file{/etc/shadow} Administratorrechte erlangen.

Siehe @uref{https://issues.guix.gnu.org/47584} für mehr Informationen zu
diesem Fehler.")
         (pt "Uma vulnerabilidade de segurança que pode levar à escalação
local de privilégio foi encontrado no código que cria contas de usuário no
Sistema Guix---em outras distros o Guix não é afetado.  O sistema só é
vulnerável durante a ativação de contas de usuário que não existem ainda.

Esta falha está consertada e recomendamos que usuários do Sistema Guix
atualizem seus sistemas, com um comando nas linhas de:

@example
guix system reconfigure /run/current-system/configuration.scm
@end example

O ataque pode ocorrer durante a execução de @command{guix system reconfigure}.
Executar @command{guix system reconfigure} pode disparar a criação de novas
contas de usuário se a configuração especificar contas novas.  Se um usuário
cuja conta está sendo criada consegue fazer login após a conta ser criada mas
antes dos ``arquivos-esqueleto'' serem copiados para seu diretório Home ter a
posse correta estabelecida, o usuário pode, criando um link simbólico
apropriadamente nomeado no diretório Home apontando para um arquivo sensível,
como @file{/etc/shadow}, obter privilégios de superusuário.

Veja @uref{https://issues.guix.gnu.org/47584} para mais informação sobre esta
falha.")))
 (entry (commit "e52ec6c64a17a99ae4bb6ff02309067499915b06")
        (title
         (en "New supported platform: powerpc64le-linux")
         (de "Neue Plattform wird unterstützt: powerpc64le-linux")
         (fr "Nouvelle plate-forme prise en charge : powerpc64le-linux")
         (pt "Nova plataforma suportada: powerpc64le-linux"))
        (body
         (en "A new platform, powerpc64le-linux, has been added for
little-endian 64-bit Power ISA processors using the Linux-Libre kernel.  This
includes POWER9 systems such as the
@uref{https://www.fsf.org/news/talos-ii-mainboard-and-talos-ii-lite-mainboard-now-fsf-certified-to-respect-your-freedom,
RYF Talos II mainboard}. This platform is available as a \"technology
preview\": although it is supported, substitutes are not yet available from
the build farm, and some packages may fail to build.  In addition, Guix System
is not yet available on this platform.  That said, the Guix community is
actively working on improving this support, and now is a great time to try it
and get involved!")
         (de "Eine neue Plattform, powerpc64le-linux, wurde hinzugefügt. Mit
ihr können Prozessoren mit 64-Bit-Power-Befehlssatz, little-endian, mit dem
Linux-Libre-Kernel betrieben werden.  Dazu gehören POWER9-Systeme wie die
@uref{https://www.fsf.org/news/talos-ii-mainboard-and-talos-ii-lite-mainboard-now-fsf-certified-to-respect-your-freedom,
RYF-zertifizierte Talos-II-Hauptplatine}.  Bei der Plattform handelt es sich
um eine „Technologievorschau“; obwohl sie unterstützt wird, gibt es noch keine
Substitute von der Erstellungsfarm und bei manchen Paketen könnte die
Erstellung fehlschlagen.  Des Weiteren ist Guix System auf dieser Plattform
noch nicht verfügbar.  Dennoch arbeitet die Guix-Gemeinde aktiv daran, diese
Unterstützung auszubauen, und jetzt ist eine gute Gelegenheit, sie
auszuprobieren und mitzumachen!")
         (fr "Une nouvelle plate-forme, powerpc64le-linux, a été ajoutée pour
les processeurs POWER 64-bits utilisant le noyau Linux-libre.  Ça inclut les
systèmes POWER9 tels que les
@uref{https://www.fsf.org/news/talos-ii-mainboard-and-talos-ii-lite-mainboard-now-fsf-certified-to-respect-your-freedom,
cartes Talos II RYF}.  Il s'agit pour le moment d'un « avant-goût » de la
technologie : bien que la plate-forme soit prise en charge, la ferme de
compilation ne fournit pas encore de substituts et certains paquets risquent
de ne pas compiler.  En outre, Guix System n'est pas encore disponible sur
cette plate-forme.  Ceci dit, la communauté Guix travaille activement pour
améliorer cette prise en charge et c'est maintenant un bon moment pour
l'essayer et pour s'impliquer !")
         (pt "Uma nova plataforma, powerpc64le-linux, foi adicionada para
processadores de ISA Power 64 bits little endian usando o kernel Linux-Libre.
Isto inclui sistemas POWER9 como a placa
@uref{https://www.fsf.org/news/talos-ii-mainboard-and-talos-ii-lite-mainboard-now-fsf-certified-to-respect-your-freedom,
RYF Talos II}. A plataforma está disponível como uma \"prévia da tecnologia\":
apesar de ser suportada, substitutos ainda não estão disponíveis a partir da
fazenda de construção, e alguns pacotes podem falhar na construção. Além
disso, o Sistema guix ainda não está disponível nesta plataforma.  Dito isso,
a comunidade Guix está trabalhando ativamente na melhoria do suporte, e agora
é um grande momento para tentar e participar!")))

 (entry (commit "9ade2b720af91acecf76278b4d9b99ace406781e")
        (title
         (en "Update on previous @command{guix-daemon} local privilege escalation")
         (de "Nachtrag zur lokalen Rechteausweitung bei @command{guix-daemon}")
         (nl "Aanvulling bij escalatie van bevoegdheden via @command{guix-daemon}")
         (pt "Atualização escalação local de privilégio no anterior @command{guix-daemon}"))
        (body
         (en "The previous news item described a potential local privilege
escalation in @command{guix-daemon}, and claimed that systems with the Linux
@uref{https://www.kernel.org/doc/Documentation/sysctl/fs.txt,
``protected hardlink''} feature enabled were unaffected by the vulnerability.

This is not entirely correct.  Exploiting the bug on such systems is harder,
but not impossible.  To avoid unpleasant surprises, all users are advised to
upgrade @command{guix-daemon}.  Run @command{info \"(guix) Upgrading Guix\"}
for info on how to do that.  See
@uref{https://guix.gnu.org/en/blog/2021/risk-of-local-privilege-escalation-via-guix-daemon/}
for more information on this bug.")
         (de "In der letzten Neuigkeit wurde eine mögliche lokale
Rechteausweitung im @command{guix-daemon} beschrieben und behauptet, dass
Systeme, auf denen Linux’
@uref{https://www.kernel.org/doc/Documentation/sysctl/fs.txt,
„Geschützte-Hardlinks“-Funktionalität} aktiviert ist, von der Sicherheitslücke
nicht betroffen seien.

Das stimmt nicht ganz.  Die Lücke auf solchen Systemen auszunutzen, ist
schwerer, aber nicht unmöglich.  Um unangenehme Überraschungen zu vermeiden,
empfehlen wir allen Nutzern, @command{guix-daemon} zu aktualisieren.  Führen
Sie @command{info \"(guix.de) Aktualisieren von Guix\"} aus, um zu erfahren,
wie Sie ihn aktualisieren können.  Siehe
@uref{https://guix.gnu.org/de/blog/2021/risk-of-local-privilege-escalation-via-guix-daemon/}
für mehr Informationen zu diesem Fehler.")
         (nl "Het vorige nieuwsbericht beschreef een beveiligingsprobleem in
@command{guix-daemon} dat kan leiden tot de escalatie van lokale bevoegdheden.
Het bericht stelde dat machines waarop de
@uref{https://www.kernel.org/doc/Documentation/sysctl/fs.txt,
``protected hardlink''}-optie van Linux is inschakeld niet kwetsbaar zijn.

Dit is niet volledig juist.  De optie maakt het uitbuiten van de fout
moeilijker maar niet onmogelijk.  Om onaangename verrassingen te voorkomen
is het voor iedereen aangeraden om @command{guix-daemon} op te waarderen.
Voer @command{info \"(guix) Upgrading Guix\"} uit voor meer informatie
daarover.  Lees
@uref{https://guix.gnu.org/en/blog/2021/risk-of-local-privilege-escalation-via-guix-daemon/}
voor meer informatie over het probleem.")
         (pt "A notícia anterior descrevia uma potencial escalação local de
privilégios em @command{guix-daemon}, e alegava que sistemas com o recurso do
Linux de @uref{https://www.kernel.org/doc/Documentation/sysctl/fs.txt,
``hardlink protegido''} habilitado não eram afetados pela vulnerabilidade.

Isto não é inteiramente correto.  Explorar a falha em tais sistemas é mais
difícil, mas não impossível.  Para evitar surpresas desagradáveis,
aconselhamos todos os usuários a atualizar @command{guix-daemon}.  Execute
@command{info \"(guix.pt_BR) Atualizando o Guix\"} para info sobre como fazer
isso.  Veja
@uref{https://guix.gnu.org/en/blog/2021/risk-of-local-privilege-escalation-via-guix-daemon/}
para mais informação sobre esta falha.")))

 (entry (commit "ec7fb669945bfb47c5e1fdf7de3a5d07f7002ccf")
        (title
         (en "Risk of local privilege escalation @i{via} @command{guix-daemon}")
         (de "Risiko lokaler Rechteausweitung über @command{guix-daemon}")
         (fr "Risque d'élévation locale de privilèges @i{via} @command{guix-daemon}")
         (nl "Risico op escalatie van bevoegdheden via @command{guix-daemon}")
         (pt "Risco de escalação local de privilégios @i{via} @command{guix-daemon}"))
        (body
         (en "A security vulnerability that can lead to local privilege
escalation has been found in @command{guix-daemon}.  It affects multi-user
setups in which @command{guix-daemon} runs locally.

It does @emph{not} affect multi-user setups where @command{guix-daemon} runs
on a separate machine and is accessed over the network, @i{via}
@env{GUIX_DAEMON_SOCKET}, as is customary on cluster setups.  Machines where
the Linux @uref{https://www.kernel.org/doc/Documentation/sysctl/fs.txt,
``protected hardlink''} feature is enabled, which is common, are also
unaffected---this is the case when the contents of
@file{/proc/sys/fs/protected_hardlinks} are @code{1}.

The attack consists in having an unprivileged user spawn a build process, for
instance with @command{guix build}, that makes its build directory
world-writable.  The user then creates a hardlink within the build directory
to a root-owned file from outside of the build directory, such as
@file{/etc/shadow}.  If the user passed the @option{--keep-failed} option and
the build eventually fails, the daemon changes ownership of the whole build
tree, including the hardlink, to the user.  At that point, the user has write
access to the target file.

You are advised to upgrade @command{guix-daemon}.  Run @command{info \"(guix)
Upgrading Guix\"}, for info on how to do that.  See
@uref{https://issues.guix.gnu.org/47229} for more information on this bug.")
         (de "Eine Sicherheitslücke, die zu einer lokalen Rechteausweitung
führen kann, wurde in @command{guix-daemon} gefunden.  Sie betrifft
Mehrbenutzersysteme, auf denen @command{guix-daemon} lokal läuft.

@emph{Nicht} betroffen sind Mehrbenutzersysteme, auf denen
@command{guix-daemon} auf einer separaten Maschine läuft und darauf über das
Netzwerk mittels @env{GUIX_DAEMON_SOCKET} zugegriffen wird, was auf
Rechen-Clustern üblich ist.  Auch Maschinen, auf denen Linux’
@uref{https://www.kernel.org/doc/Documentation/sysctl/fs.txt,
„Geschützte-Hardlinks“-Funktionalität} aktiviert ist@tie{}– was häufig der
Fall ist@tie{}–, sind nicht betroffen; sie ist aktiviert, wenn
@file{/proc/sys/fs/protected_hardlinks} den Inhalt @code{1} hat.

Der Angriff besteht darin, dass ein unprivilegierter Benutzer einen
Erstellungsprozess startet, etwa mit @command{guix build}, der allen
Schreibberechtigung auf sein Erstellungsverzeichnis erteilt.  In diesem
Erstellungsverzeichnis erzeugt der Benutzer nun eine harte Verknüpfung auf
eine Datei außerhalb des Erstellungsverzeichnisses, die dem
Administratornutzer root gehört, etwa @file{/etc/shadow}.  Wenn der Nutzer die
Befehlszeilenoption @option{--keep-failed} angegeben hat und die Erstellung
irgendwann fehlschlägt, trägt der Daemon als Besitzer des gesamten
Erstellungsverzeichnisses den Benutzer ein, Hardlink eingeschlossen.  Jetzt
hat der Benutzer Schreibzugriff auf die Zieldatei bekommen.

Wir empfehlen, dass Sie @command{guix-daemon} aktualisieren.  Führen Sie
@command{info \"(guix.de) Aktualisieren von Guix\"} aus, um zu erfahren, wie
Sie ihn aktualisieren können.  Siehe @uref{https://issues.guix.gnu.org/47229}
für mehr Informationen zu diesem Fehler.")
         (fr "Une faille de sécurité pouvant mener à une élévation locale de
privilèges a été trouvée dans @command{guix-daemon}.  Elle touche les
installations multi-utilisateur·ices dans lesquelles @command{guix-daemon}
tourne en local.

Elle @emph{n'affecte pas} les installations où @command{guix-daemon} tourne
sur une machine séparée et qu'on y accède à travers le réseau, @i{via}
@env{GUIX_DAEMON_SOCKET}, comme c'est typiquement le cas sur les grappes de
calcul (@i{clusters}).  Les machines où les
@uref{https://www.kernel.org/doc/Documentation/sysctl/fs.txt, ``liens
protégés''} de Linux sont activés, ce qui est courant, ne sont pas non plus
touchées ; cette fonctionnalité est activée si le contenu de
@file{/proc/sys/fs/protected_hardlinks} est @code{1}.

Pour mener cette attaque, un·e utilisateur·rice démarre un processus de
compilation, par exemple avec @command{guix build}, qui rend le répertoire de
compilation inscriptible pour tout le monde.  La personne créée ensuite un
lien dur (@i{hard link}) dans ce répertoire vers un fichier appartenant à
@code{root}, tel que @file{/etc/shadow}.  Si on a passé l'option
@option{--keep-failed} et que la compilation finit par échouer, le démon met
l'utilisateur·rice appelant·e comme propriétaire de l'ensemble du répertoire
de compilation, y compris le lien.  À ce stade, cette personne a accès en
écriture sur le fichier cible.

Nous conseillons de mettre à jour @command{guix-daemon}.  Lancer @command{info
\"(guix.fr) Mettre à niveau Guix\"} pour voir comment faire.  Voir
@uref{https://issues.guix.gnu.org/47229} pour plus d'informations sur cette
faille.")
         (nl "In @command{guix-daemon} werd een beveiligingsprobleem
gevonden dat kan leiden tot de escalatie van lokale bevoegdheden.  Het
probleem doet zich voor bij installaties met meerdere gebruikers waarop een
lokale @command{guix-daemon} draait.

Het heeft @emph{geen} invloed op systemen met meerdere gebruikers waarbij de
@command{guix-daemon} op een afzonderlijke machine draait en via
@env{GUIX_DAEMON_SOCKET} over het netwerk wordt aangesproken, zoals
gebruikelijk bij computerclusters.  Ook machines waarop de
@uref{https://www.kernel.org/doc/Documentation/sysctl/fs.txt,
``protected hardlink''}-optie van Linux is inschakeld, wat vaak het geval is,
zijn niet kwetsbaar.

De aanval bestaat erin dat een gebruiker zonder privileges een bouwproces
opstart, bijvoorbeeld met @command{guix build}, dat zijn werkmap beschrijfbaar
maakt voor alle gebruikers.  Vervolgens maakt de gebruiker vanuit deze map een
harde link naar een bestand erbuiten met @code{root} als eigenaar, zoals
@file{/etc/shadow}.  Als de gebruiker de @option{--keep-failed}-optie opgaf
en de bouw faalt, maakt @command{guix-daemon} de gebruiker eigenaar van de
volledige inhoud van de werkmap, met inbegrip van de harde link.  Op dat
moment bezit de gebruiker schrijfrechten over het doelbestand.

Het is aangeraden om @command{guix-daemon} op te waarderen.  Voer
@command{info \"(guix) Upgrading Guix\"} uit voor meer informatie daarover.
Lees @uref{https://issues.guix.gnu.org/47229} voor meer informatie over het
probleem.")
         (pt "Uma vulnerabilidade de segurança que pode levar a uma escalação
local de privilégios foi encontrada no @command{guix-daemon}.  Ela afeta
instalações multiusuário nas quais @command{guix-daemon} roca localmente.

Ela não afeta instalações multiusuário onde @command{guix-daemon} executa em
uma máquina separada e é acessado pela rede, @i{via} @env{GUIX_DAEMON_SOCKET},
como é costumeiro em instalações de @i{clusters}.  Máquinas onde o recurso
Linux @uref{https://www.kernel.org/doc/Documentation/sysctl/fs.txt,
``hardlink protegido''} é habilitado, o que é comum, também não são
afetadas---este é o caso quando o conteúdo de
@file{/proc/sys/fs/protected_hardlinks} é @code{1}.

O ataque consiste em um usuário não privilegiado disparando um processo de
construção, por exemplo com @command{guix build}, que torna seu diretório de
construção globalmente gravável.  O usuário então cria um hardlink, dentro do
diretório de construção, para um arquivo pertencente ao superusuário, como
@file{/etc/shadow}.  Se o usuário passou a opção @option{--keep-failed} e a
construçao eventualmente falhar, o daemon modifica a posse de toda a árvore de
construção, incluindo o hardlink, para o usuário.  Neste ponto, o usuário tem
acesso ao arquivo alvo.

Te aconselhamos a atualizar o @command{guix-daemon}.  Execute @command{info
\"(guix.pt_BR) Atualizando o Guix\"}, para info sobre como fazer isso.  Veja
@uref{https://issues.guix.gnu.org/47229} para mais informação sobre esta
falha.")))

 (entry (commit "77c2f4e2068ebec3f384c826c5a99785125ff72c")
        (title
         (en "@code{qemu-binfmt-service-type} is usable for any container")
         (de "@code{qemu-binfmt-service-type} funktioniert mit jedem Container")
         (fr "@code{qemu-binfmt-service-type} fonctionne avec tous les conteneurs")
         (pt "@code{qemu-binfmt-service-type} é utilizável para qualquer contêiner"))
        (body
         (en "The service now makes use of the statically built QEMU binaries
along with the fix binary (F) @code{binfmt_misc} flag, which allows the kernel
to fully pre-load it in memory.  QEMU can thus now be used with any container
without extra configuration.  The @code{guix-support?} field of the
@code{qemu-binfmt-configuration} record is removed, as it is no longer
necessary.")
         (de "Der Dienst benutzt jetzt statisch gebundene QEMU-Binärdateien
zusammen mit der Fix-Binary-Flag (F) von @code{binfmt_misc}.  Dadurch kann der
Kernel die QEMU-Binärdatei als Ganzes vorab in den Speicher laden.  Dann kann
sie auch ohne weitere Konfiguration in jeder Art von isolierter Umgebung
benutzt werden. Darum wurde das Feld @code{guix-support?} des
@code{qemu-binfmt-configuration}-Verbundsobjekts entfernt; es wird nicht mehr
gebraucht.")
         (fr "Le service utilise maintenant les binaire QEMU statiques avec
le drapeau « fixed » (F) de @code{binfmt_misc}, ce qui permet au noyau
de le charger entièrement en mémoire.  On peut donc maintenant utiliser QEMU
avec n'importe quel conteneur sans configuration supplémentaire.  Le champ
@code{guix-support?} de l'enregistrement @code{qemu-binfmt-configuration} a
été supprimé car il n'est pas nécessaire.")
         (pt "O serviço agora faz uso de binários QEMU construídos
estaticamente juntamente com a opção de binário fixo (F) @code{binfmt_misc}, o
que permite ao kernel pré-carregá-lo inteiramente na memória.  QEMU pode
portanto ser usado com qualquer contêiner sem configuração extra.  O campo
@code{guix-support?} do registro @code{qemu-binfmt-configuration} está
removido, já que não é mais necessário.")))

 (entry (commit "02e2e093e858e8a0ca7bd66c1f1f6fd0a1705edb")
        (title
         (en "New @command{guix import go} command")
         (de "Neuer Befehl @command{guix import go}")
         (fr "Nouvelle commande @command{guix import go}")
         (nl "Nieuwe @command{guix import go}-opdracht")
         (pt "Novo comando @command{guix import go}"))
        (body
         (en "The new @command{guix import go} command allows packagers to
generate a package definition or a template thereof given the name of a Go
package available through @url{https://proxy.golang.org}, like so:

@example
guix import go golang.org/x/sys
@end example

Run @command{info \"(guix) Invoking guix import\"} for more info.")
         (de "Mit dem neuen Befehl @command{guix import go} können
Paketautoren eine Paketdefinition oder eine Vorlage dafür anhand des Namens
eines auf @url{https://proxy.golang.org} verfügbaren Go-Pakets erzeugen, etwa
so:

@example
guix import go golang.org/x/sys
@end example

Führen Sie @command{info \"(guix.de) Aufruf von guix import\"} aus, um mehr
Informationen zu bekommen.")
         (fr "La nouvelle commande @command{guix import go} permet aux
empaqueteur·ice·s de générer une définition de paquet ou un modèle de
définition à partir du nom d'un paquet Go disponible via
@url{https://proxy.golang.org}, comme ceci :

@example
guix import go golang.org/x/sys
@end example

Lancez @command{info \"(guix.fr) Invoquer guix import\"} pour en savoir plus.")
         (nl "Met de nieuwe @command{guix import go}-opdracht kunnen
pakketschrijvers een pakketdefinitie of -sjabloon aanmaken, op basis van de
naam van een Go-pakket te vinden op @url{https://proxy.golang.org}:

@example
guix import go golang.org/x/sys
@end example

Voer @command{info \"(guix) Invoking guix import\"} uit voor meer
informatie.")
         (pt "O novo comando @command{guix import go} permite que
empacotadores gerem uma definição de pacote ou um gabarito do mesmo dado o
nome de um pacote Go disponível mediante @url{https://proxy.golang.org},
assim:

@example
guix import go golang.org/x/sys
@end example

Execute @command{info \"(guix.pt_BR) Invocando guix import\"} para mais
info.")))

 (entry (commit "1b5b882120daf7d111aa351a919a90e818324347")
        (title
         (en "The @code{linux-libre} kernel is updated to 5.11.2")
         (de "Der Kernel @code{linux-libre} wird auf 5.11.2 aktualisiert")
         (fr "Le noyau @code{linux-libre} est mis à jour vers la 5.11.2")
         (nl "De @code{linux-libre}-kernel werd bijgewertk naar 5.11.2")
         (pt "O kernel @code{linux-libre} foi atualizado para 5.11.2"))
        (body
         (en "The default @code{linux-libre} kernel is now based on the 5.11
stable kernel series, beginning with version 5.11.2.  Promiment features include
improved Wine performance, unprivileged Overlayfs mounts, support for Intel SGX,
support for new graphics hardware, and improved performance of the Btrfs
file system.")
         (de "Der standardmäßig verwendete @code{linux-libre}-Kernel basiert
jetzt auf der 5.11-„stable“-Versionsreihe, angefangen mit Version 5.11.2.  Zu
den markanten Neuerungen gehören bessere Wine-Unterstützung, Einbinden per
Overlayfs für Nutzer ohne erweiterte Rechte, Unterstützung für Intel SGX, für
neue Grafikhardware und bessere Leistung beim Btrfs-Dateisystem.")
         (fr "Le noyau @code{linux-libre} par défaut est maintenant basé sur la
lignée stable 5.11 du noyau, à commencer par la version 5.11.2.  Parmi les
fonctionnalités notables on trouve des performances améliorées pour Wine, le
montage Overlayfs non privilégié, la prise en charge d'Intel SGX, celle des
nouveaux périphériques graphiques et de meilleures performances du système de
fichiers Btrfs.")
         (nl "De standaard @code{linux-libre}-kernel is nu geëent op de
stabiele 5.11-reeks, te beginnen met versie 5.11.2.  Deze update biedt onder
andere verbeterde prestaties voor Wine en het Btfrs-bestandssysteem, laat
gewone gebruikers toe om met Overlayfs bestandssystemen te combineren, en
ondersteunt Intel SGX en nieuwe grafische apparatuur.")
         (pt "O kernel padrão @code{linux-libre} agora é baseado na série
estável 5.11, começando com a versão 5.11.2.  Características proeminentes
incluem desempenho melhorado do Wine, montagens não-privilegiadas de
Overlayfs, supporte para Intel SGX, suporte para novos hardwares gráficos, e
desempenho melhorado do sistemas de arquivos Btrfs.")))

 (entry (commit "6e8cdf1d26092cb9654e179b04730fff7c15c94f")
        (title
         (en "The @command{guix system image} command can now operate on image records")
         (de "Der Befehl @command{guix system image} kann jetzt auch mit @code{image}-Verbundsobjekten umgehen")
         (fr "La commande @command{guix system image} peut désormais fonctionner sur des images")
         (pt "O comando @command{guix system image} pode agora operar em registros de imagem"))
        (body
         (en "The @command{guix system image} command can now operate on
@code{image} records.  This means that the file parameter or the expression
passed to this command can return @code{image} or @code{operating-system}
records.

The @file{gnu/system/images} directory contains default images that can be
built by running @command{guix system image gnu/system/images/pine64.scm} for
instance.")
         (de "Sie können den Befehl @command{guix system image} jetzt auch auf
Verbundsobjekte vom Typ @code{image} anwenden.  Das heißt, wenn Sie eine Datei
oder einen Ausdruck als Parameter übergeben, darf dieser ein Verbundsobjekt
vom Typ @code{image} oder @code{operating-system} zurückliefern.

Im Verzeichnis @file{gnu/system/images} finden Sie vorkonfigurierte Abbilder
als @code{image}-Verbundsobjekte. Sie können zum Beispiel @command{guix system
image gnu/system/images/pine64.scm} ausführen, um das Abbild zu erstellen.")
         (fr "La commande @command{guix system image} peut désormais
fonctionner sur des images.  Cela signifie que le fichier ou l'expression
passé en paramètre de cette commande peuvent retourner une structure de type
@code{image} ou @code{operating-system}.

Le dossier @file{gnu/system/images} contient des images par défaut qui peuvent
être construites en lançant la commande @command{guix system image
gnu/system/images/pine64.scm} par exemple.")
         (pt "O comando @command{guix system image} pode agora operar em
registros @code{image}.  Isto significa que o parâmetro ou expressão de
arquivo passado para esse comando pode retornar registros @code{image} ou
@code{operating-system}.

O diretório @file{gnu/system/images} contém imagens padrão que podem ser
construídas executando @command{guix system image
gnu/system/images/pine64.scm} por exemplo.")))

 (entry (commit "aa8de806252e3835d57fab351b02d13db762deac")
        (title
         (en "Risk of local privilege escalation @i{via} setuid programs")
         (de "Risiko lokaler Rechteausweitung bei setuid-Programmen")
         (fr "Risque de gain local de privilèges @i{via} les programmes setuid")
         (pt "Risco de escalação local de privilégios @i{via} programas setuid")
         (zh "存在通过 setuid 程序进行本地提权的风险"))
        (body
         (en "On Guix System, setuid programs were, until now, installed as
setuid-root @emph{and} setgid-root (in the @file{/run/setuid-programs}
directory).  However, most of these programs are meant to run as setuid-root,
but not setgid-root.  Thus, this setting posed a risk of local privilege
escalation.

This bug has been fixed and users are advised to upgrade their system, with a
command along the lines of:

@example
guix system reconfigure /run/current-system/configuration.scm
@end example

Users of Guix on a ``foreign distro'' are unaffected.  See
@url{https://issues.guix.gnu.org/46395} for more information.")
         (de "Auf Guix System wurden setuid-Programme bisher mit setuid-root
@emph{und} setgid-root ausgestattet (im Verzeichnis
@file{/run/setuid-programs}).  Die meisten solchen Programme sind jedoch nur
dafür gedacht, mit setuid-root zu laufen, ohne setgid-root.  Durch diese
Einstellung war daher vielleicht eine lokale Rechteausweitung („local
privilege escalation“) möglich.

Dieser Fehler wurde behoben und Benutzern wird geraten, ihr System zu
aktualisieren, etwa mit diesem Befehl:

@example
guix system reconfigure /run/current-system/configuration.scm
@end example

Benutzer von Guix auf einer „Fremddistribution“ sind @emph{nicht} betroffen.
Siehe @url{https://issues.guix.gnu.org/46395} für weitere Informationen.")
         (fr "Sur Guix System, les programmes setuid étaient jusqu'à présent
installés setuid-root @emph{et} setgid-root (dans le répertoire
@file{/run/setuid-programs}).  Ces programmes sont généralement conçus pour
être setuid-root, mais pas setgid-root, et cette situation posait donc un
risque de gain local de privilèges.

Ce problème est corrigé et vous êtes encouragé·e à mettre à jour votre
système, avec une commande de ce genre :

@example
guix system reconfigure /run/current-system/configuration.scm
@end example

Les usagers de Guix sur une distrib externe ne sont pas touché·es.  Plus
d'informations sont disponibles à @url{https://issues.guix.gnu.org/46395} (en
anglais).")
         (pt "No Sistema Guix, programas setuid programs eram, até agora,
instalador como setuid-root @emph{e} setgid-root (no diretório
@file{/run/setuid-programs}).  Porém, a maioria destes programas é feita para
ser executada como setuid-root, mas não como setgid-root.  Portanto, esta
configuração impõe um risco de escalação local de privilégios.

Este erro foi consertado e os usuários são recomendados a atualizar seus
sistemas, com um comando nas linhas de:

@example
guix system reconfigure /run/current-system/configuration.scm
@end example

Usuários de Guix em uma distribuição estrangeira não são afetados. Veja
@url{https://issues.guix.gnu.org/46395} para mais informação.")
         (zh "到目前为止，Guix 系统上的 setuid 程序（位于 @file{/run/setuid-programs}）
同时具有 setuid-root @emph{和} setgid-root 权限。然而，此类程序大多被设计为在拥有
setuid 权限而非 setgid 权限时运行。因此，这样的设置可能会使系统受到本地提权攻击。

此漏洞已经被修复，同时建议用户使用下列命令升级他们的系统：

@example
guix system reconfigure /run/current-system/configuration.scm
@end example

在 ``第三方宿主系统'' 上使用 Guix 的用户不受此漏洞影响，详情请参阅
@url{https://issues.guix.gnu.org/46395}。")))

 (entry (commit "aedbc5ff32a62f45aeed74c6833399a6cf2c22dc")
        (title
         (en "Create a manifest with @command{guix package --export-manifest}")
         (de "Manifest erzeugen mit @command{guix package --export-manifest}")
         (fr "Créer un manifeste avec @command{guix package --export-manifest}")
         (pt "Crie um manifesto com @command{guix package --export-manifest}"))
        (body
         (en "The @command{guix package --export-manifest} command outputs a
@dfn{manifest} from your profile.  This manifest is a code snippet that can
then be passed to @command{guix package --manifest} (or any other command that
accepts the @option{--manifest} option) to deploy these packages.

The goal of this new @option{--export-manifest} option is to make it easier to
migrate from an ``imperative'' style where you repeatedly invoke @command{guix
install} and similar commands, to the declarative style where you write in a
manifest file the list of packages you want to have.

Similarly, the new @option{--export-channels} option outputs a @dfn{channel
specification} suitable for @command{guix pull --channels} from your profile.
This allows you to ``pin'' Guix to the revision that was used to build the
profile.

Run @command{info \"(guix) Invoking guix package\"} for more info.")
         (de "Mit dem Befehl @command{guix package --export-manifest} wird ein
@dfn{Manifest} aus Ihrem Profil erzeugt. Bei einem Manifest handelt es sich um
ein Stück Code, das Sie an @command{guix package --manifest} zum Einspielen
der Pakete aus dem Manifest übergeben können (oder an jeden anderen Befehl,
der die Befehlszeilenoption @option{--manifest} versteht).

Die Absicht hinter dieser neuen Befehlszeilenoption @option{--export-manifest}
ist, dass man leichter von einem „imperativen“ Stil, bei dem man wiederholt
@command{guix install} und ähnliche Befehle aufruft, zum deklarativen Stil
wechseln kann. Im deklarativen Stil tragen Sie die Liste der Pakete, die Sie
haben möchten, in eine Manifest-Datei ein.

Analog können Sie mit der neuen Befehlszeilenoption @option{--export-channels}
zu Ihrem Profil eine @dfn{Kanalspezifikation} erzeugen, die für @command{guix
pull --channels} geeignet ist.  Damit können Sie für Guix immer die Version
benutzen, mit der das Profil erstellt wurde.

Führen Sie für mehr Informationen @command{info \"(guix.de) Aufruf von guix
package\"} aus.")
         (fr "La commande @command{guix package --export-manifest} affiche un
@dfn{manifeste} pour le profil choisi.  Ce manifeste est un bout de code qu'on
peut passer à @command{guix package --manifest} (ou n'importe qu'elle commande
qui accepte l'option @option{--manifest}) pour déployer ces paquets.

L'objectif de cette nouvelle option @option{--export-manifest} est de
faciliter la migration du modèle ``impératif'', où on utilise @command{guix
install} et les commandes de ce genre, au modèle déclaratif où on écrit dans
un fichier la liste des paquets que l'on veut avoir.

De même, la nouvelle option @option{--export-channels} produit une
@dfn{spécification de canaux} pour @command{guix pull --channels} à partir du
profil.  Cela permet de ``figer'' Guix à la révision qui a été utilisée pour
produire le profil.

Voir @command{info \"(guix.fr) Invoquer guix package\"} pour plus
d'informations.")
         (pt "O comando @command{guix package --export-manifest} solta um
@dfn{manifesto} a partir do seu perfil.  Este manifesto é um bloco de código
que pode então ser passado para @command{guix package --manifest} (ou
qualquer outro comando que aceita a opção @option{--manifest}) para implantar
estes pacotes.

O objetivo desta nova opção @option{--export-manifest} é tornar mais fácil a
migração de um estilo ``imperative'' onde você repetidamente invoca comandos
@command{guix install} e similares, para um estilo declarativo onde você
escreve a lista de pacotes que você quer em um arquivo manifesto.

Semelhantemente, a nova opção @option{--export-channels} solta uma
@dfn{especificação de canal} adequada para @command{guix pull --channels} a
partir do seu perfil.  Isto te permite  ``afixar'' (@i{pin}) o Guix para a
revisão que foi usada para construir o perfil.

Execute @command{info \"(guix.pt_BR) Invocando guix package\"} para mais
info.")))

 (entry (commit "9ab817b2a4601b4a6755983590ed7d93ebdc8d09")
        (title (en "New @option{--with-latest} package transformation option")
               (de "Neue Paketumwandlungsoption @option{--with-latest}")
               (fr "Nouvelle option de transformation @option{--with-latest}")
               (pt "Nova opção de transformação de pacote @option{--with-latest}"))
        (body
         (en "The new @option{--with-latest} package transformation option
gets the latest release of a package, as would be identified by @command{guix
refresh}, and uses it instead of the currently-packaged version.  For example,
to install the latest release of GNOME Weather linked against the latest
version of libgweather, run:

@example
guix install gnome-weather \\
  --with-latest=gnome-weather --with-latest=libgweather
@end example

Run @command{info \"(guix) Package Transformation Options\"} for more info.")
         (de "Mit der neuen Paketumwandlungsoption @option{--with-latest} wird
die neueste Veröffentlichung für ein Paket verwendet.  Diese wird wie bei
@command{guix refresh} bestimmt und anstelle der zurzeit im Paket festgelegten
Version verwendet.  Um zum Beispiel die neuste Veröffentlichung von GNOME
Weather gebunden an die neuste Version von libgweather zu installieren, führen
Sie dies aus:

@example
guix install gnome-weather \\
  --with-latest=gnome-weather --with-latest=libgweather
@end example

Führen Sie für mehr Informationen @command{info \"(guix.de)
Paketumwandlungsoptionen\"} aus.")
         (fr "La nouvelle option de transformation de paquets
@option{--with-latest} récupère la dernière version d'un logiciel telle
qu'elle serait trouvée par @command{guix refresh} et l'utilise à la place la
version actuellement fournie par le paquet.  Par exemple, pour installer la
dernière version de GNOME Weather, elle-même compilée avec la dernière version
de libgweather, on lancera :

@example
guix install gnome-weather \\
  --with-latest=gnome-weather --with-latest=libgweather
@end example

Voir @command{info \"(guix.fr) Options de transformation de paquets\"} pour
plus de détails.")
         (pt "A nova opção de transformação de pacote @option{--with-latest}
obtém o lançamento mais recente de um pacote, como seria identificado por
@command{guix refresh}, e o usa em vez da versão empacotada atualmente.  Por
exemplo, para instalar o lançamento mais recente de GNOME Weather ligado
contra a versão mais recente de libgweather, execute:

@example
guix install gnome-weather \\
  --with-latest=gnome-weather --with-latest=libgweather
@end example

Execute @command{info \"(guix.pt_BR) Opções de transformação de pacote\"} para
mais info.")))

 (entry (commit "a879e35116043d5daf3d9d175b697d10b9177fd5")
        (title (en "Substitutes can now be compressed with zstd")
               (de "Substitute können nun mit zstd komprimiert werden")
               (fr "Les substituts peuvent maintenant être compressés avec zstd")
               (pt "Substitutos já podem ser comprimidos com zstd"))
        (body
         (en "The @command{guix publish} command now supports substitute
compression with zstd and @command{guix-daemon} can now fetch and decompress
them.

The advantage of zstd over the other options is its high compression and
decompression throughput, with good compression ratios (not as good as lzip,
but slightly better than gzip).  Its high decompression throughput makes it a
good choice in situations where substitute downloads would otherwise be
CPU-bound, typically when having a high-speed connection to the substitute
server.  Run @command{info \"(guix) Invoking guix publish\"} for more info.

To be able to fetch zstd-compressed substitutes (if the substitute servers you
chose provide them), you need to upgrade your daemon.  Run @command{info
\"(guix) Upgrading Guix\"} to learn how to do it.")
         (de "Mit dem Befehl @command{guix publish} können Sie jetzt auch
Substitute mit zstd komprimieren und @command{guix-daemon} kann sie laden und
dekomprimieren.

zstd bietet gegenüber den anderen Optionen einen hohen Durchsatz bei
Kompression und Dekompression mit guten Kompressionsverhältnissen (nicht so
gut wie lzip, aber etwas besser als gzip).  Wegen des hohen Durchsatzes bei
der Dekompression ist zstd eine gute Wahl, wenn beim Herunterladen von
Substituten ansonsten der Engpass bei der Prozessorleistung läge, etwa weil
eine schnelle Netzwerkverbindung zum Substitutserver besteht.  Führen Sie für
mehr Informationen @command{info \"(guix.de) Aufruf von guix publish\"} aus.

Um zstd-komprimierte Substitute benutzen zu können (wenn der Substitutserver
sie anbietet), müssen Sie Ihren Daemon aktualisieren.  Führen Sie
@command{info \"(guix.de) Aktualisieren von Guix\"} aus, um zu erfahren, wie
Sie ihn aktualisieren.")
         (fr "La commande @command{guix publish} peut maintenant compresser
les substituts avec zstd et @command{guix-daemon} est capable de les récupérer
et de les décompresser.

L'avantage de zstd par rapport aux autres méthodes est son haut débit en
compression et décompression, avec un taux de compression correct (pas aussi
bon que lzip, mais légèrement meilleur que gzip).  Sa vitesse de décompression
en fait un bon choix dans les situations où le temps de téléchargement des
substituts se retrouve sinon limité par le temps de calcul comme c'est le cas
lorsqu'on bénéficie d'une connexion rapide au serveur de substitut.  Lancer
@command{info \"(guix.fr) Invoquer guix publish\"} pour plus d'informations.

Pour pouvoir télécharger des substituts compressés avec zstd (si les serveurs
de substituts choisis les fournissent), il faudra d'abord mettre à jour le
démon.  Lancer @command{info \"(guix.fr) Mettre à niveau Guix\"} pour voir
comment faire.")
         (pt "O comando @command{guix publish} agora suporta compressão de
substitutos com zstd e @command{guix-daemon} agora pode buscá-los e
descomprimi-los.

As vantagens do zstd sobre outras opções são sua suas altas taxas de
transferência de compressão e descompressão, com boas taxas de compressão (não
tão boas quanto lzip, mas levemente melhores que gzip).  Sua alta taxa de
transferência de descompressão o torna uma boa escolha em situações onde as
baixações de substitutos seriam de outra forma limitados pelo poder de
processamento, tipicamente quando se tem uma conexão de alta velocidade ao
servidor de substitutos. Execute @command{info \"(guix.pt_BR) Invocando guix
publish\"} para mais info.

A fim de poder buscar substitutos comprimidos com zstd (se os servidores de
substitutos que você escolheu os fornecer), você precisa atualizar seu daemon.
Execute @command{info \"(guix.pt_BR) Atualizando o Guix\"} para aprender como
fazer isso.")))

 (entry (commit "e38d90d497e19e00263fa28961c688a433154386")
        (title (en "New @option{--with-patch} package transformation option")
               (de "Neue Paketumwandlungsoption @option{--with-patch}")
               (fr "Nouvelle option de transformation @option{--with-patch}")
               (pt "Nova opção de transformação de pacotes @option{--with-patch}"))
        (body
         (en "The new @option{--with-patch} package transformation option
applies patches to the specified packages before building them.  The example
below builds the GNU Core Utilities against a patched C library (glibc):

@example
guix build coreutils --with-patch=glibc=./glibc-frob.patch
@end example

Run @command{info \"(guix) Package Transformation Options\"} for more info.")
         (de "Die neue Paketumwandlungsoption @option{--with-patch} wendet
Patches auf die angegebenen Pakete an, bevor sie erstellt werden. Das folgende
Beispiel lässt die GNU Core Utilities mit einer gepatchten
C-Bibliothek (glibc) erstellen:

@example
guix build coreutils --with-patch=glibc=./glibc-frob.patch
@end example

Führen Sie für mehr Informationen @command{info \"(guix.de)
Paketumwandlungsoptionen\"} aus.")
         (fr "La nouvelle option de transformation de paquets
@option{--with-patch} applique des modifications (@i{patches}) aux paquets
spécifiés avant de les compiler.  L'exemple suivant compile les utilitaires de
base GNU avec une bibliothèque C (glibc) modifiée :

@example
guix build coreutils --with-patch=glibc=./glibc-frob.patch
@end example

Voir @command{info \"(guix.fr) Options de transformation de paquets\"} pour
plus de détails.")
         (pt "A nova opção de transformação de pacote @option{--with-patch}
aplica emendas (@i{patches}) para os pacotes especificados antes de
construí-los.  O exemplo abaixo constrói as Utilidades Essenciais GNU (@i{GNU
Core Utilities}) contra uma biblioteca C emendada (glibc):

@example
guix build coreutils --with-patch=glibc=./glibc-frob.patch
@end example

Execute @command{info \"(guix.pt_BR) Opções de transformação de pacote\"} para
mais info.")))

 (entry (commit "79f9dee3c4c0e6d21066f142116a537207ae7ba4")
        (title (en "Local substitute servers discovery is now supported")
               (de "Substitutserver können jetzt im lokalen Netz erkannt werden")
               (es "Los servidores de sustituciones se pueden descubrir localmente")
               (fr "La découverte des serveurs de substituts locaux est désormais supportée")
               (pt "Descoberta de servidores de substitutos locais já é suportada"))
        (body
         (en "The @command{guix-daemon} can now discover local substitute
servers when the @option{--discover} option is passed.  Only the substitute
servers started with the @option{--advertise} option will be discovered.  The
network discovery is based on mDNS and DNS-SD protocols, using Guile-Avahi
library for now.")
         (de "Mit dem @command{guix-daemon} können jetzt lokal laufende
Substitutserver erkannt werden, wenn die Befehlszeilenoption
@option{--discover} übergeben wurde.  Nur solche Substitutserver werden
gefunden, die mit der Befehlszeilenoption @option{--advertise} gestartet
wurden.  Die Ermittlung im Netzwerk verfügbarer Substitutserver baut auf den
Protokollen mDNS und DNS-SD auf.  Derzeit wird dazu die Bibliothek Guile-Avahi
benutzt.")
         (es "El daemon @command{guix-daemon} ahora puede descubrir servidores
de sustituciones locales cuando se le proporciona la opción
@option{--discover}.  Únicamente se descubrirán los servidores de
sustituciones que se hayan arrancado con la opción @option{--advertise}. La
búsqueda en la red se basa en los protocolos mDNS y DNS-SD, actualmente
mediante el uso de la biblioteca Guile-Avahi.")
         (fr "Le @command{guix-daemon} peut désormais découvrir les serveurs
de substituts locaux lorsque l'option @option{--discover} est passée.  Seuls
les serveurs de substituts démarrés avec l'option @option{--advertise} seront
découverts.  La découverte réseau utilise les protocoles mDNS et DNS-SD, pour
l'instant grâce à la librairie Guile-Avahi.")
         (pt "O @command{guix-daemon} agora pode descobrir servidores de
substitutos locais quando a opção @option{--discover} é passada.  Somente os
servidores de substitutos iniciados com a opção @option{--advertise} serão
descobertos.  A descoberta de rede é baseada nos protocolos mDNS e DNS-SD,
usando a biblioteca Guile-Avahi por ora.")))

 (entry (commit "a9a2fdaabcc78e7a54d9a6bcfa4ee3de308e9a90")
        (title (en "Logical Volume Manager (LVM) now supported on Guix System")
               (de "Logical Volume Manager (LVM) wird jetzt auf Guix System unterstützt")
               (es "El sistema Guix ahora implementa también volúmenes lógicos LVM")
               (fr "Le gestionnaire de volumes logiques (LVM) est maintenant pris en charge par le système Guix")
               (pt "@acronym{LVM, Logical Volume Manager} já é suportado no Sistema Guix"))
        (body
         (en "On Guix System, the new @code{lvm-device-mapping} variable
allows you to declare ``mapped devices'' for LVM, the Linux Logical Volume
Manager.  For example, LVM logical volumes ``alpha'' and ``beta'' from volume
group ``vg0'' can be declared as follows:

@lisp
(mapped-device
  (source \"vg0\")
  (target (list \"vg0-alpha\" \"vg0-beta\"))
  (type lvm-device-mapping))
@end lisp

See @command{info \"(guix) Mapped Devices\"} for more information.")
         (de "Auf Guix System erlaubt Ihnen die neue Variable
@code{lvm-device-mapping}, „zugeordnete Geräte“ (Mapped Devices) für LVM, den
Linux Logical Volume Manager, zu deklarieren. Zum Beispiel können logische
Datenträger von LVM namens „alpha“ und „beta“ aus der
Datenträgergruppe (Volume Group) „vg0“ wie folgt deklariert werden:

@lisp
(mapped-device
  (source \"vg0\")
  (target (list \"vg0-alpha\" \"vg0-beta\"))
  (type lvm-device-mapping))
@end lisp

Siehe @command{info \"(guix.de) Zugeordnete Geräte\"} für nähere Informationen.")
         (es "En el sistema Guix, la nueva variable @code{lvm-device-mapping}
le permite declarar «dispositivos traducidos» para LVM, el gestor de volúmenes
lógicos de Linux. A continuación se muestra un ejemplo con la declaración de
los volúmenes lógicos «alfa» y «beta» del grupo de volúmenes «vg0»:

@lisp
(mapped-device
  (source \"vg0\")
  (target (list \"vg0-alfa\" \"vg0-beta\"))
  (type lvm-device-mapping))
@end lisp

Véase @command{info \"(guix.es) Dispositivos traducidos\"} para obtener más
información.")
         (fr "Sur le système Guix, la nouvelle variable @code{lvm-device-mapping}
vous permet de déclarer des « périphériques mappés » pour LVM, le gestionnaire
de volumes logiques.  Par exemple, vous pouvez déclarer les volumes logiques
« alpha » et « beta » du groupe « vg0 » comme ceci :

@lisp
(mapped-device
  (source \"vg0\")
  (target (list \"vg0-alpha\" \"vg0-beta\"))
  (type lvm-device-mapping))
@end lisp

Voir @command{info \"(guix.fr) Périphériques mappés\"} pour en savoir plus.")
         (pt "No Sistema Guix, a nova variável @code{lvm-device-mapping} te
permite declarar ``dispositivos mapeados'' para o LVM, o Gerenciador de
Volumes Lógicos do Linux.  Por exemmplo, volumes lógicos LVM  ``alpha'' e
``beta'' do grupo de volumes ``vg0'' podem ser declarados como se segue:

@lisp
(mapped-device
  (source \"vg0\")
  (target (list \"vg0-alpha\" \"vg0-beta\"))
  (type lvm-device-mapping))
@end lisp

Veja @command{info \"(guix.pt_BR) Dispositivos mapeados\"} para mais
informação.")))

 (entry (commit "3b6e4e5fd05e72b8a32ff1a2d5e21464260e21e6")
        (title (en "List of substitute keys is now declarative on Guix System")
               (de "Liste der Substitutschlüssel auf Guix System ist jetzt deklarativ")
               (es "Claves para sustituciones del sistema Guix en formato declarativo")
               (fr "Liste des clefs de substituts désormais déclarative sur Guix System")
               (pt "Lista de chaves de substitutos agora é declarativa no Sistema Guix"))
        (body
         (en "The list of authorized substitute keys, available in
@file{/etc/guix/acl}, is now built by default in a purely declarative fashion
on Guix System based on the @code{authorized-keys} field of the configuration
of @code{guix-service-type}.  This means that manual changes to
@file{/etc/guix/acl} are now @emph{discarded} upon reconfiguration or
reboot (a backup is made as @file{/etc/guix/acl.bak} in that case).

We recommend updating your operating system configuration to explicitly list
all the authorized substitute keys.  See @command{info \"(guix) Base
Services\"}, for more info about @code{guix-configuration} and
@code{authorized-keys}.

Alternatively, you can set the @code{authorize-key?} field of
@code{guix-configuration} to @code{#f} to restore previous behavior.")
         (de "Die Liste von autorisierten Substitutschlüsseln, die in
@file{/etc/guix/acl} steht, wird auf Guix System nach Vorgabe jetzt auf rein
deklarative Weise erstellt, je nach Inhalt des @code{authorized-keys}-Feldes
der Konfiguration des @code{guix-service-type}. Das hat zur Folge, dass
manuelle Änderungen an @file{/etc/guix/acl} von jetzt an nach jedem
Rekonfigurieren oder Neustarten @emph{verworfen} werden (in diesem Fall wird
eine Sicherheitskopie namens @file{/etc/guix/acl.bak} angelegt).

Wir empfehlen, dass Sie Ihre Betriebssystemkonfiguration aktualisieren, damit
dort alle autorisierten Substitutschlüssel ausdrücklich aufgeführt
werden. Siehe @command{info \"(guix.de) Basisdienste\"} für mehr Informationen
zur @code{guix-configuration} und @code{authorized-keys}.

Alternativ können Sie das @code{authorize-key?}-Feld der
@code{guix-configuration} auf @code{#f} setzen, um zum alten Verhalten
zurückzugehen.")
         (es "El listado de claves autorizadas para la obtención de
sustituciones, disponible en @file{/etc/guix/acl}, ahora se genera de manera
predeterminada en el sistema Guix de forma completamente declarativa en base
al campo @code{authorized-keys} del la configuración para el servicio
@code{guix-service-type}. Esto significa que los cambios que se hayan
realizado de manera manual en @file{/etc/guix/acl} @emph{se descartan} tras
una reconfiguración del sistema o tras un reinicio (se realiza una copia de
seguridad en la ruta @file{/etc/guix/acl.bak} en este caso).

Le recomendamos que actualice su configuración del sistema operativo para que
enumere explícitamente todas las claves que desea autorizar para la obtención
de sustituciones.  Véase @command{info \"(guix.es) Servicios base\"}, para
obtener más información sobre @code{guix-configuration} y
@code{authorized-keys}.

También puede proporcionar el valor @code{#f} en el campo
@code{authorize-key?} de @code{guix-configuration} para volver al
comportamiento que se obtenía con versiones previas.")
         (fr "La liste des clefs de substituts autorisées, stockée dans
@file{/guix/guix/acl}, est dorénavant construite par défaut de manière
déclarative sur Guix System, en se basant sur le champs @code{authorized-keys}
de la configuration de @code{guix-service-type}.  Cela signifie que les
modifications apportées manuellement à @file{/etc/guix/acl} seront désormais
@emph{perdues} lors d'une reconfiguration ou d'un redémarrage (dans ce cas une
sauvegarde est faite dans @file{/etc/guix/acl.bak}).

Nous recommandons de mettre à jour sa configuration de système d'exploitation
pour y lister explicitement les clefs autorisées.  Lancez @command{info
\"(guix.fr) Services de base\"} pour plus d'informations sur
@code{guix-configuration} et @code{authorized-keys}.

Il est également possible de mettre le champs @code{authorize-key?} de
@code{guix-configuration} à @code{#f} pour restaurer le comportement qui
prévalait jusqu'à maintenant.")
         (pt "A lista de chaves autorizadas de substitutos, disponível em
@file{/etc/guix/acl}, agora é por padrão construída de uma maneira puramente
declarativa no Sistema Guix baseado no campo @code{authorized-keys} da
configuração do @code{guix-service-type}.  Isto significa que as mudanças
manuais em @file{/etc/guix/acl} agora são @emph{descartadas} em cada
reconfiguração ou reinício (uma cópia de segurança é feita em
@file{/etc/guix/acl.bak} neste caso).

Te recomendamos atualizar a sua configuração de sistema operacional para
listar explicitamente todas as chaves autorizadas de substitutos.  Veja
@command{info \"(guix.pt_BR) Serviços básicos\"} para mais info sobre
@code{guix-configuration} e @code{authorized-keys}.

Alternativamente, você pode definir o campo @code{authorize-key?} de
@code{guix-configuration} para @code{#f} a fim de restaurar o comportamento
anterior.")))

 (entry (commit "6aeda81602555fbeac0c0a209e74f5262093b513")
        (title (en "New @option{--with-debug-info} package transformation option")
               (de "Neue Paketumwandlungsoption @option{--with-debug-info}")
               (es "Nueva opción de transformación @option{--with-debug-info}")
               (fr "Nouvelle option de transformation @option{--with-debug-info}")
               (pt "Nova opção de transformação de pacote @option{--with-debug-info}"))
        (body
         (en "The new @option{--with-debug-info} option builds a variant of a
package that includes debug info and grafts it onto the application you want
to debug.  Thus, only the package for which you want debug info needs to be
recompiled.  This is useful for packages that do not already have a
@code{debug} output.

For example, here is how you would obtain debug info for the @code{glib}
library so you can inspect it while debugging Inkscape:

@example
guix build --with-debug-info=glib inkscape
@end example

Run @command{info \"(guix) Package Transformation Options\"} for more info.")
         (de "Die neue Paketumwandlungsoption @option{--with-debug-info} lässt
eine Variante eines Pakets erstellen, die auch Informationen zur Fehlersuche
enthält. Damit wird die Anwendung veredelt, wo Sie Fehler nachvollziehen
möchten. Somit muss nur das Paket, für das Sie die Informationen brauchen, neu
kompiliert werden. Das ist hilfreich bei Paketen, die noch nicht über eine
@code{debug}-Ausgabe verfügen.

Zum Beispiel würden Sie so Informationen zur Fehlersuche für die
@code{glib}-Bibliothek bekommen, um sie inspizieren zu können, wenn Sie Fehler
in Inkscape nachvollziehen möchten:

@example
guix build --with-debug-info=glib inkscape
@end example

Führen Sie für mehr Informationen @command{info \"(guix.de)
Paketumwandlungsoptionen\"} aus.")
         (es "La nueva opción @option{--with-debug-info} construye una
variante del paquete que incluye la información de depuración y la injerta
en la aplicación que desee depurar.  Por tanto, únicamente el paquete del
que desee información de depuración debe construirse de nuevo.  Es útil
para paquetes que no tienen ya una salida @code{debug}.

El siguiente ejemplo muestra como obtener información de depuración
para la biblioteca @code{glib} de modo que pueda inspeccionarla mientras
depura Inkscape:

@example
guix build --with-debug-info=glib inkscape
@end example

Ejecute @command{info \"(guix.es) Opciones de transformación de paquetes\"}
para obtener más información.")
         (fr "La nouvelle option de transformation de paquets
@option{--with-debug-info} compile une variante d'un paquet avec les
informations de déboguage et la greffe sur l'application que l'on veut
déboguer.  Ainsi seul le paquet pour lequel on demande des informations de
déboguage a besoin d'être recompilé.  C'est utile pour les paquets n'ayant pas
déjà un résultat @code{debug}.

Voici par exemple comment obtenir des informations de déboguage pour la
bibliothèque @code{glib} de manière à pouvoir l'inspecter quand on débuggue
Inkscape :

@example
guix build --with-debug-info=glib inkscape
@end example

Voir @command{info \"(guix.fr) Options de transformation de paquets\"} pour
plus de détails.")
         (pt "A nova opção @option{--with-debug-info} constrói uma variante do
pacote que inclui informação de depuração e a enxerta na aplicação que você
deseja depurar.  Portanto, somente o pacote para o qual você quer informação
de depuração precisa ser recompilado.  Isto é útil para pacotes que não tenham
ainda uma saída @code{debug}.

Por exemplo, eis como você pode obter informação de depuração para a
biblioteca @code{glib} de forma que possa inspecioná-la enquanto depura o
Inkscape:

@example
guix build --with-debug-info=glib inkscape
@end example

Execute @command{info \"(guix.pt_BR) Opções de transformação de pacote\"} para
mais info.")))

 (entry (commit "abd7a474615353149a44f4504f0b4b248dcc0716")
        (title (en "New @option{--with-c-toolchain} package transformation option")
               (de "Neue Paketumwandlungsoption @option{--with-c-toolchain}")
               (es "Nueva opción de transformación @option{--with-c-toolchain}")
               (fr "Nouvelle option de transformation @option{--with-c-toolchain}")
               (pt "Nova opção de transformação de pacote @option{--with-c-toolchain}"))
        (body
         (en "The new @option{--with-c-toolchain} package transformation
options provides an easy way for developers to rebuild their favorite packages
with the C/C++ tool chain of their choice instead of the default one.

For example, the following command rebuilds the @code{fftw} and @code{fftwf}
packages as well as every package that depends on them, up to and including
@code{octave-cli}, using GCC version 10 (currently GCC 7.5 is used by
default):

@example
guix build octave-cli \\
  --with-c-toolchain=fftw=gcc-toolchain@@10 \\
  --with-c-toolchain=fftwf=gcc-toolchain@@10
@end example

Run @command{info \"(guix) Package Transformation Options\"} for more info.")
         (de "Die neue Paketumwandlungsoption @option{--with-c-toolchain}
bietet Entwicklern die Möglichkeit, leicht ihre Lieblingspakete mit der
selbstgewählten Toolchain für C/C++ anstelle der vorgegebenen neu zu
erstellen.

Zum Beispiel werden mit folgendem Befehl die Pakete @code{fftw} und
@code{fftwf} sowie alle davon abhängigen Pakete bis einschließlich
@code{octave-cli} mit Version 10 der GCC erstellt (vorgegeben wäre zurzeit,
GCC 7.5 zu benutzen):

@example
guix build octave-cli \\
  --with-c-toolchain=fftw=gcc-toolchain@@10 \\
  --with-c-toolchain=fftwf=gcc-toolchain@@10
@end example

Führen Sie für mehr Informationen @command{info \"(guix.de)
Paketumwandlungsoptionen\"} aus.")
         (es "La nueva opción de transformación de paquetes
@option{--with-c-toolchain} proporciona a las desarrolladoras una manera
fácil de reconstruir sus paquetes favoritos con la cadena de herramientas
de compilación de C/C++ que elijan en vez de la predeterminada.

Por ejemplo, la siguiente orden reconstruye los paquetes @code{fftw} y
@code{fftwf} así como todos los paquetes que dependen de ellos hasta
@code{octave-cli}, usando la versión 10 de GCC (el compilador
predeterminado en estos momentos es GCC 7.5):

@example
guix build octave-cli \\
  --with-c-toolchain=fftw=gcc-toolchain@@10 \\
  --with-c-toolchain=fftwf=gcc-toolchain@@10
@end example

Ejecute @command{info \"(guix.es) Opciones de transformación de paquetes\"}
para obtener más información.")
         (fr "La nouvelle option de transformation de paquets
@option{--with-c-toolchain} permet aux développeur·euses de recompiler leurs
paquets préférés avec la chaîne d'outils C/C++ de leur choix à la place de
celle par défaut.

Par exemple, la commande ci-dessous recompile @code{fftw}, @code{fftwf} et
tous les paquets qui en dépendent, jusqu'à @code{octave-cli} inclus, avec GCC
10 (actuellement c'est GCC 7.5 qui est utilisé par défaut):

@example
guix build octave-cli \\
  --with-c-toolchain=fftw=gcc-toolchain@@10 \\
  --with-c-toolchain=fftwf=gcc-toolchain@@10
@end example

Voir @command{info \"(guix.fr) Options de transformation de paquets\"} pour
plus de détails.")
         (pt "A nova opção de transformação de pacote
@option{--with-c-toolchain} fornece uma maneira fácil para que desenvolvedores
reconstruam seus pacotes favoritos com a cadeia de ferramentas C/C++ de sua
escolha em vez da padrão.

Por exemplo, o comando a seguir reconstrói os pacotes @code{fftw} e
@code{fftwf} bem como cada pacote que deles dependa, até @code{octave-cli}
inclusive, usando a versão 10 do GCC (correntemente GCC 7.5 é usado por
padrão):

@example
guix build octave-cli \\
  --with-c-toolchain=fftw=gcc-toolchain@@10 \\
  --with-c-toolchain=fftwf=gcc-toolchain@@10
@end example

Execute @command{info \"(guix.pt_BR) Opções de transformação de pacote\"} para
mais info.")))

 (entry (commit "8e1907a72430aa989125b053573ef0897c480697")
        (title (en "Package transformation options now recorded in profiles")
               (es "Las opciones de transformación de paquetes ahora se
quedan registradas en el perfil")
               (de "Paketumwandlungsoptionen werden nun in Profilen gesichert")
               (fr "Options de transformation sauvegardées dans les profils")
               (pt "Opções de transformação de pacotes agora salvas nos perfis"))
        (body
         (en "When installing packages in a profile, package transformation
options such as @option{--with-input} are now recorded in the profile.  When
you eventually run @command{guix upgrade}, those transformations will be
automatically applied to the upgraded packages.

Run @command{info \"(guix) Package Transformation Options\"} for more info.")
         (es "Si durante la instalación de paquetes en un perfil se utilizaron
opciones de transformación de paquetes, como por ejemplo
@option{--with-input}, éstas se registran en el perfil. Cuando vuelva a
ejecutar @command{guix upgrade}, dichas transformaciones se aplicarán
automáticamente a los paquetes actualizados.

Ejecute @command{info \"(guix.es) Opciones de transformación de paquetes\"}
para obtener más información.")
         (de "Wenn Sie ein Paket in ein Profil installieren, werden nun
Paketumwandlungsoptionen wie @option{--with-input} im Profil gespeichert.
Sobald Sie später @command{guix upgrade} ausführen, werden dieselben
Umwandlungen automatisch auf die aktualisierten Pakete angewandt.

Führen Sie für mehr Informationen @command{info \"(guix.de)
Paketumwandlungsoptionen\"} aus.")
         (fr "Lorsqu'on installe des paquets dans un profil, les options de
transformation telles que @option{--with-input} sont désormais enregistrées
dans le profil.  Quand on le met plus tard à jour avec @command{guix upgrade},
ces transformations sont automatiquement appliquées aux nouveaux paquets.

Voir @command{info \"(guix.fr) Options de transformation de paquets\"} pour
plus de détails.")
         (pt "Ao instalar um pacote em um perfil, opções de transformação de
pacote tais como @option{--with-input} agora são salvas no perfil.  Quando
você eventualmente executar @command{guix upgrade}, estas transformações serão
automaticamente aplicadas aos pacotes atualizados.

Execute @command{info \"(guix.pt_BR) Opções de transformação de pacote\"} para
mais info.")))

 (entry (commit "313f492657f1d0863c641fa5ee7f5b7028e27c94")
        (title (en "New @option{--image-type} option for @command{guix system disk-image}.")
               (es "Nueva opción @option{--image-type} para @command{guix system disk-image}.")
               (de "Neue Option @option{--image-type} für @command{guix system disk-image}.")
               (fr "Nouvelle option @option{--image-type} pour @command{guix system disk-image}.")
               (pt "Nova opção @option{--image-type} para @command{guix system disk-image}."))
        (body
         (en "The @option{--file-system-type} option for @command{guix system
disk-image} command has been replaced by the new @option{--image-type} option.
By default, @code{raw} disk images are produced, but @code{hurd-qcow2},
@code{hurd-raw}, @code{uncompressed-iso9660} and @code{iso9660} image types
are also available.

The @option{--list-image-types} option lists all the available image types.")
         (es "La opción @option{--file-system-type} de @command{guix system
disk-image} se ha sustituido por la nueva opción @option{--image-type}.  De
manera predeterminada se producen imágenes en formato crudo (@code{raw}) pero
también están disponibles los tipos de imagen @code{hurd-qcow2},
@code{hurd-raw}, @code{uncompressed-iso9660} y @code{iso9660}.

La opción @option{--list-image-types} muestra una lista con todos los tipos
de imagen disponibles.")
         (de "Anstelle der Befehlszeilenoption @option{--file-system-type} für
@command{guix system disk-image} gibt es nun die neue Option
@option{--image-type}.  In der Vorgabeeinstellung @code{raw} werden rohe
Disk-Images erzeugt, aber es können auch die Abbildtypen @code{hurd-qcow2},
@code{hurd-raw}, @code{uncompressed-iso9660} und @code{iso9660} ausgewählt
werden.

Mit der Option @option{--list-image-types} werden alle verfügbaren Abbildtypen
aufgelistet.")
         (fr "L'option @option{--file-system-type} pour la commande
@command{guix system disk-image} a été remplacée par la nouvelle option
@option{--image-type}. Par défaut, l'option @code{raw}, produisant des images
disque brutes est sélectionnée. Les options @code{hurd-qcow2},
@code{hurd-raw}, @code{uncompressed-iso9660} et @code{iso9660} sont également
disponibles.

La nouvelle option @option{--list-image-types} énumère les types d'image
disponibles.")
         (pt "A opção @option{--file-system-type} para o comando @command{guix
system disk-image} foi substituída pela nova opção @option{--image-type}. Por
padrão, são produzidas imagens de disco @code{raw}, mas os tipos de imagem
@code{hurd-qcow2}, @code{hurd-raw}, @code{uncompressed-iso9660} e
@code{iso9660} também estão disponíveis.

A opção @option{--list-image-types} lista todos os tipos de imagem
disponíveis.")))

 (entry (commit "8819551c8d2a12cd4e84e09b51e434d05a012c9d")
        (title (en "Package transformations now apply to implicit inputs")
               (es "Las transformaciones de paquetes ahora afectan también
a las dependencias implícitas")
               (de "Paketumwandlungen betreffen jetzt auch implizite Eingaben")
               (fr "Les transformations de paquets s'appliquent aux
dépendances implicites")
               (pt "Transformações de pacotes agora se aplicam a entradas implícitas"))
        (body
         (en "Package transformation options such as @option{--with-branch},
@option{--with-input}, and so on now apply to implicit inputs---previously
only a package's explicit inputs would be affected.  This allows for things
such as replacing the Python dependency of a package that uses
@code{python-build-system}:

@example
guix install --with-input=python=python2 python-itsdangerous
@end example

Another example is grafting a different version of the GNU C
Library (@code{glibc} is an implicit input of almost all the packages and is
``deep down'' in the dependency graph):

@example
guix build --with-graft=glibc=glibc@@2.31 hello
@end example

Run @command{info \"(guix) Package Transformation Options\"} for more
info.")
         (es "Las opciones de transformación de paquetes como
@option{--with-branch}, @option{--with-input}, etcétera, ahora también
influyen en las entradas implícitas---antes únicamente las entradas explícitas
del paquete se veían afectadas. Esto permite, por ejemplo, sustituir la
dependencia en python de un paquete que use @code{python-build-system}:

@example
guix install --with-input=python=python2 python-itsdangerous
@end example

Otro ejemplo podría ser el injerto de una versión diferente de la biblioteca
de C de GNU (@code{glibc} es una entrada implícita de casi todos los paquetes
y ``muy abajo'' en el grafo de dependencias):

@example
guix build --with-graft=glibc=glibc@@2.31 hello
@end example

Ejecute @command{info \"(guix.es) Opciones de transformación de paquetes\"}
para obtener más información.")
         (de "Paketumwandlungsoptionen wie @option{--with-branch},
@option{--with-input} und so weiter betreffen nun auch implizite Eingaben —
zuvor haben sie sich nur auf die expliziten Eingaben eines Pakets
ausgewirkt. Dadurch kann jetzt zum Beispiel die Python-Abhängigkeit eines
Pakets, welches @code{python-build-system} benutzt, ersetzt werden:

@example
guix install --with-input=python=python2 python-itsdangerous
@end example

Ein weiteres Beispiel ist, mit einer anderen Version der GNU-C-Bibliothek zu
veredeln (@code{glibc} ist eine implizite Eingabe fast aller Pakete und steckt
„ganz tief“ im Abhängigkeitsgraphen):

@example
guix build --with-graft=glibc=glibc@@2.31 hello
@end example

Führen Sie für mehr Informationen @command{info \"(guix.de)
Paketumwandlungsoptionen\"} aus.")
         (fr "Les options de transformation de paquets telles que
@option{--with-branch} et @option{--with-input} s'appliquent désormais aux
dépendances implicites — jusque là seules les dépendances explicites des
paquets étaient prises en compte.  Cela permet certaines choses telles que
remplacer la dépendance sur Python d'un paquet utilisant
@code{python-build-system} :

@example
guix install --with-input=python=python2 python-itsdangerous
@end example

Un autre exemple est la possibilité de greffer une version différente de la
bibliothèque C GNU (la @code{glibc} est une dépendance implicite de tous les
paquets et se trouve « tout en bas » du graphe de dépendance) :

@example
guix build --with-graft=glibc=glibc@@2.31 hello
@end example

Voir @command{info \"(guix.fr) Options de transformation de paquets\"} pour
plus de détails.")
         (pt "Opções de transformação de pacote, como @option{--with-branch},
@option{--with-input}, entre outras, agora se aplicam a entradas
implícitas---anteriormente somente as entrada explícitas de um pacote seriam
afetadas.  Isto permite coisas como substituir a dependência Python de um
pacote que usa @code{python-build-system}:

@example
guix install --with-input=python=python2 python-itsdangerous
@end example

Outro exemplo é enxertar uma versão diferente da Biblioteca C da GNU
(@code{glibc} é uma dependência implícita de quase todos os pacotee e se situa
``bem fundo'' no grafo de dependências):

@example
guix build --with-graft=glibc=glibc@@2.31 hello
@end example

Execute @command{info \"(guix.pt_BR) Opções de transformação de pacote\"} para
mais info.")))

 (entry (commit "f458cfbcc54ed87b1a87dd9e150ea276f17eab74")
        (title (en "New @option{--without-tests} transformation option")
               (es "Nueva opción de transformación @option{--without-tests}")
               (de "Neue Paketumwandlungsoption @option{--without-tests}")
               (fr "Nouvelle option de transformation @option{--without-tests}")
               (pt "Nova opção de transformação @option{--without-tests}"))
        (body
         (en "The new @option{--without-tests} package transformation option
instructs Guix to skip the test suite of a given package.  In the example
below, @code{guile-gcrypt} is built using a variant of @code{automake} itself
built without running its (lengthy) test suite:

@example
guix build guile-gcrypt --without-tests=automake
@end example

This is primarily useful as a way to speed up development cycles, or to work
around flaky test suites---skipping tests can hide real issues, so use with
care.  Run @command{info \"(guix) Package Transformation Options\"} for more
info.")
         (es "La nueva opción de transformación de paquetes
@option{--without-tests} indica a Guix que omita la batería de pruebas del
paquete proporcionado. En el siguiente ejemplo @code{guile-gcrypt} se
construye usando una variación de @code{automake}, la cual se ha construido
sin ejecutar su (larga) batería de pruebas:

@example
guix build guile-gcrypt --without-tests=automake
@end example

Esto es principalmente útil como una forma de acelerar ciclos de desarrollo o
de omitir temporalmente baterías de pruebas problemáticas---omitir las pruebas
puede ocultar problemas reales, por lo que debe usarse con precaución.
Ejecute @command{info \"(guix.es) Opciones de transformación de paquetes\"}
para obtener más información.")
         (de "Mit der neuen Paketumwandlungsoption @option{--without-tests}
wird Guix angewiesen, den Testkatalog des angegebenen Pakets zu
überspringen. Im folgenden Beispiel wird @code{guile-gcrypt} mit einer
Variante von @code{automake} erstellt, die wiederum ohne Durchlauf ihres (viel
Zeit in Anspruch nehmenden) Testkatalogs erstellt wird:

@example
guix build guile-gcrypt --without-tests=automake
@end example

Der hauptsächliche Nutzen liegt in der Beschleunigung von Entwicklungszyklen
oder im Umgehen unzuverlässiger Testkataloge. Allerdings kann das Überspringen
dazu führen, dass echte Probleme verborgen bleiben. Setzen Sie es mit Bedacht
ein. Führen Sie @command{info \"(guix.de) Paketumwandlungsoptionen\"} aus, um
mehr Informationen zu erhalten.")
         (fr "La nouvelle option de transformation de paquets
@option{--without-tests} demande à Guix de sauter la suite de tests d'un
paquet.  Dans l'exemple ci-dessous, @code{guile-gcrypt} est construit en
utilisant une variante de @code{automake} construite sans lancer sa suite de
tests :

@example
guix build guile-gcrypt --without-tests=automake
@end example

Cette option est surtout intéressante pour raccourcir le cycle de
développement ou pour contourner une suite de tests qui n'est pas
fiable — sauter les tests peut cacher des vrais problèmes, à utiliser avec
précaution donc.  Voir @command{info \"(guix.fr) Options de transformation de
paquets\"} pour plus de détails.")
         (pt "A nova opção de transformação de pacote @option{--without-tests}
instrui Guix a ignorar o conjunto de testes de um dado pacote.  No exemplo
abaixo, @code{guile-gcrypt} é construíudo usando uma variante do
@code{automake} construída sem executar seu (extenso) conjunto de testes:

@example
guix build guile-gcrypt --without-tests=automake
@end example

Isto é primariamente útil como uma maneira de acelerar ciclos de
desenvolvimento, ou contornar conjuntos de teste volúveis---contornar testes
pode esconder problemas reais, então use isto com cautela.  Execute
@command{info \"(guix.pt_BR) Opções de transformação de pacote\"} para mais
info.")))

 (entry (commit "a98712785e0b042a290420fd74e5a4a5da4fc68f")
        (title (en "New @command{guix git authenticate} command")
               (es "Nueva orden @command{guix git authenticate}")
               (de "Neuer Befehl @command{guix git authenticate}")
               (fr "Nouvelle commande @command{guix git authenticate}")
               (pt "Novo comando @command{guix git authenticate}"))
        (body
         (en "The new @command{guix git authenticate} command authenticates a
Git repository by verifying commit signatures and ensuring they all come from
authorized parties, exactly like @command{guix pull} now does.

This command is primarily useful to developers of channels.  It allows them to
ensure, before pushing, that the channel only contains commits signed with
authorized keys.  But this command is also useful anytime you use Git and want
to allow people to authenticate code fetched from your repository.

Run @command{info \"(guix) Invoking guix git authenticate\"} for more info,
and see @uref{https://guix.gnu.org/blog/2020/securing-updates/} for details on
these mechanisms.")
         (es "La nueva orden @command{guix git authenticate} comprueba la
validez de un repositorio git verificando las firmas de las revisiones y
comprobando que todas las firmas están autorizadas, exactamente igual que
@command{guix pull}.

Esta orden es principalmente útil para desarrolladoras de canales. Permite
asegurar, antes de subir nada al repositorio remoto, que el canal contiene
únicamente revisiones firmadas por claves autorizadas. No obstante esta orden
es útil siempre que use git y quiera que otras personas puedan verificar el
código obtenido de su repositorio.

Ejecute @command{info \"(guix.es) Invocación de guix git authenticate\"}
para obtener más información y vea detalles sobre estos mecanismos en
 @uref{https://guix.gnu.org/blog/2020/securing-updates/}.")
         (de "Mit dem neuen Befehl @command{guix git authenticate} können Sie
ein Git-Repository authentifizieren. Dazu werden alle Commit-Signaturen
verifiziert und geprüft, dass jede von einer autorisierten Quelle kommt, genau
wie es @command{guix pull} nun tut.

Dieser Befehl hilft in erster Linie den Entwicklern von Kanälen. Mit ihm kann
vor einem Push sichergestellt werden, dass der Kanal nur Commits enthält, die
mit autorisierten Schlüsseln signiert worden sind. Aber der Befehl kann auch
helfen, wann immer Sie Git verwenden und ermöglichen wollen, dass Nutzer von
Ihrem Repository geladenen Code authentifizieren können.

Führen Sie @command{info \"(guix) Invoking guix git authenticate\"} aus, um
mehr Informationen zu erhalten, und lesen Sie
@uref{https://guix.gnu.org/blog/2020/securing-updates/} für die Details dieser
Mechanismen.")
         (fr "La nouvelle commande @command{guix git authenticate} authentifie
un dépôt Git en vérifiant les signatures sur les changements (@i{commits}) et
en s'assurant qu'elles sont autorisées, exactement comme @command{guix pull}
le fait désormais.

Cette commande est avant tout utile aux personnes développant un canal.  Elle
leur permet de s'assurer, avant de pousser des changements, que le canal ne
contient que des changements signés par des clefs autorisées.  Mais cette
commande peut aussi s'avérer utile dès que tu veux utiliser Git et permettre
aux autres d'authentifier le code récupéré depuis ton dépôt.

Lance @command{info \"(guix.fr) Invoking guix git authenticate\"} pour plus
d'informations.  Voir @uref{https://guix.gnu.org/blog/2020/securing-updates/}
pour en savoir plus sur ces mécanismes.")
         (pt "O novo comando @command{guix git authenticate} autentica um
repositório Git vrificando assinaturas de commits e assegurando que todos
sejam provenientes de partes autorizadas, exatamente como @command{guix pull}
faz agora.

O comando é primariamente útil para desenvolvedores de canais.  Ele os permite
assegurar, antes da inserção, que o canal só contenha commits assinados por
chaves autorizadas.  Mas este comando também é útil em qualquer momento que
você usa Git e quer permitir que pessoas autentiquem código trazidos de teu
repositório.

This command is primarily useful to developers of channels.  It allows them to
ensure, before pushing, that the channel only contains commits signed with
authorized keys.  But this command is also useful anytime you use Git and want
to allow people to authenticate code fetched from your repository.

Execute @command{info \"(guix.pt_BR) Invocando guix git authenticate\"} para
mais info, e veja @uref{https://guix.gnu.org/blog/2020/securing-updates/} para
detalhes sobre estes mecanismos.")))

 (entry (commit "43badf261f4688c8a7a7a9004a4bff8acb205835")
        (title (en "@command{guix pull} authenticates channels")
               (es "@command{guix pull} verifica los canales")
               (de "@command{guix pull} authentifiziert Kanäle")
               (fr "@command{guix pull} authentifie les canaux")
               (pt "@command{guix pull} autentica canais"))
        (body
         (en "The @command{guix pull} and @command{guix time-machine} commands
now authenticate the source code that they pull, unless the new
@option{--disable-authentication} option is passed.  What this means is that
Guix ensures that each commit received is cryptographically signed by an
authorized developer.  This protects you from attempts to tamper with the Guix
repository and from attempts to ship malicious code to users.

This feature is currently limited to the @code{guix} channel but will soon be
available to third-party channel authors.")
         (es "Las ordenes @command{guix pull} y @command{guix time-machine}
ahora verifican el código fuente que obtienen, a menos que se proporcione la
opción @option{--disable-authentication}. Lo que esto significa es que Guix se
asegura de que cada revisión que recibe está firmada criptográficamente por
una desarrolladora autorizada. Esto le protege de intentos de modificación del
repositorio de Guix y de entregas de código con malas intenciones sobre las
usuarias.

Esta característica está limitada actualmente al canal @code{guix} pero pronto
estará disponible para autoras de canales independientes.")
         (de "Die Befehle @command{guix pull} und @command{guix time-machine}
prüfen nun die Authentizität des heruntergeladenen Quellcodes, außer wenn die
neue Befehlszeilenoption @option{--disable-authentication} angegeben
wurde. Das bedeutet, Guix stellt sicher, dass jeder empfangene Commit durch
einen autorisierten Entwickler kryptografisch signiert wurde. Das schützt Sie
vor Versuchen, das Guix-Repository zu manipulieren oder bösartigen Code an die
Nutzer auszuliefern.

Diese Funktionalität ist auf den @code{guix}-Kanal beschränkt, sie wird jedoch
bald auch Autoren dritter Kanäle zur Verfügung stehen.")
         (fr "Les commandes @command{guix pull} et @command{guix time-machine}
authentifient dorénavant le code source qu'elles obtiennent, à moins que la
nouvelle option @option{--disable-authentication} soit utilisée.  Cela
signifie que Guix s'assure que chaque soumission (@i{commit}) récupéré dispose
d'une signature cryptographique par un·e développeur·euse autorisé·e.  Cela te
protège de tentatives de modifications du dépôt Guix et de tentatives de
livrer du code malintentionné.

Cette fonctionnalité n'est actuellement disponible que pour le canal
@code{guix} mais le sera bientôt pour les canaux tiers.")
         (pt "Os comandos @command{guix pull} e @command{guix time-machine}
agora autenticam o código-fonte que eles trazem, a não ser que a nova opção
@option{--disable-authentication} é passada.  O que isto significa é que o
Guix assegura que cada @i{commit} recebido está criptograficamente assinado
por um desenvolvedor autorizado.  Isto te protege contra tentativas de
adulteração com o repositório Guix e de tentativas de fornecer código
malicioso aos usuários.

Este recurso está atualmente limitado ao canal @code{guix} mas logo estará
disponível a autores de canais de terceiros.")))

 (entry (commit "c924e541390f9595d819edc33c19d979917c15ec")
        (title (en "@command{guix repl} adds support for running Guile scripts")
               (es "@command{guix repl} puede ejecutar guiones de Guile")
               (de "@command{guix repl} kann Guile-Skripte ausführen")
               (fr "@command{guix repl} permet d'exécuter des scripts en langage Guile")
               (pt "@command{guix repl} adiciona suporte à execução de scripts Guile"))
        (body
         (en "The @command{guix repl} command can now be used to run
Guile scripts.  Compared to just launching the @command{guile} command,
@command{guix repl} guarantees that all the Guix modules and all its
dependencies are available in the search path.  Scripts are run like this:

@example
guix repl -- my-script,scm --option1 --option2=option-arg arg1 arg2
@end example

Run @command{info \"(guix) Invoking guix repl\"} for more information.")
         (es "La orden @command{guix repl} ahora se puede usar para
ejecutar guiones de Guile. En comparación con únicamente la ejecución
de la orden @command{guile}, @command{guix repl} garantiza que todos
los módulos de Guix y sus dependencias están disponibles en la ruta
 de búsqueda. Los guiones se ejecutan de este modo:

@example
guix repl -- mi-guion.scm --opcion1 --opcion2=param-op2 param1 param2
@end example

Ejecute @command{info \"(guix.es) Invocación de guix repl\"} para obtener
más información.")
         (de "Der Befehl @command{guix repl} kann jetzt zur Ausführung von
Guile-Skripten verwendet werden.  Im Vergleich zum Befehl
@command{guile} garantiert @command{guix repl}, dass alle Guix-Module und
alle seine Abhängigkeiten im Suchpfad verfügbar sind.  Skripte werden wie
folgt ausgeführt:

@example
guix repl -- my-script,scm --option1 --option2 --option2=option-arg arg1 arg2
@end example

Weitere Informationen erhalten Sie mit
@command{info \"(guix.de) Aufruf von guix repl\"}.")
         (fr "La commande @command{guix repl} peut maintenant être utilisée
pour exécuter des scripts en langage Guile.  Par rapport au simple lancement
de la commande @command{guile}, @command{guix repl} garantit que tous les
modules Guix et toutes ses dépendances sont disponibles dans le chemin
de recherche.  Les scripts sont exécutés comme ceci :

@example
guix repl -- my-script,scm --option1 --option2=option-arg arg1 arg2
@end example

Exécutez @command{info \"(guix.fr) Invoquer guix repl\"} pour plus d'informations.")
         (en "O comando @command{guix repl} pode agora ser usado para executar
scripts Guile.  Comparado a meramente executar o comando @command{guile},
@command{guix repl} assegura que todos os módulos Guix modules e todas as suas
dependências estão disponíveis nos caminhos de busca.  Scripts são executados assim:

@example
guix repl -- my-script,scm --option1 --option2=option-arg arg1 arg2
@end example

Execute @command{info \"(guix.pt_BR) Invocando guix repl\"} para mais
informação.")))

 (entry (commit "b460ba7992a0b4af2ddb5927dcf062784539ef7b")
        (title (en "Add support to boot from a Btrfs subvolume")
               (es "Implementado el arranque desde un subvolumen de Btrfs")
               (de "Unterstützung für Systemstart von einem
Btrfs-Unterlaufwerk hinzugefügt")
               (fr "Ajout du support pour démarrer depuis un sous-volume Btrfs")
               (nl "Nieuwe ondersteuning voor het opstarten vanaf een Btrfs-subvolume")
               (pt "Adiciona suporte à inicialização a partir de um subvolume Btrfs"))
        (body
         (en "The generation of the GRUB configuration file produced from an
operating system declaration now takes into account the use of a Btrfs
subvolume for the partition holding @file{/gnu/store}.  Run the command
@command{info \"(guix) Btrfs file system\"} for more information and
examples.")
         (es "El fichero de configuración de GRUB producido por la
declaración de sistema operativo ahora tiene en cuenta el uso de
subvolúmenes de Btrfs en la partición que contiene @file{/gnu/store}.
Ejecute la orden @command{info \"(guix.es) Sistema de ficheros Btrfs\"}
 para obtener más información y ejemplos.")
         (de "Für die Erzeugung einer GRUB-Konfigurationsdatei aus einer
Betriebssystemdeklaration kann jetzt ein Btrfs-Unterlaufwerk („Subvolume“) für
die Partition mit @file{/gnu/store} angegeben werden.  Führen Sie
@command{info \"(guix) Btrfs file system\"} aus, wenn Sie mehr Informationen
und Beispiele sehen möchten.")
         (fr "La génération du fichier de configuration de GRUB produite à
partir de la déclaration d'un @code{operating-system} tient maintenant compte
de l'utilisation d'un sous-volume Btrfs pour la partition contenant
@file{/gnu/store}.  Exécutez la commande @command{info\"(guix) Btrfs file
system\"} pour des exemples et plus d'information.")
         (nl "Het opmaken van het GRUB-configuratiebestand op basis van
een @code{operating-system}-declaratie houdt nu rekening met het gebruik van
een Btrfs-subvolume voor de partitie die @file{/gnu/store} bevat.  Voer
@command{info \"(guix) Btrfs file system\"} uit voor meer informatie en
voorbeelden.")
         (pt "A geração do arquivo de configuração do GRUB produzido de uma
delcaração de sistema operacional agora leva em conta o uso de um subvolume
Btrfs para a partição que abriga @file{/gnu/store}.  Execute o comando
@command{info \"(guix.pt_BR) Sistema de arquivos Btrfs\"} para mais informação
e exemplos.")))

 (entry (commit "6456232164890dbf5aa20394ee24637feb4b7b9e")
        (title (en "@command{guix pack -RR} introduces a new execution
engine")
               (es "@command{guix pack -RR} introduce un nuevo motor
de ejecución")
               (de "@command{guix pack -RR} führt neuen Ausführungstreiber
ein")
               (fr "@command{guix pack -RR} introduit un nouveau moteur d'exécution")
               (pt "@command{guix pack -RR} introduz um novo motor de
execução"))
        (body
         (en "The @command{guix pack -RR} command allows you to create a
tarball containing @dfn{relocatable binaries}.  Until now, those would rely
either on Linux ``unprivileged user namespaces'' or on PRoot, when
unprivileged user namespaces are not supported.  However, PRoot introduces
significant overhead for some workloads.

To address that, @command{guix pack -RR} introduces a third option based on an
extension to the GNU run-time linker (ld.so) and on Fakechroot, which incurs
very little overhead.  You can select the fastest option when executing a
relocatable binary like this:

@example
GUIX_EXECUTION_ENGINE=performance
export GUIX_EXECUTION_ENGINE
@end example

Run @command{info \"(guix) Invoking guix pack\"} for more information.")
         (es "La orden @command{guix pack -RR} le permite crear un
archivador tar que contiene @dfn{binarios reposicionables}. Hasta ahora
dichos binarios dependían o bien de los ``espacios de nombres de usuarias
sin privilegios'' de Linux o en PRoot, cuando estos no estaban
implementados. No obstante, PRoot introduce una sobrecarga significativa
en algunos escenarios de trabajo.

Para estos casos @command{guix pack -RR} introduce una tercera opción
basada en una extensión al enlazador de tiempo de ejecución de GNU (ld.so)
y en Fakechroot, lo que conlleva muy poca sobrecarga. Puede seleccionar
la opción más rápida cuando ejecute un binario reposicionable de esta
manera:

@example
GUIX_EXECUTION_ENGINE=performance
export GUIX_EXECUTION_ENGINE
@end example

Ejecute @command{info \"(guix.es) Invocación de guix pack\"} para
obtener más información.")
         (de "Mit dem Befehl @command{guix pack -RR} können Sie einen Tarball
mit @dfn{verschieblichen Binärdateien} erzeugen (englisch „Relocatable
Binaries“).  Bisher wurden diese entweder in „unprivilegierten
Benutzernamensräumen“ ohne Berechtigungen ausgeführt, oder in PRoot, wenn
keine unprivilegierten Benutzernamensräume unterstützt wurden.  Allerdings
fällt bei der Ausführung mit PRoot bei manchen Anwendungen deutlich mehr
Rechenaufwand an.

Um dem entgegenzuwirken, stellt @command{guix pack -RR} nun eine dritte Option
zur Verfügung, die sich eine Erweiterung des GNU-Laufzeit-Binders („Run-Time
Linker“, ld.so) und Fakechroot zu Nutze macht.  Dadurch entsteht fast kein
Mehraufwand.  Sie können sich die schnellste Option aussuchen, wenn Sie eine
verschiebliche Binärdatei ausführen, zum Beispiel so:

@example
GUIX_EXECUTION_ENGINE=performance
export GUIX_EXECUTION_ENGINE
@end example

Führen Sie @command{info \"(guix.de) Aufruf von guix pack\"} aus, wenn Sie
mehr wissen wollen.")
         (fr "La commande @command{guix pack -RR} vous permet de créer une
archive tar contenant des @dfn{binaires repositionnables}.  Jusqu'ici, ils
s'appuyaient sur les « espaces de noms non privilégiés » de Linux ou sur
PRoot, quand les espaces de noms non privilégiés n'étaient pas disponibles.
Cependant, PRoot ralenti significativement certains profils d'exécution.

Pour éviter cela, @command{guix pack -RR} introduit une troisième possibilité
basée sur une extension de l'éditeur des liens à l'exécution de GNU (ld.so) et
sur Fakechroot, qui ralenti très peu l'exécution.  Vous pouvez choisir l'option
la plus rapide à l'exécution d'un binaire relocalisable de cette manière :

@example
GUIX_EXECUTION_ENGINE=performance
export GUIX_EXECUTION_ENGINE
@end example

Lancez @command{info \"(guix.fr) Invoquer guix pack\"} pour en savoir plus.")
         (pt "O comando @command{guix pack -RR} te permite criar um tarball
contendo @dfn{binários relocáveis}.  Ate agora, estes ou se apoiariam em
``espaços de nomes de usuário não-privilegiados'' do Linux ou no PRoot, quando
espaços de nomes de usuário não-privilegiados não são suportados.  Porém,
PRoot introduz significativa sobrecarga para algumas cagas de trabalho.

Para endereçar isso, @command{guix pack -RR} introduz uma terceira opção
baseada em uma extensão ao ligador de tempo de execução do GNU (ld.so) e no
Fakechroot, o que incorre em uma sobrecarga bem pequena.  Você pode selecionar
a opção mais rápida quando executando um binário relocável assim:

@example
GUIX_EXECUTION_ENGINE=performance
export GUIX_EXECUTION_ENGINE
@end example

Execute @command{info \"(guix.pt_BR) Invocando guix pack\"} para mais
informação.")))

 (entry (commit "88a96c568c47c97d05d883ada5afbc4e1200b10f")
        (title (en "New @option{--path} option for @command{guix graph}")
               (es "Nueva opción @option{--path} para @command{guix graph}")
               (de "Neue Option @option{--path} für @command{guix graph}")
               (fr "Nouvelle option @option{--path} pour @command{guix graph}")
               (pt "Nova opção @option{--path} para @command{guix graph}"))
        (body
         (en "The @command{guix graph} command has a new @option{--path}
option that instructs it to display the shortest path between two packages,
derivations, or store items.  For example, the command below displays the
shortest path from the @code{libreoffice} package to @code{libunistring}:

@example
guix graph --path libreoffice libunistring
@end example

Run @code{info \"(guix) Invoking guix graph\"} for more information.")
         (es "La orden @command{guix graph} tiene una nueva opción
@option{--path} que le indica que debe mostrar la ruta más corta entre dos
paquetes, derivaciones o elementos del almacén. Por ejemplo, la siguiente
orden muestra la ruta más corta desde el paquete @code{libreoffice} hasta
@code{libunistring}:

@example
guix graph --path libreoffice libunistring
@end example

Ejecute @code{info \"(guix.es) Invocación de guix graph\"} para obtener más
información.")
         (de "Der Befehl @command{guix graph} verfügt über eine neue
Befehlszeilenoption @option{--path}, die ihn den kürzesten Pfad zwischen zwei
Paketen, Ableitungen oder Store-Objekten ausgeben lässt.  Zum Beispiel zeigt
folgender Befehl den kürzesten Pfad vom Paket @code{libreoffice} zu
@code{libunistring}:

@example
guix graph --path libreoffice libunistring
@end example

Führen Sie @code{info \"(guix.de) Aufruf von guix graph\"} aus, um mehr zu
erfahren.")
         (fr "La commande @command{guix graph} a une nouvelle option
@option{--path} qui lui dit d'afficer le plus court chemin entre deux
paquets, dérivations ou éléments du dépôt.  Par exemple, la commande ci-dessous
affiche le plus court chemin entre le paquet @code{libreoffice} et
@code{libunistring} :

@example
guix graph --path libreoffice libunistring
@end example

Lancez @code{info \"(guix.fr) Invoquer guix graph\"} pour en savoir plus.")
         (pt "O comando @command{guix graph} tem uma nova opção
@option{--path} que o instrui a exibir o caminho mais curto entre dois
pacotes, derivações, ou itens do armazém.  Por exemplo, o comando abaixo exibe
o caminho mais curto do pacote @code{libreoffice} até @code{libunistring}:

@example
guix graph --path libreoffice libunistring
@end example

Execute @code{info \"(guix.pt_BR) Invocando guix graph\"} para mais
informação.")))

 (entry (commit "a33eac038a811603c8b9ed106ae405a5f80a0e9d")
        (title (en "GNU C Library upgraded")
               (de "GNU-C-Bibliothek aktualisiert")
               (es "Actualización de la biblioteca C de GNU")
               (fr "Mise à jour de la bibliothèque C de GNU")
               (nl "GNU C-bibliotheek bijgewerkt")
               (pt "Biblioteca C da GNU atualizada"))
        (body
         (en "The GNU C Library (glibc) has been upgraded to version 2.31.  To
run previously-installed programs linked against glibc 2.29, you need to
install locale data for version 2.29 in addition to locale data for 2.31:

@example
guix install glibc-locales glibc-locales-2.29
@end example

On Guix System, you can adjust the @code{locale-libcs} field of your
@code{operating-system} form.  Run @code{info \"(guix) Locales\"}, for more
info.")
         (de "Die GNU-C-Bibliothek (glibc) wurde auf Version 2.31
aktualisiert. Um zuvor installierte Programme, die an glibc 2.29 gebunden
worden sind, weiter benutzen zu können, müssen Sie Locale-Daten für Version
2.29 zusätzlich zu den Locale-Daten für 2.31 installieren:

@example
guix install glibc-locales glibc-locales-2.29
@end example

Auf Guix System genügt es, das @code{locale-libcs}-Feld Ihrer
@code{operating-system}-Form anzupassen. Führen Sie @code{info \"(guix.de)
Locales\"} aus, um weitere Informationen dazu zu erhalten.")
         (es "Se ha actualizado la biblioteca de C de GNU (glibc) a la versión
2.31. Para ejecutar programas instalados previamente que se encuentren
enlazados con glibc 2.29, es necesario que instale los datos de localización
de la versión 2.29 junto a los datos de localización de la versión 2.31:

@example
guix install glibc-locales glibc-locales-2.29
@end example

En el sistema Guix, puede ajustar el campo @code{locale-libcs} de su
declaración @code{operating-system}. Ejecute @code{info \"(guix.es)
Localizaciones\"} para obtener más información.")
         (fr "La bibliothèque C de GNU (glibc) a été mise à jour en version
2.31.  Pour pouvoir lancer tes programmes déjà installés et liés à glibc 2.29,
tu dois installer les données pour la version 2.29 en plus des données de
régionalisation pour la version 2.31:

@example
guix install glibc-locales glibc-locales-2.29
@end example

Sur le système Guix, tu peux ajuster le champ @code{locale-libcs} de ta forme
@code{operating-system}.  Lance @code{info \"(guix.fr) Régionalisation\"} pour
plus de détails.")
         (nl "De GNU C-bibliotheek (glibc) werd bijgewerkt naar versie 2.31.
Om gebruik te maken van reeds geïnstalleerde programma's die aan glibc 2.29
gebonden zijn, moet u de regionale informatie van versie 2.29 naast die van
versie 2.31 installeren:

@example
guix install glibc-locales glibc-locales-2.29
@end example

Op Guix System kunt u het @code{locale-libcs}-veld van uw
@code{operating-system}-vorm aanpassen.   Voer @code{info \"(guix) Locales\"}
uit voor verdere uitleg.")
         (pt "A Biblioteca C da GNU (glibc) foi atualizada para a versão 2.31.
Para executar programas previamente instalados ligados contra a glibc 2.29,
você precisa instalar os dados de localidade (@i{locale}) para a versão 2.29
além dos dados de localidade para 2.31:

@example
guix install glibc-locales glibc-locales-2.29
@end example

No Sistema Guix, você pode ajustar o campo @code{locale-libcs} de sua forma
@code{operating-system}.  Execute @code{info \"(guix.pt_BR) Localidades\"},
para mais informações.")))

 (entry (commit "e1e6491226347d9fb93ff484d78cef98848a510a")
        (title (en "Guix Cookbook now available as Info")
               (de "Guix-Kochbuch jetzt als Info-Dokument verfügbar")
               (pt "Guix Cookbook já está disponível como Info"))
        ;; TRANSLATORS: Adjust the URL and the 'info' command to refer to the
        ;; translated manual if it's available.
        (body (en "The new Guix Cookbook is now fetched by @command{guix pull}
and thus readily available in the Info format.  It aims to provide tutorials
and detailed examples covering a variety of use cases.  You can access it by
typing:

@example
info guix-cookbook
@end example

The Cookbook is currently available in English and German.  You can also find
it @uref{https://guix.gnu.org/cookbook/en/, on-line}.

Your contributions are welcome: @uref{https://guix.gnu.org/contact/, get in
touch with the developers} to share your recipes!")
              (de "Das neue Guix-Kochbuch wird nun von @command{guix pull}
geladen und steht dann im Info-Format zur Verfügung.  Darin sollen Anleitungen
und detaillierte Beispiele gezeigt werden, die eine breite Spanne an
Anwendungsfällen abdecken.  Um darauf zuzugreifen, geben Sie dies ein:

@example
info guix-cookbook.de
@end example

Das Kochbuch steht derzeit auf Deutsch und Englisch zur Verfügung.  Sie können
auch @uref{https://guix.gnu.org/cookbook/de/, online} darauf zugreifen.

Ihre Beiträge werden gerne gesehen.  Bitte
@uref{https://guix.gnu.org/contact/, kontaktieren Sie die Entwickler}, um Ihre
Rezepte mit uns zu teilen!")
              (pt "O novo Livro de Receitas Guix já pode ser buscado via
@command{guix pull} e portanto prontamente disponível em formato Info.  Ele
pretende fornecer tutoriais e exemplos detalhados cobrindo uma variedade de
casos de uso.  Você pode acessá-lo via:

@example
info guix-cookbook
@end example

O Livro de Receitas está disponível em inglês e alemão - entre outros idiomas.
Você também pode encontrá-lo
@uref{https://guix.gnu.org/cookbook/pt-br/, on-line}.

Suas contribuições são bem vindas: @uref{https://guix.gnu.org/contact/, entre
em contato com os desenvolvedores} para compartilhar suas receitas!")))

 (entry (commit "2ca7af43fe17d9acf082dce85d137a27a8ac4887")
        (title (en "Further reduced binary seed bootstrap")
               (de "Bootstrapping jetzt mit noch kleinerem Seed")
               (fr "Le bootstrap binaire est encore plus réduit")
               (pt "Semente binária de inicialização ainda mais reduzida"))
        (body
         (en "The package graph on x86_64 and i686 is now rooted in a further
@dfn{reduced set of binary seeds}.  The initial set of binaries from which
packages are built now weighs in at approximately 60 MiB, a quarter of what it
used to be.  Run @code{info \"(guix) Bootstrapping\"} to learn more, or watch
the talk at @uref{https://fosdem.org/2020/schedule/event/gnumes/}.")
         (de "Der Paketgraph auf x86_64 und i686 hat jetzt eine noch
@dfn{kleinere Menge an binären Seeds} als Wurzel. Das heißt, die ursprüngliche
Menge an Binärdateien, aus denen heraus Pakete erstellt werden, machen nun
ungefähr 60 MiB aus, ein Viertel der früheren Größe. Führen Sie @code{info
\"(guix.de) Bootstrapping\"} aus, um mehr zu erfahren, oder schauen Sie sich
den Vortrag auf @uref{https://fosdem.org/2020/schedule/event/gnumes/} an.")
         (fr "Le graphe des paquets sur x86_64 et i686 prend maintenant racine
dans un @dfn{ensemble de graines binaires} plus réduit.  L'ensemble initial
des binaires à partir desquels les paquets sont désormais construit pèse
environ 60 Mo, un quart de ce qu'il était.  Lancez
@code{info \"(guix.fr) Bootstrapping\"} pour en savoir plus, ou regardez
la présentation sur @uref{https://fosdem.org/2020/schedule/event/gnumes/}.")
         (pt "O grafo de pacotes para x86_64 e i686 agora está enraizado num
@dfn{conjunto de sementes binárias} ainda mais reduzido.  O conjunto inicial
de binários a partir dos quais os pacotes são construídos agora ocupa
aproximadamente 60 MiB, um quarto do que costumava ser.  Execute @code{info
\"(guix.pt_BR) Inicializando\"} para aprender mais, ou assista à palestra em
@uref{https://fosdem.org/2020/schedule/event/gnumes/}.")))

 (entry (commit "0468455e7d279c89ea3ad1b51935efb2b785ec47")
        (title (en "Rottlog service added to @code{%base-services}")
               (de "Rottlog-Dienst ist nun Teil der @code{%base-services}")
               (fr "Le service rottlog a été ajouté à @code{%base-services}")
               (pt "Serviço rottlog adicionado a @code{%base-services}"))
        (body (en "An instance of @code{rottlog-service-type}, the system
service responsible for log rotation, has been added to @code{%base-services}.
If your operating system configuration for Guix System is explicitly adding
@code{rottlog-service-type} to the services, you should now remove it.  See
the ``Log Rotation'' section of the manual for more information.")
              (de "Eine Instanz des @code{rottlog-service-type} für
Log-Rotation wurde zu den @code{%base-services} hinzugefügt.  Wenn der
Systemdienst bereits in Ihrer Konfiguration für Guix System ausdrücklich
genannt wurde, sollten Sie ihn jetzt daraus entfernen.  Siehe den Abschnitt
„Log-Rotation“ im Handbuch für weitere Informationen.")
              (fr "Une instance de @code{rottlog-service-type}, le service
système responsable de la rotation des journaux, a été ajoutée à
@code{%base-services}.  Si votre configuration de système d'exploitation Guix
System ajoute @code{rottlog-service-type} explicitement, vous devriez maintenant
le supprimer.  Voir la section « Rotation des journaux » dans le manuel
pour en savoir plus.")
              (pt "Uma instância de @code{rottlog-service-type}, o serviço de
sistema responsável por rotação de registros (@i{log}), foi adicionado a
@code{%base-services}.  Se sua configuração para o Sistema Guix está
explicitamente adicionando @code{rottlog-service-type} aos serviços, você deve
agora removê-lo.  Veja a seção de rotação de log do manual para mais
informação.")))

 (entry (commit "b6bee63bed4f013064c0d902e7c8b83ed7514ade")
        (title (en "@code{guile} package now refers to version 3.0")
               (de "Das @code{guile}-Paket bezeichnet jetzt Version 3.0")
               (fr "Le paquet @code{guile} se réfère maintenant à la version 3.0")
               (pt "O pacote @code{guile} agora refere-se à versão 3.0"))
        (body (en "The @code{guile} package has been upgraded to version 3.0
 (instead of 2.2).  The @code{guile3.0-} packages have been renamed to their
original name, and @code{guile2.2-} variants of these packages have been
defined.  Additionally, derivations are now all built with Guile 3.0, and
system services also run on 3.0.")
              (de "Das @code{guile}-Paket wurde auf Version 3.0
 (statt 2.2) aktualisiert. Die Pakete, deren Namen mit @code{guile3.0-}
beginnen, wurden umbenannt, so dass sie nun den unveränderten Namen tragen,
während ihre Varianten mit @code{guile2.2-} hinzugefügt wurden.  Des Weiteren
werden jetzt alle Ableitungen mit Guile 3.0 erstellt und die Systemdienste
laufen auch auf 3.0.")
              (fr "Le paquet @code{guile} a été mis à jour vers la version 3.0
(au lieu de la 2.2).  Les paquets @code{guile3.0-} ont été renommés en leur
nom d'origine et les variantes @code{guile2.2-} de ces paquets ont été définis.
En plus, les dérivation sont maintenant construites avec Guile 3.0, et les
services systèmes utilisent aussi la 3.0.")
              (pt "O pacote @code{guile} foi atualizado para a versão 3.0 (em
vez da 2.2).  Os pacotes @code{guile3.0-} foram renomeados para seus nomes
originais, e as variantes @code{guile2.2-} destes pacotes foram definidas.
Adicionalmente, as derivações agora são todas construídas com Guile 3.0, e os
serviços de sistema também rodam no 3.0.")))

 (entry (commit "e3e1a7ba08af2d58c47264c543617e499c239444")
        (title (en "@command{guix pull} now supports SSH authenticated
repositories")
               (de "@command{guix pull} unterstützt nun SSH-authentifizierte
Repositorys")
               (fr "@command{guix pull} prend maintenant en charge
l'authentification en SSH pour les dépôts.")
               (nl "@command{guix pull} ondersteunt nu SSH-geauthenticeerde
repository's.")
               (pt "@command{guix pull} agora suporta repositórios autenticados
via SSH"))
        (body (en "The @command{guix pull} command now supports SSH
authenticated repositories as argument of @option{--url} and in custom
channels definitions.  The authentication requires that an @command{ssh-agent}
is running.")
              (de "Der Befehl @command{guix pull} unterstützt nun über SSH
authentifizierte Repositorys als Argument von @option{--url} und in
selbstgeschriebenen Kanaldefinitionen. Zur Authentisierung muss ein
@command{ssh-agent} laufen.")
              (fr "La commande @command{guix pull} prend maintenant en
charge l'authentification SSH pour les dépôts dans l'argument @option{--url}
et dans le définitions de canaux personnalisés.  L'authentification
nécessite qu'un @command{ssh-agent} soit lancé.")
              (nl "Het @command{guix pull}-commando ondersteunt nu
SSH-geauthenticeerde opslag als argument na @option{--url} en bij het
schrijven van eigen kanaaldefinities.  Hiervoor moet een @command{ssh-agent}
gestart zijn.")
              (pt "O comando @command{guix pull} agora suporta repositórios
autenticados via @acronym{SSH, Secure Shell Protocol} como argumento para
@option{--url} em em definições customizadas de canais.  A autenticação exige
que um @command{ssh-agent} esteja em execução.")))

 (entry (commit "8234fe653e61d0090138cbd4c48d877568355439")
        (title (en "Guix now runs on Guile 3.0")
               (de "Guix läuft jetzt auf Guile 3.0")
               (fr "Guix tourne maintenant sous Guile 3.0")
               (nl "Guix draait nu op Guile 3.0")
               (pt "Guix agora roda no Guile 3.0"))
        (body (en "The Guix revision you just pulled runs on version 3.0 of
GNU@tie{}Guile (previously it would run on version 2.2).  Guile 3.0 improves
performance through the use of just-in-time (JIT) native code generation.  The
switch should be entirely transparent to you.  See
@uref{https://gnu.org/software/guile} for more information on Guile 3.0.")
              (de "Die Guix-Version, die Sie gerade gepullt haben, läuft auf
Version 3.0 von GNU@tie{}Guile (und nicht mehr auf Version 2.2).  Guile 3.0
verbessert die Rechenleistung, indem native Maschinenbefehle „just in time“
erzeugt werden (JIT-Kompilierung).  Der Wechsel sollte für Sie völlig
transparent sein und Guix verhält sich gleich.  Siehe
@uref{https://gnu.org/software/guile} für weitere Informationen zu Guile
3.0.")
              (fr "La révision de Guix que tu viens de récupérer tourne sous
la version 3.0 de GNU@tie{}Guile (Guix tournait avant sous la version 2.2).
Guile 3.0 améliore la performance en générant du code natif à la volée (JIT).
Le changement devrait être totalement transparent pour toi.  Voir
@uref{https://gnu.org/software/guile} pour plus d'information sur Guile 3.0.")
              (nl "De Guix die u net heeft gepulld gebruikt versie 3.0 van
GNU@tie{}Guile (voorheen was dat versie 2.2).  Guile@tie{}3.0 draait dezelfde
programma's doorgaans sneller door ze ‘just-in-time’ (JIT) te vertalen naar
machine-instructies.  De omschakeling zou voor u volledig naadloos moeten
zijn.  Lees @uref{https://gnu.org/software/guile} voor meer informatie over
Guile@tie{}3.0.")
              (pt "A revisão do Guix que você acabou de inserir roda na versão
3.0 do GNU@tie{}Guile (anteriormente rodava na versão 2.2).  Guile 3.0 melhora
o desempenho mediante o uso de geração nativa de código em tempo de
execução (@acronym{JIT, just-in-time}).  A mudança deve ser inteiramente
transparente para você.  Veja @uref{https://gnu.org/software/guile} para mais
informação sobre o Guile 3.0.")))

 (entry (commit "828a39da68a9169ef1d9f9ff02a1c66b1bcbe884")
        (title (en "New @option{--diff} option for @command{guix challenge}")
               (de "Neue @option{--diff}-Option für @command{guix challenge}")
               (fr "Nouvelle option @option{--diff} sur @command{guix challenge}")
               (pt "Nova opção @option{--diff} para @command{guix challenge}"))
        (body (en "The @command{guix challenge} command, which compares
binaries provided by different substitute servers as well as those built
locally, has a new @option{--diff} option.  With @option{--diff=simple} (the
default), @command{guix challenge} automatically downloads binaries and
reports the list of differing files; @option{--diff=diffoscope} instructs it
to pass them to @command{diffoscope}, which simplifies the comparison process.
Run @command{info \"(guix) Invoking guix challenge\"}, for more info.")
              (fr "La commande @command{guix challenge} qui compare les binaires
fournis par différents serveurs de substituts aux contsructions locales a une
nouvelle option @option{--diff}.  Avec @option{--diff=simple} (par défaut),
@command{guix challenge} télécharge automatiquement les binaires et rapporte
la liste des fichiers différents@tie{}; @option{--diff=diffoscope} lui dit
de les passer à @command{diffoscope} qui simplifie le processus de comparaison.
Lance @command{info \"(guix.fr) Invoquer guix challenge\"} pour plus d'info.")
              (de "Der Befehl @command{guix challenge}, mit dem Binärdateien
von unterschiedlichen Substitut-Servern oder lokale Erstellungen miteinander
verglichen werden können, hat eine neue Befehlszeilenoption @option{--diff}
bekommen.  Bei @option{--diff=simple} (der Voreinstellung) lädt @command{guix
challenge} automatisch Binärdateien herunter und listet sich unterscheidende
Dateien auf; wird @option{--diff=diffoscope} angegeben, werden sie an
@command{diffoscope} geschickt, was deren Vergleich erleichtert.  Führen Sie
@command{info \"(guix.de) Aufruf von guix challenge\"} aus, um nähere
Informationen zu erhalten.")
              (pt "O comando @command{guix challenge}, que compara binários
fornecidos por diferentes servidores de substituição bem como aqueles
construídos localmente, tem uma nova opção @option{--diff}.  Com
@option{--diff=simple} (o padrão), @command{guix challenge} automaticamente
baixa binários e reporta a lista de arquivos diferentes;
@option{--diff=diffoscope} instrui passar os arquivos ao @command{diffoscope},
o que simplifica o processo de comparação.  Execute @command{info
\"(guix.pt_BR) Invocando guix challenge\"} para mais informações.")))

 (entry (commit "f675f8dec73d02e319e607559ed2316c299ae8c7")
        (title (en "New command @command{guix time-machine}")
               (de "Neuer Befehl @command{guix time-machine}")
               (fr "Nouvelle commande @command{guix time-machine}")
               (pt "Novo comando @command{guix time-machine}"))
        (body (en "The new command @command{guix time-machine} facilitates
access to older or newer revisions of Guix than the one that is installed.
It can be used to install different versions of packages, and to
re-create computational environments exactly as used in the past.")
              (de "Der neue Befehl @command{guix time-machine} vereinfacht
den Zugriff auf ältere oder neuere Guix-Versionen als die installierte.
Er kann zur Installation bestimmter Paketversionen verwendet werden, aber
auch zur Wiederherstellung von Entwicklungsumgebungen, wie sie in der
Vergangenheit verwendet wurden.")
              (fr "La nouvelle commande @command{guix time-machine}
facilite l'accès à des versions antérieures ou postérieures par rapport
à la version installée.  Elle sert à installer des versions spécifiques
de paquets, ainsi à la restauration d'environnements dans un état
historique.")
              (pt "O novo comando @command{guix time-machine} facilita o
acesso a revisões mais antigas ou mais novas do Guix que aquela instalada.
Ele pode ser utilizado opara instalar versões diferentes de pacotes, e para
recriar ambientes computacionaius exatamente como usados no passado.")))

 (entry (commit "3e962e59d849e4300e447d94487684102d9d412e")
        (title (en "@command{guix graph} now supports package
transformations")
               (de "@command{guix graph} unterstützt nun Paketumwandlungen")
               (fr "@command{guix graph} prend maintenant en charge les
transformations de paquets")
               (pt "@command{guix graph} agora suporta transformações de pacotes"))
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
de dependencias @option{--with-input}.")
         (fr "La commande @command{guix graph} prend maintenant en charge les
transformations de paquets communes (voir @command{info \"(guix.fr) Options de
transformation de paquets\"}).  C'est particulièrement utile pour voir l'effet
de l'option @option{--with-input} qui réécrit de graphe de dépendance.")
         (pt "O comando @command{guix graph} agora suporta as opções comuns de
transformação de pacote (veja @command{info \"(guix.pt_BR) Opções de
transformação de pacote\"}). Isto é útil em particular para observar o efeito
da opção @option{--with-input} de reescrita de dependência de grafo.")))

 (entry (commit "49af34cfac89d384c46269bfd9388b2c73b1220a")
        (title (en "@command{guix pull} now honors
@file{/etc/guix/channels.scm}")
               (de "@command{guix pull} berücksichtigt nun
@file{/etc/guix/channels.scm}")
               (es "Ahora @command{guix pull} tiene en cuenta
@file{/etc/guix/channels.scm}")
               (fr "@command{guix pull} lit maintenant
@file{/etc/guix/channels.scm}")
               (pt "@command{guix pull} agora leva em conta @file{/etc/guix/channels.scm}"))
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
définir les canaux par défaut.")
         (pt "O comando @command{guix pull} agora lerá o arquivo
@file{/etc/guix/channels.scm} se ele existir e se o arquivo de usuário
@file{~/.config/guix/channels.scm} não estiver presente.  Isto permite a
adiministradores de sistemas multiusuário a definir padrões globais de
sistema.")))

 (entry (commit "81c580c8664bfeeb767e2c47ea343004e88223c7")
        (title (en "Insecure @file{/var/guix/profiles/per-user} permissions (CVE-2019-18192)")
               (de "Sicherheitslücke in @file{/var/guix/profiles/per-user}-Berechtigungen (CVE-2019-18192)")
               (es "Vulnerabilidad en los permisos de @file{/var/guix/profiles/per-user} (CVE-2019-18192)")
               (fr "Permissions laxistes pour @file{/var/guix/profiles/per-user} (CVE-2019-18192)")
               (nl "Onveilige @file{/var/guix/profiles/per-user}-rechten (CVE-2019-18192)")
               (pt "Vulnerabilidade de permissões em @file{/var/guix/profiles/per-user} (CVE-2019-18192)"))
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
@code{guix-daemon} met @code{herd} of @code{systemctl}.")
         (pt "O perfil padrão do usuário, @file{~/.guix-profile}, aponta para
@file{/var/guix/profiles/per-user/$USER}.  Até agora,
@file{/var/guix/profiles/per-user} tinha permissões globais de escrita,
permitindo que o comando @command{guix} criasse o subdiretório @code{$USER}.

Em um sistema multiusuário, isso permitia a um usuário malicioso criar e
povoar o subdiretório @code{$USER} para outro usuárop que ainda não estivesse
conectado.  Já que @code{/var/@dots{}/$USER} está no @code{$PATH}, o
usuário-alvo poderia acabar executando código fornecido pelo atacante.  Veja
@uref{https://issues.guix.gnu.org/issue/37744} para mais informações.

Isto foi agora solucionado permitindo ao @command{guix-daemon} criar estes
diretórios em nme dos usuários e removendo as permissões globais de escrita em
@code{per-user}.  Em sistemas multiusuário, recomendamos atualizar o daemon
imediatamente.  Para isso, execute @code{sudo guix pull} se estiver em uma
distro forasteira, ou execute @code{guix pull && sudo guix system reconfigure
@dots{}} se estiver no Sistema Guix.  Em ambos os casos, certifique-se de
reiniciar o serviço logo depois, usando @code{herd} ou @code{systemctl} ou
equivalente.")))

 (entry (commit "5f3f70391809f8791c55c05bd1646bc58508fa2c")
        (title (en "GNU C Library upgraded")
               (de "GNU-C-Bibliothek aktualisiert")
               (es "Actualización de la biblioteca C de GNU")
               (fr "Mise à jour de la bibliothèque C de GNU")
               (nl "GNU C-bibliotheek bijgewerkt")
               (pt "Biblioteca GNU C atualizada"))
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
uit voor verdere uitleg.")
         (pt "A biblioteca GNU C (glibc) foi atualizada para a versão 2.29.
Para executar programas anteriormente instalados ligados contra a glibc 2.28,
você precisa instalar dados de localidade para a versão 2.28 em adição aos
dados de localidade para a 2.29:

@example
guix install glibc-locales glibc-locales-2.28
@end example

No Sistema Guix, você pode ajustar o campo @code{locale-libcs} de sua forma
@code{operating-system}. Execute @code{info \"(guix.pt_BR) Localidades\"} para mais
informações.")))

 (entry (commit "cdd3bcf03883d129581a79e6d6611b2afd3b277b")
        (title (en "New reduced binary seed bootstrap")
               (de "Neues Bootstrapping mit kleinerem Seed")
               (es "Nueva reducción de la semilla binaria para el lanzamiento inicial")
               (fr "Nouvel ensemble de binaires de bootstrap réduit")
               (nl "Nieuwe bootstrap met verkleinde binaire kiem")
               (pt "Nova e reduzida semente binária de inicialização"))
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
@uref{https://archive.fosdem.org/2019/schedule/event/gnumes/}.")
         (pt "O grafo de pacotes nas arquiteturas x86_64 e i686 está agora
enraizado em um conjunto reduzido de sementes binárias.  O conjunto inicial de
binários a partir dos quais os pacotes são construídos agora mede
aproximadamente 130 MiB, metade do que costumava ser. Execute @code{info
\"(guix.pt_BR) Inicializando\"} para aprender mais sobre, ou assista à palestra em
@uref{https://archive.fosdem.org/2019/schedule/event/gnumes/}.")))

 (entry (commit "dcc90d15581189dbc30e201db2b807273d6484f0")
        (title (en "New channel news mechanism")
               (de "Neuer Mechanismus, um Neuigkeiten über Kanäle anzuzeigen.")
               (es "Nuevo mecanismo de noticias de los canales")
               (fr "Nouveau mécanisme d'information sur les canaux")
               (nl "Nieuw mechanisme voor nieuwsberichten per kanaal")
               (pt "Novo mecanismo de notícias para canais"))
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
informatie.")
         (pt "Você está lendo esta mensagem pelo mecanismo de notícias
de canal, parabéns!  Este mecanismo permite que autores forneçam
entradas de notícias que seus usuários podem ler com @command{guix pull
--news}.  Execute @command{info \"(guix.pt_BR) Invocando guix pull\"} para mais
informações."))))
