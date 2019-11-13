# GNU Guix --- Functional package management for GNU
# Copyright © 2012, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
#
# This file is part of GNU Guix.
#
# GNU Guix is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# GNU Guix is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

#
# Test the daemon and its interaction with 'guix substitute'.
#

set -e

guix-daemon --version
guix build --version

drv="`guix build emacs -d`"
out="`guile -c '								\
  (use-modules (guix) (guix grafts) (gnu packages emacs))			\
  (define store (open-connection))						\
  (%graft? #f)
  (display (derivation->output-path (package-derivation store emacs)))'`"

hash_part="`basename $out | cut -c 1-32`"
narinfo="$hash_part.narinfo"
substitute_dir="`echo $GUIX_BINARY_SUBSTITUTE_URL | sed -'es,file://,,g'`"

cat > "$substitute_dir/nix-cache-info"<<EOF
StoreDir: `dirname $drv`
WantMassQuery: 0
EOF

cat > "$substitute_dir/$narinfo"<<EOF
StorePath: $out
URL: /nowhere/example.nar
Compression: none
NarSize: 1234
References: 
System: `guile -c '(use-modules (guix)) (display (%current-system))'`
Deriver: $drv
EOF

# Remove the cached narinfo.
rm -f "$XDG_CACHE_HOME/guix/substitute/$hash_part"

# Make sure we see the substitute.
guile -c "
  (use-modules (guix))
  (define store (open-connection))
  (set-build-options store #:use-substitutes? #t
                     #:substitute-urls (list \"$GUIX_BINARY_SUBSTITUTE_URL\"))
  (exit (has-substitutes? store \"$out\"))"

# Now, run guix-daemon --no-substitutes.
socket="$GUIX_STATE_DIRECTORY/alternate-socket"
guix-daemon --no-substitutes --listen="$socket" --disable-chroot &
daemon_pid=$!
trap 'kill $daemon_pid' EXIT

# Make sure we DON'T see the substitute.
guile -c "
  (use-modules (guix))
  (define store (open-connection \"$socket\"))

  ;; This setting MUST NOT override the daemon's --no-substitutes.
  (set-build-options store #:use-substitutes? #t
                     #:substitute-urls (list \"$GUIX_BINARY_SUBSTITUTE_URL\"))

  (exit (not (has-substitutes? store \"$out\")))"

kill "$daemon_pid"

# Pass several '--listen' options, and make sure they are all honored.
guix-daemon --disable-chroot --listen="$socket" --listen="$socket-second" \
	    --listen="localhost" --listen="localhost:9876" &
daemon_pid=$!

for uri in "$socket" "$socket-second" \
		     "guix://localhost" "guix://localhost:9876"
do
    GUIX_DAEMON_SOCKET="$uri" guix build guile-bootstrap
done

kill "$daemon_pid"

# Make sure 'profiles/per-user' is created when connecting over TCP.

orig_GUIX_STATE_DIRECTORY="$GUIX_STATE_DIRECTORY"
GUIX_STATE_DIRECTORY="$GUIX_STATE_DIRECTORY-2"

guix-daemon --disable-chroot --listen="localhost:9877" &
daemon_pid=$!

GUIX_DAEMON_SOCKET="guix://localhost:9877"
export GUIX_DAEMON_SOCKET

test ! -d "$GUIX_STATE_DIRECTORY/profiles/per-user"

guix build guile-bootstrap -d

test -d "$GUIX_STATE_DIRECTORY/profiles/per-user/$USER"

kill "$daemon_pid"
unset GUIX_DAEMON_SOCKET
GUIX_STATE_DIRECTORY="$orig_GUIX_STATE_DIRECTORY"

# Check the failed build cache.

guix-daemon --no-substitutes --listen="$socket" --disable-chroot	\
  --cache-failures &
daemon_pid=$!

guile -c "
  (use-modules (guix) (guix grafts) (guix tests) (srfi srfi-34))
  (define store (open-connection-for-tests \"$socket\"))

  ;; Disable grafts to avoid building more than needed.
  (%graft? #f)

  (define (build-without-failing drv)
    (lambda (store)
      (guard (c ((store-protocol-error? c) (values #t store)))
        (build-derivations store (list drv))
        (values #f store))))

  ;; Make sure failed builds are cached and can be removed from
  ;; the cache.
  (run-with-store store
    (mlet* %store-monad ((drv (gexp->derivation \"failure\"
                                                #~(begin
                                                    (ungexp output)
                                                     #f)))
                         (out -> (derivation->output-path drv))
                         (ok?    (build-without-failing drv)))
      ;; Note the mixture of monadic and direct style.  Don't try
      ;; this at home!
      (return (exit (and ok?
                         (equal? (query-failed-paths store) (list out))
                         (begin
                           (clear-failed-paths store (list out))
                           (null? (query-failed-paths store)))))))
    #:guile-for-build (%guile-for-build)) "

kill "$daemon_pid"


# Make sure the daemon's default 'build-cores' setting is honored.

guix-daemon --listen="$socket" --disable-chroot --cores=42 &
daemon_pid=$!

GUIX_DAEMON_SOCKET="$socket" \
guile -c '
  (use-modules (guix) (guix tests))

  (with-store store
    (let* ((build  (add-text-to-store store "build.sh"
                                      "echo $NIX_BUILD_CORES > $out"))
           (bash   (add-to-store store "bash" #t "sha256"
                                 (search-bootstrap-binary "bash"
                                                          (%current-system))))
           (drv    (derivation store "the-thing" bash
                               `("-e" ,build)
                               #:inputs `((,bash) (,build))
                               #:env-vars `(("x" . ,(random-text))))))
      (and (build-derivations store (list drv))
           (exit
            (= 42 (pk (call-with-input-file (derivation->output-path drv)
                        read)))))))'


kill "$daemon_pid"

# Make sure the daemon's default 'timeout' and 'max-silent-time' settings are
# honored.

client_code='
  (use-modules (guix) (guix tests) (srfi srfi-34))

  (with-store store
    (let* ((build  (add-text-to-store store "build.sh"
                                      "while true ; do : ; done"))
           (bash   (add-to-store store "bash" #t "sha256"
                                 (search-bootstrap-binary "bash"
                                                          (%current-system))))
           (drv    (derivation store "the-thing" bash
                               `("-e" ,build)
                               #:inputs `((,bash) (,build))
                               #:env-vars `(("x" . ,(random-text))))))
      (exit (guard (c ((store-protocol-error? c)
                       (->bool
                        (string-contains (pk (store-protocol-error-message c))
                                         "failed"))))
              (build-derivations store (list drv))
              #f))))'


for option in --max-silent-time=1 --timeout=1
do
    guix-daemon --listen="$socket" --disable-chroot "$option" &
    daemon_pid=$!

    GUIX_DAEMON_SOCKET="$socket" guile -c "$client_code"
    kill "$daemon_pid"
done

# Make sure garbage collection from a TCP connection does not work.

tcp_socket="127.0.0.1:9998"
guix-daemon --listen="$tcp_socket" &
daemon_pid=$!

GUIX_DAEMON_SOCKET="guix://$tcp_socket"
export GUIX_DAEMON_SOCKET

if guix gc; then false; else true; fi

unset GUIX_DAEMON_SOCKET
kill "$daemon_pid"

# Log compression.

guix-daemon --listen="$socket" --disable-chroot --debug --log-compression=gzip &
daemon_pid=$!

stamp="compressed-build-log-test-$$-`date +%H%M%S`"
client_code="
  (use-modules (guix) (gnu packages bootstrap))

  (with-store store
    (run-with-store store
      (mlet %store-monad ((drv (lower-object
				(computed-file \"compressed-log-test\"
					       #~(begin
						   (display \"$stamp\")
                                                   (newline)
						   (mkdir #\$output))
					       #:guile %bootstrap-guile))))
	(display (derivation-file-name drv))
	(newline)
	(return #t))))
"

GUIX_DAEMON_SOCKET="$socket"
export GUIX_DAEMON_SOCKET

drv=`guile -c "$client_code"`
guix build "$drv"

log=`guix build "$drv" --log-file`
test -f "$log"
case "$log" in
    *.gz) test "`gunzip -c < "$log"`" = "$stamp" ;;
    *)    false ;;
esac
