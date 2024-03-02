;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020-2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu build secret-service)
  #:use-module (guix build utils)

  #:use-module (srfi srfi-26)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)

  #:export (secret-service-receive-secrets
            secret-service-send-secrets))

;;; Commentary:
;;;
;;; Utility procedures for copying secrets into a VM.
;;;
;;; Code:

(define-syntax log
  (lambda (s)
    "Log the given message."
    (syntax-case s ()
      ((_ fmt args ...)
       (with-syntax ((fmt (string-append "secret service: "
                                         (syntax->datum #'fmt))))
         ;; Log to the current output port.  That way, when
         ;; 'secret-service-send-secrets' is called from shepherd, output goes
         ;; to syslog.
         #'(format (current-output-port) fmt args ...))))))

(define-syntax with-modules
  (syntax-rules ()
    "Dynamically load the given MODULEs at run time, making the chosen
bindings available within the lexical scope of BODY."
    ((_ ((module #:select (bindings ...)) rest ...) body ...)
     (let* ((iface (resolve-interface 'module))
            (bindings (module-ref iface 'bindings))
            ...)
       (with-modules (rest ...) body ...)))
    ((_ () body ...)
     (begin body ...))))

(define (wait-for-readable-fd port timeout)
  "Wait until PORT has data available for reading or TIMEOUT has expired.
Return #t in the former case and #f in the latter case."
  (match (resolve-module '(fibers) #f #:ensure #f) ;using Fibers?
    (#f
     (log "blocking on socket...~%")
     (match (select (list port) '() '() timeout)
       (((_) () ()) #t)
       ((() () ())  #f)))
    (fibers
     ;; We're running on the Shepherd 0.9+ with Fibers.  Arrange to make a
     ;; non-blocking wait so that other fibers can be scheduled in while we
     ;; wait for PORT.
     (with-modules (((fibers) #:select (spawn-fiber sleep))
                    ((fibers channels)
                     #:select (make-channel put-message get-message)))
       ;; Make PORT non-blocking.
       (let ((flags (fcntl port F_GETFL)))
         (fcntl port F_SETFL (logior O_NONBLOCK flags)))

       (let ((channel (make-channel)))
         (spawn-fiber
          (lambda ()
            (sleep timeout)                       ;suspends the fiber
            (put-message channel 'timeout)))
         (spawn-fiber
          (lambda ()
            (lookahead-u8 port)                   ;suspends the fiber
            (put-message channel 'readable)))
         (log "suspending fiber on socket...~%")
         (match (get-message channel)
           ('readable #t)
           ('timeout  #f)))))))

(define (socket-address->string address)
  "Return a human-readable representation of ADDRESS, an object as returned by
'make-socket-address'."
  (let ((family (sockaddr:fam address)))
    (cond ((= AF_INET family)
           (string-append (inet-ntop AF_INET (sockaddr:addr address))
                          ":" (number->string (sockaddr:port address))))
          ((= AF_INET6 family)
           (string-append "[" (inet-ntop AF_INET6 (sockaddr:addr address)) "]"
                          ":" (number->string (sockaddr:port address))))
          ((= AF_UNIX family)
           (sockaddr:path address))
          (else
           (object->string address)))))

(define* (secret-service-send-secrets address secret-root
                                      #:key (retry 60)
                                      (handshake-timeout 180))
  "Copy all files under SECRET-ROOT by connecting to secret-service listening
at ADDRESS, an address as returned by 'make-socket-address'.  If connection
fails, sleep 1s and retry RETRY times; once connected, wait for at most
HANDSHAKE-TIMEOUT seconds for handshake to complete.  Return #f on failure."
  (define (file->file+size+mode file-name)
    (let ((stat (stat file-name))
          (target (substring file-name (string-length secret-root))))
      (list target (stat:size stat) (stat:mode stat))))

  (define (send-files sock)
    (let* ((files (if secret-root (find-files secret-root) '()))
           (files-sizes-modes (map file->file+size+mode files))
           (secrets `(secrets
                      (version 0)
                      (files ,files-sizes-modes))))
      (write secrets sock)
      (for-each (lambda (file)
                  (call-with-input-file file
                    (lambda (input)
                      (dump-port input sock))))
                files)))

  (log "sending secrets to ~a~%" (socket-address->string address))

  (let ((sock (socket AF_INET (logior SOCK_CLOEXEC SOCK_STREAM) 0))
        (sleep (if (resolve-module '(fibers) #f)
                   (module-ref (resolve-interface '(fibers)) 'sleep)
                   sleep)))
    ;; Connect to QEMU on the forwarded port.  The 'connect' call succeeds as
    ;; soon as QEMU is ready, even if there's no server listening on the
    ;; forward port inside the guest.
    (let loop ((retry retry))
      (catch 'system-error
        (cute connect sock address)
        (lambda (key . args)
          (when (zero? retry)
            (apply throw key args))
          (log "retrying connection [~a attempts left]~%"
               (- retry 1))
          (sleep 1)
          (loop (1- retry)))))

    (log "connected; waiting for handshake...~%")

    ;; Wait for "hello" message from the server.  This is the only way to know
    ;; that we're really connected to the server inside the guest.
    (if (wait-for-readable-fd sock handshake-timeout)
        (match (read sock)
          (('secret-service-server ('version version ...))
           (log "sending files from ~s...~%" secret-root)
           (send-files sock)
           (log "done sending files to ~a~%"
                (socket-address->string address))
           (close-port sock)
           secret-root)
          (x
           (log "invalid handshake ~s~%" x)
           (close-port sock)
           #f))
        (begin                                    ;timeout
         (log "timeout while sending files to ~a~%"
              (socket-address->string address))
         (close-port sock)
         #f))))

(define (delete-file* file)
  "Ensure FILE does not exist."
  (catch 'system-error
    (lambda ()
      (delete-file file))
    (lambda args
      (unless (= ENOENT (system-error-errno args))
        (apply throw args)))))

(define (secret-service-receive-secrets address)
  "Listen to ADDRESS, an address returned by 'make-socket-address', and wait
for a secret service client to send secrets.  Write them to the file system.
Return the list of files installed on success, and #f otherwise."

  (define (wait-for-client address)
    ;; Wait for a connection on ADDRESS.  Note: virtio-serial ports are safer
    ;; than TCP connections but they are (presumably) unsupported on GNU/Hurd.
    (let ((sock (socket AF_INET (logior SOCK_CLOEXEC SOCK_STREAM) 0)))
      (bind sock address)
      (listen sock 1)
      (log "waiting for secrets on ~a...~%"
           (socket-address->string address))

      (match (select (list sock) '() '() 60)
        (((_) () ())
         (match (accept sock)
           ((client . address)
            (log "client connection from ~a~%"
                 (inet-ntop (sockaddr:fam address)
                            (sockaddr:addr address)))

            ;; Send a "hello" message.  This allows the client running on the
            ;; host to know that it's now actually connected to server running
            ;; in the guest.
            (write '(secret-service-server (version 0)) client)
            (force-output client)
            (close-port sock)
            client)))
        ((() () ())
         (log "did not receive any secrets; time out~%")
         (close-port sock)
         #f))))

  ;; TODO: Remove when (@ (guix build utils) dump-port) has a 'size'
  ;; parameter.
  (define (dump in out size)
    ;; Copy SIZE bytes from IN to OUT.
    (define buf-size 65536)
    (define buf (make-bytevector buf-size))

    (let loop ((left size))
      (if (<= left 0)
          0
          (let ((read (get-bytevector-n! in buf 0 (min left buf-size))))
            (if (eof-object? read)
                left
                (begin
                  (put-bytevector out buf 0 read)
                  (loop (- left read))))))))

  (define (read-secrets port)
    ;; Read secret files from PORT and install them.
    (match (false-if-exception (read port))
      (('secrets ('version 0)
                 ('files ((files sizes modes) ...)))
       (for-each (lambda (file size mode)
                   (log "installing file '~a' (~a bytes)...~%"
                        file size)
                   (mkdir-p (dirname file))

                   ;; It could be that FILE already exists, for instance
                   ;; because it has been created by a service's activation
                   ;; snippet (e.g., SSH host keys).  Delete it.
                   (delete-file* file)

                   (call-with-output-file file
                     (lambda (output)
                       (dump port output size)
                       (chmod file mode))))
                 files sizes modes)
       (log "received ~a secret files~%" (length files))
       files)
      (_
       (log "invalid secrets received~%")
       #f)))

  (let* ((port   (wait-for-client address))
         (result (and=> port read-secrets)))
    (when port
      (close-port port))
    result))

;;; Local Variables:
;;; eval: (put 'with-modules 'scheme-indent-function 1)
;;; End:

;;; secret-service.scm ends here
