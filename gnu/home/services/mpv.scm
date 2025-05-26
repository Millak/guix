;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024, 2025 Tomas Volf <~@wolfsden.cz>
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

(define-module (gnu home services mpv)
  #:use-module ((gnu services configuration) #:select (%unset-value
                                                       maybe-value-set?))
  #:use-module (gnu home services)
  #:autoload   (guix diagnostics) (formatted-message)
  #:autoload   (guix i18n) (G_)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:export (make-home-mpv-configuration
            home-mpv-configuration?
            home-mpv-configuration-global
            home-mpv-configuration-profiles
            home-mpv-configuration-extra-config
            home-mpv-configuration-source-location

            serialize-home-mpv-configuration

            make-mpv-profile-configuration
            mpv-profile-configuration?
            ;; Field accessor procedures are exported by a syntax form when
            ;; they are defined, so they are not listed here.

            home-mpv-service-type))


;;;
;;; Basic types.
;;;
(define (serialize-mpv/boolean field-name value)
  #~(string-append (string-trim-right #$(symbol->string field-name) #\?)
                   "="
                   #$(if value "yes" "no")
                   "\n"))
(define mpv/boolean? boolean?)

(define (serialize-mpv/integer field-name value)
  #~(string-append #$(symbol->string field-name)
                   "="
                   #$(number->string value)
                   "\n"))
(define (mpv/integer? n)
  ;; We assume integer is a signed 32bit number.
  (and-let* (((integer? n))
             ((>= n (* -1 (expt 2 (1- 32)))))
             ((<= n (1-   (expt 2 (1- 32))))))))

(define (serialize-mpv/integer64 field-name value)
  #~(string-append #$(symbol->string field-name)
                   "="
                   #$(number->string value)
                   "\n"))
(define (mpv/integer64? n)
  ;; We assume integer is a signed 64bit number.
  (and-let* (((integer? n))
             ((>= n (* -1 (expt 2 (1- 64)))))
             ((<= n (1-   (expt 2 (1- 64))))))))

(define (serialize-mpv/string field-name value)
  #~(string-append #$(symbol->string field-name)
                   "="
                   #$value
                   "\n"))
(define mpv/string?
  string?)

(define (serialize-mpv/float field-name value)
  #~(string-append #$(symbol->string field-name)
                   "="
                   #$(number->string (exact->inexact value))
                   "\n"))
(define mpv/float?
  ;; I am not sure how to validate floats.
  real?)

(define (serialize-mpv/double field-name value)
  #~(string-append #$(symbol->string field-name)
                   "="
                   #$(number->string (exact->inexact value))
                   "\n"))
(define mpv/double?
  ;; I am not sure how to validate doubles.
  real?)




;;;
;;; Additional types (possible based on the basic ones).
;;;

;;; Aspect seems to be treated as an integer, so define it in terms of it.
(define serialize-mpv/aspect serialize-mpv/integer)
(define mpv/aspect? mpv/integer?)

;;; `Audio channels or channel map' seems to be basically a free form string
;;; with no way to validate.
(define serialize-mpv/audio-channels-or-channel-map serialize-mpv/string)
(define mpv/audio-channels-or-channel-map? mpv/string?)

;;; Does not seem possible to validate.
(define serialize-mpv/audio-format serialize-mpv/string)
(define mpv/audio-format? mpv/string?)

;;; While most options list 4.6116860184274e+18 as a maximum value, we will
;;; use integer64 here.  That should be enough for everyone for few more
;;; years.
(define serialize-mpv/byte-size serialize-mpv/integer64)
(define mpv/byte-size? mpv/integer64?)

(define (serialize-mpv/color field-name value)
  #~(string-append #$(symbol->string field-name)
                   "="
                   #$(if (list? value)
                         (string-join (map number->string value) "/")
                         value)
                   "\n"))
(define (mpv/color? value)
  (define (ok-num? n)
    (and (number? n)
         (>= n 0)
         (<= n 1)))
  (if (list? value)
      ;; Either a list of 3(4) numbers encoding RGB(A) on range from 0 to 1...
      (match value
        (((? ok-num? r) (? ok-num? g) (? ok-num? b))
         #t)
        (((? ok-num? r) (? ok-num? g) (? ok-num? b) (? ok-num? alpha))
         #t)
        (_
         #f))
      ;; ... or RGB(A) hex encoding.
      (string-match "^#([A-Fa-f0-9]{2})?[A-Fa-f0-9]{6}$" value)))

;;; I do not see value mirroring fourcc.org's database here.  It is further
;;; complicated by arbitrary hex being accepted as well.  So string it is.
(define serialize-mpv/fourcc serialize-mpv/string)
(define mpv/fourcc? mpv/string?)

;;; No way to validate.
(define serialize-mpv/image-format serialize-mpv/string)
(define mpv/image-format? mpv/string?)

;;; Looking at the documentation for --start, there is no way to make this
;;; bullet-proof, especially since even chapter numbers are accepted.
(define serialize-mpv/relative-time-or-percent-position serialize-mpv/string)
(define mpv/relative-time-or-percent-position? mpv/string?)

(define serialize-mpv/time serialize-mpv/string)
(define mpv/time? mpv/string?)

(define serialize-mpv/video-rectangle serialize-mpv/string)
(define (mpv/video-rectangle? value)
  (or (string-match "-?[0-9]+%?:-?[0-9]+%?" value)
      (string-match
       "^(-?[0-9]+%?(x-?[0-9]+%?)?)?([+-]-?[0-9]+%?[+-]-?[0-9]+%?)?$"
       value)))

(define serialize-mpv/window-geometry serialize-mpv/string)
(define (mpv/window-geometry? value)
  (or (string-match "-?[0-9]+%?:-?[0-9]+%?" value)
      (string-match
       "^(-?[0-9]+%?(x-?[0-9]+%?)?)?([+-]-?[0-9]+%?[+-]-?[0-9]+%?)?(/[0-9]+)?$"
       value)))

(define serialize-mpv/window-size serialize-mpv/string)
(define (mpv/window-size? value)
  (string-match "^([0-9]+%?(x[0-9]+%?)?)?$" value))

(define (serialize-mpv/enumeration field-name value)
  #~(string-append #$(symbol->string field-name)
                   "="
                   ;; This could be either symbol or (in case of enumerations
                   ;; with alternate type) anything.  So just use `format'.
                   #$(format #f "~s" value)
                   "\n"))
(define (mpv/enumeration? value)
  ;; There is no general way to check enumerations.  The field always has to
  ;; define custom sanitizer.
  #t)




;;;
;;; List types.
;;;
(define (serialize-mpv/list-of-string field-name lst)
  #~(string-append #$(symbol->string field-name)
                   "="
                   #$(string-join lst ",")
                   "\n"))
(define (mpv/list-of-string? lst)
  (every mpv/string? lst))

(define (serialize-mpv/list-of-key-value field-name lst)
  #~(string-append #$(symbol->string field-name)
                   "="
                   #$(string-join (map (match-lambda
                                         ((k . v) (format #f "~a=~a" k v)))
                                       lst)
                                  ",")
                   "\n"))
(define (mpv/list-of-key-value? lst)
  (every (match-lambda
           (((? string?) . (? string?)) #t)
           (_ #f))
         lst))

(define serialize-mpv/list-of-object-setting serialize-mpv/list-of-string)
(define mpv/list-of-object-setting? mpv/list-of-string?)

(define serialize-mpv/list-of-output-verbosity serialize-mpv/list-of-key-value)
(define mpv/list-of-output-verbosity? mpv/list-of-key-value?)




;;;
;;; Actual configuration record.  Contains a lot of generated code.
;;;

(define-record-type <profile-option>
  (make-profile-option name type-check serializer)
  profile-option?
  (name       profile-option-name)
  (type-check profile-option-type-check)
  (serializer profile-option-serializer))

(define %opts (make-hash-table))

(define-syntax define-opt
  (lambda (x)
    (syntax-case x ()
      ((_ name type extra-checks ...)
       (let* ((d/n (syntax->datum #'name))
              (d/t (syntax->datum #'type))
              (d/accessor (string->symbol
                           (format #f "mpv-profile-configuration-~a" d/n)))
              (d/type-check (string->symbol
                             (format #f "mpv/~a?" d/t)))
              (d/serializer (string->symbol
                             (format #f "serialize-mpv/~a" d/t))))
         (with-syntax
             ((kw (datum->syntax x (symbol->keyword d/n)))
              (accessor (datum->syntax x d/accessor))
              (type-check (datum->syntax x d/type-check))
              (serializer (datum->syntax x d/serializer))
              (val (datum->syntax x 'val)))
           #'(begin
               (hashq-set! %opts 'name
                           (make-profile-option (symbol->string 'name)
                                                (lambda (val)
                                                  (and (type-check val)
                                                       extra-checks ...))
                                                serializer))
               (define-public (accessor cfg)
                 (let ((x (hashq-ref (%mpv-profile-configuration-data cfg)
                                     'name
                                     %unset-value)))
                   (if (eq? x %unset-value)
                       %unset-value
                       (car x)))))))))))

;;; Generated by: https://git.wolfsden.cz/guix/tree/etc/update-mpv-configuration
;;; Generated code - START.
(define-opt ab-loop-a time)
(define-opt ab-loop-b time)
(define-opt
  ab-loop-count
  enumeration
  (or (memq val '(inf))
      (and (integer? val)
           (>= val 0)
           (<= val 2147483647))))
(define-opt access-references? boolean)
(define-opt ad string)
(define-opt
  ad-lavc-ac3drc
  float
  (>= val 0)
  (<= val 6))
(define-opt ad-lavc-downmix? boolean)
(define-opt ad-lavc-o list-of-key-value)
(define-opt
  ad-lavc-threads
  integer
  (>= val 0)
  (<= val 16))
(define-opt ad-queue-enable? boolean)
(define-opt
  ad-queue-max-bytes
  byte-size
  (>= val 0)
  (<= val 4.6116860184274e18))
(define-opt
  ad-queue-max-samples
  integer64
  (>= val 0))
(define-opt ad-queue-max-secs double (>= val 0))
(define-opt af list-of-object-setting)
(define-opt
  audio
  enumeration
  (or (memq val '(no auto))
      (and (integer? val) (>= val 0) (<= val 8190))))
(define-opt alang list-of-string)
(define-opt allow-delayed-peak-detect? boolean)
(define-opt
  alsa-buffer-time
  integer
  (>= val 0)
  (<= val 2147483647))
(define-opt alsa-ignore-chmap? boolean)
(define-opt alsa-mixer-device string)
(define-opt
  alsa-mixer-index
  integer
  (>= val 0)
  (<= val 99))
(define-opt alsa-mixer-name string)
(define-opt alsa-non-interleaved? boolean)
(define-opt
  alsa-periods
  integer
  (>= val 0)
  (<= val 2147483647))
(define-opt alsa-resample? boolean)
(define-opt ao list-of-object-setting)
(define-opt ao-null-broken-delay? boolean)
(define-opt ao-null-broken-eof? boolean)
(define-opt
  ao-null-buffer
  float
  (>= val 0)
  (<= val 100))
(define-opt
  ao-null-channel-layouts
  audio-channels-or-channel-map)
(define-opt ao-null-format audio-format)
(define-opt
  ao-null-latency
  float
  (>= val 0)
  (<= val 100))
(define-opt
  ao-null-outburst
  integer
  (>= val 1)
  (<= val 100000))
(define-opt
  ao-null-speed
  float
  (>= val 0)
  (<= val 10000))
(define-opt ao-null-untimed? boolean)
(define-opt ao-pcm-append? boolean)
(define-opt ao-pcm-file string)
(define-opt ao-pcm-waveheader? boolean)
(define-opt archive-exts list-of-string)
(define-opt
  audio-backward-batch
  integer
  (>= val 0)
  (<= val 1024))
(define-opt
  audio-backward-overlap
  enumeration
  (or (memq val '(auto))
      (and (integer? val) (>= val 0) (<= val 1024))))
(define-opt
  audio-buffer
  double
  (>= val 0)
  (<= val 10))
(define-opt
  audio-channels
  audio-channels-or-channel-map)
(define-opt audio-client-name string)
(define-opt audio-delay float)
(define-opt audio-demuxer string)
(define-opt audio-device string)
(define-opt
  audio-display
  enumeration
  (memq val '(no embedded-first external-first)))
(define-opt audio-exclusive? boolean)
(define-opt audio-exts list-of-string)
(define-opt audio-fallback-to-null? boolean)
(define-opt
  audio-file-auto
  enumeration
  (memq val '(no exact fuzzy all)))
(define-opt audio-file-paths list-of-string)
(define-opt audio-files list-of-string)
(define-opt audio-format audio-format)
(define-opt audio-normalize-downmix? boolean)
(define-opt audio-pitch-correction? boolean)
(define-opt
  audio-resample-cutoff
  double
  (>= val 0)
  (<= val 1))
(define-opt
  audio-resample-filter-size
  integer
  (>= val 0)
  (<= val 32))
(define-opt audio-resample-linear? boolean)
(define-opt
  audio-resample-max-output-size
  double)
(define-opt
  audio-resample-phase-shift
  integer
  (>= val 0)
  (<= val 30))
(define-opt
  audio-reversal-buffer
  byte-size
  (>= val 0)
  (<= val 4.6116860184274e18))
(define-opt
  audio-samplerate
  integer
  (>= val 0)
  (<= val 768000))
(define-opt audio-spdif string)
(define-opt audio-stream-silence? boolean)
(define-opt audio-swresample-o list-of-key-value)
(define-opt
  audio-wait-open
  float
  (>= val 0)
  (<= val 60))
(define-opt auto-window-resize? boolean)
(define-opt
  autocreate-playlist
  enumeration
  (memq val '(no filter same)))
(define-opt autofit window-size)
(define-opt autofit-larger window-size)
(define-opt autofit-smaller window-size)
(define-opt autoload-files? boolean)
(define-opt
  autosync
  enumeration
  (or (memq val '(no))
      (and (integer? val) (>= val 0) (<= val 10000))))
(define-opt
  background
  enumeration
  (memq val '(none color tiles)))
(define-opt background-color color)
(define-opt
  blend-subtitles
  enumeration
  (memq val '(no yes video)))
(define-opt bluray-device string)
(define-opt border? boolean)
(define-opt
  border-background
  enumeration
  (memq val '(none color tiles)))
(define-opt
  brightness
  float
  (>= val -100)
  (<= val 100))
(define-opt
  cache
  enumeration
  (memq val '(no auto yes)))
(define-opt cache-on-disk? boolean)
(define-opt cache-pause? boolean)
(define-opt cache-pause-initial? boolean)
(define-opt
  cache-pause-wait
  float
  (>= val 0)
  (<= val 3.4028234663853e38))
(define-opt cache-secs double (>= val 0))
(define-opt cdda-cdtext? boolean)
(define-opt cdda-device string)
(define-opt
  cdda-overlap
  integer
  (>= val 0)
  (<= val 75))
(define-opt
  cdda-paranoia
  integer
  (>= val 0)
  (<= val 2))
(define-opt
  cdda-sector-size
  integer
  (>= val 1)
  (<= val 100))
(define-opt cdda-skip? boolean)
(define-opt cdda-span-a integer)
(define-opt cdda-span-b integer)
(define-opt
  cdda-speed
  integer
  (>= val 1)
  (<= val 100))
(define-opt cdda-toc-offset integer)
(define-opt
  chapter-merge-threshold
  integer
  (>= val 0)
  (<= val 10000))
(define-opt chapter-seek-threshold double)
(define-opt chapters-file string)
(define-opt
  clipboard-backends
  list-of-object-setting)
(define-opt clipboard-monitor? boolean)
(define-opt config? boolean)
(define-opt
  container-fps-override
  double
  (>= val 0))
(define-opt
  contrast
  float
  (>= val -100)
  (<= val 100))
(define-opt cookies? boolean)
(define-opt cookies-file string)
(define-opt
  corner-rounding
  float
  (>= val 0)
  (<= val 1))
(define-opt correct-downscaling? boolean)
(define-opt correct-pts? boolean)
(define-opt
  cover-art-auto
  enumeration
  (memq val '(no exact fuzzy all)))
(define-opt cover-art-files list-of-string)
(define-opt cover-art-whitelist list-of-string)
(define-opt
  cscale
  enumeration
  (memq val
        '(bilinear
          bicubic_fast
          oversample
          spline16
          spline36
          spline64
          sinc
          lanczos
          ginseng
          bicubic
          hermite
          catmull_rom
          mitchell
          robidoux
          robidouxsharp
          box
          nearest
          triangle
          gaussian
          jinc
          ewa_lanczos
          ewa_hanning
          ewa_ginseng
          ewa_lanczossharp
          ewa_lanczos4sharpest
          ewa_lanczossoft
          haasnsoft
          ewa_robidoux
          ewa_robidouxsharp
          bartlett
          cosine
          hanning
          tukey
          hamming
          quadric
          welch
          kaiser
          blackman
          sphinx)))
(define-opt
  cscale-antiring
  float
  (>= val 0)
  (<= val 1))
(define-opt cscale-blur float)
(define-opt
  cscale-clamp
  float
  (>= val 0)
  (<= val 1))
(define-opt cscale-param1 float)
(define-opt cscale-param2 float)
(define-opt
  cscale-radius
  float
  (>= val 0.5)
  (<= val 16))
(define-opt
  cscale-taper
  float
  (>= val 0)
  (<= val 1))
(define-opt
  cscale-window
  enumeration
  (memq val
        '(bartlett
          cosine
          hanning
          tukey
          hamming
          quadric
          welch
          kaiser
          blackman
          sphinx
          jinc)))
(define-opt cscale-wparam float)
(define-opt
  cscale-wtaper
  float
  (>= val 0)
  (<= val 1))
(define-opt
  cursor-autohide
  enumeration
  (or (memq val '(no always))
      (and (integer? val) (>= val 0) (<= val 30000))))
(define-opt cursor-autohide-fs-only? boolean)
(define-opt deband? boolean)
(define-opt
  deband-grain
  float
  (>= val 0)
  (<= val 4096))
(define-opt
  deband-iterations
  integer
  (>= val 0)
  (<= val 16))
(define-opt
  deband-range
  float
  (>= val 1)
  (<= val 64))
(define-opt
  deband-threshold
  float
  (>= val 0)
  (<= val 4096))
(define-opt
  deinterlace
  enumeration
  (memq val '(no yes auto)))
(define-opt
  deinterlace-field-parity
  enumeration
  (memq val '(tff bff auto)))
(define-opt demuxer string)
(define-opt
  demuxer-backward-playback-step
  double
  (>= val 0))
(define-opt demuxer-cache-dir string)
(define-opt
  demuxer-cache-unlink-files
  enumeration
  (memq val '(immediate whendone no)))
(define-opt demuxer-cache-wait? boolean)
(define-opt demuxer-donate-buffer? boolean)
(define-opt
  demuxer-hysteresis-secs
  double
  (>= val 0))
(define-opt demuxer-lavf-allow-mimetype? boolean)
(define-opt
  demuxer-lavf-analyzeduration
  float
  (>= val 0)
  (<= val 3600))
(define-opt
  demuxer-lavf-buffersize
  integer
  (>= val 1)
  (<= val 10485760))
(define-opt demuxer-lavf-format string)
(define-opt demuxer-lavf-hacks? boolean)
(define-opt
  demuxer-lavf-linearize-timestamps
  enumeration
  (memq val '(no auto yes)))
(define-opt demuxer-lavf-o list-of-key-value)
(define-opt
  demuxer-lavf-probe-info
  enumeration
  (memq val '(no yes auto nostreams)))
(define-opt
  demuxer-lavf-probescore
  integer
  (>= val 1)
  (<= val 100))
(define-opt
  demuxer-lavf-probesize
  integer
  (>= val 32)
  (<= val 2147483647))
(define-opt demuxer-lavf-propagate-opts? boolean)
(define-opt
  demuxer-max-back-bytes
  byte-size
  (>= val 0)
  (<= val 4.6116860184274e18))
(define-opt
  demuxer-max-bytes
  byte-size
  (>= val 0)
  (<= val 4.6116860184274e18))
(define-opt demuxer-mkv-crop-compat? boolean)
(define-opt
  demuxer-mkv-probe-start-time?
  boolean)
(define-opt
  demuxer-mkv-probe-video-duration
  enumeration
  (memq val '(no yes full)))
(define-opt
  demuxer-mkv-subtitle-preroll
  enumeration
  (memq val '(no yes index)))
(define-opt
  demuxer-mkv-subtitle-preroll-secs
  double
  (>= val 0))
(define-opt
  demuxer-mkv-subtitle-preroll-secs-index
  double
  (>= val 0))
(define-opt
  demuxer-rawaudio-channels
  audio-channels-or-channel-map)
(define-opt
  demuxer-rawaudio-format
  enumeration
  (memq val
        '(u8 s8
             u16le
             u16be
             s16le
             s16be
             u24le
             u24be
             s24le
             s24be
             u32le
             u32be
             s32le
             s32be
             floatle
             floatbe
             doublele
             doublebe
             u16
             s16
             u24
             s24
             u32
             s32
             float
             double)))
(define-opt
  demuxer-rawaudio-rate
  integer
  (>= val 1000)
  (<= val 384000))
(define-opt demuxer-rawvideo-codec string)
(define-opt demuxer-rawvideo-format fourcc)
(define-opt
  demuxer-rawvideo-fps
  float
  (>= val 0.001)
  (<= val 1000))
(define-opt
  demuxer-rawvideo-h
  integer
  (>= val 1)
  (<= val 8192))
(define-opt
  demuxer-rawvideo-mp-format
  image-format)
(define-opt
  demuxer-rawvideo-size
  integer
  (>= val 1)
  (<= val 268435456))
(define-opt
  demuxer-rawvideo-w
  integer
  (>= val 1)
  (<= val 8192))
(define-opt
  demuxer-readahead-secs
  double
  (>= val 0))
(define-opt
  demuxer-seekable-cache
  enumeration
  (memq val '(auto no yes)))
(define-opt demuxer-termination-timeout double)
(define-opt demuxer-thread? boolean)
(define-opt
  directory-filter-types
  list-of-string)
(define-opt
  directory-mode
  enumeration
  (memq val '(auto lazy recursive ignore)))
(define-opt
  display-fps-override
  double
  (>= val 0))
(define-opt display-tags list-of-string)
(define-opt
  dither
  enumeration
  (memq val '(fruit ordered error-diffusion no)))
(define-opt
  dither-depth
  enumeration
  (or (memq val '(no auto))
      (and (integer? val) (>= val -1) (<= val 16))))
(define-opt
  dither-size-fruit
  integer
  (>= val 2)
  (<= val 8))
(define-opt
  drag-and-drop
  enumeration
  (memq val '(no auto replace append insert-next)))
(define-opt
  dscale
  enumeration
  (memq val
        '(bilinear
          bicubic_fast
          oversample
          spline16
          spline36
          spline64
          sinc
          lanczos
          ginseng
          bicubic
          hermite
          catmull_rom
          mitchell
          robidoux
          robidouxsharp
          box
          nearest
          triangle
          gaussian
          jinc
          ewa_lanczos
          ewa_hanning
          ewa_ginseng
          ewa_lanczossharp
          ewa_lanczos4sharpest
          ewa_lanczossoft
          haasnsoft
          ewa_robidoux
          ewa_robidouxsharp
          bartlett
          cosine
          hanning
          tukey
          hamming
          quadric
          welch
          kaiser
          blackman
          sphinx)))
(define-opt
  dscale-antiring
  float
  (>= val 0)
  (<= val 1))
(define-opt dscale-blur float)
(define-opt
  dscale-clamp
  float
  (>= val 0)
  (<= val 1))
(define-opt dscale-param1 float)
(define-opt dscale-param2 float)
(define-opt
  dscale-radius
  float
  (>= val 0.5)
  (<= val 16))
(define-opt
  dscale-taper
  float
  (>= val 0)
  (<= val 1))
(define-opt
  dscale-window
  enumeration
  (memq val
        '(bartlett
          cosine
          hanning
          tukey
          hamming
          quadric
          welch
          kaiser
          blackman
          sphinx
          jinc)))
(define-opt dscale-wparam float)
(define-opt
  dscale-wtaper
  float
  (>= val 0)
  (<= val 1))
(define-opt dump-stats string)
(define-opt
  dvbin-card
  integer
  (>= val 0)
  (<= val 15))
(define-opt dvbin-channel-switch-offset integer)
(define-opt dvbin-file string)
(define-opt dvbin-full-transponder? boolean)
(define-opt dvbin-prog string)
(define-opt
  dvbin-timeout
  float
  (>= val 0)
  (<= val 3.4028234663853e38))
(define-opt
  dvd-angle
  integer
  (>= val 1)
  (<= val 99))
(define-opt dvd-device string)
(define-opt dvd-speed integer)
(define-opt
  edition
  enumeration
  (or (memq val '(auto))
      (and (integer? val) (>= val 0) (<= val 8190))))
(define-opt egl-config-id integer)
(define-opt
  egl-output-format
  enumeration
  (memq val
        '(auto rgb8
               rgba8
               rgb10
               rgb10_a2
               rgb16
               rgba16
               rgb16f
               rgba16f
               rgb32f
               rgba32f)))
(define-opt embeddedfonts? boolean)
(define-opt
  end
  relative-time-or-percent-position)
(define-opt error-diffusion string)
(define-opt external-files list-of-string)
(define-opt fbo-format string)
(define-opt
  focus-on
  enumeration
  (memq val '(never open all)))
(define-opt force-media-title string)
(define-opt force-render? boolean)
(define-opt force-rgba-osd-rendering? boolean)
(define-opt force-seekable? boolean)
(define-opt
  force-window
  enumeration
  (memq val '(no yes immediate)))
(define-opt force-window-position? boolean)
(define-opt
  framedrop
  enumeration
  (memq val '(no vo decoder decoder+vo)))
(define-opt
  frames
  enumeration
  (or (memq val '(all))
      (and (integer? val)
           (>= val 0)
           (<= val 2147483647))))
(define-opt
  fs-screen
  enumeration
  (or (memq val '(all current))
      (and (integer? val) (>= val 0) (<= val 32))))
(define-opt fs-screen-name string)
(define-opt fullscreen? boolean)
(define-opt
  gamma
  float
  (>= val -100)
  (<= val 100))
(define-opt gamma-auto? boolean)
(define-opt
  gamma-factor
  float
  (>= val 0.1)
  (<= val 2))
(define-opt
  gamut-mapping-mode
  enumeration
  (memq val
        '(auto clip
               perceptual
               relative
               saturation
               absolute
               desaturate
               darken
               warn
               linear)))
(define-opt
  gapless-audio
  enumeration
  (memq val '(no yes weak)))
(define-opt geometry window-geometry)
(define-opt glsl-shader-opts list-of-key-value)
(define-opt glsl-shaders list-of-string)
(define-opt gpu-api list-of-object-setting)
(define-opt gpu-context list-of-object-setting)
(define-opt gpu-debug? boolean)
(define-opt
  gpu-dumb-mode
  enumeration
  (memq val '(auto yes no)))
(define-opt gpu-hwdec-interop string)
(define-opt gpu-shader-cache? boolean)
(define-opt gpu-shader-cache-dir string)
(define-opt gpu-sw? boolean)
(define-opt
  gpu-tex-pad-x
  integer
  (>= val 0)
  (<= val 4096))
(define-opt
  gpu-tex-pad-y
  integer
  (>= val 0)
  (<= val 4096))
(define-opt
  hdr-compute-peak
  enumeration
  (memq val '(auto yes no)))
(define-opt
  hdr-contrast-recovery
  float
  (>= val 0)
  (<= val 2))
(define-opt
  hdr-contrast-smoothness
  float
  (>= val 1)
  (<= val 100))
(define-opt
  hdr-peak-decay-rate
  float
  (>= val 0)
  (<= val 1000))
(define-opt
  hdr-peak-percentile
  float
  (>= val 0)
  (<= val 100))
(define-opt
  hdr-scene-threshold-high
  float
  (>= val 0)
  (<= val 20))
(define-opt
  hdr-scene-threshold-low
  float
  (>= val 0)
  (<= val 20))
(define-opt hidpi-window-scale? boolean)
(define-opt
  hls-bitrate
  enumeration
  (or (memq val '(no min max))
      (and (integer? val)
           (>= val 0)
           (<= val 2147483647))))
(define-opt
  hr-seek
  enumeration
  (memq val '(no absolute yes always default)))
(define-opt hr-seek-demuxer-offset float)
(define-opt hr-seek-framedrop? boolean)
(define-opt http-header-fields list-of-string)
(define-opt http-proxy string)
(define-opt hue float (>= val -100) (<= val 100))
(define-opt hwdec list-of-string)
(define-opt hwdec-codecs string)
(define-opt
  hwdec-extra-frames
  integer
  (>= val 0)
  (<= val 256))
(define-opt hwdec-image-format image-format)
(define-opt
  hwdec-software-fallback
  enumeration
  (or (memq val '(no yes))
      (and (integer? val)
           (>= val 1)
           (<= val 2147483647))))
(define-opt icc-3dlut-size string)
(define-opt icc-cache? boolean)
(define-opt icc-cache-dir string)
(define-opt
  icc-force-contrast
  enumeration
  (or (memq val '(no inf))
      (and (integer? val) (>= val 0) (<= val 1000000))))
(define-opt icc-intent integer)
(define-opt icc-profile string)
(define-opt icc-profile-auto? boolean)
(define-opt icc-use-luma? boolean)
(define-opt
  idle
  enumeration
  (memq val '(no once yes)))
(define-opt
  ignore-path-in-watch-later-config?
  boolean)
(define-opt
  image-display-duration
  double
  (>= val 0))
(define-opt image-exts list-of-string)
(define-opt image-lut string)
(define-opt
  image-lut-type
  enumeration
  (memq val '(auto native normalized conversion)))
(define-opt image-subs-video-resolution? boolean)
(define-opt include string)
(define-opt
  index
  enumeration
  (memq val '(default recreate)))
(define-opt initial-audio-sync? boolean)
(define-opt input-ar-delay integer)
(define-opt input-ar-rate integer)
(define-opt input-builtin-bindings? boolean)
(define-opt input-builtin-dragging? boolean)
(define-opt input-commands list-of-string)
(define-opt input-conf string)
(define-opt input-cursor? boolean)
(define-opt input-cursor-passthrough? boolean)
(define-opt input-default-bindings? boolean)
(define-opt
  input-doubleclick-time
  integer
  (>= val 0)
  (<= val 1000))
(define-opt input-dragging-deadzone integer)
(define-opt input-ime? boolean)
(define-opt input-ipc-client string)
(define-opt input-ipc-server string)
(define-opt
  input-key-fifo-size
  integer
  (>= val 2)
  (<= val 65000))
(define-opt input-media-keys? boolean)
(define-opt input-preprocess-wheel? boolean)
(define-opt input-right-alt-gr? boolean)
(define-opt input-terminal? boolean)
(define-opt input-test? boolean)
(define-opt input-touch-emulate-mouse? boolean)
(define-opt input-vo-keyboard? boolean)
(define-opt interpolation? boolean)
(define-opt interpolation-preserve? boolean)
(define-opt interpolation-threshold float)
(define-opt inverse-tone-mapping? boolean)
(define-opt jack-autostart? boolean)
(define-opt jack-connect? boolean)
(define-opt jack-name string)
(define-opt jack-port string)
(define-opt
  jack-std-channel-layout
  enumeration
  (memq val '(waveext any)))
(define-opt
  keep-open
  enumeration
  (memq val '(no yes always)))
(define-opt keep-open-pause? boolean)
(define-opt keepaspect? boolean)
(define-opt keepaspect-window? boolean)
(define-opt lavfi-complex string)
(define-opt
  length
  relative-time-or-percent-position)
(define-opt libplacebo-opts list-of-key-value)
(define-opt linear-downscaling? boolean)
(define-opt linear-upscaling? boolean)
(define-opt
  load-auto-profiles
  enumeration
  (memq val '(no yes auto)))
(define-opt load-commands? boolean)
(define-opt load-console? boolean)
(define-opt load-positioning? boolean)
(define-opt load-scripts? boolean)
(define-opt load-select? boolean)
(define-opt load-stats-overlay? boolean)
(define-opt load-unsafe-playlists? boolean)
(define-opt log-file string)
(define-opt
  loop-file
  enumeration
  (or (memq val '(no inf yes))
      (and (integer? val) (>= val 0) (<= val 10000))))
(define-opt
  loop-playlist
  enumeration
  (or (memq val '(no inf yes force))
      (and (integer? val) (>= val 1) (<= val 10000))))
(define-opt lut string)
(define-opt
  lut-type
  enumeration
  (memq val '(auto native normalized conversion)))
(define-opt mc float (>= val 0) (<= val 100))
(define-opt media-controls? boolean)
(define-opt merge-files? boolean)
(define-opt metadata-codepage string)
(define-opt mf-fps double)
(define-opt mf-type string)
(define-opt
  monitoraspect
  float
  (>= val 0)
  (<= val 9))
(define-opt
  monitorpixelaspect
  float
  (>= val 0.03125)
  (<= val 32))
(define-opt msg-color? boolean)
(define-opt msg-level list-of-output-verbosity)
(define-opt msg-module? boolean)
(define-opt msg-time? boolean)
(define-opt mute? boolean)
(define-opt native-fs? boolean)
(define-opt native-keyrepeat? boolean)
(define-opt native-touch? boolean)
(define-opt network-timeout double (>= val 0))
(define-opt oac string)
(define-opt oacopts list-of-key-value)
(define-opt ocopy-metadata? boolean)
(define-opt of string)
(define-opt ofopts list-of-key-value)
(define-opt on-all-workspaces? boolean)
(define-opt ontop? boolean)
(define-opt
  ontop-level
  enumeration
  (or (memq val '(window system desktop))
      (and (integer? val)
           (>= val 0)
           (<= val 2147483647))))
(define-opt opengl-check-pattern-a integer)
(define-opt opengl-check-pattern-b integer)
(define-opt
  opengl-early-flush
  enumeration
  (memq val '(no yes auto)))
(define-opt
  opengl-es
  enumeration
  (memq val '(auto yes no)))
(define-opt opengl-glfinish? boolean)
(define-opt opengl-pbo? boolean)
(define-opt opengl-rectangle-textures? boolean)
(define-opt opengl-swapinterval integer)
(define-opt opengl-waitvsync? boolean)
(define-opt orawts? boolean)
(define-opt ordered-chapters? boolean)
(define-opt ordered-chapters-files string)
(define-opt oremove-metadata list-of-string)
(define-opt osc? boolean)
(define-opt
  osd-align-x
  enumeration
  (memq val '(left center right)))
(define-opt
  osd-align-y
  enumeration
  (memq val '(top center bottom)))
(define-opt osd-back-color color)
(define-opt osd-bar? boolean)
(define-opt
  osd-bar-align-x
  float
  (>= val -1)
  (<= val 1))
(define-opt
  osd-bar-align-y
  float
  (>= val -1)
  (<= val 1))
(define-opt
  osd-bar-h
  float
  (>= val 0.1)
  (<= val 50))
(define-opt
  osd-bar-marker-min-size
  float
  (>= val 0)
  (<= val 1000))
(define-opt
  osd-bar-marker-scale
  float
  (>= val 0)
  (<= val 100))
(define-opt
  osd-bar-marker-style
  enumeration
  (memq val '(none triangle line)))
(define-opt
  osd-bar-outline-size
  float
  (>= val 0)
  (<= val 1000))
(define-opt
  osd-bar-w
  float
  (>= val 1)
  (<= val 100))
(define-opt
  osd-blur
  float
  (>= val 0)
  (<= val 20))
(define-opt osd-bold? boolean)
(define-opt
  osd-border-style
  enumeration
  (memq val
        '(outline-and-shadow opaque-box background-box)))
(define-opt osd-color color)
(define-opt
  osd-duration
  integer
  (>= val 0)
  (<= val 3600000))
(define-opt osd-font string)
(define-opt
  osd-font-provider
  enumeration
  (memq val '(auto none fontconfig)))
(define-opt
  osd-font-size
  float
  (>= val 1)
  (<= val 9000))
(define-opt osd-fonts-dir string)
(define-opt osd-fractions? boolean)
(define-opt osd-italic? boolean)
(define-opt
  osd-justify
  enumeration
  (memq val '(auto left center right)))
(define-opt
  osd-level
  enumeration
  (memq val '(#{0}# #{1}# #{2}# #{3}#)))
(define-opt
  osd-margin-x
  integer
  (>= val 0)
  (<= val 300))
(define-opt
  osd-margin-y
  integer
  (>= val 0)
  (<= val 600))
(define-opt osd-msg1 string)
(define-opt osd-msg2 string)
(define-opt osd-msg3 string)
(define-opt
  osd-on-seek
  enumeration
  (memq val '(no bar msg msg-bar)))
(define-opt osd-outline-color color)
(define-opt osd-outline-size float)
(define-opt osd-playing-msg string)
(define-opt
  osd-playing-msg-duration
  integer
  (>= val 0)
  (<= val 3600000))
(define-opt
  osd-playlist-entry
  enumeration
  (memq val '(title filename both)))
(define-opt
  osd-scale
  float
  (>= val 0)
  (<= val 100))
(define-opt osd-scale-by-window? boolean)
(define-opt osd-selected-color color)
(define-opt osd-selected-outline-color color)
(define-opt osd-shadow-offset float)
(define-opt
  osd-spacing
  float
  (>= val -10)
  (<= val 10))
(define-opt osd-status-msg string)
(define-opt oset-metadata list-of-key-value)
(define-opt ovc string)
(define-opt ovcopts list-of-key-value)
(define-opt panscan float (>= val 0) (<= val 1))
(define-opt pause? boolean)
(define-opt
  pipewire-buffer
  enumeration
  (or (memq val '(native))
      (and (integer? val) (>= val 1) (<= val 2000))))
(define-opt pipewire-remote string)
(define-opt
  pipewire-volume-mode
  enumeration
  (memq val '(channel global)))
(define-opt
  pitch
  double
  (>= val 0.01)
  (<= val 100))
(define-opt
  play-direction
  enumeration
  (memq val '(forward + backward -)))
(define-opt
  player-operation-mode
  enumeration
  (memq val '(cplayer pseudo-gui)))
(define-opt playlist-exts list-of-string)
(define-opt
  playlist-start
  enumeration
  (or (memq val '(auto no))
      (and (integer? val)
           (>= val 0)
           (<= val 2147483647))))
(define-opt prefetch-playlist? boolean)
(define-opt profile list-of-string)
(define-opt pulse-allow-suspended? boolean)
(define-opt
  pulse-buffer
  enumeration
  (or (memq val '(native))
      (and (integer? val) (>= val 1) (<= val 2000))))
(define-opt pulse-host string)
(define-opt pulse-latency-hacks? boolean)
(define-opt quiet? boolean)
(define-opt really-quiet? boolean)
(define-opt rebase-start-time? boolean)
(define-opt referrer string)
(define-opt
  replaygain
  enumeration
  (memq val '(no track album)))
(define-opt replaygain-clip? boolean)
(define-opt
  replaygain-fallback
  float
  (>= val -200)
  (<= val 60))
(define-opt
  replaygain-preamp
  float
  (>= val -150)
  (<= val 150))
(define-opt reset-on-next-file list-of-string)
(define-opt resume-playback? boolean)
(define-opt resume-playback-check-mtime? boolean)
(define-opt
  rtsp-transport
  enumeration
  (memq val '(lavf udp tcp http udp_multicast)))
(define-opt
  saturation
  float
  (>= val -100)
  (<= val 100))
(define-opt save-position-on-quit? boolean)
(define-opt save-watch-history? boolean)
(define-opt
  scale
  enumeration
  (memq val
        '(bilinear
          bicubic_fast
          oversample
          spline16
          spline36
          spline64
          sinc
          lanczos
          ginseng
          bicubic
          hermite
          catmull_rom
          mitchell
          robidoux
          robidouxsharp
          box
          nearest
          triangle
          gaussian
          jinc
          ewa_lanczos
          ewa_hanning
          ewa_ginseng
          ewa_lanczossharp
          ewa_lanczos4sharpest
          ewa_lanczossoft
          haasnsoft
          ewa_robidoux
          ewa_robidouxsharp
          bartlett
          cosine
          hanning
          tukey
          hamming
          quadric
          welch
          kaiser
          blackman
          sphinx)))
(define-opt
  scale-antiring
  float
  (>= val 0)
  (<= val 1))
(define-opt scale-blur float)
(define-opt
  scale-clamp
  float
  (>= val 0)
  (<= val 1))
(define-opt scale-param1 float)
(define-opt scale-param2 float)
(define-opt
  scale-radius
  float
  (>= val 0.5)
  (<= val 16))
(define-opt
  scale-taper
  float
  (>= val 0)
  (<= val 1))
(define-opt
  scale-window
  enumeration
  (memq val
        '(bartlett
          cosine
          hanning
          tukey
          hamming
          quadric
          welch
          kaiser
          blackman
          sphinx
          jinc)))
(define-opt scale-wparam float)
(define-opt
  scale-wtaper
  float
  (>= val 0)
  (<= val 1))
(define-opt scaler-resizes-only? boolean)
(define-opt
  screen
  enumeration
  (or (memq val '(default))
      (and (integer? val) (>= val 0) (<= val 32))))
(define-opt screen-name string)
(define-opt screenshot-avif-encoder string)
(define-opt
  screenshot-avif-opts
  list-of-key-value)
(define-opt screenshot-avif-pixfmt string)
(define-opt screenshot-directory string)
(define-opt
  screenshot-format
  enumeration
  (memq val '(jpg jpeg png webp jxl avif)))
(define-opt screenshot-high-bit-depth? boolean)
(define-opt
  screenshot-jpeg-quality
  integer
  (>= val 0)
  (<= val 100))
(define-opt
  screenshot-jpeg-source-chroma?
  boolean)
(define-opt
  screenshot-jxl-distance
  double
  (>= val 0)
  (<= val 15))
(define-opt
  screenshot-jxl-effort
  integer
  (>= val 1)
  (<= val 9))
(define-opt
  screenshot-png-compression
  integer
  (>= val 0)
  (<= val 9))
(define-opt
  screenshot-png-filter
  integer
  (>= val 0)
  (<= val 5))
(define-opt screenshot-sw? boolean)
(define-opt screenshot-tag-colorspace? boolean)
(define-opt screenshot-template string)
(define-opt
  screenshot-webp-compression
  integer
  (>= val 0)
  (<= val 6))
(define-opt screenshot-webp-lossless? boolean)
(define-opt
  screenshot-webp-quality
  integer
  (>= val 0)
  (<= val 100))
(define-opt script-opts list-of-key-value)
(define-opt scripts list-of-string)
(define-opt
  secondary-sid
  enumeration
  (or (memq val '(no auto))
      (and (integer? val) (>= val 0) (<= val 8190))))
(define-opt
  secondary-sub-ass-override
  enumeration
  (memq val '(no yes scale force strip)))
(define-opt secondary-sub-delay float)
(define-opt
  secondary-sub-pos
  float
  (>= val 0)
  (<= val 150))
(define-opt secondary-sub-visibility? boolean)
(define-opt sharpen float)
(define-opt show-in-taskbar? boolean)
(define-opt shuffle? boolean)
(define-opt
  sub
  enumeration
  (or (memq val '(no auto))
      (and (integer? val) (>= val 0) (<= val 8190))))
(define-opt
  sigmoid-center
  float
  (>= val 0)
  (<= val 1))
(define-opt
  sigmoid-slope
  float
  (>= val 1)
  (<= val 20))
(define-opt sigmoid-upscaling? boolean)
(define-opt slang list-of-string)
(define-opt snap-window? boolean)
(define-opt
  speed
  double
  (>= val 0.01)
  (<= val 100))
(define-opt
  spirv-compiler
  enumeration
  (memq val '(auto)))
(define-opt sstep double (>= val 0))
(define-opt
  start
  relative-time-or-percent-position)
(define-opt
  stop-playback-on-init-failure?
  boolean)
(define-opt
  stop-screensaver
  enumeration
  (memq val '(no yes always)))
(define-opt
  stream-buffer-size
  byte-size
  (>= val 4096)
  (<= val 536870912))
(define-opt stream-dump string)
(define-opt stream-lavf-o list-of-key-value)
(define-opt stream-record string)
(define-opt stretch-dvd-subs? boolean)
(define-opt
  stretch-image-subs-to-screen?
  boolean)
(define-opt
  sub-align-x
  enumeration
  (memq val '(left center right)))
(define-opt
  sub-align-y
  enumeration
  (memq val '(top center bottom)))
(define-opt sub-ass? boolean)
(define-opt sub-ass-force-margins? boolean)
(define-opt sub-ass-justify? boolean)
(define-opt
  sub-ass-override
  enumeration
  (memq val '(no yes scale force strip)))
(define-opt
  sub-ass-prune-delay
  double
  (>= val -1))
(define-opt sub-ass-scale-with-window? boolean)
(define-opt
  sub-ass-style-overrides
  list-of-string)
(define-opt sub-ass-styles string)
(define-opt
  sub-ass-use-video-data
  enumeration
  (memq val '(none aspect-ratio all)))
(define-opt
  sub-ass-video-aspect-override
  aspect
  (>= val 0)
  (<= val 10))
(define-opt
  sub-ass-vsfilter-color-compat
  enumeration
  (memq val '(no basic full force-601)))
(define-opt
  sub-auto
  enumeration
  (memq val '(no exact fuzzy all)))
(define-opt sub-auto-exts list-of-string)
(define-opt sub-back-color color)
(define-opt
  sub-blur
  float
  (>= val 0)
  (<= val 20))
(define-opt sub-bold? boolean)
(define-opt
  sub-border-style
  enumeration
  (memq val
        '(outline-and-shadow opaque-box background-box)))
(define-opt sub-clear-on-seek? boolean)
(define-opt sub-codepage string)
(define-opt sub-color color)
(define-opt sub-create-cc-track? boolean)
(define-opt sub-delay float)
(define-opt sub-demuxer string)
(define-opt sub-file-paths list-of-string)
(define-opt sub-files list-of-string)
(define-opt sub-filter-jsre list-of-string)
(define-opt sub-filter-regex list-of-string)
(define-opt sub-filter-regex-enable? boolean)
(define-opt sub-filter-regex-plain? boolean)
(define-opt sub-filter-regex-warn? boolean)
(define-opt sub-filter-sdh? boolean)
(define-opt sub-filter-sdh-enclosures string)
(define-opt sub-filter-sdh-harder? boolean)
(define-opt sub-fix-timing? boolean)
(define-opt sub-font string)
(define-opt
  sub-font-provider
  enumeration
  (memq val '(auto none fontconfig)))
(define-opt
  sub-font-size
  float
  (>= val 1)
  (<= val 9000))
(define-opt sub-fonts-dir string)
(define-opt sub-forced-events-only? boolean)
(define-opt sub-fps float)
(define-opt
  sub-gauss
  float
  (>= val 0)
  (<= val 3))
(define-opt sub-gray? boolean)
(define-opt
  sub-hinting
  enumeration
  (memq val '(none light normal native)))
(define-opt sub-italic? boolean)
(define-opt
  sub-justify
  enumeration
  (memq val '(auto left center right)))
(define-opt sub-lavc-o list-of-key-value)
(define-opt
  sub-line-spacing
  float
  (>= val -1000)
  (<= val 1000))
(define-opt
  sub-margin-x
  integer
  (>= val 0)
  (<= val 300))
(define-opt
  sub-margin-y
  integer
  (>= val 0)
  (<= val 600))
(define-opt sub-outline-color color)
(define-opt sub-outline-size float)
(define-opt sub-past-video-end? boolean)
(define-opt
  sub-pos
  float
  (>= val 0)
  (<= val 150))
(define-opt
  sub-scale
  float
  (>= val 0)
  (<= val 100))
(define-opt sub-scale-by-window? boolean)
(define-opt sub-scale-signs? boolean)
(define-opt sub-scale-with-window? boolean)
(define-opt sub-shadow-offset float)
(define-opt
  sub-shaper
  enumeration
  (memq val '(simple complex)))
(define-opt
  sub-spacing
  float
  (>= val -10)
  (<= val 10))
(define-opt sub-speed float)
(define-opt sub-stretch-durations? boolean)
(define-opt sub-use-margins? boolean)
(define-opt sub-visibility? boolean)
(define-opt sub-vsfilter-bidi-compat? boolean)
(define-opt
  subs-fallback
  enumeration
  (memq val '(no default yes)))
(define-opt
  subs-fallback-forced
  enumeration
  (memq val '(no yes always)))
(define-opt subs-match-os-language? boolean)
(define-opt
  subs-with-matching-audio
  enumeration
  (memq val '(no forced yes)))
(define-opt
  swapchain-depth
  integer
  (>= val 1)
  (<= val 8))
(define-opt sws-allow-zimg? boolean)
(define-opt sws-bitexact? boolean)
(define-opt
  sws-cgb
  float
  (>= val 0)
  (<= val 100))
(define-opt sws-chs integer)
(define-opt
  sws-cs
  float
  (>= val -100)
  (<= val 100))
(define-opt sws-cvs integer)
(define-opt sws-fast? boolean)
(define-opt
  sws-lgb
  float
  (>= val 0)
  (<= val 100))
(define-opt
  sws-ls
  float
  (>= val -100)
  (<= val 100))
(define-opt
  sws-scaler
  enumeration
  (memq val
        '(fast-bilinear
          bilinear
          bicubic
          x
          point
          area
          bicublin
          gauss
          sinc
          lanczos
          spline)))
(define-opt
  target-colorspace-hint
  enumeration
  (memq val '(auto no yes)))
(define-opt
  target-contrast
  enumeration
  (or (memq val '(auto inf))
      (and (integer? val) (>= val 10) (<= val 1000000))))
(define-opt
  target-gamut
  enumeration
  (memq val
        '(auto bt.601-525
               bt.601-625
               bt.709
               bt.2020
               bt.470m
               apple
               adobe
               prophoto
               cie1931
               dci-p3
               display-p3
               v-gamut
               s-gamut
               ebu3213
               film-c
               aces-ap0
               aces-ap1)))
(define-opt target-lut string)
(define-opt
  target-peak
  enumeration
  (or (memq val '(auto))
      (and (integer? val) (>= val 10) (<= val 10000))))
(define-opt
  target-prim
  enumeration
  (memq val
        '(auto bt.601-525
               bt.601-625
               bt.709
               bt.2020
               bt.470m
               apple
               adobe
               prophoto
               cie1931
               dci-p3
               display-p3
               v-gamut
               s-gamut
               ebu3213
               film-c
               aces-ap0
               aces-ap1)))
(define-opt
  target-trc
  enumeration
  (memq val
        '(auto bt.1886
               srgb
               linear
               gamma1.8
               gamma2.0
               gamma2.2
               gamma2.4
               gamma2.6
               gamma2.8
               prophoto
               pq
               hlg
               v-log
               s-log1
               s-log2
               st428)))
(define-opt taskbar-progress? boolean)
(define-opt
  teletext-page
  integer
  (>= val -1)
  (<= val 999))
(define-opt temporal-dither? boolean)
(define-opt
  temporal-dither-period
  integer
  (>= val 1)
  (<= val 128))
(define-opt
  term-osd
  enumeration
  (memq val '(force auto no)))
(define-opt term-osd-bar? boolean)
(define-opt term-osd-bar-chars string)
(define-opt term-playing-msg string)
(define-opt term-status-msg string)
(define-opt term-title string)
(define-opt terminal? boolean)
(define-opt title string)
(define-opt title-bar? boolean)
(define-opt tls-ca-file string)
(define-opt tls-cert-file string)
(define-opt tls-key-file string)
(define-opt tls-verify? boolean)
(define-opt
  tone-mapping
  enumeration
  (memq val
        '(auto clip
               mobius
               reinhard
               hable
               gamma
               linear
               spline
               bt.2390
               bt.2446a
               st2094-40
               st2094-10)))
(define-opt
  tone-mapping-max-boost
  float
  (>= val 1)
  (<= val 10))
(define-opt tone-mapping-param float)
(define-opt tone-mapping-visualize? boolean)
(define-opt track-auto-selection? boolean)
(define-opt
  tscale
  enumeration
  (memq val
        '(oversample
          linear
          spline16
          spline36
          spline64
          sinc
          lanczos
          ginseng
          bicubic
          hermite
          catmull_rom
          mitchell
          robidoux
          robidouxsharp
          box
          nearest
          triangle
          gaussian
          bartlett
          cosine
          hanning
          tukey
          hamming
          quadric
          welch
          kaiser
          blackman
          sphinx
          jinc)))
(define-opt
  tscale-antiring
  float
  (>= val 0)
  (<= val 1))
(define-opt tscale-blur float)
(define-opt
  tscale-clamp
  float
  (>= val 0)
  (<= val 1))
(define-opt tscale-param1 float)
(define-opt tscale-param2 float)
(define-opt
  tscale-radius
  float
  (>= val 0.5)
  (<= val 16))
(define-opt
  tscale-taper
  float
  (>= val 0)
  (<= val 1))
(define-opt
  tscale-window
  enumeration
  (memq val
        '(bartlett
          cosine
          hanning
          tukey
          hamming
          quadric
          welch
          kaiser
          blackman
          sphinx
          jinc)))
(define-opt tscale-wparam float)
(define-opt
  tscale-wtaper
  float
  (>= val 0)
  (<= val 1))
(define-opt untimed? boolean)
(define-opt use-embedded-icc-profile? boolean)
(define-opt use-filedir-conf? boolean)
(define-opt user-agent string)
(define-opt vaapi-device string)
(define-opt vd string)
(define-opt vd-apply-cropping? boolean)
(define-opt vd-lavc-assume-old-x264? boolean)
(define-opt vd-lavc-bitexact? boolean)
(define-opt vd-lavc-check-hw-profile? boolean)
(define-opt
  vd-lavc-dr
  enumeration
  (memq val '(auto no yes)))
(define-opt vd-lavc-fast? boolean)
(define-opt
  vd-lavc-film-grain
  enumeration
  (memq val '(auto cpu gpu)))
(define-opt
  vd-lavc-framedrop
  enumeration
  (memq val
        '(none default nonref bidir nonkey all)))
(define-opt vd-lavc-o list-of-key-value)
(define-opt vd-lavc-show-all? boolean)
(define-opt
  vd-lavc-skipframe
  enumeration
  (memq val
        '(none default nonref bidir nonkey all)))
(define-opt
  vd-lavc-skipidct
  enumeration
  (memq val
        '(none default nonref bidir nonkey all)))
(define-opt
  vd-lavc-skiploopfilter
  enumeration
  (memq val
        '(none default nonref bidir nonkey all)))
(define-opt vd-lavc-threads integer (>= val 0))
(define-opt vd-queue-enable? boolean)
(define-opt
  vd-queue-max-bytes
  byte-size
  (>= val 0)
  (<= val 4.6116860184274e18))
(define-opt
  vd-queue-max-samples
  integer64
  (>= val 0))
(define-opt vd-queue-max-secs double (>= val 0))
(define-opt vf list-of-object-setting)
(define-opt
  video
  enumeration
  (or (memq val '(no auto))
      (and (integer? val) (>= val 0) (<= val 8190))))
(define-opt
  video-align-x
  float
  (>= val -1)
  (<= val 1))
(define-opt
  video-align-y
  float
  (>= val -1)
  (<= val 1))
(define-opt
  video-aspect-method
  enumeration
  (memq val '(bitstream container ignore)))
(define-opt
  video-aspect-override
  aspect
  (>= val -2)
  (<= val 10))
(define-opt
  video-backward-batch
  integer
  (>= val 0)
  (<= val 1024))
(define-opt
  video-backward-overlap
  enumeration
  (or (memq val '(auto))
      (and (integer? val) (>= val 0) (<= val 1024))))
(define-opt video-crop video-rectangle)
(define-opt video-exts list-of-string)
(define-opt video-latency-hacks? boolean)
(define-opt
  video-margin-ratio-bottom
  float
  (>= val 0)
  (<= val 1))
(define-opt
  video-margin-ratio-left
  float
  (>= val 0)
  (<= val 1))
(define-opt
  video-margin-ratio-right
  float
  (>= val 0)
  (<= val 1))
(define-opt
  video-margin-ratio-top
  float
  (>= val 0)
  (<= val 1))
(define-opt video-osd? boolean)
(define-opt
  video-output-levels
  enumeration
  (memq val '(auto limited full)))
(define-opt video-pan-x float)
(define-opt video-pan-y float)
(define-opt video-recenter? boolean)
(define-opt
  video-reversal-buffer
  byte-size
  (>= val 0)
  (<= val 4.6116860184274e18))
(define-opt
  video-rotate
  enumeration
  (or (memq val '(no))
      (and (integer? val) (>= val 0) (<= val 359))))
(define-opt
  video-scale-x
  float
  (>= val 0)
  (<= val 10000))
(define-opt
  video-scale-y
  float
  (>= val 0)
  (<= val 10000))
(define-opt
  video-sync
  enumeration
  (memq val
        '(audio display-resample
                display-resample-vdrop
                display-resample-desync
                display-tempo
                display-adrop
                display-vdrop
                display-desync
                desync)))
(define-opt
  video-sync-max-audio-change
  double
  (>= val 0)
  (<= val 1))
(define-opt
  video-sync-max-factor
  integer
  (>= val 1)
  (<= val 10))
(define-opt
  video-sync-max-video-change
  double
  (>= val 0))
(define-opt
  video-timing-offset
  double
  (>= val 0)
  (<= val 1))
(define-opt
  video-unscaled
  enumeration
  (memq val '(no yes downscale-big)))
(define-opt
  video-zoom
  float
  (>= val -20)
  (<= val 20))
(define-opt vlang list-of-string)
(define-opt vo list-of-object-setting)
(define-opt vo-image-avif-encoder string)
(define-opt vo-image-avif-opts list-of-key-value)
(define-opt vo-image-avif-pixfmt string)
(define-opt
  vo-image-format
  enumeration
  (memq val '(jpg jpeg png webp jxl avif)))
(define-opt vo-image-high-bit-depth? boolean)
(define-opt
  vo-image-jpeg-quality
  integer
  (>= val 0)
  (<= val 100))
(define-opt vo-image-jpeg-source-chroma? boolean)
(define-opt
  vo-image-jxl-distance
  double
  (>= val 0)
  (<= val 15))
(define-opt
  vo-image-jxl-effort
  integer
  (>= val 1)
  (<= val 9))
(define-opt vo-image-outdir string)
(define-opt
  vo-image-png-compression
  integer
  (>= val 0)
  (<= val 9))
(define-opt
  vo-image-png-filter
  integer
  (>= val 0)
  (<= val 5))
(define-opt vo-image-tag-colorspace? boolean)
(define-opt
  vo-image-webp-compression
  integer
  (>= val 0)
  (<= val 6))
(define-opt vo-image-webp-lossless? boolean)
(define-opt
  vo-image-webp-quality
  integer
  (>= val 0)
  (<= val 100))
(define-opt vo-kitty-alt-screen? boolean)
(define-opt vo-kitty-cols integer)
(define-opt vo-kitty-config-clear? boolean)
(define-opt vo-kitty-height integer)
(define-opt vo-kitty-left integer)
(define-opt vo-kitty-rows integer)
(define-opt vo-kitty-top integer)
(define-opt vo-kitty-use-shm? boolean)
(define-opt vo-kitty-width integer)
(define-opt
  vo-null-fps
  double
  (>= val 0)
  (<= val 10000))
(define-opt vo-sixel-alt-screen? boolean)
(define-opt vo-sixel-buffered? boolean)
(define-opt vo-sixel-cols integer)
(define-opt vo-sixel-config-clear? boolean)
(define-opt
  vo-sixel-dither
  enumeration
  (memq val
        '(auto none
               atkinson
               fs
               jajuni
               stucki
               burkes
               arithmetic
               xor)))
(define-opt vo-sixel-fixedpalette? boolean)
(define-opt vo-sixel-height integer)
(define-opt vo-sixel-left integer)
(define-opt vo-sixel-pad-x integer)
(define-opt vo-sixel-pad-y integer)
(define-opt vo-sixel-reqcolors integer)
(define-opt vo-sixel-rows integer)
(define-opt vo-sixel-threshold integer)
(define-opt vo-sixel-top integer)
(define-opt vo-sixel-width integer)
(define-opt vo-tct-256? boolean)
(define-opt
  vo-tct-algo
  enumeration
  (memq val '(plain half-blocks)))
(define-opt
  vo-tct-buffering
  enumeration
  (memq val '(pixel line frame)))
(define-opt vo-tct-height integer)
(define-opt vo-tct-width integer)
(define-opt vo-vaapi-scaled-osd? boolean)
(define-opt
  vo-vaapi-scaling
  enumeration
  (memq val '(default fast hq nla)))
(define-opt vo-vdpau-chroma-deint? boolean)
(define-opt vo-vdpau-colorkey color)
(define-opt vo-vdpau-composite-detect? boolean)
(define-opt
  vo-vdpau-denoise
  float
  (>= val 0)
  (<= val 1))
(define-opt vo-vdpau-force-yuv? boolean)
(define-opt vo-vdpau-fps double)
(define-opt
  vo-vdpau-hqscaling
  integer
  (>= val 0)
  (<= val 9))
(define-opt
  vo-vdpau-output-surfaces
  integer
  (>= val 2)
  (<= val 15))
(define-opt vo-vdpau-pullup? boolean)
(define-opt vo-vdpau-queuetime-fs integer)
(define-opt vo-vdpau-queuetime-windowed integer)
(define-opt
  vo-vdpau-sharpen
  float
  (>= val -1)
  (<= val 1))
(define-opt
  volume
  float
  (>= val -1)
  (<= val 1000))
(define-opt
  volume-gain
  float
  (>= val -150)
  (<= val 150))
(define-opt
  volume-gain-max
  float
  (>= val 0)
  (<= val 150))
(define-opt
  volume-gain-min
  float
  (>= val -150)
  (<= val 0))
(define-opt
  volume-max
  float
  (>= val 100)
  (<= val 1000))
(define-opt vulkan-async-compute? boolean)
(define-opt vulkan-async-transfer? boolean)
(define-opt vulkan-device string)
(define-opt vulkan-display-display integer)
(define-opt vulkan-display-mode integer)
(define-opt vulkan-display-plane integer)
(define-opt
  vulkan-queue-count
  integer
  (>= val 1)
  (<= val 8))
(define-opt
  vulkan-swap-mode
  enumeration
  (memq val
        '(auto fifo fifo-relaxed mailbox immediate)))
(define-opt watch-history-path string)
(define-opt watch-later-directory string)
(define-opt watch-later-options list-of-string)
(define-opt wayland-app-id string)
(define-opt
  wayland-configure-bounds
  enumeration
  (memq val '(auto no yes)))
(define-opt
  wayland-content-type
  enumeration
  (memq val '(auto none photo video game)))
(define-opt wayland-disable-vsync? boolean)
(define-opt
  wayland-edge-pixels-pointer
  integer
  (>= val 0)
  (<= val 2147483647))
(define-opt
  wayland-edge-pixels-touch
  integer
  (>= val 0)
  (<= val 2147483647))
(define-opt
  wayland-internal-vsync
  enumeration
  (memq val '(no auto yes)))
(define-opt wayland-present? boolean)
(define-opt wid integer64)
(define-opt window-dragging? boolean)
(define-opt window-maximized? boolean)
(define-opt window-minimized? boolean)
(define-opt
  window-scale
  double
  (>= val 0.001)
  (<= val 100))
(define-opt
  write-filename-in-watch-later-config?
  boolean)
(define-opt
  x11-bypass-compositor
  enumeration
  (memq val '(no yes fs-only never)))
(define-opt x11-name string)
(define-opt
  x11-netwm
  enumeration
  (memq val '(auto no yes)))
(define-opt
  x11-present
  enumeration
  (memq val '(no auto yes)))
(define-opt x11-wid-title? boolean)
(define-opt xv-adaptor integer (>= val -1))
(define-opt
  xv-buffers
  integer
  (>= val 1)
  (<= val 10))
(define-opt
  xv-ck
  enumeration
  (memq val '(use set cur)))
(define-opt
  xv-ck-method
  enumeration
  (memq val '(none bg man auto)))
(define-opt xv-colorkey integer)
(define-opt xv-port integer (>= val 0))
(define-opt ytdl? boolean)
(define-opt ytdl-format string)
(define-opt ytdl-raw-options list-of-key-value)
(define-opt
  zimg-dither
  enumeration
  (memq val '(no ordered random error-diffusion)))
(define-opt zimg-fast? boolean)
(define-opt
  zimg-scaler
  enumeration
  (memq val
        '(point bilinear
                bicubic
                spline16
                spline36
                lanczos)))
(define-opt
  zimg-scaler-chroma
  enumeration
  (memq val
        '(point bilinear
                bicubic
                spline16
                spline36
                lanczos)))
(define-opt zimg-scaler-chroma-param-a double)
(define-opt zimg-scaler-chroma-param-b double)
(define-opt zimg-scaler-param-a double)
(define-opt zimg-scaler-param-b double)
(define-opt
  zimg-threads
  enumeration
  (or (memq val '(auto))
      (and (integer? val) (>= val 1) (<= val 64))))
;;; Generated code - END.

(define-record-type <mpv-profile-configuration>
  (%make-mpv-profile-configuration data)
  mpv-profile-configuration?
  (data %mpv-profile-configuration-data))

(define (make-mpv-profile-configuration . args)
  ;; I am not sure how can I copy a hash-map.  Documentation does not mention
  ;; anything.
  (let ((new (make-hash-table)))
    (let loop ((args args))
      (match args
        ((#:inherit cfg . tail)
         (hash-for-each (lambda (key val)
                          (hashq-set! new key val))
                        (%mpv-profile-configuration-data cfg))
         (loop tail))
        (((? keyword? key) val . tail)
         (let* ((key (keyword->symbol key))
                (opt (hashq-ref %opts key)))
           (unless opt
             (raise-exception
              (formatted-message
               (G_ "option ~a not found for mpv-profile-configuration") key)))
           (if (maybe-value-set? val)
               (if ((profile-option-type-check opt) val)
                   (hashq-set! new key (cons val
                                             (profile-option-serializer opt)))
                   (raise-exception
                    (formatted-message
                     (G_ "invalid mpv configuration for ~a: ~a~%")
                     key val)))
               (hashq-remove! new key)))
         (loop tail))
        (()
         (%make-mpv-profile-configuration new))))))

(define (serialize-mpv-profile-configuration _ cfg)
  (let ((sorted (sort
                 (hash-map->list cons (%mpv-profile-configuration-data cfg))
                 (lambda (a b)
                   (string<? (symbol->string (car a))
                             (symbol->string (car b)))))))
    #~(string-append
       #$@(map (match-lambda
                 ((field-name . value)
                  ((cdr value) field-name (car value))))
               sorted))))




;;;
;;; Configuration base.
;;;
(define (serialize-mpv/mpv-profile-configurations _ profiles)
  #~(string-append
     #$@(map (match-lambda
               ((name . config)
                #~(string-append
                   #$(format #f "[~a]~%" name)
                   #$(serialize-mpv-profile-configuration _ config))))
             profiles)))
(define (mpv/mpv-profile-configurations? alist)
  (and (list? alist)
       (every (match-lambda
                (((? symbol?) . (? mpv-profile-configuration?)) #t)
                (_ #f))
              alist)))

(define (serialize-mpv/extra _ value)
  (if value
      #~(string-append #$value
                       ;; Ensure the extra content ends in a new line.
                       #$(if (string-suffix? "\n" value)
                             "" "\n"))
      #~""))
(define (mpv/extra? val)
  (or (string? val)
      (gexp? val)))

(define-record-type <home-mpv-configuration>
  (%make-home-mpv-configuration global profiles extra-config)
  home-mpv-configuration?
  (global       home-mpv-configuration-global)
  (profiles     home-mpv-configuration-profiles)
  (extra-config home-mpv-configuration-extra-config))

(define* (make-home-mpv-configuration
          #:key
          (inherit #f)
          (global (if inherit
                      (home-mpv-configuration-global inherit)
                      (make-mpv-profile-configuration)))
          (profiles (if inherit
                        (home-mpv-configuration-profiles inherit)
                        '()))
          (extra-config (if inherit
                            (home-mpv-configuration-extra-config inherit)
                            #f)))
  (unless (mpv-profile-configuration? global)
    (raise-exception
     (formatted-message
      (G_ "global must satisfy mpv-profile-configuration?"))))
  (unless (mpv/mpv-profile-configurations? profiles)
    (raise-exception
     (formatted-message
      (G_ "profiles must be an alist of mpv-profile-configuration?"))))
  (unless (or (not extra-config) (mpv/extra? extra-config))
    (raise-exception
     (formatted-message
      (G_ "extra-config must be a string or a gexp"))))
  (%make-home-mpv-configuration global profiles extra-config))

(define (serialize-home-mpv-configuration cfg)
  #~(string-append #$(serialize-mpv-profile-configuration
                      'global
                      (home-mpv-configuration-global cfg))
                   #$(serialize-mpv/mpv-profile-configurations
                      'profiles
                      (home-mpv-configuration-profiles cfg))
                   #$(serialize-mpv/extra
                      'extra-config
                      (home-mpv-configuration-extra-config cfg))))

(define (mpv-configuration-files cfg)
  `(("mpv/mpv.conf" ,(mixed-text-file "mpv.conf"
                                      (serialize-home-mpv-configuration cfg)))))

(define home-mpv-service-type
  (service-type
   (name 'home-mpv)
   (extensions
    (list (service-extension home-xdg-configuration-files-service-type
                             mpv-configuration-files)))
   (description
    "Install configuration files for mpv into XDG configuration directory.")))
