;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2024 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages nushell)
  #:use-module (gnu packages c)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-shell)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))


(define-public nushell
  (package
    (name "nushell")
    (version "0.91.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zrw4jv57iwijd684vf43rw5sc9r0aq38shaizj96jqrgb8g9nja"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--"
         ;; Not all files included
         "--skip=tests::test_config_path::test_alternate_config_path"
         "--skip=tests::test_config_path::test_default_config_path"
         "--skip=tests::test_config_path::test_default_config_path_symlinked_config_files"
         "--skip=tests::test_config_path::test_default_symlink_config_path_broken_symlink_config_files"
         "--skip=tests::test_config_path::test_default_symlinked_config_path_empty"
         ;; Path not available inside build environment
         "--skip=path::canonicalize::canonicalize_tilde"
         "--skip=path::canonicalize::canonicalize_tilde_relative_to"
         ;; could not get mutex lock: PoisonError { .. }
         "--skip=plugins::config::closure"
         "--skip=plugins::config::none"
         "--skip=plugins::config::record"
         "--skip=plugins::core_inc::by_one_with_field_passed"
         "--skip=plugins::core_inc::by_one_with_no_field_passed"
         "--skip=plugins::core_inc::chooses_highest_increment_if_given_more_than_one"
         "--skip=plugins::core_inc::explicit_flag"
         "--skip=plugins::core_inc::semversion_major_inc"
         "--skip=plugins::core_inc::semversion_minor_inc"
         "--skip=plugins::core_inc::semversion_patch_inc"
         "--skip=plugins::core_inc::semversion_without_passing_field"
         "--skip=plugins::custom_values::can_generate_and_updated_multiple_types_of_custom_values"
         "--skip=plugins::custom_values::can_get_custom_value_from_plugin_and_instantly_collapse_it"
         "--skip=plugins::custom_values::can_get_custom_value_from_plugin_and_pass_it_over"
         "--skip=plugins::custom_values::can_get_custom_value_from_plugin_and_pass_it_over_as_an_argument"
         "--skip=plugins::custom_values::can_get_describe_plugin_custom_values"
         "--skip=plugins::custom_values::fails_if_passing_custom_values_across_plugins"
         "--skip=plugins::custom_values::fails_if_passing_engine_custom_values_to_plugins"
         "--skip=plugins::formats::eml::from_eml_get_another_header_field"
         "--skip=plugins::formats::eml::from_eml_get_replyto_field"
         "--skip=plugins::formats::eml::from_eml_get_subject_field"
         "--skip=plugins::formats::eml::from_eml_get_to_field"
         "--skip=plugins::formats::ics::from_ics_text_to_table"
         "--skip=plugins::formats::ics::from_ics_text_with_linebreak_to_table"
         "--skip=plugins::formats::ics::infers_types"
         "--skip=plugins::formats::ini::parses_ini"
         "--skip=plugins::formats::ini::parses_utf16_ini"
         "--skip=plugins::formats::ini::read_ini_with_missing_session"
         "--skip=plugins::formats::vcf::from_vcf_text_to_table"
         "--skip=plugins::formats::vcf::from_vcf_text_with_linebreak_to_table"
         "--skip=plugins::formats::vcf::infers_types"
         "--skip=plugins::register::help"
         "--skip=plugins::register::search_terms"
         "--skip=plugins::stream::collect_external_accepts_list_of_binary"
         "--skip=plugins::stream::collect_external_accepts_list_of_string"
         "--skip=plugins::stream::collect_external_big_stream"
         "--skip=plugins::stream::collect_external_produces_raw_input"
         "--skip=plugins::stream::seq_big_stream"
         "--skip=plugins::stream::seq_describe_no_collect_succeeds_without_error"
         "--skip=plugins::stream::seq_produces_stream"
         "--skip=plugins::stream::seq_stream_collects_to_correct_list"
         "--skip=plugins::stream::sum_accepts_list_of_float"
         "--skip=plugins::stream::sum_accepts_list_of_int"
         "--skip=plugins::stream::sum_accepts_stream_of_float"
         "--skip=plugins::stream::sum_accepts_stream_of_int"
         "--skip=plugins::stream::sum_big_stream")
       #:features '("extra")
       #:install-source? #f
       #:cargo-inputs
       (("rust-crossterm" ,rust-crossterm-0.27)
        ("rust-ctrlc" ,rust-ctrlc-3)
        ("rust-log" ,rust-log-0.4)
        ("rust-miette" ,rust-miette-7)
        ("rust-mimalloc" ,rust-mimalloc-0.1)
        ("rust-nix" ,rust-nix-0.27)
        ("rust-nu-cli" ,rust-nu-cli-0.91)
        ("rust-nu-cmd-base" ,rust-nu-cmd-base-0.91)
        ("rust-nu-cmd-dataframe" ,rust-nu-cmd-dataframe-0.91)
        ("rust-nu-cmd-extra" ,rust-nu-cmd-extra-0.91)
        ("rust-nu-cmd-lang" ,rust-nu-cmd-lang-0.91)
        ("rust-nu-command" ,rust-nu-command-0.91)
        ("rust-nu-engine" ,rust-nu-engine-0.91)
        ("rust-nu-explore" ,rust-nu-explore-0.91)
        ("rust-nu-lsp" ,rust-nu-lsp-0.91)
        ("rust-nu-parser" ,rust-nu-parser-0.91)
        ("rust-nu-path" ,rust-nu-path-0.91)
        ("rust-nu-plugin" ,rust-nu-plugin-0.91)
        ("rust-nu-protocol" ,rust-nu-protocol-0.91)
        ("rust-nu-std" ,rust-nu-std-0.91)
        ("rust-nu-utils" ,rust-nu-utils-0.91)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-reedline" ,rust-reedline-0.30)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-simplelog" ,rust-simplelog-0.12)
        ("rust-time" ,rust-time-0.3)
        ("rust-winresource" ,rust-winresource-0.1))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-2)
        ("rust-divan" ,rust-divan-0.1)
        ("rust-nu-test-support" ,rust-nu-test-support-0.91)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-rstest" ,rust-rstest-0.18)
        ("rust-serial-test" ,rust-serial-test-3)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list mimalloc openssl))
    (home-page "https://www.nushell.sh")
    (synopsis "Shell with a structured approach to the command line")
    (description
     "Nu draws inspiration from projects like PowerShell, functional
programming languages, and modern CLI tools.  Rather than thinking of files
and services as raw streams of text, Nu looks at each input as something with
structure.  For example, when you list the contents of a directory, what you
get back is a table of rows, where each row represents an item in that
directory.  These values can be piped through a series of steps, in a series
of commands called a ``pipeline''.")
    (license license:expat)))
