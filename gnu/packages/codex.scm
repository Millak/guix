;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Danny Milosavljevic <dannym@friendly-machines.com>
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

;;; This module is separate from (gnu packages rust-apps) to avoid a
;;; circular module dependency: (gnu packages rust-sources), which
;;; defines rust-codex-0.98.0, transitively loads (gnu packages
;;; rust-apps) through its #:use-module chain.  If the codex package
;;; lived in rust-apps.scm, loading rust-sources would trigger loading
;;; rust-apps before rust-codex-0.98.0 is defined, causing an unbound
;;; variable error.

(define-module (gnu packages codex)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust-sources)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control))

(define-public codex
  (package
    (name "codex")
    (version (package-version rust-codex-0.98.0))
    (source
     (origin
       (inherit (package-source rust-codex-0.98.0))
       (patches (search-patches
                 "codex-0.98.0-remove-patch-sections.patch"
                 "rust-codex-0.98.0-test-shebangs.patch"
                 "rust-codex-0.98.0-test-timeout.patch"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:cargo-install-paths '(list "cli" "exec" "exec-server"
                                   "linux-sandbox" "mcp-server" "network-proxy"
                                   "app-server" "tui")
      ;; schema_fixtures_match_generated (upstream fixture is stale:
      ;; FileChange::Update in codex-protocol gained old_content,
      ;; new_content, move_path fields but the committed JSON schema
      ;; fixture was not regenerated).
      #:cargo-test-flags '(list "--workspace"
                                "--exclude" "codex-app-server-protocol"
                                "--"
                                ;; These tests exercise sandbox denial and
                                ;; escalation, which requires Landlock to
                                ;; cleanly deny filesystem access.  Inside the
                                ;; build container Landlock returns NotEnforced
                                ;; and the sandbox binary panics instead.
                                ;; Disabling Landlock would not help either,
                                ;; since these tests need a working sandbox to
                                ;; have anything to deny and escalate.
                                "--skip" "sandbox_denied_shell_returns_original_output"
                                "--skip" "shell_escalated_permissions_rejected_then_ok"
                                "--skip" "unified_exec_runs_under_sandbox"
                                ;; These tests (in codex-exec) directly call
                                ;; spawn_command_under_linux_sandbox to verify
                                ;; that python and bash work correctly inside
                                ;; the Landlock sandbox.  The sandbox binary
                                ;; (codex-exec) panics with LandlockRestrict
                                ;; (exit code 101) before the inner command
                                ;; even starts.
                                "--skip" "python_getpwuid_works_under_sandbox"
                                "--skip" "python_multiprocessing_lock_works_under_sandbox"
                                "--skip" "sandbox_distinguishes_command_and_policy_cwds"
                                ;; These linux-sandbox tests directly invoke
                                ;; the Landlock sandbox via
                                ;; process_exec_tool_call; same root cause.
                                "--skip" "test_writable_root"
                                "--skip" "test_timeout"
                                "--skip" "test_root_read"
                                "--skip" "test_dev_null_write"
                                "--skip" "test_no_new_privs_is_enabled"
                                ;; This test iterates many approval scenarios;
                                ;; one of them
                                ;; (danger_full_access_on_request_allows_network)
                                ;; runs a command through the Landlock sandbox
                                ;; binary, which panics with LandlockRestrict
                                ;; inside the build container.  Cargo --skip
                                ;; cannot target individual scenarios, so we
                                ;; skip the entire matrix.
                                "--skip" "approval_matrix_covers_all_modes"
                                ;; This test verifies session-level patch
                                ;; approval caching: approve once, skip
                                ;; future prompts for the same file.  When
                                ;; Landlock is unavailable (as in the Guix
                                ;; build container) the sandbox binary panics,
                                ;; triggering the escalation-retry path, which
                                ;; interferes with the approval cache and
                                ;; causes a spurious re-prompt on the second
                                ;; patch.
                                "--skip" "approving_apply_patch_for_session_skips_future_prompts_for_same_file"
                                ;; These tests expect to interrupt a
                                ;; long-running 'sleep 60' and receive
                                ;; TurnAborted.  Default test config is
                                ;; OnRequest + ReadOnly.  What happens:
                                ;;
                                ;; 1. ReadOnly wraps the command with
                                ;;    codex-linux-sandbox (Landlock-based).
                                ;; 2. Landlock is unavailable in the Guix
                                ;;    build container, so the sandbox
                                ;;    binary exits instantly (~1 ms).
                                ;; 3. Orchestrator gets SandboxErr::Denied.
                                ;;    wants_no_sandbox_approval(OnRequest)
                                ;;    returns false (sandboxing.rs:222),
                                ;;    so no escalation -- denial returned
                                ;;    directly.
                                ;; 4. ToolEmitter::finish sends the error
                                ;;    to the mock model as
                                ;;    function_call_output.
                                ;; 5. Second mock SSE response fires,
                                ;;    turn finishes with TurnComplete.
                                ;; 6. Op::Interrupt arrives 100 ms later,
                                ;;    but the turn is already done --
                                ;;    TurnAborted is never emitted,
                                ;;    test times out.
                                ;;
                                ;; The similar interrupt_long_running_tool_
                                ;; emits_turn_aborted passes because it
                                ;; sends the interrupt with no delay and
                                ;; has only one mock response (so the turn
                                ;; cannot complete first).
                                "--skip" "interrupt_persists_turn_aborted_marker_in_next_request"
                                "--skip" "interrupt_tool_records_history_entries"
                                ;; Upstream bug: test hardcodes "0.0.0" in the
                                ;; expected user-agent string but the workspace
                                ;; version is "0.98.0".
                                "--skip" "get_user_agent_returns_current_codex_user_agent"
                                ;; Same upstream bug: mcp-server tests
                                ;; check the initialize response which
                                ;; includes "version": "0.0.0" but the
                                ;; server returns "0.98.0".
                                "--skip" "test_codex_tool_passes_base_instructions"
                                "--skip" "test_shell_command_approval_triggers_elicitation"
                                "--skip" "test_patch_approval_triggers_elicitation"
                                ;; These codex-exec-server tests need
                                ;; "dotslash", a Meta tool that lazily
                                ;; downloads pre-built binaries from a
                                ;; JSON manifest.  The test helper
                                ;; create_transport runs
                                ;; `dotslash -- fetch <path>` to obtain a
                                ;; custom bash binary described in
                                ;; exec-server/tests/suite/bash.
                                ;; dotslash is not available in the build
                                ;; container.
                                "--skip" "list_tools"
                                "--skip" "accept_elicitation_for_prompt_rule"
                                ;;; Test isolation bug: each test in
                                ;;; state/src/runtime.rs calls
                                ;;; unique_temp_dir() to get its own
                                ;;; temporary directory (and thus its
                                ;;; own SQLite database).  That function
                                ;;; names directories using the current
                                ;;; nanosecond timestamp, so when tests
                                ;;; run in parallel several can receive
                                ;;; the same name and open the same
                                ;;; database.  The initial SQLite
                                ;;; migration runs CREATE TABLE threads
                                ;;; (without IF NOT EXISTS), so any init
                                ;;; after the first panics with "table
                                ;;; threads already exists".  Any of
                                ;;; these tests
                                ;;; can be the victim.
                                "--skip" "init_removes_legacy_state_db_files"
                                "--skip" "upsert_and_get_thread_memory"
                                "--skip" "get_last_n_thread_memories_for_cwd_matches_exactly"
                                "--skip" "upsert_thread_memory_errors_for_unknown_thread"
                                "--skip" "get_last_n_thread_memories_for_cwd_zero_returns_empty"
                                "--skip" "get_last_n_thread_memories_for_cwd_does_not_prefix_match"
                                "--skip" "deleting_thread_cascades_thread_memory")
      #:cargo-package-crates
      ''(;;; Tier 0: No internal deps.
         "codex-async-utils"
         "codex-client"
         "codex-execpolicy"
         "codex-file-search"
         "codex-git"
         "codex-keyring-store"
         "codex-utils-absolute-path"
         "codex-utils-cache"
         "codex-utils-cargo-bin"
         "codex-utils-home-dir"
         "codex-utils-json-to-toml"
         "codex-utils-pty"
         "codex-utils-readiness"
         "codex-utils-string"
         "codex-backend-openapi-models"
         "codex-process-hardening"
         "codex-ansi-escape"
         ;;; Tier 1: Depends on tier 0.
         "codex-utils-image"
         "codex-apply-patch"
         "codex-protocol"
         "codex-windows-sandbox"
         "codex-api"
         "codex-experimental-api-macros"
         "codex-secrets"
         "codex-execpolicy-legacy"
         "codex-debug-client"
         ;;; Tier 2.
         "codex-app-server-protocol"
         "codex-rmcp-client"
         "codex-otel"
         "codex-state"
         "codex-core"
         "codex-linux-sandbox"
         "codex-feedback"
         ;;; Tier 3.
         "codex-arg0"
         "codex-lmstudio"
         "codex-login"
         "codex-ollama"
         "codex-common"
         "codex-mcp-server"
         "codex-backend-client"
         "codex-responses-api-proxy"
         ;;; Tier 4.
         "codex-cloud-requirements"
         "codex-exec"
         "codex-exec-server"
         "codex-stdio-to-uds"
         "codex-network-proxy"
         "codex-chatgpt"
         "codex-cloud-tasks-client"
         ;;; Tier 5.
         "codex-app-server"
         "codex-app-server-test-client"
         "codex-tui"
         ;;; Tier 6.
         "codex-cloud-tasks"
         ;; The main executable.
         "codex-cli")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir-to-workspace
            (lambda _
              (chdir "codex-rs")))
          (add-after 'chdir-to-workspace 'update-version-in-snapshots
            (lambda _
              ;; Snapshot test files contain hardcoded v0.0.0 version strings.
              ;; Update them to match the actual package version.
              (let ((snap-files (find-files "." "\\.snap$")))
                (substitute* snap-files
                  (("\\(v0\\.0\\.0\\) ") "(v0.98.0)")))))
          (add-after 'chdir-to-workspace 'patch-git-deps-to-vendor
            (lambda _
              ;; Replace git dependencies with version references so cargo
              ;; resolves them from the vendored sources.
              (substitute* "Cargo.toml"
                (("nucleo = \\{ git = [^}]+\\}")
                 "nucleo = \"0.5.0\"")
                (("runfiles = \\{ git = [^}]+\\}")
                 "runfiles = \"0.1.0\""))))
          (add-after 'chdir-to-workspace 'add-version-to-workspace-deps
            (lambda _
              ;; cargo package requires all dependencies to have versions.
              ;; cargo package requires all dependencies to have versions.
              ;; Add version = "0.98.0" to internal path dependencies.
              (let ((cargo-files (find-files "." "^Cargo\\.toml$")))
                (substitute* cargo-files
                  ;; Handle inline deps: name = { path = "..." }
                  (("(codex-[a-z0-9-]+) = \\{ path = " all name)
                   (string-append name " = { version = \"0.98.0\", path = "))
                  ;; Handle inline deps with package: name = { package = "...", path = "..." }
                  (("(codex-[a-z0-9-]+) = \\{ package = " all name)
                   (string-append name " = { version = \"0.98.0\", package = "))
                  ;; Handle section deps: [dependencies.X] with path = "..."
                  (("^(path = \"\\.\\./[^\"]*\")" all path-line)
                   (string-append path-line "\nversion = \"0.98.0\""))))))
          (add-after 'chdir-to-workspace 'patch-hardcoded-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((bash-bin (string-append
                               (assoc-ref inputs "bash-minimal") "/bin"))
                    (coreutils-bin (string-append
                                    (assoc-ref inputs "coreutils") "/bin"))
                    (git-bin (string-append
                               (assoc-ref inputs "git-minimal") "/bin"))
                    (sed-bin (string-append
                               (assoc-ref inputs "sed") "/bin"))
                    ;; Include .policy files: the execpolicy-legacy
                    ;; crate embeds default.policy via include_str!
                    ;; at compile time, so its paths must also be
                    ;; patched.
                    (rs-files (find-files "." "\\.(rs|policy)$")))
                (substitute* rs-files
                  (("\"/bin/bash\"")
                   (string-append "\"" bash-bin "/bash\""))
                  (("\"/bin/sh\"")
                   (string-append "\"" bash-bin "/sh\""))
                  (("\"/usr/bin/bash\"")
                   (string-append "\"" bash-bin "/bash\""))
                  (("\"/usr/bin/sh\"")
                   (string-append "\"" bash-bin "/sh\""))
                  ;;; bash/sh with inline arguments, e.g. "/bin/bash -i".
                  (("\"/bin/bash ")
                   (string-append "\"" bash-bin "/bash "))
                  (("\"/bin/sh ")
                   (string-append "\"" bash-bin "/sh "))
                  ;; coreutils.
                  (("\"/bin/(cat|cp|date|echo|head|ls|rm|sleep|true|touch)\"" all cmd)
                   (string-append "\"" coreutils-bin "/" cmd "\""))
                  ;; coreutils.
                  (("\"/usr/bin/(cat|cp|head|ls|touch|true)\"" all cmd)
                   (string-append "\"" coreutils-bin "/" cmd "\""))
                  ;; coreutils with inline arguments
                  ;; like "/bin/echo END-EVENT".
                  (("\"/bin/(cat|cp|date|echo|head|ls|rm|sleep|true|touch) " all cmd)
                   (string-append "\"" coreutils-bin "/" cmd " "))
                  (("\"/usr/bin/git\"")
                   (string-append "\"" git-bin "/git\""))
                  (("\"/usr/bin/sed\"")
                   (string-append "\"" sed-bin "/sed\"")))
                ;; @SHELL@ placeholder from test-shebangs patch
                (substitute*
                  (list "rmcp-client/src/program_resolver.rs"
                        "tui/src/external_editor.rs")
                  (("@SHELL@")
                   (string-append bash-bin "/sh")))
                ;; shebang in test-only file
                (substitute*
                  "core/tests/suite/user_notification.rs"
                  (("#!/bin/bash")
                   (string-append "#!" bash-bin "/bash"))))))
          (add-before 'check 'set-home
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "USER" "nixbld"))))))
    (native-inputs (list clang              ;bindgen uses libclang to parse BoringSSL's C headers
                         cmake-minimal    ;BoringSSL is compiled from C source
                         libunwind        ;BoringSSL tests verify stack unwinding in assembly
                         perl python-minimal ;for tests
                         pkg-config))
    (inputs (cons* bash-minimal coreutils git-minimal sed
                   openssl sqlite `(,zstd "lib")
                   (cargo-inputs 'codex)))
    (home-page "https://github.com/openai/codex")
    (synopsis "AI-assisted coding CLI and TUI")
    (description
     "Codex is an AI-powered coding assistant that runs in the terminal.
It provides an interactive TUI for conversations with AI models, with
support for shell command execution, file editing, and code generation.
Configure providers via @file{~/.codex/config.toml}.")
    (license license:asl2.0)))

(define-public codex-acp
  (package
    (name "codex-acp")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zed-industries/codex-acp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "190sq6s6jfz8dkj1y8305r7x6ln86qqr2j1bnfjci7f1x2wyzmsj"))
       (patches (search-patches "codex-acp-0.9.2-remove-patch-sections.patch"
                                "codex-acp-0.9.2-replace-result-flatten.patch"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-codex-deps
            (lambda _
              ;; Rewrite git dependencies to use vendored sources from rust-codex
              (substitute* "Cargo.toml"
                (("git = \"https://github.com/zed-industries/codex\", branch = \"acp\"")
                 "version = \"0.0.0\"")))))))
    (native-inputs (list pkg-config))
    (inputs (cons* openssl sqlite `(,zstd "lib") (cargo-inputs 'codex-acp)))
    (home-page "https://github.com/zed-industries/codex-acp")
    (synopsis "ACP-compatible agent bridging Zed Codex with ACP clients")
    (description
     "This package provides an Agent Client Protocol (ACP) compatible agent
that bridges the Zed Codex runtime with ACP clients over stdio.  It
supports multiple LLM providers through configuration in
@file{~/.codex/config.toml} and integrates with MCP servers for filesystem
operations.")
    (license license:asl2.0)))
