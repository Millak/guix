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
;;; defines rust-codex-0.124.0, transitively loads (gnu packages
;;; rust-apps) through its #:use-module chain.  If the codex package
;;; lived in rust-apps.scm, loading rust-sources would trigger loading
;;; rust-apps before rust-codex-0.124.0 is defined, causing an unbound
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
  #:use-module (gnu packages linux)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust-sources)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization))

(define-public codex
  (package
    (name "codex")
    (version (package-version rust-codex-0.124.0))
    (source
     (origin
       (inherit (package-source rust-codex-0.124.0))
       (patches (search-patches
                 "codex-acp-0.11.1-disable-code-mode.patch"
                 "rust-codex-0.124.0-code-mode-stub-toolname.patch"
                 "rust-codex-0.124.0-remove-patch-sections.patch"
                 "rust-codex-0.120.0-remove-libwebrtc.patch"
                 "rust-codex-0.98.0-test-shebangs.patch"
                 "rust-codex-0.120.0-test-timeout.patch"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      ;; exec-server is library-only in 0.124 (no [[bin]] in
      ;; exec-server/Cargo.toml); cargo install --path exec-server fails
      ;; with "no packages found with binaries or examples".  Drop it
      ;; from the install paths -- the library is consumed via the
      ;; workspace by other binaries here, not installed standalone.
      #:cargo-install-paths '(list "cli" "exec"
                                   "linux-sandbox" "mcp-server"
                                   "app-server" "tui")
      ;; schema_fixtures_match_generated (upstream fixture is stale:
      ;; FileChange::Update in codex-protocol gained old_content,
      ;; new_content, move_path fields but the committed JSON schema
      ;; fixture was not regenerated).
      #:cargo-test-flags '(list "--workspace"
                                "--exclude" "codex-app-server-protocol"
                                "--"
                                ;;; BEGIN Landlock returns NotEnforced
                                ;;; in the build container; the sandbox
                                ;;; binary panics instead of cleanly
                                ;;; denying filesystem access.
                                ;; Sandbox denial and escalation tests.
                                "--skip" "sandbox_denied_shell_returns_original_output"
                                ;; Sandbox denial and escalation tests.
                                "--skip" "shell_escalated_permissions_rejected_then_ok"
                                ;; Sandbox denial and escalation tests.
                                "--skip" "unified_exec_runs_under_sandbox"
                                ;; codex-exec: spawn_command_under_linux_sandbox
                                ;; panics with LandlockRestrict (exit 101).
                                "--skip" "python_getpwuid_works_under_sandbox"
                                ;; Same as above.
                                "--skip" "python_multiprocessing_lock_works_under_sandbox"
                                ;; Same as above.
                                "--skip" "sandbox_distinguishes_command_and_policy_cwds"
                                ;; linux-sandbox: process_exec_tool_call
                                ;; invokes Landlock; same panic.
                                "--skip" "test_writable_root"
                                ;; Same as above.
                                "--skip" "test_timeout"
                                ;; Same as above.
                                "--skip" "test_root_read"
                                ;; Same as above.
                                "--skip" "test_dev_null_write"
                                ;; Same as above.
                                "--skip" "test_no_new_privs_is_enabled"
                                ;; One scenario runs through Landlock
                                ;; which panics; cargo --skip cannot
                                ;; target individual scenarios.
                                "--skip" "approval_matrix_covers_all_modes"
                                ;; Landlock panic triggers escalation-retry
                                ;; which interferes with the approval cache.
                                "--skip" "approving_apply_patch_for_session_skips_future_prompts_for_same_file"
                                ;; These tests expect to interrupt a
                                ;; long-running 'sleep 60' and receive
                                ;; TurnAborted.  What happens:
                                ;;
                                ;; 1. ReadOnly wraps the command with
                                ;;    codex-linux-sandbox (Landlock-based).
                                ;; 2. Landlock is unavailable, so the
                                ;;    sandbox binary exits instantly.
                                ;; 3. Orchestrator gets SandboxErr::Denied.
                                ;;    wants_no_sandbox_approval(OnRequest)
                                ;;    returns false, so no escalation --
                                ;;    denial returned directly.
                                ;; 4. ToolEmitter::finish sends the error
                                ;;    to the mock model.
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
                                ;; Same causal chain as above.
                                "--skip" "interrupt_tool_records_history_entries"
                                ;; WorkspaceWrite sandbox policy wraps
                                ;; commands with codex-linux-sandbox;
                                ;; Landlock panics, command never runs,
                                ;; exit_code is None instead of Some(0).
                                "--skip" "request_permissions_grants_apply_to_later_exec_command_calls"
                                ;; Same root cause.
                                "--skip" "request_permissions_grants_apply_to_later_shell_command_calls"
                                ;; Same root cause.
                                "--skip" "partial_request_permissions_grants_do_not_preapprove_new_permissions"
                                ;;; END Landlock
                                ;;; BEGIN mcp_process.rs initialize
                                ;;; assertion expects "version":"0.0.0"
                                ;;; but the server returns "0.120.0".
                                "--skip" "test_codex_tool_passes_base_instructions"
                                "--skip" "test_shell_command_approval_triggers_elicitation"
                                "--skip" "test_patch_approval_triggers_elicitation"
                                ;;; END version mismatch
                                ;;; BEGIN Test isolation bug:
                                ;;; unique_temp_dir() in
                                ;;; state/src/runtime.rs names dirs
                                ;;; using nanosecond timestamps, so
                                ;;; parallel tests can collide.  The
                                ;;; SQLite migration runs CREATE TABLE
                                ;;; threads (without IF NOT EXISTS),
                                ;;; so the second init panics with
                                ;;; "table threads already exists".
                                "--skip" "init_removes_legacy_state_db_files"
                                "--skip" "upsert_and_get_thread_memory"
                                "--skip" "get_last_n_thread_memories_for_cwd_matches_exactly"
                                "--skip" "upsert_thread_memory_errors_for_unknown_thread"
                                "--skip" "get_last_n_thread_memories_for_cwd_zero_returns_empty"
                                "--skip" "get_last_n_thread_memories_for_cwd_does_not_prefix_match"
                                "--skip" "deleting_thread_cascades_thread_memory"
                                ;;; END SQLite test isolation
                                ;; The test spawns 'sh' after
                                ;; env_clear() with an empty env map;
                                ;; glibc execvp uses confstr(_CS_PATH)
                                ;; ("/bin:/usr/bin") when PATH is
                                ;; unset, but /bin/sh does not exist
                                ;; in the build container.
                                "--skip" "cancellation_expiration_keeps_process_alive_until_terminated"
                                ;; Proxy baseline_policy returns 403
                                ;; "not_allowed_local" for example.com;
                                ;; test expects 200.
                                "--skip" "managed_network_proxy_decider_survives_full_access_start"
                                ;;; BEGIN bash_snapshot_script() uses
                                ;;; "compgen -e" to list exports but
                                ;;; bash-minimal lacks programmable
                                ;;; completion; export section is empty.
                                ;; stdout.contains("VALID_NAME") fails.
                                "--skip" "bash_snapshot_filters_invalid_exports"
                                ;; stdout.contains("MULTILINE_CERT") fails.
                                "--skip" "bash_snapshot_preserves_multiline_exports"
                                ;; snapshot.contains("PATH") fails.
                                "--skip" "linux_bash_snapshot_includes_sections"
                                ;;; END compgen
                                ;; substitute* patches PAGER to store
                                ;; path of cat; test expects bare "cat".
                                "--skip" "unified_exec_env_injects_defaults"
                                ;;; BEGIN V8 disabled (codex-code-mode
                                ;;; default-features = false).
                                "--skip" "suite::code_mode::"
                                "--skip" "suite::js_repl::"
                                "--skip" "suite::view_image::js_repl_"
                                ;;; END V8 disabled
                                ;;; BEGIN same compgen root cause:
                                ;;; assert_posix_snapshot_sections
                                ;;; asserts snapshot.contains("PATH").
                                "--skip" "linux_shell_command_uses_shell_snapshot"
                                "--skip" "linux_unified_exec_uses_shell_snapshot"
                                "--skip" "shell_command_snapshot_still_intercepts_apply_patch"
                                ;;; END compgen / shell snapshot
                                ;;; BEGIN These sandbox tests set
                                ;;; exclude_slash_tmp: true which makes
                                ;;; bwrap exclude /tmp from its mount
                                ;;; namespace.  Since Guix builds under
                                ;;; /tmp, the codex-linux-sandbox binary
                                ;;; becomes invisible to bwrap even though
                                ;;; the test adds the helper dir as a
                                ;;; filesystem carve-out -- the parent /tmp
                                ;;; bind-mount is absent so the carve-out
                                ;;; has nothing to carve out of.
                                "--skip" "sandbox_reenables_writable_subpaths_under_unreadable_parents"
                                "--skip" "managed_proxy_mode_routes_through_bridge_and_blocks_direct_egress"
                                ;;; END exclude_slash_tmp
                                ;; Expects 3 TerminalInteraction events
                                ;; across timed poll windows; under CPU
                                ;; contention events merge into 2.
                                "--skip" "unified_exec_terminal_interaction_captures_delayed_output"
                                ;;; BEGIN assert_parallel_duration
                                ;;; requires < 1600 ms for parallel ops.
                                ;; Two 300 ms test_sync_tool calls.
                                "--skip" "read_file_tools_run_in_parallel"
                                ;; Two "sleep 0.25" shell commands.
                                "--skip" "shell_tools_run_in_parallel"
                                ;; One test_sync_tool + one shell sleep.
                                "--skip" "mixed_parallel_tools_run_in_parallel"
                                ;;; END assert_parallel_duration
                                ;; Asserts elapsed >= 4500 ms and
                                ;; < 5800 ms for a 5 s timeout; under
                                ;; contention elapsed drifts past 5800.
                                "--skip" "remote_models_request_times_out_after_5s"
                                ;; Queued inter-agent mail must be
                                ;; injected between the reasoning/
                                ;; commentary item and the gate release;
                                ;; under contention the gate fires before
                                ;; the mail is processed, so the snapshot
                                ;; shows the stale function_call instead
                                ;; of the expected mail message.
                                "--skip" "queued_inter_agent_mail_triggers_follow_up_after_commentary_message_item"
                                ;; Same root cause: snapshot mismatch at
                                ;; pending_input.rs assert_two_responses_
                                ;; input_snapshot; expects mail at position
                                ;; 04 but gets function_call/shell instead.
                                "--skip" "queued_inter_agent_mail_triggers_follow_up_after_reasoning_item"
                                ;;; BEGIN DNS for hostnames like
                                ;;; example.com fails in the build
                                ;;; sandbox.  host_resolves_to_non_
                                ;;; public_ip() in runtime.rs treats
                                ;;; failed DNS as non-public and
                                ;;; returns NotAllowedLocal, so tests
                                ;;; expecting Allowed or Decider get
                                ;;; Deny/BaselinePolicy instead.
                                "--skip" "host_blocked_requires_allowlist_match"
                                "--skip" "add_allowed_domain_removes_matching_deny_entry"
                                "--skip" "host_blocked_subdomain_wildcards_exclude_apex"
                                "--skip" "host_blocked_global_wildcard_allowlist_allows_public_hosts_except_denylist"
                                "--skip" "evaluate_host_policy_emits_domain_event_for_decider_ask"
                                "--skip" "evaluate_host_policy_emits_domain_event_for_decider_allow_override"
                                "--skip" "http_connect_accept_blocks_in_limited_mode"
                                "--skip" "http_connect_accept_allows_allowlisted_host_in_full_mode"
                                "--skip" "mitm_policy_blocks_disallowed_method_and_records_telemetry"
                                ;;; END DNS failure / NotAllowedLocal
                                ;;; BEGIN Flaky: wiremock mock expects
                                ;;; 1 POST to /codex/safety/arc but
                                ;;; receives 0.  Passes in some builds,
                                ;;; fails in others with identical code
                                ;;; and env.  The identical custom_
                                ;;; variant always passes; only the
                                ;;; CODEX_APPS_MCP_SERVER_NAME variant
                                ;;; flakes, suggesting a timing race
                                ;;; in the async POST under contention.
                                "--skip" "approve_mode_blocks_when_arc_returns_interrupt_for_model"
                                "--skip" "approve_mode_blocks_when_arc_returns_interrupt_without_annotations"
                                ;;; END ARC monitor flake
                                ;;; BEGIN nss-certs sets SSL_CERT_FILE
                                ;;; which makes build_reqwest_client load
                                ;;; 171 CA certs via add_root_certificate.
                                ;;; This changes the reqwest TLS backend
                                ;;; configuration; the ARC safety POST to
                                ;;; the localhost mock server never arrives
                                ;;; (wiremock expects 1 request, gets 0).
                                "--skip" "approve_mode_blocks_when_arc_returns_interrupt_for_model"
                                "--skip" "approve_mode_blocks_when_arc_returns_interrupt_without_annotations"
                                ;;; END nss-certs / ARC monitor
                                ;; Upstream bug: JsonRpcConnection has
                                ;; no websocket keepalive or read timeout.
                                ;; When the exec-server is killed, the
                                ;; client's reader task blocks on
                                ;; websocket_reader.next() forever because
                                ;; TCP doesn't detect the dead peer without
                                ;; a write or keepalive probe.  Previously
                                ;; masked because empty env made "sleep 10"
                                ;; fail instantly; with PATH the process
                                ;; lives long enough to hit the deadlock.
                                "--skip" "remote_exec_process_reports_transport_disconnect"
                                ;; 500 ms timeout on MCP list response
                                ;; (mcp_server_status.rs timeout at
                                ;; Duration::from_millis(500)).  With
                                ;; nss-certs TLS now works, so the
                                ;; startup plugin sync HTTP requests
                                ;; (chatgpt.com, api.github.com) wait
                                ;; for TCP timeout instead of failing
                                ;; instantly at TLS, eating the budget.
                                "--skip" "mcp_server_status_list_tools_and_auth_only_skips_slow_inventory_calls"
                                ;;; BEGIN Stack overflow in 'current_thread'
                                ;;; tokio runtime.  The sibling tests in
                                ;;; tracing_tests.rs use the local helper
                                ;;; run_current_thread_test_with_stack
                                ;;; (4 MiB) but this one uses bare
                                ;;; #[tokio::test] on the 2 MiB default
                                ;;; test-thread stack, and TurnStart's
                                ;;; future graph exceeds it.  With
                                ;;; RUST_MIN_STACK raised the overflow is
                                ;;; gone but the test then deterministically
                                ;;; times out waiting for a span tagged
                                ;;; codex.op = "user_input" on the remote
                                ;;; trace that upstream never emits on that
                                ;;; trace in our environment.
                                "--skip" "turn_start_jsonrpc_span_parents_core_turn_spans"
                                ;;; END tracing test stack/timeout
                                ;;; BEGIN The upstream loader resolves the
                                ;;; MCP OAuth credentials store mode by
                                ;;; calling
                                ;;; resolve_mcp_oauth_credentials_store_mode
                                ;;; with env!("CARGO_PKG_VERSION") -- which
                                ;;; in the released 0.124.0 tarball is
                                ;;; "0.124.0".  These fixture tests
                                ;;; construct the expected Config by
                                ;;; passing the LOCAL_DEV_BUILD_VERSION
                                ;;; constant ("0.0.0") to the same
                                ;;; resolver, and the resolver only returns
                                ;;; File for "0.0.0" -- Auto otherwise.
                                ;;; Upstream CI builds from a checkout
                                ;;; still carrying version = "0.0.0" so the
                                ;;; tests pass there; they cannot pass
                                ;;; against a released tag.
                                "--skip" "test_precedence_fixture_with_gpt3_profile"
                                "--skip" "test_precedence_fixture_with_gpt5_profile"
                                "--skip" "test_precedence_fixture_with_o3_profile"
                                "--skip" "test_precedence_fixture_with_zdr_profile"
                                ;;; END LOCAL_DEV_BUILD_VERSION fixture mismatch
                                ;;; BEGIN Verifies that Codex's user-shell
                                ;;; spawn produces a child where
                                ;;; CODEX_SANDBOX_NETWORK_DISABLED is unset
                                ;;; (asserts stdout == "not-set").  We
                                ;;; deliberately set that env var at the
                                ;;; check phase to trigger upstream's
                                ;;; skip_if_no_network! guards; the var then
                                ;;; leaks into the spawned shell and the
                                ;;; assertion fails.  The guix skip is a
                                ;;; direct cost of the network-gate fix --
                                ;;; trading one broken test for two that
                                ;;; now skip cleanly.
                                "--skip" "user_shell_command_does_not_set_network_sandbox_env_var"
                                ;;; END CODEX_SANDBOX_NETWORK_DISABLED leak
                                ;;; BEGIN The exec-server/tests/file_system
                                ;;; integration tests invoke the real
                                ;;; system bwrap, which tries to execvp the
                                ;;; test binary at the cargo target path
                                ;;; under the build dir.  Same root cause
                                ;;; as the existing
                                ;;; sandbox_reenables_writable_subpaths_under_unreadable_parents
                                ;;; skip: guix builds live under /tmp but
                                ;;; bwrap's mount namespace excludes /tmp,
                                ;;; making the binary invisible inside the
                                ;;; namespace.
                                "--skip" "file_system_copy_preserves_symlink_source"
                                "--skip" "file_system_copy_rejects_symlink_escape_destination"
                                "--skip" "file_system_copy_rejects_symlink_escape_source"
                                "--skip" "file_system_create_directory_rejects_symlink_escape"
                                "--skip" "file_system_read_directory_rejects_symlink_escape"
                                "--skip" "file_system_remove_rejects_symlink_escape"
                                "--skip" "file_system_remove_removes_symlink_not_target"
                                "--skip" "file_system_sandboxed_read_allows_readable_root"
                                "--skip" "file_system_sandboxed_read_rejects_symlink_escape"
                                "--skip" "file_system_sandboxed_read_rejects_symlink_parent_dotdot_escape"
                                "--skip" "file_system_sandboxed_write_allows_additional_write_root"
                                "--skip" "file_system_sandboxed_write_rejects_symlink_escape"
                                "--skip" "file_system_sandboxed_write_rejects_unwritable_path"
                                ;;; END bwrap-cant-see-/tmp file_system tests
                                ;;; BEGIN Landlock is unavailable on Guix,
                                ;;; so the sandbox cannot deny network
                                ;;; syscalls.  These tests expect
                                ;;; SandboxErr::Denied for the nc, ping,
                                ;;; ssh, getent, and dev_tcp_redirection
                                ;;; commands but those commands instead
                                ;;; block on connect() until the test's 2s
                                ;;; timeout fires (Timeout, exit 124).
                                ;;; Same root cause as the Landlock skips
                                ;;; at the top of this list; new test names
                                ;;; in 0.124.
                                "--skip" "sandbox_blocks_nc"
                                "--skip" "sandbox_blocks_ping"
                                "--skip" "sandbox_blocks_dev_tcp_redirection"
                                "--skip" "sandbox_blocks_getent"
                                "--skip" "sandbox_blocks_ssh"
                                ;;; END Landlock network block
                                ;;; BEGIN Requires github.com network access.
                                ;;; With no "extraKnownMarketplaces" entry
                                ;;; in settings.json,
                                ;;; collect_marketplace_import_sources
                                ;;; inserts a fallback entry for the
                                ;;; "claude-plugins-official" marketplace
                                ;;; pointing at the github shorthand
                                ;;; "anthropics/claude-plugins-official".
                                ;;; add_marketplace -> parse_marketplace_source
                                ;;; expands that to a github.com/.git URL
                                ;;; and clone_git_source runs 'git clone'.
                                ;;; The Guix build sandbox has no network,
                                ;;; so the clone fails and the marketplace
                                ;;; lands in failed_marketplaces.  No mock
                                ;;; cloner is injected via
                                ;;; ExternalAgentConfigService::new_for_test,
                                ;;; so there is no in-tree way to satisfy
                                ;;; the test offline.
                                "--skip" "import_plugins_infers_claude_official_marketplace_when_missing_from_settings"
                                ;;; END github.com network access
                                ;;; BEGIN Async race between the rollout
                                ;;; writer task and a direct SQLite read.
                                ;;; The test calls
                                ;;; update_memory_settings_with_app_server,
                                ;;; which routes through the in-memory
                                ;;; thread path on the app-server's
                                ;;; ThreadMemoryModeSet handler ->
                                ;;; Session::set_thread_memory_mode ->
                                ;;; persist_thread_memory_mode_update,
                                ;;; which writes a RolloutItem::SessionMeta
                                ;;; with the new mode to the rollout file
                                ;;; via recorder.record_items + flush.  The
                                ;;; rollout writer task then asynchronously
                                ;;; calls sync_thread_state_after_write,
                                ;;; which calls state_db::apply_rollout_items
                                ;;; to mirror the SessionMeta into SQLite.
                                ;;; flush() only awaits the rollout-file
                                ;;; write, not the subsequent SQLite sync.
                                ;;; The test opens a fresh StateRuntime and
                                ;;; reads get_thread_memory_mode
                                ;;; immediately, so it observes the creation
                                ;;; default of "enabled" written by
                                ;;; upsert_thread_with_creation_memory_mode
                                ;;; instead of the new "disabled".  Upstream
                                ;;; CI wins this race; we lose.
                                "--skip" "update_memory_settings_updates_current_thread_memory_mode"
                                ;;; END memory-mode rollout/sqlite race
                                ;;; BEGIN The Stopwatch::new constructor in
                                ;;; codex_shell_escalation anchors
                                ;;; T0 = Instant::now() at construction;
                                ;;; the spawned cancellation task fires at
                                ;;; T0 + limit.  The test captures
                                ;;; start = Instant::now() at T1 > T0 (after
                                ;;; cancellation_token() returns) and
                                ;;; asserts start.elapsed() >= limit,
                                ;;; i.e. (T0 + limit) - T1 >= limit, which
                                ;;; is always false by the offset T1 - T0.
                                ;;; The test only passes when scheduler
                                ;;; jitter on the sleep wakeup happens to
                                ;;; exceed that offset.  Upstream wins the
                                ;;; race; we don't always.
                                "--skip" "cancellation_receiver_fires_after_limit"
                                ;;; END Stopwatch construction/start offset race
                                )
      #:cargo-package-crates
      ''(;;; Tier 0: No internal deps.
         "codex-ansi-escape"
         "codex-async-utils"
         "codex-backend-openapi-models"
         "codex-client"
         "codex-execpolicy"
         "codex-file-search"
         "codex-git-utils"
         "codex-keyring-store"
         "codex-process-hardening"
         "codex-utils-absolute-path"
         "codex-utils-cache"
         "codex-utils-cargo-bin"
         "codex-utils-elapsed"
         "codex-utils-fuzzy-match"
         "codex-utils-home-dir"
         "codex-utils-json-to-toml"
         "codex-utils-path"
         "codex-utils-plugins"
         "codex-utils-pty"
         "codex-utils-readiness"
         "codex-utils-rustls-provider"
         "codex-utils-sleep-inhibitor"
         "codex-utils-stream-parser"
         "codex-utils-string"
         "codex-utils-template"
         ;;; Tier 1.
         "codex-utils-image"
         "codex-utils-output-truncation"
         "codex-apply-patch"
         "codex-protocol"
         "codex-windows-sandbox"
         "codex-api"
         "codex-experimental-api-macros"
         "codex-secrets"
         "codex-execpolicy-legacy"
         "codex-debug-client"
         "codex-analytics"
         "codex-rollout"
         "codex-rollout-trace"
         "codex-terminal-detection"
         "codex-utils-approval-presets"
         "codex-utils-cli"
         "codex-uds"
         "codex-install-context"
         "codex-device-key"
         ;;; Tier 2.
         "codex-app-server-protocol"
         "codex-rmcp-client"
         "codex-otel"
         "codex-thread-store"
         "codex-state"
         "codex-features"
         "codex-model-provider"
         "codex-config"
         "codex-agent-identity"
         "codex-aws-auth"
         "codex-hooks"
         "codex-code-mode"
         "codex-feedback"
         "codex-skills"
         "codex-test-binary-support"
         "codex-core"
         "codex-core-plugins"
         "codex-utils-sandbox-summary"
         "codex-linux-sandbox"
         "codex-sandboxing"
         "codex-connectors"
         "codex-core-skills"
         ;;; Tier 3.
         "codex-arg0"
         "codex-lmstudio"
         "codex-login"
         "codex-ollama"
         "codex-utils-oss"
         "codex-mcp-server"
         "codex-backend-client"
         "codex-responses-api-proxy"
         "codex-shell-command"
         "codex-shell-escalation"
         "codex-plugin"
         "codex-model-provider-info"
         "codex-models-manager"
         ;;; Tier 4.
         "codex-cloud-requirements"
         "codex-exec"
         "codex-exec-server"
         "codex-network-proxy"
         "codex-stdio-to-uds"
         "codex-chatgpt"
         "codex-cloud-tasks-client"
         "codex-cloud-tasks-mock-client"
         "codex-tools"
         "codex-mcp"
         "codex-collaboration-mode-templates"
         ;;; Tier 5.
         "codex-app-server"
         "codex-app-server-test-client"
         "codex-tui"
         "codex-response-debug-context"
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
                  (("\\(v0\\.0\\.0\\)   ") "(v0.124.0) ")))))
          (add-after 'chdir-to-workspace 'patch-git-deps-to-vendor
            (lambda _
              ;; Replace git dependencies with version references so cargo
              ;; resolves them from the vendored sources.
              (substitute* "Cargo.toml"
                (("nucleo = \\{ git = [^}]+\\}")
                 "nucleo = \"0.5.0\"")
                (("runfiles = \\{ git = [^}]+\\}")
                 "runfiles = \"0.1.0\""))
              ;; Remove workspace members that have unbuildable deps
              ;; (v8-poc requires V8).  code-mode stays a workspace member
              ;; so its codex-protocol resolves to the same local copy as
              ;; the rest of the build; disabling its default features
              ;; (below) avoids pulling in V8.
              (substitute* "Cargo.toml"
                (("\"v8-poc\",") ""))
              ;; Disable V8 runtime in codex-code-mode by turning off
              ;; default features.  Keep path= so code-mode is resolved
              ;; from the local source tree and shares codex-protocol
              ;; (and other workspace crates) with the rest of the build;
              ;; resolving it from the vendor instead introduces a
              ;; duplicate codex-protocol and causes E0308 type
              ;; mismatches in codex-tools.
              (substitute* "Cargo.toml"
                (("codex-code-mode = \\{ path = \"code-mode\" \\}")
                 "codex-code-mode = { path = \"code-mode\", default-features = false }"))
              ;; cargo build at workspace root ignores per-dep
              ;; default-features=false and builds code-mode with its
              ;; own default features, which include v8-runtime and
              ;; would pull in V8.  Make the default feature empty.
              (substitute* "code-mode/Cargo.toml"
                (("^default = \\[\"v8-runtime\"\\]") "default = []"))))
          (add-after 'patch-git-deps-to-vendor 'add-version-to-workspace-deps
            (lambda _
              ;; cargo package requires all dependencies to have versions.
              ;; Add version = "0.124.0" to internal path dependencies.
              (let ((cargo-files (find-files "." "^Cargo\\.toml$")))
                (substitute* cargo-files
                  ;; Handle inline deps: name = { path = "..." }
                  (("(codex-[a-z0-9-]+) = \\{ path = " all name)
                   (string-append name " = { version = \"0.124.0\", path = "))
                  ;; Handle inline deps with package: name = { package = "...", path = "..." }
                  (("(codex-[a-z0-9-]+) = \\{ package = " all name)
                   (string-append name " = { version = \"0.124.0\", package = "))
                  ;; Handle section deps: [dependencies.X] with path = "..."
                  (("^(path = \"\\.\\./[^\"]*\")" all path-line)
                   (string-append path-line "\nversion = \"0.124.0\""))))))
          (add-after 'chdir-to-workspace 'use-gnu-store-in-sandbox
            (lambda _
              ;; LINUX_PLATFORM_DEFAULT_READ_ROOTS in linux-sandbox/src/
              ;; bwrap.rs is the read-only baseline that codex's bwrap
              ;; sandbox bind-mounts so commands can read /usr/bin/ls,
              ;; libc, etc.  Upstream lists "/nix/store" for NixOS;
              ;; Guix's equivalent is "/gnu/store".  Without this swap,
              ;; the codex sandbox cannot locate any binaries on a Guix
              ;; system because every "system" path resolves into
              ;; /gnu/store/<hash>-pkg/bin/...
              (substitute* "linux-sandbox/src/bwrap.rs"
                (("\"/nix/store\"") "\"/gnu/store\""))))
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
                  ;; Single-quoted paths in test assertion strings,
                  ;; e.g. "exec '/bin/bash' -c ...".  Must match the
                  ;; double-quoted substitutions above so both sides
                  ;; of assert_eq!/contains use the store path.
                  (("'/bin/bash'")
                   (string-append "'" bash-bin "/bash'"))
                  (("'/bin/sh'")
                   (string-append "'" bash-bin "/sh'"))
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
                ;; Bare Command::new("git") and Command::new("ls")
                ;; in codex source files that exec internally.
                (substitute* (find-files "git-utils" "\\.rs$")
                  (("Command::new\\(\"git\"\\)")
                   (string-append "Command::new(\"" git-bin "/git\")")))
                (substitute* "tui/src/get_git_diff.rs"
                  (("Command::new\\(\"git\"\\)")
                   (string-append "Command::new(\"" git-bin "/git\")")))
                (substitute* "core/src/turn_diff_tracker.rs"
                  (("Command::new\\(\"git\"\\)")
                   (string-append "Command::new(\"" git-bin "/git\")")))
                (substitute* "cloud-tasks/src/env_detect.rs"
                  (("Command::new\\(\"git\"\\)")
                   (string-append "Command::new(\"" git-bin "/git\")")))
                (substitute* "core/src/plugins/startup_sync.rs"
                  (("\"git\",")
                   (string-append "\"" git-bin "/git\",")))
                (substitute* "file-search/src/lib.rs"
                  (("Command::new\\(\"ls\"\\)")
                   (string-append "Command::new(\"" coreutils-bin "/ls\")")))
                ;; Neutralize pager with absolute path to cat.
                (substitute* "core/src/unified_exec/process_manager.rs"
                  (("\"PAGER\", \"cat\"")
                   (string-append "\"PAGER\", \"" coreutils-bin "/cat\""))
                  (("\"GIT_PAGER\", \"cat\"")
                   (string-append "\"GIT_PAGER\", \"" coreutils-bin "/cat\""))
                  (("\"GH_PAGER\", \"cat\"")
                   (string-append "\"GH_PAGER\", \"" coreutils-bin "/cat\"")))
                ;; Bare /bin/sleep inside a format! string
                ;; (no surrounding double quotes, so the main
                ;; pattern does not match).
                (substitute*
                  "shell-escalation/src/unix/escalate_server.rs"
                  (("/bin/sleep")
                   (string-append coreutils-bin "/sleep")))
                ;; @SHELL@ placeholder from test-shebangs patch
                (substitute*
                  (list "rmcp-client/src/program_resolver.rs"
                        "tui/src/external_editor.rs")
                  (("@SHELL@")
                   (string-append bash-bin "/sh")))
                ;; Shebangs inside raw string literals (r#"..."#)
                ;; that are written to scripts at runtime.  The
                ;; main "/bin/sh" patterns only match double-quoted
                ;; occurrences; these shebangs have no surrounding
                ;; double quotes.
                (substitute*
                  (list "core/src/plugins/startup_sync_tests.rs"
                        "core/src/tools/runtimes/shell/unix_escalation_tests.rs"
                        "core/tests/suite/client.rs"
                        "core/tests/suite/js_repl.rs"
                        "core/tests/suite/skill_approval.rs"
                        "core/tests/suite/user_notification.rs"
                        "exec-server/tests/file_system.rs"
                        "login/src/auth/auth_tests.rs"
                        "models-manager/src/manager_tests.rs"
                        "sandboxing/src/bwrap_tests.rs")
                  (("#!/bin/bash")
                   (string-append "#!" bash-bin "/bash"))
                  (("#!/bin/sh")
                   (string-append "#!" bash-bin "/sh")))
                ;; Test helpers pass env: Default::default() to the
                ;; spawn code which calls env_clear(), leaving
                ;; processes with no PATH.  Inject the build-time
                ;; PATH so tests find coreutils, bash, etc.
                (substitute* (find-files "." "\\.rs$")
                  (("env: Default::default\\(\\)")
                   (string-append
                    "env: std::env::var(\"PATH\")"
                    ".into_iter()"
                    ".map(|p| (\"PATH\".to_string(), p))"
                    ".collect()")))
                ;; Test-only absolute path; bare name works via PATH.
                (substitute* "exec-server/tests/exec_process.rs"
                  (("\"/usr/bin/python3\"")
                   "\"python3\""))
                ;; Same empty-env issue but in JSON literals
                ;; (serde_json::json! macro).
                (substitute* "exec-server/tests/process.rs"
                  (("\"env\": \\{\\}")
                   (string-append
                    "\"env\": {\"PATH\": \""
                    (getenv "PATH")
                    "\"}"))))))
          (add-before 'build 'set-bubblewrap-source
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "CODEX_BWRAP_SOURCE_DIR"
                      (string-append
                       (assoc-ref inputs "bubblewrap-source")
                       "/"))))
          (add-before 'build 'create-node-version-file
            (lambda _
              (call-with-output-file "guix-vendor/node-version.txt"
                (lambda (port)
                  (display "22.22.0" port)))))
          (add-before 'check 'set-home
            (lambda _
              ;; HOME must not be a prefix of /tmp, otherwise
              ;; the TUI's path shortener replaces /tmp/... with
              ;; ~/... and 30 snapshot tests fail.
              (setenv "HOME" "/tmp/guix-home")
              (mkdir-p "/tmp/guix-home")
              (setenv "USER" "nixbld")
              ;; Default libtest thread stack is 2 MiB, which is not
              ;; enough for tokio current_thread tests that drive
              ;; codex-core's full turn pipeline.  Upstream gates such
              ;; tests through run_current_thread_test_with_stack (4 MiB)
              ;; in app-server/src/message_processor/tracing_tests.rs;
              ;; raise the global default so future additions that
              ;; forget the wrapper still pass.
              (setenv "RUST_MIN_STACK" "8388608")
              ;; Disable network access.
              (setenv "CODEX_SANDBOX_NETWORK_DISABLED" "1"))))))
    (native-inputs `(("bubblewrap" ,bubblewrap) ;tests need bwrap on PATH
                     ("clang" ,clang)
                     ("cmake-minimal" ,cmake-minimal)
                     ("libunwind" ,libunwind)
                     ("lsof" ,lsof)            ;app-server tests
                     ("nss-certs-for-test" ,nss-certs-for-test) ;OTLP gRPC TLS
                     ("perl" ,perl)
                     ("procps" ,procps)         ;tests use ps
                     ("python-minimal" ,python-minimal)
                     ("pkg-config" ,pkg-config)
                     ("bubblewrap-source" ,(package-source bubblewrap))))
    (inputs (cons* bash-minimal coreutils git-minimal sed
                   libcap oniguruma openssl sqlite zlib `(,zstd "lib")
                   (cargo-inputs 'codex)))
    (properties '((timeout . 108000)))  ;30 hours
    (home-page "https://github.com/openai/codex")
    (synopsis "AI-assisted coding CLI and TUI")
    (description
     "Codex is an AI-powered coding assistant that runs in the terminal.
It provides an interactive TUI for conversations with AI models, with
support for shell command execution, file editing, and code generation.
Configure providers via @file{~/.codex/config.toml}.

codex-code-mode's V8 Javascript executor is disabled.")
    (license license:asl2.0)))

(define-public codex-acp
  (package
    (name "codex-acp")
    (version "0.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zed-industries/codex-acp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rlgkbvrbdl5jhf49sjn2mifn2rq6z0vwf0gard23y1bz6sr3wmr"))
       (patches (search-patches "codex-acp-0.11.1-remove-patch-sections.patch"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-codex-deps
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Rewrite git dependencies to use vendored sources from rust-codex
              (substitute* "Cargo.toml"
                (("git = \"https://github.com/openai/codex\", tag = \"rust-v0.117.0\"")
                 "version = \"0.117.0\""))
              ;; Disable V8 runtime in codex-code-mode (transitive dep).
              (let ((port (open-file "Cargo.toml" "a")))
                (display "
[dependencies.codex-code-mode]
version = \"0.117.0\"
default-features = false
" port)
                (close-port port))
              ;; Point codex-linux-sandbox at system bubblewrap source.
              (setenv "CODEX_BWRAP_SOURCE_DIR"
                      (string-append
                       (assoc-ref inputs "bubblewrap-source")
                       "/"))))
          (add-before 'check 'set-home
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "USER" "nixbld")))
          (add-before 'build 'create-node-version-file
            (lambda _
              ;; codex-core's js_repl uses include_str! to read
              ;; node-version.txt four directories up from its source;
              ;; create it where the vendored crate expects it.
              (call-with-output-file "guix-vendor/node-version.txt"
                (lambda (port)
                  (display "22.22.0" port))))))))
    (native-inputs
     `(("cmake-minimal" ,cmake-minimal)
       ("clang" ,clang)
       ("pkg-config" ,pkg-config)
       ("bubblewrap-source" ,(package-source bubblewrap))))
    (inputs (cons* libcap openssl sqlite zlib `(,zstd "lib")
                   (cargo-inputs 'codex-acp)))
    (home-page "https://github.com/zed-industries/codex-acp")
    (synopsis "ACP-compatible agent bridging Zed Codex with ACP clients")
    (description
     "This package provides an Agent Client Protocol (ACP) compatible agent
that bridges the Zed Codex runtime with ACP clients over stdio.  It
supports multiple LLM providers through configuration in
@file{~/.codex/config.toml} and integrates with MCP servers for filesystem
operations.")
    (license license:asl2.0)))
