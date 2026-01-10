;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan@hyperpolymath.io>

;;; STATE.scm - Project State & Configuration
;;; This file tracks the current state of the TotalUpdate & DNFinition project

(define-module (total-update state)
  #:export (project-state
            component-status
            implementation-status))

;;; =============================================================================
;;; Project Metadata
;;; =============================================================================

(define project-state
  '((name . "TotalUpdate & DNFinition")
    (version . "0.1.0")
    (status . "alpha")
    (license . "AGPL-3.0-or-later")
    (mission . "TotalUpdate prevents missed patches; DNFinition provides a universal PM/TUI")
    (standalone . "Both tools are standalone and can be combined")
    (egress-checker . "Idris2 policy gate")

    ;; Architecture
    (architecture
     (ada . "Safety-critical operations, SPARK verification")
     (elixir . "Coordination, OTP supervision, backends")
     (scheme . "Configuration, state files"))

    ;; Primary Package Managers (per CLAUDE.md)
    (primary-pm . guix)
    (fallback-pm . nix)))

;;; =============================================================================
;;; Component Status
;;; =============================================================================

(define component-status
  '(;; Ada Components
    (ada
     (dnfinition
      (platform-detection . implemented)
      (backend-interface . implemented)
      (backend-guix . implemented)
      (backend-nix . implemented)
      (safety-boundary . implemented)
      (safety-invariant . implemented)
      (plugin-registry . implemented)
      (strategy-matrix . implemented)
      (reversibility-types . spec-only)
      (snapshot-manager . spec-only)
      (tui . spec-only)
      (tui-main-window . spec-only))

     (totalupdate
      (main . implemented)
      (daemon . implemented)
      (config . implemented)
      (logging . implemented)
      (scheduler . implemented)
      (watcher . implemented)
      (strategy . implemented)))

    ;; Elixir Components
    (elixir
     (dnfinition
      (application . implemented)
      (daemon . implemented)
      (backends
       (backend . implemented)
       (guix-backend . implemented)
       (nix-backend . implemented))
      (plugins
       (plugin . implemented)
       (plugin-registry . implemented)
       (strategy-matrix . implemented)
       (guix-plugin . implemented)
       (nix-plugin . implemented))
      (safe-operations . implemented)
      (snapshot-manager . implemented))

     (totalupdate
      (application . implemented)
      (daemon . implemented)
      (scheduler . implemented)
      (watcher . implemented)
      (download-manager . implemented)
      (plugin-manager . implemented)
      (strategy-engine . implemented)))))

;;; =============================================================================
;;; Implementation Status by Feature
;;; =============================================================================

(define implementation-status
  '(;; Core Features
    (features
     ;; Walking Skeleton - COMPLETE
     (walking-skeleton
      (status . complete)
      (package-managers . (guix nix))
      (operations . (install upgrade rollback))
      (safety-boundary . verified))

     ;; Plugin Architecture - COMPLETE
     (plugin-architecture
      (status . complete)
      (registry . implemented)
      (capability-selection . implemented)
      (builtin-plugins . (guix nix)))

     ;; Strategy Matrix - COMPLETE
     (strategy-matrix
      (status . complete)
      (backends . (native btrfs zfs lvm snapper transaction-log))
      (risk-levels . (low medium high critical))
      (documentation . "docs/STRATEGY_MATRIX.adoc"))

     ;; TUI - STUB
     (tui
      (status . stub)
      (framework . ncurses)
      (spec-only . #t))

     ;; CLI Commands - PARTIAL
     (cli-commands
      (status . partial)
      (implemented . (info version help))
      (stub . (search install remove upgrade list snapshots rollback))))

    ;; Safety Features
    (safety
     (recovery-point-token . implemented)
     (critical-invariant . verified)
     (spark-proofs . partial)
     (generation-tracking . implemented))

    ;; Next Steps (Priority Order)
    (next-steps
     1 "Implement DNFinition CLI commands (connect to backends)"
     2 "Complete TUI implementation"
     3 "Add more package manager backends (apt, dnf, pacman)"
     4 "Implement download manager with aria2"
     5 "Add systemd integration for daemon")))

;;; =============================================================================
;;; Configuration Defaults
;;; =============================================================================

(define default-config
  '((check-interval-seconds . 14400)  ; 4 hours
    (auto-update . #f)
    (snapshot-method . auto)          ; auto-detect best available
    (bandwidth-limit . 0)             ; unlimited
    (max-parallel-downloads . 3)
    (log-level . info)

    ;; Package Manager Priority (when multiple available)
    (pm-priority . (guix nix apt dnf pacman zypper))

    ;; Paths
    (user-config . "~/.config/totalupdate/config.scm")
    (system-config . "/etc/totalupdate/config.scm")
    (state-dir . "~/.local/share/totalupdate")
    (log-file . "~/.local/share/totalupdate/daemon.log")))

;;; STATE.scm ends here
