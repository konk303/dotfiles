;;; early-init.el --- -*- lexical-binding: t; -*-

;; libgccjit derives -mmacosx-version-min from the Darwin kernel version
;; (Darwin 27 -> "18.0"), which clang rejects since macOS jumped from 15 to 26.
;; Pass a valid deployment target explicitly so native compilation works.
(when (and (eq system-type 'darwin) (featurep 'native-compile))
  (setq native-comp-driver-options '("-mmacosx-version-min=26.0")))
