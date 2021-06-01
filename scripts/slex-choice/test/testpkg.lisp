(defpackage :slex-test
  (:use #:cl #:fiveam)
  (:local-nicknames (:bi :scripts.slex.bi-table)
                    (:race-role :scripts.slex.race-role-map)
                    (:entity :scripts.slex.entity))
  (:export #:run-tests))

(defpackage #:test)

(in-package :slex-test)
