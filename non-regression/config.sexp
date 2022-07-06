
(prover
  (name ae-read-status)
  (cmd "grep :status $file")
  (unknown ":status unknown")
  (sat ":status sat")
  (unsat ":status valid"))

(dir
  (path "$HOME/workspace/alt-ergo/non-regression/valid")
  (pattern ".*.ae|.*.smt2")
  (expect (run ae-read-status)))

(dir
  (path "$HOME/workspace/alt-ergo/non-regression/invalid")
  (pattern ".*.ae|.*.smt2")
  (expect (run ae-read-status)))

(dir
  (path "$HOME/workspace/alt-ergo/non-regression/challenges/valid")
  (pattern ".*.ae|.*.smt2")
  (expect (run ae-read-status)))

(prover
  (name alt-ergo)
  (cmd "alt-ergo $file")
  (unsat "Valid")
  (unknown "I Don't Know"))

; Example:
; benchpress run -c $HOME/workspace/alt-ergo/non-regression/config.sexp -p alt-ergo $HOME/workspace/alt-ergo/non-regression/invalid $HOME/workspace/alt-ergo/non-regression/valid $HOME/workspace/alt-ergo/non-regression/challenges/valid/
; stores the resulting db in ~/.local/share/benchpress/
