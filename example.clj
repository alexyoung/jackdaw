(use 'jackdaw)

; this is almost how the API should be,
; the drawing functions will need optional parameters
; for dynamic styles

(app "Jackdaw API Example" { :width 800, :height 600 }
  (fill 200 100 100)
  (rect 10 10 100 100)
  (stroke 255 10 55)
  (rect 120 10 100 100)
  (fill 100 100 200)
  (oval 20 120 100 100)
  (fill 25 25 100)
  (para "The Namespace system maintains global maps of symbols to Var objects (see Namespaces). If a def expression does not find an interned entry in the current namespace for the symbol being def-ed, it creates one, otherwise it uses the existing Var. This find-or-create process is called interning. This means that, unless they have been unmap-ed, Var objects are stable references and need not be looked up every time. It also means that namespaces constitute a global environment in which, as described in Evaluation, the compiler attempts to resolve all free symbols as Vars"))
