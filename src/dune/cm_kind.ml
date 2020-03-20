open Stdune

type t =
  | Cmi
  | Cmj
  | Cmo
  | Cmx

let compare = Poly.compare

let all = [ Cmi; Cmj; Cmo; Cmx ]

let choose cmi cmj cmo cmx = function
  | Cmi -> cmi
  | Cmj -> cmj
  | Cmo -> cmo
  | Cmx -> cmx

let ext = choose ".cmi" ".cmj" ".cmo" ".cmx"

let source = choose Ml_kind.Intf Impl Impl Impl

module Dict = struct
  type 'a t =
    { cmi : 'a
    ; cmj : 'a
    ; cmo : 'a
    ; cmx : 'a
    }

  let get t = function
    | Cmi -> t.cmi
    | Cmj -> t.cmj
    | Cmo -> t.cmo
    | Cmx -> t.cmx

  let of_func f =
    { cmi = f ~cm_kind:Cmi
    ; cmj = f ~cm_kind:Cmj
    ; cmo = f ~cm_kind:Cmo
    ; cmx = f ~cm_kind:Cmx
    }

  let make_all x = { cmi = x; cmj = x; cmo = x; cmx = x }
end

let to_dyn =
  let open Dyn.Encoder in
  function
  | Cmi -> constr "cmi" []
  | Cmj -> constr "cmj" []
  | Cmo -> constr "cmo" []
  | Cmx -> constr "cmx" []
