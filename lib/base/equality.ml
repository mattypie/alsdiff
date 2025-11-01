module type EQUALABLE = sig
  type t
  val equal : t -> t -> bool
end

module MakeDefaultEq (T: sig type t end) : EQUALABLE with type t = T.t = struct
  type t = T.t
  let equal = (=)
end


module IntEq = MakeDefaultEq(struct type t = int end)
module FloatEq = MakeDefaultEq(struct type t = float end)
module BoolEq = MakeDefaultEq(struct type t = bool end)
module CharEq = MakeDefaultEq(struct type t = char end)
module StringEq = MakeDefaultEq(struct type t = string end)


module type IDENTIFIABLE = sig
  type t
  include EQUALABLE with type t := t (* IDENTIFIABLE implies EQUALABLE *)

  (** return true if two values has the same identifier *)
  val has_same_id : t -> t -> bool
  val id_hash : t -> int
end
