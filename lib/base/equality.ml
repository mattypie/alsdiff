module type EQUALABLE = sig
  type t
  val equal : t -> t -> bool
end

module MakeDefaultEq (T: sig type t end) : EQUALABLE with type t = T.t = struct
  type t = T.t
  let equal = (=)
end


module type IDENTIFIABLE = sig
  type t
  include EQUALABLE with type t := t (* IDENTIFIABLE implies EQUALABLE *)

  (** return true if two values has the same identifier *)
  val has_same_id : t -> t -> bool
  val id_hash : t -> int
end
