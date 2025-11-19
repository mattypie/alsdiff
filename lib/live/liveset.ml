
module IntHashtbl = Hashtbl.Make(Int)

let () =
  let h = IntHashtbl.create 17 in
  IntHashtbl.add h 10 "hello";


module Locator = struct
  type t = {
    name : string;
    time : float;
  }
end


type t = {
  name : string;
  major_version : string;
  minor_version : string;
  creator : string;
  tracks : Track.t list;
  pointees : int IntHashtbl.t;

}
