

type track_ident = {
  id : int;
}

type audio_clip

module AudioTrack = struct
  type t = {
    ident : track_ident;
    name : string;
    clips : audio_clip list;
  }
end
