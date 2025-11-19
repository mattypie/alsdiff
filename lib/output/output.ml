open Alsdiff_live


(** The interface for modules that can render automation patches into a specific format. *)
module type Output = sig
  (** The type of the rendered output (e.g., string for plain-text, or a more
      structured type for HTML). *)
  type t

  (** [render_automation_patch patch] is the main entry point. It renders the
      entire automation patch, including its list of envelope operations. *)
  val render_automation_patch : Automation.Patch.t -> t

  (** [render_audio_clip patch] renders the details of a patched audio clip,
      including name, timing, loop, signature, and sample reference changes. *)
  val render_audio_clip : Clip.AudioClip.Patch.t -> t

  (** [render_midi_clip patch] renders the details of a patched midi clip,
      including name, timing, loop, signature, and notes changes. *)
  val render_midi_clip : Clip.MidiClip.Patch.t -> t

  (** [render_device patch] renders the details of a patched device,
      including its parameters and any nested devices. *)
  val render_device : Device.Patch.t -> t

  (** [render_track patch] renders the details of a patched track,
      including its clips, devices, and automation. *)
  val render_track : Track.Patch.t -> t

  (* val render_liveset : Liveset.Patch.t -> t *)

end
