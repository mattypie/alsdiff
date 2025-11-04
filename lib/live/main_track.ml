
type t = {
  devices : Device.t list;
  bpm : float * Automation.t list; (* manual value & automations *)
}
