type payload = {
  data: bytes;
  length: int
}

module type TEST = sig
  val run_client : addr:string -> payload -> unit Lwt.t
  val run_server : addr:string -> unit -> unit Lwt.t
  val close : unit -> unit Lwt.t
end