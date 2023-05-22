open Types

let ctx = Zmq.Context.create ()

(* ZMQ is bad, it has to wait between create and connect *)
let wait () = Lwt_unix.sleep 1.

module Ezzmq = struct
  type ctx = Zmq.Context.t
  type ('a, 'b, 'c) t = {
    id : int;
    sock : 'b Zmq_lwt.Socket.t;
    mutable  uri: string;
    mutable finalized : bool;
  }
  type s = S : _ t -> s

  let table : (int, s) Hashtbl.t = Hashtbl.create 17
  let update : _ t -> string -> string -> unit =
    fun s _ a ->
    Hashtbl.remove table s.id;
    s.uri <-a;
    (* Fmt.pr "%s: '%s [%s]'@." u s.name s.uri;*)
    Hashtbl.add table s.id (S s)


  let id = ref 0
  let make  (tag: 'a Zmq.Socket.kind) (ctx: Zmq.Context.t) : (_, 'a, _) t =
    let s = Zmq.Socket.create ctx tag in
    Zmq.Socket.set_linger_period s 0;
    let lwts = Zmq_lwt.Socket.of_socket s in
    let res = {id = !id; sock = lwts; finalized = false; uri="init"} in
    incr id;
    Hashtbl.add table res.id (S res);
    res

  type yes = Yes
  type no = No
  type 'a readable = yes * 'a
  type 'a writable = 'a * yes

  type 'a request = ('a, [`Req], (yes * yes)) t

  let request ctx : _ request =
    let s = make Zmq.Socket.req ctx in
    let sock = Zmq_lwt.Socket.to_socket s.sock in
    Zmq.Socket.set_req_correlate sock true;
    Zmq.Socket.set_req_relaxed sock true;
    s

  type 'a response = ('a, [`Rep], (yes * yes)) t

  let response ctx : _ response = make Zmq.Socket.rep ctx

  type 'a push = ('a, [`Push], (no * yes)) t

  let push ctx : _ push = make Zmq.Socket.push ctx

  type 'a pull = ('a, [`Pull], (yes * no)) t

  let pull ctx : _ pull = make Zmq.Socket.pull ctx


  let bind : _ t -> string -> unit =
    fun s a ->
    let sock = Zmq_lwt.Socket.to_socket s.sock in
    try
      Zmq.Socket.bind sock a;
      update s "Bind" a
    with
    | Unix.Unix_error (err, err', _) ->
      raise (Unix.Unix_error (err, err', a))

  let unbind : _ t -> string -> unit =
    fun s a ->
    let sock = Zmq_lwt.Socket.to_socket s.sock in
    try Zmq.Socket.unbind sock a with
    | Unix.Unix_error (err, err', _) ->
      raise (Unix.Unix_error (err, err', a))

  let connect : _ t -> string -> unit =
    fun s a ->
    (* Fmt.pr "connecting to %s@." a; *)
    let sock = Zmq_lwt.Socket.to_socket s.sock in
    try Zmq.Socket.connect sock a; update s "Connect" a with
    | Unix.Unix_error (err, err', _) ->
      raise (Unix.Unix_error (err, err', a))

  let disconnect : _ t -> string -> unit =
    fun s a ->
    (* Fmt.pr "disconnecting from %s@." a; *)
    let sock = Zmq_lwt.Socket.to_socket s.sock in
    try Zmq.Socket.disconnect sock a  with
    | Unix.Unix_error (_, _, _) -> ()

  let close : _ t -> unit =
    fun s ->
    (* Fmt.pr "close socket...@."; *)
    if not s.finalized then begin
      let s' = Zmq_lwt.Socket.to_socket s.sock in
      Hashtbl.remove table s.id;
      Zmq.Socket.close s';
      s.finalized <- true
    end


  let read_raw : ?timeout:float -> (string, _, _ readable) t -> string Lwt.t =
    fun ?(timeout = -1.0) s ->
    let read () = let%lwt msg = Zmq_lwt.Socket.recv s.sock in
      (* Fmt.pr "read: %s@." msg; *)
      Lwt.return msg in
    if timeout <= 0.0 then begin
      read ()
    end else begin
      let waiter, wakener = Lwt.task () in
      let cancelable =
        let%lwt r = waiter in
        read r in
      Lwt.wakeup wakener ();
      Lwt_unix.with_timeout timeout (fun () -> cancelable)
    end

  let write_raw : ?timeout:float -> (string, _, _ writable) t -> string -> unit Lwt.t =
    fun ?(timeout = -1.0) s v ->
    let write () =
      (* Fmt.pr "write : %s@." msg; *)
      Zmq_lwt.Socket.send s.sock v in
    if timeout <= 0.0 then begin
      write ()
    end else begin
      let waiter, wakener = Lwt.task () in
      let cancelable =
        let%lwt r = waiter in
        write r in
      Lwt.wakeup wakener ();
      Lwt_unix.with_timeout timeout (fun () -> cancelable)
    end
end
open Ezzmq

let make_push () = push ctx
let make_pull () = pull ctx

let make_write () = response ctx
let make_read () = request ctx

let introduce name typ = Fmt.pr "[%s] [%s] Running ZMQ on %s...@." name typ
let conclude name typ = Fmt.pr "[%s] [%s] Closing ZMQ from %s...@." name typ

let wrap_error name typ f = try f () with e -> Fmt.pr "[%s] [%s] Error [exn: %a]@." name typ Fmt.exn e


module PushPull : TEST = struct
  let exit_db = ref []
  let on_exit f =
    (* Lwt_main.at_exit f; *)
    exit_db := f :: !exit_db

  let run_server ~addr () =
    let name = "SEVER" in
    let typ = "PP" in
    introduce name typ addr;
    let pull = make_pull () in
    let%lwt () = wait () in
    let () = wrap_error name typ @@ fun () ->
      bind pull addr;
      on_exit (fun () ->
          conclude name typ addr;
          close pull;
          Lwt.return_unit)
    in
    let%lwt () = wait () in
    let%lwt data = read_raw pull in
    let sz = String.length data in
    Fmt.pr "[SERVER] [PP] Received [size: %i]@." sz;
    Lwt.return_unit

  let run_client ~addr p =
    let name = "CLIENT" in
    let typ = "PP" in
    introduce name typ addr;
    let push = make_push () in
    let%lwt () = wait () in
    let%lwt () = wait () in
    let () = wrap_error name typ @@ fun () ->
      connect push addr;
      on_exit (fun () ->
          conclude name typ addr;
          disconnect push addr;
          close push;
          Lwt.return_unit) in
    let%lwt () = wait () in
    Fmt.pr "[CLIENT] [PP] Sending [size: %i]@." p.length;
    let () = Stats.start () in
    let%lwt () = write_raw push (String.of_bytes p.data) in
    let () = Stats.stop () in
    Fmt.pr "[CLIENT] [PP] Result:%a@." Stats.(pp ~export:(True (Fmt.str "ZMQ_CLIENT_PP_TO:%s" addr)) ~sz:p.length) ();
    Lwt.return_unit

  let close () =
    Lwt_list.iter_p (fun f -> try f () with e -> Lwt.return @@ Fmt.pr "[CLOSE] [PP] [acceptable error: %a]@." Fmt.exn e) !exit_db
end


module ReadWrite : TEST = struct
  let exit_db = ref []
  let on_exit f = exit_db := f :: !exit_db

  let run_server ~addr () =
    let name = "SERVER" in
    let typ = "RW" in
    introduce name typ addr;
    let pull = make_read () in
    let%lwt () = wait () in
    let () = wrap_error name typ @@ fun () ->
      bind pull addr;
      on_exit (fun () ->
          close pull;
          Lwt.return_unit)
    in
    let%lwt () = wait () in
    let%lwt data = read_raw pull in
    let sz = String.length data in
    Fmt.pr "[SERVER] [RW] Received [size: %i]@." sz;
    conclude name typ addr;
    Lwt.return_unit

  let run_client ~addr p =
    let name = "CLIENT" in
    let typ = "RW" in
    let%lwt () = wait () in
    let%lwt () = wait () in
    introduce name typ addr;
    let push = make_write () in
    let%lwt () = wait () in
    let%lwt () = wait () in
    let () = wrap_error name typ @@ fun () ->
      connect push addr;
      on_exit (fun () ->
          disconnect push addr;
          close push;
          Lwt.return_unit)
    in
    let%lwt () = wait () in
    let%lwt () = wait () in
    Fmt.pr "[CLIENT] [RW] Sending [size: %i]@." p.length;
    let () = Stats.start () in
    let%lwt () = write_raw push (String.of_bytes p.data) in
    let () = Stats.stop () in
    Fmt.pr "[CLIENT] [PP] Result:%a@." Stats.(pp ~export:(True "ZMQ_CLIENT_RW") ~sz:p.length) ();
    conclude name typ addr;
    Lwt.return_unit

  let close () =
    Lwt_list.iter_p (fun f -> try f () with e -> Lwt.return @@ Fmt.pr "[CLOSE] [RW] %a@." Fmt.exn e) !exit_db
end