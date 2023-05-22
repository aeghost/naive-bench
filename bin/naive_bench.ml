
(* Size in Mio *)
let cfg : Generator.C.t = {
  size = 6000;
  typ = `Bytes '0'
}

type test = [
    `Test of (string * string * string) * (Types.payload -> unit Lwt.t)
]
let tests : test list ref = ref []
let add_test ~name ~typ ~protocol f =
  tests := `Test ((name, typ, protocol), f) :: !tests

let introduce name typ protocol =
  Fmt.pr "@.--------@.";
  Fmt.pr "BENCHING : [name: %s] [typ: %s] [protocol: %s]@." name typ protocol

let () =
  add_test
    ~name:"ZMQ" ~typ:"PUSH-PULL" ~protocol:"INPROC"
  @@ fun payload ->
  let%lwt () = Lwt.join [
      Benchs.Zmq_bench.PushPull.run_server ~addr:"inproc://tmp" ();
      Benchs.Zmq_bench.PushPull.run_client ~addr:"inproc://tmp" payload;
    ] in
  Benchs.Zmq_bench.PushPull.close ()

let () =
  add_test
    ~name:"ZMQ" ~typ:"PUSH-PULL" ~protocol:"IPC"
  @@ fun payload ->
  let%lwt () = Lwt.join [
      Benchs.Zmq_bench.PushPull.run_server ~addr:"ipc://./_socks/tmp" ();
      Benchs.Zmq_bench.PushPull.run_client ~addr:"ipc://./_socks/tmp" payload;
    ] in
  Benchs.Zmq_bench.PushPull.close ()

(* let () =
   add_test
    ~name:"ZMQ" ~typ:"READ-WRITE" ~protocol:"TCP"
   @@ fun payload ->
   let%lwt () = Lwt.join [
      Benchs.Zmq_bench.ReadWrite.run_server ~addr:"tcp://127.0.0.1:8081" ();
      Benchs.Zmq_bench.ReadWrite.run_client ~addr:"tcp://127.0.0.1:8081" payload;
    ] in
   Benchs.Zmq_bench.ReadWrite.close () *)

let run_tests (p: Types.payload) =
  Lwt_list.iter_s (function `Test ((name, typ, protocol), fl) ->
      introduce name typ protocol;
      let%lwt () = fl p in
      Lwt.return @@ Gc.full_major ()
    ) (List.rev !tests)

let main () =
  Fmt.pr "@.-------@.";
  Fmt.pr "Generating Datas...@.";
  let%lwt payload = Generator.generate cfg in
  Fmt.pr "Data size :%a@."
    Generator.pp_size payload.length;
  run_tests payload;%lwt
  Fmt.pr "@.-------@.";
  Lwt.return @@ Fmt.pr "Done@."

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ ->
  Lwt_eio.run_lwt @@ main
