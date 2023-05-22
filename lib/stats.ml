type time = {
  mutable start: float;
  mutable stop: float;
}
let time : time = {
  start = 0.;
  stop = 0.
}

let report = ref []

let () =
  at_exit @@ (fun () -> Yojson.to_file "./_report/report.json" (`Assoc !report))

let start () =
  time.start <- Unix.(gettimeofday ())

let stop () =
  time.stop <- Unix.(gettimeofday ())

let running () = time.stop -. time.start

let transfer_ratio ?(div = 1000000.) (sz: int) =
  ((float_of_int sz) /. running ()) /. div

type export = True of string | False
let pp ppf ?(export = False) ?(sz : int option) () =
  let pp_tm ppf (tm: Unix.tm) =
    Fmt.pf ppf "[%i:%i:%i]" tm.tm_hour tm.tm_min tm.tm_sec in
  let start = Unix.gmtime time.start in
  let stop = Unix.gmtime time.stop in
  let pp_ratio ppf = function
      None ->
      Fmt.pf ppf "NOT_CALCULATED"
    | Some sz ->
      Fmt.pf ppf "%f" @@ transfer_ratio sz
  in
  let () = match export with
    | True name ->
      let infos : Yojson.t = `Assoc [
          ("start", `Float time.start)
        ; ("stop", `Float time.stop)
        ; ("running", `Float (running ()))
        ; ("bandwidth",
           (match sz with
              None -> `Null
            | Some sz -> `Float (transfer_ratio sz)))
        ]
      in
      let () = report :=
          (name, infos)
          :: !report in
      ()
    | False -> ()
  in
  Fmt.pf ppf {|
    [start     : %a]
    [stop      : %a]
    [running   : %f s]
    [bandwidth : %a Mo/s]
    ------------------
    |}
    pp_tm start
    pp_tm stop
    (running ())
    pp_ratio sz


