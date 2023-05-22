open Types

module C = struct
  (** [cfg] type use 2 parameters [size] in Mo and typ decribe data nature  *)
  type typ = [`Bytes of char]
  and t = {
    size: int;
    typ: typ;
  }
  [@@ deriving show]

  let default : t = {
    size = 1000;
    typ = `Bytes '0'
  }
end

let mio_to_o sz = sz * 1024 * 1024
let o_to_mio sz = (sz / 1024) / 1024
let o_to_mo sz = (sz / 1000) / 1000

let generate_str ?(char = 'a') (sz: int) : payload Lwt.t =
  let size_o = mio_to_o sz in
  let data = Bytes.make size_o char in
  Lwt.return { data;
               length = size_o }

let generate (c: C.t) = match c.typ with
  | `Bytes x -> generate_str ~char:x c.size

let pp_size ppf sz =
  Fmt.pf ppf {|
    [size: %i o]
    [size: %i Mio]
    [size: %i Mo]
    ------------------
  |}
    sz
    (o_to_mio sz)
    (o_to_mo sz)