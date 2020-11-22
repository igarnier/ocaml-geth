open Core
open Async
open Geth
module X = Json_encoding.Make (Json_repr.Ezjsonm)

let id = ref 0
let infura = lazy (String.split ~on:':' (Sys.getenv_exn "INFURA") |> List.hd_exn)
let src = Logs.Src.create "geth.ws"

module XX = (val Logs.src_log src : Logs.LOG)
open XX

let url =
  Uri.make ~scheme:"https" ~host:"mainnet.infura.io"
    ~path:(Filename.of_parts ["ws"; "v3"; Lazy.force infura])
    ()

let with_id x =
  let cur = !id in
  incr id ; (cur, x)

let ofJson e json =
  try X.destruct e json
  with Json_encoding.Cannot_destruct (_path, exn) ->
    Format.kasprintf failwith "%a"
      (Json_encoding.print_error ?print_unknown:None)
      exn

let of_frame frame =
  Fastws_async.of_frame_s frame
  |> Ezjsonm.value_from_string |> ofJson Geth.Rpc.msg

let to_frame msg =
  X.construct Geth.Rpc.sub msg
  |> Ezjsonm.value_to_string |> Fastws_async.to_frame_s

let process_user_input x =
  Queue.filter_map
    ~f:(fun s ->
      match String.split ~on:' ' (String.strip s) with
      | ["heads"] -> Some (with_id Rpc.NewHeads)
      | ["pending"] -> Some (with_id Rpc.NewTxs)
      | ["syncing"] -> Some (with_id Rpc.Syncing)
      | ["logs"] -> Some (with_id (Rpc.Logs {addr= None; topics= [||]}))
      | "logs" :: addr :: tl ->
          let addr = Types.Address.of_0x addr in
          let topics = List.to_array tl in
          let topics = Array.map topics ~f:Types.Hash256.of_0x in
          Some (with_id (Rpc.Logs {addr= Some addr; topics}))
      | _ ->
          err (fun m -> m "Unknown command") ;
          None)
    x
  |> return

let main () =
  Async_uri.with_connection url (fun {r; w; _} ->
      Fastws_async.with_connection url r w of_frame to_frame (fun r w ->
          don't_wait_for
            (Pipe.transfer'
               Reader.(pipe @@ Lazy.force stdin)
               w ~f:process_user_input) ;
          Pipe.iter_without_pushback r ~f:(fun msg ->
              info (fun m -> m "%a" Rpc.pp_msg msg))))

let () =
  Command.async_or_error ~summary:"Geth WS"
    (let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param [] in
      fun () ->
        Log.Global.set_level `Debug ;
        let pp_header ppf _ = Format.pp_print_string ppf "" in
        Logs.set_reporter
          (Logs_async_reporter.reporter ~pp_header (Lazy.force Log.Global.log)) ;
        Fastws_async_raw.to_or_error (main ())])
  |> Command.run
