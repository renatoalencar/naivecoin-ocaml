open Lwt.Infix

type t = { filename: string
         ; blockchain: Blockchain.state }

let make blockchain filename = { filename ; blockchain }

let store t =
  Lwt_io.open_file ~mode:Output t.filename >>= fun output ->
  Lwt_io.write_value output t.blockchain >>= fun () ->
  Lwt_io.close output

let load t =
  try
    let channel = open_in_bin t.filename in
    let chain = input_value channel in
    t.blockchain.Blockchain.chain <- chain.Blockchain.chain;
    t.blockchain.Blockchain.utxos <- chain.Blockchain.utxos
  with Sys_error _ -> ()

let start t =
  let rec loop () =
    Lwt_unix.sleep 5.0 >>= fun () ->
    store t >>=
    loop
  in
  Lwt.async loop
