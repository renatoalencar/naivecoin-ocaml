let transaction_to_json transaction =
  let tx_in_to_json tx_in =
    `Assoc [ "tx_out_id", `String (Util.cstruct_to_hex tx_in.Transaction.TxIn.tx_out_id)
           ; "tx_out_index", `Int tx_in.Transaction.TxIn.tx_out_index
           ; "signature", `String (Util.cstruct_to_hex tx_in.Transaction.TxIn.signature) ]
  in
  let tx_out_to_json tx_out =
    `Assoc [ "address", `String (Util.cstruct_to_hex tx_out.Transaction.TxOut.address)
           ; "amount", `Int tx_out.Transaction.TxOut.amount ]
  in
  `Assoc [ "id", `String (Util.cstruct_to_hex transaction.Transaction.id)
         ; "tx_ins", `List (List.map tx_in_to_json transaction.Transaction.tx_ins)
         ; "tx_outs", `List (List.map tx_out_to_json transaction.Transaction.tx_outs) ]

let block_to_json block =
  `Assoc [ "index", `Int block.Block.index
         ; "hash", `String (Util.cstruct_to_hex block.Block.hash)
         ; "previous_hash", `String (Util.cstruct_to_hex block.Block.previous_hash)
         ; "timestamp", `Int block.Block.timestamp
         ; "difficulty", `Int block.Block.difficulty
         ; "nonce", `Int block.Block.nonce
         ; "merkle_root", `String (Util.cstruct_to_hex block.merkle_root)
         ; "transactions", `List (List.map transaction_to_json block.Block.transactions) ]

let utxo_to_json utxo =
  `Assoc [ "tx_out_id", `String (Util.cstruct_to_hex utxo.Transaction.UnspentTxOut.tx_out_id)
         ; "tx_out_index", `Int utxo.Transaction.UnspentTxOut.tx_out_index
         ; "address", `String (Util.cstruct_to_hex utxo.Transaction.UnspentTxOut.address)
         ; "amount", `Int utxo.Transaction.UnspentTxOut.amount ]

open Lwt.Infix

let error msg =
  `Assoc [ "error", `String msg ]

let list_utxo _request =
  let utxos =
    `List (List.map utxo_to_json (Blockchain.utxos ()))
  in
  utxos
  |> Yojson.Safe.to_string
  |> Dream.json

let list_blocks _request =
  let blocks =
    `List (List.map block_to_json (Blockchain.current ()))
  in
  blocks
  |> Yojson.Safe.to_string
  |> Dream.json

let block_by_index request =
  let index = int_of_string (Dream.param "index" request) in
  let chain = Blockchain.current () in
  let nth = List.length chain - index - 1 in
  let block = List.nth chain nth in
  block
  |> block_to_json
  |> Yojson.Safe.to_string
  |> Dream.json

let block_by_hash request =
  let hash = Cstruct.of_hex (Dream.param "hash" request) in
  let chain = Blockchain.current () in
  match
    List.find_opt (fun block -> block.Block.hash = hash)
      chain
  with
  | Some block ->
    block
    |> block_to_json
    |> Yojson.Safe.to_string
    |> Dream.json
  | None ->
    Dream.empty `Not_Found

let add_block request =
  Dream.body request >>= fun body ->
  let chain = Blockchain.current () in
  let block = Proof_of_work.generate_next_block chain (Cstruct.of_string body) [] in
  let () = assert (Blockchain.add_block_to_chain block) in
  P2p.broadcast_latest_block () >>= fun _ ->
  block
  |> block_to_json
  |> Yojson.Safe.to_string
  |> Dream.json

let mine_transaction identity request =
  request
  |> Dream.body
  |> Lwt.map Dream.from_form_urlencoded >>= function
  | [ "address", address ; "amount", amount ] ->
    let amount = int_of_string amount in
    let address = Cstruct.of_hex address in
    if amount > Wallet.get_balance identity then
      error "Not enough funds"
      |> Yojson.Safe.to_string
      |> Dream.json ~status:`Bad_Request
    else
      let block = Blockchain.generate_next_block_with_transaction ~identity address amount in
      P2p.broadcast_latest_block () >>= fun _ ->
      block
      |> block_to_json
      |> Yojson.Safe.to_string
      |> Dream.json
  | _ ->
    Dream.empty `Bad_Request

let mine_block identity _ =
  let block = Blockchain.generate_next_block ~identity in
  P2p.broadcast_latest_block () >>= fun _ ->
  block
  |> block_to_json
  |> Yojson.Safe.to_string
  |> Dream.json

let balance identity _request =
  Dream.json @@ string_of_int (Wallet.get_balance identity)

let connect_to_peer request =
  Lwt.map Dream.from_form_urlencoded (Dream.body request) >>= function
  | [ "host", host ; "port", port ] ->
    let port = int_of_string port in
    P2p.connect_to_peer host port >>= fun () ->
    Dream.empty `Created
  | _ ->
    Dream.empty `Bad_Request

let send_transaction identity request =
  Lwt.map Dream.from_form_urlencoded (Dream.body request) >>= function
  | [ "address", address ; "amount", amount ] ->
    let amount = int_of_string amount in
    let address = Cstruct.of_hex address in
    if amount > Wallet.get_balance identity then
      error "Not enough funds"
      |> Yojson.Safe.to_string
      |> Dream.json ~code:400
    else
      let utxos = Blockchain.utxos () in
      let transaction = Transaction.forge
          ~utxos
          ~private_key:identity.Identity.private_key
          ~address
          amount
      in
      Transaction_pool.add_to_transaction_pool utxos transaction;
      P2p.broadcast_transaction_pool (-1) () >>= fun () ->
      transaction
      |> transaction_to_json
      |> Yojson.Safe.to_string
      |> Dream.json
  | _ ->
    Dream.empty `Bad_Request

let transaction_pool _request =
  let transactions =
    `List (List.map transaction_to_json
             (Transaction_pool.current ()))
  in
  transactions
  |> Yojson.Safe.to_string
  |> Dream.json

let get_identity identity _request =
  `String (Util.cstruct_to_hex identity.Identity.address)
  |> Yojson.Safe.to_string
  |> Dream.json

let cors handler request =
  Lwt.map
    (fun response ->
       Dream.add_header "Access-Control-Allow-Origin" "*" response)
    (handler request)

let start ~identity ~port_offset =
  Dream.run ~port:(Config.node_api_base_port + port_offset)
  @@ Dream.logger
  @@ cors
  @@ Dream.router
    [ Dream.get "/blocks" list_blocks
    ; Dream.get "/block/:index" block_by_index
    ; Dream.get "/block/:hash" block_by_hash
    ; Dream.get "/balance" (balance identity)
    ; Dream.get "/identity" (get_identity identity)
    ; Dream.post "/add_block" add_block
    ; Dream.post "/mine_transaction" (mine_transaction identity)
    ; Dream.post "/send_transaction" (send_transaction identity)
    ; Dream.get "/transaction_pool" transaction_pool
    ; Dream.post "/mine_block" (mine_block identity)
    ; Dream.post "/add_peer" connect_to_peer 
    ; Dream.get "/utxos" list_utxo ]
  @@ Dream.not_found
