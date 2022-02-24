
module TxOut = struct
  type t = { address: Cstruct.t
           ; amount: int }

  let make address amount = { address ; amount }
end

module TxIn = struct
  type t = { tx_out_id: Cstruct.t
           ; tx_out_index: int
           ; signature: Cstruct.t }

  let make tx_out_id tx_out_index signature =
    { tx_out_id
    ; tx_out_index
    ; signature }
end

module UnspentTxOut = struct
  type t = { tx_out_id: Cstruct.t
           ; tx_out_index: int
           ; address: Cstruct.t
           ; amount: int }

  let make tx_out_id tx_out_index address amount =
    { tx_out_id
    ; tx_out_index
    ; address
    ; amount }
end

type t = { id: Cstruct.t
         ; tx_ins: TxIn.t list
         ; tx_outs: TxOut.t list }

let hash_tx_contents inputs outputs =
  let contents =
    List.append
      (inputs
       |> List.map
         (fun (out_id, out_index) ->
            [ out_id ; Util.cstruct_of_int_be out_index ])
       |> List.concat)
      (outputs
       |> List.map
         (fun (address, amount) ->
            [ address ; Util.cstruct_of_int_be amount ])
       |> List.concat)
  in
  let open Mirage_crypto.Hash in
  contents
  |> List.fold_left SHA256.feed SHA256.empty
  |> SHA256.get

let transaction_id tx_ins tx_outs =
  hash_tx_contents
    (tx_ins
     |> List.map (fun tx_in -> (tx_in.TxIn.tx_out_id, tx_in.TxIn.tx_out_index)))
    (tx_outs
     |> List.map (fun tx_out -> (tx_out.TxOut.address, tx_out.TxOut.amount)))

let valid_coinbase_tx transaction block_index =
  if transaction_id transaction.tx_ins transaction.tx_outs <> transaction.id then
    false
  else if List.length transaction.tx_ins <> 1 then
    false
  else if (List.hd transaction.tx_ins).TxIn.tx_out_index <> block_index then
    false
  else if List.length transaction.tx_outs <> 1 then
    false
  else if (List.hd transaction.tx_outs).TxOut.amount <> Config.coinbase_amount then
    false
  else
    true

let has_duplicates tx_ins =
  let rec aux visited tx_ins =
    match tx_ins with
    | tx_in :: rest ->
      if List.mem tx_in visited then
        true
      else
        aux (tx_in :: visited) rest
    | [] -> false
  in
  tx_ins
  |> List.map (fun tx_in -> (tx_in.TxIn.tx_out_id, tx_in.TxIn.tx_out_index))
  |> aux []

let validate_tx_in transaction unspent_tx_outs tx_in =
  let referenced_utxo =
    unspent_tx_outs
    |> List.find_opt (fun utxo ->
        UnspentTxOut.(
          utxo.tx_out_id = tx_in.TxIn.tx_out_id &&
          utxo.tx_out_index = tx_in.TxIn.tx_out_index
        ))
  in
  match referenced_utxo with
  | Some utxo ->
    let address = utxo.UnspentTxOut.address in
    let pub = Mirage_crypto_ec.Ed25519.pub_of_cstruct address in
    let key = Result.get_ok pub in
    if Mirage_crypto_ec.Ed25519.verify ~key ~msg:transaction.id tx_in.TxIn.signature then
      true
    else begin
      Dream.log "Invalid signature";
      false
    end
  | None ->
    Dream.log "UTXO not available for given input: %s %d"
      (Util.cstruct_to_hex tx_in.TxIn.tx_out_id)
      tx_in.TxIn.tx_out_index;
    false

let tx_in_amount unspent_tx_outs tx_in =
  let utxo =
    unspent_tx_outs
    |> List.find (fun utxo ->
        utxo.UnspentTxOut.tx_out_id = tx_in.TxIn.tx_out_id &&
        utxo.UnspentTxOut.tx_out_index = tx_in.TxIn.tx_out_index
      )
  in
  utxo.UnspentTxOut.amount

let validate_transaction unspent_tx_outs transaction =
  Dream.log "Validating transaction %s" (Util.cstruct_to_hex transaction.id);
  if transaction_id transaction.tx_ins transaction.tx_outs <> transaction.id then begin
    Dream.log "Invalid transaction id";
    false
  end
  else
    let valid_tx_ins =
      transaction.tx_ins
      |> List.for_all (validate_tx_in transaction unspent_tx_outs)
    in
    if not valid_tx_ins then
      false
    else
      let total_tx_in_values =
        transaction.tx_ins
        |> List.map (tx_in_amount unspent_tx_outs)
        |> List.fold_left (+) 0
      in
      let total_tx_out_values =
        transaction.tx_outs
        |> List.map (fun tx -> tx.TxOut.amount)
        |> List.fold_left (+) 0
      in
      if total_tx_in_values <> total_tx_out_values then begin
        Dream.log "values in input and outputs don't match %d <> %d" total_tx_in_values total_tx_out_values;
        false
      end
      else
        true

let validate_block_transactions transactions unspent_tx_outs block_index =
  (* For some reason valid_transaction is incorrectly validating the coinbase transaction *)
  if not (valid_coinbase_tx (List.hd transactions) block_index) then begin
    print_endline "Invalid coinbase transaction";
    false
    end
  else
    let tx_ins =
      transactions
      |> List.map (fun tx -> tx.tx_ins)
      |> List.concat
    in
    if has_duplicates tx_ins then
      begin
        print_endline "Duplicated tx in";
        false
      end
    else
      transactions
      |> List.tl
      |> List.for_all (validate_transaction unspent_tx_outs)

let update_unspent_tx_outs transactions unspent_tx_outs =
  let new_unspent_tx_outs =
    transactions
    |> List.map (fun tx ->
       tx.tx_outs
       |> List.mapi (fun index tx_out ->
           UnspentTxOut.make tx.id index tx_out.TxOut.address tx_out.amount))
    |> List.concat
  in
  let unspent_tx_outs =
    let tx_ins =
      transactions
      |> List.map (fun tx ->
          List.map
            (fun tx_in -> (tx_in.TxIn.tx_out_id, tx_in.TxIn.tx_out_index))
            tx.tx_ins)
      |> List.concat
    in
    unspent_tx_outs
    |> List.filter (fun utxo ->
        tx_ins
        |> List.find_opt (fun (id, index) ->
            UnspentTxOut.(
              utxo.tx_out_id = id &&
              utxo.tx_out_index = index))
        |> Option.is_none)
  in
  List.append unspent_tx_outs new_unspent_tx_outs

let process_transactions transactions unspent_tx_outs block_index =
  if block_index = 0 then
    unspent_tx_outs
  else if not (validate_block_transactions transactions unspent_tx_outs block_index) then begin
    Dream.log "Invalid block transactions";
    unspent_tx_outs
  end
  else
    update_unspent_tx_outs transactions unspent_tx_outs

let make id tx_ins tx_outs =
  { id ; tx_ins ; tx_outs }

let tx_in_of_utxo ~key ~transaction_id utxo =
  let signature = Mirage_crypto_ec.Ed25519.sign ~key transaction_id in
  TxIn.make utxo.UnspentTxOut.tx_out_id utxo.tx_out_index signature

let of_utxo ~private_key ~tx_outs utxos =
  let transaction_id =
    hash_tx_contents
      (utxos
       |> List.map (fun utxo -> (utxo.UnspentTxOut.tx_out_id, utxo.UnspentTxOut.tx_out_index)))
      (tx_outs
       |> List.map (fun tx_out -> (tx_out.TxOut.address, tx_out.TxOut.amount)))
  in
  let tx_ins =
    utxos
    |> List.map (tx_in_of_utxo ~key:private_key ~transaction_id)
  in
  let transaction = make transaction_id tx_ins tx_outs in
  transaction

let forge ~utxos ~private_key ~address amount =
  let rec build_utxo_list current utxos amount =
    match utxos with
    | utxo :: rest ->
      if current < amount then
        utxo :: build_utxo_list 0 rest (utxo.UnspentTxOut.amount + current)
      else
        [utxo]
    | [] -> []
  in

  let open Mirage_crypto_ec in
  let pub = Ed25519.pub_of_priv private_key in 
  let source_address = Ed25519.pub_to_cstruct pub in
  let utxos =
    List.filter
      (fun utxo -> utxo.UnspentTxOut.address = source_address)
      utxos
  in
  let inputs = build_utxo_list 0 utxos amount in
  if inputs = [] then
    failwith "Not enough funds";
  let output_amount =
    utxos
    |> List.map (fun utxo -> utxo.UnspentTxOut.amount)
    |> List.fold_left (+) 0
  in
  let output =
    if output_amount > amount then
      [ TxOut.make address amount
      ; TxOut.make source_address (output_amount - amount) ]
    else
      [ TxOut.make address amount ]
  in
  of_utxo ~private_key ~tx_outs:output utxos

let coinbase address block_index =
  let tx_in = [TxIn.make (Cstruct.create 32) block_index (Cstruct.create 32)] in
  let tx_out = [TxOut.make address Config.coinbase_amount] in
  let id = transaction_id tx_in tx_out in
  make id tx_in tx_out

let transaction_list_hash transactions =
  (* TODO: Replace this by a merkle tree *)
  Mirage_crypto.Hash.SHA256.(
    transactions
    |> List.map (fun tx -> tx.id)
    |> List.fold_left feed empty
    |> get
  )
