type t = Transaction.t list

let state = ref []

let current () = !state

let has_tx_in unspent_tx_outs tx_in =
  unspent_tx_outs
  |> List.find_opt
       (fun utxo -> Transaction.(
        utxo.UnspentTxOut.tx_out_id == tx_in.TxIn.tx_out_id &&
        utxo.UnspentTxOut.tx_out_index == tx_in.TxIn.tx_out_index))
  |> Option.is_some

let is_valid_tx_for_pool tx pool =
  let tx_pool_ins =
    pool
    |> List.map (fun tx -> tx.Transaction.tx_ins)
    |> List.concat
  in
  let contains_tx tx_ins tx_in =
    tx_ins
    |> List.find_opt
         (fun tx_pool_in ->
          tx_pool_in.Transaction.TxIn.tx_out_id = tx_in.Transaction.TxIn.tx_out_id &&
          tx_pool_in.tx_out_index = tx_in.tx_out_index)
    |> Option.is_some
  in
  not (List.exists (contains_tx tx_pool_ins) tx.Transaction.tx_ins)
  
let add_to_transaction_pool unspent_tx_outs transaction =
  if not (Transaction.validate_transaction unspent_tx_outs transaction) then begin
    Dream.log "Transaction pool received invalid transaction: %s"
      (Util.cstruct_to_hex transaction.Transaction.id);
  end
  else if not (is_valid_tx_for_pool transaction (current ())) then begin
    Dream.log "Not valid for tx pool: %s"
      (Util.cstruct_to_hex transaction.Transaction.id);
  end
  else begin
    Dream.log "Adding %s to the transaction pool"
      (Util.cstruct_to_hex transaction.Transaction.id);
    state := transaction :: current ()
  end

let update_transaction_pool unspent_tx_outs =
  state := List.filter
      (fun tx -> List.for_all (has_tx_in unspent_tx_outs) tx.Transaction.tx_ins)
      (current ())
