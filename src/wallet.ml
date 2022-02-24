let get_balance_for_address address =
  Blockchain.utxos ()
  |> List.filter (fun utxo -> utxo.Transaction.UnspentTxOut.address = address)
  |> List.map (fun utxo -> utxo.Transaction.UnspentTxOut.amount)
  |> List.fold_left (+) 0

let get_balance identity =
  get_balance_for_address identity.Identity.address
