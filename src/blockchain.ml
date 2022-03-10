open Lwt.Infix

type t = Block.t list

type state = { mutable chain: t
             ; mutable utxos: Transaction.UnspentTxOut.t list }

let state = { chain = [Block.genesis]
            ; utxos = [] }

let rec valid_chain chain =
  match chain with
  | [] -> false
  | genesis :: [] ->
    if Block.is_valid_genesis genesis then
      true
    else begin
      Dream.log "Invalid genesis block";
      false
    end
  | block :: previous :: _ ->
    Block.is_valid_block block previous
    && valid_chain (List.tl chain)

let cummulative_work chain =
  chain
  |> List.map (fun block -> 1 lsl block.Block.difficulty)
  |> List.fold_left (+) 0

let replace_chain new_blocks =
  if valid_chain new_blocks && cummulative_work new_blocks > cummulative_work state.chain then begin
    state.chain <- new_blocks;
    state.utxos <-
      new_blocks
      |> List.rev
      |> List.fold_left
        (fun utxo block ->
           Transaction.process_transactions
             block.Block.transactions
             utxo
             block.index)
        [];
    Transaction_pool.update_transaction_pool state.utxos;
    true
  end
  else begin
    Dream.log "Invalid chain replacement";
    false
  end

let get_latest_block () =
  List.hd state.chain

let add_block_to_chain new_block =
  if Block.is_valid_block new_block (get_latest_block ()) then
    let new_utxos = Transaction.process_transactions
                      new_block.Block.transactions
                      state.utxos
                      new_block.index
    in
    state.chain <- new_block :: state.chain;
    state.utxos <- new_utxos;
    Transaction_pool.update_transaction_pool state.utxos;
    true
  else begin
    Dream.log "Invalid block";
    false
  end

let generate_next_block_with_transaction ~identity address amount =
  let transaction =
    Transaction.forge
      ~utxos:state.utxos
      ~private_key:identity.Identity.private_key
      ~address
      amount
  in
  let coinbase_tx =
    Transaction.coinbase
      identity.address
      ((get_latest_block ()).Block.index + 1)
  in
  let transactions = [ coinbase_tx ; transaction ] in
  Proof_of_work.generate_next_block
    state.chain
    (Transaction.transaction_list_hash transactions)
    transactions >|= fun block ->
  begin
    assert (add_block_to_chain block);
    block
  end

let generate_next_block ~identity =
  let coinbase_tx =
    Transaction.coinbase
      identity.Identity.address
      ((get_latest_block ()).Block.index + 1)
  in
  let transactions = coinbase_tx :: Transaction_pool.current () in
  Proof_of_work.generate_next_block
    state.chain
    (Transaction.transaction_list_hash transactions)
    transactions >|= fun block ->
  begin
    if add_block_to_chain block then
      Dream.log "Failed to add generated block %d-%s to chain"
        block.Block.index
        (Util.cstruct_to_hex block.Block.hash);

    block
  end

let current () =
  state.chain

let utxos () =
  state.utxos

let mine ~identity f =
  let rec loop () =
    Lwt_unix.sleep Config.mine_interval >>= fun () ->
    generate_next_block ~identity >>= fun block ->
    Dream.log "Mined block %d-%s" block.Block.index (Util.cstruct_to_hex block.Block.hash);
    f () >>=
    loop
  in
  Lwt.async loop
