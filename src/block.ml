
type t = { index: int
         ; hash: Cstruct.t
         ; previous_hash: Cstruct.t
         ; timestamp: int
         ; merkle_root: Cstruct.t
         ; difficulty: int
         ; nonce: int
         ; transactions: Transaction.t list }

let make ~index ~hash ~previous_hash ~timestamp ~difficulty ~nonce ~merkle_root transactions =
  { index
  ; hash
  ; previous_hash
  ; timestamp
  ; merkle_root
  ; difficulty
  ; nonce
  ; transactions }

let calculate_hash ~index ~previous_hash ~timestamp ~difficulty ~nonce merkle_root =
  let open Util in
  let value = Cstruct.concat [ cstruct_of_int_be index
                             ; previous_hash
                             ; cstruct_of_int_be timestamp
                             ; merkle_root
                             ; cstruct_of_int_be nonce
                             ; cstruct_of_int_be difficulty ]
  in
  Mirage_crypto.Hash.SHA256.digest value

let genesis =
  let index = 0 in
  let previous_hash = Cstruct.create 32 in
  let timestamp = 1645013663 in
  let difficulty = 0 in
  let nonce = 0 in
  let merkle_root = Mirage_crypto.Hash.SHA256.(get empty) in
  let hash = calculate_hash
    ~index
    ~previous_hash
    ~timestamp
    ~difficulty
    ~nonce
    merkle_root
  in
  make ~index ~hash ~previous_hash ~timestamp ~difficulty ~nonce ~merkle_root []

let hash { index ; previous_hash ; timestamp ; merkle_root ; difficulty ; nonce ; _ } =
  calculate_hash ~index ~previous_hash ~timestamp ~difficulty ~nonce merkle_root

let is_valid_timestamp new_block previous_block =
  previous_block.timestamp - 60 < new_block.timestamp &&
  new_block.timestamp - 60 < (int_of_float @@ Unix.time ())

let is_valid_block block previous_block =
  if previous_block.index + 1 <> block.index then begin
    Dream.log "Invalid block index";
    false
  end
  else if previous_block.hash <> block.previous_hash then begin
    Dream.log "Invalid previous hash";
    false
  end
  else if hash block <> block.hash then begin
    Dream.log "Invalid block hash";
    false
  end
  else if block.timestamp <= previous_block.timestamp
       || not (is_valid_timestamp block previous_block)
  then begin
    Dream.log "Invalid timestamp";
    false
  end
  else
    true

let is_valid_genesis block =
  block.index = genesis.index &&
  block.hash = genesis.hash &&
  block.previous_hash = genesis.previous_hash &&
  block.merkle_root = genesis.merkle_root &&
  block.timestamp = genesis.timestamp
