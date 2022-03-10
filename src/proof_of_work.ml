let hash_matches_difficulty hash difficulty =
  let rec aux d idx =
    if d > 8 then
      if Cstruct.get_uint8 hash idx = 0 then
        aux (d - 8) (idx + 1)
      else
        false
    else
      let b = Cstruct.get_uint8 hash idx in  
      let mask = 0xff lsl (8 - d) land 0xff in
      b land mask = 0
  in
  aux difficulty 0      

let find_block ~index ~previous_hash ~timestamp ~difficulty ~merkle_root transactions =
  let open Lwt.Infix in
  let rec aux nonce =
    let hash = Block.calculate_hash ~index ~previous_hash ~timestamp ~difficulty ~nonce merkle_root in
    if hash_matches_difficulty hash difficulty then 
      let block = Block.make ~index ~hash ~previous_hash ~timestamp ~difficulty ~nonce ~merkle_root transactions in
      Lwt.return block
    else
      if nonce mod Config.nonce_pause_interval = 0  then
        Lwt.pause () >>= fun () ->
        aux (nonce + 1)
      else
        aux (nonce + 1)
  in
  aux 0

let get_adjusted_difficulty latest_block chain =
  let previous_adjustment_block = List.nth chain Config.difficulty_adjustment_interval in
  let time_expected = Config.block_generation_interval * Config.difficulty_adjustment_interval in
  let time_taken = latest_block.Block.timestamp - previous_adjustment_block.Block.timestamp in
  if time_taken < time_expected / 2 then
    previous_adjustment_block.Block.difficulty + 1
  else if time_taken > time_expected / 2 then
    max (previous_adjustment_block.Block.difficulty - 1) 0
  else
    previous_adjustment_block.Block.difficulty

let get_difficulty chain =
  let latest_block = List.hd chain in
  if latest_block.Block.index mod Config.block_generation_interval = 0 && latest_block.Block.index <> 0 then
    get_adjusted_difficulty latest_block chain
  else
    latest_block.Block.difficulty

let generate_next_block chain merkle_root transactions =
  let previous_block = List.hd chain in
  let index = previous_block.Block.index + 1 in
  let timestamp =
    Unix.time () |> int_of_float
  in
  let difficulty = get_difficulty chain in
  Dream.log "Mining block %d" index;
  find_block ~index ~previous_hash:previous_block.Block.hash ~timestamp ~difficulty ~merkle_root transactions
