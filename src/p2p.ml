open Lwt.Infix

type message =
  | QueryAll
  | QueryLatest
  | ResponseBlockchain of Block.t list
  | QueryTransactionPool
  | ResponseTransactionPool of Transaction.t list

let peers = ref []
let peer_id = ref 0

let write_to_peer (data: message) (_, output) =
  Lwt_io.write_value output data >>= fun () ->
  Lwt_io.flush output

let remove_peer peer_id =
  Dream.log "Removing peer %d" peer_id;
  peers := List.remove_assoc peer_id !peers;
  Lwt.return_unit

let broadcast ?ignore:ignore_peer_id data =
  let ignore_peer_id = Option.value ~default:(-1) ignore_peer_id in
  !peers
  |> List.map (fun (peer_id, peer) ->
      Lwt.catch
        (fun () ->
           if ignore_peer_id <> peer_id then
             write_to_peer data peer
           else
             Lwt.return_unit)
        (function
         | End_of_file -> remove_peer peer_id
         | exn -> Lwt.fail exn))        
  |> Lwt.all
  |> Lwt.map ignore

let broadcast_latest_block () =
  broadcast (ResponseBlockchain [ Blockchain.get_latest_block () ])

let broadcast_whole_chain () =
  broadcast (ResponseBlockchain (Blockchain.current ()))

let broadcast_query_all () =
  broadcast QueryAll

let broadcast_transaction_pool ignore () =
  broadcast ~ignore (ResponseTransactionPool (Transaction_pool.current ()))

let query_latest peer =
  write_to_peer QueryLatest peer

let query_transaction_pool peer =
  write_to_peer QueryTransactionPool peer

let handle_blockchain_received blocks =
  let length = List.length blocks in
  Dream.log "Received %d blocks" length;
  if length > 0 then
    let latest_block_received = List.hd blocks in
    let latest_block_held = Blockchain.get_latest_block () in
    if latest_block_received.Block.index > latest_block_held.Block.index then begin
      Dream.log "Blockchain possibly behind. We got: %d Peer got: %d"
        latest_block_held.Block.index
        latest_block_received.Block.index;

      if latest_block_held.Block.hash = latest_block_received.Block.previous_hash then
        if Blockchain.add_block_to_chain latest_block_received then begin
          Dream.log "Received latest block, broadcasting to other peers";
          broadcast_latest_block ()
        end
        else
          Lwt.return_unit
      else if length = 1 then begin
        (* Broadcast querying all peers *)
        Dream.log "We have to query the chain from our peer";
        broadcast_query_all ()
      end
      else begin
        Dream.log "Received blockchain is longer than current blockchain";
        if Blockchain.replace_chain blocks then
          broadcast_latest_block ()
        else
          Lwt.return_unit
      end
    end
    else
      Lwt.return_unit
  else begin
    Dream.log "Received blockchain is not longer than current blockchain. Do nothing";
    Lwt.return_unit
  end

let handle_received_transactions transactions =
  let utxos = Blockchain.utxos () in
  List.iter
    (Transaction_pool.add_to_transaction_pool utxos)
    transactions

let peer_event_loop peer_id peer =
  let (input, _) = peer in
  let rec loop () =
    Lwt_io.read_value input >>= function
    | ResponseBlockchain blocks ->
      Dream.log "Receiving blocks";
      handle_blockchain_received blocks >>=
      loop
    | QueryLatest ->
      Dream.log "Been queried for latest block";
      write_to_peer (ResponseBlockchain [ Blockchain.get_latest_block () ]) peer >>=
      loop
    | QueryAll ->
      Dream.log "Been queried for the whole blockchain";
      write_to_peer (ResponseBlockchain (Blockchain.current ())) peer >>=
      loop
    | QueryTransactionPool ->
      write_to_peer (ResponseTransactionPool (Transaction_pool.current ())) peer >>=
      loop
    | ResponseTransactionPool transactions ->
      handle_received_transactions transactions;
      broadcast_transaction_pool peer_id () >>=
      loop
  in
  Dream.log "Waiting messages";
  Lwt.catch loop
    (function
      | End_of_file -> remove_peer peer_id
      | exn -> Lwt.fail exn)

let connection_handler _ peer =
  let id = !peer_id in
  peers := (id, peer) :: !peers;
  incr peer_id;

  query_latest peer >>= fun () ->
  query_transaction_pool peer >>= fun () ->
  peer_event_loop id peer

let connect_to_peer host port =
  let addr = Unix.(ADDR_INET (inet_addr_of_string host, port)) in
  Lwt_io.open_connection addr >>= fun peer ->
  Lwt.async (fun () -> connection_handler addr peer);
  Lwt.return_unit

let start_p2p_server ~port_offset =
  Lwt.async (fun () ->
      let addr = Unix.(ADDR_INET (inet_addr_any, Config.p2p_base_port + port_offset)) in
      Lwt.map ignore @@
      Lwt_io.establish_server_with_client_address
        addr
        connection_handler
    )
