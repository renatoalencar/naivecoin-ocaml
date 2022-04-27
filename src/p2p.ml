open Lwt.Infix

module Peer = struct
  type t = string * Lwt_io.output_channel

  let compare (a, _) (b, _) = String.compare a b
end

module Message = struct
  type t =
    | Port of int
    | Blockchain of Blockchain.t
    | QueryAll
    | QueryLatest

  let port port = Port port

  let blockchain chain = Blockchain chain

  let query_all = QueryAll

  let query_latest = QueryLatest
end

module Peer_set = Set.Make(Peer)

let peer_set = ref Peer_set.empty

let peers () =
  !peer_set
  |> Peer_set.to_seq
  |> List.of_seq
  |> List.map (fun (peer, _) -> peer)

let broadcast value =
  !peer_set
  |> Peer_set.to_seq
  |> Seq.map (fun (_, output) -> Lwt_io.write_value output value)
  |> List.of_seq
  |> Lwt.all
  |> Lwt.map ignore

let send (value: Message.t) output =
  Lwt_io.write_value output value

let broadcast_latest_block () =
  Dream.log "Broadcasting latest block";

  let block = Blockchain.get_latest_block () in
  broadcast (Message.blockchain [block])

let broadcast_transaction_pool _ () =
  Lwt.return_unit

let query_all_blocks () =
  Dream.log "Query for all blocks";
  broadcast Message.query_all

let query_latest_block output =
  send Message.query_latest output

let handle_blockchain chain =
  let latest_block_held = Blockchain.get_latest_block () in
  match chain with
  | [] -> Lwt.return_unit
  | latest_block_received :: blocks
       when latest_block_received.Block.index > latest_block_held.Block.index ->
     Dream.log "Received block %d, got %d. Node probably behind"
       latest_block_received.Block.index
       latest_block_held.Block.index;

     if latest_block_received.Block.previous_hash = latest_block_held.Block.hash
        && Blockchain.add_block_to_chain latest_block_received then
       broadcast_latest_block ()
     else if blocks = [] then
       (* query all blocks *)
       query_all_blocks ()
     else if Blockchain.replace_chain chain then
       broadcast_latest_block ()
     else
       Lwt.return_unit
  | _ :: _ -> Lwt.return_unit

module Connection = struct
  let handler node addr (input, output) =
    let addr =
      match addr with
      | Unix.ADDR_INET (addr, _) -> Unix.string_of_inet_addr addr
      | _ -> assert false
    in
    let full_addr = ref None in
    let handle_event = function
      | Message.Port port ->
         Dream.log "Received Port %d from %s" port addr;
         let peer = Printf.sprintf "%s:%d" addr port in
         peer_set := Peer_set.add (peer, output) !peer_set;
         full_addr := Some peer;
         Lwt.return_unit
      | Blockchain blocks ->
         Dream.log "Received %d blocks" (List.length blocks);
         handle_blockchain blocks
      | QueryAll ->
         Dream.log "Being queried for the whole chain, sending...";
         send (Message.blockchain (Blockchain.current ())) output
      | QueryLatest ->
         Dream.log "Being queried for the latest block";
         send (Message.blockchain [Blockchain.get_latest_block ()]) output
    in
    let rec loop () =
      let p =
        Lwt_io.read_value input >>=
        handle_event
      in
      p >>= fun () ->
      Lwt.catch
        loop
        (function
         | End_of_file ->
            let () =
              match !full_addr with
              | Some peer -> 
                 Dream.log "Lost connection with %s, removing peer." peer;
                 peer_set := Peer_set.remove (peer, output) !peer_set
              | None -> ()
            in
            Lwt.return_unit
         | exn -> Lwt.fail exn)
           
    in
    let port = Config.p2p_base_port + node in
    send (Message.port port) output >>= fun () ->
    query_latest_block output >>=
    loop

  let connect node host port =
    let address = Unix.(ADDR_INET (inet_addr_of_string host, port)) in
    let open_connection () =
      Lwt_io.open_connection address >>= fun pair ->
      Dream.log "Connected successfuly, waiting events from %s:%d" host port;

      Lwt.async (fun () -> handler node address pair);

      Lwt_result.return ()
    in
    Lwt.catch
      open_connection
      (function
       | Unix.Unix_error _ as exn ->
          Dream.log "Connection to %s:%d failed" host port;
          Lwt_result.fail exn
       | exn -> Lwt.fail exn)

  let rec wait_for_bootstrap_node node =
    let host, port = Config.bootstrap_node in
    Dream.log "Connecting to bootstrap node";
    connect node host port >>= function
    | Ok () -> Lwt.return_unit
    | Error _ ->
       Dream.log "Waiting 5 seconds...";
       Lwt_unix.sleep 5.0 >>= fun () ->
       wait_for_bootstrap_node node
end

let connect_to_peer = Connection.connect

let start ~node =
  let connection_handler addr pair =
    Dream.log "Received connection, waiting for events...";
    Connection.handler node addr pair
  in
  Lwt.async (fun () ->
      let address = Unix.(ADDR_INET (inet_addr_any, Config.p2p_base_port + node)) in
      Lwt_io.establish_server_with_client_address
        address
        connection_handler
      >|= ignore
    );

  if node <> 0 then
    Lwt.async (fun () -> Connection.wait_for_bootstrap_node node)
