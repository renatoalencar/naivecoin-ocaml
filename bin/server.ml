

let () =
  let identity = Naivecoin.Identity.init () in
  Dream.log "Starting node. Identity: %s" (Naivecoin.Identity.address_to_string identity);

  let port_offset = int_of_string @@ Sys.argv.(1) in
  Naivecoin.P2p.start_p2p_server ~port_offset;
  Naivecoin.Api.start ~identity ~port_offset
