open Naivecoin

let api = ref false
let mine = ref false
let data = ref ""

let usage = "server [-api] [-mine] <node>"

let spec_list =
  [ "-api", Arg.Set api, "Run API endpoint"
  ; "-mine", Arg.Set mine, "Run miner"
  ; "-data", Arg.Set_string data, "Data directory"  ]

let node = ref 0

let anon_fun snode =
  node := int_of_string snode

let () =
  Arg.parse spec_list anon_fun usage;

  let identity_file = Printf.sprintf "%s/identity.%d" !data !node in
  let state_file = Printf.sprintf "%s/state.%d" !data !node in

  let identity = Identity.init identity_file in
  Dream.log "Starting node. Identity: %s" (Identity.address_to_string identity);

  P2p.start ~node:!node;

  let storage = Storage.make Blockchain.state state_file in
  begin
    Storage.load storage;
    Storage.start storage
  end;

  if !mine then
    Naivecoin.Blockchain.mine ~identity Naivecoin.P2p.broadcast_latest_block;

  if !api then
    Naivecoin.Api.start ~identity ~node:!node
