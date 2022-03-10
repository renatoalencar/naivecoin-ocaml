open Mirage_crypto_ec

type t = { private_key: Ed25519.priv
         ; public_key: Ed25519.pub
         ; address: Cstruct.t }

let init filename =
  try
    let input = open_in_bin filename in
    let identity = input_value input in
    close_in input;
    identity
  with Sys_error _ ->
    let identity =
      Mirage_crypto_rng_lwt.initialize ();
      let private_key, public_key = Ed25519.generate () in
      { private_key
      ; public_key
      ; address = Ed25519.pub_to_cstruct public_key }
    in
    let output = open_out_bin filename in
    output_value output identity;
    close_out output;
    identity

let address_to_string identity =
  Util.cstruct_to_hex identity.address
