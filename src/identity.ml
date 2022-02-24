open Mirage_crypto_ec

type t = { private_key: Ed25519.priv
         ; public_key: Ed25519.pub
         ; address: Cstruct.t }

let init () =
  Mirage_crypto_rng_lwt.initialize ();
  let private_key, public_key = Ed25519.generate () in
  { private_key
  ; public_key
  ; address = Ed25519.pub_to_cstruct public_key }

let address_to_string identity =
  Util.cstruct_to_hex identity.address
