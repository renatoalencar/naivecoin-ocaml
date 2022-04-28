
type t = Cstruct.t

let of_cstruct value = value

let to_cstruct t = t

let to_string t =
  Base58.encode @@ Cstruct.to_string t

let of_string value =
  Cstruct.of_string @@ Base58.decode_exn value

let of_public_key pub = Mirage_crypto_ec.Ed25519.pub_to_cstruct pub