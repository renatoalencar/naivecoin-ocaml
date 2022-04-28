
let encode value =
  match Tezos_base58.encode ~prefix:"" value with
  | Base58 value -> value

let decode value =
  Tezos_base58.decode ~prefix:"" (Base58 value)

let decode_exn value =
  match decode value with
  | Some value -> value
  | None -> failwith "Invalid argument"
