
let cstruct_of_int_be value =
  let buf = Cstruct.create 4 in
  Cstruct.BE.set_uint32 buf 0 (Int32.of_int value);
  buf

let cstruct_to_hex value =
  let value = Cstruct.to_string value in
  let length = String.length value * 2 in
  let buffer = Buffer.create length in
  String.iter
    (fun c ->
       Printf.bprintf buffer "%02x" (Char.code c))
    value;
  Buffer.sub buffer 0 length
