type jwt = { header : string; payload : string; signature : string }

type jwt_json = {
  header : Yojson.Safe.t;
  payload : Yojson.Safe.t;
  signature : string;
}
[@@deriving yojson]

let split_jwt jwt_input : jwt =
  let jwt_pieces = jwt_input |> String.split_on_char '.' |> Array.of_list in
  if Array.length jwt_pieces < 3 then failwith "Invalid JWT.";

  {
    header = jwt_pieces.(0);
    payload = jwt_pieces.(1);
    signature = jwt_pieces.(2);
  }

let no_padding_base64_decode = Base64.decode_exn ~pad:false

let decode_jwt (jwt : jwt) : jwt_json =
  {
    header = jwt.header |> no_padding_base64_decode |> Yojson.Safe.from_string;
    payload = jwt.payload |> no_padding_base64_decode |> Yojson.Safe.from_string;
    signature = jwt.signature;
  }

let print_banner () =
  "_______________________________________________\n\n  ███████╗ ██╗   ██╗ ███╗    ██╗ ██████╗  ██╗\n  ██╔════╝ ╚██╗_██╔╝ ████╗   ██║ ██╔══██╗ ██║\n  ███████╗  ╚████╔╝  ██╔██╗  ██║ ██║  ██║ ██║\n  ╚════██║   ╚██╔╝   ██║╚██╗ ██║ ██║  ██║ ██║\n  ███████║    ██║    ██║ ╚█████║ ██████╔╝ ██║\n  ╚══════╝    ╚═╝    ╚═╝  ╚════╝ ╚═════╝  ╚═╝\n_______________________________________________\n"
  |> print_endline
  [@@ocamlformat "disable"]

let usage_msg = "synd [-verbose] <file1> [<file2>] ... -o <output>"

let input = ref ""

let secret = ref ""

let speclist =
  [
    ("-j", Arg.Set_string input, "The JWT to be decoded.");
    ("-s", Arg.Set_string secret, "The secret to verify the JWT.");
  ]

let anon_args args = input := args

let () =
  print_banner ();
  Arg.parse speclist anon_args usage_msg;
  !input |> split_jwt |> decode_jwt |> jwt_json_to_yojson
  |> Yojson.Safe.pretty_print Format.std_formatter;
  print_newline ()
