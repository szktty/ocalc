open Printf

let version = "1.0"

let anon_fun _ = ()

let usage = "Usage: ocalc [options]"

let options = [
  ("-version", Arg.Unit (fun () -> printf "%s\n" version), "Print version")
]

let print_error src pos msg =
  let buf = Buffer.create 64 in
  Buffer.add_string buf src;
  Buffer.add_string buf "\n";
  for _ = 0 to (pos-1) do
    Buffer.add_string buf " "
  done;
  Buffer.add_string buf "^\n";
  Buffer.add_string buf "error: ";
  Buffer.add_string buf msg;
  printf "%s\n" (Buffer.contents buf)

(* entry point *)
let _ =
  Arg.parse options anon_fun usage;
  let eval = Eval.create() in
  printf "Try \"h\" for more information.\n";
  while true do
    printf ">>> ";
    let line = String.trim @@ read_line () in
    try begin
      Eval.run eval line;
      begin match Eval.top eval with
      | None -> ()
      | Some v -> printf "%s\n" (Value.to_string v)
      end;
    end with
    | Eval.Error (pos, msg) -> print_error line pos msg
    | exn -> raise exn
  done
