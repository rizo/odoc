
let mli file =
  let filename = Fs.File.to_string file in
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf filename;
  let signature = Parse.interface lexbuf in
  let parent =
    let name = Filename.remove_extension filename in
    let file = Model.Root.Odoc_file.create_unit name ~force_hidden:false in
    let root = {Model.Root.package = ""; file; digest = Digest.file filename} in
    Model.Paths.Identifier.Root (root, name)
  in
  let attribute _iter attr =
    match Loader.read_comment parent attr with
    | Ok _ -> ()
    | Error e -> failwith (Model.Error.to_string e)
  in
  let iterator = { Ast_iterator.default_iterator with attribute } in
  Ast_iterator.default_iterator.signature iterator signature


let mld _file = ()

