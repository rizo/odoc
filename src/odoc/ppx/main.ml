open Migrate_parsetree
open Ast_406
open Ast_mapper
(* open Ast_helper *)
(* open Asttypes *)
(* open Parsetree *)
(* open Longident *)


let fail loc s =
  raise (Location.Error (Location.error ~loc ("odoc.ppx: " ^ s)))


let signature _mapper signature =
  let id =
    let name = Filename.remove_extension "filename.mli" in
    let file = Model.Root.Odoc_file.create_unit name ~force_hidden:false in
    let root = {Model.Root.package = ""; file; digest = "aaa"} in
    Model.Paths.Identifier.Root (root, name)
  in
  Odoc.Lint.signature id signature;
  fail Location.none "OH NO"


let () =
  let rewriter _config _cookies = { default_mapper with signature } in
  Driver.register ~name:"odoc.ppx" Versions.ocaml_406 rewriter
