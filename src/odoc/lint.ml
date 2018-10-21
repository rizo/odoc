module File = Fs.File
module Identifier = Model.Paths.Identifier


(* Copied from Loader.Cmti *)
let parenthesise name =
  match name with
  | "asr" | "land" | "lnot" | "lor" | "lsl" | "lsr"
  | "lxor" | "mod" -> "(" ^ name ^ ")"
  | _ ->
    if (String.length name > 0) then
      match name.[0] with
      | 'a' .. 'z' | '\223' .. '\246' | '\248' .. '\255' | '_'
      | 'A' .. 'Z' | '\192' .. '\214' | '\216' .. '\222' -> name
      | _ -> "(" ^ name ^ ")"
    else name

let check_doc parent id attrs =
  match Loader.read_attributes parent id attrs with
  | Ok _ -> ()
  | Error e -> failwith (Model.Error.to_string e)

let check_txt parent attr =
  match Loader.read_comment parent attr with
  | Ok _ -> ()
  | Error e -> failwith (Model.Error.to_string e)

let make_iterator parent =
  let open Parsetree in
  let super = Ast_iterator.default_iterator in
  let container = Identifier.(label_parent_of_parent (parent_of_signature parent)) in

  let attribute self x =
    super.attribute self x;
    check_txt container x
  in
  let value_description self x =
    super.value_description self x;
    let id = Identifier.Value (parent, parenthesise x.pval_name.txt) in
    check_doc container id x.pval_attributes
  in
  let type_declaration self x =
    super.type_declaration self x;
    let id = Identifier.Type (parent, x.ptype_name.txt) in
    check_doc container id x.ptype_attributes;
    match x.ptype_kind with
    | Ptype_abstract ->
      begin match x.ptype_manifest with
        | Some {ptyp_desc=Ptyp_variant (row_fields, _, _); _} ->
          let row_field tag =
            match tag with
            | Rtag (label, attrs, _, _) ->
              let id = Identifier.Constructor (id, label.txt) in
              check_doc container id attrs;
            | _ -> ()
          in
          List.iter row_field row_fields
        | _ -> ()
      end
    | Ptype_variant xs ->
      let constructor_declaration x =
        let id = Identifier.Constructor (id, x.pcd_name.txt) in
        check_doc container id x.pcd_attributes
      in
      List.iter constructor_declaration xs
    | Ptype_record xs ->
      let label_declaration x =
        let id = Identifier.Field (id, x.pld_name.txt) in
        check_doc container id x.pld_attributes
      in
      List.iter label_declaration xs
    | _ -> ()
  in
  let type_extension self x =
    super.type_extension self x;
    check_doc container parent x.ptyext_attributes;
    let extension_constructor x =
      let id = Identifier.Extension (parent, x.pext_name.txt) in
      check_doc container id x.pext_attributes
    in
    List.iter extension_constructor x.ptyext_constructors
  in
  let module_declaration self x =
    super.module_declaration self x;
    let id = Identifier.Module (parent, x.pmd_name.txt) in
    check_doc container id x.pmd_attributes
  in
  let module_type_declaration self x =
    super.module_type_declaration self x;
    let id = Identifier.ModuleType (parent, x.pmtd_name.txt) in
    check_doc container id x.pmtd_attributes
  in
  let extension_constructor self x =
    super.extension_constructor self x;
    let id = Identifier.Extension (parent, x.pext_name.txt) in
    check_doc container id x.pext_attributes
  in
  let include_description self x =
    super.include_description self x;
    check_doc container parent x.pincl_attributes
  in
  let class_description self x =
    super.class_description self x;
    let id = Identifier.Class (parent, x.pci_name.txt) in
    check_doc container id x.pci_attributes;
    match x.pci_expr.pcty_desc with
    | Pcty_signature {pcsig_fields; _} ->
      let class_type_field x =
        match x.pctf_desc with
        | Pctf_method (label, _, _, _) ->
          let id = Identifier.Method (id, label.txt) in
          check_doc container id x.pctf_attributes
        | _ -> ()
      in
      List.iter class_type_field pcsig_fields
    | _ -> ()
  in {
    super with
    attribute;
    value_description;
    type_declaration;
    type_extension;
    module_declaration;
    module_type_declaration;
    extension_constructor;
    include_description;
    class_description;
  }


let signature id items =
  let iterator = make_iterator id in
  iterator.signature iterator items


let mli file =
  let filename = Fs.File.to_string file in
  let items =
    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf filename;
    try Parse.interface lexbuf with
    | Syntaxerr.Error e ->
      Format.printf "%a" Syntaxerr.report_error e;
      exit 1
  in
  let id : Identifier.signature =
    let name = Filename.remove_extension filename in
    let file = Model.Root.Odoc_file.create_unit name ~force_hidden:false in
    let root = {Model.Root.package = ""; file; digest = Digest.file filename} in
    Model.Paths.Identifier.Root (root, name)
  in
  signature id items

