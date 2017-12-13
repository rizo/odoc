(*
 * Copyright (c) 2016, 2017 Thomas Refis <trefis@janestreet.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)



module Comment = Model.Comment
module Html = Tyxml.Html

type flow = Html_types.flow5
type phrasing = Html_types.phrasing
type non_link_phrasing = Html_types.phrasing_without_interactive



module Reference = struct
  module Id = Html_tree.Relative_link.Id

  open Model.Paths

  let rec render_resolved : type a. a Reference.Resolved.t -> string =
    fun r ->
      let open Reference.Resolved in
      match r with
      | Identifier id -> Identifier.name id
      | SubstAlias(_, r) -> render_resolved r
      | Module (r, s) -> render_resolved r ^ "." ^ s
      | Canonical (_, Reference.Resolved r) -> render_resolved r
      | Canonical (p, _) -> render_resolved p
      | ModuleType (r, s) -> render_resolved r ^ "." ^ s
      | Type (r, s) -> render_resolved r ^ "." ^ s
      | Constructor (r, s) -> render_resolved r ^ "." ^ s
      | Field (r, s) -> render_resolved r ^ "." ^ s
      | Extension (r, s) -> render_resolved r ^ "." ^ s
      | Exception (r, s) -> render_resolved r ^ "." ^ s
      | Value (r, s) -> render_resolved r ^ "." ^ s
      | Class (r, s) -> render_resolved r ^ "." ^ s
      | ClassType (r, s) -> render_resolved r ^ "." ^ s
      | Method (r, s) ->
        (* CR trefis: do we really want to print anything more than [s] here?  *)
        render_resolved r ^ "." ^ s
      | InstanceVariable (r, s) ->
        (* CR trefis: the following makes no sense to me... *)
        render_resolved r ^ "." ^ s
      | Label (r, s) -> render_resolved r ^ ":" ^ s

  let rec ref_to_string : type a. a Reference.t -> string = function
    | Reference.Root (s, _) -> s
    | Reference.Dot (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Module (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.ModuleType (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Type (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Constructor (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Field (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Extension (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Exception (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Value (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Class (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.ClassType (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Method (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.InstanceVariable (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Label (parent, s) -> ref_to_string parent ^ "." ^ s
    | Reference.Resolved r -> render_resolved r


  (* This is the entry point. stop_before is false on entry, true on recursive
     call. *)
  let rec to_html
      : type a.
        ?text:(non_link_phrasing Html.elt) ->
        stop_before:bool ->
        a Reference.t ->
          phrasing Html.elt =

    fun ?text ~stop_before ref ->
      let span' (txt : phrasing Html.elt list) : phrasing Html.elt =
        Html.span txt ~a:[ Html.a_class ["xref-unresolved"]
                  ; Html.a_title (Printf.sprintf "unresolved reference to %S"
                                (ref_to_string ref))
                  ]
      in
      let open Reference in
      match ref with
      | Root (s, _) ->
        begin match text with
        | None -> Html.pcdata s
        | Some s -> (span' [(s :> phrasing Html.elt)] :> phrasing Html.elt)
        end
      | Dot (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Module (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | ModuleType (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Type (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Constructor (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Field (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Extension (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Exception (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Value (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Class (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | ClassType (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Method (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | InstanceVariable (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Label (parent, s) ->
        unresolved_parts_to_html ?text span' parent s
      | Resolved r ->
        (* IDENTIFIER MUST BE RENAMED TO DEFINITION. *)
        let id = Reference.Resolved.identifier r in
        let txt : non_link_phrasing Html.elt =
          match text with
          | None -> Html.pcdata (render_resolved r)
          | Some s -> s
        in
        begin match Id.href ~stop_before id with
        | exception Id.Not_linkable -> (txt :> phrasing Html.elt)
        | exception exn ->
          (* FIXME: better error message *)
          Printf.eprintf "Id.href failed: %S\n%!" (Printexc.to_string exn);
          (txt :> phrasing Html.elt)
        | href ->
          Html.a ~a:[ Html.a_href href ] [txt]
        end

  and unresolved_parts_to_html
      : type a.
        ?text:(non_link_phrasing Html.elt) ->
        ((phrasing Html.elt list) -> (phrasing Html.elt)) ->
        a Reference.t ->
        string ->
          (phrasing Html.elt) =
    fun ?text span' parent s ->
      match text with
      | Some s -> (span' [(s :> phrasing Html.elt)] :> phrasing Html.elt)
      | None ->
        let tail = [ Html.pcdata ("." ^ s) ] in
        span' (
          match to_html ~stop_before:true parent with
          | content -> content::tail
        )
end



let rec non_link_inline_element
    : Comment.non_link_inline_element -> non_link_phrasing Html.elt =
  function
  | `Space -> Html.pcdata " "
  | `Word s -> Html.pcdata s
  | `Code_span s -> Html.code [Html.pcdata s]
  | `Styled (style, content) ->
    let content = non_link_inline_element_list content in
    match style with
    | `Bold -> Html.b content
    | `Italic -> Html.i content
    | `Emphasis -> Html.em content
    | `Superscript -> Html.sup content
    | `Subscript -> Html.sub content

and non_link_inline_element_list elements =
  List.map non_link_inline_element elements



let rec inline_element : Comment.inline_element -> phrasing Html.elt = function
  | #Comment.non_link_inline_element as e ->
    (non_link_inline_element e :> phrasing Html.elt)
  | `Reference (path, content) ->
    (* TODO Rework that ugly function. *)
    (* TODO References should be set in code style, if they are to code
            elements. *)
    let content =
      match content with
      | [] -> None
      | _ -> Some (Html.span (non_link_inline_element_list content))
    in
    Reference.to_html ?text:content ~stop_before:false path
  | `Link (target, content) ->
    let content =
      match content with
      (* TODO Actually allow empty links, and fill them with the URL? *)
      | [] -> [Html.pcdata target]
      | _ -> non_link_inline_element_list content
    in
    Html.a ~a:[Html.a_href target] content

and inline_element_list elements =
  List.map inline_element elements



let rec nestable_block_element
    : Comment.nestable_block_element -> flow Html.elt =
  function
  | `Paragraph content -> Html.p (inline_element_list content)
  | `Code_block s -> Html.pre [Html.code [Html.pcdata s]]
  | `Verbatim s -> Html.pre [Html.pcdata s]
  | `List (kind, items) ->
    let items =
      items
      |> List.map begin function
        | [`Paragraph content] ->
          (inline_element_list content :> (flow Html.elt) list)
        | item ->
          nestable_block_element_list item
        end
      |> List.map Html.li
    in

    match kind with
    | `Unordered -> Html.ul items
    | `Ordered -> Html.ol items

and nestable_block_element_list elements =
  List.map nestable_block_element elements



let tag : Comment.tag -> flow Html.elt = function
  | `Author s ->
    Html.(dl [
      dt [pcdata "author"];
      dd [pcdata s]])
  | `Deprecated content ->
    Html.(dl [
      dt [pcdata "deprecated"];
      dd (nestable_block_element_list content)])
  | `Param (name, content) ->
    Html.(dl [
      dt [pcdata "parameter "; pcdata name];
      dd (nestable_block_element_list content)])
  | `Raise (name, content) ->
    Html.(dl [
      dt [pcdata "raises "; pcdata name];
      dd (nestable_block_element_list content)])
  | `Return content ->
    Html.(dl [
      dt [pcdata "returns"];
      dd (nestable_block_element_list content)])
  | `See _ ->
    (* TODO *)
    failwith "unimplemented"
  | `Since s ->
    Html.(dl [
      dt [pcdata "since"];
      dd [pcdata s]])
  | `Before (version, content) ->
    Html.(dl [
      dt [pcdata "before "; pcdata version];
      dd (nestable_block_element_list content)])
  | `Version s ->
    Html.(dl [
      dt [pcdata "version"];
      dd [pcdata s]])



let block_element : Comment.block_element -> flow Html.elt = function
  | #Comment.nestable_block_element as e ->
    nestable_block_element e
  | `Heading (level, _label, content) ->
    (* TODO The label. *)
    let content = non_link_inline_element_list content in
    begin match level with
    | `Title -> Html.h1 content
    | `Section -> Html.h2 content
    | `Subsection -> Html.h3 content
    | `Subsubsection -> Html.h4 content
    end
  | `Tag t ->
    tag t

let block_element_list elements =
  List.map block_element elements



let prerr_error (error : Model.Error.t) =
  prerr_endline (Model.Error.to_string error);
  Pervasives.flush stderr

let first_to_html (_t : Model.Comment.t) =
  (* TODO *)
  failwith "unimplemented"

(* TODO Ignoring [wrap]. Wrapping in doc comment markup was a mistake in
   ocamldoc, and there is no need to emulate it. *)
let to_html ?wrap:_ (docs : Model.Comment.t) : (flow Html.elt) list =
  match docs with
  | Error e ->
    prerr_error e;
    []
  | Ok comment ->
    block_element_list comment

let has_doc (t : Model.Comment.t) =
  match t with
  | Ok body -> body <> []
  | Error e -> prerr_error e; false