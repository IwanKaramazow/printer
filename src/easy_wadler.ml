(* Attempt to unify break-style between list & label *)
module Break = struct
  type t =
    | Never
    | IfNeed
    | Always
    | Always_rec
end

type list_break = [
  (* | `Wrap_atoms *)
  (* | `Always_wrap *)
  | `Never_wrap
  | `Force_breaks
  | `Force_breaks_rec
  | `No_breaks
]

type list_config = {
  opening: string;
  closing: string;
  separator: string;
  space_after_opening : bool;
  space_after_separator : bool;
  space_before_separator : bool;

  (* This is a terrible name,
   * it really means that the separator
   * sticks to the right side of a list item
   * If this is true, we get the following fmting:
   * {
   *   a,
   *   b,
   *   c,
   * }
   *)
  separators_stick_left : bool;
  space_before_closing : bool;
  stick_to_label : bool;
  align_closing : bool;
  wrap_body : list_break;
  indent_body : int;

  (* I think we can safely omit these *)
  (* list_style : style_name option; *)
  (* opening_style : style_name option; *)
  (* body_style : style_name option; *)
  (* separator_style : style_name option; *)
  (* closing_style : style_name option; *)
}

let default_list_config= {
  opening = "";
  closing = "";
  separator = " ";
  space_after_opening = true;
  space_after_separator = true;
  space_before_separator = false;
  separators_stick_left = true;
  space_before_closing = true;
  stick_to_label = true;
  align_closing = true;
  wrap_body = `Never_wrap;
  indent_body = 2;
}

type label_break = [
  | `Auto
  | `Always
  | `Always_rec
  | `Never
]

type label_config = {
  label_break: label_break;
  space_after_label : bool;
  indent_after_label : int;
  (* can be omitted ? *)
  (* label_style : style_name option; *)
}

let default_label_config = {
  label_break = `Auto;
  space_after_label = true;
  indent_after_label = 2;
}

type t =
  | Atom of string
  | List of list_config * t list
  | Label of t * t * label_config
            (* key value config *)

let rec to_doc = function
  | Atom str -> Pretty.text str
  | List (config, lst) ->
      let open Pretty in
      let open Infix in
      let ending =
        (if config.space_before_closing && config.align_closing == true then sep " "
        else if config.space_before_closing && config.align_closing == false then text " "
        else sep "")
        ++ text config.closing
      in
      let rec docs_of_list acc = function
        | x::xs ->
            (* TODO performance *)
            docs_of_list (acc
            ++ (to_doc x)
            ++ (if config.space_before_separator then text " " else empty)
            (* easy format doesn't print final semi by default *)
            ++ (if List.length xs > 0 then text config.separator else empty)
            ++ (if List.length xs > 0 then
              (if config.space_after_separator then sep " " else sep "") else empty)
            )
            xs
        | [] -> acc ++ (if config.align_closing == false then ending else empty)
      in
      group (
        (if config.align_closing then empty
         else text config.opening ++ (if config.space_after_opening then text " " else empty))
        ++
        nest config.indent_body (
          (if config.align_closing == true then
            text config.opening
            ++ (if config.space_after_opening then sep " " else sep "")
            else empty)
          ++ (group (docs_of_list empty lst))
        )
        ++ (if config.align_closing == true then ending else empty)
      )
  | Label (key, value, config) ->
      let open Pretty in
      let open Infix in
      group (
        nest config.indent_after_label (
          to_doc key
          ++ (if config.space_after_label then sep " " else empty)
          ++ (if not(config.space_after_label) then sep "" else empty)
        )
        ++ to_doc value
      )

