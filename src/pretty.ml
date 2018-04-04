(*
 * Implements the strict version of the wadler pretty printing algorithm:
 * https://pdfs.semanticscholar.org/c73c/a9cc74351fe7f8f3dddc69d2e82326af82c0.pdf
 *)

(* complex document *)
type doc = 
  | Nil
  | Text of string
  | Cons of doc * doc
  | Nest of int * doc
  | Separator of string
  | IfBreaks of string * string
  | Group of doc 
  | Cursor
  | Fill of doc list

let empty = Nil
let concat x y = Cons(x,y) 
let text s = Text s
let nest i x = Nest(i,x)
let sep s = Separator s
let group d = Group d
let cursor = Cursor
let ifbreaks a b = IfBreaks(a, b)
let fill xs = Fill xs

module Infix = struct
  let (++) = concat
end

(* simple document *)
type sdoc =
  | SNil
  | SText of (string * sdoc)
  | SLine of (int * sdoc) (* newline + spaces *)
  | SCursor of sdoc

let newline = "\n"

type printResult = {
  cursor: int;
  output: string
}

let sdocToPrintResult sdoc =
  let rec aux sdoc index cursorOffset acc =
    match sdoc with
    | SNil -> 
      {cursor = cursorOffset; output = acc }
    | SText(text, sdoc) -> 
        (* TODO calculate up front *)
        let nextIndex = index + String.length text in
        aux sdoc nextIndex cursorOffset (acc ^ text)
    | SLine (i, sdoc) ->
        let prefix = String.make i ' ' in
        aux sdoc (index + i + 2) cursorOffset (acc ^ newline ^ prefix)
    | SCursor sdoc ->
        aux sdoc index index acc 
  in
  aux sdoc 0 (-1) ""

(* mode of a group*)      
type mode = 
  | Flat
  | Break

(* TODO make this unicode aware, default to utf8, guess encoding up front ? *)
let strlen = String.length

(* The fits predicate actually checks a list of triples because the cons -operator is unfolded *)
(* into a list. Each triple (i,m,d) hold the current indentation i , the mode m of the current *)
(* group and the document d . The function can stop after it has w characters are consumed or *)
(* the document ended – whatever happens first. So at most w characters must be consumed. *)
(* Since a group is checked when rendered flat it never can contain a break which indicates a *)
(* newline. *)
let rec fit w = function
  | _ when w < 0 -> false
  | [] -> true
  | (i, m, Nil)::xs -> fit w xs
  | (i, m, Cursor)::xs -> fit w xs
  | (i, m, Text(t))::xs -> fit (w - strlen t) xs
  | (i, m, Fill(docs))::xs -> 
      let rest = List.fold_left (fun xs x -> (i, m, x)::xs) xs docs in
      fit w rest
  | (i, m, Cons(doc1, doc2))::xs -> fit w ((i, m, doc1)::(i, m, doc2)::xs)
  | (i, m, Nest(indent, doc))::xs -> fit w ((i + indent, m, doc)::xs)
  | (i, Flat, Separator(sep))::xs -> fit (w - strlen sep) xs
  | (i, Break, Separator(sep))::xs -> true
  | (i, Flat, IfBreaks(_, noBreak))::xs -> fit (w - strlen noBreak) xs
  | (i, Break, IfBreaks(whenBreaks, _))::xs -> fit (w - strlen whenBreaks) xs
  | (i, m, Group(doc))::xs -> fit w ((i, Flat, doc)::xs)


(* transforms a complex doc into a simple doc*)
(*
 * So the actual mode m and indentation level i are paired with every element by the format function.
 * Its parameter w denotes the actual line length and the parameter k
 * how much characters of the current line have already been consumed.
 *)
let rec format w k = function
  | [] -> SNil
  | (i, m, Nil)::rest -> format w k rest
  | (i, m, Cursor)::rest -> SCursor(format w k rest)
  | (i, m, Text text)::rest -> 
      SText(text, format w (k + strlen text) rest)
  | (i, Flat, IfBreaks(_, noBreak))::rest ->
      SText(noBreak, format w (k + strlen noBreak) rest)
  | (i, Break, IfBreaks(whenBreaks, _))::rest ->
      SText(whenBreaks, format w (k + strlen whenBreaks) rest)
  | (i, m, Cons (doc1, doc2))::rest -> format w k ((i, m, doc1)::(i, m, doc2)::rest)
  | (i, Flat, Separator(sep))::rest -> SText(sep, format w (k + strlen sep) rest)
  | (i, Break, Separator(s))::rest -> SLine(i, format w i rest)
  | (i, m, Nest(indent, doc))::rest -> format w k ((i + indent, m, doc)::rest)
  | (i, m, Fill(docs))::rest ->
      (* TODO performance *)
      let rec aux acc = function
        | x::xs -> 
            if fit (w-k) ((i, Flat, x)::(acc@rest)) then
              aux ((i, Flat, x)::acc) xs
            else
              aux ((i, Break, x)::(i, Break, Separator(""))::acc) xs
        | [] -> List.rev acc
      in
      let newParts = aux [] docs in
      format w k (newParts@rest)
  | (i, m, Group(doc))::rest -> 
    if fit (w-k) ((i, Flat, doc)::rest) then
      format w k ((i, Flat, doc)::rest)
    else
      format w k ((i, Break, doc)::rest)

let pretty w doc =
  let sdoc = format w 0 [(0, Flat, Group doc)] in
  sdocToPrintResult sdoc
