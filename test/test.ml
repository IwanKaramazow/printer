(* module W = Wadler.Pretty *)
module W = Pretty

open W.Infix

let test_passed = ref true

let test ?cursorOffset:(cursorOffset=(-1)) ~expected ~actual () =
  let cursorTest = match cursorOffset with
  | (-1) -> true
  | offset ->
  let open W in
      offset = actual.cursor
  in
  let open W in
  if expected = actual.output && cursorTest = true
  then ()
  else 
    let () = test_passed := false in
    let () = print_endline ">>>> Test failed: " in
    let () = print_endline "|> Got: " in
    let () = print_newline () in
    let () = print_endline actual.output in
    let () = print_endline "|> But expected: " in
    let () = print_endline expected in
    let () = match cursorOffset with
    | -1 -> ()
    | offset ->
        print_endline "\n >>> Cursor: ";
        print_endline "|> Got: ";
        print_newline ();
        print_endline (string_of_int actual.cursor);
        print_endline "|> But expected: ";
        print_endline (string_of_int offset) 
    in
    print_newline ()

let () = 
  let doc = W.empty in
  let s = W.pretty 15 doc in
  test ~expected:"" ~actual:s ()

let () =
  let doc = 
    W.group (W.text "julian" ++ W.sep " " ++ W.text "carax") in
  test ~expected:"julian\ncarax" ~actual:(W.pretty 10 doc) ()

let () =
  let doc = 
    W.group (W.nest 2 ((W.text "julian") ++ W.sep " ") ++ W.text "carax")
  in
  test ~expected:"julian\n  carax" ~actual:(W.pretty 10 doc) ()

(* this is weird, a `Nest` only indents when it's in a group that breaks, *)
(* which makes actually sense when you think about how the implementation works  *)
let () =
  let doc = W.group (W.nest 2 (W.text "julian")) in
  let s = W.pretty 10 doc in
  let expected = "julian" in
  test ~expected ~actual:s ()

let () =
  let doc = W.group (W.nest 2 (W.sep "" ++ W.text "julian")) in
  let s = W.pretty 4 doc in
  let expected = "\n  julian" in
  test ~expected ~actual:s ()

let () =
  let binop left op right =
    W.group (
      W.nest 2 (W.group (W.group (W.text left ++ W.sep " " ++ W.text op) ++ W.sep " " ++ W.text right))
    )
  in
  let doc = binop "a" "==" "b" in
  test ~expected:"a == b" ~actual:(W.pretty 6 doc) ();
  test ~expected:"a ==\n  b" ~actual:(W.pretty 5 doc) ();
  test ~expected:"a\n  ==\n  b" ~actual:(W.pretty 3 doc) ()

let () =
  let binop left op right =
    W.nest 2 (W.text left ++ W.sep " " ++ W.text op ++ W.sep " " ++ W.text right)
  in
  let doc = binop "a" "==" "b" in
  test ~expected:"a == b" ~actual:(W.pretty 6 doc) ();
  test ~expected:"a\n  ==\n  b" ~actual:(W.pretty 5 doc) ();
  test ~expected:"a\n  ==\n  b" ~actual:(W.pretty 3 doc) ()

let () =
  let binop left op right =
    W.group (W.nest 2 (W.group (W.text left ++ W.sep " " ++ W.text op)) ++ W.sep " " ++ W.text right)
  in
  let doc = binop "a" "==" "b" in
  test ~expected:"a == b"  ~actual:(W.pretty 6 doc) ();
  test ~expected:"a ==\nb" ~actual:(W.pretty 5 doc) ();
  test ~expected:"a\n  ==\nb" ~actual:(W.pretty 3 doc) ()

let () =
  let binop left op right =
    W.group (W.nest 2 ((W.group (W.text left ++ W.sep " " ++ W.text op)) ++ W.sep " " ++ W.text right))
  in
  let ifthen c e1 e2 =
    (W.group (W.nest 2 (W.text "if" ++ W.sep " " ++ c))) ++ W.sep " " ++
    (W.group (W.nest 2 (W.text "then" ++ W.sep " " ++ e1))) ++ W.sep " " ++
    (W.group (W.nest 2 (W.text "else" ++ W.sep " " ++ e2)))
  in
  let doc = ifthen (binop "a" "==" "b") (binop "a" "<<" "b") (binop "a" "+" "b") in
  test ~expected:"if a == b then a << b else a + b" ~actual:(W.pretty 32 doc) ();
  test ~expected:"if a == b\nthen a << b\nelse a + b" ~actual:(W.pretty 15 doc) ();
  test ~expected:"if a == b\nthen\n  a << b\nelse a + b" ~actual:(W.pretty 10 doc) ();
  test ~expected:"if\n  a == b\nthen\n  a << b\nelse\n  a + b" ~actual:(W.pretty 8 doc) ();
  test ~expected:"if\n  a ==\n    b\nthen\n  a <<\n    b\nelse\n  a + b" ~actual:(W.pretty 7 doc) ();
  test ~expected:"if\n  a ==\n    b\nthen\n  a <<\n    b\nelse\n  a +\n    b" ~actual:(W.pretty 6 doc) ()

let () =
  let open W in
  let doc =
    nest 2 (text "{" ++ sep " " ++ group (
      text "julian" ++ text "," ++ sep " "
      ++ text "julian" ++ text "," ++ sep " "
      ++ text "julian" ++ text "," )) ++ sep " " ++ text "}"
  in
  test ~expected:"{\n  julian,\n  julian,\n  julian,\n}" ~actual:(W.pretty 10 doc) ()

let () =
  let open W in
  let doc =
    nest 2 (text "{" ++ sep " " ++ group (
      text "julian" ++ text "," ++ sep " "
      ++ text "julian" ++ text "," ++ sep " "
      ++ text "julian" ++ text ","  ++ text " " ++ text "}"))
  in
  test ~expected:"{\n  julian,\n  julian,\n  julian, }" ~actual:(W.pretty 10 doc) ()

let () =
  let open W in
  let doc = text "{" ++ text " " ++
    nest 2 ( group (
      text "julian" ++ text "," ++ sep " "
      ++ text "julian" ++ text "," ++ sep " "
      ++ text "julian" ++ text ","  ++ text " " ++ text "}"))
  in
  test ~expected:"{ julian,\n  julian,\n  julian, }" ~actual:(W.pretty 10 doc) ()


let () =
  let open W in
  let doc = text "{" ++ text " " ++
    nest 2 ( group (
      text "julian" ++ text "," ++ sep " "
      ++ cursor ++ text "julian" ++ text "," ++ sep " "
      ++ text "julian" ++ text ","  ++ text " " ++ text "}"))
  in
  let result = W.pretty 10 doc in
  test ~cursorOffset:13 ~expected:"{ julian,\n  julian,\n  julian, }" ~actual:result ()

let () =
  let open W in
  let doc = text "{" ++ text " " ++
    nest 2 ( group (
      text "julian" ++ text "," ++ sep " "
      ++ text "julian" ++ text "," ++ sep " "
      ++ text "julian" ++ ifbreaks "," ""  ++ text " " ++ text "}"))
  in
  test ~expected:"{ julian,\n  julian,\n  julian, }" ~actual:(W.pretty 10 doc) ();
  test ~expected:"{ julian, julian, julian }" ~actual:(W.pretty 100 doc) ()


let () =
  let open W in
  let doc = 
    group (
      group(
        nest 2 (
          text "window.setTimeout" ++ text "(" ++ sep "" ++
          group(
            nest 2 (
              text "function()" ++ text "{" ++ sep " " ++ text "doStuff();"
            )
            ++ sep " " ++ text "}"
          )
        ) 
        ++ sep "" ++ text "}"
      )
    )
  in
  test ~expected:{|window.setTimeout(
  function(){
    doStuff();
  }
}|} ~actual:(W.pretty 20 doc) ()

let () =
  if !test_passed then
    print_endline "Wadler printing algorithm passed :D !"
  else
    print_endline "Wadler printing algorithm failed!!!!!! :("

