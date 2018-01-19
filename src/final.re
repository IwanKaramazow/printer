/* exploration of http://davidchristiansen.dk/drafts/final-pretty-printer-draft.pdf */
/* type chunk('w) = */
  /* | Text(string) */
  /* | Space('w); */

/* type atom('w) = */
  /* | Chunk(chunck('w)) */
  /* | Newline; */

/* type line('w, 'fmt) = list((chunk('w), fmt)); */

/* type output('w, 'ann) = */
  /* | Nil */
  /* | Atom(atom('w)) */
  /* | Annot('ann, output('w, 'ann)) */
  /* | Cons(output('w, 'ann), output('w, 'ann)); */
