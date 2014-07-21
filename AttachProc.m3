MODULE AttachProc;

IMPORT Text;

PROCEDURE Equal(k1 :T; k2 :T) :BOOLEAN =
BEGIN
  (* it doesn't matter what this returns, it has to be here to
  satisfy the List.ig interface, but we're never going to call it -
  anyway, a notion of equality of procs is dodgy at best *)
  RETURN Text.Equal(k1.VBTname, k2.VBTname);
END Equal;

BEGIN
END AttachProc.

