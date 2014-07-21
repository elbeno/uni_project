MODULE Note;

IMPORT Text, Point;

PROCEDURE Equal(k1 :T; k2 :T) :BOOLEAN =
BEGIN
  RETURN (Text.Equal(k1.t,k2.t) AND Point.Equal(k1.p,k2.p));
END Equal;

BEGIN
END Note.
