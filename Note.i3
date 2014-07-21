INTERFACE Note;

IMPORT Point, Font;

TYPE
  T = OBJECT
        t :TEXT;
        f :Font.T;
        p :Point.T;
      END;

PROCEDURE Equal(k1:T; k2:T) :BOOLEAN;

END Note.