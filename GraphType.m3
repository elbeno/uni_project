MODULE GraphType;

IMPORT Text;

PROCEDURE Equal(k1 :T; k2 :T): BOOLEAN =
BEGIN
  RETURN Text.Equal(k1.desc, k2.desc);
END Equal;

BEGIN
END GraphType.

