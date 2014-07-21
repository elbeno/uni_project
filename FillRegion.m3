MODULE FillRegion;

IMPORT Region;

PROCEDURE Equal(k1 :T; k2 :T) :BOOLEAN =
BEGIN
  RETURN Region.Equal(k1.r,k2.r);
END Equal;

BEGIN
END FillRegion.
