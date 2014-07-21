INTERFACE FillRegion;

IMPORT Region, PaintOp;

TYPE
  T = OBJECT
        p :PaintOp.T;
        r :Region.T;
      END;

PROCEDURE Equal(k1:T; k2:T) :BOOLEAN;

END FillRegion.