INTERFACE Approximation;

IMPORT Data;

EXCEPTION
  Error(TEXT);

TYPE
  T = OBJECT
        desc :TEXT;
        xcol :INTEGER;
        ycol :INTEGER;
        order :INTEGER := -1;
      METHODS
        eval(a :REF ARRAY OF Data.Element; z :Data.T): Result RAISES {Error};
      END;

  Result = RECORD
             yval :Data.Element;
             error :Data.Element;
           END;

PROCEDURE Create(t: TEXT; z :Data.T; x,y :INTEGER; order :INTEGER := 0; fractacc :Data.Element :=0.0d0; orthfunctions :TEXT := "polybasis1"): T RAISES {Error};

END Approximation.
