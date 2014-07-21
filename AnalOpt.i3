INTERFACE AnalOpt;

IMPORT FormsVBT;

TYPE
  Proc = PROCEDURE(form :FormsVBT.T) RAISES {Error};
  T = RECORD
        desc :TEXT;
        VBTname :TEXT;
        proc :Proc;
      END;

PROCEDURE Equal(k1 :T; k2 :T) :BOOLEAN;

EXCEPTION
  Error(TEXT);

END AnalOpt.