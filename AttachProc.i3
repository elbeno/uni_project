INTERFACE AttachProc;

IMPORT FormsVBT;

TYPE
  T = RECORD
        VBTname :TEXT;
        p :FormsVBT.Proc;
      END;

PROCEDURE Equal(k1 :T; k2 :T) :BOOLEAN;

END AttachProc.
