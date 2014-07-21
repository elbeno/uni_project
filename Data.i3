INTERFACE Data;

(* A Data.T is the basic Data Object in the program. *)
(* It is a reference to an array (perhaps a list?) of data elements. *)

IMPORT LongReal, FloatMode, Lex, Fmt;

EXCEPTION
  FileError(TEXT);
  NoSuchField;

TYPE
   Element = LONGREAL;                (* for now, make it a longreal number *)
   Private <: ROOT;
   Public = Private OBJECT
      fields  :INTEGER;
      records :INTEGER;
      values  :REF ARRAY OF Element;
      METHODS
         init(fields :INTEGER :=2; records :INTEGER:=1) :T;
         initFromDataFile(filename :TEXT) :T RAISES {FileError};
         initFromGraphFile(filename :TEXT) :T RAISES {FileError};
         saveToDataFile(filename :TEXT) RAISES {FileError};
         saveToGraphFile(filename :TEXT) RAISES {FileError};
         max(field :INTEGER) :Element RAISES {NoSuchField};
         min(field :INTEGER) :Element RAISES {NoSuchField};
      END;
   T <: Public;

PROCEDURE ScanEl(t :TEXT) :Element RAISES {Lex.Error, FloatMode.Trap};
PROCEDURE FmtEl(e :Element; s := Fmt.Style.Auto; prec :CARDINAL := LongReal.MaxSignifDigits - 1; l := FALSE) :TEXT;

END Data.