MODULE Data;

IMPORT IO, Rd, Lex, Text, FloatMode, Scan, Fmt, Wr, LongReal;

REVEAL
  Private = BRANDED "PRIVDATA" OBJECT END;
  T = Public BRANDED "PUBDATA" OBJECT 
      OVERRIDES
        init := InitEmpty;
        initFromDataFile := DataFileInit;
        initFromGraphFile := GraphFileInit;
        saveToDataFile := DataFileSave;
        saveToGraphFile := GraphFileSave;
        max := MaxValue;
        min := MinValue;
      END;

PROCEDURE FmtEl(e :Element; s := Fmt.Style.Auto; p :CARDINAL := LongReal.MaxSignifDigits - 1; l := FALSE) :TEXT = 
BEGIN
  RETURN Fmt.LongReal(e,s,p,l);
END FmtEl;

PROCEDURE ScanEl(t :TEXT) :Element RAISES {Lex.Error, FloatMode.Trap} =
BEGIN
  RETURN Scan.LongReal(t);
END ScanEl;

PROCEDURE InitEmpty(self :T; f :INTEGER := 2; r :INTEGER := 1): T =
BEGIN
  self.fields := f;
  self.records := r;
  self.values := NEW(REF ARRAY OF Element, f*r);
(*
  (* fill the array with dummy data for testing *)
  (* an x^3 curve from -1 to 1 *)
  FOR i := 0 TO r-1 DO
      self.values^[i*f+1] := -1.0d0 + FLOAT(i, LONGREAL)/10.0d0;
      self.values^[i*f] := (self.values^[i*f+1])*(self.values^[i*f+1])*(self.values^[i*f+1]);
  END;
*)
  RETURN self;
END InitEmpty;

PROCEDURE MaxValue(self :T; field :INTEGER): Element RAISES {NoSuchField}=
VAR
  max :Element;
BEGIN
  IF field > self.fields THEN
    RAISE NoSuchField;
  END;
  max := self.values[field];
  FOR i := 1 TO self.records-1 DO
    IF self.values[i*self.fields + field] > max THEN
      max := self.values[i*self.fields + field];
    END;
  END;
  RETURN max;
END MaxValue;

PROCEDURE MinValue(self :T; field :INTEGER): Element RAISES {NoSuchField} =
VAR
  min :Element;
BEGIN
  IF field > self.fields THEN
    RAISE NoSuchField;
  END;
  min := self.values[field];
  FOR i := 1 TO self.records-1 DO
    IF self.values[i*self.fields + field] < min THEN
      min := self.values[i*self.fields + field];
    END;
  END;
  RETURN min;
END MinValue;

PROCEDURE DataFileInit(self :T; filename :TEXT):T RAISES {FileError} =
VAR
  file :Rd.T;
  line :TEXT;
  pos :INTEGER;
  textval :TEXT;
  val :LONGREAL;
BEGIN
  self.fields := 0;
  self.records := 0;
  (* initialise from a file *)
  file := IO.OpenRead(filename);
  IF file = NIL THEN
    RAISE FileError("Bad Filename");
  END;
  (* how many values are in the file? *)
  (* get the first line and find the number of longreals on it *)
  TRY
    line := IO.GetLine(file);
  EXCEPT
    | IO.Error => RAISE FileError("File Error");
  END;
  INC(self.records);
  WHILE Text.Length(line) # 0 DO
    pos := Text.FindChar(line,' ')+1;
    textval := Text.Sub(line,0,pos);
    TRY
      val := ScanEl(textval);
    EXCEPT
      | Lex.Error, FloatMode.Trap => RAISE FileError("Bad Format");
    END;
    INC(self.fields);
    line := Text.Sub(line,pos);
  END;  
  (* OK, now we know the number of fields *)
  WHILE NOT(Rd.EOF(file)) DO
    TRY
      line := IO.GetLine(file);
    EXCEPT
      | IO.Error => RAISE FileError("File Error");
    END;
    INC(self.records);
  END;
  (* now we know the number of records too *)
  self.values := NEW(REF ARRAY OF Element, self.fields*self.records);
  Rd.Seek(file,0);  (* seek back to the beginning *)
  FOR i := 0 TO self.fields*self.records-1 DO
    TRY
      self.values^[i] := Lex.LongReal(file);
    EXCEPT
      | Lex.Error, FloatMode.Trap => RAISE FileError("Bad Format");
      ELSE RAISE FileError("File Error*Premature EOF?");
    END;
  END;
  RETURN self;
END DataFileInit;

PROCEDURE GraphFileInit(self :T; filename :TEXT):T RAISES {FileError} =
BEGIN
  RETURN self;
END GraphFileInit;

PROCEDURE DataFileSave(self :T; filename :TEXT) RAISES {FileError} =
VAR
  file :Wr.T;
BEGIN
  file := IO.OpenWrite(filename);
  IF file = NIL THEN
    RAISE FileError("Bad Filename");
  END;
  FOR i := 0 TO self.records-1 DO
    FOR j := 0 TO self.fields-1 DO
      Wr.PutText(file, FmtEl(self.values^[i*self.fields+j]) & " ");
    END;
    Wr.PutText(file, "\n");
  END;
  Wr.Close(file);
END DataFileSave;

PROCEDURE GraphFileSave(self :T; filename :TEXT) RAISES {FileError} =
BEGIN
END GraphFileSave;

BEGIN
END Data.



