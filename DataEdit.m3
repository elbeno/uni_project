MODULE DataEdit;

IMPORT Data, Fmt, FormsVBT, Lex, Wr, Stdio, FloatMode, Rsrc, GraphUIBundle, Text, VBT, Trestle, Filter, TextPort, ViewportVBT;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

REVEAL
  Private = FormsVBT.T BRANDED "PRIVDEDIT" OBJECT END;
  T = Public BRANDED "PUBDEDIT" OBJECT 
               rows :INTEGER;
               cols :INTEGER;
             OVERRIDES
               init := Init;
               cellstodata := CellsToData;
               datatocells := DataToCells;
               addrows := InsertRecords;
               removerows := RemoveRecords;
               addcols := InsertFields;
               removecols := RemoveFields;
             END;

PROCEDURE New() :T =
BEGIN
  WITH n = NEW(T) DO
    n.data := NEW(Data.T).init();
    n.datavisible := FALSE;
    RETURN n.init();
  END;
END New;

PROCEDURE Init(self: T): T =
VAR
  celldesc1 := "(Shape %cell";
  celldesc2 := " (Width 100) (Rim (Pen 5) (Font (Family \"fixed\") (PointSize 130) (WeightName \"medium\")) (Frame Lowered (TypeIn (BgColor \"White\")))))"; 
  cellno :TEXT;
  v :VBT.T;
BEGIN
  self.rows := self.data.records;
  self.cols := self.data.fields;
  TRY
    EVAL self.initFromRsrc("DataControl.fv",Rsrc.BuildPath(GraphUIBundle.Get()));
  EXCEPT
    | FormsVBT.Error(e) => Wr.PutText(Stdio.stdout,e);
                           Wr.Close(Stdio.stdout);
  END;
  FOR i := 0 TO self.rows-1 DO   (* iterate over rows *)
    EVAL FormsVBT.Insert(self, "records","(HBox %fields" & Fmt.Int(i) & ")");
    EVAL FormsVBT.Insert(self,"fields" & Fmt.Int(i),"(Shape (Width 50) (Text \"" & Fmt.Int(i) & "\"))");
    FOR j := 0 TO self.cols-1 DO   (* iterate over columns *)
      cellno := Fmt.Int(i) & "_" & Fmt.Int(j);
      EVAL FormsVBT.Insert(self,"fields" & Fmt.Int(i), celldesc1 & cellno & celldesc2);
      v := FormsVBT.GetVBT(self, "cell" & cellno);
      v := Filter.Child(Filter.Child(Filter.Child(v)));
      TextPort.SetText(v,Data.FmtEl(self.data.values^[i*self.cols+j]));
    END;
  END;
  (* now we have the whole of the data loaded into TypeinVBT's arranged by
     rows and columns *) 
  (* fill the bottom of the VBox if there aren't enough records *)
  IF self.rows * 28 < 400 THEN
    WITH filler = NEW(FormsVBT.T).init("(VBox (Glue " & Fmt.Int(400-self.rows*28) & "))") DO
      FormsVBT.InsertVBT(self, "records", filler); 
    END; 
  END; 
  FormsVBT.AttachProc(self,"add",Add);
  FormsVBT.AttachProc(self,"remove",Remove);
  FormsVBT.AttachProc(self,"upd",Update);
  FormsVBT.AttachProc(self,"update",UpdateClose);
  FormsVBT.AttachProc(self,"discard",Discard);
  RETURN self; 
END Init;

PROCEDURE DataToCells(self :T) =
VAR
  v :VBT.T;
BEGIN
  self.rows := self.data.records;
  self.cols := self.data.fields;
  FOR i := 0 TO self.rows-1 DO
    FOR j := 0 TO self.cols-1 DO
      v := FormsVBT.GetVBT(self,"cell" & Fmt.Int(i) & "_" & Fmt.Int(j));
      v := Filter.Child(Filter.Child(Filter.Child(v)));
      TextPort.SetText(v, Data.FmtEl(self.data.values^[i*self.cols+j]));
    END;
  END;
END DataToCells;

PROCEDURE CellsToData(self :T) =
VAR
  v :VBT.T;
BEGIN
  self.data.records := self.rows;
  self.data.fields := self.cols;
  IF NUMBER(self.data.values^) # self.rows * self.cols THEN
    self.data.values := NEW(REF ARRAY OF Data.Element, self.rows * self.cols);
  END;
  FOR i := 0 TO self.rows-1 DO
    FOR j := 0 TO self.cols-1 DO
      v := FormsVBT.GetVBT(self,"cell" & Fmt.Int(i) & "_" & Fmt.Int(j));
      v := Filter.Child(Filter.Child(Filter.Child(v)));      
      TRY
        self.data.values^[i*self.cols+j] := Data.ScanEl(TextPort.GetText(v));
      EXCEPT
        | FormsVBT.Error(e) =>
          Wr.PutText(Stdio.stdout, e);
        | Lex.Error, FloatMode.Trap => 
          TextPort.SetText(v, "0");
          self.data.values^[i] := 0.0d0;  
      END;
    END;
  END; 
END CellsToData;

PROCEDURE InsertRecords(self :T; start, number :INTEGER) =
VAR
  celldesc1 := "(Shape %cell";
  celldesc2 := " (Width 100) (Rim (Pen 5) (Font (Family \"fixed\") (PointSize 130) (WeightName \"medium\")) (Frame Lowered (TypeIn (BgColor \"White\")))))"; 
  cellno :TEXT;
  v,w :VBT.T;
BEGIN
  IF number = 0 THEN
    RETURN;
  END;
  IF start > self.rows THEN 
    start := self.rows; 
  END;
  (* remove the glue *)
  IF self.rows * 28 < 400 THEN
      FormsVBT.Delete(self, "records", self.rows); 
  END; 
  (* insert the cells into the vbt at the end to start with *)
  FOR i := self.rows TO self.rows + number-1 DO   (* iterate over rows *)
    EVAL FormsVBT.Insert(self, "records","(HBox %fields" & Fmt.Int(i) & ")");
    EVAL FormsVBT.Insert(self,"fields" & Fmt.Int(i),"(Shape (Width 50) (Text \"" & Fmt.Int(i) & "\"))");
    FOR j := 0 TO self.cols-1 DO   (* iterate over columns *)
      cellno := Fmt.Int(i) & "_" & Fmt.Int(j);
      EVAL FormsVBT.Insert(self,"fields" & Fmt.Int(i), celldesc1 & cellno & celldesc2);
    END;
  END;  
  (* inserted the cells - now move the data *)
  FOR i := self.rows-1 TO start BY -1 DO
    FOR j := 0 TO self.cols-1 DO
      cellno := Fmt.Int(i) & "_" & Fmt.Int(j);
      v := FormsVBT.GetVBT(self,"cell" & cellno);
      v := Filter.Child(Filter.Child(Filter.Child(v)));
      w := FormsVBT.GetVBT(self,"cell" & Fmt.Int(i+number) & "_" & Fmt.Int(j));
      w := Filter.Child(Filter.Child(Filter.Child(w)));  
      TextPort.SetText(w, TextPort.GetText(v));
      TextPort.SetText(v,"");    
    END;
  END; 
  (* update the number of records *)
  self.rows := self.rows + number;
  (* put the glue back *)
  IF self.rows * 28 < 400 THEN
    WITH filler = NEW(FormsVBT.T).init("(VBox (Glue " & Fmt.Int(400-self.rows*28) & "))") DO
      FormsVBT.InsertVBT(self, "records", filler); 
    END; 
  END; 
END InsertRecords;

PROCEDURE RemoveRecords(self :T; start, number :INTEGER) =
VAR
  cellno :TEXT;
  v,w :VBT.T;
BEGIN
  IF number = 0 THEN
    RETURN;
  END;
  IF start+number > self.rows THEN
    number := self.rows-start;
  END;
  (* remove the glue *)
  IF self.rows * 28 < 400 THEN
      FormsVBT.Delete(self, "records", self.rows); 
  END;
  (* move the data up to start with *)
  FOR i := start+number TO self.rows-1 DO
    FOR j := 0 TO self.cols-1 DO
      cellno := Fmt.Int(i) & "_" & Fmt.Int(j);
      v := FormsVBT.GetVBT(self,"cell" & cellno);
      v := Filter.Child(Filter.Child(Filter.Child(v)));
      w := FormsVBT.GetVBT(self,"cell" & Fmt.Int(i-number) & "_" & Fmt.Int(j));
      w := Filter.Child(Filter.Child(Filter.Child(w)));  
      TextPort.SetText(w, TextPort.GetText(v));    
    END;
  END;
  (* remove the records from the end *)
  FOR i := self.rows-1 TO self.rows-number BY -1 DO
    FOR j := self.cols TO 1 BY -1 DO
      FormsVBT.Delete(self, "fields" & Fmt.Int(i), j);
    END;
    FormsVBT.Delete(self, "records", i);
  END;
  (* update the number of records *)
  self.rows := self.rows - number;
  (* put the glue back *)
  IF self.rows * 28 < 400 THEN
    WITH filler = NEW(FormsVBT.T).init("(VBox (Glue " & Fmt.Int(400-self.rows*28) & "))") DO
      FormsVBT.InsertVBT(self, "records", filler); 
    END; 
  END;
END RemoveRecords;

PROCEDURE InsertFields(self :T; start, number :INTEGER) =
VAR
  celldesc1 := "(Shape %cell";
  celldesc2 := " (Width 100) (Rim (Pen 5) (Font (Family \"fixed\") (PointSize 130) (WeightName \"medium\")) (Frame Lowered (TypeIn (BgColor \"White\")))))"; 
  cellno :TEXT;
  v,w :VBT.T;
  viewno :INTEGER;
BEGIN
  IF number = 0 THEN
    RETURN;
  END;
  IF start > self.cols THEN 
    start := self.cols;
  END;
  (* insert the cells into the vbt at the end to start with *)
  FOR i := self.cols TO self.cols + number-1 DO   (* iterate over columns *)
    FOR j := 0 TO self.rows-1 DO   (* iterate over rows *)
      cellno := Fmt.Int(j) & "_" & Fmt.Int(i);
      TRY
        EVAL FormsVBT.Insert(self,"fields" & Fmt.Int(j), celldesc1 & cellno & celldesc2);
      EXCEPT
        | FormsVBT.Error(e) => Wr.PutText(Stdio.stdout,e);
                               Wr.Close(Stdio.stdout);
      END;
    END;
  END;  
  (* inserted the cells - now move the data *)
  FOR i := self.cols-1 TO start BY -1 DO
    FOR j := 0 TO self.rows-1 DO
      cellno := Fmt.Int(j) & "_" & Fmt.Int(i);
      v := FormsVBT.GetVBT(self,"cell" & cellno);
      v := Filter.Child(Filter.Child(Filter.Child(v)));
      w := FormsVBT.GetVBT(self,"cell" & Fmt.Int(j) & "_" & Fmt.Int(i+number));
      w := Filter.Child(Filter.Child(Filter.Child(w)));  
      TextPort.SetText(w, TextPort.GetText(v));
      TextPort.SetText(v,"");    
    END;
  END; 
  (* update the number of fields *)
  self.cols := self.cols + number;
  WITH view = FormsVBT.GetVBT(self,"view") DO
    viewno := ViewportVBT.AddView(view,-1);
    ViewportVBT.RemoveView(view,viewno);
  END;
END InsertFields;

PROCEDURE RemoveFields(self :T; start, number :INTEGER) =
VAR
  cellno :TEXT;
  v,w :VBT.T;
  viewno :INTEGER;
BEGIN
  IF number = 0 THEN
    RETURN;
  END;
  IF start+number > self.cols THEN
    number := self.cols-start;
  END;
  (* move the data up to start with *)
  FOR i := start+number TO self.cols-1 DO
    FOR j := 0 TO self.rows-1 DO
      cellno := Fmt.Int(j) & "_" & Fmt.Int(i);
      v := FormsVBT.GetVBT(self,"cell" & cellno);
      v := Filter.Child(Filter.Child(Filter.Child(v)));
      w := FormsVBT.GetVBT(self,"cell" & Fmt.Int(j) & "_" & Fmt.Int(i-number));
      w := Filter.Child(Filter.Child(Filter.Child(w)));  
      TextPort.SetText(w, TextPort.GetText(v));    
    END;
  END;
  (* remove the fields from the end *)
  FOR i := self.cols TO self.cols-number+1 BY -1 DO
    FOR j := self.rows-1 TO 0 BY -1 DO
      FormsVBT.Delete(self, "fields" & Fmt.Int(j), i);
    END;
  END;
  (* update the number of records *)
  self.cols := self.cols - number;
  WITH view = FormsVBT.GetVBT(self,"view") DO
    viewno := ViewportVBT.AddView(view,-1);
    ViewportVBT.RemoveView(view,viewno);
  END;
END RemoveFields;

PROCEDURE UpdateClose(             form :FormsVBT.T;
                      <* UNUSED *> event:Text.T;
                      <* UNUSED *> cl   :REFANY;
                      <* UNUSED *> ts   :VBT.TimeStamp)=
BEGIN
  WITH f = NARROW(form, T) DO
    f.cellstodata();
    f.datavisible := FALSE;
  END;
  Trestle.Delete(form);
END UpdateClose;

PROCEDURE Update(             form :FormsVBT.T;
                 <* UNUSED *> event:Text.T;
                 <* UNUSED *> cl   :REFANY;
                 <* UNUSED *> ts   :VBT.TimeStamp)=
BEGIN
  WITH f = NARROW(form, T) DO
    f.cellstodata();
  END;
END Update;

PROCEDURE Discard(             form :FormsVBT.T;
                  <* UNUSED *> event:Text.T;
                  <* UNUSED *> cl   :REFANY;
                  <* UNUSED *> ts   :VBT.TimeStamp)=
BEGIN
  WITH f = NARROW(form, T) DO
    f.datavisible := FALSE;
  END;
  Trestle.Delete(form);
END Discard;

PROCEDURE Add(             form :FormsVBT.T;
              <* UNUSED *> event:Text.T;
              <* UNUSED *> cl   :REFANY;
              <* UNUSED *> ts   :VBT.TimeStamp)=
VAR
  numtoadd :INTEGER;
  startpos :INTEGER;
BEGIN
  numtoadd := FormsVBT.GetInteger(form, "number");
  startpos := FormsVBT.GetInteger(form, "pos");
  IF Text.Equal(FormsVBT.GetChoice(form, "rowsorcols"), "rows") THEN
    WITH f = NARROW(form, T) DO
      f.addrows(startpos, numtoadd);
    END;
  ELSE
    WITH f = NARROW(form, T) DO
      f.addcols(startpos, numtoadd);
    END;
  END;
END Add;

PROCEDURE Remove(             form :FormsVBT.T;
                 <* UNUSED *> event:Text.T;
                 <* UNUSED *> cl   :REFANY;
                 <* UNUSED *> ts   :VBT.TimeStamp)=
VAR
  numtoadd :INTEGER;
  startpos :INTEGER;
BEGIN
  numtoadd := FormsVBT.GetInteger(form, "number");
  startpos := FormsVBT.GetInteger(form, "pos");
  IF Text.Equal(FormsVBT.GetChoice(form, "rowsorcols"), "rows") THEN
    WITH f = NARROW(form, T) DO
      f.removerows(startpos, numtoadd);
    END;
  ELSE
    WITH f = NARROW(form, T) DO
      f.removecols(startpos, numtoadd);
    END;
  END;
END Remove;

BEGIN
END DataEdit.

