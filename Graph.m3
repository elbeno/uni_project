MODULE Graph;

IMPORT Trestle, Fmt, Wr, Stdio, GraphUI, Common, Eval, DataEdit, DisplayGraph, FormsVBT, Value, Text, Data, Scan, Approximation, AnalOpt, Thread;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented, Thread.Alerted, Wr.Failure *>

REVEAL
  Private = GraphUI.T BRANDED "PRIVGRAPH" OBJECT END;
  T = Public BRANDED "PUBGRAPH" OBJECT
      OVERRIDES
        init := Init;
        formeval := FormEval;
        eval := Evaluate;
      END;

PROCEDURE New(): T =
BEGIN
  RETURN NEW(T).init();
END New;

PROCEDURE Init(self: T) :T =
VAR
  i :INTEGER;
  p :DisplayGraph.InitProc;
  q :DisplayGraph.OptBlockProc;
  z :DisplayGraph.T;
BEGIN
  (* Install the Control Panel *)
  i := Common.NumberofPanels.inc();   (* new panel number *)
  self := GraphUI.T.init(self);    (* init the Control Panel *)
  self.number := i;

  (* Initialise the Data Editing Window *) 
  self.graphdata := DataEdit.New(); 

  (* The Displayable Graph *)
  (* set it up as the first one in the graph type list *)
  p := Common.GraphTypes.head.proc;
  q := Common.GraphTypes.head.newoptions;
  self.graph := NEW(FormsVBT.T).init("(Shape (Width 500) (Height 500) (Generic %gengraph))");
  z := DisplayGraph.New(self,p,q);
  FormsVBT.PutGeneric(self.graph,"gengraph",z);

  (* initialise the formula *)
  self.formula := Eval.New();

  (* other stuff *)

  RETURN self;
END Init;

PROCEDURE FormEval(self :T; decls :TEXT; expr :TEXT) RAISES {Eval.LexError, Eval.SyntaxError, Eval.EvalError, Value.Silly} =
VAR
  l :REF Eval.EntryList;
  i, size, step, incstep, count :INTEGER := 1;
  e :Eval.Entry;
  t :TEXT;
  max, min, v, increment :LONGREAL;
  refval :REF LONGREAL;
BEGIN

  (* start with a new symbol table and stack *)
  self.formula := Eval.New();

  Wr.PutText(Stdio.stdout, "Made a new symbol table and stack\n");
  Wr.Flush(Stdio.stdout);

  (* start by evaluating the variable declarations *)
  self.formula.formula := decls;
  Wr.PutText(Stdio.stdout, decls);
  Wr.Flush(Stdio.stdout);
  
  self.formula.parse();
  (* successfully parsed *)

  Wr.PutText(Stdio.stdout, "Parsed the declarations successfully\n");
  Wr.Flush(Stdio.stdout);  

  (* dimension the mappings array *)
  self.formula.mappings := NEW(REF ARRAY OF TEXT, self.formula.varcount);

  (* step through the symbol table mapping the id's to column values *)
  l := self.formula.symtable.list;
  i := 0;
  WHILE l # NIL DO
    IF l^.head.token = Eval.id THEN
      (* we've got hold of an id *)
      self.formula.mappings[i] := l^.head.lexeme;   (* map it to the next data column *)
      INC(i);
    END;
    l := l^.tail;
  END;

  Wr.PutText(Stdio.stdout, "Mapped the variables to column values\n");
  Wr.Flush(Stdio.stdout);

  WHILE NOT(self.formula.stack.empty()) DO
    EVAL self.formula.eval();
  END;
  (* successfully evaluated *)

  Wr.PutText(Stdio.stdout, "Evaluated the declarations\n");
  Wr.Flush(Stdio.stdout);

  (* now the symbol table contains all the variables needed, possibly also *)
  (* the domains of those variables <var>max <var>min and <var>steps *)
  (* let's check that we have all the domains we need *)
  FOR j := 0 TO self.formula.varcount - 1 DO
    (* just check for the existence of <var>max - we can assume the rest *)
    e := self.formula.symtable.lookup(self.formula.mappings[j]);
    IF self.formula.symtable.lookup(self.formula.mappings[j] & "max").token = 0 AND
       NOT(Text.Equal(e.attrib.totext(),"subject")) THEN
      (* we don't have the domain, and it's not the subject, so help! *)
      RAISE Eval.EvalError("No domain specified for " & self.formula.mappings[j]);
    END;
    (* find the subject of the formula and put it in column 0 *)
    IF Text.Equal(e.attrib.totext(), "subject") THEN
      t := self.formula.mappings[j];
      self.formula.mappings[j] := self.formula.mappings[0];
      self.formula.mappings[0] := t;      
    END;
  END;  
  
  Wr.PutText(Stdio.stdout, "Got all the domains\n");
  Wr.Flush(Stdio.stdout);

  (* OK, so now we have all the variables mapped to data columns, and *)
  (* we also have the domains in the symbol table (except for the subject) *)
  (* well, let's parse the expression then *)
  i := self.formula.varcount;
  self.formula.formula := expr;
  self.formula.parse();
  IF i # self.formula.varcount THEN
    RAISE Eval.EvalError("Undeclared variable*in expression");
  END;

  Wr.PutText(Stdio.stdout, "Parsed the expression OK\n");
  Wr.Flush(Stdio.stdout);
  
  Wr.PutText(Stdio.stdout, "The symbol table looks like this:\n");
  l := self.formula.symtable.list;
  WHILE l # NIL DO
    IF l^.head.token = Eval.id THEN
      (* we've got hold of an id *)
      Wr.PutText(Stdio.stdout, l^.head.lexeme & " : " & l^.head.attrib.totext()&"\n");
    END;
    l := l^.tail;
  END;
  Wr.Flush(Stdio.stdout);

  (* right, let's calculate the size of the Data.T we need *)
  FOR j := 1 TO self.formula.varcount - 1 DO
    e := self.formula.symtable.lookup(self.formula.mappings[j] & "steps");
    i := Scan.Int(e.attrib.totext()) + 1;  (* add one to include both endpoints *)
    size := size * i;
  END;  

  (* now we know the Data.T must have self.varcount columns and size rows *)
  IF self.graphdata.datavisible THEN
    Trestle.Delete(self.graphdata);
  END;
  self.graphdata := NEW(DataEdit.T);
  self.graphdata.datavisible := FALSE;
  self.graphdata.data := NEW(Data.T).init(self.formula.varcount, size);

  Wr.PutText(Stdio.stdout, "Made the new Data.T, size = " & Fmt.Int(size) & "\n");
  Wr.Flush(Stdio.stdout);

  (* fill up the Data.T *)
  incstep := size;
  FOR j := 1 TO self.formula.varcount - 1 DO
    step := Scan.Int(self.formula.symtable.lookup(self.formula.mappings[j]&"steps").attrib.totext());      
    max := Scan.LongReal(self.formula.symtable.lookup(self.formula.mappings[j]&"max").attrib.totext());
    min := Scan.LongReal(self.formula.symtable.lookup(self.formula.mappings[j]&"min").attrib.totext());
    incstep := incstep DIV step;
    increment := (max-min)/FLOAT(step, LONGREAL);
    v := min;
    count := 0;
    FOR k := 0 TO size-1 DO
      self.graphdata.data.values^[k*self.formula.varcount+j] := v;
      IF (k+1) MOD incstep = 0 THEN
        v := v + increment;
        INC(count);
        IF count > step THEN
          count := 0;
          v := min;
        ELSIF count = step THEN
          v := max;
        END;
      END;
    END;
  END;  

  Wr.PutText(Stdio.stdout, "Filled up the Data.T\n");
  Wr.Flush(Stdio.stdout);

  (* now we just have to iterate over the Data.T and load the right values *)
  (* into the symbol table each time *)
  refval := NEW(REF LONGREAL);
  FOR j := 0 TO size - 1 DO
    (* put the values in the symbol table from the Data.T *)
    FOR k := 1 TO self.formula.varcount - 1 DO
      refval^ := self.graphdata.data.values^[j*self.formula.varcount + k];
      self.formula.symtable.assignattrib(self.formula.mappings[k], Value.Create(refval));
    END; 

    Wr.PutText(Stdio.stdout, "The symbol table looks like this:\n");
    l := self.formula.symtable.list;
    WHILE l # NIL DO
      IF l^.head.token = Eval.id THEN
        (* we've got hold of an id *)
        Wr.PutText(Stdio.stdout, l^.head.lexeme & " : " & l^.head.attrib.totext()&"\n");
      END;
      l := l^.tail;
    END;
    Wr.Flush(Stdio.stdout);

    (* we know it works now, so every time we re-parse it, but keep the *)
    (* old symbol table the same, just give it a new stack *)
    self.formula.stack := NEW(Eval.Stack); 
    (* and of course it will eat the expression as it parses *)
    self.formula.formula := expr;
(*    TRY *)
      self.formula.parse(); 
      self.graphdata.data.values^[j*self.formula.varcount] := Scan.LongReal(self.formula.eval().value.totext());
(*    EXCEPT      
      ELSE RAISE Eval.EvalError("Something Bad (TM) has happened");
    END;*)
  END;
END FormEval;

PROCEDURE Evaluate(self :T; x :REF ARRAY OF Data.Element; useformula :BOOLEAN := FALSE) :Approximation.Result RAISES {AnalOpt.Error} =
VAR
  expr :TEXT;
  val := NEW(REF Data.Element);
  result :Approximation.Result;
BEGIN
  IF useformula THEN
    expr := FormsVBT.GetText(self,"formtext2");  
    IF self.formula = NIL OR Text.Equal(expr,"") THEN
      RAISE AnalOpt.Error("Formula not defined*or not used");
    END;
    self.formula.formula := expr;
    val^ := x[0];
    self.formula.stack := NEW(Eval.Stack);
    self.formula.symtable.assignattrib(self.formula.mappings[1],Value.Create(val));
    self.formula.parse();
    result.yval := Data.ScanEl(self.formula.eval().value.totext());
    result.error := 0.0d0;
    RETURN result;
  ELSE
    TRY
      RETURN self.approx.eval(x, self.graphdata.data);
    EXCEPT
      Approximation.Error(e) => RAISE AnalOpt.Error(e);
    END;
  END;
END Evaluate;

BEGIN
END Graph.



