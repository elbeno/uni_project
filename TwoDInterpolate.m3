MODULE TwoDInterpolate;

IMPORT Data, FormsVBT, Text, Graph, Approximation, Trestle, AnalOpt, Eval, Value, Fmt, TwoDCartesian, DisplayGraph, Axis, TrestleComm, Lex, FloatMode, Common;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented, TrestleComm.Failure *>

PROCEDURE Master(form :FormsVBT.T) RAISES {AnalOpt.Error}=
VAR
  choice, polychoice :TEXT;
  x :REF ARRAY OF Data.Element;
  result :Approximation.Result;
  i := 0;
  step, acc :Data.Element;
  newdata :REF ARRAY OF Data.Element;
  xcol, ycol, s, ord :INTEGER; 
  makenew :BOOLEAN;
  newgraph :Graph.T;
BEGIN
  (* This function gets called when we hit the interpolate button 
     for a 2D Cartesian graph. 
     Let's sort out which kind of interpolation we are doing,
     and for which value of X.
     NB We will always use the formula if it exists, no matter what
     interpolation method is specified. *)

  x := NEW(REF ARRAY OF Data.Element,1);
  choice := FormsVBT.GetChoice(form,"interpchoice1");

  makenew := FormsVBT.GetBoolean(form,"interpolmakegraph1");
  WITH f = NARROW(form, Graph.T) DO

    xcol := NARROW(NARROW(FormsVBT.GetGeneric(f.graph,"gengraph"),DisplayGraph.T).graphopts,TwoDCartesian.OptBlock)^.axes[Axis.T.Hor].datacolumn;
    ycol := NARROW(NARROW(FormsVBT.GetGeneric(f.graph,"gengraph"),DisplayGraph.T).graphopts,TwoDCartesian.OptBlock)^.axes[Axis.T.Ver].datacolumn;

    (* assign the correct interpolation routine *)
    TRY
      IF Text.Equal(choice,"lagrange1") THEN
        IF f.approx = NIL THEN
          f.approx := Approximation.Create("2dlagrange",f.graphdata.data,xcol,ycol);
        ELSIF NOT(Text.Equal(f.approx.desc,"lagrange1")) THEN
          f.approx := Approximation.Create("2dlagrange",f.graphdata.data,xcol,ycol);  
        END;

      ELSIF Text.Equal(choice,"newtondivdiff1") THEN
        IF f.approx = NIL THEN
          f.approx := Approximation.Create("2dnewtondivdiff",f.graphdata.data,xcol,ycol);
        ELSIF NOT(Text.Equal(f.approx.desc,"newtondivdiff1")) THEN
          f.approx := Approximation.Create("2dnewtondivdiff",f.graphdata.data,xcol,ycol);  
        END;

      ELSIF Text.Equal(choice,"rational1") THEN
        IF f.approx = NIL THEN
          f.approx := Approximation.Create("2drational",f.graphdata.data,xcol,ycol);
        ELSIF NOT(Text.Equal(f.approx.desc,"rational1")) THEN
          f.approx := Approximation.Create("2drational",f.graphdata.data,xcol,ycol); 
        END;

      ELSIF Text.Equal(choice,"cubicspline1") THEN
        IF f.approx = NIL THEN
          f.approx := Approximation.Create("2dcspline",f.graphdata.data,xcol,ycol);
        ELSIF NOT(Text.Equal(f.approx.desc,"cubicspline1")) THEN
          f.approx := Approximation.Create("2dcspline",f.graphdata.data,xcol,ycol); 
        END;
      ELSIF Text.Equal(choice,"leastsquares1") THEN
        ord := FormsVBT.GetInteger(f,"lsorder1") + 1;
        polychoice := FormsVBT.GetChoice(f,"basisfunctions1");
        TRY
          acc := Data.ScanEl(FormsVBT.GetText(f,"lsaccuracy1"));
        EXCEPT
          | Lex.Error, FloatMode.Trap =>
            RAISE AnalOpt.Error("Missing Accuracy");
        END;
        IF f.approx = NIL THEN
          f.approx := Approximation.Create("2dleastsquares",f.graphdata.data,xcol,ycol,ord,acc, polychoice);
        ELSIF NOT(Text.Equal(f.approx.desc,"leastsquares1")) OR f.approx.order # ord THEN
          f.approx := Approximation.Create("2dleastsquares",f.graphdata.data,xcol,ycol,ord,acc, polychoice);
        END;
      END;

    EXCEPT
      | Approximation.Error(e) =>
        RAISE AnalOpt.Error(e);
    END;


    IF makenew THEN
      s := FormsVBT.GetInteger(f,"interpsample1");
      newgraph := Graph.New();
      newgraph.graphdata.data := newgraph.graphdata.data.init(2,s+1);
      TRY
        step := (f.graphdata.data.max(xcol) - f.graphdata.data.min(xcol))/FLOAT(s,Data.Element);
        x[0] := f.graphdata.data.min(xcol);
      EXCEPT
        | Data.NoSuchField =>
          EVAL Common.NumberofPanels.dec();
          RAISE AnalOpt.Error("Something BAD (TM)*has happened"); 
      END;
      FOR i := 0 TO s DO
        IF i = s THEN
          TRY
            x[0] := f.graphdata.data.max(xcol);
          EXCEPT
            | Data.NoSuchField =>
              EVAL Common.NumberofPanels.dec();
              RAISE AnalOpt.Error("Something BAD (TM)*has happened"); 
          END;
        END;
        newgraph.graphdata.data.values[i*2 + 1] := x[0]; 
        TRY
          IF Text.Equal(choice,"useformula1") THEN
            result := f.eval(x,TRUE);
          ELSE
            result := f.eval(x);
          END;        
        EXCEPT
          | AnalOpt.Error(e) =>
            EVAL Common.NumberofPanels.dec();
            RAISE AnalOpt.Error(e);
        END;
        newgraph.graphdata.data.values[i*2] := result.yval;
        x[0] := x[0] + step;
      END;
      Trestle.Install(newgraph, "GraphDraw Control Panel " & Fmt.Int(newgraph.number));

    ELSE
      TRY
        x[0] := Data.ScanEl(FormsVBT.GetText(form,"interpxvalue1"));
      EXCEPT
        | Lex.Error, FloatMode.Trap =>
          RAISE AnalOpt.Error("Missing Interpolation Value");
      END;
      (* check that we haven't already got the element *)
      FOR i := 0 TO f.graphdata.data.records-1 DO
        IF f.graphdata.data.values[i*f.graphdata.data.fields + xcol] = x[0] THEN
          RAISE AnalOpt.Error("Interpolation Value*already exists");
        END;
      END;

      newdata := NEW(REF ARRAY OF Data.Element, f.graphdata.data.fields * (f.graphdata.data.records + 1));
      TRY
        result := f.eval(x);
        IF Text.Equal(choice,"useformula1" ) THEN
          result := f.eval(x,TRUE);
        END;
      EXCEPT
        | Eval.LexError(i) =>
          RAISE AnalOpt.Error("Lexical Error*at line " & Fmt.Int(i));
        | Eval.SyntaxError(i) =>
          RAISE AnalOpt.Error("Syntax Error*at line " & Fmt.Int(i));
        | Eval.EvalError(e) =>
          RAISE AnalOpt.Error(e);
        | Value.Silly(e) =>
          RAISE AnalOpt.Error(e);
      END;
   (* now we've done the interpolation, insert the new value into the Data.T *)
      (* find the place to insert *)
      WHILE i < f.graphdata.data.records DO
        IF x[0] < f.graphdata.data.values[i*f.graphdata.data.fields + xcol]  THEN
          EXIT;
        END;
        newdata[i*f.graphdata.data.fields] := f.graphdata.data.values[i*f.graphdata.data.fields + ycol];
        newdata[i*f.graphdata.data.fields + 1] := f.graphdata.data.values[i*f.graphdata.data.fields + xcol];
        INC(i);
      END;
      (* insert new value *)
      newdata[i*f.graphdata.data.fields] := result.yval;
      newdata[i*f.graphdata.data.fields + 1] := x[0];    
      (* do the rest *)
      FOR j := i TO f.graphdata.data.records - 1 DO       
        newdata[(j+1) * f.graphdata.data.fields + 1] := f.graphdata.data.values[j*f.graphdata.data.fields + xcol];
        newdata[(j+1) * f.graphdata.data.fields] := f.graphdata.data.values[j*f.graphdata.data.fields + ycol];
      END;

      IF f.graphdata.datavisible THEN  
        f.graphdata.datavisible := FALSE;
        Trestle.Delete(f.graphdata);
      END;
      f.graphdata.data.values := newdata;
      INC(f.graphdata.data.records);
    END;
  END;
END Master;

BEGIN
END TwoDInterpolate.


