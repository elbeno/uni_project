MODULE TwoDDifferentiate;

IMPORT Graph, Data, FormsVBT, Text, Fmt, AnalOpt, Approximation, Trestle, TwoDCartesian, DisplayGraph, Axis, Lex, FloatMode, Common, TrestleComm;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented, TrestleComm.Failure *>

PROCEDURE Master(form :FormsVBT.T) RAISES {AnalOpt.Error}=
VAR
  method :TEXT;
  a, result, step, h :Data.Element;
  makenew :BOOLEAN;
  s, xcol, ycol, iter :INTEGER := 0;
  newgraph: Graph.T;
BEGIN
  (* This function gets called when we hit the differentiate button
     for a 2D Cartesian Graph. *)
  method := FormsVBT.GetChoice(form,"diffmethod1");
  makenew := FormsVBT.GetBoolean(form,"diffmakegraph1");
  s := FormsVBT.GetInteger(form,"diffsample1");
  TRY
    a := Data.ScanEl(FormsVBT.GetText(form,"diffxvalue1"));
  EXCEPT
    | Lex.Error, FloatMode.Trap =>
      IF NOT(makenew) THEN
        RAISE AnalOpt.Error("Missing Abscissa Value");
      END;
  END;

  WITH f = NARROW(form, Graph.T) DO

    iter := FormsVBT.GetInteger(f,"richarditer1");
    TRY
      h := Data.ScanEl(FormsVBT.GetText(f,"diffhvalue1"));
    EXCEPT
      | Lex.Error, FloatMode.Trap =>
        RAISE AnalOpt.Error("Missing Parameter");
    END;

    xcol := NARROW(NARROW(FormsVBT.GetGeneric(f.graph,"gengraph"),DisplayGraph.T).graphopts,TwoDCartesian.OptBlock)^.axes[Axis.T.Hor].datacolumn;
    ycol := NARROW(NARROW(FormsVBT.GetGeneric(f.graph,"gengraph"),DisplayGraph.T).graphopts,TwoDCartesian.OptBlock)^.axes[Axis.T.Ver].datacolumn;

    IF f.approx = NIL THEN
      TRY
        f.approx := Approximation.Create("2dlagrange",f.graphdata.data,xcol,ycol);
      EXCEPT
        | Approximation.Error(e) =>
          RAISE AnalOpt.Error(e);
      END;
    END;
    IF makenew THEN
      newgraph := Graph.New();
      newgraph.graphdata.data := newgraph.graphdata.data.init(2,s+1);
      TRY
        step := (f.graphdata.data.max(xcol) - f.graphdata.data.min(xcol))/FLOAT(s,Data.Element);
        a := f.graphdata.data.min(xcol);
      EXCEPT
        | Data.NoSuchField =>
          RAISE AnalOpt.Error("Something BAD (TM)*has happened");
      END;
      FOR i := 0 TO s DO
        IF i = s THEN
          TRY
            a := f.graphdata.data.max(xcol);
          EXCEPT
            | Data.NoSuchField =>
              RAISE AnalOpt.Error("Something BAD (TM)*has happened");
          END;
        END;
        newgraph.graphdata.data.values[i*2 + 1] := a; 
        TRY
          IF Text.Equal(method,"forward1") THEN
            result := ForwardDiff(f,a,h);
          ELSIF Text.Equal(method,"backward1") THEN
            result := BackwardDiff(f,a,h);
          ELSIF Text.Equal(method,"central1") THEN
            result := CentralDiff(f,a,h);
          ELSIF Text.Equal(method,"richardson1") THEN
            result := Richardson(f,a,h,iter);
          END;        
        EXCEPT
          | AnalOpt.Error(e) =>
            EVAL Common.NumberofPanels.dec();
            RAISE AnalOpt.Error(e);
        END;
        newgraph.graphdata.data.values[i*2] := result;
        a := a + step;
      END;
      Trestle.Install(newgraph, "GraphDraw Control Panel " & Fmt.Int(newgraph.number));
    ELSE
      IF Text.Equal(method,"forward1") THEN
        result := ForwardDiff(f,a,h);
      ELSIF Text.Equal(method,"backward1") THEN
        result := BackwardDiff(f,a,h);
      ELSIF Text.Equal(method,"central1") THEN
        result := CentralDiff(f,a,h);
      ELSIF Text.Equal(method,"richardson1") THEN
        result := Richardson(f,a,h,iter);
      END;
      RAISE AnalOpt.Error("The answer is*" & Data.FmtEl(result));     
    END;
  END;
END Master;

(* NAIVETY ALERT! These routines have been written in the obvious naive way! *)

PROCEDURE ForwardDiff(z :Graph.T; a,h :Data.Element): Data.Element RAISES {AnalOpt.Error} =
VAR
  r := NEW(REF ARRAY OF Data.Element,1);
  result :Data.Element;
BEGIN
  (* calculate the derivative using the forward difference formula
     f'(a) = (f(a+h) - f(a))/h  (has error O(h))  *)

  r[0] := a+h;
  result := z.eval(r).yval;
  r[0] := a;
  result := result - z.eval(r).yval;
  result := result/h;
  RETURN result;
END ForwardDiff;

PROCEDURE BackwardDiff(z :Graph.T; a,h :Data.Element): Data.Element RAISES {AnalOpt.Error} =
VAR
  r := NEW(REF ARRAY OF Data.Element,1);
  result :Data.Element;
BEGIN
  (* calculate the derivative using the backward difference formula
     f'(a) = (f(a) - f(a-h))/h  (has error O(h)) *)

  r[0] := a-h;
  result := z.eval(r).yval;
  r[0] := a;
  result := z.eval(r).yval - result;
  result := result/h;
  RETURN result;
END BackwardDiff;

PROCEDURE CentralDiff(z :Graph.T; a,h :Data.Element): Data.Element RAISES {AnalOpt.Error} =
VAR
  r := NEW(REF ARRAY OF Data.Element,1);
  result :Data.Element;
BEGIN
  (* calculate the derivative using the central difference formula
     f'(a) = (f(a+h) - f(a-h))/2h (has error O(h^2)) *)

  r[0] := a+h;
  result := z.eval(r).yval/(h*2.0d0);
  r[0] := a-h;
  result := result - z.eval(r).yval/(h*2.0d0);
  RETURN result;
END CentralDiff;

PROCEDURE Richardson(z :Graph.T; a,h :Data.Element; refine :INTEGER): Data.Element RAISES {AnalOpt.Error} =
VAR
  r, tmp1, tmp2 :Data.Element;
  d :INTEGER := 1;
BEGIN
  (* Do Richardson's extrapolation using central difference
     see Numerical Analysis, Burden & Faires, chapter 4 for details *)

  IF refine = 1 THEN
    r := CentralDiff(z,a,h);
  ELSE
    tmp1 := Richardson(z,a,h/2.0d0,refine-1);
    tmp2 := Richardson(z,a,h,refine-1);
    FOR j := 1 TO refine-1 DO
      d := d * 4;
    END;
    d := d-1;
    r := tmp1 + (tmp1-tmp2)/FLOAT(d,Data.Element);
  END;
  RETURN r;
END Richardson;

BEGIN
END TwoDDifferentiate.

