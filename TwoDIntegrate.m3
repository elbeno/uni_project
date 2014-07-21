MODULE TwoDIntegrate;

IMPORT Graph, Data, FormsVBT, Text, AnalOpt, Approximation, Lex, FloatMode, TwoDCartesian, DisplayGraph, Axis, Trestle, Fmt, TrestleComm, Math, Common;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented, TrestleComm.Failure *>

VAR
  pointstoadd :INTEGER;
  resultsofar :Data.Element;

PROCEDURE Master(form :FormsVBT.T) RAISES {AnalOpt.Error}=
VAR
  method :TEXT;
  a, b, c, step, result:Data.Element;
  xcol, ycol, s:INTEGER;
  makenew :BOOLEAN;
  newgraph :Graph.T;
BEGIN
  (* This function gets called when we hit the integrate button
     for a 2D Cartesian Graph. *)

  method := FormsVBT.GetChoice(form,"integmethod1");
  makenew := FormsVBT.GetBoolean(form,"integmakegraph1");
  s := FormsVBT.GetInteger(form,"integsample1");

  WITH f = NARROW(form, Graph.T) DO

    xcol := NARROW(NARROW(FormsVBT.GetGeneric(f.graph,"gengraph"),DisplayGraph.T).graphopts,TwoDCartesian.OptBlock)^.axes[Axis.T.Hor].datacolumn;
    ycol := NARROW(NARROW(FormsVBT.GetGeneric(f.graph,"gengraph"),DisplayGraph.T).graphopts,TwoDCartesian.OptBlock)^.axes[Axis.T.Ver].datacolumn;

    IF f.approx = NIL THEN
      (* use lagrange as default approximation *)
      TRY
        f.approx:=Approximation.Create("2dlagrange",f.graphdata.data,xcol,ycol);
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
        newgraph.graphdata.data.values[i*2+1] := a; 
        IF a > 0.0d0 THEN
          c := a;
          b := 0.0d0;
        ELSE
          b := a;
          c := 0.0d0;
        END;
        TRY
          IF Text.Equal(method,"rectangle1") THEN
            result := Rectangle(f,b,c);
          ELSIF Text.Equal(method,"midpoint1") THEN
            result := MidPoint(f,b,c);
          ELSIF Text.Equal(method,"trapezium1") THEN
            result := Trapezium(f,b,c);
          ELSIF Text.Equal(method,"simpson1") THEN
            result := Simpson(f,b,c);
          ELSIF Text.Equal(method,"romberg1") THEN
            result := Romberg(f,b,c);
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
      TRY
        a := Data.ScanEl(FormsVBT.GetText(form, "lowerquadlimit1"));
        b := Data.ScanEl(FormsVBT.GetText(form, "upperquadlimit1"));
      EXCEPT
        ELSE
          RAISE AnalOpt.Error("Limit Missing");
      END;
      IF a > b THEN
        c := a;
        a := b;
        b := c;
      END;
      IF Text.Equal(method,"rectangle1") THEN
        result := Rectangle(f,a,b);
      ELSIF Text.Equal(method,"midpoint1") THEN
        result := MidPoint(f,a,b);
      ELSIF Text.Equal(method,"trapezium1") THEN
        result := Trapezium(f,a,b);
      ELSIF Text.Equal(method,"simpson1") THEN
        result := Simpson(f,a,b);
      ELSIF Text.Equal(method,"romberg1") THEN
        result := Romberg(f,a,b);
      ELSIF Text.Equal(Text.Sub(method,0,5),"gauss") THEN
        result := Gauss(f,a,b);
      END;
      RAISE AnalOpt.Error("The answer is*" & Data.FmtEl(result));
    END;
  END;
END Master;

PROCEDURE Trap(z :Graph.T; a, b :Data.Element; n :INTEGER) :Data.Element RAISES {AnalOpt.Error} =
VAR
  tnm,sum,del :Data.Element := 0.0d0;
  r, s := NEW(REF ARRAY OF Data.Element,1);
BEGIN
  (* this procedure progressively refines the extended trapezium rule
     with the call n=1 then it computes the first refinement, then call
     successively with 2,3 etc until required accuracy is met 
     see Numerical Recipes chapter 4 for details *)

  r[0] := a;
  s[0] := b;
  IF n = 1 THEN
    pointstoadd := 1;
    resultsofar := 0.5d0 * (b-a) * (z.eval(r).yval + z.eval(s).yval);
  ELSE
    tnm := FLOAT(pointstoadd, Data.Element);
    del := (b-a)/tnm;
    r[0] := a + 0.5d0 * del;
    FOR j := 1 TO pointstoadd DO
      sum := sum + z.eval(r).yval;
      r[0] := r[0] + del;
    END;
    pointstoadd := pointstoadd * 2;
    resultsofar := 0.5d0 * (resultsofar + (b-a) * sum/tnm);
  END;
  RETURN resultsofar;
END Trap;

PROCEDURE Rectangle(z :Graph.T; a, b:Data.Element): Data.Element RAISES {AnalOpt.Error} =
VAR
  r := NEW(REF ARRAY OF Data.Element,1);
  inc :Data.Element;
  result :Data.Element;
  step :INTEGER;
BEGIN
  step := FormsVBT.GetInteger(z,"quadstep1");
  r[0] := a;
  inc := (b-a)/FLOAT(step, Data.Element);
  FOR i := 1 TO step DO
    result := result + inc * z.eval(r).yval;
    r[0] := r[0] + inc; 
  END;
  RETURN result;
END Rectangle;

PROCEDURE MidPoint(z :Graph.T; a, b:Data.Element): Data.Element RAISES {AnalOpt.Error} =
VAR
  r := NEW(REF ARRAY OF Data.Element,1);
  inc :Data.Element;
  result :Data.Element;
  step :INTEGER;
BEGIN
  step := FormsVBT.GetInteger(z,"quadstep1");
  inc := (b-a)/FLOAT(step, Data.Element);
  r[0] := a + inc * 0.5d0;
  FOR i := 1 TO step DO
    result := result + inc * z.eval(r).yval;
    r[0] := r[0] + inc; 
  END;
  RETURN result;
END MidPoint;

PROCEDURE Trapezium(z :Graph.T; a, b:Data.Element): Data.Element RAISES {AnalOpt.Error} =
VAR
  maxiterations := 20;
  fractacc :Data.Element;
  oldresult :Data.Element;
  result :Data.Element;
BEGIN
  (* see Numerical Recipes chapter 4 for details *)

  TRY
    fractacc := Data.ScanEl(FormsVBT.GetText(z,"quadaccuracy1"));
  EXCEPT
    | Lex.Error, FloatMode.Trap =>
      RAISE AnalOpt.Error("Accuracy must be a*Real Number");
  END;
  oldresult := FLOAT(1.0e30, Data.Element);
  FOR j := 1 TO maxiterations DO
    result := Trap(z,a,b,j);
    IF ABS(result-oldresult) < fractacc * ABS(oldresult) THEN
      RETURN result;
    END;
    oldresult := result;
  END;
  RAISE AnalOpt.Error("Too many iterations*of Trapezium Rule");
END Trapezium;

PROCEDURE NaiveTrapezium(z :Graph.T; a, b:Data.Element): Data.Element RAISES {AnalOpt.Error} =
VAR
  r,s := NEW(REF ARRAY OF Data.Element,1);
  inc :Data.Element;
  result :Data.Element;
  step :INTEGER;
BEGIN
  step := FormsVBT.GetInteger(z,"quadstep1");
  r[0] := a;
  inc := (b-a)/FLOAT(step, Data.Element);
  FOR i := 1 TO step DO
    s[0] := r[0] + inc;
    result := result + 0.5d0 * inc * (z.eval(r).yval + z.eval(s).yval);
    r[0] := r[0] + inc; 
  END;
  RETURN result;
END NaiveTrapezium;

PROCEDURE Simpson(z :Graph.T; a, b:Data.Element): Data.Element RAISES {AnalOpt.Error} =
VAR
  maxiterations :INTEGER;
  fractacc :Data.Element;
  r1,r2,oldr1,oldr2 :Data.Element;
BEGIN
  (* see Numerical Recipes chapter 4 for details *)

  maxiterations := FormsVBT.GetInteger(z,"quaditer1");
  TRY
    fractacc := Data.ScanEl(FormsVBT.GetText(z,"quadaccuracy1"));
  EXCEPT
    | Lex.Error, FloatMode.Trap =>
      RAISE AnalOpt.Error("Accuracy must be a*Real Number");
  END;
  oldr1 := FLOAT(1.0e30, Data.Element);
  oldr2 := oldr1;
  FOR j := 1 TO maxiterations DO
    r2 := Trap(z,a,b,j);
    r1 := (4.0d0 * r2 - oldr2)/3.0d0;
    IF ABS(r1-oldr1) < fractacc * ABS(oldr1) THEN
      RETURN r1;
    END;
    oldr1 := r1;
    oldr2 := r2;
  END;
  RAISE AnalOpt.Error("Too many iterations*of Simpson Rule");
END Simpson;

PROCEDURE NaiveSimpson(z :Graph.T; a, b:Data.Element): Data.Element RAISES {AnalOpt.Error} =
VAR
  r,s,t := NEW(REF ARRAY OF Data.Element,1);
  inc :Data.Element;
  result :Data.Element;
  step :INTEGER;
BEGIN
  step := FormsVBT.GetInteger(z,"quadstep1");
  r[0] := a;
  inc := (b-a)/FLOAT(step, Data.Element);
  FOR i := 1 TO step DO
    t[0] := r[0] + inc;
    s[0] := r[0] + inc * 0.5d0;
    result := result + inc * 0.5d0 * (z.eval(r).yval/3.0d0 + z.eval(s).yval*4.0d0/3.0d0 + z.eval(t).yval/3.0d0);
    r[0] := t[0]; 
  END;
  RETURN result;
END NaiveSimpson;

PROCEDURE Romberg(z :Graph.T; a, b:Data.Element): Data.Element RAISES {AnalOpt.Error} =
VAR
  maxiterations, order, ns :INTEGER;
  fractacc :Data.Element;
  erresult, result, den, dif, mindif, w :Data.Element;
  s, h, c, d :REF ARRAY OF Data.Element;
BEGIN
  (* see Numerical Recipes chapter 4 for details *)

  maxiterations := FormsVBT.GetInteger(z,"quaditer1");
  TRY
    fractacc := Data.ScanEl(FormsVBT.GetText(z,"quadaccuracy1"));
  EXCEPT
    | Lex.Error, FloatMode.Trap =>
      RAISE AnalOpt.Error("Accuracy must be a*Real Number");
  END;  
  order := FormsVBT.GetInteger(z,"rombergorder1");
  s := NEW(REF ARRAY OF Data.Element,maxiterations + 1); (* store successive *)
  h := NEW(REF ARRAY OF Data.Element,maxiterations + 1); (* approximations *)
  h[1] := 1.0d0;                                         (* and step sizes *)
  FOR j := 1 TO maxiterations DO
    s[j] := Trap(z,a,b,j);
    IF j >= order THEN
      (* do Neville's algorithm *)
      ns := 1;
      mindif := ABS(h[j-order+1]); 
      c := NEW(REF ARRAY OF Data.Element, order);  
      d := NEW(REF ARRAY OF Data.Element, order);
      FOR i := 1 TO order DO    (* find the closest table entry, index ns *)
        dif := ABS(h[j-order+i]);
        IF dif < mindif THEN
          ns := i;
          mindif := dif;
        END;
        c[i-1] := s[j-order+i];  
        d[i-1] := c[i-1];   (* c's and d's initialised *)
      END;
      result := s[j-order+ns];  
      DEC(ns);   (* initial approximation to the limit *)
      FOR m := 1 TO order-1 DO
        FOR i := 1 TO order-m DO
          w := c[i] - d[i-1];
          den := h[j-order+i]-h[j-order+i+m];
          IF den = 0.0d0 THEN
            RAISE AnalOpt.Error("Error in*Romberg Routine");
                    (* occurs if 2 x-values identical *)
          END;
          den := w/den;
          d[i-1] := h[j-order+i+m] * den;   (* c's and d's updated *)
          c[i-1] := h[j-order+i] * den;
        END;
        IF 2*ns < order-m THEN
          erresult := c[ns];
        ELSE
          erresult := d[ns-1];
          DEC(ns);
        END;
        result := result + erresult;
      END;
      (* end of Neville's algorithm *)
      IF ABS(erresult) < fractacc * ABS(result) THEN
        RETURN result;
      END;
    END;
    s[j+1] := s[j];
    h[j+1] := 0.25d0 * h[j];   (* factor is 0.25 even though step size is *)
                               (* decreased by 0.5 => extrapolation to limit *)
                               (* of polynomial in h^2 rather than just h *)
  END;
  RAISE AnalOpt.Error("Too many iterations*of Romberg Rule");
END Romberg;

PROCEDURE Gauss(f :Graph.T; a, b:Data.Element): Data.Element RAISES {AnalOpt.Error} =
VAR
  p :INTEGER;
  w, x :REF ARRAY OF Data.Element;
  result, z :Data.Element;
  type :TEXT;
BEGIN
  (* see Elementary Numerical Analysis chapter 7 for details *)

  p := FormsVBT.GetInteger(f,"gaussorder1");
  x := NEW(REF ARRAY OF Data.Element, 1);
  type := FormsVBT.GetChoice(f,"integmethod1");

  IF Text.Equal(type,"gaussleg1") THEN
    w := Legendre(p); 
    CASE p OF
      | 1 =>
        x[0] := (w[0] * (b-a) + b+a)/2.0d0;
        result := f.eval(x).yval * w[1];
      | 2 =>
        x[0] := (w[0] * (b-a) + b+a)/2.0d0;
        z := f.eval(x).yval;
        x[0] := (w[2] * (b-a) + b+a)/2.0d0;
        z := z + f.eval(x).yval;
        result := z * w[1];
      | 3 =>
        x[0] := (w[0] * (b-a) + b+a)/2.0d0;
        z := f.eval(x).yval;
        x[0] := (w[4] * (b-a) + b+a)/2.0d0;
        z := z + f.eval(x).yval;
        result := z * w[1];
        x[0] := (w[2] * (b-a) + b+a)/2.0d0;
        z := f.eval(x).yval;
        result := result + z * w[3];
      | 4 =>
        x[0] := (w[0] * (b-a) + b+a)/2.0d0;
        z := f.eval(x).yval;
        x[0] := (w[6] * (b-a) + b+a)/2.0d0;
        z := z + f.eval(x).yval;
        result := z * w[1];
        x[0] := (w[2] * (b-a) + b+a)/2.0d0;
        z := f.eval(x).yval;
        x[0] := (w[4] * (b-a) + b+a)/2.0d0;
        z := z + f.eval(x).yval;
        result := result + z * w[3];
      | 5 =>
        x[0] := (w[0] * (b-a) + b+a)/2.0d0;
        z := f.eval(x).yval;
        x[0] := (w[8] * (b-a) + b+a)/2.0d0;
        z := z + f.eval(x).yval;
        result := z * w[1];
        x[0] := (w[2] * (b-a) + b+a)/2.0d0;
        z := f.eval(x).yval;
        x[0] := (w[6] * (b-a) + b+a)/2.0d0;
        z := z + f.eval(x).yval;
        result := result + z * w[3];
        x[0] := (w[4] * (b-a) + b+a)/2.0d0;
        z := f.eval(x).yval;
        result := result + z * w[5];
      | 6 =>
        x[0] := (w[0] * (b-a) + b+a)/2.0d0;
        z := f.eval(x).yval;
        x[0] := (w[10] * (b-a) + b+a)/2.0d0;
        z := z + f.eval(x).yval;
        result := z * w[1];
        x[0] := (w[2] * (b-a) + b+a)/2.0d0;
        z := f.eval(x).yval;
        x[0] := (w[8] * (b-a) + b+a)/2.0d0;
        z := z + f.eval(x).yval;
        result := result + z * w[3];
        x[0] := (w[4] * (b-a) + b+a)/2.0d0;
        z := f.eval(x).yval;
        x[0] := (w[6] * (b-a) + b+a)/2.0d0;
        z := z + f.eval(x).yval;
        result := result + z * w[5];
      ELSE
        RAISE AnalOpt.Error("Maximum Order is 6*for Gauss-Legendre");
    END;
  ELSIF Text.Equal(type,"gausscheb1") THEN
    w := Chebyshev(p);
    result := 0.0d0;
    FOR i := 0 TO p-1 DO
      x[0] := (w[2*i] * (b-a) + b+a)/2.0d0;
      result := result + f.eval(x).yval * w[2*i+1];
    END;
    result := result * FLOAT(Math.Pi,Data.Element)/FLOAT(p,Data.Element);
  END;
  result := result * (b-a)/2.0d0;
  RETURN result;
END Gauss;

PROCEDURE Chebyshev(np :INTEGER): REF ARRAY OF Data.Element =
VAR
  a :REF ARRAY OF Data.Element;
BEGIN
  (* Returns the points for Gauss-Chebyshev integration.
     Also return the weights (odd elements) *)
  a := NEW(REF ARRAY OF Data.Element, np*2);
  FOR i := 0 TO np-1 DO
    a[2*i] := Math.cos(FLOAT((2*i + 1),Data.Element) * FLOAT(Math.Pi,Data.Element) / FLOAT((2 * np), Data.Element));
    a[2*i+1] := Math.sin(FLOAT((2*i + 1),Data.Element) * FLOAT(Math.Pi,Data.Element) / FLOAT((2 * np), Data.Element));
  END;
  RETURN a;
END Chebyshev;

PROCEDURE Legendre(np :INTEGER): REF ARRAY OF Data.Element =
VAR
  a :REF ARRAY OF Data.Element;
BEGIN
  (* Returns the weights and points (between -1 and 1) for
     Gauss-Legendre integration. Even numbered array elements
     are points, odd no.s are weights. Max np = 6 *)
  a := NEW(REF ARRAY OF Data.Element, np * 2);
  CASE np OF
    | 1 =>
      a[0] := 0.0d0;
      a[1] := 2.0d0;
    | 2 =>
      a[0] := 0.577350269189626d0;   (* root 1/3 *)
      a[1] := 1.0d0;
    | 3 =>
      a[4] := 0.774596669241483d0;   (* root 3/5 *)
      a[5] := 5.0d0/9.0d0;
      a[2] := 0.0d0;
      a[3] := 8.0d0/9.0d0; 
    | 4 =>
      a[6] := 0.861136311594053d0;   (* all these roots are *)
      a[7] := 0.347854845137454d0;   (* complicated to solve *)
      a[4] := 0.339981043584856d0;
      a[5] := 0.652145154862546d0;
    | 5 =>
      a[8] := 0.906179845938664d0;
      a[9] := 0.236926885056189d0;
      a[6] := 0.538469310105683d0;
      a[7] := 0.478628670499366d0;
      a[4] := 0.0d0;
      a[5] := 1408.0d0/2475.0d0;     
    | 6 =>
      a[10] := 0.932469514203152d0;
      a[11] := 0.171324492379170d0;
      a[8] := 0.661209386466265d0;
      a[9] := 0.360761573048139d0;
      a[6] := 0.238619186083197d0;
      a[7] := 0.467913934572691d0;
    ELSE
  END;
  FOR i := 1 TO np DIV 2 DO
    a[(i-1)*2] := -a[(np-i)*2];
    a[(i-1)*2+1] := a[(np-i)*2+1];
  END;
  RETURN a;
END Legendre;

BEGIN
END TwoDIntegrate.
