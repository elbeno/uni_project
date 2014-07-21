MODULE TwoDRootFind;

IMPORT Data, FormsVBT, Text, Graph, Approximation, AnalOpt, TwoDCartesian, DisplayGraph, Axis, Lex, FloatMode, Common;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

PROCEDURE Master(form :FormsVBT.T) RAISES {AnalOpt.Error}=
VAR
  choice :TEXT;
  xcol, ycol, iter :INTEGER; 
  a, b, fractacc, result :Data.Element;
BEGIN
  (* This function gets called when we hit the root finding button 
     for a 2D Cartesian graph. 
     Let's sort out which kind of root finding we are doing,
     and for which value of X.
     NB We will always use the formula if it exists, no matter what
     interpolation method is specified. *)

  choice := FormsVBT.GetChoice(form,"rootmethod1");
  WITH f = NARROW(form, Graph.T) DO

    xcol := NARROW(NARROW(FormsVBT.GetGeneric(f.graph,"gengraph"),DisplayGraph.T).graphopts,TwoDCartesian.OptBlock)^.axes[Axis.T.Hor].datacolumn;
    ycol := NARROW(NARROW(FormsVBT.GetGeneric(f.graph,"gengraph"),DisplayGraph.T).graphopts,TwoDCartesian.OptBlock)^.axes[Axis.T.Ver].datacolumn;

    iter := FormsVBT.GetInteger(f,"rootiter1");

    TRY
      a := Data.ScanEl(FormsVBT.GetText(f,"rootx1value1"));
      b := Data.ScanEl(FormsVBT.GetText(f,"rootx2value1"));
      fractacc := Data.ScanEl(FormsVBT.GetText(f,"rootaccuracy1"));
    EXCEPT
      | Lex.Error, FloatMode.Trap =>
        RAISE AnalOpt.Error("Missing Value");
    END;

    (* if no approximation routine, use lagrange *)
    IF f.approx = NIL THEN
      TRY
        f.approx := Approximation.Create("2dlagrange",f.graphdata.data,xcol,ycol);
      EXCEPT
        | Approximation.Error(e) =>
          RAISE AnalOpt.Error(e);
      END;
    END;

    (* do the correct root finding routine *)

    IF Text.Equal(choice,"bisection1") THEN
      result := Bisection(f,a,b,fractacc,iter);
    ELSIF Text.Equal(choice,"newtonraphson1") THEN
      result := NewtonRaphson(f,a,b,fractacc,iter);
    ELSIF Text.Equal(choice,"secant1") THEN
      result := Secant(f,a,b,fractacc,iter);
    ELSIF Text.Equal(choice,"falsepos1") THEN
      result := FalsePos(f,a,b,fractacc,iter);
    ELSIF Text.Equal(choice,"brent1") THEN
      result := Brent(f,a,b,fractacc,iter);
    END;
    RAISE AnalOpt.Error("Root found at*" & Data.FmtEl(result));
  END;
END Master;

PROCEDURE Bisection(z :Graph.T; a, b, acc :Data.Element; maxiter :INTEGER): Data.Element RAISES {AnalOpt.Error} =
VAR
  x1 := NEW(REF ARRAY OF Data.Element,1);
  x2 := NEW(REF ARRAY OF Data.Element,1);
  dx, f, fmid, r :Data.Element;
BEGIN
  (* Bisection method. It can't fail!
     See Numerical Recipes chapter 9 for details *)

  x1[0] := a;
  x2[0] := b;
  f := z.eval(x1).yval;
  fmid := z.eval(x2).yval;
  IF f*fmid >= 0.0d0 THEN
    RAISE AnalOpt.Error("Can't find root in*this interval"); 
  END;
  IF f < 0.0d0 THEN
    dx := x2[0]-x1[0];          (* orient the search so that *)
    r := x1[0];                 (* f > 0 lies at x + dx *)
  ELSE
    dx := x1[0]-x2[0];
    r := x2[0];
  END;
  FOR j := 1 TO maxiter DO
    dx := dx * 0.5d0;           (* bisection loop *)
    x1[0] := r + dx;
    fmid := z.eval(x1).yval;
    IF fmid <= 0.0d0 THEN
      r := x1[0];
    END;
    IF ABS(dx) < acc OR fmid = 0.0d0 THEN
      RETURN r;
    END;
  END;
  RAISE AnalOpt.Error("Too many iterations*of Bisection Method");
END Bisection;

PROCEDURE FalsePos(z :Graph.T; a,b,acc :Data.Element; maxiter :INTEGER): Data.Element RAISES {AnalOpt.Error} =
VAR
  x1 := NEW(REF ARRAY OF Data.Element,1);
  x2 := NEW(REF ARRAY OF Data.Element,1);
  fl,fh,xl,xh,swap,dx,del,f,r :Data.Element;
BEGIN
  (* False Position method
     see Numerical Recipes chapter 9 for details *)

  x1[0] := a;
  x2[0] := b;
  fl := z.eval(x1).yval;          (* make sure there's a root *)
  fh := z.eval(x2).yval;          (* in the interval *)
  IF fl*fh > 0.0d0 THEN
    RAISE AnalOpt.Error("Can't find root in*this interval"); 
  END;
  IF fl < 0.0d0 THEN              (* identify limits so that *)
    xl := x1[0];                  (* xl corresponds to low side *)
    xh := x2[0];
  ELSE
    xl := x2[0];
    xh := x1[0];
    swap := fl;
    fl := fh;
    fh := swap;
  END;
  dx := xh-xl;
  FOR j := 1 TO maxiter DO        (* false position loop *)
    r := xl+dx*fl/(fl-fh);        (* inc wrt latest value *)
    x1[0] := r;
    f := z.eval(x1).yval;
    IF f < 0.0d0 THEN             (* replace appropriate limit *)
      del := xl - r;
      xl := r;
      fl := f;
    ELSE
      del := xh - r;
      xh := r;
      fh := f;
    END;
    dx := xh - xl;                       (* convergence *)
    IF ABS(del) < acc OR f = 0.0d0 THEN
      RETURN r;
    END;
  END;
  RAISE AnalOpt.Error("Too many iterations*of False Position Method");
END FalsePos;

PROCEDURE Secant(z :Graph.T; a,b,acc :Data.Element; maxiter :INTEGER): Data.Element RAISES {AnalOpt.Error} =
VAR
  x1 := NEW(REF ARRAY OF Data.Element,1);
  x2 := NEW(REF ARRAY OF Data.Element,1);
  fl,xl,swap,dx,f,r :Data.Element;
BEGIN
  (* Secant method
     see Numerical Recipes chapter 9 for details *)

  x1[0] := a;
  x2[0] := b;
  fl := z.eval(x1).yval;
  f := z.eval(x2).yval;
  IF ABS(fl) < ABS(f) THEN       (* pick the bound with the smaller *)
    r := x1[0];                  (* function value as the most recent guess *)
    xl := x2[0];
    swap := fl;
    fl := f;
    f := swap;
  ELSE
    xl := x1[0];
    r := x2[0];
  END;
  FOR j := 1 TO maxiter DO       (* secant loop *)
    dx := (xl-r)*f/(f-fl);       (* inc wrt latest value *)
    xl := r;
    fl := f;
    r := r + dx;
    x1[0] := r;
    f := z.eval(x1).yval;              (* convergence *)
    IF ABS(dx) < acc OR f = 0.0d0 THEN
      RETURN r;
    END;
  END;
  RAISE AnalOpt.Error("Too many iterations*of Secant Method");
END Secant;

PROCEDURE Brent(z :Graph.T; a,b,acc :Data.Element; maxiter :INTEGER): Data.Element RAISES {AnalOpt.Error} =
VAR
  x1 := NEW(REF ARRAY OF Data.Element,1);
  x2 := NEW(REF ARRAY OF Data.Element,1);
  eps,c,d,e,min1,min2,min3,fa,fb,fc,p,q,r,s,tol1,xm  :Data.Element;
BEGIN
  (* Van Wijngaarden - Dekker - Brent method
     see Numerical Recipes chapter 9 for details *)

  eps := Common.macheps;

  x1[0] := a;
  x2[0] := b;
  fa := z.eval(x1).yval;
  fb := z.eval(x2).yval;

  IF fa*fb > 0.0d0 THEN
    RAISE AnalOpt.Error("Can't find root in*this interval"); 
  END;
  fc := fb;

  FOR it := 1 TO maxiter DO
    IF fb * fc > 0.0d0 THEN
      c := a;                      (* rename a, b, c, and adjust *)
      fc := fa;                    (* bounding interval d *)
      d := b-a;
      e := d;
    END;
    IF ABS(fc) < ABS(fb) THEN
      a := b;
      b := c;
      c := a;
      fa := fb;
      fb := fc;
      fc := fa;
    END;
    tol1 := 2.0d0 * eps * ABS(b) + 0.5d0 * acc;    (* convergence check *)
    xm := 0.5d0 * (c-b);
    IF ABS(xm) <= tol1 OR fb = 0.0d0 THEN
      RETURN b;
    END;
    IF ABS(e) >= tol1 AND ABS(fa) > ABS(fb) THEN
      s := fb/fa;            (* attempt inverse quadratic interpolation *)
      IF a = c THEN
        p := 2.0d0 * xm * s;
        q := 1.0d0 - s;
      ELSE
        q := fa/fb;
        r := fb/fc;
        p := s * (2.0d0 * xm * q * (q-r) - (b-a) * (r-1.0d0));
        q := (q-1.0d0) * (r-1.0d0) * (s-1.0d0);
      END;
      IF p > 0.0d0 THEN     (* check whether in bounds *)
        q := -q;
      END;
      p := ABS(p);
      min1 := 3.0d0 * xm * q - ABS(tol1 * q);
      min2 := ABS(e*q);
      IF min1 < min2 THEN
        min3 := min1;
      ELSE
        min3 := min2;
      END;
      IF 2.0d0 * p < min3 THEN
        e := d;                      (* accept interpolation *)
        d := p/q;
      ELSE
        d := xm;                     (* interpolation failed, use bisection *)
        e := d;
      END;
    ELSE
      d := xm;          (* bounds decreasing too slowly, use bisection *)
      e := d;
    END;
    a := b;             (* last best guess *)
    fa := fb;
    IF ABS(d) > tol1 THEN    (* new trial root *)
      b := b + d;
    ELSE
      IF xm > 0.0d0 THEN
        b := b + ABS(tol1);
      ELSE
        b := b - ABS(tol1);
      END;
    END;
    x1[0] := b;
    fb := z.eval(x1).yval;
  END;
  RAISE AnalOpt.Error("Too many iterations*of Brent Method");
END Brent;

PROCEDURE NewtonRaphson(z :Graph.T; a,b,acc :Data.Element; maxiter :INTEGER): Data.Element RAISES {AnalOpt.Error} =
VAR
  x1 := NEW(REF ARRAY OF Data.Element,1);
  x2 := NEW(REF ARRAY OF Data.Element,1);
  df,dx,dxold,xh,xl,r,fh,fl,f,temp :Data.Element;
BEGIN
  (* a 'safer' combination of Newton-Raphson and Bisection
     which does not take the solution out of bounds
     see Numerical Recipes chapter 9 for details *)

  (* uses Richardson method (h1 = 0.16, 4 refinements) to find derivative *)

  x1[0] := a;
  x2[0] := b;
  fl := z.eval(x1).yval;
  fh := z.eval(x2).yval;
  
  IF fl * fh >= 0.0d0 THEN
    RAISE AnalOpt.Error("Can't find root in*this interval"); 
  END;
  IF fl < 0.0d0 THEN     (* orient search so that f(xl) < 0 *)
    xl := a;
    xh := b;
  ELSE
    xh := a;
    xl := b;
  END;
  r := 0.5d0 * (a + b);   (* initial root guess *)
  dxold := ABS(b-a);      (* step size before last *)
  dx := dxold;            (* last step size *)
  x1[0] := r;
  f := z.eval(x1).yval;
  df := Derivative(z,r,0.16d0,4);
  FOR j := 1 TO maxiter DO          (* loop over allowed iterations *)
    IF ((r-xh)*df-f)*((r-xl)*df-f) >= 0.0d0 OR ABS(2.0d0*f) > ABS(dxold*df) THEN
      dxold := dx;
      dx := 0.5d0 * (xh-xl);
      r := xl + dx;
      IF xl = r THEN
        RETURN r;
      END;
    ELSE
      dxold := dx;
      dx := f/df;
      temp := r;
      r := r-dx;
      IF temp = r THEN
        RETURN r;
      END;
    END;
    IF ABS(dx) < acc THEN         (* convergence *)
      RETURN r;
    END;
    x1[0] := r;
    f := z.eval(x1).yval;
    df := Derivative(z,r,0.16d0,4);
    IF f < 0.0d0 THEN
      xl := r;
    ELSE
      xh := r;
    END;
  END;
  RAISE AnalOpt.Error("Too many iterations*of Newton Raphson");
END NewtonRaphson;

PROCEDURE Derivative(z :Graph.T; a,h :Data.Element; refine :INTEGER): Data.Element RAISES {AnalOpt.Error} =
VAR
  result, tmp1, tmp2 :Data.Element;
  d :INTEGER := 1;
  x := NEW(REF ARRAY OF Data.Element,1);
BEGIN
  (* Do Richardson's extrapolation using central difference
     see Numerical Analysis, Burden & Faires, chapter 4 for details *)

  IF refine = 1 THEN
    x[0] := a+h;
    result := z.eval(x).yval/(h*2.0d0);
    x[0] := a-h;
    result := result - z.eval(x).yval/(h*2.0d0);
  ELSE
    tmp1 := Derivative(z,a,h/2.0d0,refine-1);
    tmp2 := Derivative(z,a,h,refine-1);
    FOR j := 1 TO refine-1 DO
      d := d * 4;
    END;
    d := d-1;
    result := tmp1 + (tmp1-tmp2)/FLOAT(d,Data.Element);
  END;
  RETURN result;
END Derivative;

BEGIN
END TwoDRootFind.






