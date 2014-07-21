MODULE Approximation;

IMPORT Data, Text, Math;

TYPE
  TwoDCSpline = T OBJECT
                    a :REF ARRAY OF Data.Element;
                    b :REF ARRAY OF Data.Element;
                    c :REF ARRAY OF Data.Element;
                    d :REF ARRAY OF Data.Element;
                  OVERRIDES
                    eval := TwoDCSplineEval;
                  END;
  TwoDLag = T OBJECT
              OVERRIDES
                eval := TwoDLagrangeEval;
              END;
  TwoDRat = T OBJECT
              OVERRIDES
                eval := TwoDRationalEval;
              END;
  TwoDNewt = T OBJECT
                 poly :REF ARRAY OF Data.Element;
               OVERRIDES
                 eval := TwoDNewtonDivEval;
               END;
  TwoDLeastSq = T OBJECT
                    ortho :TEXT;
                    fitparams :REF ARRAY OF Data.Element;
                  OVERRIDES
                    eval := TwoDLeastSquaresEval;
                  END;

PROCEDURE Create(t: TEXT; z: Data.T; x,y: INTEGER; order :INTEGER := 0; fractacc :Data.Element := 0.0d0; orthfunctions :TEXT := "polybasis1"):T RAISES {Error} =
VAR
  self :T;
BEGIN
  IF Text.Equal(t,"2dlagrange") THEN
    self := TwoDLagrange(z);
  ELSIF Text.Equal(t, "2drational") THEN
    self := TwoDRational(z);
  ELSIF Text.Equal(t, "2dnewtondivdiff") THEN
    self := TwoDNewtonDiv(z,x,y);
  ELSIF Text.Equal(t, "2dcspline") THEN
    self := TwoDCubicSpline(z,x,y);
  ELSIF Text.Equal(t, "2dleastsquares") THEN
    self := TwoDLeastSquares(z,x,y,order,fractacc, orthfunctions); 
  ELSE
    RAISE Error("Don't know how*to do that!");
  END;
  self.xcol := x;
  self.ycol := y;
  RETURN self;
END Create;

PROCEDURE TwoDLagrangeEval(self :TwoDLag; a :REF ARRAY OF Data.Element; z :Data.T): Result RAISES {Error} =
VAR
  c, d :REF ARRAY OF Data.Element;
  i, ns, m :INTEGER := 1;
  r :Result;
  ho, hp, w, den, dif, dift :Data.Element;
  x := a[0];
BEGIN
  (* Lagrange Polynomial approximation by Neville's algorithm
     see Numerical Recipes chapter 3 for details *)

  dif := ABS(x - z.values[self.xcol]); 
         (* first x value is z.values[xcol] *)
  c := NEW(REF ARRAY OF Data.Element, z.records);  
  d := NEW(REF ARRAY OF Data.Element, z.records);
  WHILE i <= z.records DO    (* find the closest table entry, index ns *)
    dift := ABS(x - z.values[(i-1) * z.fields + self.xcol]);
    IF dift = 0.0d0 THEN
      r.yval := z.values[(i-1) * z.fields + self.ycol];
      r.error := 0.0d0;
      RETURN r;
    END;
    IF dift < dif THEN
      ns := i;
      dif := dift;
    END;
    c[i-1] := z.values[(i-1) * z.fields + self.ycol];  
    d[i-1] := c[i-1];   (* c's and d's initialised *)
    INC(i);
  END;
  r.yval := z.values[(ns-1) * z.fields + self.ycol];  
  DEC(ns);   (* initial approximation to y *)
  WHILE m < z.records DO
    i := 1;
    WHILE i <= z.records-m DO
      ho := z.values[(i-1) * z.fields + self.xcol] - x;
      hp := z.values[(i+m-1) * z.fields + self.xcol] - x;
      w := c[i] - d[i-1];
      den := w/(ho-hp);
      d[i-1] := hp * den;   (* c's and d's updated *)
      c[i-1] := ho * den;
      INC(i);
    END;
    IF 2*ns < z.records-m THEN
      r.error := c[ns];
    ELSE
      r.error := d[ns-1];
      DEC(ns);
    END;
    r.yval := r.yval + r.error;
    INC(m);
  END;
  RETURN r;
END TwoDLagrangeEval;

PROCEDURE TwoDRationalEval(self :TwoDRat; a :REF ARRAY OF Data.Element; z :Data.T): Result RAISES {Error} =
VAR
  c, d :REF ARRAY OF Data.Element;
  i, ns, m :INTEGER := 1;
  r :Result;
  w, t, hh, h, dd :Data.Element;
  x := a[0];
BEGIN
  (* Rational function (Pade) Approximation by Bulirsch-Stoer Algorithm
     see Numerical Recipes chapter 3 for details *)

(*  (* start off by checking we've not got the value already *)
  FOR j := 0 TO z.records-1 DO
    IF a[0] = z.values[j*z.fields+self.xcol] THEN
      r.yval := z.values[j*z.fields+self.ycol];
      r.error := 0.0d0;
      RETURN r;
    END;
  END;
*)
  c := NEW(REF ARRAY OF Data.Element, z.records);
  d := NEW(REF ARRAY OF Data.Element, z.records);
  hh := ABS(x - z.values[self.xcol]);  
                 (* first x-value is z.values[self.xcol] *)
  WHILE i <= z.records DO
    h := ABS(x - z.values[(i-1) * z.fields + self.xcol]);    
    IF h = 0.0d0 THEN
      r.yval := z.values[(i-1) * z.fields + self.ycol];
      r.error := 0.0d0;
      RETURN r;
    ELSIF h < hh THEN
      ns := i;
      hh := h;
    END;
    c[i-1] := z.values[(i-1) * z.fields + self.ycol];
    d[i-1] := c[i-1] + FLOAT(1.0e-25, Data.Element); 
           (* prevent zero-over-zero condition *)
    INC(i);
  END;
  r.yval := z.values[(ns-1) * z.fields + self.ycol];  
  DEC(ns);   (* initial approximation to y *)
  WHILE m < z.records DO
    i := 1;
    WHILE i <= z.records-m DO
      w := c[i] - d[i-1];
      h := z.values[(i+m-1) * z.fields + self.xcol] - x;  
                           (* never zero - tested earlier *)
      t := (z.values[(i-1) * z.fields + self.xcol] - x) * d[i-1] / h;
      dd := t - c[i];
      IF dd = 0.0d0 THEN         (* function has a pole at x - rats! *)
        RAISE Error("Function has a Pole*at Interpolation Value");
      END;
      dd := w/dd;
      d[i-1] := c[i] * dd;
      c[i-1] := t * dd;
      INC(i);
    END;
    IF 2*ns < z.records-m THEN
      r.error := c[ns];
    ELSE
      r.error := d[ns-1];
      DEC(ns);
    END;
    r.yval := r.yval + r.error;
    INC(m);
  END;
  RETURN r; 
END TwoDRationalEval;

PROCEDURE TwoDNewtonDivEval(self :TwoDNewt; a :REF ARRAY OF Data.Element; z :Data.T): Result RAISES {Error} =
VAR
  r :Result;
BEGIN

  (* start off by checking we've not got the value already *)
  FOR j := 0 TO z.records-1 DO
    IF a[0] = z.values[j*z.fields+self.xcol] THEN
      r.yval := z.values[j*z.fields+self.ycol];
      r.error := 0.0d0;
      RETURN r;
    END;
  END;

  r.yval := self.poly[z.records * z.records - 1];
  FOR i := z.records-1 TO 1 BY -1 DO
    r.yval := r.yval * (a[0]-z.values[(i-1) * z.fields + self.xcol]);
    r.yval := r.yval + self.poly[(i-1) * z.records + (i-1)];
  END;
  RETURN r;
END TwoDNewtonDivEval;

PROCEDURE TwoDCSplineEval(self :TwoDCSpline; a :REF ARRAY OF Data.Element; z :Data.T): Result RAISES {Error} =
VAR
  r :Result;
  i := 1;
  p :Data.Element;
BEGIN
  (* start off by checking we've not got the value already *)
  FOR j := 0 TO z.records-1 DO
    IF a[0] = z.values[j*z.fields+self.xcol] THEN
      r.yval := z.values[j*z.fields+self.ycol];
      r.error := 0.0d0;
      RETURN r;
    END;
  END;

  (* check if the value is outside the possible range *)
  IF a[0] < z.values[0 + self.xcol] OR a[0] > z.values[(z.records-1) * z.fields + self.xcol] THEN
    RAISE Error("Interpolation Value*outside Interpolation Range");
  END;

  (* find the relevant coefficients to use *)
  WHILE i < z.records-1 DO
    IF z.values[i*z.fields + self.xcol] > a[0] THEN
      (* found i such that x[i-1] <= a[0] < x[i] *)
      EXIT;
    END;
    INC(i);
  END;
  DEC(i);
  p := a[0] - z.values[i * z.fields + self.xcol];

  (* Now S(x) = Si(x) = a(i) + b(i)p + c(i)p^2 + d(i)p^3 *)

  r.yval := self.d[i];
  r.yval := r.yval * p;
  r.yval := r.yval + self.c[i];
  r.yval := r.yval * p;
  r.yval := r.yval + self.b[i];
  r.yval := r.yval * p;
  r.yval := r.yval + self.a[i];

  RETURN r;
END TwoDCSplineEval;

PROCEDURE TwoDLeastSquaresEval(self :TwoDLeastSq; a :REF ARRAY OF Data.Element;<* UNUSED *> z :Data.T): Result RAISES {Error} =
VAR
  r :Result;
  w :REF ARRAY OF Data.Element;
BEGIN
  w := NEW(REF ARRAY OF Data.Element,self.order);
  Orthogonal(a[0],w,self.order,self.ortho);
  r.yval := 0.0d0;
  FOR i := 0 TO self.order-1 DO
    r.yval := r.yval + self.fitparams[i] * w[i];
  END;
  RETURN r;
END TwoDLeastSquaresEval;

PROCEDURE TwoDLagrange(<* UNUSED *> z :Data.T): TwoDLag =
VAR
  self := NEW(TwoDLag);
BEGIN
  self.desc := "lagrange1";
  RETURN self;
END TwoDLagrange;

PROCEDURE TwoDRational(<* UNUSED *> z :Data.T): TwoDRat =
VAR
  self := NEW(TwoDRat);
BEGIN
  self.desc := "rational1";
  RETURN self;
END TwoDRational;

PROCEDURE TwoDNewtonDiv(z :Data.T; xcol :INTEGER; ycol :INTEGER): TwoDNewt =
VAR
  self := NEW(TwoDNewt);
BEGIN
  (* Newton's Interpolatory Divided-Difference Formula
     see Numerical Analysis, Burden & Faires, chapter 3 for details *)

  self.desc := "newtondivdiff1";
  self.poly := NEW(REF ARRAY OF Data.Element, z.records * z.records);
  FOR i := 0 TO z.records-1 DO
    self.poly[i*z.records] := z.values[i*z.fields + ycol];
  END;
  FOR i := 1 TO z.records-1 DO
    FOR j := 1 TO i DO
      self.poly[i*z.records + j] := (self.poly[i*z.records+(j-1)] - self.poly[(i-1)*z.records + (j-1)]) / (z.values[i*z.fields + xcol] - z.values[(i-j) * z.fields + xcol]);
    END;
  END;
  RETURN self;
END TwoDNewtonDiv;

PROCEDURE TwoDCubicSpline(z :Data.T; xcol,ycol :INTEGER): TwoDCSpline =
VAR
  self := NEW(TwoDCSpline);
  h, alpha, l, mu, c, zz :REF ARRAY OF Data.Element;
BEGIN
  (* a natural cubic spline satisfying S"(x0) = S"(xN) = 0 
     see Numerical Analysis, Burden & Faires, chapter 3 for details *)

  self.desc := "cubicspline1";
  self.a := NEW(REF ARRAY OF Data.Element, z.records);
  self.b := NEW(REF ARRAY OF Data.Element, z.records);
  self.c := NEW(REF ARRAY OF Data.Element, z.records);
  self.d := NEW(REF ARRAY OF Data.Element, z.records);
  h := NEW(REF ARRAY OF Data.Element, z.records-1);
  alpha := NEW(REF ARRAY OF Data.Element, z.records-1);
  l := NEW(REF ARRAY OF Data.Element, z.records);
  mu := NEW(REF ARRAY OF Data.Element, z.records-1);
  c := NEW(REF ARRAY OF Data.Element, z.records);
  zz := NEW(REF ARRAY OF Data.Element, z.records);

  FOR i := 0 TO z.records-1 DO
    self.a[i] := z.values[i*z.fields + ycol];
  END;

  FOR i := 0 TO z.records-2 DO
    h[i] := z.values[(i+1)*z.fields + xcol] - z.values[i*z.fields + xcol];
  END;

  FOR i := 1 TO z.records-2 DO
    alpha[i] :=((self.a[i+1] - self.a[i]) * 3.0d0/h[i]) - ((self.a[i] - self.a[i-1]) * 3.0d0/h[i-1]);
  END;

  l[0] := 1.0d0;
  mu[0] := 0.0d0;
  zz[0] := 0.0d0;

  FOR i := 1 TO z.records-2 DO
    l[i] := 2.0d0 * (z.values[(i+1)*z.fields + xcol] - z.values[(i-1)*z.fields + xcol]) - h[i-1] * mu[i-1];
    mu[i] := h[i]/l[i];
    zz[i] := (alpha[i] - h[i-1]*zz[i-1])/l[i];
  END;

  l[z.records-1] := 1.0d0;
  zz[z.records-1] := 0.0d0;
  self.c[z.records-1] := 0.0d0;

  FOR j := z.records-2 TO 0 BY -1 DO
    self.c[j] := zz[j] - mu[j]*self.c[j+1];
    self.b[j] := (self.a[j+1] - self.a[j])/h[j] - h[j]*(self.c[j+1] + 2.0d0*self.c[j])/3.0d0;
    self.d[j] := (self.c[j+1] - self.c[j])/(3.0d0*h[j]);
  END;

  RETURN self;
END TwoDCubicSpline;

PROCEDURE TwoDLeastSquares(z :Data.T; xcol,ycol,o :INTEGER; fractacc :Data.Element; orthofuncs :TEXT): TwoDLeastSq RAISES {Error} =
VAR
  self := NEW(TwoDLeastSq);
  b :REF ARRAY OF Data.Element;
  u, v, w, afunc :REF ARRAY OF Data.Element;
  wmax :Data.Element;
BEGIN
  (* computes a least squares approximation of given order using Legendre
     polynomials as basis functions and using singular value decomposition
     see Numerical Recipes chapter 14 for details *)
  (* assume that the individual std deviations of data are all 1 *)
  
  self.desc := "leastsquares1";

  b := NEW(REF ARRAY OF Data.Element,z.records);
  u := NEW(REF ARRAY OF Data.Element,z.records * o);
  v := NEW(REF ARRAY OF Data.Element,o * o);
  w := NEW(REF ARRAY OF Data.Element,o);
  afunc := NEW(REF ARRAY OF Data.Element, o);

  self.order := o;
  self.ortho := orthofuncs;

  FOR i := 0 TO z.records-1 DO
    Orthogonal(z.values[i*z.fields + xcol],afunc,o,self.ortho);
    FOR j := 0 TO o-1 DO
      u[i*o+j] := afunc[j];
      (* set u[i,j] to jth Orthogonal Function of x[j] *)
    END;
    b[i] := z.values[i*z.fields+ycol];
  END;

  SingValDecomp(u,z.records,o,w,v);
  wmax := 0.0d0;
  FOR j := 0 TO o-1 DO
    IF w[j] > wmax THEN
      wmax := w[j];
    END;
  END;
  FOR j := 0 TO o-1 DO
    IF w[j] < wmax * fractacc THEN
      w[j] := 0.0d0;
    END;
  END; 
  self.fitparams := SingValBackSub(u,w,v,z.records,o,b);
  RETURN self;
END TwoDLeastSquares;

PROCEDURE Orthogonal(x :Data.Element; pl :REF ARRAY OF Data.Element; n :INTEGER; t :TEXT) =
VAR
  twox,f2,f1,d :Data.Element;
BEGIN
  IF Text.Equal(t,"polybasis1") THEN
    (* general polynomials *)
    pl[0] := 1.0d0;
    FOR j := 1 TO n-1 DO
      pl[j] := pl[j-1]*x;
    END;
  ELSIF Text.Equal(t,"legbasis1") THEN
    (* legendre polynomials *)
    pl[0] := 1.0d0;
    IF n > 1 THEN
      pl[1] := x;
    END;
    IF n > 2 THEN
      twox := 2.0d0*x;
      f2 := x;
      d := 1.0d0;
      FOR j := 2 TO n-1 DO
        f1 := d;
        d := d + 1.0d0;
        f2 := f2 + twox;
        pl[j] := (f2*pl[j-1] - f1*pl[j-2])/d;
      END;
    END;
  END;
END Orthogonal;

PROCEDURE SingValDecomp(a :REF ARRAY OF Data.Element; m,n :INTEGER; w,v :REF ARRAY OF Data.Element) RAISES {Error} =
VAR
  rv1 :REF ARRAY OF Data.Element;
  anorm := 0.0d0;
  scale := 0.0d0;
  g := 0.0d0;
  r,flag,l,nm :INTEGER;
  c,f,h,s,x,y,z :Data.Element;
BEGIN

  (* see Numerical Recipes chapter 2 for details of 
     Singular Value Decomposition *)

  IF m<n THEN
    RAISE Error("Must augment A*with extra rows");
  END;

  rv1 := NEW(REF ARRAY OF Data.Element,n);

  (* Householder reduction to bidiagonal form *)

  FOR i := 1 TO n DO
    l := i+1;
    rv1[i-1] := scale * g;
    g := 0.0d0;
    scale := 0.0d0;
    s := 0.0d0;
    IF i <= m THEN
      FOR k := i TO m DO
        scale := scale + ABS(a[(k-1)*n + (i-1)]);
      END;
      IF scale # 0.0d0 THEN
        FOR k := i TO m DO
          a[(k-1)*n + (i-1)] := a[(k-1)*n + (i-1)] / scale;
          s := s + a[(k-1)*n + (i-1)] * a[(k-1)*n + (i-1)];
        END;
        f := a[(i-1)*n + (i-1)];
        g := -Sign(Math.sqrt(s),f);
        h := f*g-s;
        a[(i-1)*n + (i-1)] := f-g;
        IF i # n THEN
          FOR j := l TO n DO
            s := 0.0d0;
            FOR k := i TO m DO
              s := s + a[(k-1)*n + (i-1)] * a[(k-1)*n + (j-1)];
            END;
            f := s/h;
            FOR k := i TO m DO
              a[(k-1)*n + (j-1)] := a[(k-1)*n + (j-1)] + f*a[(k-1)*n + (i-1)];
            END;
          END;
        END;
        FOR k := i TO m DO
          a[(k-1)*n + (i-1)] := a[(k-1)*n + (i-1)] * scale;
        END;
      END;
    END;
    w[i-1] := scale * g;
    g := 0.0d0;
    s := 0.0d0;
    scale := 0.0d0;
    IF i <= m AND i # n THEN
      FOR k := l TO n DO
        scale := scale + ABS(a[(i-1)*n + (k-1)]);
      END;
      IF scale # 0.0d0 THEN
        FOR k := l TO n DO
          a[(i-1)*n + (k-1)] := a[(i-1)*n + (k-1)] / scale;
          s := s + a[(i-1)*n + (k-1)] * a[(i-1)*n + (k-1)];
        END;
        f := a[(i-1)*n + (l-1)];
        g := -Sign(Math.sqrt(s),f);
        h := f*g-s;
        a[(i-1)*n + (l-1)] := f-g;
        FOR k := l TO n DO
          rv1[k-1] := a[(i-1)*n + (k-1)]/h;
        END;
        IF i # m THEN
          FOR j := l TO m DO
            s := 0.0d0;
            FOR k := l TO n DO
              s := s + a[(j-1)*n + (k-1)] * a[(i-1)*n + (k-1)];
            END;
            FOR k := l TO n DO
              a[(j-1)*n + (k-1)] := a[(j-1)*n + (k-1)] + s * rv1[k-1];
            END;
          END;
        END;
        FOR k := l TO n DO
          a[(i-1)*n + (k-1)] := a[(i-1)*n + (k-1)] * scale;
        END;
      END;
    END;
    anorm := Max(anorm,ABS(w[i-1]) + ABS(rv1[i-1]));
  END;

  (* accumulation of right-hand transformations *)

  FOR i := n TO 1 BY -1 DO
    IF i < n THEN
      IF g # 0.0d0 THEN
        FOR j := l TO n DO    (* double division to avoid underflow *)
          v[(j-1)*n + (i-1)] := (a[(i-1)*n + (j-1)]/a[(i-1)*n + (l-1)])/g;
        END;
        FOR j := l TO n DO
          s := 0.0d0;
          FOR k := l TO n DO
            s := s + a[(i-1)*n + (k-1)] * v[(k-1)*n + (j-1)];
          END;
          FOR k := l TO n DO
            v[(k-1)*n + (j-1)] := v[(k-1)*n + (j-1)] + s * v[(k-1)*n + (i-1)];
          END;
        END;
      END;
      FOR j := l TO n DO
        v[(i-1)*n + (j-1)] := 0.0d0;
        v[(j-1)*n + (i-1)] := 0.0d0;
      END;
    END;
    v[(i-1)*n + (i-1)] := 1.0d0;
    g := rv1[i-1];
    l := i;
  END;

  (* accumulation of left-hand transformations *)

  FOR i := n TO 1 BY -1 DO
    l := i+1;
    g := w[i-1];
    IF i < n THEN
      FOR j := l TO n DO
        a[(i-1)*n + (j-1)] := 0.0d0;
      END;
    END;
    IF g # 0.0d0 THEN
      g := 1.0d0/g;
      IF i # n THEN
        FOR j := l TO n DO
          s := 0.0d0;
          FOR k := l TO m DO
            s := s + a[(k-1)*n + (i-1)] * a[(k-1)*n + (j-1)];
          END;
          f := (s/a[(i-1)*n + (i-1)])*g;
          FOR k := i TO m DO
            a[(k-1)*n + (j-1)] := a[(k-1)*n + (j-1)] + f*a[(k-1)*n + (i-1)];
          END;
        END;
      END;
      FOR j := i TO m DO
        a[(j-1)*n + (i-1)] := a[(j-1)*n + (i-1)] * g;
      END;
    ELSE
      FOR j := i TO m DO
        a[(j-1)*n + (i-1)] := 0.0d0;
      END;
    END;
    a[(i-1)*n + (i-1)] := a[(i-1)*n + (i-1)] + 1.0d0;
  END;

  (* diagonalisation of the bidiagonal form *)

  FOR k := n TO 1 BY -1 DO         (* loop over singular values *)
    FOR its := 1 TO 30 DO          (* max no. of iterations *)
      flag := 1;
      l := k;
      WHILE l >= 1 DO              (* test for splitting *)
        nm := l-1;                 (* note rv1[0] is always 0 *)
        IF ABS(rv1[l-1]) + anorm = anorm THEN
          flag := 0;
          EXIT;
        END;
        IF ABS(w[nm-1]) + anorm = anorm THEN
          EXIT;
        END;
        l := l-1;        
      END;
      IF flag # 0 THEN
        c := 0.0d0;                (* cancellation of rv1[l-1] if l>1 *)
        s := 1.0d0;
        FOR i := l TO k DO
          f := s * rv1[i-1];
          rv1[i-1] := c * rv1[i-1];
          IF ABS(f) + anorm = anorm THEN
            EXIT;
          END;    
          g := w[i-1];
          h := PyThag(f,g);
          w[i-1] := h;
          h := 1.0d0/h;
          c := g*h;
          s := -f * h;
          FOR j := 1 TO m DO
            y := a[(j-1)*n + (nm-1)];
            z := a[(j-1)*n + (i-1)];
            a[(j-1)*n + (nm-1)] := y*c + z*s;
            a[(j-1)*n + (i-1)] := z*c - y*s;
          END;       
        END;
      END;
      z := w[k-1];
      IF l=k THEN                        (* convergence *)
        IF z < 0.0d0 THEN
          w[k-1] := -z;                  (* make singular value non-negative *)
          FOR j := 1 TO n DO
            v[(j-1)*n + (k-1)] := -v[(j-1)*n + (k-1)];
          END;
        END;
        EXIT;
      END;
      IF its = 30 THEN
        RAISE Error("No Convergence*in 30 iterations");
      END;
      x := w[l-1];             (* shift from bottom 2 * 2 minor *)
      nm := k-1;
      y := w[nm-1];
      g := rv1[nm-1];
      h := rv1[k-1];
      f := ((y-z)*(y+z)+(g-h)*(g+h))/(2.0d0*h*y);
      g := PyThag(f,1.0d0);
      f := ((x-z)*(x+z)+h*((y/(f+Sign(g,f)))-h))/x;
      c := 1.0d0;              (* next QR transformation *)
      s := 1.0d0;
      FOR j := l TO nm DO
        r := j+1;
        g := rv1[r-1];
        y := w[r-1];
        h := s*g;
        g := c*g;
        z := PyThag(f,h);
        rv1[j-1] := z;
        c := f/z;
        s := h/z;
        f := x*c + g*s;
        g := g*c - x*s;
        h := y*s;
        y := y*c;
        FOR jj := 1 TO n DO
          x := v[(jj-1)*n + (j-1)];
          z := v[(jj-1)*n + (r-1)];
          v[(jj-1)*n + (j-1)] := x*c + z*s;
          v[(jj-1)*n + (r-1)] := z*c - x*s;
        END;
        z := PyThag(f,h);
        w[j-1] :=z;            (* rotation can be arbitrary if z=0 *)
        IF z # 0.0d0 THEN
          z := 1.0d0/z;
          c := f*z;
          s := h*z;
        END;
        f := c*g + s*y;
        x := c*y - s*g;
        FOR jj := 1 TO m DO
          y := a[(jj-1)*n + (j-1)];
          z := a[(jj-1)*n + (r-1)];
          a[(jj-1)*n + (j-1)] := y*c + z*s;
          a[(jj-1)*n + (r-1)] := z*c - y*s;
        END;
      END;
      rv1[l-1] := 0.0d0;
      rv1[k-1] := f;
      w[k-1] := x;
    END;
  END;

END SingValDecomp;

PROCEDURE PyThag(a,b :Data.Element): Data.Element =
VAR
  at,bt,ct :Data.Element;
BEGIN
  at := ABS(a);
  bt := ABS(b);
  IF at > bt THEN
    ct := bt/at;
    RETURN at * Math.sqrt(1.0d0 + ct*ct);
  ELSE
    IF bt # 0.0d0 THEN
      ct := at/bt;
      RETURN bt * Math.sqrt(1.0d0 + ct*ct);
    ELSE
      RETURN 0.0d0;
    END;
  END;
END PyThag;

PROCEDURE Sign(a,b :Data.Element) :Data.Element =
BEGIN
  IF b > 0.0d0 THEN
    RETURN ABS(a);
  ELSE
    RETURN -ABS(a);
  END;
END Sign;

PROCEDURE Max(a,b :Data.Element): Data.Element =
BEGIN
  IF a > b THEN
    RETURN a;
  ELSE
    RETURN b;
  END;
END Max;

PROCEDURE SingValBackSub(u,w,v :REF ARRAY OF Data.Element; m,n :INTEGER; b :REF ARRAY OF Data.Element): REF ARRAY OF Data.Element =
VAR
  x, tmp :REF ARRAY OF Data.Element;
  s :Data.Element;
BEGIN
  (* solves A.x = B for a vector X
     see Numerical Recipes chapter 2 for details *)

  x := NEW(REF ARRAY OF Data.Element,n);
  tmp := NEW(REF ARRAY OF Data.Element,n);
  FOR j := 0 TO n-1 DO
    s := 0.0d0;
    IF w[j] # 0.0d0 THEN
      FOR i := 0 TO m-1 DO
        s := s + u[i*n + j] * b[i];
      END;
      s := s / w[j];
    END;
    tmp[j] := s;
  END;
  FOR j := 0 TO n-1 DO
    s := 0.0d0;
    FOR jj := 0 TO n-1 DO
      s := s + v[j*n + jj] * tmp[jj];
    END;
    x[j] := s;
  END;
  RETURN x;
END SingValBackSub;

BEGIN
END Approximation.









