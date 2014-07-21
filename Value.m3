MODULE Value;

IMPORT Fmt, Math, Data;

TYPE
  Integer = T OBJECT
                    v :INTEGER;
                  OVERRIDES
                    negate := Ineg;
                    add := Iadd;
                    subtract := Isub;
                    multiply := Imult;
                    divide := Idivide;
                    div := Idiv;
                    mod := Imod;
                    raiseto := Ipower;
                    exp := Iexp;
                    ln := Iln;
                    log := Ilog;
                    sqrt := Isqrt;
                    cos := Icos;
                    sin := Isin;
                    tan := Itan;
                    acos := Iacos;
                    asin := Iasin;
                    atan := Iatan;
                    factorial := Ifact;
                    abs := Iabs;
                    max := Imax;
                    min := Imin;
                    floor := Inop;
                    ceiling := Inop;
                    trunc := Inop;
                    round := Inop;
                    totext := Itext;
                  END;

  Real = T OBJECT
                 v :Data.Element;
               OVERRIDES
                 negate := Rneg;
                 add := Radd;
                 subtract := Rsub;
                 multiply := Rmult;
                 divide := Rdivide;
                 div := Rdiv;
                 mod := Rmod;
                 raiseto := Rpower;
                 exp := Rexp;
                 ln := Rln;
                 log := Rlog;
                 sqrt := Rsqrt;
                 cos := Rcos;
                 sin := Rsin;
                 tan := Rtan;
                 acos := Racos;
                 asin := Rasin;
                 atan := Ratan;
                 factorial := Rfact;
                 abs := Rabs;
                 max := Rmax;
                 min := Rmin;
                 floor := Rfloor;
                 ceiling := Rceiling;
                 trunc := Rtrunc;
                 round := Rround;
                 totext := Rtext;
               END;

  Text = T OBJECT
                 v :TEXT;
               OVERRIDES
                 totext := Ttext;
               END;

  Char = T OBJECT
                 v :CHAR;
               OVERRIDES
                 totext := Ctext;
               END;

PROCEDURE Create(raw :REFANY) :T =
VAR
  result :T;
BEGIN
  IF raw = NIL THEN
    result := NEW(Integer, v:=0);
  ELSE
    TYPECASE raw OF
      | REF INTEGER => result := NEW(Integer, v:=NARROW(raw, REF INTEGER)^);
      | REF Data.Element => result := NEW(Real, v:=NARROW(raw, REF Data.Element)^);
      | REF TEXT => result := NEW(Text, v:=NARROW(raw, REF TEXT)^);
      | REF CHAR => result := NEW(Char, v:=NARROW(raw, REF CHAR)^);
      ELSE
        (* this never happens but keeps the compiler happy *)
        result := NEW(Integer, v:=0);
    END;
  END;
  RETURN result;
END Create;

PROCEDURE Iadd(self :Integer; n: T): T RAISES {Silly} =
VAR
  result :T;
  r :Data.Element;
BEGIN
  TRY
    TYPECASE n OF
      | Integer(i) => r:=FLOAT(self.v, Data.Element) + FLOAT(i.v, Data.Element);
                      IF r <= FLOAT(LAST(INTEGER), Data.Element) AND r >= FLOAT(FIRST(INTEGER), Data.Element) THEN
                        result := NEW(Integer, v:=ROUND(r));
                      ELSE
                        result := NEW(Real, v:=r);
                      END;
      | Real(r) => result := NEW(Real, v:=FLOAT(self.v, Data.Element) + r.v);
      ELSE RETURN self;
    END;
  EXCEPT
    ELSE RAISE Silly("Exception in Iadd");
  END;
  RETURN result;
END Iadd;

PROCEDURE Isub(self :Integer; n: T): T RAISES {Silly} =
VAR
  result :T;
  r :Data.Element;
BEGIN
  TRY
    TYPECASE n OF
      | Integer(i) => r:=FLOAT(self.v, Data.Element) - FLOAT(i.v, Data.Element);
                      IF r <= FLOAT(LAST(INTEGER), Data.Element) AND r >= FLOAT(FIRST(INTEGER), Data.Element) THEN
                        result := NEW(Integer, v:=ROUND(r));
                      ELSE
                        result := NEW(Real, v:=r);
                      END;
      | Real(r) => result := NEW(Real, v:=FLOAT(self.v, Data.Element) - r.v);
      ELSE RETURN self;
    END;
  EXCEPT
    ELSE RAISE Silly("Exception in Isub");
  END;
  RETURN result;
END Isub;

PROCEDURE Imult(self :Integer; n: T): T RAISES {Silly} =
VAR
  result :T;
  r :Data.Element;
BEGIN
  TRY
    TYPECASE n OF
      | Integer(i) => r:=FLOAT(self.v, Data.Element) * FLOAT(i.v, Data.Element);
                      IF r <= FLOAT(LAST(INTEGER), Data.Element) AND r >= FLOAT(FIRST(INTEGER), Data.Element) THEN
                        result := NEW(Integer, v:=ROUND(r));
                      ELSE
                        result := NEW(Real, v:=r);
                      END;
      | Real(r) => result := NEW(Real, v:=FLOAT(self.v, Data.Element) * r.v);
      ELSE RETURN self;
    END;
  EXCEPT
    ELSE RAISE Silly("Exception in Imult");
  END;
  RETURN result;
END Imult;

PROCEDURE Idivide(self :Integer; n: T): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    TYPECASE n OF
      | Integer(i) => IF i.v = 0 THEN
                        RAISE Silly("Division by zero*not allowed");
                      ELSE
                        result := NEW(Real, v:=FLOAT(self.v, Data.Element) / FLOAT(i.v, Data.Element));
                      END;
      | Real(r) => IF r.v = 0.0d0 THEN
                     RAISE Silly("Division by zero*not allowed");
                   ELSE
                     result := NEW(Real, v:=FLOAT(self.v, Data.Element) / r.v);
                   END;
      ELSE RETURN self;
    END;
  EXCEPT
    | Silly(e) => RAISE Silly(e);
    ELSE RAISE Silly("Exception in Idivide");
  END;
  RETURN result;
END Idivide;

PROCEDURE Imod(self :Integer; n: T): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    TYPECASE n OF
      | Integer(i) => IF i.v = 0 THEN
                        RAISE Silly("MOD by zero*not allowed");
                      ELSE
                        result := NEW(Integer, v:=self.v MOD i.v);
                      END;
      | Real => RAISE Silly("MOD must take*Integer arguments");
      ELSE RETURN self;
    END;
  EXCEPT
    | Silly(e) => RAISE Silly(e);
    ELSE RAISE Silly("Exception in Imod");
  END;
  RETURN result;
END Imod;

PROCEDURE Idiv(self :Integer; n: T): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    TYPECASE n OF
      | Integer(i) => IF i.v = 0 THEN
                        RAISE Silly("DIV by zero*not allowed");
                      ELSE
                        result := NEW(Integer, v:=self.v DIV i.v);
                      END;
      | Real => RAISE Silly("DIV must take*Integer arguments");
      ELSE RETURN self;
    END;
  EXCEPT
    | Silly(e) => RAISE Silly(e);
    ELSE RAISE Silly("Exception in Idiv");
  END;
  RETURN result;
END Idiv;

PROCEDURE Ineg(self :Integer): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    result := NEW(Integer, v:=-self.v);
  EXCEPT
    ELSE RAISE Silly("Exception in Ineg");
  END;
  RETURN result;
END Ineg;

PROCEDURE Ipower(self :Integer; n: T): T RAISES {Silly} =
VAR
  result :T;
  r :Data.Element;
BEGIN
  TRY
    TYPECASE n OF
      | Integer(i) => r:=Math.pow(FLOAT(self.v, Data.Element), FLOAT(i.v, Data.Element));
                      IF i.v >= 0 AND r <= FLOAT(LAST(INTEGER), Data.Element) AND r >= FLOAT(FIRST(INTEGER), Data.Element) THEN
                        result := NEW(Integer, v:=ROUND(r));
                      ELSE
                        result := NEW(Real, v:=r);
                      END;
      | Real(r) => IF self.v <= 0 THEN
                     RAISE Silly("Can only raise positive numbers*to fractional powers");
                   ELSE
                     result := NEW(Real, v:=Math.pow(FLOAT(self.v, Data.Element),r.v));
                   END;
      ELSE RETURN self;
    END;
  EXCEPT
    | Silly(e) => RAISE Silly(e);
    ELSE RAISE Silly("Exception in Ipower");
  END;
  RETURN result;
END Ipower;

PROCEDURE Iexp(self :Integer): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    result := NEW(Real, v:=Math.exp(FLOAT(self.v, Data.Element)));
  EXCEPT
    ELSE RAISE Silly("Exception in Iexp");
  END;
  RETURN result;
END Iexp;

PROCEDURE Iln(self :Integer): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  IF self.v < 1 THEN 
    RAISE Silly("Bad domain of LN");
  END;
  TRY
    result := NEW(Real, v:=Math.log(FLOAT(self.v, Data.Element)));
  EXCEPT
    ELSE RAISE Silly("Exception in Iln");
  END;
  RETURN result;
END Iln;

PROCEDURE Ilog(self :Integer): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  IF self.v < 1 THEN
    RAISE Silly("Bad domain of LOG");
  END;
  TRY
    result := NEW(Real, v:=Math.log10(FLOAT(self.v, Data.Element)));
  EXCEPT
    ELSE RAISE Silly("Exception in Ilog");
  END;
  RETURN result;
END Ilog;

PROCEDURE Isqrt(self :Integer): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  IF self.v < 0 THEN
    RAISE Silly("Can't compute the square root*of a negative number");
  END;
  TRY
    result := NEW(Real, v:=Math.sqrt(FLOAT(self.v, Data.Element)));
  EXCEPT
    ELSE RAISE Silly("Exception in Isqrt");
  END;
  RETURN result;
END Isqrt;

PROCEDURE Icos(self :Integer): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    result := NEW(Real, v:=Math.cos(FLOAT(self.v, Data.Element)));
  EXCEPT
    ELSE RAISE Silly("Exception in Icos");
  END;
  RETURN result;
END Icos;

PROCEDURE Isin(self :Integer): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    result := NEW(Real, v:=Math.sin(FLOAT(self.v, Data.Element)));
  EXCEPT
    ELSE RAISE Silly("Exception in Isin");
  END;
  RETURN result;
END Isin;

PROCEDURE Itan(self :Integer): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    result := NEW(Real, v:=Math.tan(FLOAT(self.v, Data.Element)));
  EXCEPT
    ELSE RAISE Silly("Exception in Itan");
  END;
  RETURN result;
END Itan;

PROCEDURE Iacos(self :Integer): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    result := NEW(Real, v:=Math.acos(FLOAT(self.v, Data.Element)));
  EXCEPT
    ELSE RAISE Silly("Exception in Iacos");
  END;
  RETURN result;
END Iacos;

PROCEDURE Iasin(self :Integer): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    result := NEW(Real, v:=Math.asin(FLOAT(self.v, Data.Element)));
  EXCEPT
    ELSE RAISE Silly("Exception in Iasin");
  END;
  RETURN result;
END Iasin;

PROCEDURE Iatan(self :Integer): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    result := NEW(Real, v:=Math.atan(FLOAT(self.v, Data.Element)));
  EXCEPT
    ELSE RAISE Silly("Exception in Iatan");
  END;
  RETURN result;
END Iatan;

PROCEDURE Ifact(self :Integer): T RAISES {Silly} =
VAR
  i, f :INTEGER;
BEGIN
  f := 1;
  i := self.v;
  IF i = 0 OR i = 1 THEN
  RETURN NEW(Integer, v:=1);
  ELSIF i < 0 THEN
    RAISE Silly("Can't compute the factorial*of a negative number");
  ELSE
    WHILE i > 1 DO
      TRY
        f := f * i;
      EXCEPT
        ELSE RAISE Silly("Exception in Ifact");
      END;
      DEC(i);
    END;
  END;
  RETURN NEW(Integer, v:=f);
END Ifact;

PROCEDURE Iabs(self :Integer) :T =
BEGIN
  RETURN NEW(Integer, v := ABS(self.v));
END Iabs;

PROCEDURE Imax(self :Integer; n :T) :T =
BEGIN
  TYPECASE n OF
    | Integer(i) => RETURN NEW(Integer, v := MAX(self.v,i.v));
    | Real(r) => IF MAX(FLOAT(self.v, Data.Element), r.v) = r.v THEN
                   RETURN r;
                 ELSE
                   RETURN self;
                 END;
    ELSE RETURN self;
  END;
END Imax;

PROCEDURE Imin(self :Integer; n :T) :T =
BEGIN
  TYPECASE n OF
    | Integer(i) => RETURN NEW(Integer, v := MIN(self.v,i.v));
    | Real(r) => IF MIN(FLOAT(self.v, Data.Element), r.v) = r.v THEN
                   RETURN r;
                 ELSE
                   RETURN self;
                 END;
    ELSE RETURN self;
  END;
END Imin;

PROCEDURE Inop(self :Integer) :T =
BEGIN
  RETURN self;
END Inop;

PROCEDURE Itext(self :Integer): TEXT =
BEGIN
  RETURN Fmt.Int(self.v);
END Itext;

PROCEDURE Radd(self :Real; n: T): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    TYPECASE n OF
      | Integer(i) => result := NEW(Real, v:=self.v + FLOAT(i.v, Data.Element));
      | Real(r) => result := NEW(Real, v:=self.v + r.v);
      ELSE RETURN self;
    END;
  EXCEPT
    ELSE RAISE Silly("Exception in Radd");
  END;
  RETURN result;
END Radd;

PROCEDURE Rsub(self :Real; n: T): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    TYPECASE n OF
      | Integer(i) => result := NEW(Real, v:=self.v - FLOAT(i.v, Data.Element));
      | Real(r) => result := NEW(Real, v:=self.v - r.v);
      ELSE RETURN self;
    END;
  EXCEPT
    ELSE RAISE Silly("Exception in Rsub");
  END;
  RETURN result;
END Rsub;

PROCEDURE Rmult(self :Real; n: T): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    TYPECASE n OF
      | Integer(i) => result := NEW(Real, v:=self.v * FLOAT(i.v, Data.Element));
      | Real(r) => result := NEW(Real, v:=self.v * r.v);
      ELSE RETURN self;
    END;
  EXCEPT
    ELSE RAISE Silly("Exception in Rmult");
  END;
  RETURN result;
END Rmult;

PROCEDURE Rdivide(self :Real; n: T): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    TYPECASE n OF
      | Integer(i) => IF i.v = 0 THEN
                        RAISE Silly("Division by zero*not allowed");
                      ELSE
                        result := NEW(Real, v:=self.v / FLOAT(i.v, Data.Element));
                      END;
      | Real(r) => IF r.v = 0.0d0 THEN
                     RAISE Silly("Division by zero*not allowed");
                   ELSE
                     result := NEW(Real, v:=self.v / r.v);
                   END;
      ELSE RETURN self;
    END;
  EXCEPT
    | Silly(e) => RAISE Silly(e);
    ELSE RAISE Silly("Exception in Rdivide");
  END;
  RETURN result;
END Rdivide;

PROCEDURE Rmod(<* UNUSED *> self :Real; <* UNUSED *> n: T): T  RAISES {Silly} =
BEGIN
  RAISE Silly("MOD must take*Integer arguments");
END Rmod;

PROCEDURE Rdiv(<* UNUSED *> self :Real; <* UNUSED *> n: T): T RAISES {Silly} =
BEGIN
  RAISE Silly("DIV must take*Integer arguments");
END Rdiv;

PROCEDURE Rneg(self :Real): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    result := NEW(Real, v:=-self.v);
  EXCEPT
    ELSE RAISE Silly("Exception in Rneg");
  END;
  RETURN result;
END Rneg;

PROCEDURE Rpower(self :Real; n: T): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY 
    TYPECASE n OF
      | Integer(i) => result := NEW(Real, v:=Math.pow(self.v,FLOAT(i.v, Data.Element)));
      | Real(r) => IF self.v <= 0.0d0 THEN
                     RAISE Silly("Can only raise positive numbers*to fractional powers");
                   ELSE
                     result := NEW(Real, v:=Math.pow(self.v,r.v));
                   END;
      ELSE RETURN self;
    END;
  EXCEPT
    | Silly(e) => RAISE Silly(e);
    ELSE RAISE Silly("Exception in Rpower");
  END;
  RETURN result;
END Rpower;

PROCEDURE Rexp(self :Real): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    result := NEW(Real, v:=Math.exp(self.v));
  EXCEPT
    ELSE RAISE Silly("Exception in Rexp");
  END;
  RETURN result;
END Rexp;

PROCEDURE Rln(self :Real): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  IF self.v < 1.0d0 THEN 
    RAISE Silly("Bad domain of LN");
  END;
  TRY
    result := NEW(Real, v:=Math.log(self.v));
  EXCEPT
    ELSE RAISE Silly("Exception in Rln");
  END;
  RETURN result;
END Rln;

PROCEDURE Rlog(self :Real): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  IF self.v < 1.0d0 THEN 
    RAISE Silly("Bad domain of LOG");
  END;
  TRY
    result := NEW(Real, v:=Math.log10(self.v));
  EXCEPT
    ELSE RAISE Silly("Exception in Rlog");
  END;
  RETURN result;
END Rlog;

PROCEDURE Rsqrt(self :Real): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  IF self.v < 0.0d0 THEN 
    RAISE Silly("Can't compute the square root*of a negative number");
  END;
  TRY
    result := NEW(Real, v:=Math.sqrt(self.v));
  EXCEPT
    ELSE RAISE Silly("Exception in Rsqrt");
  END;
  RETURN result;
END Rsqrt;

PROCEDURE Rcos(self :Real): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    result := NEW(Real, v:=Math.cos(self.v));
  EXCEPT
    ELSE RAISE Silly("Exception in Rcos");
  END;
  RETURN result;
END Rcos;

PROCEDURE Rsin(self :Real): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    result := NEW(Real, v:=Math.sin(self.v));
  EXCEPT
    ELSE RAISE Silly("Exception in Rsin");
  END;
  RETURN result;
END Rsin;

PROCEDURE Rtan(self :Real): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    result := NEW(Real, v:=Math.tan(self.v));
  EXCEPT
    ELSE RAISE Silly("Exception in Rtan");
  END;
  RETURN result;
END Rtan;

PROCEDURE Racos(self :Real): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    result := NEW(Real, v:=Math.acos(self.v));
  EXCEPT
    ELSE RAISE Silly("Exception in Racos");
  END;
  RETURN result;
END Racos;

PROCEDURE Rasin(self :Real): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    result := NEW(Real, v:=Math.asin(self.v));
  EXCEPT
    ELSE RAISE Silly("Exception in Rasin");
  END;
  RETURN result;
END Rasin;

PROCEDURE Ratan(self :Real): T RAISES {Silly} =
VAR
  result :T;
BEGIN
  TRY
    result := NEW(Real, v:=Math.atan(self.v));
  EXCEPT
    ELSE RAISE Silly("Exception in Ratan");
  END;
  RETURN result;
END Ratan;

PROCEDURE Rfact(<* UNUSED *> self :Real): T  RAISES {Silly} =
BEGIN
  RAISE Silly("Can only compute*Integer factorials");
END Rfact;

PROCEDURE Rabs(self :Real) :T =
BEGIN
  RETURN NEW(Real, v := ABS(self.v));
END Rabs;

PROCEDURE Rmax(self :Real; n :T) :T =
BEGIN
  TYPECASE n OF
    | Real(r) => RETURN NEW(Real, v := MAX(self.v,r.v));
    | Integer(i) => IF MAX(FLOAT(i.v, Data.Element), self.v) = FLOAT(i.v, Data.Element) THEN
                      RETURN i;
                    ELSE
                      RETURN self;
                    END;
    ELSE RETURN self;
  END;
END Rmax;

PROCEDURE Rmin(self :Real; n :T) :T =
BEGIN
  TYPECASE n OF
    | Real(r) => RETURN NEW(Real, v := MIN(self.v,r.v));
    | Integer(i) => IF MIN(FLOAT(i.v, Data.Element), self.v) = FLOAT(i.v, Data.Element) THEN
                      RETURN i;
                    ELSE
                      RETURN self;
                    END;
    ELSE RETURN self;
  END;
END Rmin;

PROCEDURE Rfloor(self :Real) :T =
BEGIN
  RETURN NEW(Integer, v := FLOOR(self.v));
END Rfloor;

PROCEDURE Rceiling(self :Real) :T =
BEGIN
  RETURN NEW(Integer, v := CEILING(self.v));
END Rceiling;

PROCEDURE Rtrunc(self :Real) :T =
BEGIN
  RETURN NEW(Integer, v := TRUNC(self.v));
END Rtrunc;

PROCEDURE Rround(self :Real) :T =
BEGIN
  RETURN NEW(Integer, v := ROUND(self.v));
END Rround;

PROCEDURE Rtext(self :Real): TEXT =
BEGIN
  RETURN Data.FmtEl(self.v);
END Rtext;

PROCEDURE Ttext(self :Text): TEXT =
BEGIN
  RETURN self.v;
END Ttext;

PROCEDURE Ctext(self :Char): TEXT =
BEGIN
  RETURN Fmt.Char(self.v);
END Ctext;

BEGIN
END Value.





