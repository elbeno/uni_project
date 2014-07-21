MODULE Eval;

IMPORT LongReal, Value, Text, Lex, FloatMode, Scan, Math, Wr, Stdio;

REVEAL
  Private = BRANDED "PRIVEVAL" OBJECT END;
  T = Public BRANDED "PUBEVAL" OBJECT
             METHODS
               lexan() :TVPair RAISES {LexError} := Lexan;
               emit(t :TVPair) RAISES {LexError} := Emit;
               match(t :TVPair) RAISES {LexError, SyntaxError} := Match;
               expr() RAISES {LexError, SyntaxError} := Expr;
               term() RAISES {LexError, SyntaxError} := Term;
               factor() RAISES {LexError, SyntaxError} := Factor;
               subfactor() RAISES {LexError, SyntaxError} := SubFactor;
               index() RAISES {LexError, SyntaxError} := Index;
               atom() RAISES {LexError, SyntaxError} := Atom;
             OVERRIDES
               parse := Parse;
               eval := Evaluate;
             END;
  SymTable = PublicSymTable BRANDED "PUBSYM" OBJECT
                            OVERRIDES
                              insert := Insert;
                              lookup := Lookup;
                              assignattrib := AssignAttrib;
                            END;
  Stack = PublicStack BRANDED "PUBSTACK" OBJECT
                      OVERRIDES
                        push := Push;
                        pop := Pop;
                        empty := Empty;
                      END;

PROCEDURE New() :T =
VAR
  r :T;
BEGIN
  r := NEW(T);
  r.stack := NEW(Stack);
  r.symtable := InitSymTable();
  RETURN r;
END New;

PROCEDURE Push(self :Stack; t :TVPair) =
BEGIN
  self.list := NEW(REF TVPairList, head := t, tail := self.list);
END Push;

PROCEDURE Pop(self :Stack):TVPair =
VAR
  r :TVPair;
BEGIN
  r := self.list.head;
  self.list := self.list.tail;
  RETURN r;
END Pop;

PROCEDURE Empty(self :Stack):BOOLEAN =
BEGIN
  RETURN (self.list = NIL);
END Empty;

PROCEDURE InitSymTable(): SymTable =
VAR
  self :SymTable := NEW(SymTable);
  r :REF LONGREAL;
BEGIN
  r := NEW(REF LONGREAL);
  EVAL self.insert("DIV",div);
  EVAL self.insert("MOD",mod);
  EVAL self.insert("SIN",sin);
  EVAL self.insert("COS",cos);
  EVAL self.insert("TAN",tan);
  EVAL self.insert("ASIN",asin);
  EVAL self.insert("ACOS",acos);
  EVAL self.insert("ATAN",atan);
  EVAL self.insert("EXP",exp);
  EVAL self.insert("LN",ln);
  EVAL self.insert("LOG",log);
  EVAL self.insert("SQRT",sqrt);
  EVAL self.insert("ABS",abs);
  EVAL self.insert("MAX",max);
  EVAL self.insert("MIN",min);
  EVAL self.insert("FLOOR",floor);
  EVAL self.insert("CEILING",ceiling);
  EVAL self.insert("TRUNC",trunc);
  EVAL self.insert("ROUND",round);
  EVAL self.insert("PI",constpi);
  r^ := FLOAT(Math.Pi, LONGREAL);
  self.assignattrib("PI",Value.Create(r));
  EVAL self.insert("e",conste);
  r^ := FLOAT(Math.E, LONGREAL);
  self.assignattrib("e",Value.Create(r));
  EVAL self.insert("INF",infinity);
  r^ := LongReal.MaxFinite;
  self.assignattrib("INF",Value.Create(r));
  EVAL self.insert("DOMAIN",domain);
  EVAL self.insert("SUBJECT",subject);
  RETURN self;
END InitSymTable;

PROCEDURE Insert(self :SymTable; s :TEXT; t :INTEGER) :Entry = 
VAR 
  e := Entry{s,t,NIL};
  l :REF EntryList := self.list;
BEGIN
  (* if the entry is already in the table, we can overwrite *)
  WHILE l # NIL DO
    IF Text.Equal(s,l^.head.lexeme) THEN
      l^.head.token := t;
      l^.head.attrib := NIL;
      RETURN l^.head;
    END;
    l := l^.tail;
  END;
  self.list := NEW(REF EntryList, head := e, tail := self.list);  
  RETURN e;
END Insert;

PROCEDURE Lookup(self :SymTable; s :TEXT) :Entry =
VAR
  l :REF EntryList := self.list;
BEGIN
  WHILE l # NIL DO
    IF Text.Equal(s,l^.head.lexeme) THEN
      RETURN l^.head;
    END;
    l := l^.tail;
  END;
  RETURN Entry {"", 0, NIL};
END Lookup;

PROCEDURE AssignAttrib(self :SymTable; s :TEXT; a :Value.T) =
VAR
  l :REF EntryList := self.list;
BEGIN
  WHILE l # NIL DO
    IF Text.Equal(s,l^.head.lexeme) THEN
       l^.head.attrib := a;
       RETURN;
    END;
    l := l^.tail;
  END;
END AssignAttrib;

PROCEDURE Lexan(self :T):TVPair RAISES {LexError} =
VAR
  t :CHAR;
  p :Entry;
  lexeme :TEXT := "";
  tokenval :REFANY;
  test :LONGREAL;
BEGIN
  WHILE TRUE DO
    IF Text.Length(self.formula) = 0 THEN
      RETURN TVPair{done,Value.Create(NIL)};
    END;
    t := Text.GetChar(self.formula,0);
    IF t=' ' OR t='\t' THEN   (* ignore whitespace *)
      self.formula := Text.Sub(self.formula,1);
    ELSIF t='\n' THEN         (* another line *)
      INC(self.lineno);
      self.formula := Text.Sub(self.formula,1);
    ELSIF t IN numerals THEN  (* it's a number of some sort *)
      WHILE t IN numerals DO
        lexeme := Text.Cat(lexeme, Text.FromChar(t));        
        self.formula := Text.Sub(self.formula, 1);
        t := Text.GetChar(self.formula,0);
      END;
      TRY
        test := Scan.LongReal(lexeme);    (* assume it's a longreal *)
      EXCEPT
        Lex.Error, FloatMode.Trap => RAISE LexError(self.lineno);
      END;
      IF Text.FindChar(lexeme, '.') = -1 AND 
         test >= FLOAT(FIRST(INTEGER), LONGREAL) AND 
         test <= FLOAT(LAST(INTEGER), LONGREAL) THEN
        tokenval := NEW(REF INTEGER);        (* convert to an integer if we can *)
        NARROW(tokenval, REF INTEGER)^ := ROUND(test);
        RETURN TVPair{int,Value.Create(tokenval)};
      ELSE
        tokenval := NEW(REF LONGREAL);       (* it's a longreal after all *)
        NARROW(tokenval, REF LONGREAL)^ := test;
        RETURN TVPair{real,Value.Create(tokenval)};
      END;     
    ELSIF t IN alphas THEN     (* it's an id or a operation/function *)
      WHILE t IN alphas + numerals DO
        lexeme := Text.Cat(lexeme, Text.FromChar(t));        
        self.formula := Text.Sub(self.formula,1);
        t := Text.GetChar(self.formula,0);      
      END;
      p := self.symtable.lookup(lexeme);     (* is it in the symbol table? *)
      IF p.token = 0 THEN
        p := self.symtable.insert(lexeme,id);  (* it isn't - so put it in *)
        tokenval := NEW(REF TEXT);
        NARROW(tokenval, REF TEXT)^ := "id";   (* give new id's a value of "id" *)
        self.symtable.assignattrib(lexeme,Value.Create(tokenval));
        INC(self.varcount);
      END;
      tokenval := NEW(REF TEXT);
      NARROW(tokenval, REF TEXT)^ := lexeme;
      RETURN TVPair{p.token, Value.Create(tokenval)};  
    ELSE                         (* it's a char - hope it's one we recognise *)
      tokenval := NEW(REF CHAR);
      NARROW(tokenval, REF CHAR)^ := t;
      self.formula := Text.Sub(self.formula,1);
      RETURN TVPair{ORD(t), Value.Create(tokenval)};
    END;
  END;
END Lexan;

PROCEDURE Emit(self :T; t :TVPair) RAISES {LexError} =
BEGIN
  CASE t.token OF
    | ORD('+'), ORD('-'), ORD('*'), ORD('/'), ORD('^'), ORD('~'), ORD('!') =>
        Wr.PutText(Stdio.stdout, t.value.totext() & "\n");
        Wr.Flush(Stdio.stdout);
    | div, mod, sin, cos, tan, asin, acos, atan, exp, ln, log, sqrt, domain, subject, abs, max, min, floor, ceiling, round, trunc =>
        Wr.PutText(Stdio.stdout, t.value.totext() & "\n");
        Wr.Flush(Stdio.stdout);
    | id, constpi, conste, int, real, infinity =>
        Wr.PutText(Stdio.stdout, t.value.totext() & "\n");
        Wr.Flush(Stdio.stdout);
    ELSE
        RAISE LexError(self.lineno);  (* oops - don't know that char *)
    END;
  self.stack.push(t);
END Emit;

PROCEDURE Match(self :T; t :TVPair) RAISES {LexError, SyntaxError} =
BEGIN
  IF self.lookahead.token = t.token THEN
    self.lookahead := self.lexan();
  ELSE
    RAISE SyntaxError(self.lineno);
  END;
END Match;

PROCEDURE Parse(self :T) RAISES {LexError, SyntaxError, Value.Silly} = 
BEGIN
  self.lookahead := self.lexan();
  WHILE self.lookahead.token # done DO
    self.expr();
    self.match(TVPair{ORD(';'),Value.Create(NIL)});
  END;
END Parse;

PROCEDURE Expr(self :T) RAISES {LexError, SyntaxError} = 
VAR
  t :TVPair;
BEGIN
  self.term();
  WHILE TRUE DO
    CASE self.lookahead.token OF
      | ORD('+'), ORD('-') =>
        t := self.lookahead;
        self.match(self.lookahead);
        self.term();
        self.emit(t);
      ELSE 
        RETURN;  
    END;
  END;  
END Expr;

PROCEDURE Term(self :T) RAISES {LexError, SyntaxError} = 
VAR
  t :TVPair;
BEGIN
  self.factor();
  WHILE TRUE DO
    CASE self.lookahead.token OF
      | ORD('*'), ORD('/'), div, mod =>
        t := self.lookahead;
        self.match(self.lookahead);
        self.factor();
        self.emit(t);
      ELSE 
        RETURN;  
    END;
  END;  
END Term;

PROCEDURE Factor(self :T) RAISES {LexError, SyntaxError} = 
VAR
  t :TVPair;
BEGIN
  self.subfactor();
  WHILE TRUE DO
    CASE self.lookahead.token OF
      | ORD('^') =>
        t := self.lookahead;
        self.match(self.lookahead);
        self.factor();
        self.emit(t);
      ELSE 
        RETURN;  
    END;
  END;  
END Factor;

PROCEDURE SubFactor(self :T) RAISES {LexError, SyntaxError} = 
VAR
  t :TVPair;
BEGIN
  self.index();
  CASE self.lookahead.token OF
    | ORD('!') =>
      t := self.lookahead;
      self.match(self.lookahead);
      self.emit(t);
    ELSE 
      RETURN;  
  END; 
END SubFactor;

PROCEDURE Index(self :T) RAISES {LexError, SyntaxError} = 
VAR
  t :TVPair;
BEGIN
  CASE self.lookahead.token OF
    | ORD('-') =>
      t := self.lookahead;
      self.match(self.lookahead);
      self.index();
      self.emit(TVPair{ORD('~'), t.value});
    ELSE
      self.atom(); 
      RETURN;  
  END;  
END Index;

PROCEDURE Atom(self :T) RAISES {LexError, SyntaxError} = 
VAR
  t :TVPair;
BEGIN
  CASE self.lookahead.token OF
    | sin, cos, tan, asin, acos, atan, exp, ln, log, sqrt, subject, abs, floor, ceiling, trunc, round =>
      t := self.lookahead;
      self.match(self.lookahead);
      self.match(TVPair{ORD('('),Value.Create(NIL)});  (* match open bracket *)
      self.expr();                                     (* do expr inside *)
      self.match(TVPair{ORD(')'),Value.Create(NIL)});  (* match closebracket *)
      self.emit(t);                                    (* emit the op *)
    | ORD('(') =>
      self.match(TVPair{ORD('('),Value.Create(NIL)});  (* same here *)
      self.expr();                                     (* but don't emit *)
      self.match(TVPair{ORD(')'),Value.Create(NIL)});  (* rpn => no brackets *)
    | int, real, id, constpi, conste, infinity =>
      self.emit(self.lookahead);
      self.match(self.lookahead);
    | max, min =>
      t := self.lookahead;
      self.match(self.lookahead);
      self.match(TVPair{ORD('('),Value.Create(NIL)});  (* match open bracket *)
      self.expr();
      self.match(TVPair{ORD(','),Value.Create(NIL)});  (* match comma *)
      self.expr();
      self.match(TVPair{ORD(')'),Value.Create(NIL)});  (* match closebracket *)
      self.emit(t);
    | domain =>
      t := self.lookahead;
      self.match(self.lookahead);
      self.match(TVPair{ORD('('),Value.Create(NIL)});  (* match open bracket *)
      self.expr();
      self.match(TVPair{ORD(','),Value.Create(NIL)});  (* match comma 1 *)
      self.expr();
      self.match(TVPair{ORD(','),Value.Create(NIL)});  (* match comma 2 *)
      self.expr();
      self.match(TVPair{ORD(','),Value.Create(NIL)});  (* match comma 3 *)
      self.expr();
      self.match(TVPair{ORD(')'),Value.Create(NIL)});  (* match closebracket *)
      self.emit(t);                                    (* emit DOMAIN *)
    ELSE 
      RAISE SyntaxError(self.lineno);
  END;  
END Atom;

PROCEDURE Evaluate(self :T) :TVPair RAISES {Value.Silly, EvalError} = 
VAR
  a, b, c, d, op :TVPair;
  e :Entry;
  r :REF TEXT;
BEGIN
  IF self.stack.empty() THEN
    RAISE EvalError("Help! No Expression Left!");
  END;
  op := self.stack.pop();
  CASE op.token OF
    | ORD('~') =>
      a := self.eval();
      a.value := a.value.negate();
      RETURN a;
    | ORD('!') =>
      a := self.eval();
      a.value := a.value.factorial();
      RETURN a;
    | ORD('+') =>
      b := self.eval();
      a := self.eval();
      a.value := a.value.add(b.value);
      RETURN a;
    | ORD('-') =>
      b := self.eval();
      a := self.eval();
      a.value := a.value.subtract(b.value);
      RETURN a;      
    | ORD('*') =>
      b := self.eval();
      a := self.eval();
      a.value := a.value.multiply(b.value);
      RETURN a;
    | ORD('/') =>
      b := self.eval();
      a := self.eval();
      a.value := a.value.divide(b.value);
      RETURN a;
    | ORD('^') =>
      b := self.eval();
      a := self.eval();
      a.value := a.value.raiseto(b.value);
      RETURN a;
    | div =>
      b := self.eval();
      a := self.eval();
      a.value := a.value.div(b.value);
      RETURN a;
    | mod =>
      b := self.eval();
      a := self.eval();
      a.value := a.value.mod(b.value);
      RETURN a;
    | sin =>
      a := self.eval();
      a.value := a.value.sin(); 
      RETURN a;
    | cos =>
      a := self.eval();
      a.value := a.value.cos();
      RETURN a;
    | tan =>
      a := self.eval();
      a.value := a.value.tan();
      RETURN a;
    | asin =>
      a := self.eval();
      a.value := a.value.asin();
      RETURN a;
    | acos =>
      a := self.eval();
      a.value := a.value.acos();
      RETURN a;
    | atan =>
      a := self.eval();
      a.value := a.value.atan();
      RETURN a;
    | exp =>
      a := self.eval();
      a.value := a.value.exp();
      RETURN a;
    | ln =>
      a := self.eval();
      a.value := a.value.ln();
      RETURN a;
    | sqrt =>
      a := self.eval();
      a.value := a.value.sqrt();
      RETURN a;
    | log =>
      a := self.eval();
      a.value := a.value.log();
      RETURN a; 
    | abs =>
      a := self.eval();
      a.value := a.value.abs();
      RETURN a; 
    | max =>
      b := self.eval();
      a := self.eval();
      a.value := a.value.max(b.value);
      RETURN a; 
    | min =>
      b := self.eval();
      a := self.eval();
      a.value := a.value.min(b.value);
      RETURN a; 
    | floor =>
      a := self.eval();
      a.value := a.value.floor();
      RETURN a; 
    | ceiling =>
      a := self.eval();
      a.value := a.value.ceiling();
      RETURN a; 
    | trunc =>
      a := self.eval();
      a.value := a.value.trunc();
      RETURN a; 
    | round =>
      a := self.eval();
      a.value := a.value.round();
      RETURN a; 
    | domain =>
      d := self.eval();    (* no. of steps - if this is a longreal, round it *)
      d.value := d.value.round();
      c := self.eval();    (* max value *)
      b := self.eval();    (* min value *)
      a := self.stack.pop();  (* variable name - but we don't want to convert *)
      IF a.token # id THEN
        RAISE EvalError("First arg of DOMAIN*must be an identifier");
      END;
      Wr.PutText(Stdio.stdout, "Putting domain of " & a.value.totext() & " into symbol table\n");
      Wr.Flush(Stdio.stdout);
      (* put three id's into the symbol table specifying the domain of the variable *)
      EVAL self.symtable.insert(a.value.totext() & "max",id);
      self.symtable.assignattrib(a.value.totext() & "max", c.value);
      EVAL self.symtable.insert(a.value.totext() & "min",id);
      self.symtable.assignattrib(a.value.totext() & "min", b.value);
      EVAL self.symtable.insert(a.value.totext() & "steps",id);
      self.symtable.assignattrib(a.value.totext() & "steps", d.value);
      RETURN d;      
    | subject =>
      a := self.stack.pop();  (* variable name *)
      IF a.token # id THEN
        RAISE EvalError("Argument of SUBJECT*must be an identifier");
      END;
      Wr.PutText(Stdio.stdout, "Making " & a.value.totext() & " the subject\n");
      Wr.Flush(Stdio.stdout);
      (* assign it an attribute of "subject" *)
      r := NEW(REF TEXT);
      r^ := "subject";
      self.symtable.assignattrib(a.value.totext(),Value.Create(r));
      RETURN a;
    | id, constpi, conste, infinity =>
      (* look it up in the symbol table and return TVPair{id,value} *)
      e := self.symtable.lookup(op.value.totext());
      IF e.token = 0 THEN
        (* it's not in the symbol table *)
        RAISE EvalError(op.value.totext() & " not recognised");
      ELSIF Text.Equal(e.attrib.totext(), "subject") THEN
        RAISE EvalError("Subject of expression*appears in expression");
      END;
      op.value := e.attrib;
      RETURN op;
    ELSE
      (* we must be at the end of this evaluation and we've got a numeric value *)
      RETURN op;
  END;
END Evaluate;

BEGIN
END Eval.
