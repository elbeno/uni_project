INTERFACE Eval;

IMPORT Value;

TYPE
  Entry = RECORD
            lexeme :TEXT;
            token :INTEGER;
            attrib :Value.T;
          END;
  EntryList = RECORD
                head :Entry;
                tail :REF EntryList;
              END;

  PublicSymTable = OBJECT
               list :REF EntryList;
             METHODS
               insert(s :TEXT; t :INTEGER) :Entry;
               lookup(s :TEXT) :Entry;
               assignattrib(s: TEXT; a: Value.T);
             END;

  SymTable <: PublicSymTable;

  TVPair = RECORD
             token :INTEGER;
             value :Value.T;
           END;
  TVPairList = RECORD
                 head :TVPair;
                 tail :REF TVPairList;
               END;

  PublicStack = OBJECT
            list :REF TVPairList;
          METHODS
            push(t :TVPair);
            pop():TVPair;
            empty():BOOLEAN;
          END;

  Stack <: PublicStack;

  Private <: ROOT;
  Public = Private OBJECT
             formula :TEXT;
             stack :Stack;
             symtable :SymTable;
             lookahead :TVPair;
             lineno :INTEGER := 1;
             varcount :INTEGER := 0;
             mappings :REF ARRAY OF TEXT;
           METHODS
             parse() RAISES {LexError, SyntaxError, Value.Silly};
             eval() :TVPair RAISES {Value.Silly, EvalError};
           END;
  T <: Public;

CONST
  int = 256;
  div = 257;
  mod = 258;
  sin = 259;
  cos = 260;
  id  = 261;
  done = 262;
  real = 263;  
  tan = 264;
  exp = 265;
  ln = 266;
  log = 267;
  sqrt = 268;
  asin = 269;
  acos = 270;
  atan = 271;
  domain = 272;
  subject = 273;
  constpi = 274;
  conste = 275;
  infinity = 276;
  abs = 277;
  max = 278;
  min = 279;
  floor = 280;
  ceiling = 281;
  trunc = 282;
  round = 283;
  numerals = SET OF CHAR{'0' .. '9', '.'};
  alphas = SET OF CHAR{'A' .. 'Z', 'a' .. 'z'};

EXCEPTION
  SyntaxError(INTEGER);
  LexError(INTEGER);
  EvalError(TEXT);

PROCEDURE New() :T;

END Eval.
