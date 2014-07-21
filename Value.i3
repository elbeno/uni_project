INTERFACE Value;

TYPE
  T = OBJECT
      METHODS
        negate() :T RAISES {Silly};
        add(v :T) :T RAISES {Silly};
        subtract(v :T) :T RAISES {Silly};
        multiply(v :T) :T RAISES {Silly};
        divide(v: T) :T RAISES {Silly};
        div(v: T) :T RAISES {Silly};
        mod(v: T) :T RAISES {Silly};
        raiseto(v: T) :T RAISES {Silly};
        exp() :T RAISES {Silly};
        ln() :T RAISES {Silly};
        log() :T RAISES {Silly};
        sqrt() :T RAISES {Silly};
        cos() :T RAISES {Silly};
        sin() :T RAISES {Silly};
        tan() :T RAISES {Silly};
        acos() :T RAISES {Silly};
        asin() :T RAISES {Silly};
        atan() :T RAISES {Silly};
        factorial() :T RAISES {Silly};
        abs() :T;
        max(v :T) :T;
        min(v :T) :T;
        floor() :T;
        ceiling() :T;
        trunc() :T;
        round() :T;
        totext() :TEXT;
      END;

EXCEPTION Silly(TEXT);

PROCEDURE Create(r :REFANY) :T;

END Value.