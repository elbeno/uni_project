INTERFACE Graph;

(* A Graph.T is the basic Graph Object in the program. *)
(* It contains all the datatypes necessary for the displayable graph *)
(* and the UI and various other stuff *)
(* It has methods init, load, save, analyse, print *)

IMPORT Text, GraphUI, DataEdit, FormsVBT, Data, Eval, Value, Approximation, AnalOpt;

TYPE
   Private <: GraphUI.T;
   Public = Private OBJECT
         graph     :FormsVBT.T; (* DisplayGraph.T; *)
         graphdata :DataEdit.T;   (* the data window and data *)
         number    :INTEGER;      (* the panel number *)
         formula :Eval.T;         (* the formula to be evaluated *)
         approx :Approximation.T; (* the approximating poly/spline/whatever *)
       METHODS
         init() :T;
         analyse();
         print(filename: Text.T);
         formeval(decls :TEXT; expr :TEXT) RAISES {Eval.LexError, Eval.SyntaxError, Eval.EvalError, Value.Silly};   (* evaluates the formula *)
         eval(x :REF ARRAY OF Data.Element; override :BOOLEAN := FALSE):Approximation.Result RAISES {AnalOpt.Error};
      END;
   T <: Public;

PROCEDURE New(): T;

END Graph.