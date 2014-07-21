INTERFACE GraphAxis;

(* A GraphAxis.T is the basic Graph Axis Object in the program. *)

IMPORT Font;

TYPE
   Private <: ROOT;
   Public = Private OBJECT
         on :BOOLEAN := TRUE;                (* axis line turned on or off *)
         datacolumn :INTEGER := 0;    (* which data column the axis maps to *)
         tickson :BOOLEAN := TRUE;           (* ticks turned on or off *)
         ticknoteson :BOOLEAN := TRUE;       (* tick labelling on or off *)
         ticksize :INTEGER := 20;            (* size of ticks in pixels *)
         legendfont :Font.T := Font.BuiltIn; (* legend font/size *)
         tickspacing :INTEGER := 5;          (* tick spacing *)
         tickside :BOOLEAN := TRUE;   (* which side of the axis ticks appear *)
         legend :TEXT := "";                 (* axis legend *)
      END;
   T <: Public;

PROCEDURE New() :T;

END GraphAxis.