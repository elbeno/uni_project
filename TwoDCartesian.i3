INTERFACE TwoDCartesian;

IMPORT DisplayGraph, Graph, GraphType, Axis;

TYPE
  CAxis = RECORD
            on :BOOLEAN := TRUE;       (* axis line turned on or off *)
            datacolumn :INTEGER := 0;  (* which data col the axis maps to *)
            tickson :BOOLEAN := TRUE;    (* ticks turned on or off *)
            ticknoteson :BOOLEAN := TRUE; (* tick labelling on or off *)
            ticksize :INTEGER := 10;     (* size of ticks in pixels *)
            legendfont :TEXT := "-adobe-courier-bold-r-*-*-*-120-*-*-*-*-*-*"; 
                                        (* legend font *)
            tickspacing :INTEGER := 50; (* tick spacing *)
            reverse :INTEGER := 1;     (* which side of the axis ticks are *)
            legend :TEXT := "";        (* axis legend *)
          END;
  OptBlock = REF RECORD
               a :Axis.T := Axis.T.Hor;
               title :TEXT := "";
               showplot :BOOLEAN := TRUE;
               showmarkers :BOOLEAN := FALSE;
               showgrid :BOOLEAN := FALSE;
               gridspacing :INTEGER := 20;
               axes :ARRAY Axis.T OF CAxis;
             END;

PROCEDURE MakeListEntry():GraphType.T;
PROCEDURE Init(self :DisplayGraph.T; g :Graph.T): DisplayGraph.T;
PROCEDURE NewBlock():REFANY;

END TwoDCartesian.
