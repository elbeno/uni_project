INTERFACE DisplayGraph;

(* A DisplayGraph.T is the basic displayable Graph Object in the program. *)

IMPORT VBT, Graph, NoteList, Path, PointList, FillRegionList, FormsVBT;

TYPE
  Private <: VBT.Leaf;
  Public = Private OBJECT
        visible :BOOLEAN := FALSE;
        sizechanged :BOOLEAN := FALSE;
        graphpath :Path.T;
        axespaths :REF ARRAY OF Path.T;
        tickspaths :REF ARRAY OF Path.T;
        points :PointList.T;
        gridpath :Path.T;
        titles :NoteList.T;
        labels :NoteList.T;
        notes :NoteList.T;
        fills :FillRegionList.T;
        horizsize :INTEGER := 500;
        vertsize :INTEGER := 500;
        border :INTEGER := 50;
        graphopts :REFANY;
        titlefont :TEXT := "-adobe-courier-bold-r-*-*-*-120-*-*-*-*-*-*";
        labelfont :TEXT := "-adobe-courier-bold-r-*-*-*-120-*-*-*-*-*-*";
        notesfont :TEXT := "-adobe-courier-bold-r-*-*-*-120-*-*-*-*-*-*";
        
     METHODS
        init(g :Graph.T): T;  (* changed from a Data.T *)
     END;
  T <: Public;
  InitProc = PROCEDURE(self: T; g :Graph.T): T;
  PrintProc = PROCEDURE(form :FormsVBT.T; filename :TEXT) RAISES {FileError};
  OptBlockProc = PROCEDURE():REFANY;  

EXCEPTION
  FileError(TEXT);

PROCEDURE New(g :Graph.T; initproc :InitProc; optinitproc :OptBlockProc): T;

END DisplayGraph.




