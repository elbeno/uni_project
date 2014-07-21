MODULE GraphDraw EXPORTS Main;

IMPORT Graph, Thread, Common, Trestle, TrestleComm, Fmt, GraphType, TwoDCartesian, GraphTypeList, Data;

<* FATAL TrestleComm.Failure *>

VAR
  wholegraph :Graph.T;
  g :GraphType.T;

BEGIN
  Common.InitPanelCount();

  (* build up the master list of graph types 
     just add extra graph types here *)
  g := TwoDCartesian.MakeListEntry();
  Common.GraphTypes := GraphTypeList.Cons(g,Common.GraphTypes);

  (* calculate macheps *)
  Common.macheps := FLOAT(3.0e-8, Data.Element);

  wholegraph := Graph.New();
  Trestle.Install(wholegraph, "GraphDraw Control Panel " & Fmt.Int(wholegraph.number));
(*  Trestle.Install(wholegraph.graph, "Display " & Fmt.Int(wholegraph.number)); *)
  LOCK Common.NumberofPanels DO
    Thread.Wait(Common.NumberofPanels, Common.NumberofPanels.WaitForNone);
  END;

END GraphDraw.
