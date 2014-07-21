MODULE GraphUI;

IMPORT Eval, Value, FormsVBT, GraphUIBundle, Fmt, Rsrc, Trestle, Text, VBT, Rd, Thread, TrestleComm, FileBrowserVBT, Common, Graph, DataEdit, Data, ListVBT, GraphTypeList, DisplayGraph, AttachProcList, Filter, VBTClass, Region, AnalOptList, AnalOpt, FS, OSError, Pickle, Wr, IO;

<* FATAL Rsrc.NotFound, FormsVBT.Error, FormsVBT.Unimplemented, Rd.Failure, Thread.Alerted, TrestleComm.Failure *>

REVEAL 
  Private = FormsVBT.T BRANDED "PRIVUI" OBJECT END;
  T = Public BRANDED "PUBUI" OBJECT
      OVERRIDES
        init := Init;
      END;

PROCEDURE New():T =
BEGIN
  RETURN NEW(T).init();
END New;

PROCEDURE Init(self :T) :T =
VAR
  gl :GraphTypeList.T := Common.GraphTypes;
  pl :AttachProcList.T;

BEGIN
  EVAL self.initFromRsrc("ZControl.fv",Rsrc.BuildPath(GraphUIBundle.Get()));

  (* attach the callback procedures *)
  FormsVBT.AttachProc(self,"new",NewGraph);
  FormsVBT.AttachProc(self,"quit",Quit);
  FormsVBT.AttachProc(self,"loadsuffix",LoadFileExt);
  FormsVBT.AttachProc(self,"loadbox",BoxPopup);
  FormsVBT.AttachProc(self,"savebox",BoxPopup);
  FormsVBT.AttachProc(self,"printbox",BoxPopup);
  FormsVBT.AttachProc(self,"loadcancel",BoxCancel);
  FormsVBT.AttachProc(self,"savecancel",BoxCancel);
  FormsVBT.AttachProc(self,"printcancel",BoxCancel);
  FormsVBT.AttachProc(self,"close",Close);
  FormsVBT.AttachProc(self,"editdata",EditPanel);
  FormsVBT.AttachProc(self,"editformula",BoxPopup);
  FormsVBT.AttachProc(self,"updateformula",ApplyFormula);
  FormsVBT.AttachProc(self,"cancelnowformula",ApplyCancel);
  FormsVBT.AttachProc(self,"cancellaterformula",BoxCancel);
  FormsVBT.AttachProc(self,"displaygraph",ShowGraph);
  FormsVBT.AttachProc(self,"updategraph",UpdateGraph);
  FormsVBT.AttachProc(self,"load",Load);
  FormsVBT.AttachProc(self,"save",Save);
  FormsVBT.AttachProc(self,"print",Print);
  FormsVBT.AttachProc(self,"graphopts",GraphBoxPopup);
  FormsVBT.AttachProc(self,"axisopts",AxisBoxPopup); 
  FormsVBT.AttachProc(self,"analysisopts",AnalOptPopup);
  FormsVBT.AttachProc(self,"graphtype",SelectGraphType);
  FormsVBT.AttachProc(self,"analyse",Analyse);

  (* build the graph types browser *)
  WITH l = NARROW(FormsVBT.GetVBT(self,"graphtype"), ListVBT.T) DO
    l.insertCells(0,GraphTypeList.Length(Common.GraphTypes));
    FOR i := 0 TO GraphTypeList.Length(Common.GraphTypes)-1 DO
      l.setValue(i, GraphTypeList.Nth(Common.GraphTypes,i).desc);
    END;
    l.selectOnly(0);
  END;

  (* build the analysis routines browser *)
  WITH l = NARROW(FormsVBT.GetVBT(self,"routines"), ListVBT.T) DO
    l.insertCells(0,AnalOptList.Length(GraphTypeList.Nth(Common.GraphTypes,0).analoptlist));
    FOR i := 0 TO AnalOptList.Length(GraphTypeList.Nth(Common.GraphTypes,0).analoptlist)-1 DO
      l.setValue(i,AnalOptList.Nth(GraphTypeList.Nth(Common.GraphTypes,0).analoptlist,i).desc);
    END;
    l.selectOnly(0);
  END;

  (* attach the graph-type specific procedures *)
  WHILE NOT(gl=NIL) DO
    pl := gl.head.proclist;
    WHILE NOT(pl=NIL) DO
      FormsVBT.AttachProc(self,pl.head.VBTname,pl.head.p);
      pl := pl.tail;
    END;
    gl := gl.tail;
  END;

  RETURN self;
END Init;

(* File suffix handling routines *)

PROCEDURE LoadFileExt(             form :FormsVBT.T;
                      <* UNUSED *> event:Text.T;
                      <* UNUSED *> cl   :REFANY;
                      <* UNUSED *> ts   :VBT.TimeStamp)=
BEGIN
  WITH ext = FormsVBT.GetChoice(form, "loadsuffix") DO
    IF Text.Equal(ext,"loaddata") THEN
      FileBrowserVBT.SetSuffixes(FormsVBT.GetVBT(form,"loadbrowser"),"dat");
    ELSIF Text.Equal(ext,"loadgraph") THEN
      FileBrowserVBT.SetSuffixes(FormsVBT.GetVBT(form,"loadbrowser"),"grp");
    ELSE
      FileBrowserVBT.SetSuffixes(FormsVBT.GetVBT(form,"loadbrowser"),"");
    END;
  END;
END LoadFileExt;

(* The routines which handle the reactivity of the panels *)

PROCEDURE BoxPopup(             form :FormsVBT.T;
                   <* UNUSED *> event:Text.T;
                   <* UNUSED *> cl   :REFANY;
                   <* UNUSED *> ts   :VBT.TimeStamp) =
BEGIN
  FormsVBT.MakeDormant(form,"cpfilter");
END BoxPopup;

PROCEDURE GraphBoxPopup(             form :FormsVBT.T;
                        <* UNUSED *> event:Text.T;
                        <* UNUSED *> cl   :REFANY;
                        <* UNUSED *> ts   :VBT.TimeStamp) =
VAR
  t :TEXT;
  l :GraphTypeList.T := Common.GraphTypes;
BEGIN
  FormsVBT.MakeDormant(form,"cpfilter");
  (* look up the name of the zchild to pop up *)
  t := FormsVBT.GetText(form,"graphtype");
  WHILE NOT(Text.Equal(l.head.desc,t)) DO
    l := l.tail;
  END;
  FormsVBT.PopUp(form,l.head.graphoptsVBTname);
END GraphBoxPopup;

PROCEDURE AxisBoxPopup(             form :FormsVBT.T;
                       <* UNUSED *> event:Text.T;
                       <* UNUSED *> cl   :REFANY;
                       <* UNUSED *> ts   :VBT.TimeStamp) =
VAR
  t :TEXT;
  l :GraphTypeList.T := Common.GraphTypes;
BEGIN
  FormsVBT.MakeDormant(form,"cpfilter");
  (* look up the name of the zchild to pop up *)
  t := FormsVBT.GetText(form,"graphtype");
  WHILE NOT(Text.Equal(l.head.desc,t)) DO
    l := l.tail;
  END;
  FormsVBT.PopUp(form,l.head.axisoptsVBTname);
END AxisBoxPopup;

PROCEDURE AnalOptPopup(             form :FormsVBT.T;
                       <* UNUSED *> event:Text.T;
                       <* UNUSED *> cl   :REFANY;
                       <* UNUSED *> ts   :VBT.TimeStamp) =
VAR
  t :TEXT;
  l :GraphTypeList.T := Common.GraphTypes;
  al :AnalOptList.T;
BEGIN
  FormsVBT.MakeDormant(form,"cpfilter");
  (* look up the name of the zchild to pop up *)
  t := FormsVBT.GetText(form,"graphtype");
  WHILE NOT(Text.Equal(l.head.desc,t)) DO
    l := l.tail;
  END;
  al := l.head.analoptlist;
  t := FormsVBT.GetText(form,"routines");
  WHILE NOT(Text.Equal(al.head.desc,t)) DO
    al := al.tail;
  END; 
  FormsVBT.PopUp(form,al.head.VBTname);
END AnalOptPopup;

PROCEDURE BoxCancel(             form :FormsVBT.T;
                    <* UNUSED *> event:Text.T;
                    <* UNUSED *> cl   :REFANY;
                    <* UNUSED *> ts   :VBT.TimeStamp) =
BEGIN
  FormsVBT.MakeActive(form,"cpfilter");
END BoxCancel;

PROCEDURE OptionBoxCancel(form :FormsVBT.T;
                          event:Text.T;
                          cl   :REFANY;
                          ts   :VBT.TimeStamp) =
BEGIN
  FormsVBT.MakeActive(form,"cpfilter");
  UpdateGraph(form,event,cl,ts);
END OptionBoxCancel;

PROCEDURE Load(form :FormsVBT.T;
               event:Text.T;
               cl   :REFANY;
               ts   :VBT.TimeStamp) =
VAR
  filename :TEXT;
  i :INTEGER;
BEGIN
  filename := FormsVBT.GetText(form, "loadbrowser");
  WITH f = NARROW(form, Graph.T) DO
    IF Text.Equal(Text.Sub(filename, Text.Length(filename)-4), ".grp") THEN
      (* it's a graph file *)
      TRY
        UILoad(f,filename);
      EXCEPT
        | FileError(e) =>
          i := Text.FindChar(e,'*'); 
          IF i = -1 THEN
            Alert(form,e,"",TRUE);
          ELSE
            Alert(form,Text.Sub(e,0,i),Text.Sub(e,i+1),TRUE);
          END;
      END;
    ELSE
      (* don't know what it is - assume *)
      (* or it's a data file *)
      TRY
        f.graphdata.data := f.graphdata.data.initFromDataFile(filename);
      EXCEPT
        | Data.FileError(e) =>
          i := Text.FindChar(e,'*'); 
          IF i = -1 THEN
            Alert(form,e,"",TRUE);
          ELSE
            Alert(form,Text.Sub(e,0,i),Text.Sub(e,i+1),TRUE);
          END;
          f.graphdata.data := f.graphdata.data.init();
      END;
      IF f.graphdata.datavisible THEN
        Trestle.Delete(f.graphdata);
        f.graphdata := DataEdit.T.init(f.graphdata);
        Trestle.Install(f.graphdata, "GraphDraw Data " & Fmt.Int(f.number));
      ELSE
        f.graphdata := DataEdit.T.init(f.graphdata);
      END;
      UpdateGraph(form,event,cl,ts);
    END;
  END;
  FormsVBT.PopDown(form,"fileload");
  FormsVBT.MakeActive(form,"cpfilter");
END Load;

PROCEDURE Save(             form :FormsVBT.T;
               <* UNUSED *> event:Text.T;
               <* UNUSED *> cl   :REFANY;
               <* UNUSED *> ts   :VBT.TimeStamp) =
VAR
  filename :TEXT;
  i :INTEGER;
  backup :BOOLEAN;
BEGIN
  filename := FormsVBT.GetText(form, "savebrowser");
  backup := FormsVBT.GetBoolean(form, "backupfile");
  WITH f = NARROW(form, Graph.T) DO
    IF backup THEN
      (* rename the file filename.bak - delete it first because some
         implementations don't let you rename over files *)
      TRY
        FS.DeleteFile(filename & ".bak");
      EXCEPT
        | OSError.E =>
          (* the file obviously didn't exist - so what *)
      END;
      TRY
        FS.Rename(filename, filename & ".bak");
      EXCEPT
        | OSError.E =>
          (* the file obviously didn't exist - so what *)
      END;
    END;
    IF Text.Equal(Text.Sub(filename, Text.Length(filename)-4), ".grp") THEN
      (* it's a graph file *)
      TRY
        UISave(f,filename);
      EXCEPT
        | FileError(e) =>
          i := Text.FindChar(e,'*'); 
          IF i = -1 THEN
            Alert(form,e,"",TRUE);
          ELSE
            Alert(form,Text.Sub(e,0,i),Text.Sub(e,i+1),TRUE);
          END;
      END;
    ELSE
      (* don't know what it is - assume *)
      (* or it's a data file *)
      TRY
        f.graphdata.data.saveToDataFile(filename);
      EXCEPT
        | Data.FileError(e) =>
          i := Text.FindChar(e,'*'); 
          IF i = -1 THEN
            Alert(form,e,"",TRUE);
          ELSE
            Alert(form,Text.Sub(e,0,i),Text.Sub(e,i+1),TRUE);
          END;
      END;
    END;
  END;
  FormsVBT.PopDown(form,"filesave");
  FormsVBT.MakeActive(form,"cpfilter");
END Save;

PROCEDURE Print(             form :FormsVBT.T;
                <* UNUSED *> event:Text.T;
                <* UNUSED *> cl   :REFANY;
                <* UNUSED *> ts   :VBT.TimeStamp) =
VAR
  t, filename :TEXT;
  l :GraphTypeList.T := Common.GraphTypes;
  i :INTEGER;
BEGIN
  filename := FormsVBT.GetText(form, "printbrowser");
  (* look up the name of the proc to call *)
  t := FormsVBT.GetText(form,"graphtype");
  WHILE NOT(Text.Equal(l.head.desc,t)) DO
    l := l.tail;
  END;
  TRY 
    l.head.printproc(form,filename);
  EXCEPT
    DisplayGraph.FileError(e) =>
      i := Text.FindChar(e,'*'); 
      IF i = -1 THEN
        Alert(form,e,"",TRUE);
      ELSE
        Alert(form,Text.Sub(e,0,i),Text.Sub(e,i+1),TRUE);
      END;
  END;
  FormsVBT.PopDown(form,"fileprint");
  FormsVBT.MakeActive(form,"cpfilter");
END Print;

PROCEDURE SelectGraphType(             form :FormsVBT.T;
                          <* UNUSED *> event:Text.T;
                          <* UNUSED *> cl   :REFANY;
                          <* UNUSED *> ts   :VBT.TimeStamp) =
VAR
  t :TEXT;
  l :GraphTypeList.T := Common.GraphTypes;
  ip :DisplayGraph.InitProc;
  op :DisplayGraph.OptBlockProc;
  f :DisplayGraph.T;
  al :AnalOptList.T;
BEGIN
  (* get the graph type to display *)
  t := FormsVBT.GetText(form,"graphtype");
  (* find the thing in the list *)
  WHILE NOT(l = NIL) OR ip = NIL DO
    IF Text.Equal(l.head.desc,t) THEN 
      ip := l.head.proc;
      op := l.head.newoptions;
      al := l.head.analoptlist;
    END;
    l := l.tail;
  END; 
  f := NARROW(FormsVBT.GetGeneric(NARROW(form,Graph.T).graph,"gengraph"),DisplayGraph.T);  
  WITH g = NARROW(form, Graph.T) DO 
    f := DisplayGraph.New(g,ip,op);
  END;

  (* rebuild the analysis routines browser *)
  WITH rl = NARROW(FormsVBT.GetVBT(form,"routines"), ListVBT.T) DO
    rl.removeCells(0,rl.count());
    rl.insertCells(0,AnalOptList.Length(al));
    FOR i := 0 TO AnalOptList.Length(al)-1 DO
      rl.setValue(i,AnalOptList.Nth(al,i).desc);
    END;
    rl.selectOnly(0);
  END;
END SelectGraphType;

PROCEDURE NewGraph(<* UNUSED *> form :FormsVBT.T;
                   <* UNUSED *> event:Text.T;
                   <* UNUSED *> cl   :REFANY;
                   <* UNUSED *> ts   :VBT.TimeStamp) =
BEGIN
  WITH f = Graph.New() DO
    Trestle.Install(f, "GraphDraw Control Panel " & Fmt.Int(f.number));
  END;
END NewGraph;

PROCEDURE EditPanel(             form :FormsVBT.T;
                    <* UNUSED *> event:Text.T;
                    <* UNUSED *> cl   :REFANY;
                    <* UNUSED *> ts   :VBT.TimeStamp) =
BEGIN
  (* install the data editor panel *)  
  WITH f = NARROW(form,Graph.T) DO
    IF NOT(f.graphdata.datavisible) THEN
      f.graphdata.datavisible := TRUE; 
      f.graphdata := f.graphdata.init();  (* init form *)
      Trestle.Install(f.graphdata, "GraphDraw Data " & Fmt.Int(f.number));
    ELSE
    END;
  END;
END EditPanel;

PROCEDURE ShowGraph(             form :FormsVBT.T;
                    <* UNUSED *> event:Text.T;
                    <* UNUSED *> cl   :REFANY;
                    <* UNUSED *> ts   :VBT.TimeStamp) =
VAR
  z :DisplayGraph.T;
  newf :FormsVBT.T;
BEGIN
  (* install the graph display panel *)    
  WITH f = NARROW(form,Graph.T) DO
    z := FormsVBT.GetGeneric(f.graph,"gengraph");
    IF NOT(z.visible) THEN
      z := z.init(f);
      z.visible := TRUE;
      newf := NEW(FormsVBT.T).init("(Shape (Width " & Fmt.Int(z.horizsize) & ") (Height " & Fmt.Int(z.vertsize) & ") (Generic %gengraph))");
      FormsVBT.PutGeneric(f.graph,"gengraph",NIL);
      EVAL Filter.Replace(FormsVBT.GetVBT(newf,"gengraph"),z); 
      f.graph := newf; 
      Trestle.Install(f.graph, "Display " & Fmt.Int(f.number));
    ELSE
      z.visible := FALSE;
      Trestle.Delete(f.graph);
    END;
  END;
END ShowGraph;

PROCEDURE UpdateGraph(form  :FormsVBT.T;
                      event :Text.T;
                      cl    :REFANY;
                      ts    :VBT.TimeStamp) =
VAR
  z :DisplayGraph.T;
BEGIN
  WITH f = NARROW(form, Graph.T) DO
    z := FormsVBT.GetGeneric(f.graph,"gengraph");
    IF z.visible AND NOT(z.sizechanged) THEN
      z := z.init(f);
      VBTClass.Repaint(z,Region.Full);
    ELSIF z.visible THEN
      ShowGraph(form,event,cl,ts);
      ShowGraph(form,event,cl,ts);
    ELSE 
      ShowGraph(form,event,cl,ts);
    END;
    z.sizechanged := FALSE;
  END;
END UpdateGraph;

PROCEDURE ApplyFormula(form  :FormsVBT.T;
                       event :Text.T;
                       cl    :REFANY;
                       ts    :VBT.TimeStamp) =
VAR
  i :INTEGER;
BEGIN
  WITH f = NARROW(form, Graph.T) DO
    TRY
      f.formeval(FormsVBT.GetText(form,"formtext1"),FormsVBT.GetText(form,"formtext2"));
    EXCEPT
      | Eval.LexError(n) =>
        Alert(form,"Lexical Error","at line " & Fmt.Int(n),TRUE);
        RETURN;
      | Eval.SyntaxError(n) =>
        Alert(form,"Syntax Error","at line " & Fmt.Int(n),TRUE);
        RETURN;
      | Value.Silly(e) =>
        i := Text.FindChar(e,'*'); 
        IF i = -1 THEN
          Alert(form,e,"",TRUE);
        ELSE
          Alert(form,Text.Sub(e,0,i),Text.Sub(e,i+1),TRUE);
        END;
        RETURN;
      | Eval.EvalError(e) =>
        i := Text.FindChar(e,'*'); 
        IF i = -1 THEN
          Alert(form,e,"",TRUE);
        ELSE
          Alert(form,Text.Sub(e,0,i),Text.Sub(e,i+1),TRUE);
        END;
        RETURN;
    END;
  END;
  UpdateGraph(form,event,cl,ts);
END ApplyFormula;

PROCEDURE ApplyCancel(form  :FormsVBT.T;
                      event :Text.T;
                      cl    :REFANY;
                      ts    :VBT.TimeStamp) =
BEGIN
  ApplyFormula(form,event,cl,ts);
  FormsVBT.MakeActive(form,"cpfilter");
END ApplyCancel;

PROCEDURE Alert(form :FormsVBT.T; message1 :TEXT; message2 :TEXT; modal :BOOLEAN) =
BEGIN
  IF NOT(modal) THEN
    FormsVBT.PutText(form,"alerttext1",message1);
    FormsVBT.PutText(form,"alerttext2",message2);
    FormsVBT.PopUp(form,"alertbox");
  ELSE
    FormsVBT.PutText(form,"modalalerttext1",message1);
    FormsVBT.PutText(form,"modalalerttext2",message2);
    FormsVBT.PopUp(form,"modalalertbox");
  END;
END Alert;

PROCEDURE Analyse(             form  :FormsVBT.T;
                  <* UNUSED *> event :Text.T;
                  <* UNUSED *> cl    :REFANY;
                  <* UNUSED *> ts    :VBT.TimeStamp) =
VAR
  t :TEXT;
  l :GraphTypeList.T := Common.GraphTypes;
  al :AnalOptList.T;
  i :INTEGER;
BEGIN
  (* look up the name of the proc to call *)
  t := FormsVBT.GetText(form,"graphtype");
  WHILE NOT(Text.Equal(l.head.desc,t)) DO
    l := l.tail;
  END;
  al := l.head.analoptlist;
  t := FormsVBT.GetText(form,"routines");
  WHILE NOT(Text.Equal(al.head.desc,t)) DO
    al := al.tail;
  END;
  TRY 
    al.head.proc(form);
  EXCEPT
    AnalOpt.Error(e) =>
      i := Text.FindChar(e,'*'); 
      IF i = -1 THEN
        Alert(form,e,"",TRUE);
      ELSE
        Alert(form,Text.Sub(e,0,i),Text.Sub(e,i+1),TRUE);
      END;
  END;
END Analyse;

PROCEDURE Close(             form :FormsVBT.T;
                <* UNUSED *> event:Text.T;
                <* UNUSED *> cl   :REFANY;
                <* UNUSED *> ts   :VBT.TimeStamp) =
VAR
  i :INTEGER;
  z :DisplayGraph.T;
BEGIN
  WITH f = NARROW(form, Graph.T) DO
    z := FormsVBT.GetGeneric(f.graph,"gengraph");
    IF z.visible THEN
      Trestle.Delete(f.graph);
    END;    
    IF f.graphdata.datavisible THEN
      Trestle.Delete(f.graphdata);
    END;
  END;
  Trestle.Delete(form);
  i := Common.NumberofPanels.dec();
END Close;

PROCEDURE Quit(<* UNUSED *> form :FormsVBT.T;
               <* UNUSED *> event:Text.T;
               <* UNUSED *> cl   :REFANY;
               <* UNUSED *> ts   :VBT.TimeStamp) =
BEGIN
  LOCK Common.NumberofPanels DO
    Thread.Signal(Common.NumberofPanels.WaitForNone);
  END;
END Quit;


PROCEDURE UISave(self :Graph.T; filename :TEXT) RAISES {FileError} =
VAR
  file :Wr.T;
BEGIN
  file := IO.OpenWrite(filename);
  IF file = NIL THEN
    RAISE FileError("Error Opening File*" & filename);
  END;
  TRY
    Pickle.Write(file,self);
  EXCEPT
    | Pickle.Error, Wr.Failure =>
      Wr.Close(file);
      RAISE FileError("Error Writing to File*" & filename);
  END;
  Wr.Close(file);
END UISave;

PROCEDURE UILoad(self :Graph.T; filename :TEXT) RAISES {FileError} =
VAR
  file :Rd.T;
BEGIN
  file := IO.OpenRead(filename);
  IF file = NIL THEN
    RAISE FileError("Error Opening File*" & filename);
  END;
  TRY
    self := Pickle.Read(file);
  EXCEPT
    | Rd.EndOfFile =>
      Rd.Close(file);
      RAISE FileError("Premature EOF");
    | Pickle.Error =>
      Rd.Close(file);
      RAISE FileError("Error Reading from File*" & filename);
  END;
END UILoad;

BEGIN
(* The main bit! *)
END GraphUI.

