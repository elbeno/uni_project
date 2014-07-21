MODULE TwoDCartesian;

IMPORT TwoDInterpolate, TwoDIntegrate, TwoDDifferentiate, Data, VBT, Path, Point, Fmt, DisplayGraph, Note, Font, NoteList, GraphType, AttachProcList, AnalOptList, AttachProc, AnalOpt, FormsVBT, Text, Axis, Graph, GraphUI, Rect, PointList, Approximation, TwoDRootFind, IO, Wr;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

PROCEDURE MakeListEntry():GraphType.T =
VAR
  pl :AttachProcList.T;
  al :AnalOptList.T;
BEGIN
  (* build the list of callback procedures *)
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="showplot1",p:=PlotToggle},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="showmarkers1",p:=MarkerToggle},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="showgrid1",p:=GridToggle},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="gridspace1",p:=GridSpace},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="horizsize1",p:=SetWidth},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="vertsize1",p:=SetHeight},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="border1",p:=SetBorder},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="axischoice1",p:=SetAxis},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="maptocol1",p:=AxisMap},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="showaxis1",p:=AxisToggle},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="showticks1",p:=TickToggle},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="showlabels1",p:=LabelToggle},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="reverseticks1",p:=ReverseTicks},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="ticksize1",p:=TickSize},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="tickspace1",p:=TickSpace},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="cancelnowgraphopts1",p:=GraphUI.OptionBoxCancel},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="cancelnowaxisopts1",p:=GraphUI.OptionBoxCancel},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="cancellatergraphopts1",p:=GraphUI.BoxCancel},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="cancellateraxisopts1",p:=GraphUI.BoxCancel},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="updategraphopts1",p:=GraphUI.UpdateGraph},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="updateaxisopts1",p:=GraphUI.UpdateGraph},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="OKlabelfont1",p:=UpdateLabelFont},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="cancellabelfont1",p:=ABoxCancel},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="setlabelfont1",p:=ABoxPopup},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="OKtitlefont1",p:=UpdateTitleFont},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="OKnotesfont1",p:=UpdateNotesFont},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="OKlegendfont1",p:=UpdateLegendFont},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="canceltitlefont1",p:=GBoxCancel},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="cancelnotesfont1",p:=GBoxCancel},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="cancellegendfont1",p:=ABoxCancel},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="settitlefont1",p:=GBoxPopup},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="setnotesfont1",p:=GBoxPopup},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="setlegendfont1",p:=ABoxPopup},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="graphtitle1",p:=GraphTitle},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="legend1",p:=AxisLegend},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="okinterpolopts1",p:=GraphUI.BoxCancel},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="okintegrateopts1",p:=GraphUI.BoxCancel},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="okdiffopts1",p:=GraphUI.BoxCancel},pl);
  pl := AttachProcList.Cons(AttachProc.T{VBTname:="okrootfindopts1",p:=GraphUI.BoxCancel},pl);

  pl := AttachProcList.Cons(AttachProc.T{VBTname:="legend1",p:=AxisLegend},pl);

  al := AnalOptList.Cons(AnalOpt.T{desc:="Interpolate",VBTname:="interpolopts1",proc:=TwoDInterpolate.Master},al);
  al := AnalOptList.Cons(AnalOpt.T{desc:="Quadrature",VBTname:="integrateopts1",proc:=TwoDIntegrate.Master},al);
  al := AnalOptList.Cons(AnalOpt.T{desc:="Differentiate",VBTname:="diffopts1",proc:=TwoDDifferentiate.Master},al);
  al := AnalOptList.Cons(AnalOpt.T{desc:="Find Roots",VBTname:="rootfindopts1",proc:=TwoDRootFind.Master},al);

  (* the thing to return *)
  RETURN GraphType.T{desc:="2D Cartesian",proc:=Init,printproc:=Print,axisoptsVBTname:="axisoptions1",graphoptsVBTname:="graphoptions1",newoptions:=NewBlock,proclist:=pl,analoptlist:=al};
END MakeListEntry;

PROCEDURE NewBlock() :REFANY =
  VAR
    b := NEW(OptBlock);
BEGIN
  b.axes[Axis.T.Hor].datacolumn := 1;   (* map the 0 column to the abscissa *)
  RETURN b;                             (* 1 column is mapped to the ordinate *)
END NewBlock;

PROCEDURE Init(self: DisplayGraph.T; g: Graph.T): DisplayGraph.T =
VAR
  origin, p, q, labelp, offset :Point.T;
  n :Note.T;
  max, min, range, zeropoint, scale :ARRAY Axis.T OF Data.Element;
  axes, ticks :ARRAY Axis.T OF Path.T;
  t, count, b, step :Data.Element;
  tempf :Font.T;
  data :Data.T;
  s, h1, h2   :INTEGER;
  a := NEW(REF ARRAY OF Data.Element,1);
  datacol :ARRAY Axis.T OF INTEGER;
BEGIN

  WITH opts = NARROW(self.graphopts,OptBlock) DO

    IF NOT(opts^.showplot) THEN
      (* check to see if we have an approximation *)
      IF g.approx = NIL THEN
        g.approx:=Approximation.Create("2dlagrange",g.graphdata.data,opts^.axes[Axis.T.Hor].datacolumn,opts^.axes[Axis.T.Ver].datacolumn);
      END;

      (* put the approximation data into the data *)
      s := FormsVBT.GetInteger(g,"approxpoints1");
      data := NEW(Data.T,records:=s+1,fields:=2);
      data.values := NEW(REF ARRAY OF Data.Element,(s+1)*2);
      a[0] := g.graphdata.data.min(opts^.axes[Axis.T.Hor].datacolumn);
      b := g.graphdata.data.max(opts^.axes[Axis.T.Hor].datacolumn);
      step := (b-a[0])/FLOAT(s, Data.Element);

      FOR i := 0 TO s DO
        IF i = s THEN
          a[0] := b;
        END;
        data.values[i*2+1] := a[0]; 
        data.values[i*2] := g.eval(a).yval;
        a[0] := a[0] + step;
      END;

      (* reset the data column mappings to 0 and 1 for y and x *)
      datacol[Axis.T.Hor] := 1;
      datacol[Axis.T.Ver] := 0;

    ELSE
      data := g.graphdata.data;      
      datacol[Axis.T.Ver] := opts^.axes[Axis.T.Ver].datacolumn;
      datacol[Axis.T.Hor] := opts^.axes[Axis.T.Hor].datacolumn;
    END;

    (* work out where the origin is going to be *)
    FOR i := Axis.T.Hor TO Axis.T.Ver DO
      max[i] := data.max(datacol[i]);
      min[i] := data.min(datacol[i]);
      IF max[i] = min[i] AND max[i] = 0.0d0 THEN 
        max[i] := 1.0d0;
        min[i] := -1.0d0;
      ELSIF max[i] = min[i] THEN
        max[i] := max[i] + max[i]/2.0d0;
        min[i] := min[i] - min[i]/2.0d0;
      END;
    END;
    scale[Axis.T.Hor] := FLOAT(self.horizsize-self.border*2, Data.Element)/(max[Axis.T.Hor]-min[Axis.T.Hor]);
    scale[Axis.T.Ver] := FLOAT(self.vertsize-self.border*2, Data.Element)/(max[Axis.T.Ver]-min[Axis.T.Ver]);

    (* ok now the min and max arrays contain the min and max values of
       each column of the data *)
    FOR i := Axis.T.Hor TO Axis.T.Ver DO
      range[i] := max[i] - min[i];
      (* 3 cases: max and min both positive, or max positive min negative,
         or max and min both negative *)
      IF min[i] >= 0.0d0 THEN  (* both positive *)
        zeropoint[i] := FLOAT(self.border, Data.Element)-(min[i]*scale[i]);
      ELSIF max[i] <= 0.0d0 THEN (* both negative *)
        IF Axis.Equal(i,Axis.T.Ver) THEN
          zeropoint[i] := FLOAT(self.vertsize-self.border, Data.Element)-(max[i]*scale[i]);
        ELSE
          zeropoint[i] := FLOAT(self.horizsize-self.border, Data.Element)-(max[i]*scale[i]);
        END;
      ELSE (* max pos and min neg *)
        zeropoint[i] := FLOAT(self.border, Data.Element)-(min[i]*scale[i]);
      END;
    END;
 
    origin := Point.FromCoords(ROUND(zeropoint[Axis.T.Hor]), self.vertsize-ROUND(zeropoint[Axis.T.Ver]));
    offset := Point.FromCoords(0,0);

    IF origin.h < self.border THEN
      offset.h := self.border-origin.h;
    ELSIF origin.h > self.horizsize-self.border THEN
      offset.h := self.horizsize-self.border-origin.h;
    END;
    IF origin.v < self.border THEN
      offset.v := self.border-origin.v;
    ELSIF origin.v > self.vertsize-self.border THEN
      offset.v := self.vertsize-self.border-origin.v;
    END;

    (* build the graph path *)
    self.graphpath := NEW(Path.T);

    p := Point.FromCoords(origin.h+ROUND(data.values^[datacol[Axis.T.Hor]]*scale[Axis.T.Hor]), origin.v-ROUND(data.values^[datacol[Axis.T.Ver]]*scale[Axis.T.Ver]));   
    FOR i := 1 TO data.records-1 DO
      q := Point.FromCoords(origin.h+ROUND(data.values^[i*data.fields+datacol[Axis.T.Hor]]*scale[Axis.T.Hor]), origin.v-ROUND(data.values^[i*data.fields+datacol[Axis.T.Ver]]*scale[Axis.T.Ver]));
      Path.MoveTo(self.graphpath,p);
      Path.LineTo(self.graphpath,q);
      p := q;
    END;

    (* add the point markers *)
    self.points := NEW(PointList.T);
    IF opts^.showmarkers THEN
      FOR i := 0 TO g.graphdata.data.records-1 DO
        q := Point.FromCoords(origin.h+ROUND(g.graphdata.data.values^[i*g.graphdata.data.fields+datacol[Axis.T.Hor]]*scale[Axis.T.Hor]), origin.v-ROUND(g.graphdata.data.values^[i*g.graphdata.data.fields+datacol[Axis.T.Ver]]*scale[Axis.T.Ver]));
        self.points := PointList.Cons(q,self.points);
      END;
    END;

    (* build the graph axis and ticks and grid paths *)
    axes[Axis.T.Hor] := NEW(Path.T);
    axes[Axis.T.Ver] := NEW(Path.T);
    ticks[Axis.T.Hor] := NEW(Path.T);
    ticks[Axis.T.Ver] := NEW(Path.T);
    self.gridpath := NEW(Path.T);

    (* ordinate *)
    p := Point.FromCoords(self.border, origin.v+offset.v);
    q := Point.FromCoords(self.horizsize-self.border, origin.v+offset.v);
    Path.MoveTo(axes[Axis.T.Hor], p);
    Path.LineTo(axes[Axis.T.Hor], q);

    (* do the grid *)
    IF opts^.showgrid THEN      
      h1 := origin.v + offset.v;
      WHILE h1 <= self.vertsize-self.border DO
        h2 := origin.h + offset.h;
        WHILE h2 <= self.horizsize-self.border DO
          Path.MoveTo(self.gridpath,Point.FromCoords(h2,h1));
          Path.LineTo(self.gridpath,Point.FromCoords(h2,h1));
          h2 := h2 + opts^.gridspacing; 
        END;
        h2 := origin.h + offset.h;
        WHILE h2 >= self.border DO
          Path.MoveTo(self.gridpath,Point.FromCoords(h2,h1));
          Path.LineTo(self.gridpath,Point.FromCoords(h2,h1));
          h2 := h2 - opts^.gridspacing; 
        END;
        h1 := h1 + opts^.gridspacing;
      END;
      h1 := origin.v + offset.v;
      WHILE h1 >= self.border DO
        h2 := origin.h + offset.h;
        WHILE h2 <= self.horizsize-self.border DO
          Path.MoveTo(self.gridpath,Point.FromCoords(h2,h1));
          Path.LineTo(self.gridpath,Point.FromCoords(h2,h1));
          h2 := h2 + opts^.gridspacing; 
        END;
        h2 := origin.h + offset.h;
        WHILE h2 >= self.border DO
          Path.MoveTo(self.gridpath,Point.FromCoords(h2,h1));
          Path.LineTo(self.gridpath,Point.FromCoords(h2,h1));
          h2 := h2 - opts^.gridspacing; 
        END;
        h1 := h1 - opts^.gridspacing;
      END;
    END;

    (* abscissa *)
    p := Point.FromCoords(origin.h+offset.h, self.border);
    q := Point.FromCoords(origin.h+offset.h, self.vertsize-self.border);
    Path.MoveTo(axes[Axis.T.Ver], p);
    Path.LineTo(axes[Axis.T.Ver], q);

    self.labels := NIL;
    tempf := Font.FromName(ARRAY OF TEXT{self.labelfont});
    (* axis ticks and labels *)
    FOR i := Axis.T.Hor TO Axis.T.Ver DO
      t := FLOAT(opts^.axes[i].tickspacing, Data.Element)/scale[i];
      IF t > 0.0d0 THEN
        count := FLOAT(FLOOR(max[i]/t), Data.Element)*t;
        WHILE count >= min[i] DO
          IF Axis.Equal(i,Axis.T.Hor) THEN
            p := Point.FromCoords(origin.h+ROUND(count*scale[i]),origin.v+offset.v);
            q := Point.FromCoords(p.h,p.v+opts^.axes[i].ticksize*opts^.axes[i].reverse);
            IF opts^.axes[i].reverse = 1 THEN
              labelp := Point.FromCoords(q.h - Rect.HorSize(VBT.BoundingBox(self,Data.FmtEl(count, Fmt.Style.Fix, 2),tempf)) DIV 2,p.v+opts^.axes[i].ticksize+Rect.VerSize(VBT.BoundingBox(self,Data.FmtEl(count, Fmt.Style.Fix, 2),tempf)));
            ELSE
              labelp := Point.FromCoords(q.h - Rect.HorSize(VBT.BoundingBox(self,Data.FmtEl(count, Fmt.Style.Fix, 2),tempf)) DIV 2,p.v-opts^.axes[i].ticksize);
            END;
          ELSE
            p := Point.FromCoords(origin.h+offset.h,origin.v-ROUND(count*scale[i]));
            q := Point.FromCoords(p.h-opts^.axes[i].ticksize*opts^.axes[i].reverse,p.v);   
            IF opts^.axes[i].reverse = 1 THEN
              labelp := Point.FromCoords(q.h - Rect.HorSize(VBT.BoundingBox(self,Data.FmtEl(count, Fmt.Style.Fix, 2),tempf)),q.v + Rect.VerSize(VBT.BoundingBox(self,Data.FmtEl(count, Fmt.Style.Fix, 2),tempf)) DIV 2); 
            ELSE
              labelp := Point.FromCoords(q.h,q.v + Rect.VerSize(VBT.BoundingBox(self,Data.FmtEl(count, Fmt.Style.Fix, 2),tempf)) DIV 2); 
            END;       
          END;
          Path.MoveTo(ticks[i],p);
          Path.LineTo(ticks[i],q);
          n := NEW(Note.T, t:=Data.FmtEl(count, Fmt.Style.Fix, 2), f:=tempf, p:=labelp);
          IF opts^.axes[i].ticknoteson THEN
            self.labels := NoteList.Cons(n,self.labels);
          END;
          count := count - t;
        END;
      END;
    END;

   (* text bits *)
    self.titles := NIL;
    tempf := Font.FromName(ARRAY OF TEXT{self.titlefont});
    q := Point.FromCoords((self.horizsize - Rect.HorSize(VBT.BoundingBox(self,opts^.title,tempf))) DIV 2, self.border DIV 2);
    n := NEW(Note.T, t:=opts^.title, f:=tempf, p:=q);
    self.titles := NoteList.Cons(n,self.titles); 
    self.notes := NIL;
    p := Point.FromCoords(100,100);
    q := Point.FromCoords(100,300);
(*    n := NEW(Note.T, t:="Hello", f:=Font.FromName(ARRAY OF TEXT{self.notesfont}), p:=q);
    self.notes := NoteList.Cons(n,self.notes);
*)
    self.axespaths := NEW(REF ARRAY OF Path.T, 2);
    IF opts^.axes[Axis.T.Hor].on THEN
      self.axespaths^[0] := axes[Axis.T.Hor];
    ELSE
      self.axespaths^[0] := NEW(Path.T);
    END;
    IF opts^.axes[Axis.T.Ver].on THEN
      self.axespaths^[1] := axes[Axis.T.Ver];
    ELSE
      self.axespaths^[1] := NEW(Path.T);
    END;

    self.tickspaths := NEW(REF ARRAY OF Path.T, 2);
    IF opts^.axes[Axis.T.Hor].tickson THEN
      self.tickspaths^[0] := ticks[Axis.T.Hor];
    ELSE
      self.tickspaths^[0] := NEW(Path.T);
    END;
    IF opts^.axes[Axis.T.Ver].tickson THEN
      self.tickspaths^[1] := ticks[Axis.T.Ver];
    ELSE
      self.tickspaths^[1] := NEW(Path.T);
    END;

  END;

  RETURN self;
END Init;

PROCEDURE Print(form: FormsVBT.T; filename :TEXT) RAISES {DisplayGraph.FileError} =
VAR
  origin, p, q, labelp, offset :Point.T;
  n :Note.T;
  max, min, range, zeropoint, scale :ARRAY Axis.T OF Data.Element;
  axes, ticks :ARRAY Axis.T OF Path.T;
  t, count, b, step :Data.Element;
  tempf :Font.T;
  data :Data.T;
  s, h1, h2   :INTEGER;
  a := NEW(REF ARRAY OF Data.Element,1);
  datacol :ARRAY Axis.T OF INTEGER;
  file :Wr.T;
BEGIN
  RAISE DisplayGraph.FileError("Not Implemented Yet!");
  (* open the file for writing *)

  WITH g = NARROW(form, Graph.T) DO
    WITH self = GetDisplay(form) DO
      WITH opts = NARROW(self.graphopts,OptBlock) DO

  file := IO.OpenWrite(filename);
  IF file = NIL THEN
    RAISE DisplayGraph.FileError("Error Opening File*" & filename);
  END;

  Wr.PutText(file,"%!PS-Adobe-1.0\n");

  WITH opts = NARROW(self.graphopts,OptBlock) DO

    IF NOT(opts^.showplot) THEN
      (* check to see if we have an approximation *)
      IF g.approx = NIL THEN
        g.approx:=Approximation.Create("2dlagrange",g.graphdata.data,opts^.axes[Axis.T.Hor].datacolumn,opts^.axes[Axis.T.Ver].datacolumn);
      END;

      (* put the approximation data into the data *)
      s := FormsVBT.GetInteger(g,"approxpoints1");
      data := NEW(Data.T,records:=s+1,fields:=2);
      data.values := NEW(REF ARRAY OF Data.Element,(s+1)*2);
      a[0] := g.graphdata.data.min(opts^.axes[Axis.T.Hor].datacolumn);
      b := g.graphdata.data.max(opts^.axes[Axis.T.Hor].datacolumn);
      step := (b-a[0])/FLOAT(s, Data.Element);

      FOR i := 0 TO s DO
        IF i = s THEN
          a[0] := b;
        END;
        data.values[i*2+1] := a[0]; 
        data.values[i*2] := g.eval(a).yval;
        a[0] := a[0] + step;
      END;

      (* reset the data column mappings to 0 and 1 for y and x *)
      datacol[Axis.T.Hor] := 1;
      datacol[Axis.T.Ver] := 0;

    ELSE
      data := g.graphdata.data;      
      datacol[Axis.T.Ver] := opts^.axes[Axis.T.Ver].datacolumn;
      datacol[Axis.T.Hor] := opts^.axes[Axis.T.Hor].datacolumn;
    END;

    (* work out where the origin is going to be *)
    FOR i := Axis.T.Hor TO Axis.T.Ver DO
      max[i] := data.max(datacol[i]);
      min[i] := data.min(datacol[i]);
      IF max[i] = min[i] AND max[i] = 0.0d0 THEN 
        max[i] := 1.0d0;
        min[i] := -1.0d0;
      ELSIF max[i] = min[i] THEN
        max[i] := max[i] + max[i]/2.0d0;
        min[i] := min[i] - min[i]/2.0d0;
      END;
    END;
    scale[Axis.T.Hor] := FLOAT(self.horizsize-self.border*2, Data.Element)/(max[Axis.T.Hor]-min[Axis.T.Hor]);
    scale[Axis.T.Ver] := FLOAT(self.vertsize-self.border*2, Data.Element)/(max[Axis.T.Ver]-min[Axis.T.Ver]);

    (* ok now the min and max arrays contain the min and max values of
       each column of the data *)
    FOR i := Axis.T.Hor TO Axis.T.Ver DO
      range[i] := max[i] - min[i];
      (* 3 cases: max and min both positive, or max positive min negative,
         or max and min both negative *)
      IF min[i] >= 0.0d0 THEN  (* both positive *)
        zeropoint[i] := FLOAT(self.border, Data.Element)-(min[i]*scale[i]);
      ELSIF max[i] <= 0.0d0 THEN (* both negative *)
        IF Axis.Equal(i,Axis.T.Ver) THEN
          zeropoint[i] := FLOAT(self.vertsize-self.border, Data.Element)-(max[i]*scale[i]);
        ELSE
          zeropoint[i] := FLOAT(self.horizsize-self.border, Data.Element)-(max[i]*scale[i]);
        END;
      ELSE (* max pos and min neg *)
        zeropoint[i] := FLOAT(self.border, Data.Element)-(min[i]*scale[i]);
      END;
    END;
 
    origin := Point.FromCoords(ROUND(zeropoint[Axis.T.Hor]), self.vertsize-ROUND(zeropoint[Axis.T.Ver]));
    offset := Point.FromCoords(0,0);

    IF origin.h < self.border THEN
      offset.h := self.border-origin.h;
    ELSIF origin.h > self.horizsize-self.border THEN
      offset.h := self.horizsize-self.border-origin.h;
    END;
    IF origin.v < self.border THEN
      offset.v := self.border-origin.v;
    ELSIF origin.v > self.vertsize-self.border THEN
      offset.v := self.vertsize-self.border-origin.v;
    END;

    (* build the graph path *)
    self.graphpath := NEW(Path.T);

    p := Point.FromCoords(origin.h+ROUND(data.values^[datacol[Axis.T.Hor]]*scale[Axis.T.Hor]), origin.v-ROUND(data.values^[datacol[Axis.T.Ver]]*scale[Axis.T.Ver]));   
    FOR i := 1 TO data.records-1 DO
      q := Point.FromCoords(origin.h+ROUND(data.values^[i*data.fields+datacol[Axis.T.Hor]]*scale[Axis.T.Hor]), origin.v-ROUND(data.values^[i*data.fields+datacol[Axis.T.Ver]]*scale[Axis.T.Ver]));
      Path.MoveTo(self.graphpath,p);
      Path.LineTo(self.graphpath,q);
      p := q;
    END;

    (* add the point markers *)
    IF opts^.showmarkers THEN
      FOR i := 0 TO g.graphdata.data.records-1 DO
        q := Point.FromCoords(origin.h+ROUND(g.graphdata.data.values^[i*g.graphdata.data.fields+datacol[Axis.T.Hor]]*scale[Axis.T.Hor]), origin.v-ROUND(g.graphdata.data.values^[i*g.graphdata.data.fields+datacol[Axis.T.Ver]]*scale[Axis.T.Ver]));
        self.points := PointList.Cons(q,self.points);
      END;
    END;

    (* build the graph axis and ticks and grid paths *)
    axes[Axis.T.Hor] := NEW(Path.T);
    axes[Axis.T.Ver] := NEW(Path.T);
    ticks[Axis.T.Hor] := NEW(Path.T);
    ticks[Axis.T.Ver] := NEW(Path.T);
    self.gridpath := NEW(Path.T);

    (* ordinate *)
    p := Point.FromCoords(self.border, origin.v+offset.v);
    q := Point.FromCoords(self.horizsize-self.border, origin.v+offset.v);
    Path.MoveTo(axes[Axis.T.Hor], p);
    Path.LineTo(axes[Axis.T.Hor], q);

    (* do the horizontal grid *)
    IF opts^.showgrid THEN      
      h1 := origin.v + offset.v;
      WHILE h1 <= self.vertsize-self.border DO
        h2 := origin.h + offset.h;
        WHILE h2 <= self.horizsize-self.border DO
          Path.MoveTo(self.gridpath,Point.FromCoords(h2,h1));
          Path.LineTo(self.gridpath,Point.FromCoords(h2,h1));
          h2 := h2 + opts^.gridspacing; 
        END;
        h2 := origin.h + offset.h;
        WHILE h2 >= self.border DO
          Path.MoveTo(self.gridpath,Point.FromCoords(h2,h1));
          Path.LineTo(self.gridpath,Point.FromCoords(h2,h1));
          h2 := h2 - opts^.gridspacing; 
        END;
        h1 := h1 + opts^.gridspacing;
      END;
      h1 := origin.v + offset.v;
      WHILE h1 >= self.border DO
        h2 := origin.h + offset.h;
        WHILE h2 <= self.horizsize-self.border DO
          Path.MoveTo(self.gridpath,Point.FromCoords(h2,h1));
          Path.LineTo(self.gridpath,Point.FromCoords(h2,h1));
          h2 := h2 + opts^.gridspacing; 
        END;
        h2 := origin.h + offset.h;
        WHILE h2 >= self.border DO
          Path.MoveTo(self.gridpath,Point.FromCoords(h2,h1));
          Path.LineTo(self.gridpath,Point.FromCoords(h2,h1));
          h2 := h2 - opts^.gridspacing; 
        END;
        h1 := h1 - opts^.gridspacing;
      END;
    END;

    (* abscissa *)
    p := Point.FromCoords(origin.h+offset.h, self.border);
    q := Point.FromCoords(origin.h+offset.h, self.vertsize-self.border);
    Path.MoveTo(axes[Axis.T.Ver], p);
    Path.LineTo(axes[Axis.T.Ver], q);

    self.labels := NIL;
    tempf := Font.FromName(ARRAY OF TEXT{self.labelfont});
    (* axis ticks and labels *)
    FOR i := Axis.T.Hor TO Axis.T.Ver DO
      t := FLOAT(opts^.axes[i].tickspacing, Data.Element)/scale[i];
      IF t > 0.0d0 THEN
        count := FLOAT(FLOOR(max[i]/t), Data.Element)*t;
        WHILE count >= min[i] DO
          IF Axis.Equal(i,Axis.T.Hor) THEN
            p := Point.FromCoords(origin.h+ROUND(count*scale[i]),origin.v+offset.v);
            q := Point.FromCoords(p.h,p.v+opts^.axes[i].ticksize*opts^.axes[i].reverse);
            IF opts^.axes[i].reverse = 1 THEN
              labelp := Point.FromCoords(q.h - Rect.HorSize(VBT.BoundingBox(self,Data.FmtEl(count, Fmt.Style.Fix, 2),tempf)) DIV 2,p.v+opts^.axes[i].ticksize+Rect.VerSize(VBT.BoundingBox(self,Data.FmtEl(count, Fmt.Style.Fix, 2),tempf)));
            ELSE
              labelp := Point.FromCoords(q.h - Rect.HorSize(VBT.BoundingBox(self,Data.FmtEl(count, Fmt.Style.Fix, 2),tempf)) DIV 2,p.v-opts^.axes[i].ticksize);
            END;
          ELSE
            p := Point.FromCoords(origin.h+offset.h,origin.v-ROUND(count*scale[i]));
            q := Point.FromCoords(p.h-opts^.axes[i].ticksize*opts^.axes[i].reverse,p.v);   
            IF opts^.axes[i].reverse = 1 THEN
              labelp := Point.FromCoords(q.h - Rect.HorSize(VBT.BoundingBox(self,Data.FmtEl(count, Fmt.Style.Fix, 2),tempf)),q.v + Rect.VerSize(VBT.BoundingBox(self,Data.FmtEl(count, Fmt.Style.Fix, 2),tempf)) DIV 2); 
            ELSE
              labelp := Point.FromCoords(q.h,q.v + Rect.VerSize(VBT.BoundingBox(self,Data.FmtEl(count, Fmt.Style.Fix, 2),tempf)) DIV 2); 
            END;       
          END;
          Path.MoveTo(ticks[i],p);
          Path.LineTo(ticks[i],q);
          n := NEW(Note.T, t:=Data.FmtEl(count, Fmt.Style.Fix, 2), f:=tempf, p:=labelp);
          IF opts^.axes[i].ticknoteson THEN
            self.labels := NoteList.Cons(n,self.labels);
          END;
          count := count - t;
        END;
      END;
    END;

   (* text bits *)
    self.titles := NIL;
    tempf := Font.FromName(ARRAY OF TEXT{self.titlefont});
    q := Point.FromCoords((self.horizsize - Rect.HorSize(VBT.BoundingBox(self,opts^.title,tempf))) DIV 2, self.border DIV 2);
    n := NEW(Note.T, t:=opts^.title, f:=tempf, p:=q);
    self.titles := NoteList.Cons(n,self.titles); 
    self.notes := NIL;
    p := Point.FromCoords(100,100);
    q := Point.FromCoords(100,300);
    n := NEW(Note.T, t:="Hello", f:=Font.FromName(ARRAY OF TEXT{self.notesfont}), p:=q);
    self.notes := NoteList.Cons(n,self.notes);

    self.axespaths := NEW(REF ARRAY OF Path.T, 2);
    IF opts^.axes[Axis.T.Hor].on THEN
      self.axespaths^[0] := axes[Axis.T.Hor];
    ELSE
      self.axespaths^[0] := NEW(Path.T);
    END;
    IF opts^.axes[Axis.T.Ver].on THEN
      self.axespaths^[1] := axes[Axis.T.Ver];
    ELSE
      self.axespaths^[1] := NEW(Path.T);
    END;

    self.tickspaths := NEW(REF ARRAY OF Path.T, 2);
    IF opts^.axes[Axis.T.Hor].tickson THEN
      self.tickspaths^[0] := ticks[Axis.T.Hor];
    ELSE
      self.tickspaths^[0] := NEW(Path.T);
    END;
    IF opts^.axes[Axis.T.Ver].tickson THEN
      self.tickspaths^[1] := ticks[Axis.T.Ver];
    ELSE
      self.tickspaths^[1] := NEW(Path.T);
    END;

  END;
  Wr.Close(file);
      END;
    END;
  END;

END Print;

(* the procedures handling the callbacks *)

PROCEDURE PlotToggle(             form :FormsVBT.T;
                     <* UNUSED *> event:Text.T;
                     <* UNUSED *> cl   :REFANY;
                     <* UNUSED *> ts   :VBT.TimeStamp)=
BEGIN
  WITH f = GetDisplay(form) DO
    WITH opts = NARROW(f.graphopts,OptBlock) DO
      opts^.showplot := Text.Equal(FormsVBT.GetChoice(form,"showplot1"),"showexact1");
    END;
  END;
END PlotToggle;

PROCEDURE MarkerToggle(             form :FormsVBT.T;
                       <* UNUSED *> event:Text.T;
                       <* UNUSED *> cl   :REFANY;
                       <* UNUSED *> ts   :VBT.TimeStamp)=
BEGIN
  WITH f = GetDisplay(form) DO
    WITH opts = NARROW(f.graphopts,OptBlock) DO
      opts^.showmarkers := FormsVBT.GetBoolean(form,"showmarkers1");
    END;
  END;
END MarkerToggle;

PROCEDURE GridToggle(             form :FormsVBT.T;
                     <* UNUSED *> event:Text.T;
                     <* UNUSED *> cl   :REFANY;
                     <* UNUSED *> ts   :VBT.TimeStamp)=
BEGIN
  WITH f = GetDisplay(form) DO
    WITH opts = NARROW(f.graphopts,OptBlock) DO
      opts^.showgrid := FormsVBT.GetBoolean(form,"showgrid1");
    END;
  END;
END GridToggle;

PROCEDURE GridSpace(             form :FormsVBT.T;
                    <* UNUSED *> event:Text.T;
                    <* UNUSED *> cl   :REFANY;
                    <* UNUSED *> ts   :VBT.TimeStamp)=
BEGIN
  WITH f = GetDisplay(form) DO
    WITH opts = NARROW(f.graphopts,OptBlock) DO
      opts^.gridspacing := FormsVBT.GetInteger(form,"gridspace1");
    END;
  END;
END GridSpace;

PROCEDURE SetHeight(             form :FormsVBT.T;
                    <* UNUSED *> event:Text.T;
                    <* UNUSED *> cl   :REFANY;
                    <* UNUSED *> ts   :VBT.TimeStamp)=
BEGIN
  WITH f = GetDisplay(form) DO
    f.vertsize := FormsVBT.GetInteger(form,"vertsize1");
    f.sizechanged := TRUE;
  END;
END SetHeight;

PROCEDURE SetWidth(             form :FormsVBT.T;
                   <* UNUSED *> event:Text.T;
                   <* UNUSED *> cl   :REFANY;
                   <* UNUSED *> ts   :VBT.TimeStamp)=
BEGIN
  WITH f = GetDisplay(form) DO
    f.horizsize := FormsVBT.GetInteger(form,"horizsize1");
    f.sizechanged := TRUE;
  END;
END SetWidth;

PROCEDURE SetBorder(             form :FormsVBT.T;
                    <* UNUSED *> event:Text.T;
                    <* UNUSED *> cl   :REFANY;
                    <* UNUSED *> ts   :VBT.TimeStamp)=
BEGIN
  WITH f = GetDisplay(form) DO
    f.border := FormsVBT.GetInteger(form,"border1");
  END;
END SetBorder;

PROCEDURE SetAxis(             form :FormsVBT.T;
                  <* UNUSED *> event:Text.T;
                  <* UNUSED *> cl   :REFANY;
                  <* UNUSED *> ts   :VBT.TimeStamp)=
VAR
  t :TEXT;
BEGIN
  WITH f = GetDisplay(form) DO
    WITH opts = NARROW(f.graphopts,OptBlock) DO
      t := FormsVBT.GetChoice(form,"axischoice1");
       IF Text.Equal(t,"horizaxis1") THEN
        opts^.a := Axis.T.Hor;
      ELSE
        opts^.a := Axis.T.Ver;
      END;
      FormsVBT.PutBoolean(form,"showaxis1",opts^.axes[opts^.a].on);
      FormsVBT.PutInteger(form,"maptocol1",opts^.axes[opts^.a].datacolumn);
      FormsVBT.PutBoolean(form,"showticks1",opts^.axes[opts^.a].tickson);
      FormsVBT.PutBoolean(form,"showlabels1",opts^.axes[opts^.a].ticknoteson);
      FormsVBT.PutInteger(form,"ticksize1",opts^.axes[opts^.a].ticksize);
      FormsVBT.PutInteger(form,"tickspace1",opts^.axes[opts^.a].tickspacing);
      FormsVBT.PutBoolean(form,"reverseticks1",opts^.axes[opts^.a].reverse=-1);
      FormsVBT.PutText(form,"legend1",opts^.axes[opts^.a].legend);
    END;
  END;
END SetAxis;

PROCEDURE AxisMap(             form :FormsVBT.T;
                  <* UNUSED *> event:Text.T;
                  <* UNUSED *> cl   :REFANY;
                  <* UNUSED *> ts   :VBT.TimeStamp)=
VAR
  i :INTEGER;
  g :Graph.T;
BEGIN
  g := NARROW(form, Graph.T);
  WITH f = GetDisplay(form) DO
    WITH opts = NARROW(f.graphopts,OptBlock) DO
      i := FormsVBT.GetInteger(form,"maptocol1");
      IF i > g.graphdata.data.fields-1 THEN
        i := g.graphdata.data.fields-1;
        FormsVBT.PutInteger(form,"maptocol1",i);
      END;
      opts^.axes[opts^.a].datacolumn := i;
    END;
  END;
END AxisMap;

PROCEDURE AxisToggle(             form :FormsVBT.T;
                     <* UNUSED *> event:Text.T;
                     <* UNUSED *> cl   :REFANY;
                     <* UNUSED *> ts   :VBT.TimeStamp)=
BEGIN
  WITH f = GetDisplay(form) DO
    WITH opts = NARROW(f.graphopts,OptBlock) DO
      opts^.axes[opts^.a].on := FormsVBT.GetBoolean(form,"showaxis1");
    END;
  END;
END AxisToggle;

PROCEDURE TickToggle(             form :FormsVBT.T;
                     <* UNUSED *> event:Text.T;
                     <* UNUSED *> cl   :REFANY;
                     <* UNUSED *> ts   :VBT.TimeStamp)=
BEGIN
  WITH f = GetDisplay(form) DO
    WITH opts = NARROW(f.graphopts,OptBlock) DO
      opts^.axes[opts^.a].tickson := FormsVBT.GetBoolean(form,"showticks1");
    END;
  END;
END TickToggle;

PROCEDURE LabelToggle(             form :FormsVBT.T;
                     <* UNUSED *> event:Text.T;
                     <* UNUSED *> cl   :REFANY;
                     <* UNUSED *> ts   :VBT.TimeStamp)=
BEGIN
  WITH f = GetDisplay(form) DO
    WITH opts = NARROW(f.graphopts,OptBlock) DO
      opts^.axes[opts^.a].ticknoteson := FormsVBT.GetBoolean(form,"showlabels1");
    END;
  END;
END LabelToggle;

PROCEDURE ReverseTicks(             form :FormsVBT.T;
                       <* UNUSED *> event:Text.T;
                       <* UNUSED *> cl   :REFANY;
                       <* UNUSED *> ts   :VBT.TimeStamp)=
BEGIN
  WITH f = GetDisplay(form) DO
    WITH opts = NARROW(f.graphopts,OptBlock) DO
      IF FormsVBT.GetBoolean(form,"reverseticks1") THEN
        opts^.axes[opts^.a].reverse := -1;
      ELSE
        opts^.axes[opts^.a].reverse := 1;
      END;
    END;
  END;
END ReverseTicks;

PROCEDURE TickSize(             form :FormsVBT.T;
                   <* UNUSED *> event:Text.T;
                   <* UNUSED *> cl   :REFANY;
                   <* UNUSED *> ts   :VBT.TimeStamp)=
BEGIN
  WITH f = GetDisplay(form) DO
    WITH opts = NARROW(f.graphopts,OptBlock) DO
      opts^.axes[opts^.a].ticksize := FormsVBT.GetInteger(form,"ticksize1");
    END;
  END;
END TickSize;

PROCEDURE TickSpace(             form :FormsVBT.T;
                    <* UNUSED *> event:Text.T;
                    <* UNUSED *> cl   :REFANY;
                    <* UNUSED *> ts   :VBT.TimeStamp)=
BEGIN
  WITH f = GetDisplay(form) DO
    WITH opts = NARROW(f.graphopts,OptBlock) DO
      opts^.axes[opts^.a].tickspacing := FormsVBT.GetInteger(form,"tickspace1");
    END;
  END;
END TickSpace;

PROCEDURE GraphTitle(             form :FormsVBT.T;
                     <* UNUSED *> event:Text.T;
                     <* UNUSED *> cl   :REFANY;
                     <* UNUSED *> ts   :VBT.TimeStamp)=
BEGIN
  WITH f = GetDisplay(form) DO
    WITH opts = NARROW(f.graphopts,OptBlock) DO
      opts^.title := FormsVBT.GetText(form,"graphtitle1");
    END;
  END;
END GraphTitle;

PROCEDURE AxisLegend(             form :FormsVBT.T;
                     <* UNUSED *> event:Text.T;
                     <* UNUSED *> cl   :REFANY;
                     <* UNUSED *> ts   :VBT.TimeStamp)=
BEGIN
  WITH f = GetDisplay(form) DO
    WITH opts = NARROW(f.graphopts,OptBlock) DO
      opts^.axes[opts^.a].legend := FormsVBT.GetText(form,"legend1");
    END;
  END;
END AxisLegend;

PROCEDURE GetDisplay(f :FormsVBT.T): DisplayGraph.T =
BEGIN
  (* yuk! this is way too horrible to think about! *)
  RETURN NARROW(FormsVBT.GetGeneric(NARROW(f,Graph.T).graph,"gengraph"),DisplayGraph.T);
END GetDisplay;

PROCEDURE UpdateTitleFont(             form :FormsVBT.T;
                          <* UNUSED *> event:Text.T;
                          <* UNUSED *> cl   :REFANY;
                          <* UNUSED *> ts   :VBT.TimeStamp) =
VAR
  family, weight, slant, size :TEXT;
BEGIN
  family := "-" & FormsVBT.GetText(form,"titlefamily1");
  weight := "-" & FormsVBT.GetText(form,"titleweight1");
  slant := "-" & FormsVBT.GetText(form,"titleslant1");
  IF Text.Equal(slant,"-italic") AND NOT(Text.Equal(family,"-times")) THEN
    slant := "-o";
  ELSIF Text.Equal(slant,"-italic") THEN
    slant := "-i";
  ELSE
    slant := "-r";
  END;
  size := "-" & FormsVBT.GetText(form,"titlesize1") & "0";
  WITH f = GetDisplay(form) DO
    f.titlefont := "-adobe" & family & weight & slant & "-*-*-*" & size & "-*-*-*-*-*-*";
  END;
  FormsVBT.MakeActive(form,"graphoptsfilter1");
END UpdateTitleFont;

PROCEDURE UpdateLabelFont(             form :FormsVBT.T;
                          <* UNUSED *> event:Text.T;
                          <* UNUSED *> cl   :REFANY;
                          <* UNUSED *> ts   :VBT.TimeStamp) =
VAR
  family, weight, slant, size :TEXT;
BEGIN
  family := "-" & FormsVBT.GetText(form,"labelfamily1");
  weight := "-" & FormsVBT.GetText(form,"labelweight1");
  slant := "-" & FormsVBT.GetText(form,"labelslant1");
  IF Text.Equal(slant,"-italic") AND NOT(Text.Equal(family,"-times")) THEN
    slant := "-o";
  ELSIF Text.Equal(slant,"-italic") THEN
    slant := "-i";
  ELSE
    slant := "-r";
  END;
  size := "-" & FormsVBT.GetText(form,"labelsize1") & "0";
  WITH f = GetDisplay(form) DO
    f.labelfont := "-adobe" & family & weight & slant & "-*-*-*" & size & "-*-*-*-*-*-*";
  END;  
  FormsVBT.MakeActive(form,"axisoptsfilter1");
END UpdateLabelFont;

PROCEDURE UpdateNotesFont(             form :FormsVBT.T;
                          <* UNUSED *> event:Text.T;
                          <* UNUSED *> cl   :REFANY;
                          <* UNUSED *> ts   :VBT.TimeStamp) =
VAR
  family, weight, slant, size :TEXT;
BEGIN
  family := "-" & FormsVBT.GetText(form,"notesfamily1");
  weight := "-" & FormsVBT.GetText(form,"notesweight1");
  slant := "-" & FormsVBT.GetText(form,"notesslant1");
  IF Text.Equal(slant,"-italic") AND NOT(Text.Equal(family,"-times")) THEN
    slant := "-o";
  ELSIF Text.Equal(slant,"-italic") THEN
    slant := "-i";
  ELSE
    slant := "-r";
  END;
  size := "-" & FormsVBT.GetText(form,"notessize1") & "0";
  WITH f = GetDisplay(form) DO
    f.notesfont := "-adobe" & family & weight & slant &  "-*-*-*" & size & "-*-*-*-*-*-*";
  END;  
  FormsVBT.MakeActive(form,"graphoptsfilter1");
END UpdateNotesFont;

PROCEDURE UpdateLegendFont(             form :FormsVBT.T;
                           <* UNUSED *> event:Text.T;
                           <* UNUSED *> cl   :REFANY;
                           <* UNUSED *> ts   :VBT.TimeStamp) =
VAR
  family, weight, slant, size :TEXT;
BEGIN
  family := "-" & FormsVBT.GetText(form,"legendfamily1");
  weight := "-" & FormsVBT.GetText(form,"legendweight1");
  slant := "-" & FormsVBT.GetText(form,"legendslant1");
  IF Text.Equal(slant,"-italic") AND NOT(Text.Equal(family,"-times")) THEN
    slant := "-o";
  ELSIF Text.Equal(slant,"-italic") THEN
    slant := "-i";
  ELSE
    slant := "-r";
  END;
  size := "-" & FormsVBT.GetText(form,"legendsize1") & "0";
  WITH f = GetDisplay(form) DO
    WITH opts = NARROW(f.graphopts,OptBlock) DO
      opts^.axes[opts^.a].legendfont := "-adobe" & family & weight & slant & "-*-*-*" & size & "-*-*-*-*-*-*";
    END;  
  END;
  FormsVBT.MakeActive(form,"axisoptsfilter1");
END UpdateLegendFont;

PROCEDURE GBoxCancel(             form :FormsVBT.T;
                    <* UNUSED *> event:Text.T;
                    <* UNUSED *> cl   :REFANY;
                    <* UNUSED *> ts   :VBT.TimeStamp) =
BEGIN
  
  FormsVBT.MakeActive(form,"graphoptsfilter1");
END GBoxCancel;

PROCEDURE GBoxPopup(             form :FormsVBT.T;
                   <* UNUSED *> event:Text.T;
                   <* UNUSED *> cl   :REFANY;
                   <* UNUSED *> ts   :VBT.TimeStamp) =
BEGIN
  FormsVBT.MakeDormant(form,"graphoptsfilter1");
END GBoxPopup;

PROCEDURE ABoxCancel(             form :FormsVBT.T;
                    <* UNUSED *> event:Text.T;
                    <* UNUSED *> cl   :REFANY;
                    <* UNUSED *> ts   :VBT.TimeStamp) =
BEGIN
  FormsVBT.MakeActive(form,"axisoptsfilter1");
END ABoxCancel;

PROCEDURE ABoxPopup(             form :FormsVBT.T;
                   <* UNUSED *> event:Text.T;
                   <* UNUSED *> cl   :REFANY;
                   <* UNUSED *> ts   :VBT.TimeStamp) =
BEGIN
  FormsVBT.MakeDormant(form,"axisoptsfilter1");
END ABoxPopup;

BEGIN
END TwoDCartesian.



