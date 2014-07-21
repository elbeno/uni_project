MODULE DisplayGraph;

IMPORT Graph, VBT, Path, PaintOp, Region, Point, NoteList, Note, Rect, VBTClass, PointList, FillRegionList;

REVEAL
  Private = VBT.Leaf BRANDED "PRIVDGRAPH" OBJECT END;
  T = Public BRANDED "PUBDGRAPH" OBJECT
        textstate :SelectState := SelectState.None;
        curtext   :Note.T;
        mousedown :Point.T;
        textselected :BOOLEAN := FALSE;
        hiddeninitproc :InitProc;
        oldtextregion :Region.T;
      OVERRIDES
        init := Init;
        repaint := Repaint;
        position := Position;
        mouse := Mouse;
      END;

TYPE
  SelectState = {Selected, Dragging, None};

VAR
  offset :Point.T;

PROCEDURE Init(self :T; g :Graph.T) :T =  (* changed g from a Data.T *)
BEGIN
  self := self.hiddeninitproc(self,g);
  RETURN self;
END Init;

PROCEDURE Repaint(self: T; READONLY rgn: Region.T) =
VAR
  n :NoteList.T;
  p :PointList.T;
  pp :Path.T;
  r :FillRegionList.T;
BEGIN

  VBT.PaintRegion(self, rgn, PaintOp.Bg);
  FOR i := 0 TO LAST(self.axespaths^) DO 
    VBT.Stroke(self, rgn.r, self.axespaths^[i], op:=PaintOp.Fg);
  END;
  FOR i := 0 TO LAST(self.tickspaths^) DO
    VBT.Stroke(self, rgn.r, self.tickspaths^[i], op:=PaintOp.Fg);
  END;
  VBT.Stroke(self, rgn.r, self.gridpath, op:=PaintOp.Fg);
  VBT.Stroke(self, rgn.r, self.graphpath, op:=PaintOp.Fg);

  p := self.points;
  WHILE NOT PointList.Length(p) = 0 DO
    pp := NEW(Path.T);
    Path.MoveTo(pp,Point.FromCoords(p.head.h-5,p.head.v));
    Path.LineTo(pp,Point.FromCoords(p.head.h+5,p.head.v));
    Path.MoveTo(pp,Point.FromCoords(p.head.h,p.head.v-5));
    Path.LineTo(pp,Point.FromCoords(p.head.h,p.head.v+5));
    VBT.Stroke(self, rgn.r, pp, op := PaintOp.Fg);
    p := p.tail;
  END;

  r := self.fills;
  WHILE NOT FillRegionList.Length(r) = 0 DO
    VBT.PaintRegion(self, r.head.r, r.head.p);
    r := r.tail;
  END;

  n := self.notes;
  WHILE NOT NoteList.Length(n) = 0 DO
    VBT.PaintText(self, pt:=n.head.p, fnt:=n.head.f, t:=n.head.t);
    n := n.tail;
  END;

  n := self.titles;
  WHILE NOT NoteList.Length(n) = 0 DO
    VBT.PaintText(self, pt:=n.head.p, fnt:=n.head.f, t:=n.head.t);
    n := n.tail;
  END;

  n := self.labels;
  WHILE NOT NoteList.Length(n) = 0 DO
    VBT.PaintText(self, pt:=n.head.p, fnt:=n.head.f, t:=n.head.t);
    n := n.tail;
  END;

END Repaint;

PROCEDURE Mouse(self: T; READONLY mr :VBT.MouseRec) =
VAR
  bbox :Rect.T := Rect.Empty;
  p :Point.T;
  n :NoteList.T :=self.notes;
  t :Note.T; (* will become self.curtext *)
BEGIN
  IF mr.clickType = VBT.ClickType.FirstDown THEN
    self.mousedown := mr.cp.pt;
    (* find the text bounding box *)
    IF NOT NoteList.Length(n) = 0 THEN 
      p := n.head.p;
      bbox := Rect.Add(VBT.BoundingBox(self,n.head.t,n.head.f),p);
      t := n.head;
    END;
    WHILE NOT(NoteList.Length(n) = 0 OR Rect.Member(mr.cp.pt, bbox)) DO
      p := n.head.p;
      bbox := Rect.Add(VBT.BoundingBox(self,n.head.t,n.head.f),p);
      t := n.head;
      n := n.tail;
    END;
    IF NOT(Rect.Member(mr.cp.pt, bbox)) THEN
      (* we couldn't find a bounding box, so a new piece of text *)
    ELSE
      (* edit/drag an existing piece of text *)
      offset := Point.Sub(mr.cp.pt, t.p);
      IF self.textstate = SelectState.Selected THEN
        self.textstate := SelectState.None;
        XorTextBox(self);
      END;
      IF self.curtext # t THEN
        self.textselected := FALSE;
        self.curtext := t;
      END;
      self.oldtextregion := Region.FromRect(bbox);
      XorTextBox(self);
      self.textstate := SelectState.Dragging;
      VBT.SetCage(self,VBT.CageFromPosition(mr.cp));
    END;
  ELSIF self.textstate = SelectState.Dragging THEN
    IF mr.clickType = VBT.ClickType.LastUp THEN
      (* released the mouse button, do a permanent redraw *)
      IF self.mousedown # mr.cp.pt OR self.textselected THEN
        XorTextBox(self);
        self.textstate := SelectState.None;
        self.textselected := FALSE;
        VBTClass.Repaint(self, self.oldtextregion); 
      ELSE
        self.textstate := SelectState.Selected;    
        self.textselected := TRUE;
      END;
    END;
  END;
END Mouse;

PROCEDURE Position(self :T; READONLY pr :VBT.PositionRec) =
BEGIN
  IF NOT (self.textstate = SelectState.Dragging) THEN
    VBT.SetCage(self, VBT.EverywhereCage);
    (* ignore further movement *)
  ELSIF pr.cp.gone THEN
    VBT.SetCage(self, VBT.GoneCage);
    (* ignore further movement until we get the mouse back *)
  ELSE
    (* rub out old text *)
    XorText(self);
    XorTextBox(self);
    (* update position *)
    self.curtext.p := Point.Sub(pr.cp.pt, offset);
    (* redraw text *)
    XorText(self);
    XorTextBox(self);
    VBT.SetCage(self, VBT.CageFromPosition(pr.cp));
  END;
END Position;

PROCEDURE XorText(self :T) =
BEGIN
  VBT.PaintText(self, pt:=self.curtext.p, fnt:=self.curtext.f, t:=self.curtext.t, op:=PaintOp.Swap);
END XorText;

PROCEDURE XorTextBox(self :T) =
VAR
  bbox :Rect.T;
  p :Path.T;
BEGIN
  bbox := Rect.Add(VBT.BoundingBox(self,self.curtext.t,self.curtext.f),self.curtext.p);
  p := NEW(Path.T);
  Path.MoveTo(p,Rect.NorthWest(bbox));
  Path.LineTo(p,Rect.NorthEast(bbox));
  Path.LineTo(p,Rect.SouthEast(bbox));
  Path.LineTo(p,Rect.SouthWest(bbox));
  Path.LineTo(p,Rect.NorthWest(bbox));
  VBT.Stroke(self,Rect.Full,p,op:=PaintOp.Swap);
END XorTextBox;

PROCEDURE New(g :Graph.T; initproc :InitProc; optinitproc :OptBlockProc):T =
VAR
  n := NEW(T, hiddeninitproc := initproc); 
BEGIN
  n.graphopts := optinitproc();
  RETURN n.init(g);
END New;

BEGIN
END DisplayGraph.



