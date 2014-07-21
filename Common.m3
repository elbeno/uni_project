MODULE Common;

IMPORT Thread;

PROCEDURE Inc(self: PanelCount):INTEGER =
VAR 
  i :INTEGER;
BEGIN
  LOCK self DO
    self.value := self.value + 1;
    i := self.value;
  END;
  RETURN i;
END Inc;

PROCEDURE Dec(self: PanelCount):INTEGER =
VAR
  i :INTEGER;
BEGIN
  LOCK self DO
    self.value := self.value - 1;
    IF self.value = 0 THEN 
      Thread.Signal(self.WaitForNone); 
    END;
    i := self.value;
  END;
  RETURN i;
END Dec;

PROCEDURE Val(self: PanelCount): INTEGER =
VAR
  i :INTEGER;
BEGIN
  LOCK self DO
    i := self.value;
  END;
  RETURN i;
END Val;

PROCEDURE InitPanelCount() =
BEGIN
  NumberofPanels := NEW(PanelCount);
  NumberofPanels.WaitForNone := NEW(Thread.Condition);
END InitPanelCount;

BEGIN
END Common.
