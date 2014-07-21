INTERFACE Common;

IMPORT Thread, GraphTypeList, Data;

TYPE
  PanelCount = Thread.Mutex OBJECT
                 value := 0;
                 WaitForNone :Thread.Condition;
               METHODS
                 inc():INTEGER := Inc;
                 dec():INTEGER := Dec;
                 val():INTEGER := Val;
               END;

VAR
  NumberofPanels :PanelCount;
  GraphTypes: GraphTypeList.T;
  macheps :Data.Element;

PROCEDURE Inc(self: PanelCount):INTEGER;
PROCEDURE Dec(self: PanelCount):INTEGER;
PROCEDURE Val(self: PanelCount):INTEGER;   
PROCEDURE InitPanelCount();

END Common.
