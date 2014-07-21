INTERFACE GraphUI;

IMPORT FormsVBT, Text, VBT;

TYPE 
  Private <: FormsVBT.T;
  Public = Private OBJECT
      METHODS
        init() :T;
      END;
 T <: Public;

PROCEDURE New():T;
PROCEDURE BoxCancel(             form :FormsVBT.T;
                    <* UNUSED *> event:Text.T;
                    <* UNUSED *> cl   :REFANY;
                    <* UNUSED *> ts   :VBT.TimeStamp);
PROCEDURE OptionBoxCancel(form :FormsVBT.T;
                          event:Text.T;
                          cl   :REFANY;
                          ts   :VBT.TimeStamp);
PROCEDURE UpdateGraph(form :FormsVBT.T;
                      event:Text.T;
                      cl   :REFANY;
                      ts   :VBT.TimeStamp);
  
EXCEPTION
  FileError(TEXT);

END GraphUI.

