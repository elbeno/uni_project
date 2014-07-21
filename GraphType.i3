INTERFACE GraphType;

IMPORT DisplayGraph, AttachProcList, AnalOptList;

TYPE
  T = RECORD
        desc :TEXT;
        proc :DisplayGraph.InitProc;
        printproc :DisplayGraph.PrintProc;
        axisoptsVBTname :TEXT;
        graphoptsVBTname :TEXT;
        newoptions :DisplayGraph.OptBlockProc;
        proclist :AttachProcList.T;
        analoptlist :AnalOptList.T;
      END;

PROCEDURE Equal(k1 :T; k2 :T) :BOOLEAN;

END GraphType.