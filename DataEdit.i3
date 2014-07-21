INTERFACE DataEdit;

IMPORT Data, FormsVBT;

TYPE
  Private <: FormsVBT.T;
  Public = Private OBJECT
             data: Data.T;
             datavisible :BOOLEAN;
           METHODS
             init() :T;
             cellstodata();
             datatocells();
             addrows(start, number :INTEGER);
             removerows(start, number :INTEGER);
             addcols(start, number :INTEGER);
             removecols(start, number :INTEGER);
           END;
  T <: Public;

PROCEDURE New():T;

END DataEdit.
