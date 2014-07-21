MODULE GraphUIBundle;
(* Generated by m3bundle; see its manpage. *)

IMPORT Bundle, BundleRep, Text;
IMPORT Thread, Wr, TextWr;

TYPE T = Bundle.T OBJECT OVERRIDES get := LookUp END;

VAR bundle: T := NIL;

PROCEDURE Get(): Bundle.T =
  BEGIN
    IF (bundle = NIL) THEN bundle := NEW (T) END;
    RETURN bundle;
  END Get;

PROCEDURE LookUp (<*UNUSED*> self: T;  element: TEXT): TEXT = 
  BEGIN
    FOR i := 0 TO LAST (Names)-1 DO
      IF Text.Equal (Names[i], element) THEN
        IF Elements[i] = NIL THEN Elements[i] := GetElt (i) END;
        RETURN Elements[i];
      END;
    END;
    RETURN NIL;
  END LookUp;

CONST Names = ARRAY [0..3] OF TEXT {
  "ZControl.fv",
  "DisplayOptions.fv",
  "FormulaEdit.fv",
  NIL
};

VAR Elements := ARRAY [0..3] OF TEXT {
  NIL (* E0 .. E0_1 *),
  NIL (* E1 .. E1_0 *),
  E2,
  NIL
};

PROCEDURE GetElt (n: INTEGER): TEXT =
  <*FATAL Thread.Alerted, Wr.Failure *>
  VAR wr := TextWr.New ();
  BEGIN
    CASE n OF
    | 0 =>
        Wr.PutText (wr, E0);
        Wr.PutText (wr, E0_0);
        Wr.PutText (wr, E0_1);
    | 1 =>
        Wr.PutText (wr, E1);
        Wr.PutText (wr, E1_0);
    ELSE (*skip*)
    END;
    RETURN TextWr.ToText (wr);
  END GetElt;

CONST E0 = 
   "(ZSplit %main\n  (ZBackground\n    (ZChild %CPanel (Open #TRUE)\n      "
 & "(Filter %cpfilter\n        (Shape (Width 350) (Height 500)\n          ("
 & "Rim (Pen 10)\n            (Frame\n              (Rim (Pen 5)\n         "
 & "       (LabelFont (Family \"Times\") (PointSize 180) (WeightName \"medi"
 & "um\"))\n                (VBox\n                  (Text (BgColor \"Yello"
 & "w\") \"File Controls\")\n                  (Glue 10)\n                 "
 & " (HBox\n                    Fill\n                    (VBox\n          "
 & "            (Button %new \"New\")\n                      (Glue 5)\n    "
 & "                  (PopButton %loadbox (For fileload) \"Load\")\n       "
 & "             )\n                    Fill\n                    (VBox\n  "
 & "                    (PopButton %savebox (For filesave) \"Save\")\n     "
 & "                 (Glue 5)\n                      (Button %printbox \"Pr"
 & "int\")\n                    )\n                    Fill\n              "
 & "      (VBox\n                      (Button %close \"Close\")\n         "
 & "             (Glue 5)\n                      (Guard (Button %quit (Colo"
 & "r \"Red\") \"Quit\"))\n                    )\n                    Fill\n"
 & "                  )\n                  (Glue 10)\n                  (Te"
 & "xt (BgColor \"Yellow\") \"Graph Controls\")\n                  (Glue 10"
 & ")\n                  (HBox\n                    Fill\n                 "
 & "   (VBox\n                      (Button %editdata \"Edit Data\")\n     "
 & "                 (Glue 5)\n                      (Button %editform \"Ed"
 & "it Formula\")\n                      (Glue 5)\n                      (B"
 & "utton %dispopts \"Display Options\")\n                    )\n          "
 & "          Fill\n                    (VBox\n                      (Butto"
 & "n %analyse \"Analyse...\")\n                      (Glue 5)\n           "
 & "           (Button %type \"Graph Type...\")\n                      Fill"
 & "\n                    )\n                    Fill\n                  )\n"
 & "                  (Glue 300)\n                )\n              )\n     "
 & "       )\n          )\n        )\n      )\n    )\n  )\n\n  (ZChild %fil"
 & "eload\n    (Filter %loadboxfilter\n      ";

CONST E0_0 = 
   "(Shape (Width 300)\n        (Rim (Pen 10)\n          (Frame\n          "
 & "  (Rim (Pen 5)\n              (LabelFont (Family \"Times\") (PointSize "
 & "180) (WeightName \"medium\"))\n              (Font (Family \"fixed\") ("
 & "PointSize 130))\n              (VBox\n                (ZMove (BgColor \""
 & "Yellow\") \"Load Graph File\")\n                (Glue 10)\n            "
 & "    (Radio %overwrite\n                  =makenew\n                  (H"
 & "Box\n                    Fill\n                    (Choice %makenew \"M"
 & "ake New\")\n                    Fill\n                    (Choice %usec"
 & "urrent \"Use Current\")\n                    Fill\n                  )\n"
 & "                )\n                (Glue 5)\n                (Shape (He"
 & "ight 155)\n                  (HBox\n                    Fill\n         "
 & "           (VBox\n                      (HBox \n                       "
 & " Fill \n                        (Shape (Width 150)\n                   "
 & "       (DirMenu (For loadbrowser))\n                        )\n        "
 & "                Fill\n                      )\n                      (G"
 & "lue 5)\n                      (Shape (Width 180)\n                     "
 & "   (Frame Lowered (BgColor \"White\")\n                          (FileB"
 & "rowser %loadbrowser (ReadOnly #TRUE))\n                        )\n     "
 & "                 )\n                    )\n                    Fill\n  "
 & "                  (Radio %loadsuffix =loadall\n                      (V"
 & "Box\n                        Fill\n                        (Choice %loa"
 & "ddata \"*.dat\")\n                        (Choice %loadgraph \"*.grp\")"
 & "\n                        (Choice %loadall \"*.*\")\n                  "
 & "      Fill\n                        (Button %load \"Load\")\n          "
 & "              Fill\n                        (CloseButton %loadcancel \""
 & "Cancel\")\n                        Fill\n                      )\n     "
 & "               )                  \n                    Fill\n         "
 & "         )\n                )\n                (Glue 5)\n              "
 & "  (Frame Lowered (BgColor \"White\")\n                  (Helper (For lo"
 & "adbrowser))\n            ";

CONST E0_1 = 
   "    )\n                (Glue 10)\n              )\n            )\n     "
 & "     )\n        )\n      )\n    )\n  )\n\n  (ZChild %filesave\n    (Fil"
 & "ter %saveboxfilter\n      (Shape (Width 300)\n        (Rim (Pen 10)\n  "
 & "        (Frame\n            (Rim (Pen 5)\n              (LabelFont (Fam"
 & "ily \"Times\") (PointSize 180) (WeightName \"medium\"))\n              "
 & "(Font (Family \"fixed\") (PointSize 130))\n              (VBox\n       "
 & "         (ZMove (BgColor \"Yellow\") \"Save Graph File\")\n            "
 & "    (Glue 10)\n                (HBox\n                  Fill\n         "
 & "         (Boolean %backupfile (Value #TRUE) \"Backup Overwrites\")\n   "
 & "               Fill\n                )\n                (Glue 5)\n     "
 & "           (Shape (Height 155)\n                  (HBox\n              "
 & "      Fill\n                    (VBox\n                      (HBox \n  "
 & "                      Fill \n                        (Shape (Width 150)"
 & "\n                          (DirMenu (For savebrowser))\n              "
 & "          )\n                        Fill\n                      )\n   "
 & "                   (Glue 5)\n                      (Shape (Width 180)\n"
 & "                        (Frame Lowered (BgColor \"White\")\n           "
 & "               (FileBrowser %savebrowser (ReadOnly #TRUE))\n           "
 & "             )\n                      )\n                    )\n       "
 & "             Fill\n                    (VBox\n                      Fil"
 & "l\n                      (Button %save \"Save\")\n                     "
 & " Fill\n                      (CloseButton %savecancel \"Cancel\")\n    "
 & "                  Fill\n                    )                  \n      "
 & "              Fill \n                  )\n                )\n          "
 & "      (Glue 5)\n                (Frame Lowered (BgColor \"White\")\n   "
 & "               (Helper (For savebrowser))\n                )\n         "
 & "       (Glue 10)  \n              )    \n            )\n          )\n  "
 & "      )\n      )\n    )\n  )\n    \n)\n";

CONST E1 = 
   "(Shape (Width 300)\n(Rim (Pen 10)\n  (Frame\n    (Rim (Pen 5)\n      (L"
 & "abelFont (Family \"Times\") (PointSize 180) (WeightName \"medium\"))\n "
 & "     (Font (Family \"fixed\") (PointSize 130) (WeightName \"medium\"))\n"
 & "      (VBox\n        (Text (BgColor \"Yellow\") \"Display Options\")\n "
 & "       (Glue 10)\n        (Frame Lowered\n          (VBox\n            "
 & "(HBox\n              Fill\n              (VBox\n                (Glue 2"
 & ")\n                (Boolean %markerson \"\")\n                (Boolean "
 & "%lineson \"\")\n                (Boolean %tickson \"\")\n              "
 & "  (Glue 2)\n              )\n              (VBox\n                (Glue"
 & " 2)\n                (Text (LeftAlign #TRUE) \"Display Point Markers\")"
 & "\n                (Text (LeftAlign #TRUE) \"Display Line Segments\")\n "
 & "               (Text (LeftAlign #TRUE) \"Display Axis Ticks\")\n       "
 & "         (HBox\n                  (Text (LeftAlign #TRUE) \"Tick Spacin"
 & "g:\")\n                  (Shape (Width 50) (Frame Lowered (TypeIn (BgCo"
 & "lor \"White\"))))\n                  Fill\n                )\n         "
 & "       (Glue 2)\n              )\n              Fill\n            )\n  "
 & "        ) \n        )\n        (Glue 5)\n        (Frame Lowered\n      "
 & "    (VBox\n            (Glue 2)\n            (Text \"Domain of Graph:\""
 & ")\n            (Glue 5)\n            (Radio %domain =mandomain\n       "
 & "       (HBox\n                Fill\n                (Choice %autodomain"
 & " \n                  (Text (LeftAlign #TRUE) \"Auto\")\n               "
 & " )\n                Fill\n                (Choice %mandomain\n         "
 & "           (Text (LeftAlign #TRUE) \"Manual\")\n                )\n    "
 & "            Fill\n              )\n            )\n            (Glue 5)\n"
 & "            (HBox\n              Fill\n              (Shape (Width 50) "
 & "(Frame Lowered (TypeIn (BgColor \"White\"))))\n              (Glue 5)\n"
 & "              (Text \"<\")\n              (Text %domainvar \"X\")\n    "
 & "          (Text \"<\")\n              (Glue 5)\n              (Shape (W"
 & "idth 50) (Frame Lowered (TypeIn (BgColor \"White\")))) \n              "
 & "Fill\n            )\n            (Glue 2)\n        ";

CONST E1_0 = 
   "  )\n        )\n        (Glue 5)\n        (Frame Lowered\n          (VB"
 & "ox\n            (Glue 2)\n            (Text \"Range of Graph:\")\n     "
 & "       (Glue 5)\n            (Radio %range =autorange\n              (H"
 & "Box\n                Fill\n                (Choice %autorange \n       "
 & "           (Text (LeftAlign #TRUE) \"Auto\")\n                )\n      "
 & "          Fill\n                (Choice %manrange\n                    "
 & "(Text (LeftAlign #TRUE) \"Manual\")\n                )\n               "
 & " Fill\n              )\n            )\n            (Glue 5)\n          "
 & "  (HBox\n              Fill\n              (Shape (Width 50) (Frame Low"
 & "ered (TypeIn (BgColor \"White\"))))\n              (Glue 5)\n          "
 & "    (Text \"<\")\n              (Text %rangevar \"Y\")\n              ("
 & "Text \"<\")\n              (Glue 5)\n              (Shape (Width 50) (F"
 & "rame Lowered (TypeIn (BgColor \"White\"))))\n              Fill\n      "
 & "      )\n            (Glue 2)\n          )\n        )\n\n      )  \n   "
 & " )\n  )\n)\n)\n";

CONST E2 = 
   "(Shape (Width 300)\n(Rim (Pen 10)\n  (Frame\n    (Rim (Pen 5)\n      (L"
 & "abelFont (Family \"Times\") (PointSize 180) (WeightName \"medium\"))\n "
 & "     (Font (Family \"fixed\") (PointSize 130) (WeightName \"medium\"))\n"
 & "      (VBox\n        (Text (BgColor \"Yellow\") \"Edit Graph Formula\")"
 & "\n        (Glue 5)\n        (Shape (Height 47) (Frame Lowered (TextEdit"
 & " (BgColor \"White\"))))\n        (Glue 5)\n        (HBox\n          Fil"
 & "l\n          (Button %replot \"Replot\")\n          Fill\n          (Bu"
 & "tton %cancel \"Cancel\")\n          Fill\n        )\n      )  \n    )\n"
 & "  )\n)\n)\n";


BEGIN
END GraphUIBundle.