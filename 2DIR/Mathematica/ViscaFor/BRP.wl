(* Mathematica Package *)

(* :Title: BRP` *)
(* :Context: BRP` *)
(* :Author: Rolf *)
(* :Date: 2017-10-10 *)

(* :Package Version: 0.3 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 GluonVision *)
(* :Keywords: *)
(* :Discussion: BreakPoint generating a Dialog Notebook *)

BeginPackage["BRP`"]

    BRP::usage="BRP[any, stuff, ...] starts a kernel Dialog[] and opens a notebook.";
    $Break::usage="If $Break is False, then BRP[] does not do anything.";
    
Begin["`Private`"]

    $Break = True;

SetAttributes[BRP, HoldAll];
BRP[expr__] /; $Break :=
    With[{context = $Context},
    Module[ {continue,cd,toInputOrText,title},
        continue[] :=
            Block[ {bn = ButtonNotebook[]},
                SelectionMove[bn, After,Notebook];
                NotebookWrite[bn, {Cell["Return[]","Input", FontColor -> Red]}, All];
                SelectionEvaluate[bn]
            ];
        title = context <> "  |  " <> DateString[];
        SetAttributes[toInputOrText,HoldAll];
        toInputOrText[x__] :=
            Map[Function[h,If[ Head[Unevaluated[h]]===String,
                               TextCell[h, "Text"],
                               ExpressionCell[Defer@h,"Input"]
                           ],HoldFirst], Unevaluated[{x}]];
        cd =
        CreateDocument[
		        	toInputOrText[ expr]
        	,
        DockedCells -> Cell[BoxData @ ToBoxes @ 
          TextGrid[{
			{  context
        	 , "Exit by \[EscapeKey]"
        	 , DefaultButton[continue[] ]
        	}
           } , Alignment->{{Left,Center,Right},Center},ItemSize->Scaled[1/3]
           ] ,"DockedCell"
             , Background -> LightBlue]
             , WindowTitle -> title
             , Magnification-> 1.25
             , WindowMargins-> {{Automatic,0},{Automatic,0}}
             , Background -> LightGreen,WindowSize -> {800,400}
             , NotebookEventActions:>{"EscapeKeyDown":>(continue[])}
             , CellContext -> context
        ];
        SelectionMove[cd, Next, Cell];
        Dialog[];
        NotebookClose[cd];
        Unevaluated[expr]
   ]];


End[]

EndPackage[]
