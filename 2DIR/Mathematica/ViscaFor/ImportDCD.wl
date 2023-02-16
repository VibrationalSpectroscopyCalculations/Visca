(* Wolfram Language Package *)

(* mostly taken from 
http://mathematica.stackexchange.com/a/4189/745

RM: added restoration of default Dot behaviour
*)

BeginPackage["ImportDCD`"]

ImportDCD::usage = "ImportDCD[file] imports a DCD file."

readDCDHeader::usage = "readDCDHeader  "

(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

(*Some utility functions*)
SetAttributes[MapShowIt, {HoldAll, Listable}];
MapShowIt[code__] := MapShowIt[{code}];
MapShowIt[code_] := With[{y = code}, Defer[code = y]]

SetAttributes[Puts, HoldAll];
Options[Puts] = {DisplayFunction -> Shallow};
$VerbosePrint = False;
Puts[msg_, data_, opt : OptionsPattern[]] := 
  If[$VerbosePrint, Print[msg, OptionValue[DisplayFunction][data]]];
Puts[msg_List] := If[$VerbosePrint, Print[MapShowIt[msg]]];
Puts[msg_] := If[$VerbosePrint, Print[msg]];
(*warning this is still unsafe ... it should be done as \
answered  here
http://mathematica.stackexchange.com/a/4189/745
*)

changeDot[]:= (
  Unprotect[Dot];
  SetAttributes[Dot, HoldRest];
  Dot[h_, key_Symbol | key_String] := 
    System`Utilities`HashTableGet[h, ToString[Unevaluated[key]]];
  Dot /: Set[Dot[h_, key_Symbol | key_String], value_] := (
    Quiet[
      System`Utilities`HashTableRemove[h, ToString[Unevaluated[key]]]
    , System`Utilities`HashTableRemove::norem
    ];
      System`Utilities`HashTableAdd[h, ToString[Unevaluated[key]], 
    value]);
Protect[Dot];
);

(*
changeDot[];
*)

On[Assert];

Options[readDCDHeader] = {"Verbose" -> True};
readDCDHeader::nonintframes = 
  "Number of frames calculated from files size (`1`) is not an integer!";
readDCDHeader::diffframes = 
  "Header claims `1` frames, but there are `2` frames!";
readDCDHeader[fileName_?FileExistsQ, opts : OptionsPattern[]] :=
  Module[{str, asso, magicnum, h, i, newsize, newsize1, numlines, fint}, 
   Block[{$VerbosePrint = OptionValue["Verbose"]},
    (* Print["entering readDCDHeader"];*)
      h = System`Utilities`HashTable[];
      str = OpenRead[fileName, BinaryFormat -> True];
      fint = BinaryRead[str, "Integer32"];
      changeDot[];
      (*if we don't read an 84 then try to reverse the endianes
      If[fint=!=84,(*then*)
        Close[str];
       str= OpenRead[fileName,BinaryFormat -> True,
    ByteOrdering->-$ByteOrdering];]*)
      Assert[fint == 84, "First integer must be 84"];
      h . stream = str;
      magicnum = BinaryReadList[str, "Character8", 4];

      Assert[magicnum == {"C", "O", "R", "D"}, "CORD not present"];

      h . nset = BinaryRead[str, "Integer32"];
      h . istart = BinaryRead[str, "Integer32"];
      h . nsavc = BinaryRead[str, "Integer32"];

      (*read free indexes*)
      SetStreamPosition[str, 40];
      h . numfree = BinaryRead[str, "Integer32"];

      Puts[{h . nset, h . istart, h . nsavc, h . numfree}];

      (*find out if is charmm DCD*)
      SetStreamPosition[str, 84];
      i = BinaryRead[str, "Integer32"];
      If[i == 0, (*then*)
         h . charmm = False;
       ,(*else*)
       h . charmm = True;

       (* check for extra block*)
       SetStreamPosition[str, 48];
       i = BinaryRead[str, "Integer32"];
       h . charmm$extrablock = (i == 1);

                     SetStreamPosition[str, 52];
                      i = BinaryRead[str, "Integer32"];
       h . charmm$4dims = (i == 1);
       ];

      Puts[{h . charmm, h . charmm$extrablock, h . charmm$4dims}];

      (*read the timestep*)  
      SetStreamPosition[str, 44];
      If[h . charmm, (*then*)
       h . DELTA = BinaryRead[str, "Real32"];
       ,(*else*)
       h . DELTA = BinaryRead[str, "Real64"];];
      h . step = h . DELTA;

      (*get the title*)
      SetStreamPosition[str, 92];
      newsize = BinaryRead[str, "Integer32"];
      numlines = BinaryRead[str, "Integer32"];
      (*TODO check for curoupted Ntitle values*)
      h . title = 
     StringJoin@BinaryReadList[str, "Character8", numlines*80];
      newsize1 = BinaryRead[str, "Integer32"];
      Assert[newsize == newsize1];
      Puts[h . title];
      i = BinaryRead[str, "Integer32"]; 
    Assert[i == 4, "4 must be read before num of atoms"];
      h . numatoms = BinaryRead[str, "Integer32"];
      h . N = h . numatoms;
      i = BinaryRead[str, "Integer32"]; 
    Assert[i == 4, "4 must be read after num of atoms"];
      Puts[{h . DELTA, h . N}];

      (*love this comment from the original matdcd package*)
      (*stuff with freeindexes.  Just smile and nod.*)
      If[ h . numfree =!= 0, (*then*)
          i = BinaryRead[str, "Integer32"];  (* should be N-NAMNF*4*)

     h . freeindexes = BinaryReadList[str, "Integer32", h . N - h . numfree];
          i = BinaryRead[str, "Integer32"];  (* should be N-NAMNF*4*)
       ];

    h . headerend = StreamPosition[str];  
    (*calculate one frame size in bytes*)
    h . framesize = 3*(h . numatoms*4 + 2*4(*for the blocksize*))
        + If[h . charmm$extrablock, 12*4 + 2*4, 0]
        + If[h . charmm$4dims, +h . numatoms*4 + 4*2, 0];

    h . numframes = (FileByteCount[fileName] - h . headerend)/h . framesize;
    (* Warn if noninteger frame number and if the actual frames differ from h.nset*)
    If[Head[h . numframes] =!= Integer,(*then*)
       Message[readDCDHeader::nonintframes, h . numframes]];
    If[Head[h . numframes] != h . nset,(*then*)
       Message[readDCDHeader::diffframes, h . nset, h . numframes]];

    restoreDot[];
    asso = h // System`Utilities`HashTableToAssociation;
    asso =   KeyMap[StringSplit[ToString[#],"`"][[-1]]&, asso];
    Global`H = asso;
    asso
    ]];

Options[readDCDStep] = {
  "Verbose" -> False,
  "Atoms" -> All(*or a one based list of atoms to take*)
  };

readDCDStep[h_Association, opts : OptionsPattern[]] :=
  Module[{x, y, z, str, blocksize, ind}, 
   Block[{$VerbosePrint = OptionValue["Verbose"]},
      ind = OptionValue["Atoms"];
      Assert[h@"numfree" == 0, 
     "Fixed atoms anad free indices are not supported"];
      str = h@"stream";

      If[h@"charmm" && h@"charmm$extrablock", (*then*)
       (*unit cell info*) 
         blocksize = BinaryRead[str, "Integer32"];
         Puts[
      "Skipping unit info cords. (blocksize: " <> 
       ToString[blocksize] <> ")"];
         Skip[str, "Byte", blocksize];

         Assert[blocksize == BinaryRead[str, "Integer32"], 
      "Wrong blocksize in extra block "];
       ];
     (* Get x coordinates *)
      blocksize = BinaryRead[str, "Integer32"];
      Puts[
     "Getting x cords. (blocksize: " <> ToString[blocksize] <> ")"];
      x = BinaryReadList[str, "Real32", blocksize/4]; 
      If[Head[ind] == List, x = Part[x, ind]];
      Puts["x:\n", x];
      Assert[blocksize == BinaryRead[str, "Integer32"], 
     "Wrong blocksize in x coords"];

      (* Get y coordinates *)
      blocksize = BinaryRead[str, "Integer32"];
      Puts[
     "Getting y cords. (blocksize: " <> ToString[blocksize] <> ")"];
      y = BinaryReadList[str, "Real32", blocksize/4];
      If[Head[ind] == List, y = Part[y, ind]];
      Puts["y:\n", y];
      Assert[blocksize == BinaryRead[str, "Integer32"], 
     "Wrong blocksize in y coords"];

      (* Get z coordinates *)
      Puts[
     "Getting z cords. (blocksize: " <> ToString[blocksize] <> ")"];
      blocksize = BinaryRead[str, "Integer32"];
      z = BinaryReadList[str, "Real32", blocksize/4];
      If[Head[ind] == List, z = Part[z, ind]];
      Puts["z:\n", z];
      Assert[blocksize == BinaryRead[str, "Integer32"], 
     "Wrong blocksize in z coords"];

      (*skip 4th dimension if it exists*)
      If[h@"charmm" && h@"charmm$4dims",(*then*)
         Puts["Skipping w cords."];   
         blocksize = BinaryRead[str, "Integer32"];
                       Skip[str, "Byte", blocksize];

     Assert[blocksize == BinaryRead[str, "Integer32"], 
      "Wrong blocksize in 4th dim"];
       ];
    Assert[Length[x] == Length[y] == Length[z], 
     "Wrong size of x or y or z"];  
    Return[Developer`ToPackedArray[Transpose@{x, y, z}]];
    ]
];

(*CloseDCD[h_System`Utilities`HashTable] := Close[h . stream];*)
CloseDCD[a_Association] := Close[a @"stream"];


restoreDot[]:= (
  Unprotect[Dot];
  ClearAttributes[Dot, HoldRest];
  DownValues[Dot] = {};
  UpValues[Dot] = {};
  Protect[Dot];
);


Options[ImportDCD] = Evaluate[Options[readDCDStep]];
ImportDCD[fileName_, options : OptionsPattern[]] :=
  Module[{asso, data, ropts, opts, numberOfFrames}, 
   Block[{VerbosePrint = OptionValue["Verbose"]},
(*      Pause[.04];*)
      opts = DeleteDuplicates[{options}~Join~Options[ImportDCD], 
      ToString[First[#1]] == ToString[First[#2]] &
      ];
      Puts[opts];  
      asso = readDCDHeader[fileName, Verbose -> OptionValue["Verbose"]];
      
      numberOfFrames = asso@"numframes";
      Print["numberOfFrames = ", numberOfFrames];

      ropts = Evaluate[FilterRules[opts, Options[readDCDStep]]];
      data = (readDCDStep[asso, ropts]) & /@ Range[numberOfFrames];

      Close @ asso["stream"];
      Return[data]
    ]
];


End[] (* End Private Context *)

EndPackage[]