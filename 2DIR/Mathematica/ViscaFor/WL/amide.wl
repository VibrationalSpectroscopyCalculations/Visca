

(* ::Package:: *)
$TIME=0.;
If[!ValueQ[$debug], $debug=False];

(*
SetAttributes[timeIt, HoldAll];
timeIt[expr_]:= Block[{$res, $tinitial}, ( $tinitial=AbsoluteTime[];$res=expr; $TIME=$TIME + (AbsoluteTime[]-$tinitial) ; $res)];
*)
timeIt = Identity;


esc = Association["reset" -> "\033[1;0m", 
	        "black" -> "\033[1;30m", 
	        "red" -> "\033[1;31m", 
	        "green" -> "\033[1;32m", 
	        "yellow" -> "\033[1;33m", 
	        "blue" -> "\033[1;34m", 
	        "brightblue" -> "\033[1;94m", 
	        "brightgreen" -> "\033[1;92m", 
	        "magenta" -> "\033[1;35m"
];
(*]]]]]] *)
SetAttributes[print, HoldAll];
SetAttributes[cPrint, HoldAll];
SetAttributes[dPrint, HoldAll];
print[x__] := Print[ DateString[{"Second", ":", "Millisecond"}] <> "  ",x]
dPrint[x__] := If[TrueQ[$debug], Print[ DateString[{"Second", ":", "Millisecond"}] <> "  ",x]]

cPrint[col_String, x__]:= If[$Notebooks
	, print[Style[Row[{x}," "], col /.{"red" -> Red, "green"R5_neat_2021_OrientedToRes13to19_TDCM-B3LYP.txt -> Green, "blue" -> Blue}]
    , print[esc[col],x, esc["reset"]]
]]

(*cPrint["magenta", "Entering amide.wl"];*)

(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]]*)

countNumRes::usage=""
compiledQ::usage = "compiledQ[cFunc] gives True if the compiled function cFunc does not use MainEvaluate."


(* ::Section:: *)
(*notes*)


(* ::Subsection:: *)
(* https://mathematica.stackexchange.com/questions/269061/is-it-possible-to-use-the-dgeev-and-dsyev-lapack-subroutines-in-mathematica*)


(* ::Section:: *)
(*write*)


(* ::Program:: *)
(*IF (cla==0) THEN*)
(* WRITE(*,*) '*****************************************************************************'*)
(* WRITE(*,*) '*Amide I Vibrational Spectra Simulator                                      *'*)
(* WRITE(*,*) '* Written by Sean A. Fischer, 2014                                          *'*)
(* WRITE(*,*) '*  References to follow soon                                                *'*)
(* WRITE(*,*) '*                                                                           *'*)
(* WRITE(*,*) '*Usage example:                                                             *'*)
(* WRITE(*,*) '* ./amide -pdb prot_ref.pdb -dcd traj.dcd -IR -charge 0                     *'*)
(* WRITE(*,*) '*Options:                                                                   *'*)
(* WRITE(*,*) '* -pdb filename.pdb  (NOT optional)                                         *'*)
(* WRITE(*,*) '* -dcd filename.dcd  (NOT optional)                                         *'*)
(* WRITE(*,*) '* -wt filename.dat   (optional, defaults to equal weighting)                *'*)
(* WRITE(*,*) '* -grps filename.dat (optional, if omitted will NOT breakdown spectra)      *'*)
(* WRITE(*,*) '* -IR                (optional, if omitted will NOT print IR spectrum)      *'*)
(* WRITE(*,*) '* -Raman             (optional, if omitted will NOT print Raman spectrum)   *'*)
(* WRITE(*,*) '* -SFG               (optional, if omitted will NOT print SFG spectrum)     *'*)
(*!saf*)
(* WRITE(*,*) '* -Ham               (optional, if omitted will NOT print avg. Hamiltonian) *'*)
(* WRITE(*,*) '* -TDM               (optional, if omitted will NOT print first TDM)        *'*)
(* WRITE(*,*) '* -inhom 0          (optional, defaults to 0)                              *'*)
(*!saf*)
(* WRITE(*,*) '* -width 5           (optional, defaults to 5 cm^-1)                        *'*)
(* WRITE(*,*) '* -charge 0          (optional, defaults to 0)                              *'*)
(* WRITE(*,*) '*   charge picks the atomic charge parameters to be used                    *'*)
(* WRITE(*,*) '*   0 -- B3LYP/aug-cc-pVTZ/PCM(water),MK based atomic charges               *'*)
(* WRITE(*,*) '*         has a parameter set for both N-H and N-C residues                 *'*)
(* WRITE(*,*) '*   1 -- B3LYP/6-31+G(d), Mulliken based atomic charges,                    *'*)
(* WRITE(*,*) '*         single parameter set for all residues (N-D)                       *'*)
(* WRITE(*,*) '* -coup 0            (optional, defaults to 0)                              *'*)
(* WRITE(*,*) '*   coup picks the coupling scheme to be used                               *'*)
(* WRITE(*,*) '*   0 -- Transition charge method (TCC/TCI)                                 *'*)
(* WRITE(*,*) '*   1 -- Transition dipole method (TDC)                                     *'*)
(* WRITE(*,*) '* -dip 0             (optional, defaults to 0)                              *'*)
(* WRITE(*,*) '*   location for transition dipole (for TDC scheme)                         *'*)
(* WRITE(*,*) '*   0 -- 70.6% of C=O bond from C along C=O bond                            *'*)
(* WRITE(*,*) '*   1 -- 54.1% of C=O bond from C along C=O bond,                           *'*)
(* WRITE(*,*) '*        19.4% of C-N bond from C along C-N bond                            *'*)
(* WRITE(*,*) '*   2 -- center of mass of amide group                                      *'*)
(* WRITE(*,*) '* -nncm 1            (optional, default is 1)                               *'*)
(* WRITE(*,*) '*   0 -- Do not use nearest-neighbor coupling map                           *'*)
(* WRITE(*,*) '*   1 -- Use nearest-neighbor coupling map                                  *'*)
(* WRITE(*,*) '*   Currently only parameterized for N-H type residues                      *'*)
(* WRITE(*,*) '*   (i.e. will use that parameter set for all residues)                     *'*)
(* WRITE(*,*) '* -avgOH 10          (optional, default is 10)                              *'*)
(* WRITE(*,*) '*   number of frames to average together for C=O bond length for            *'*)
(* WRITE(*,*) '*   determining frequency shift                                             *'*)
(* WRITE(*,*) '* -mOH 1500      (optional, default is 1500)                                *'*)
(* WRITE(*,*) '*   slope for linear relationship between OH bond length and frequency      *'*)
(* WRITE(*,*) '* -Omega0 1650      (optional, default is 1650)                             *'*)
(* WRITE(*,*) '*    gas-phase frequency                                                    *'*)
(* WRITE(*,*) '*****************************************************************************'*)




If[!ValueQ[$$],
$$ = <| 
  "pdb_flag" -> False,
  "dcd_flag" -> False,
  "wt_flag" -> False,
  "grp_flag" -> False,
  "IR_flag" -> False,
  "Raman_flag" -> True,
  "SFG_flag" -> True, 
  
  "Hamiltonian_flag"-> False, 
  "TransDipMom_flag"-> False, 
  "w_inhom" -> 0, 
 
  "width"  -> 5.0,
  "char_flag"  ->0,
  "coup_flag"  -> 0,
  "dip_flag"  -> 0,
  "nnc_flag"  -> 1,
  "avgOHmax"  -> 10,
  "slopeOH"  -> 1500.0,
  "OmegaZero"  -> 1650.0,
  
  "spec_min" -> 1500,
  "spec_max" -> 1800
|>
]

$$["nspts"] = $$["spec_max"] - $$["spec_min"] + 1



SetAttributes[importData, HoldFirst];
importData[as_(*Association*), pdbFile_String, dcdFile_String] := Module[{lines}, Null
  ; lines = Import[pdbFile, "Lines"]
  ; as["pdbLines"] = lines
]


(* ::Input:: *)
(*Import[pdbFile,"Elements"]*)


(* ::Input:: *)
(*Length@Flatten[Import[pdbFile,"Residues"]]*)


(* ::Text:: *)
(*11   format(a6,a5,1x,a4,a1,a3,1x,a1,a4,a1,3x,f8.3,f8.3,f8.3,f6.2,f6.2)*)


(* ::Program:: *)
(*!Read number of atoms and number of residues*)
(*nchains=0*)
(*DO*)
(* READ(9,'(A80)',IOSTAT=ERR) line*)
(* IF (ERR/=0) EXIT*)
(* IF (INDEX(line,"TER")/=0 .AND. INDEX(line,"MASTER")==0) nchains=nchains+1*)
(*END DO*)

(* put nchains determination 
  !Read number of atoms and number of residues
  also into readPDB
*)
SetAttributes[readPDB,HoldFirst];
readPDB[as_] := Module[{j, line, lines, Ci, Oi, Ni, Hi, CAi, n, proFlag, nchains, i, atom, res}, Null
  ; dPrint["entering readPDB"]
  ; countNumRes[as]
  ; Do[proFlag[j]=0, {j, as["tot_amide"]}]
  ; as["pro_flag"] =proFlag 
  ; lines =  as["pdbLines"]
  ; Ci=0 ; Oi=0 ; Ni=0 ; Hi=0 ; CAi=0
  ; n=1
  ; as["cap_flag"] = False
  ; nchains = 0
  (* 
   !The C and O of residue i, along with the N and H/CD(Pro) of residue i+1 make up amide group i
   !Array atsw keeps track of which atoms belong in which groups
  *)
  ; i = 1
  ; Do[ 
      If[!StringFreeQ[line, "TER"] 
         , i = i+1 ; Ci=0 ; Oi=0 ; Ni=0 ; Hi=0 ; CAi=0 
      ]
  ; as["i"]=i
  ; If[StringFreeQ[line, "ATOM"], Continue[] ]
  (* READ(line,11) scr1,scr2,atom,scr6,res,scr7,scr4,scr8,coor(1),coor(2),coor(3),scr9,scr10 *)
  ; {atom, res} = StringTrim @ StringTake[line ,{{13, 16},{18, 20}}]
(*  ; If[n < 10, print["{i, n, atom, res} = ", InputForm @ {i, " ", n, " ", atom, " ", res, " "}]];*)
  ; If[StringMatchQ[res, "HAP" | "HOH" | "SOL"] , Continue[] (* CYCLE *) ]
  ; If[ i == 1, If[res === "ACE", as["cap_flag"] = True]]

  ; If[ atom === "C" , Ci = Ci + 1 ; If[Ci < as["num_res"][[i]], as["atsw"][n] = 1] ]          
  ; If[ atom === "O" , Oi = Oi + 1 ; If[Oi < as["num_res"][[i]], as["atsw"][n] = 2] ]          
  ; If[ atom === "N" , Ni = Ni + 1 ; If[Ni > 1 || as["cap_flag"],  as["atsw"][n] = 3] ]          
  
  (*  ELSE IF (TRIM(ADJUSTL(atom))=='H'.OR.
       (TRIM(ADJUSTL(res))=='PRO'.AND.TRIM(ADJUSTL(atom))=='CD')
      ) THEN
      Hi=Hi+1
      atsw(n)=4
      IF (TRIM(ADJUSTL(res))=='PRO') pro_flag(Hi)=1
  *)
  ; If[ (atom === "H") || ( ( res === "PRO") && ( atom === "CD")) 
        , 
        Hi = Hi + 1 ; as["atsw"][n] = 4 
        ; If[ res ===  "PRO" , as["pro_flag"][Hi] = 1 ]
    ]
  ; If[ atom === "CA" 
        , CAi = CAi + 1 
        ; If[CAi > 1 || as["cap_flag"],  as["atsw"][n] = 5] 
    ]          
  ; If[ (atom === "CH3") && (res === "NME")
        , CAi = CAi + 1 
        ; as["atsw"][n] = 5 
    ]          
    
  ; n = n + 1
  ,{line, lines}
  ]
  ; as["Ci"] = Ci
  (*
  ; AppendTo[as,  <|"Ci" -> Ci, "Ni" -> Ni, "Oi" -> Oi  |>]
  *)
  ; dPrint["exiting readPDB"]
]
  
SetAttributes[countNumRes, HoldFirst];
countNumRes[as_] := Module[{ line, lines, nchains, i, j, resName, resNameb, numRes, numAt, numAmide, totAmide, atsw },Null
  ; lines = as["pdbLines"]
  ; resNameb = ""
  ; nchains = 0
  ; dPrint@"in countNumRes"
  (* 
  nchains=0
  DO
   READ(9,'(A80)',IOSTAT=ERR) line
   IF (ERR/=0) EXIT
   IF (INDEX(line,"TER")/=0 .AND. INDEX(line,"MASTER")==0) nchains=nchains+1
  END DO
  *)
  ; Do[ !StringFreeQ[line, "TER"] && StringFreeQ[line, "MASTER"],
        nchains = nchains + 1
        ,{line, lines}
    ]
    
  ; as["nchains"] = nchains

  ; numRes = numAmide = ConstantArray[0, nchains]
  ; numAt = 0
  ; dPrint[StringTemplate["Found `` separate peptides/proteins"] @ nchains ]
  ; i = 1;    
  ; Do[ If[!StringFreeQ[line, "TER"] , i = i+1 ]
      ; If[!StringFreeQ[line, "ATOM"]
          ,
          (*   READ(line,11) scr1,scr2,atom,scr6,res,scr7,scr4,scr8,coor(1),coor(2),coor(3),scr9,scr10 *)
            resName (* src4 *) = StringTake[line, {23 ,26}]
          ; If[ resName =!= resNameb, numRes[[i]] = numRes[[i]]+1 ]
          ; resNameb = resName
          ; numAt = numAt + 1
        ]
     ,{line, lines} 
    ]
  ;
  ;(*!Number of amide groups is one less than the number of residues*)
  ; totAmide=0
  
  ; Do[
      print[StringTemplate["``  residues on peptide  ``"][numRes[[i]], i ]]
      ; numAmide[[i]]=numRes[[i]]-1
      ; If[numRes[[i]] > 0, totAmide = totAmide + (numRes[[i]]-1)]
      , {i, nchains}
    ]
    
(* INTEGER,DIMENSION(num_at),INTENT(OUT) :: atsw *)
  ; Do[atsw[j]=0, {j, numAt}]
  ; as["atsw"] = atsw   (* so atsw is NOT a list ... *)
  ; as["num_at"] = numAt 
  ; as["num_res"] = numRes
  ; as["num_amide"] = numAmide
  ; as["tot_amide"] = totAmide
(* !Allocate arrays *)
  ; as["sgn"] = ConstantArray[0, totAmide]

  ; print@StringTemplate["`` atoms total"]@numAt;
  ; print@StringTemplate["`` amide groups total"]@totAmide;
  ]

(* ::Section:: *)
(*pars*)


(* ::Text:: *)
(*pdb_flag=.false.*)
(*dcd_flag=.false.*)
(*wt_flag=.false.*)
(*grp_flag=.false.*)
(*IR_flag=.false.*)
(*Raman_flag=.false.*)
(*SFG_flag=.false.*)
(*!saf*)
(*Hamiltonian_flag=.false.*)
(*TransDipMom_flag=.false.*)
(*w_inhom=0.d0*)
(*!saf*)
(*width=5.0*)
(*char_flag=0*)
(*coup_flag=0*)
(*dip_flag=0*)
(*nnc_flag=1*)
(*avgOHmax=10*)
(*slopeOH=1500.0*)
(*OmegaZero=1650.0*)



(* 
DOUBLE PRECISION,DIMENSION(3,0:1) :: Ox, C, N, H
DOUBLE PRECISION,DIMENSION(0:1),INTENT(OUT) :: freq, mass
DOUBLE PRECISION,DIMENSION(0:1),INTENT(OUT) :: nCO_ref, nCN_ref
DOUBLE PRECISION,DIMENSION(4,0:1),INTENT(OUT) :: q_ref, dq_ref
DOUBLE PRECISION,DIMENSION(3,4,0:1),INTENT(OUT) :: dR_ref
DOUBLE PRECISION,DIMENSION(3,0:1),INTENT(OUT) :: RCO_ref, RCN_ref
DOUBLE PRECISION,DIMENSION(74),INTENT(OUT) :: nnp
*)

(* ::Subsection:: *)
(*CALL ref_data(RCO_ref,RCN_ref,nCO_ref,nCN_ref,dR_ref,q_ref,dq_ref,freq,mass,nnp,char_flag)*)

SetAttributes[refData, HoldFirst];
refData[$$_,(*char_flag=0*)0] := Module[{},Null
; dPrint["entering refData[0]"]
(*!B3LYP/aug-cc-pVTZ/PCM(water) MK(dipole) atomic charges*)
(*!NMA*)
  ; $$["freq(0)"]=1667.0243
  ; $$["mass(0)"]=6.4836
  ; $$["C(1,0)"]=0.481166
  ; $$["C(2,0)"]=0.155011
  ; $$["C(3,0)"]=0.000006
  ; $$["q_ref(1,0)"]=0.767223
  ; $$["Ox(1,0)"]=0.396645
  ; $$["Ox(2,0)"]=1.385968
  ; $$["Ox(3,0)"]=0.000003
  ; $$["q_ref(2,0)"]=-0.688647
  ; $$["q_ref(2,0)"]=-0.688647
  ; $$["N(1,0)"]=-0.609994
  ; $$["N(2,0)"]=-0.635192
  ; $$["N(3,0)"]=0.000014
  ; $$["q_ref(3,0)"]=-0.355889
  ; $$["H(1,0)"]=-0.485608
  ; $$["H(2,0)"]=-1.632058
  ; $$["H(3,0)"]=0.000028
  ; $$["q_ref(4,0)"]=0.306550
  ; $$["dR_ref(1,1,0)"]=-0.04287
  ; $$["dR_ref(2,1,0)"]=0.58069
  ; $$["dR_ref(3,1,0)"]=-0.00003
  ; $$["dq_ref(1,0)"]=0.014238
  ; $$["dR_ref(1,2,0)"]=0.02799
  ; $$["dR_ref(2,2,0)"]=-0.33031
  ; $$["dR_ref(3,2,0)"]=-0.00006
  ; $$["dq_ref(2,0)"]=0.019609
  ; $$["dR_ref(1,3,0)"]=0.02759
  ; $$["dR_ref(2,3,0)"]=-0.06363
  ; $$["dR_ref(3,3,0)"]=0.00013
  ; $$["dq_ref(3,0)"]=-0.038632
  ; $$["dR_ref(1,4,0)"]=-0.54387
  ; $$["dR_ref(2,4,0)"]=-0.12117
  ; $$["dR_ref(3,4,0)"]=-0.00087
  ; $$["dq_ref(4,0)"]=0.004784

(* TOOD: ask SR if we need those NMA-deuterated values? *)

(*!NMA-deuterated*)
(*! freq(0)=1658.8465*)
(*! mass(0)=8.5097*)
(*! C(1,0)=0.481166;C(2,0)=0.155011;C(3,0)=0.000006*)
(*! q_ref(1,0)=0.767223*)
(*! Ox(1,0)=0.396645;Ox(2,0)=1.385968;Ox(3,0)=0.000003*)
(*! q_ref(2,0)=-0.688647*)
(*! N(1,0)=-0.609994;N(2,0)=-0.635192;N(3,0)=0.000014*)
(*! q_ref(3,0)=-0.355889*)
(*! H(1,0)=-0.485608;H(2,0)=-1.632058;H(3,0)=0.000028*)
(*! q_ref(4,0)=0.306550*)
(*! dR_ref(1,1,0)=-0.00950;dR_ref(2,1,0)=0.67803;dR_ref(3,1,0)=-0.00004*)
(*! dq_ref(1,0)=0.012329*)
(*! dR_ref(1,2,0)=0.02800;dR_ref(2,2,0)=-0.38918;dR_ref(3,2,0)=-0.00007*)
(*! dq_ref(2,0)=0.021130*)
(*! dR_ref(1,3,0)=-0.03244;dR_ref(2,3,0)=-0.07867;dR_ref(3,3,0)=0.00015*)
(*! dq_ref(3,0)=-0.035187*)
(*! dR_ref(1,4,0)=-0.10931;dR_ref(2,4,0)=-0.09745;dR_ref(3,4,0)=-0.00044*)
(*! dq_ref(4,0)=0.001719*)
(*!DMA*)
  ; $$["freq(1)"]=1640.7010
    ; $$["freq(:)"] = {$$["freq(0)"], $$["freq(1)"]}

  ; $$["mass(1)"]=6.3042
    ; $$["mass(:)"] = {$$["mass(0)"], $$["mass(1)"]}
  
  ; $$["C(1,1)"]=0.720956
  ; $$["C(2,1)"]=-0.289116
  ; $$["C(3,1)"]=0.000004
    ; $$["C(1,:)"] = {$$["C(1,0)"], $$["C(1,1)"]}
    ; $$["C(2,:)"] = {$$["C(2,0)"], $$["C(2,1)"]}
    ; $$["C(3,:)"] = {$$["C(3,0)"], $$["C(3,1)"]}

  ; $$["q_ref(1,1)"]=0.595027
  ; $$["q_ref(2,1)"]=-0.683758
  ; $$["q_ref(3,1)"]=-0.008925
  ; $$["q_ref(4,1)"]=-0.321878
    ; $$["q_ref(1,:)"] = {$$["q_ref(1,0)"], $$["q_ref(1,1)"]}
    ; $$["q_ref(2,:)"] = {$$["q_ref(2,0)"], $$["q_ref(2,1)"]}
    ; $$["q_ref(3,:)"] = {$$["q_ref(3,0)"], $$["q_ref(3,1)"]}
    ; $$["q_ref(4,:)"] = {$$["q_ref(4,0)"], $$["q_ref(4,1)"]}


  ; $$["Ox(1,1)"]=1.067539
  ; $$["Ox(2,1)"]=-1.476352
  ; $$["Ox(3,1)"]=0.000053
    ; $$["Ox(1,:)"] = {$$["Ox(1,0)"], $$["Ox(1,1)"]}
    ; $$["Ox(2,:)"] = {$$["Ox(2,0)"], $$["Ox(2,1)"]}
    ; $$["Ox(3,:)"] = {$$["Ox(3,0)"], $$["Ox(3,1)"]}

  ; $$["N(1,1)"]=-0.584764
  ; $$["N(2,1)"]=0.076942
  ; $$["N(3,1)"]=-0.000028
    ; $$["N(1,:)"] = {$$["N(1,0)"], $$["N(1,1)"]}
    ; $$["N(2,:)"] = {$$["N(2,0)"], $$["N(2,1)"]}
    ; $$["N(3,:)"] = {$$["N(3,0)"], $$["N(3,1)"]}

  ; $$["H(1,1)"]=-1.066336
  ; $$["H(2,1)"]=1.450219
  ; $$["H(3,1)"]=0.000044
    ; $$["H(1,:)"] = {$$["H(1,0)"], $$["H(1,1)"]}
    ; $$["H(2,:)"] = {$$["H(2,0)"], $$["H(2,1)"]}
    ; $$["H(3,:)"] = {$$["H(3,0)"], $$["H(3,1)"]}

  ; $$["dR_ref(1,1,1)"]=-0.19161
  ; $$["dR_ref(2,1,1)"]=0.53200
  ; $$["dR_ref(3,1,1)"]=-0.00005
  ; $$["dR_ref(1,2,1)"]=0.08564
  ; $$["dR_ref(2,2,1)"]=-0.30683
  ; $$["dR_ref(3,2,1)"]=0.00001
  ; $$["dR_ref(1,3,1)"]=0.08449
  ; $$["dR_ref(2,3,1)"]=-0.09115
  ; $$["dR_ref(3,3,1)"]=-0.00012
  ; $$["dR_ref(1,4,1)"]=0.00686
  ; $$["dR_ref(2,4,1)"]=-0.02253
  ; $$["dR_ref(3,4,1)"]=-0.00016
    ; $$["dR_ref(1,1,:)"] = {$$["dR_ref(1,1,0)"], $$["dR_ref(1,1,1)"]}
    ; $$["dR_ref(2,1,:)"] = {$$["dR_ref(2,1,0)"], $$["dR_ref(2,1,1)"]}
    ; $$["dR_ref(3,1,:)"] = {$$["dR_ref(3,1,0)"], $$["dR_ref(3,1,1)"]}
    ; $$["dR_ref(1,2,:)"] = {$$["dR_ref(1,2,0)"], $$["dR_ref(1,2,1)"]}
    ; $$["dR_ref(2,2,:)"] = {$$["dR_ref(2,2,0)"], $$["dR_ref(2,2,1)"]}
    ; $$["dR_ref(3,2,:)"] = {$$["dR_ref(3,2,0)"], $$["dR_ref(3,2,1)"]}
    ; $$["dR_ref(1,3,:)"] = {$$["dR_ref(1,3,0)"], $$["dR_ref(1,3,1)"]}
    ; $$["dR_ref(2,3,:)"] = {$$["dR_ref(2,3,0)"], $$["dR_ref(2,3,1)"]}
    ; $$["dR_ref(3,3,:)"] = {$$["dR_ref(3,3,0)"], $$["dR_ref(3,3,1)"]}
    ; $$["dR_ref(1,4,:)"] = {$$["dR_ref(1,4,0)"], $$["dR_ref(1,4,1)"]}
    ; $$["dR_ref(2,4,:)"] = {$$["dR_ref(2,4,0)"], $$["dR_ref(2,4,1)"]}
    ; $$["dR_ref(3,4,:)"] = {$$["dR_ref(3,4,0)"], $$["dR_ref(3,4,1)"]}

  ; $$["dq_ref(1,1)"]=0.019217
  ; $$["dq_ref(2,1)"]=-0.040956
  ; $$["dq_ref(3,1)"]=0.042806
  ; $$["dq_ref(4,1)"]=-0.021052
    ; $$["dq_ref(1,:)"] = {$$["dq_ref(1,0)"], $$["dq_ref(1,1)"]}
    ; $$["dq_ref(2,:)"] = {$$["dq_ref(2,0)"], $$["dq_ref(2,1)"]}
    ; $$["dq_ref(3,:)"] = {$$["dq_ref(3,0)"], $$["dq_ref(3,1)"]}
    ; $$["dq_ref(4,:)"] = {$$["dq_ref(4,0)"], $$["dq_ref(4,1)"]}
  
  ; refDataLast[$$]
  ; dPrint["exiting refData[0]"]
];

SetAttributes[refDataLast, HoldFirst];
refDataLast[$$_]:= ( Null; 
(* RM *) ; $$["C"] = {$$["C(1,:)"], $$["C(2,:)"], $$["C(3,:)"]}
(* RM *) ; $$["q_ref"] = {$$["q_ref(1,:)"], $$["q_ref(2,:)"], $$["q_ref(3,:)"], $$["q_ref(4,:)"]}
(* RM *) ; $$["Ox"] = {$$["Ox(1,:)"], $$["Ox(2,:)"], $$["Ox(3,:)"]}
(* RM *) ; $$["N"] = {$$["N(1,:)"], $$["N(2,:)"], $$["N(3,:)"]}
(* RM *) ; $$["H"] = {$$["H(1,:)"], $$["H(2,:)"], $$["H(3,:)"]}

(* RM *) ; $$["dR_ref(1)"] = {$$["dR_ref(1,1,:)"], $$["dR_ref(1,2,:)"], $$["dR_ref(1,3,:)"], $$["dR_ref(1,4,:)"]}
(* RM *) ; $$["dR_ref(2)"] = {$$["dR_ref(2,1,:)"], $$["dR_ref(2,2,:)"], $$["dR_ref(2,3,:)"], $$["dR_ref(2,4,:)"]}
(* RM *) ; $$["dR_ref(3)"] = {$$["dR_ref(3,1,:)"], $$["dR_ref(3,2,:)"], $$["dR_ref(3,3,:)"], $$["dR_ref(3,4,:)"]}

(* RM *) ; $$["dR_ref"] = {$$["dR_ref(1)"], $$["dR_ref(2)"], $$["dR_ref(3)"]}

(* RM *) ; $$["dq_ref"] = {$$["dq_ref(1,:)"], $$["dq_ref(2,:)"],$$["dq_ref(3,:)"],$$["dq_ref(4,:)"]}

(*DO i=0,1*)
(* RCO_ref(:,i)=C(:,i)-Ox(:,i)*)
(* RCN_ref(:,i)=C(:,i)-N(:,i)*)
(* nCO_ref(i)=SQRT(SUM(RCO_ref(:,i)**2))*)
(* nCN_ref(i)=SQRT(SUM(RCN_ref(:,i)**2))*)
(*END DO*)
  ; $$["RCO_ref"] = $$["C"] - $$["Ox"]
  ; $$["RCN_ref"] = $$["C"] - $$["N"]
  ; $$["nCO_ref"] = Sqrt[ Total @ ($$["RCO_ref"]^2) ] 
  ; $$["nCN_ref"] = Sqrt[ Total @ ($$["RCN_ref"]^2) ] 
  ; nnpConstruction[$$]
);

refData[$$_,(*"char_flag=1"*)1] := Module[{ f = {#, #}&}, Null
  ; dPrint["entering refData[1]"]
(*	!B3LYP/6-31+G(d), Mulliken atomic charges*)
(*  ; $$["freq(:)"] = f @ 1746.0686*)
  ; $$["freq(:)"] = f @ 1746.0686031562
(*  ; $$["mass(:)"] = f @ 9.8478*)
  ; $$["mass(:)"] = f @ 9.8478005482178

  ; $$["C(1,:)"] = f @ 0.483
  ; $$["C(2,:)"] = f @ 0.160
  ; $$["C(3,:)"] = f @ -0.001

(* DOUBLE PRECISION,DIMENSION(4,0:1) :: q_ref, dq_ref *)
  ; $$["q_ref(1,:)"] = f @  0.603948
  ; $$["q_ref(2,:)"] = f @ -0.534156
  ; $$["q_ref(3,:)"] = f @ -0.515416
  ; $$["q_ref(4,:)"] = f @ 0.390304

(*  Ox(1,:)=0.385;Ox(2,:)=1.385;Ox(3,:)=-0.000 *)
  ; $$["Ox(1,:)"] = f @ 0.385
  ; $$["Ox(2,:)"] = f @ 1.385
  ; $$["Ox(3,:)"] = f @ -0.000
(* N(1,:)=-0.616;N(2,:)=-0.650;N(3,:)=-0.010 *)
  ; $$["N(1,:)"] = f @ -0.616
  ; $$["N(2,:)"] = f @ -0.650
  ; $$["N(3,:)"] = f @ -0.010
(*  H(1,:)=-0.492;H(2,:)=-1.651;H(3,:)=-0.020 *)
  ; $$["H(1,:)"] = f @ -0.492
  ; $$["H(2,:)"] = f @ -1.651
  ; $$["H(3,:)"] = f @ -0.020

(*  DOUBLE PRECISION,DIMENSION(3,4,0:1) :: dR_ref *)
  ; $$["dq_ref(1,:)"] = f @ 0.007716  (* dq_ref(1,:)=0.007716 *)
  ; $$["dq_ref(2,:)"] = f @ 0.018198  (* dq_ref(2,:)=0.018198 *)
  ; $$["dq_ref(3,:)"] = f @ -0.026049 (* dq_ref(3,:)=-0.026049 *)
  ; $$["dq_ref(4,:)"] = f @ 0.000135  (* dq_ref(4,:)=0.000135 *)

(*  dR_ref(1,1,:)=-0.03;dR_ref(2,1,:)=0.73;dR_ref(3,1,:)=0.00 *)
  ; $$["dR_ref(1,1,:)"] = f @ -0.03 
  ; $$["dR_ref(2,1,:)"] = f @ 0.73
  ; $$["dR_ref(3,1,:)"] = f @ 0.00

(*  dR_ref(1,2,:)=0.04;dR_ref(2,2,:)=-0.43;dR_ref(3,2,:)=0.00 *)
  ; $$["dR_ref(1,2,:)"] = f @ 0.04
  ; $$["dR_ref(2,2,:)"] = f @ -0.43
  ; $$["dR_ref(3,2,:)"] = f @ 0.00
(*  dR_ref(1,3,:)=-0.03;dR_ref(2,3,:)=-0.07;dR_ref(3,3,:)=0.00 *)
  ; $$["dR_ref(1,3,:)"] = f @ -0.03
  ; $$["dR_ref(2,3,:)"] = f @ -0.07
  ; $$["dR_ref(3,3,:)"] = f @ 0.00
(*  dR_ref(1,4,:)=-0.10;dR_ref(2,4,:)=-0.10;dR_ref(3,4,:)=0.00*)
  ; $$["dR_ref(1,4,:)"] = f @ -0.10
  ; $$["dR_ref(2,4,:)"] = f @ -0.10
  ; $$["dR_ref(3,4,:)"] = f @ 0.00

  ; refDataLast[$$]
  ; dPrint["exiting refData[1]"]
]


(*!Nearest-neighbor coupling map, stored as Fourier series*)
SetAttributes[nnpConstruction, HoldFirst];
nnpConstruction[$$_]:=Module[{nnp},
$$["nnp"] = {
nnp[1]=2.98049155409105,
nnp[2]=-6.599810192233803,
nnp[3]=-0.30853721377655763,
nnp[4]=0.08082590050798008,
nnp[5]=0.04740097894564941,
nnp[6]=0.008048225450833241,
nnp[7]=0.0015733734467524448,
nnp[8]=-0.9658675048030658,
nnp[9]=-5.22997717307316,
nnp[10]=-0.4018105791392881,
nnp[11]=-0.017339459064999913,
nnp[12]=0.008386549055078336,
nnp[13]=-0.050489074051387244,
nnp[14]=0.006789470425119076,
nnp[15]=-0.3126564488007089,
nnp[16]=-3.7797746273994806,
nnp[17]=-0.11414803857970511,
nnp[18]=-0.00017611006675912795,
nnp[19]=0.12579542585907855,
nnp[20]=0.011124863033873535,
nnp[21]=0.013850235703546394,
nnp[22]=-0.029503792846472005,
nnp[23]=-0.5059330170060446,
nnp[24]=0.19249211456707013,
nnp[25]=0.04314979965266982,
nnp[26]=0.15582397653156857,
nnp[27]=0.00007122142283001677,
nnp[28]=0.03310964759175535,
nnp[29]=-0.05620365560427052,
nnp[30]=-0.09323618884490228,
nnp[31]=0.07271537246962877,
nnp[32]=-0.006111394586803572,
nnp[33]=0.1140144332728223,
nnp[34]=-0.030650858533796854,
nnp[35]=0.010434624767047847,
nnp[36]=-0.006201344264232881,
nnp[37]=-0.07180433223027921,
nnp[38]=0.040607634844420835,
nnp[39]=-0.0979541787497221,
nnp[40]=0.11934199604608554,
nnp[41]=-0.012207576981502277,
nnp[42]=-0.018422318034652232,
nnp[43]=0.01883823305948914,
nnp[44]=0.0424046662659559,
nnp[45]=-0.03887914582205208,
nnp[46]=-0.1022335472962132,
nnp[47]=0.07300790795664054,
nnp[48]=-0.08500015077795386,
nnp[49]=-0.04615152341898034,
nnp[50]=6.610403410038493,
nnp[51]=0.590712804631773,
nnp[52]=-0.24362981965778352,
nnp[53]=-0.08002649779702173,
nnp[54]=0.019711383777822555,
nnp[55]=4.993250818063718,
nnp[56]=0.17452844187043454,
nnp[57]=-0.16960105630340355,
nnp[58]=-0.06764409458606896,
nnp[59]=-0.013064547947709688,
nnp[60]=0.27881995936872217,
nnp[61]=-0.3207748042878569,
nnp[62]=-0.03773019256433872,
nnp[63]=-0.10820787738659833,
nnp[64]=-0.05028414650455027,
nnp[65]=0.02492705580043824,
nnp[66]=0.01010521093108222,
nnp[67]=0.021042805555903196,
nnp[68]=-0.018502096344155176,
nnp[69]=-0.05345701390359108,
nnp[70]=0.06185935268126845,
nnp[71]=-0.01716502455463741,
nnp[72]=0.050050157280630725,
nnp[73]=-0.0820698925785323,
nnp[74]=-0.04129646850913813
}
];
(**)
(*END SUBROUTINE ref_data*)





(* tools *)

compiledQ[f_CompiledFunction] := Module[{}, Needs["CompiledFunctionTools`"]; StringFreeQ[#, "MainEvaluate"] &@ ExportString[CompiledFunctionTools`CompilePrint[f], "Text"] ]

SIGN[x_?NumericQ, y_?NumericQ] := If[y >= 0 , Abs[x], -Abs[x]]




(* DCD loop*)

(*  avgHOi loop *)

(*
avgOHiC = Compile[{{avgOHmax, _Integer}},
    Table[{i, {avgOHMax}]
	];
*)


(* took this apart ... *)
SetAttributes[calculateSgn, HoldFirst];
calculateSgn[$$_] := sgnC[
    $$ @ "dR_ref",
    $$ @ "RCO_ref",
    $$ @ "nCO_ref",
    $$ @ "nCO",
    Array[$$ @ "pro_flag", $$["tot_amide"]]
];

(*print @ " compiling sgnC ";*)

Block[{dRref, RCOref,nCOref,nCO,proFlag, i},
	sgnC = Compile[{ 
	    {dRref, _Real, 3}, 
	    {RCOref, _Real, 2}, 
	    {nCOref, _Real, 1 }, 
	    {nCO, _Real, 1},
	    {proFlag, _Integer, 1}
	    },
	  Module[{aCO, l}, 
	    Table[
	      l = proFlag[[i]]+1;
	      aCO = RCOref[[All, l]] . dRref[[All,1,l]] / (nCOref[[l]]*nCO[[i]]);
	      If[aCO > 0., -1., 1.]
	      , {i,  Length @ proFlag }
	    ]
	  ]
	]
];


(*Echo["green", " sgnC compiledQ : ", sgnC // compiledQ]*)

Clear[dRC];
SetAttributes[calculateDR, HoldFirst];
calculateDR[$$_] := Module[{},
          $$["sgn"] = calculateSgn[$$];
          $$["pro_flag_List"] = Array[$$["pro_flag"], $$["tot_amide"]];
            Apply[dRC] @ Prepend[#, 
              (* ALLOCATE(dR(3,4,tot_amide,2)) *)
              ConstantArray[0., {3, 4, $$["tot_amide"], 2}]
              ]& @ Lookup[$$,
              {"R", 
               "dR_ref", 
               "RCN", 
               "RCN_ref",
               "RCO", 
               "RCO_ref", 
               "pro_flag_List", 
               "nCN_ref", 
               "nCN", "nCO_ref", "nCO", "sgn", "amp"}
            ]
        ]

Block[{dr,dRin,R,dRref,RCN,RCNref,RCO,RCOref,proFlag,nCNref,nCN,nCOref,nCO,sgN,amp,i,j},
dRC = Block[{},
  With[  (* initialize dR *)
      {id = Compile[{{dr, _Real, 4}}, dr]}(*https:// mathematica.stackexchange.com/a/847/29 *)
      ,
      Compile[{  {dRin, _Real, 4}, 
                 {R, _Real, 3}, 
                 {dRref, _Real, 3}, 
                 {RCN, _Real, 2}, 
                 {RCNref, _Real, 2}, 
                 {RCO, _Real, 2}, 
                 {RCOref, _Real, 2}, 
                 {proFlag, _Integer, 1}, 
                 {nCNref, _Real, 1}, 
                 {nCN, _Real, 1},
                 {nCOref, _Real, 1}, 
                 {nCO, _Real, 1},
                 {sgN, _Real, 1},
                 {amp, _Real, 1}
      },
        Module[{dRm, aCN, l, aCO}, 
          dRm = id[dRin];
          Do[
            Table[
              l = proFlag[[i]]+1;
              aCO = RCOref[[All, l]] . dRref[[All,j,l]] / (nCOref[[l]]*nCO[[i]]);
              aCN = RCNref[[All, l]] . dRref[[All,j,l]] / (nCNref[[l]]*nCN[[i]] );
              dRm[[All, j, i, 1]] = R[[All,j,i]] - sgN[[i]]*(amp[[l]]/2)*(aCO*RCO[[All,i]]+aCN*RCN[[All,i]]);
              dRm[[All, j, i, 2]] = R[[All,j,i]] + sgN[[i]]*(amp[[l]]/2)*(aCO*RCO[[All,i]]+aCN*RCN[[All,i]])
            , {i,  Length @ proFlag }
            ]
            ,
            {j, 4}
          ];
          dRm
        ]
      , { {dRm[_], _Real, 4 }}
      ]
  ]]
];
(*dPrint["XXXXXXXXXXXXXX dRC compiledQ :YYYYYYYYYYYYYYYY ", dRC // compiledQ]*)
$AssertFunction = ( Print["no no no !, quitting"];  Quit[])&;
Assert[dRC // compiledQ]

    
avgOHiC =  Block[{r, avgOHmax,numAt,atsw,xyz,Rin},
  With[ {id = Compile[{{r, _Real, 4}}, r]}(*https://
   mathematica.stackexchange.com/a/847/29 *)
   ,
    Compile[{{avgOHmax, _Integer}, {numAt, _Integer}, {atsw, _Integer, 
       1} , {xyz, _Real, 2}, {Rin, _Real, 4}},
       
       (*  IF (i9(1)==1) THEN   ????????????? *) 
     
       Module[ {rin, i, Ci, Oi, Ni, Hi, CAi, avgOHi},
         rin = id@Rin;
         Do[
             Ci = Oi = Ni = Hi = CAi = 1;
             Do[
                Switch[
                   atsw[[i]]
                   ,  1 , rin[[1, 1, Ci, avgOHi]] = xyz[[i, 1]];
                          rin[[2, 1, Ci, avgOHi]] = xyz[[i, 2]];
                          rin[[3, 1, Ci, avgOHi]] = xyz[[i, 3]];
                          Ci = Ci + 1
                   ,  2 , rin[[1, 2, Oi, avgOHi]] = xyz[[i, 1]];
                          rin[[2, 2, Oi, avgOHi]] = xyz[[i, 2]];
                          rin[[3, 2, Oi, avgOHi]] = xyz[[i, 3]];
                          Oi = Oi + 1
                   ,  3 , rin[[1, 3, Ni, avgOHi]] = xyz[[i, 1]];
                          rin[[2, 3, Ni, avgOHi]] = xyz[[i, 2]];
                          rin[[3, 3, Ni, avgOHi]] = xyz[[i, 3]];
                          Ni = Ni + 1
                   ,  4 , rin[[1, 4, Hi, avgOHi]] = xyz[[i, 1]];
                          rin[[2, 4, Hi, avgOHi]] = xyz[[i, 2]];
                          rin[[3, 4, Hi, avgOHi]] = xyz[[i, 3]];
                          Hi = Hi + 1
                   ,  5 , rin[[1, 5, CAi, avgOHi]] = xyz[[i, 1]];
                          rin[[2, 5, CAi, avgOHi]] = xyz[[i, 2]];
                          rin[[3, 5, CAi, avgOHi]] = xyz[[i, 3]];
                          CAi = CAi + 1
                   , _, 0
                 ]
               ,
               {i, numAt}
              ]
            ,
            {avgOHi, 1, avgOHmax }
          ];
         rin
       ]
        ,
        {{rin[_], _Real, 4}}
      ]
  ]
];

(*dPrint[" compiledQ : ", avgOHiC // compiledQ];*)
    

date[]:=DateString[{"ISODateTime", ".", "Millisecond"}];

(*SetAttributes[ptLoop,HoldFirst];*)
ptLoop[dcdFrame_, frame_Integer]:= 
  Module[ {R, RCO, RCN, avgOHi, nCO, mmnCO, nCN, mmnCN, avgOHmax, numat, atsw, Rin, totAmide},
    dPrint["xxxxxxxxxx ", date[], "  entering ptLoop,  frame ", frame];
    If[!ValueQ[$$$], $$$ = $$];
    If[KeyFreeQ[$$$, "frameList"], $$$["frameList"]={}];
(*    Print["frameList doing ", frame, "   ", $$$["frameList"]];*)
    $$$["frameList"] = Append[$$$["frameList"], frame];
    avgOHmax = $$$["avgOHmax"];
  	numat=$$$["num_at"]; 
  	totAmide=$$$["tot_amide"]; 
  	atsw=Array[$$$["atsw"],  $$$["num_at"]]; 
  	Rin=ConstantArray[0.,{3,5, totAmide,$$$@"avgOHmax"}];
    
    $$$["Rin"] = avgOHiC[avgOHmax, numat, atsw, dcdFrame, Rin];
    $$$["nCO_avg"] = ( Map[Norm] @ Transpose[$$$["Rin"][[;;,1,;;,1]] - $$$["Rin"][[;;,2,;;,1]]] ) / avgOHmax;

    Do[ (* avgHOi=1, ovgOHmax !PT Loop *)
        R = $$$["Rin"][[;;,;;,;;,avgOHi]]; (*  R(:,:,:)=Rin(:,:,:,avgOHi) *)
        $$$["R"] = R; (* TODO: clarify why this is done, even though avgOHi can vary *)
        RCO = RCN = ConstantArray[0, Dimensions[R][[{1,3}]]];
        (* 
          !Calculate CO and CN vectors
          !Atom 1 is C
          !Atom 2 is O
          !Atom 3 is N
          !Atom 4 is H
          !Atom 5 is CA
        *)
        RCO = R[[;;,1,All]] - R[[;;,2,All]];
        $$$["RCO"] = RCO;
        RCN = R[[;;,1,All]] - R[[;;,3,All]];
        $$$["RCN"] = RCN;
        (* !Calculate the length of the CO and CN vectors *)
        nCO = Map[Norm] @ Transpose @ RCO;
        mmnCO = MinMax @ nCO;
        If[mmnCO[[1]] < 0.615 || mmnCO[[2]] > 2.46, write["SOMEHTING HAS GONE SERIOUSLY WRONG!!! C=O bond length out of range in frame ", frame]];
        nCN = Map[Norm] @ Transpose @ RCN;
        mmnCN = MinMax @ nCN;
        If[mmnCN[[1]] < 0.663 || mmnCO[[2]] > 2.7, write["SOMEHTING HAS GONE SERIOUSLY WRONG!!! C-N bond length out of range in frame ", frame]];
        (* 
        !This is used to determine the displacement of the atoms of the amide groups along
        !the normal mode of the amide I vibration
        *)
        
        ; $$$["nCO"] = nCO
        ; $$$["nCN"] = nCN
        
        
        ; $$$["sgn"] = calculateSgn[$$$]
        ; $$$["dR"] = calculateDR[$$$]
          (* ALLOCATE(mu(3,tot_amide)) *)
          (* !Transition dipole moment 
             !Calculated as the change in charge with change in coordinates 
          *)
    ; calculateMu[$$$] 
    ; calculateScrAndMuR[$$$] 
    ; $$$["alpha"]=ramanTensor[totAmide, R] 
          (* three above: .3 *)
    ; oneExcitonHamiltonian[$$$] 

      ,  {avgOHi, $$$["avgOHmax"]}
    ]
      (* !Vectors connecting transition dipoles*)
    ; dPrint["xxxxxxxxxx ", date[], "  exiting ptLoop,  frame ", frame];
    ; $$ = $$$
    ; frame -> (Association@@ { "SFG" -> $$$["SFG"], "IR" -> $$$["IR"], "Raman" -> $$$["Raman"], "kappa" -> $$$["kappa"] }) 
  ]
  
    
  ramanTensor[totAmide_Integer, R_List] :=
      Module[ {alpha,avec,bdist,bvec,cdist,cvec,dco,i,j,k,molAlpha,norm,OC,ON,phi,refAxis,theta,trans},
          alpha = dco = ConstantArray[0., {3, 3, totAmide}];
(*      dPrint["entering ramanTensor"];*)
      refAxis = IdentityMatrix[3]//N;
      molAlpha = {0.05, 0.2, 1.};
      Do[OC = R[[;;,1,i]] - R[[;;,2,i]];
         trans = 2*R[[;;,1,i]] - R[[;;,3,i]];
         ON = trans - R[[;;,2,i]];
         phi = ArcCos[OC . ON/(Sqrt[OC . OC]*Sqrt[ON . ON])];
         theta = (34.5*Pi)/180.;
         cdist = (Sqrt[OC . OC]*Sin[theta])/Sin[Pi - theta - phi];
         cvec = R[[;;,2,i]] + (cdist*(trans - R[[;;,2,i]]))/Sqrt[(trans - R[[;;,2,i]]) . (trans - R[[;;,2,i]])];
         cvec = cvec - R[[;;,1,i]];
         norm = Sqrt[cvec . cvec];
         cvec = cvec/norm;
         ON = R[[;;,3,i]] - R[[;;,2,i]];
         phi = ArcCos[OC . ON/(Sqrt[OC . OC]*Sqrt[ON . ON])];
         theta = (55.5*Pi)/180.;
         bdist = (Sqrt[OC . OC]*Sin[theta])/Sin[Pi - theta - phi];
         bvec = R[[;;,2,i]] + (bdist*(R[[;;,3,i]] - R[[;;,2,i]]))/Sqrt[(R[[;;,3,i]] - R[[;;,2,i]]) . (R[[;;,3,i]] - R[[;;,2,i]])];
         bvec = bvec - R[[;;,1,i]];
         norm = Sqrt[bvec . bvec];
         bvec = bvec/norm;
         avec = {0., 0., 0.};
         avec[[1]] = bvec[[2]]*cvec[[3]] - bvec[[3]]*cvec[[2]];
         avec[[2]] = bvec[[3]]*cvec[[1]] - bvec[[1]]*cvec[[3]];
         avec[[3]] = bvec[[1]]*cvec[[2]] - bvec[[2]]*cvec[[1]];
         norm = Sqrt[avec . avec];
         avec = avec/norm;
         Do[dco[[1,j]] = refAxis[[;;,j]] . avec;
            dco[[2,j]] = refAxis[[;;,j]] . bvec;
            dco[[3,j]] = refAxis[[;;,j]] . cvec
            , {j, 3}
         ];
         Do[alpha[[j,j,i]] = alpha[[j,j,i]] + dco[[k,j]]^2*molAlpha[[k]]
           , {j, 3}
           , {k, 3}
         ];
         Do[alpha[[2,3,i]] = alpha[[2,3,i]] + dco[[j,2]]*dco[[j,3]]*molAlpha[[j]];
            alpha[[1,2,i]] = alpha[[1,2,i]] + dco[[j,1]]*dco[[j,2]]*molAlpha[[j]];
            alpha[[1,3,i]] = alpha[[1,3,i]] + dco[[j,1]]*dco[[j,3]]*molAlpha[[j]]
            , {j, 3}
         ];
         alpha[[3,2,i]] = alpha[[2,3,i]];
         alpha[[2,1,i]] = alpha[[1,2,i]];
         alpha[[3,1,i]] = alpha[[1,3,i]]
         , {i, totAmide}
         ];

(*      dPrint["exiting ramanTensor"];*)

      alpha
  ]

SetAttributes[calculateMu,HoldFirst];
calculateMu[as_]:= Module[{mu, qref, dqref, sgn, dR, sig, i, j, k, l}, Null
  ; mu = ConstantArray[0., {3, as["tot_amide"]}]
  ; qref= as["q_ref"]
  ; dqref= as["dq_ref"]
  ; dR = as["dR"] 
  ; sgn = as["sgn"]
  ; sig[1]=-1
  ; sig[2]=1
  ; Do[
    l = 1+as["pro_flag_List"][[i]]
    ; Do[
        Do[ 
          mu[[All, i]] = mu[[All, i]] + sig[k]*dR[[All, j, i, k]]* (qref[[j, l]] + sgn[[i]]*.5*sig[k]*dqref[[j,l]]) 
          ;
     (*     Print[i, " ,", j, " ,",k, " ,", l,  ScientificForm[mu[[All,i]]]]*)
          , {k, 1, 2}
        ] , {j, 1, 4}
      ]
      , {i, 1, as["tot_amide"]}
    ];
  ; as["mu"] = mu
  ; mu
];

SetAttributes[calculateScrAndMuR, HoldFirst];
calculateScrAndMuR[as_] (*/; (as["coup_flag"]==1)*) := If[as["coup_flag"] ==1, Module[{R, scr, totAmide, muR, i, j, dipFlag, proFlagList}, Null
  ; dPrint["entering calculateScrAndMuR"]
  ; {R, totAmide, dipFlag, proFlagList} = Lookup[as, {"R", "tot_amide", "dip_flag", "pro_flag_List"}]
  ; scr = ConstantArray[0., {3, totAmide}]
  ; Do[
      Switch[ 
       as["dip_flag"]
       ,
        0  (* !Transition dipole is placed 70.6% of C=O bond away from the C atom along the C=O bond *)
       ,
        scr[[All, i]] = R[[All,1,i]]  + 0.706*(R[[All,2,i]] - R[[All,1, i ]])
       ,
        1
       ,
        (* 
        !Transition dipole is placed 54.1% of C=O bond away from the C atom along the C=O bond
        !and 19.4% of C-N bond away from the C atom along the C-N bond
        *)
        scr[[All, i]] = R[[All,1,i]] + 0.541*(R[[All,2,i]] - R[[All,1, i ]]) + (0.194*(R[[All,3,i]]-R[[All,1,i]]))
       ,
        2
        (* 
        !Transition dipole is placed at the center of mass of the amide group
            IF (pro_flag(i)==1) THEN
              scr(:,i)=(12.0*R(:,1,i)+16.0*R(:,2,i)+14.0*R(:,3,i)+12.0*R(:,4,i))/(12.0+16.0+14.0+12.0)
            ELSE
              scr(:,i)=(12.0*R(:,1,i)+16.0*R(:,2,i)+14.0*R(:,3,i)+1.0*R(:,4,i))/(12.0+16.0+14.0+1.0)
            END IF
        *)
       ,
        If[ proFlagList[[i]] == 1
          ,
          scr[[All, i]] = (12*R[[All,1,i]] + 16*R[[All,2,i]] + 14*R[[All,3,i]] + 12*R[[All,4,i]]/(12 + 16 + 14 + 12))
          ,
          scr[[All, i]] = (12*R[[All,1,i]] + 16*R[[All,2,i]] + 14*R[[All,3,i]] + 1 *R[[All,4,i]]/(12 + 16 + 14 + 1))
        ]
      ]
    , {i, totAmide}
    ]
  (* alternatively:
  ; scr = Transpose @ Table[ R[[;;,1,i]] 0.706 + R[[;;,2,i]] - R[[;;,1, i ]], {i, as["tot_amide"]}]
  *)
  (*
  ; muR = ConstantArray[0.,{3, totAmide, totAmide}];
  ; Do[muR[[;;,i,j]]= scr[[;;,i]] - scr[[;;,j]], {i, totAmide},{j, totAmide}] 
  *)
  ; muR = murscrC[ scr ]

(*
  ; With[{srct = Transpose[scr]}, Outer[Subtract, srct, srct, 1]; muR = Array[tt[[All, All, #]] &, 3]]
*)

  ; as["scr"] = scr
  ; as["mu_R"] = muR
  ; Clear[scr, muR]
  ; dPrint["exiting calculateScrAndMuR"]
]]

(*equivalent to Do[muR[[;;,i,j]]= scr[[;;,i]] - scr[[;;,j]], {i, totAmide},{j, totAmide}] *)
murscrC = Module[{scr},
Compile[{{scr,_Real,2}},
  Module[{srct=Transpose[scr],t},
    t=Outer[Subtract, srct,srct,1];
    {t[[All,All,1]],t[[All,All,2]],t[[All,All,3]]}]]
];



SetAttributes[oneExcitonHamiltonian, HoldFirst];
oneExcitonHamiltonian[as_]:= Module[
	{i, j, k, l, ii, jj, q1,q2,q3,q4, r1, r2, r3, r4, qref, dqref, kappa, sgn, dR, pf, nchains, totAmide, 
	numAmide, mu, muR, R,  nnp, OmegaZero, slopeOH, winhom, grand, A, eigenMu, AA, AT, eigenAlpha, alpha, anisoAlpha, isoAlpha, beta, wt, W(*, awts*)},
 {qref,    dqref,      sgn, dR,    pf,   totAmide,   numAmide,     nchains,   mu,   muR, R, nnp, OmegaZero, slopeOH, winhom} = Lookup[as, 
 {"q_ref", "dq_ref", "sgn", "dR", "pf", "tot_amide", "num_amide", "nchains", "mu", "mu_R", "R", "nnp", "OmegaZero", "slopeOH", "w_inhom"}];
 dPrint["entering oneExcitonHamiltonian "];
 kappa = ConstantArray[0., {totAmide, totAmide}];
 as["kappa"] = kappa;
 
 If[as["coup_flag"] == 0
 	(*!Transition charge method for coupling (TCC/TCI)*)
 	,
	 Do[ (* i-2, tot_amide *)
	 	ii = as["pro_flag"][i];

	 	Do[ (* j-1, i-1 *)
	      jj = as["pro_flag"][j];
	      Do[(*k=1,4   l=1,4 *)
	        q1=(qref[[k,ii]]-0.5*sgn[[i]]*dqref[[k,ii]])*(qref[[l,jj]]-0.5*sgn[[j]]*dqref[[l,jj]]);
	        q2=(qref[[k,ii]]+0.5*sgn[[i]]*dqref[[k,ii]])*(qref[[l,jj]]-0.5*sgn[[j]]*dqref[[l,jj]]);
	        q3=(qref[[k,ii]]-0.5*sgn[[i]]*dqref[[k,ii]])*(qref[[l,jj]]+0.5*sgn[[j]]*dqref[[l,jj]]);
	        q4=(qref[[k,ii]]+0.5*sgn[[i]]*dqref[[k,ii]])*(qref[[l,jj]]+0.5*sgn[[j]]*dqref[[l,jj]]);
	
	        r1=Norm[ dR[[;;,k,i,1]] - dR[[;;,l,j,1]] ];
	        r2=Norm[ dR[[;;,k,i,2]] - dR[[;;,l,j,1]] ];
	        r3=Norm[ dR[[;;,k,i,1]] - dR[[;;,l,j,2]] ];
	        r4=Norm[ dR[[;;,k,i,2]] - dR[[;;,l,j,2]] ];
	        kappa[[i,j]] = kappa[i,j] + (q1/r1+q4/r4-q2/r2-q3/r3)
	      	, {k, 4}, {l, 4}
	      ] ;
	      kappa[[i,j]] = pf*kappa[[i,j]];
	      kappa[[j,i]] = kappa[[i,j]]
	 	  , {j, i - 1}
	 	]
	   , {i, 2, totAmide}
	 ]
  ]
  ;
  If[as["coup_flag"] == 1
  	(* !Transition dipole method for coupling (TDC )*)
 	,  
 	kappa = calculateKappa[as]
  ]; (* end coup_flag *)

    
   If[ as["nnc_flag"] == 1
 		,
     	(* !Calculate nearest-neighbor coupling from dihedral map *)
     	kappa = modifyOffDiagonalsKappa[ kappa, as]

   ]; (* end If nnc_flag *)
   
(*   !Linear relationship between C=O bond length and frequency*)
   Do[
     If[ as["pro_flag"][i] == 1
         ,
         kappa[[i, i]] = OmegaZero-26.3+(-slopeOH*(as["nCO_avg"][[i]]-1.232))
         ,
(*  TODO: Ask SR if this is needed *)
         (*!For deuterated
           !    kappa(i,i)=1631.9+(-(slopeOH)*(nCO_avg(i)-1.232))
         *)
         kappa[[i, i]] = OmegaZero+(-slopeOH*(as["nCO_avg"][[i]]-1.229))
     ]
     , {i, totAmide}
   ] ;
   
  If[ winhom != 0
  	,
    grand = gasdev[totAmide]; (*  RandomVariate[ NormalDistribution[0,1], totAmide]*)
    Do[
      kappa[[i,i]]=kappa[[i,i]] + winhom*grand[[i]]	
      , {i, totAmide}
    ]
  ];
(* careful !!  need to update here again*)
 as["kappa"] = kappa;
  
   A = Developer`ToPackedArray @ Re @ kappa;
   as["A"] = A;
   dPrint["starting EigenSystem "];
   timeIt[{W, AA} = Eigensystem[A]];
   dPrint["finished EigenSystem "];
   AT=Transpose@AA;
   as["AT"] = AT; (* TODO: optimize this for big matrices ?*)
   

(* !Calculate IR and Raman responses of eigenmodes*)
   eigenMu = ConstantArray[0.0, {3, totAmide}];
   eigenAlpha = ConstantArray[0.0, {3, 3, totAmide}];
   If[as["grp_flag"], readBreakDownFile[]]; (* TODO: ask SR for a sample breakdown file *)
   
   alpha = as["alpha"];

   (*timeIt @ *)
   If[
   	as["grp_flag"],
   Do[
(*!4.803 converts the transition dipole to Debye*)
    (*eigenMu[[All,i]] = eigenMu[[All,i]] + AT[[j,i]]*mu[[All,j]]*4.803;*)
    If[as["grp_flag"], calculateAWTS[i,j, A]]; (* TODO: implement calculateAWTS*)

(*
    eigenAlpha[[All, All, i]] = eigenAlpha[[All, All, i]] + AT[[j,i]]*(*$$["alpha"]*)alpha[[All,All,j]]
*)

   	 , {i, totAmide}, {j, totAmide}
   ]
   ];
   
   	eigenMu = 4.803 Transpose[AA . Transpose[mu]];
   	eigenAlpha = alpha.AT;

   as["eigenMu"] = eigenMu; as["eigenAlpha"] = eigenAlpha; (* Clear[eigenM, eigenAlpha] *)
   
   (*!Calculate isotropic Raman intensity*)
   isoAlpha = anisoAlpha = ConstantArray[0., {totAmide}];
   Do[
     Do[
        isoAlpha[[i]] = isoAlpha[[i]] + eigenAlpha[[j,j,i]]
      , {j, 3}
    ];
    isoAlpha[[i]] = 1/9*isoAlpha[[i]]^2;
    anisoAlpha[[i]] = anisoAlpha[[i]]+0.5*((eigenAlpha[[1,1,i]]-eigenAlpha[[2,2,i]])^2+
        (eigenAlpha[[2,2,i]]-eigenAlpha[[3,3,i]])^2+(eigenAlpha[[3,3,i]]-eigenAlpha[[1,1,i]])^2)+
        6.0*(eigenAlpha[[1,2,i]]^2+eigenAlpha[[2,3,i]]^2+eigenAlpha[[1,3,i]]^2)

    , {i, totAmide}
   ];
   (* !Calculate hyperpolarizability *)
   beta = ConstantArray[0., {3,3,3,totAmide}];
   Do[ beta[[j,k,All,i]]=eigenMu[[All,i]]*eigenAlpha[[j,k,i]]
   	 , {i,totAmide}, {j,3}, {k,3}
   ] ;
   as["beta"] = beta;
   wt = 1;
   If[!KeyFreeQ[as, "wtname"]
   	  ,
   	  as["wt_flag"] = Quiet[TrueQ[FileExistsQ[as["wtname"]]]];
   	  ; If[as["wt_flag"] === True,
   	  	   wt = readWeightingFromFile[as["wtname"]]
   	  ]
   ];
   as["wt"] = wt;
   

   calculateSpectra[as, eigenMu, isoAlpha, anisoAlpha, W,
	             totAmide,
	             as@"grp_flag", as@"IR_flag", as@"Raman_flag"
   ](* // timeIt 83 ms *);
   
   

 dPrint["exiting oneExcitonHamiltonian ", $KernelID];


(* oneExcitonHamiltonian *)
]

(*
    k = 1;
     IR = ConstantArray[0., $$["nspts"]];
	Do[ (* i=spec_min,spec_max*)
	  Do[ (* j=1,tot_amide *)
            IR[[k]]=IR[[k]]+wt*Abs[(Norm[eigenMu[[All,j]]])/(W[[j]]-i-I*width)]^2
	    , {j, totAmide}
	  ]
	  ; k = k + 1
	  , {i, specMin, specMax}
	];
	
	IR2 = Module[{i = Range[specMin, specMax]},
	  Sum[ wt*Abs[(Norm[eigenMu[[All, j]]])/(W[[j]] - i - I*width)]^2, {j, totAmide}]
	]
	
*)

SetAttributes[calculateSpectra, HoldFirst];
calculateSpectra[as_,eigenMu_, isoAlpha_, anisoAlpha_, W_List (* eigen values *),
	             totAmide_Integer,
	             grpFlag_?BooleanQ, IRFlag_?BooleanQ, RamanFlag_?BooleanQ
    ]:= Module[{k, IR, Raman, IRgrp, width, wt, awts,j, Ramgrp, ngrps, beta, SFG},
  k = 1;

  If[grpFlag
  	 , 
  	 IRgrp = ConstantArray[0., {as["ngrps"], as["nspts"]}];
  	 Ramgrp = ConstantArray[0., {as["ngrps"], 2, as["nspts"]}]
  ];
  {width, wt, ngrps, beta} = Lookup[as, {"width", "wt", "ngrps", "beta"}];
  If[grpFlag, awts = as["awts"] ];
  (*dPrint["starting IR for frame ", as["frames"]+1 ]*);
  IR = With[{iii = Range[as["spec_min"], as["spec_max"]]}, 
  	     Sum[wt*Abs[(Norm[eigenMu[[All, j]]])/(W[[j]] - iii - I*width)]^2, {j, totAmide}]
  	   ];
  (*dPrint["finished IR for frame ", as["frames"]+1]*);

  (*dPrint["starting Raman for frame ", as["frames"]+1]*);
  Raman = With[{iii = Range[as["spec_min"], as["spec_max"]]}, 
          Sum[{ wt*Abs[(isoAlpha[[j]]+(2.0/15.0)*anisoAlpha[[j]])/(W[[j]]-iii-I*width)]^2
  	             , 
  	             wt*Abs[0.1*anisoAlpha[[j]]/(W[[j]]-iii-I*width)]^2
               }, {j, totAmide}
            ]
  	   ];
   (*dPrint["finished Raman for frame ", as["frames"]+1]*);

   
(* 

!Missing hbar so absolute intensities won't be right but relatives are good
!PPP not implemented yet because prefactors (n) are not calculated
!Missing prefactors depend on indices of refraction and angles of light beams
!See J. Phys. Chem. A 117 6311-6322 (2013) for definitions of prefactors
!and other expressions
!(1)SSP -> n1*(XXZ+YYZ)/2
!(2)SPS -> n2*(XZX+YZY)/2
!(3)PSS -> n3*(ZXX+ZYY)/2
!PPP -> n4*(XXZ+YYZ)/2+n5*(XZX+YZY)/2+n6*(ZXX+ZYY)/2+n7*ZZZ
!(4)ZZZ
   
! For all of the 3 pol. combs. below (PSP,SPP and PPS, respectively; each composed of 2 hyperpolarizability tensor elements), we assume that, due to (hopefully) sufficient sampling over phi [and psi?], we only have to calculate the cos[theta] component (with theta=0) of the macroscopic hyp. pols.
*)
   
If[as["SFG_flag"]
  ,

(*  dPrint["starting  SFG"];*)

With[{i = Range[as["spec_min"], as["spec_max"]]}, 
  SFG = Sum[{
    (*1*) wt*(-1.0/2.0)*((beta[[3,2,1,j]]-beta[[3,1,2,j]]/2.0)/(W[[j]]-i-I*width)), (* !ChiZYX *)
    (*2*) wt*(-1.0/2.0)*((beta[[1,2,3,j]]-beta[[2,1,3,j]]/2.0)/(W[[j]]-i-I*width)), (* !ChiXYZ *)
    (*3*) wt*(-1.0/2.0)*((beta[[2,3,1,j]]-beta[[1,3,2,j]]/2.0)/(W[[j]]-i-I*width)), (* !ChiYZX *)
    (*4*) wt*(-1.0/2.0)*((beta[[2,1,3,j]]-beta[[1,2,3,j]]/2.0)/(W[[j]]-i-I*width)), (* !ChiYXZ *)
    (*5*) wt*(-1.0/2.0)*((beta[[3,1,2,j]]-beta[[3,2,1,j]]/2.0)/(W[[j]]-i-I*width)), (* !ChiZXY *)
    (*6*) wt*(-1.0/2.0)*((beta[[1,3,2,j]]-beta[[2,3,1,j]]/2.0)/(W[[j]]-i-I*width)), (* !ChiXZY *)
    (*7*) wt*(-1.0/2.0)*((beta[[1,1,3,j]]+beta[[2,2,3,j]]/2.0)/(W[[j]]-i-I*width)), (* !ChiYYZ *)
    (*8*) wt*(-1.0/2.0)*((beta[[1,3,1,j]]+beta[[2,3,2,j]]/2.0)/(W[[j]]-i-I*width)), (* !ChiYZY *)
    (*9*) wt*(-1.0/2.0)*((beta[[3,1,1,j]]+beta[[3,2,2,j]]/2.0)/(W[[j]]-i-I*width)), (* !ChiZYY *)
    (*10*)wt*(-1.0/2.0)*((beta[[3,3,3,j]])/(W[[j]]-i-I*width)) (* !ChiZZZ *)
    }, {j, totAmide}
  ]
];
as["SFG"] = (*as["SFG"] + *)SFG;

dPrint["finished SFG"];

];
   
as["IR"] = (*as["IR"] + *)IR;
as["Raman"] = (*as["Raman"] +*) Raman;
]



(* 
data = RandomVariate[NormalDistribution[0, 1], 10^4];
gdata = gasdev[10^4];
Show[
 Histogram[#, 100, "ProbabilityDensity"],
 Plot[PDF[NormalDistribution[0, 1], x], {x, -3, 3}, 
  PlotStyle -> Thick], ImageSize -> 123
  ]& /@ {data,gdata}
  
*)
gasdev[n_] := RandomVariate[NormalDistribution[0,1],n];

slowGasdev[n_Integer]:= Module[{ zeta, gr, i, s},
  gr = ConstantArray[0., n];
    Do[ 
      s=10.;
      Until[ !( s == 0 || s >= 1)
        ,
        zeta = RandomReal[{0,1}, 2];
        zeta[[1]] = 2 (zeta[[1]] - .5);
        zeta[[2]] = 2 (zeta[[2]] - .5);
        s = Total[zeta^2];
      ];
      gr[[i]] = zeta[[1]]*Sqrt[(-2*Log[s]/s)];
      If[ (i + 1) <= n
          ,
          gr[[i+1]]=zeta[[2]]*Sqrt[(-2*Log[s])/s]
      ]
      ,
      {i, 1, n, 2}
    ];
   gr
   ]
 

(* !Calculates Phi and Psi dihedral angles *)
dihedralOld[ R1_List, R2_List, R3_List, R4_List ] :=
    Module[ {v21,v23,v34,dihScr,vc},
        v21 = R1 - R2;
        v23 = R3 - R2;
        v34 = R4 - R3;
        dihScr = Total[v21*v23]/Total[v23^2];
        v21 = v21-dihScr*v23;
        dihScr = Total[v34*v23]/Total[v23^2];
        v34 = v34-dihScr*v23;
        dihScr = Total[v21*v34/(Sqrt[Total[v21^2]]*Sqrt[Total[v34^2]])];
        If[ Abs[(Abs[dihScr]-1)] < 10^-5,
            dihScr = SIGN[1, dihScr]
        ];
        dihScr = (180.0/Pi)*ArcCos[dihScr];
        vc = { 
            (* vc(1)= *) v21[[2]]*v34[[3]]-v21[[3]]*v34[[2]] ,
            (* vc(2)= *) v21[[3]]*v34[[1]]-v21[[1]]*v34[[3]] ,
            (* vc(3)= *) v21[[1]]*v34[[2]]-v21[[2]]*v34[[1]] 
        };
        If[ Total[ vc*v23 ] < 0,
            dihScr = - dihScr
        ];
        dihScr
    ];
    
Clear[dihedral];
Block[{x,y,R1,R2,R3,R4},
With[{SIGNC = Compile[{{x,_Real}, {y,_Real}}, If[y >= 0., Abs[x], -Abs[x]]]
     },
    dihedral = Compile[{{R1,_Real,1},{R2,_Real,1},{R3,_Real,1},{R4,_Real,1}},
    Module[ {v21,v23,v34,dihScr,vc},
        v21 = R1 - R2;
        v23 = R3 - R2;
        v34 = R4 - R3;
        dihScr = Total[v21*v23]/Total[v23^2];
        v21 = v21-dihScr*v23;
        dihScr = Total[v34*v23]/Total[v23^2];
        v34 = v34-dihScr*v23;
        dihScr = Total[v21*v34/(Sqrt[Total[v21^2]]*Sqrt[Total[v34^2]])];
        If[ Abs[(Abs[dihScr]-1)] < 10^-5,
            dihScr = SIGNC[1., dihScr]
        ];
        dihScr = (180.0/Pi)*ArcCos[dihScr];
        vc = { 
            (* vc(1)= *) v21[[2]]*v34[[3]]-v21[[3]]*v34[[2]] ,
            (* vc(2)= *) v21[[3]]*v34[[1]]-v21[[1]]*v34[[3]] ,
            (* vc(3)= *) v21[[1]]*v34[[2]]-v21[[2]]*v34[[1]] 
        };
        If[ Total[ vc*v23 ] < 0,
            dihScr = - dihScr
        ];
        dihScr
    ]
    ]
]
]
   
   SetAttributes[cmd2Rules, HoldFirst];
   cmd2Rules[as_(*Association*), s_String] :=
       Module[ {args,s2,argSubst,tmp,todo},
           s2 = FixedPoint[StringReplace[#,"  " -> " "]&, s];
           args = StringTrim/@StringSplit[#,"-"]&@s2;
           args = StringSplit[#," "]& /@ args;
           argSubst = {"wt"->"wtname", "grps"->"grp_flag","IR" -> "IR_flag", "Raman"->"Raman_flag",
           "SFG"->"SFG_flag", "Ham"->"Hamiltonian_flag", "TDM"->"TransDipMom_flag",
           "inhom"->"w_inhom","charge"->"char_flag", "coup"->"coup_flag",
           "dip"->"dip_flag", "nncm" -> "nnc_flag", "avgOH"->"avgOHmax",
           "mOH"->"slopeOH","Omega0"->"OmegaZero"};
           tmp = (args/.argSubst)/.{x_String}:> {x, True};
           todo = Inactive[Set][Inactive[as][#1], ToExpression[#2]]&@@@tmp;
        (*   TODO=todo;*)
           Activate @ todo
       ];
(*cmd2Rules @ "-IR -coup 1 -dip 0 -nncm 1 -avgOH 1 -mOH 410 -Omega0 1650"*)

SetAttributes[main, HoldFirst];

main[as_, pdbFile_String?FileExistsQ, dcdFile_String?FileExistsQ,
	 cmdOptions_String
  ]:= Module[{normal, start = AbsoluteTime[], dcdFrame}, Null 
  ; $TIME=0;
  ; dPrint["entering main ", date[]]
  ; cmd2Rules[as, cmdOptions]
  ; importData[as,  pdbFile, dcdFile]
  ; refData[as,  as["char_flag"] ]
  ; readPDB[as](* no argument needed, since importData populates $$["pdbLines"] *)
   (* amp(:)=1.E10*SQRT(6.62607E-34/(8.E0*PI*PI*freq(:)*2.9979E10*mass(:)*1.6605402E-27)) *)
  ; as["amp(:)"] = 1.*10^10*Sqrt[(6.62607*10^-34)/(8.*Pi*Pi*as["freq(:)"]*2.9979*10^10*as["mass(:)"]*1.6605402*10^-27)]
  ; as["amp"] = as["amp(:)"]
  ; as["pf"]= ((1.602 10^-19)^2*10^10)/(4.*Pi*8.854 10^-12*6.62607 10^-34*2.9979 10^10)
  ; as["IR"] = ConstantArray[0., as["nspts"] ]
  ; as["Raman"] = ConstantArray[0., {2, as["nspts"]} ]
  ; as["avg_kappa"] = ConstantArray[0., as["tot_amide"]]
  
  ; dPrint["start ImportDCD"]
  ; allDCDFrames = ImportDCD[dcdFile]
  ; dPrint["finished ImportDCD"]
  ; If[$TEST, allDCDFrames = allDCDFrames[[1;;128]]]
  ; dcdList = Transpose[{Range[Length @ allDCDFrames], allDCDFrames}]
  ; as["frames"] = 0  (* just to be conistent witht the Fortran code *)
  ; as["IR"] = ConstantArray[0., as["nspts"]]
  ; as["Raman"] = ConstantArray[0., {2, as["nspts"]}]
  ; as["SFG"] = ConstantArray[0., {10, as["nspts"]}]
  ; as["frames"] = Length@allDCDFrames
  ; dcdcount = 0
  ; tab = If[$parallel===True , ParallelTable , Table ]
  ; If[$parallel===True, DistributeDefinitions[as]](* synchronize $$ , kind of common block ...*)
  ; dcdLoopResults =  With[{
      tabl= tab, dcds = dcdList
  	  },
     tabl[ (* DCD loop *) Null
  	   ; dPrint["Doing DCD frame ", dcdFrame[[1]]]
  	   (*
  	   ; If[IntegerQ[dcdFrame[[1]]], dPrint["green", " frame ", dcdFrame[[1]]," id: ", $KernelID, " time used = ",Round[AbsoluteTime[] -start,.1 ]]]
  	   *)
  	 
       (* $$$ is "local" to each subkernel ... *)
       (* just need to return all results from ptLoop *)
       ; pres = ptLoop[dcdFrame[[2]] (*frame *), dcdFrame[[1]]]
(*       ; BRP[ScientificForm[Column[$$["SFG"][[1;;7,1]]] ]]*)
       ; pres
       , {dcdFrame, dcds}
    ]]
(*  ; cPrint["green", "time needed for DCD loop ", Round[AbsoluteTime[] - start, .1]]*)
  ; dcdLoopResultsAsso = Association @@ {dcdLoopResults}
  

  ; as["SFGSum"] = dcdLoopResultsAsso[[All, "SFG"]] // Values // Total
  ; as["IRSum"] = dcdLoopResultsAsso[[All, "IR"]] // Values // Total
  ; as["RamanSum"] = dcdLoopResultsAsso[[All, "Raman"]] // Values // Total
  ; as["avg_kappa"] = (dcdLoopResultsAsso[[All, "kappa"]] // Values // Total) / as["frames"]

  ; dPrint["averaging avg_kappa over ", as["frames"]," frames"]
  ; as["avg_kappa"] = as["avg_kappa"]/as["frames"]
  (*!Normalization constant so that intensity spectra integrate to 1 between 1500 and 1800 *)
  ; normal = ConstantArray[0., 3]
  ; Do[ (* i=1, nspts *)
  	   If[ 
  	   	 (i == 1) || (i == as["nspts"])
  	   	 ,
  	   	 normal[[1]]=normal[[1]] + as["IRSum"][[i]]/2;
  	   	 normal[[2]]=normal[[2]] + as["RamanSum"][[1,i]]/2;
  	   	 normal[[3]]=normal[[3]] + Abs[ as["SFGSum"][[1,i]]^2 ]/2
  	   	 ,
  	   	 normal[[1]]=normal[[1]] + as["IRSum"][[i]];
  	   	 normal[[2]]=normal[[2]] + as["RamanSum"][[1,i]];
  	   	 normal[[3]]=normal[[3]] + Abs[ as["SFGSum"][[1,i]]^2  ]
  	   ]
  	   , {i, as["nspts"]}
    ]
  ; as["normal"] = normal
  
  ; If[as["IR_flag"]
  	   ,
  	   print["Linewidth=", as["width"]]
  	   ; irDatFile = StringReplace[pdbFile, ".pdb" -> "_IR_M.dat"]
  	   ; Export[ irDatFile, formatIR[], "Text"]
    ]
  
  ; If[as["Raman_flag"]
  	   ,
  	     raDatFile = StringReplace[pdbFile, ".pdb" -> "_Raman_M.dat"]
  	   ; Export[ raDatFile, formatRaman[], "Text"]
    ]
  ; If[as["SFG_flag"]
  	,
  	  $$["SFGNorm"] = formatSFG[]
  	  ; sfgDatFile = StringReplace[pdbFile, ".pdb" -> "_SFG_M.dat"]
      ; Export[ sfgDatFile, formatSFG[], "Text"]
  ]
  
  ; print["EXITING main ", date[]," time used: ", Round[AbsoluteTime[] - start, .01], " seconds"]

];


SetAttributes[calculateKappa, HoldFirst];
calculateKappa[$$_]:= Module[{totAmide},
	dPrint["entering calculateKappa"];
	totAmide = $$["tot_amide"];
	$$["kappa"] = 
	calculateKappaC @@  {
		ConstantArray[0., {totAmide, totAmide}],
		totAmide,  
		$$["pf"],
		$$["mu_R"],
		$$["mu"]
	};
	dPrint["exiting calculateKappa"];
	$$["kappa"] 
]
    
calculateKappaC =  Module[{r,totAmide, pf, muR, mu, kappaInOut, kappa },
  With[ {id = Compile[{{r, _Real, 2}}, r]}(*https:// mathematica.stackexchange.com/a/847/29 *)
   ,
    Compile[{{kappaInOut,_Real,2}, {totAmide, _Integer}, {pf, _Real}, {muR,_Real,3}, {mu, _Real,2}
    },
       
       Module[ {kappa, i=0, j=0},
         kappa = id@kappaInOut;

Do[
      Do[
      	kappa[[i,j]] = pf*
      	  (
      	    ( mu[[All,i]] . mu[[All,j]] )/( Sqrt[ Total[muR[[All,i,j]]^2] ]^3 ) -
              3*(muR[[All,i,j]] . mu[[All,i]] * (muR[[All,i,j]] . mu[[All,j]])
                )/( Sqrt[Total[muR[[All,i,j]]^2]]^5 )
      	  );
      	kappa[[j,i]] = kappa[[i,j]]
      	, 
      	{j, i-1}
      ]
      ,
      {i, 2, totAmide}
    ]; 
    kappa

       ]
        ,
        {{kappa[_], _Real, 2}}
      ]
  ]
];
(*print["calculateKappaC compiled : ", compiledQ @ calculateKappaC];*)

SetAttributes[modifyOffDiagonalsKappa, HoldFirst]
modifyOffDiagonalsKappa[kappa_, as_Association] := Module[{nchains, numAmide, totAmide, R, nnp,offDiagonals, ii, flatList, nnc},
 {nchains, numAmide, totAmide, R, nchains, nnp} =  Lookup[as, {"nchains", "num_amide", "tot_amide", "R", "nchains", "nnp"}];
 dPrint["entering modifyOffDiagonalsKappa ", $KernelID];
 (*
  Export["/tmp/test.mx", {ConstantArray[0.,{totAmide, totAmide}],R, numAmide, nchains, nnp}];
  *)

 flatList  = doOffDiagonalsKappaLoopC[ConstantArray[0.,{totAmide, totAmide}],R, numAmide, nchains, nnp];
 nnc = First @ flatList;
 offDiagonals= ArrayReshape[flatList[[2;; (totAmide^2)]], {totAmide, totAmide}];
 Do[
 	kappa[[ii, ii+1]] = 
 	kappa[[ii+1, ii]] = offDiagonals[[ii,ii+1]]
   ,
   {ii, 1, Length[kappa]-1}
 ];
 dPrint["exiting modifyOffDiagonalsKappa kappa[[1,1]] = ", kappa[[1,1]]];
 kappa
]

doOffDiagonalsKappaLoopC =  (* returns the modified kappa *)
Block[{kappaIn, R, numAmide,nchains, nnp,r},
With[{id = Compile[{{r, _Real, 2}}, r]}, (*https:// mathematica.stackexchange.com/a/847/29 *)
  Compile[{{kappaIn,_Real, 2}, {R, _Real, 3}, {numAmide,_Integer,1}, {nchains, _Integer}, {nnp,_Real,1}},
  	Module[{phi,psi,nnc=0.,k,j,ii,i, l, kappa},
    ii = 1; kappa = id @ kappaIn;
	Do[ (* l=1;nchains *)
		   Do[ (* i=1, num_amide(l) -1 *)
		     phi = dihedral[R[[All,1,ii]], R[[All,3,ii]], R[[All,5,ii]], R[[All,1,ii+1]]];
	         psi = dihedral[R[[All,3,ii]], R[[All,5,ii]], R[[All,1,ii+1]],R[[All,3,ii+1]]];
	         nnc = 0.0;
	         Do[
	           nnc=nnc+nnp[[j+1+k*7]]*Cos[j*(psi/180.0)*Pi]*Cos[k*(phi/180.0)*Pi]
	           ,
	           {k,0,6}, {j,0,6}
	         ];
	         Do[
	           nnc = nnc + nnp[[j+49+(k-1)*5]]*Sin[j*(psi/180.0)*Pi]*Sin[k*(phi/180.0)*Pi]
	           ,
	           {j, 5}, {k, 5}
	         ];
	         kappa[[ii, ii+1]] = nnc;
	         kappa[[ii+1, ii]] = nnc;
	         ii = ii + 1;
	 	     , {i, 1, numAmide[[l]]-1 }
		   ] ; (* end i - loop *)
		   If[ numAmide[[l]] > 0, ii = ii + 1 ]
	     , 
	    	 {l, nchains}
	  
	]; 
	Prepend[Flatten@kappa, nnc]
]]]]


numForm[r_, prec_] := 
 NumberForm[r /. Except[_Real] :> N[r], {prec, prec},
  ExponentFunction -> (# &),
  NumberSigns -> {"-", " "},
  NumberFormat -> 
   Function@StringJoin[#1, "E", exp[#3]] (*constructs output*)]
exp[s_] := StringReplace[s,
       {
        "-" ~~ d : DigitCharacter .. :> 
     "-" <> StringPadLeft[d, Max[2, StringLength[d]], "0"],
        d : DigitCharacter .. :> 
     "+" <> StringPadLeft[d, Max[2, StringLength[d]], "0"]
        }
   ];
   
   
formatIR[] :=
    Module[ {frange, freqs, ints,intsIR},
        frange = Lookup[$$, {"spec_min","spec_max"}];
        freqs = Range@@frange;
        intsIR =  $$["IRSum"]/$$["normal"][[1]];
        $$["IRNormal"] = intsIR;
        ints = ImportString[#,"Lines"]&@ Block[ {CForm = OutputForm@numForm[#, 6]&}, ExportString[intsIR, "Table"] ];
        INT=ints;
        " Frequency        IR Intensity     Linewidth= " <> 
        ( ToString @ NumberForm[#, {4,2}]& @ $$["width"] ) <> "\n" <> 
        ( StringReplace[#,"\t"->"       "]& @ 
          ExportString[MapThread[{"    "<>ToString[#1]<>".0", #2}&]@{freqs,ints},"Table","TextDelimiters"->""
          ] 
        )
    ]  
    
    
formatSFG[] := 
  Module[{(*frange,freqs,ints,intsSFG*)}, 
   frange = Lookup[$$, {"spec_min", "spec_max"}];
   XX = 9;
   r2s = ToString[numForm[#, 6]] &;
   freqs = "    " <> ToString[#1] <> ".0" & /@ (Range @@ frange);
   intsSFG = $$["SFGSum"]/2500(*$$["normal"][[3]]*);
   sfgReImAbs = 
    Prepend[Activate[ 
      Map[Inactive[Sequence][Re[#], Im[#], Abs[#]^2] &, intsSFG]], 
     freqs];
   $$["SFGNormal"] = sfgReImAbs;
   sfgReImAbsStrings = sfgReImAbs /. n_?NumberQ :> r2s[n];
   header = {" Frequency", "Re(ZYX)", "Im(ZYX)", "Abs(ZYX)", 
       "Re(XYZ)", "Im(XYZ)", "Abs(XYZ)", "Re(YZX)", "Im(YZX)", 
       "Abs(YZX)", "Re(YXZ)", "Im(YXZ)", "Abs(YXZ)", "Re(ZXY)", 
       "Im(ZXY)", "Abs(ZXY)", "Re(XZY)", "Im(XZY)", "Abs(XZY)", 
       "Re(YYZ)", "Im(YYZ)", "Abs(YYZ)", "Re(YZY)", "Im(YZY)", 
       "Abs(YZY)", "Re(ZYY)", "Im(ZYY)", "Abs(ZYY)", "Re(ZZZ)", 
       "Im(ZZZ)", "Abs(ZZZ)", "Linewidth=  5.00\n"
       } /. 
      r_String :> StringReplace[r, {"Re" -> " Re", "Im" -> " Im"}] //. 
     s_String :> If[StringLength[s] < XX, " " <> s, s];
   sfgRes = StringJoin[StringRiffle[header, "       "]] <>
     ExportString[sfgReImAbsStrings // Transpose, "Table", 
      "TextDelimiters" -> ""];
   sfgRes  
];
    
   
   
formatRaman[] :=
    Module[ {frange, freqs, ints,intsRaman},
        frange = Lookup[$$, {"spec_min","spec_max"}];
        freqs = Range@@frange;
        intsRaman =  $$["RamanSum"]/$$["normal"][[2]];
        $$["RamanNormal"] = intsRaman;
        ints = ImportString[#,"Lines"]&@ Block[ {CForm = OutputForm@numForm[#, 6]&}, ExportString[Transpose@intsRaman, "Table"] ];
        RINT=ints;
        " Frequency        Raman In. Para.     Raman In. Perp.     Linewidth " <> 
        ( ToString @ NumberForm[#, {4,2}]& @ $$["width"] ) <> "\n" <> 
        ( StringReplace[#,"\t"->"       "]& @ 
          ExportString[MapThread[{"    "<>ToString[#1]<>".0", #2}&]@{freqs,ints},"Table","TextDelimiters"->""
          ] 
        )
    ]  


(*print["doOffDiagonalsKappaLoopC compiled : ", compiledQ @ doOffDiagonalsKappaLoopC ];*)
    
(* -coup 1 -dip 0 -nncm 1 -avgOH 1 -mOH 400 -Omega0 1658 *)

dPrint["Exiting amide.wl ", AbsoluteTime[] - ViscaFor`$START]