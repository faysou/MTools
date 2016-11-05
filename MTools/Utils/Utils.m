(* ::Package:: *)

BeginPackage["MTools`Utils`Utils`",
	{
		"MTools`Core`MPlusPlus`",
		"MTools`Utils`MSync`"
	}
]

FrozenPaneGrid::usage = "FrozenPaneGrid[tl,tr,bl,br,opts]";
DeleteCachedValues::usage = "DeleteCachedValues[f] deletes all cached values of a function using memoization (a cached value hasn't any pattern variable like x)"
DeleteCachedSubValues::usage = "DeleteCachedSubValues[f]"
GetNonEmptyColumn::usage = "GetNonEmptyColumn  "
InitSingleton::usage = "InitSingleton  "
ReplaceKey::usage = "ReplaceKey  "
ListToString::usage = "ListToString  "
DeleteKeys::usage = "DeleteKeys  "
JoinTables::usage = "JoinTables  "
AdjustMatrix::usage = "AdjustMatrix[l, emptyElemement:\"\"]";
DecorateTable::usage = "DecorateTable  "
RoundAsPercent::usage = "RoundAsPercent  "
RoundAsIntegerPercent::usage = "RoundAsIntegerPercent  "
RoundNDigits::usage = "RoundNDigits  "
SignColor::usage = "SignColor  "
OpenerTree::usage = "OpenerTree  "
openerView::usage = "openerView  ";
CustomInputField::usage = "CustomInputField  "
GetCalendar::usage = "GetCalendar  "
AssociationToJson::usage = "AssociationToJson  "
GetQuery::usage = "GetQuery  "
Quote::usage = "Quote  "
$BackQuote::usage = "$BackQuote  "
GetSelectedKeyAfterDelete::usage = "GetSelectedKeyAfterDelete  "
TickNotify::usage = "TickNotify  "
HeldTickNotify::usage = "HeldTickNotify  "
TrackTick::usage = "TrackTick  "
TrackHeldTick::usage = "TrackHeldTick  "
NewTick::usage = "NewTick  "
TrackObject::usage = "TrackObject  "
TrackHeldValue::usage = "TrackHeldValue  "
TrackSymbol::usage = "TrackSymbol  "
permutate::usage = "permutate  "
GetAllOptions::usage = "GetAllOptions  "
JoinOptions::usage = "JoinOptions  "
SelectEquivalents::usage = "SelectEquivalents  "
ReleaseAllHold::usage = "ReleaseAllHold  "
InitJavaKernels::usage = "InitJavaKernels[nKernels:0,reservedKernels:None]"
GetSymbolName::usage = "GetSymbolName  "
SendEmail::usage = "SendEmail[subject,message,options]"

System`$LogItFile = "log.txt";
System`$LogItPattern = _;
System`LogIt;
System`$LogFiles;
System`CloseLogFile

MakeRef::usage = "MakeRef  "
RefValue::usage = "RefValue  "
RemoveObserver::usage = "RemoveObserver  "
$Observers::usage = "$Observers  "
Ref::usage = "Ref  "
Observe::usage = "Observe  "

Begin["`Private`"]

nameRule=((__~~"`")...)~~Shortest[x__]~~(("$"~~__)...)~~(("_")...) :> x;
SetAttributes[GetSymbolName,HoldFirst];
GetSymbolName[symbol_String]:= symbol;
GetSymbolName[Verbatim[Pattern][x_, _]]:= GetSymbolName@x;
g:GetSymbolName[symbol_] := g = StringReplace[ToString@HoldForm@symbol,nameRule];

SetAttributes[DeleteCachedValues,Listable];
DeleteCachedValues[f_] := (DownValues[f] = Select[DownValues[f], !FreeQ[Hold@#,Pattern]&];);
DeleteCachedValues[f_,nrules_] := (DownValues[f] = Extract[DownValues[f], List /@ Range[-nrules, -1]];);

DeleteCachedSubValues[f_] := (SubValues[f] = Select[SubValues[f], !FreeQ[Hold@#,Pattern]&];);
DeleteCachedSubValues[f_,nrules_] := (SubValues[f] = Extract[SubValues[f], List /@ Range[-nrules, -1]];);

GetNonEmptyColumn[table_,column_]:=If[table === {},{},table[[All,column]]];

SetAttributes[InitSingleton,HoldAll];
InitSingleton[symbol_,value_]:=
	If[!ValueQ@symbol,
		symbol = value;
	];

ReplaceKey[a_Association,replacements___]:=Association[a,replacements];

ListToString[list_]:=ToString@list//StringReplace[#,{"{" -> "", "}" -> ""}]&;

SetAttributes[DeleteKeys,HoldFirst];
DeleteKeys[assoc_,keyPattern_]:=
	Module[{keysConsidered},
		keysConsidered=Cases[Keys@assoc,keyPattern];
		Unset@assoc[#]&/@keysConsidered//Quiet;
	];

SetAttributes[JoinTables,{OneIdentity}];
Options[JoinTables] = {"JoiningDirection" -> "Horizontal","EmptyElemement"->0};
JoinTables[tab1_List, tab2_List, opts:OptionsPattern[]] :=
	Module[ {dimTab1, dimTab2, dim1, dim2, emptyElemement, joiningDirection},
		
		dim1 = Dimensions[tab1];
		dim2 = Dimensions[tab2];
		dimTab1 = Length[dim1];
		dimTab2 = Length[dim2];
		joiningDirection = OptionValue@JoiningDirection;
		emptyElemement = OptionValue@EmptyElemement;
		
		Which[
			tab1 === {} || tab1 === {{}}, 
				tab2,
			tab2 === {} || tab2 === {{}}, 
				tab1,
			True,
				Switch[joiningDirection,
					"Horizontal",
						Switch[{dimTab1, dimTab2},
							{1,1}, 
								AdjustMatrix[{tab1, tab2},emptyElemement]//Transpose
							,
							{1,2}, 
								AdjustMatrix[Join[{tab1},tab2//Transpose],emptyElemement]//Transpose
							,
							{2,1}, 
								AdjustMatrix[Join[tab1//Transpose,{tab2}],emptyElemement]//Transpose (*does same job : ArrayFlatten@{{array, List /@ column}}*)
							, 
							{2,2}, 
								AdjustMatrix[Join[tab1//Transpose,tab2//Transpose],emptyElemement]//Transpose
						]
					,
					"Vertical",
						Switch[{dimTab1, dimTab2},
							{1,1}, 
								AdjustMatrix[{tab1, tab2},emptyElemement]
							,
							{1,2}, 
								AdjustMatrix[Join[{tab1},tab2],emptyElemement]
							,
							{2,1}, 
								AdjustMatrix[Join[tab1,{tab2}],emptyElemement]
							,
							{2,2}, 
								AdjustMatrix[Join[tab1, tab2],emptyElemement]
						]
				]
		]
	];
	
JoinTables[tabs__List, opts:OptionsPattern[]]:=Fold[JoinTables[#1, #2, opts]&,First@{tabs},Rest@{tabs}];

AdjustMatrix[l_, emptyElemement_:""]:=
	Module[{maxInnerLength}, 
		If[Length@Dimensions@l == 2, 
			l
			, 
			maxInnerLength = Max[Length /@ l];
			PadRight[#, maxInnerLength, emptyElemement]& /@ l
		]
	];

DecorateTable[table_,title_,{rowHeaders_,columnHeaders_}]:=DecorateTable[table,title,rowHeaders,columnHeaders];
DecorateTable[table_,title_,rowHeaders_,columnHeaders_]:=JoinTables[{title}~Join~columnHeaders,JoinTables[rowHeaders,table],"JoiningDirection"->"Vertical"];

(*DecorateTable[{{1,2},{3,4}},"Title",{"r1","r2"},{"c1","c2"}]//TableForm*)

(*test
l1 = {1, 2};
l2 = {a, b, c};
m1 = {{3, 4}, {5, 6}};
m2 = {{d, e, f}, {g, h, i}, {j, k, l}};

TableForm /@ JoinTables @@@ Tuples[{l1, l2, m1, m2}, 3]
TableForm /@ (JoinTables[##, "JoiningDirection" -> "Vertical"]& @@@ Tuples[{l1, l2, m1, m2}, 3])
*)

RoundAsPercent = Round[100 #,0.01]&;
RoundAsIntegerPercent = Round[100 #]&;
RoundNDigits[x_,nDigits_:2]:=Round[x,10.^(-nDigits)];
SignColor = Interpretation[Style[#,If[# >= 0,Darker@Green,Red]],#]&;

Options[ReleaseAllHold] = {"FirstLevel"->0,"LastLevel"->Infinity,"ReplacedSymbols"->{Hold,HoldForm,HoldPattern,HoldComplete}};
ReleaseAllHold[expr_,OptionsPattern[]] := 
	With[{alternativeSymbols = Alternatives @@ (OptionValue@"ReplacedSymbols")},
		Replace[expr, alternativeSymbols[e___] :> e, {OptionValue@"FirstLevel", OptionValue@"LastLevel"}, Heads -> True]
	];
	
emptyElement="";
ClearAll@customOpener;

{g1,g2}= Graphics[{Gray,Rotate[Polygon[{{-1,0},{1,0},{0,2}}],#]},ImageSize->7]& /@ {3 Pi/2, Pi};
customToggler[x_]:=Toggler[x,{False->g1,True->g2}];
(*customToggler can replace Opener in the function below*)

customOpener[Dynamic[openVariable_,callBack_],header_,content_]:=
	PaneSelector[
		{
			True->
				Grid[
					{
						{
							(*Opener*)customToggler[
								Dynamic[
									openVariable
									,
									callBack[#]&
								]
							]
							,
							Item[header,Alignment->Left]
						}
						,
						{
							emptyElement
							,
							content
						}
					}
					,
					Spacings -> {0, Automatic}
				]
			,
			False->
				Grid[
					{
						{
							(*Opener*)customToggler[
								Dynamic[
									openVariable
									,
									callBack[#]&
								]
							]
							,
							Item[header,Alignment->Left]
						}
					}
					,
					Spacings -> {0, Automatic}
				]
		}
		,
		Dynamic[openVariable]
		,
		ImageSize->Automatic
	];

ClearAll@expandOpener;
expandOpener[openList_,openerListId_,element_,content_,newLevel_,defaultValue_]:=
	Hold[ 
		If[defaultValue =!= None,
			openList[[newLevel]] = defaultValue;
		]; 
		
		myOpener[ 
			dynamic[ 
				openList[[newLevel]]
				, 
				(
					openList[[newLevel]] = #;
					$currentOpenState[openerListId] = openList
				)& 
			]
			, 
			element
			, 
			content 
		]
	];

ClearAll@expandLine;
SetAttributes[expandLine,HoldAll];
expandLine[openList_,openerListId_,openerView[head_,rest_,defaultValue_:None],nOpeners_]:=
	Module[{contentExpanded,newLevel},
		
		newLevel=++nOpeners;
		
		contentExpanded = 
			Column[
				expandLine[openList,openerListId,#,nOpeners]&/@rest
			];
			
		expandOpener[openList,openerListId,head,contentExpanded,newLevel,defaultValue]
	];
expandLine[openList_,openerListId_,element_,nOpeners_]:=element;

InitSingleton[$currentOpenState,Association[]];

Options[OpenerTree] = {"DefaultOpenValue"->False,"OpenerGridOptions"->{},"UseExistingStates"->True};
OpenerTree[tree_,firstLine_,openerListId_:"",OptionsPattern[]]:=
	DynamicModule[{linesHeld,openList,
					defaultValue = OptionValue@"DefaultOpenValue",
					nOpeners=0,
					useExistingStates = OptionValue@"UseExistingStates"},

		linesHeld=expandLine[openList,openerListId,#,nOpeners]&/@Prepend[tree,firstLine]//Hold@Column[#(*,Spacings -> {0, 0}*)(*OptionValue@"OpenerGridOptions"*)]&;

		If[!KeyExistsQ[$currentOpenState,openerListId],
			$currentOpenState[openerListId] = {};
		];

		Which[
			useExistingStates && Length@$currentOpenState[openerListId] == nOpeners,
				openList = $currentOpenState[openerListId];
			,
			useExistingStates && Length@$currentOpenState[openerListId] < nOpeners,
				openList = Join[$currentOpenState[openerListId],ConstantArray[defaultValue,nOpeners-Length@$currentOpenState[openerListId]]]
			,
			useExistingStates && Length@$currentOpenState[openerListId] > nOpeners,
				openList = $currentOpenState[openerListId][[;;nOpeners]],
			True,
				openList=ConstantArray[defaultValue,nOpeners];
				$currentOpenState[openerListId]=openList;
		];
		
		linesHeld/.{dynamic->Dynamic,myOpener->customOpener}//ReleaseAllHold
	];	

SetAttributes[CustomInputField,HoldAll];
CustomInputField[dynamicVariable_,dyanmicEnabled_,imageSize_:{300,100},allowReturn_:True,displayScrollBar_:True]:=
	With[{opts = {FrameMargins -> 0, ImageMargins -> 0}},
		Framed[
			Framed[
				Pane[
					EventHandler[
						InputField[
							dynamicVariable
							, 
							String
							, 
							ContinuousAction -> False, 
							Appearance -> "Frameless", 
							FieldSize -> {400, 600},
							Enabled -> dyanmicEnabled
						]	
						, 
						"ReturnKeyDown" :> 
				 			If[allowReturn,
				 				FrontEndExecute[{NotebookWrite[InputNotebook[], "\n", After]}]
				 			]
					]
					, 
					ImageSize -> imageSize, 
					ImageMargins -> 0, 
					AppearanceElements -> {}, 
					FrameMargins -> {{5, 5}, {0, 0}}, 
					Scrollbars -> {False, displayScrollBar}
				]
				,
				FrameStyle -> Blue(*Red*), 
				RoundingRadius -> 5, 
				opts
			]
			,
			BoxFrame -> 3, 
			RoundingRadius -> 7, 
			FrameStyle -> LightBlue(*Lighter@Orange*), 
			opts
		]
	];
	
(*http://mathematica.stackexchange.com/q/16542/66*)
GetCalendar[callback_,yearRange_:Range[2000,2050]]:=
	With[{startDayOffset = Thread[{Sunday, Monday, Tuesday, Wednesday, Thursday,Friday, Saturday} -> Range@7]},   
	    DynamicModule[{month, year, date, today = DateList[][[;;3]], daysInMonth, calendarView},
	
	        {year, month, date} = today;
	
	        daysInMonth[m_Integer,y_Integer] := DatePlus[{y, m, 1}, {{1, "Month"}, {-1, "Day"}}][[3]];
	
	        calendarView[m_Integer, y_Integer] := 
		        Grid[
		            {
		            	Style[#, FontWeight -> Bold]& /@ {"Su","M","Tu","W","Th","F","Sa"}
		            } 
		            ~Join~ 
		            Partition[Range@daysInMonth[m, y], 7, 7, {DayName[{y, m, 1}] /. startDayOffset, 1}, {""}]
		            ,
		            Frame -> All,
		            FrameStyle -> LightGray
		        ] /. 
		        	{ 
		        		i_Integer :> 
			        		Button[
			                    i
			                    , 
			                    date=i;
			                    callback[{year,month,date}];
			                    , 
			                    Appearance->"Palette", 
			                    Background -> 
				                    Which[
				                        date==i, 
				                        	LightRed,
				                        {year, month, i} === today, 
				                        	LightBlue,
				                        !FreeQ[DayName[{year, month, i}],Saturday|Sunday],
				                        	LightGreen,
				                        True,
				                        	White
				                    ],
			                    ImageSize->{32,32}
			                ],
		                s_String :> 
			                Button[
			                    s
			                    , 
			                    Null
			                    , 
			                    Appearance -> "Palette", 
			                    Enabled -> False, 
			                    Background -> If[!s == "", LightGray],
			                    ImageSize->{32,32}
			                ]
		            };
	
	        Panel[
	            Column[
	            	{
		                Row[
		                	{
			                    Style["Year ",FontSize->16], 
			                    PopupMenu[
			                    	Dynamic[
			                    		year
			                    		,
			                    		(
			                    			year = #;
			                    			callback[{year,month,date}];
			                    		)&
			                    	]
			                    	, 
			                    	yearRange
			                    ],
			                    Spacer[10],
			                    Style["Month ",FontSize->16],
			                    PopupMenu[
			                    	Dynamic[
			                    		month
			                    		,
			                    		(
			                    			month = #;
			                    			callback[{year,month,date}];
			                    		)&
			                    	]
			                    	, 
			                    	Range@12 
			                    ]
		               		}
		                ],
		                Dynamic@calendarView[month, year]
	            	}
	            ]
	        ]
	    ]
	];
	
GetSelectedKeyAfterDelete[keys_,key_,emptyKey_:None]:=
	Module[{newKey,keyPosition},
		
		(*We find the key to display after a key has been deleted*)
		If[Length@keys == 1,
			newKey = emptyKey;
			,
			keyPosition = Position[keys,key][[1,1]];
			
			If[keyPosition == Length@keys,
				newKey = keys[[Length@keys-1]];
				,
				newKey = keys[[keyPosition+1]];
			];
		];
		
		newKey
	];

AssociationToJson[expr_String]:=expr;	
AssociationToJson[expr_]:=expr//.a_Association:>Normal[a]//ExportString[#,"JSON"]&;

GetQuery[statement_,params_]:=
	Module[{requestData},
		requestData = Association@params;
		StringTemplate[statement][requestData]
	];
	
Quote[string_String]:="\""~~string~~"\"";
$BackQuote = "`";
	
SetAttributes[TickNotify,HoldFirst];
TickNotify[tick_]:=
	If[ValueQ@tick,
		tick=!tick;
	];
	
HeldTickNotify[Hold[tick_]]:=
	If[ValueQ@tick,
		tick=!tick;
	];

SetAttributes[TrackTick,HoldAll];
TrackTick[ticks_List,expr_]:=
	Dynamic[
		(*If any tick of ticks changes the expression will refresh*)
		ticks; 
		expr
		,
		TrackedSymbols :> ticks	
	];
	
SetAttributes[TrackHeldTick,HoldRest];
TrackHeldTick[heldTicks_List,expr_]:=
	(Join @@ heldTicks) /. Hold[ticks__] :> TrackTick[{ticks},expr];
	
NewTick[]:=
	Module[{tick=False},
		Hold[tick]	
	];

SetAttributes[TrackObject,HoldRest];	
TrackObject[symbols_List,expr_,callback___]:=
	Dynamic[
		expr
		,
		callback
		,
		GetTrackedSymbols[symbols]
	];
TrackObject[object_]:=
	Dynamic[
		object["Value"]
		,
		GetTrackedSymbols[{object}]	
	];
	
TrackHeldValue[heldValue_,callback___]:=
	heldValue /. 
		_[value_] :>
			Dynamic[
				value
				,
				callback
				,
				TrackedSymbols:>{value}
			];
	
SetAttributes[TrackSymbol,HoldAll];
TrackSymbol[symbols_,expr_]:=
	Dynamic[
		expr
		,
		TrackedSymbols :> symbols
	];
TrackSymbol[symbol_]:=TrackSymbol[(*{symbol}*)Full,symbol];

SetAttributes[AssignOptions,HoldFirst];
AssignOptions[container_,function_,currentOptions_,options___]:=
	(
	    (container[GetSymbolName[#]]=OptionValue[function,currentOptions,#])&
		/@
		Flatten[{options}];
	);

GetAllOptions[function_,currentOptions___]:=
	Module[{allOptions},
		AssignOptions[allOptions,function,FilterRules[Flatten@{currentOptions},Options[function]],Options[function][[All,1]]];
		allOptions
	];
	
JoinOptions[options__]:= 
	Module[{optionsRenamed=Flatten@{options}},
	    optionsRenamed[[All,1]]=GetSymbolName /@ optionsRenamed[[All,1]];
	    DeleteDuplicates[optionsRenamed, (First[#1] === First[#2]&)]
	];
	
Options[SelectEquivalents] = 
    {
        "TagElement"->Identity,
        "TransformElement"->Identity,
        "TransformResults"->(#2&) (*#1=tag,#2 list of elements corresponding to tag*),
        "MapLevel"->1,
        "TagPattern"->_,
        "FinalFunction"->Identity
    };

SelectEquivalents[x_List,OptionsPattern[]] := 
	With[
	    {
	    	tagElement=OptionValue@"TagElement",
	    	transformElement=OptionValue@"TransformElement",
	    	transformResults=OptionValue@"TransformResults",
	    	mapLevel=OptionValue@"MapLevel",
	    	tagPattern=OptionValue@"TagPattern",
	    	finalFunction=OptionValue@"FinalFunction"
	    }
	    ,
		Reap[
		    Map[
		        Sow[
		            transformElement@#
		            ,
		            {tagElement@#}
	        	]&
	        	, 
	        	x
	        	, 
	        	{mapLevel}
	    	] 
	        , 
	        tagPattern
	        , 
	        transformResults
	    ][[2]] // 
	    finalFunction
	];	 

Options[SendEmail] = 
	{
		"Attachments"->{},
		"To"->{"--@gmail.com"},
		"From"->"--@gmail.com",
		"Smtp"->"smtp.gmail.com",
		"UserName"->"--@gmail.com",
		(*with gmail you need to generate a token for an app*)
		"Password"->""
	};
SendEmail[subject_,message_,OptionsPattern[]]:=
	SendMail[
		"To"->OptionValue@"To",
		"Subject"->subject,
		"Body"->message,
		"From"->OptionValue@"From",
		"AttachedFiles"->OptionValue@"Attachments",
		"Server"->OptionValue@"Smtp",
		"UserName"->OptionValue@"UserName",
		"Password"->OptionValue@"Password",
		"PortNumber"->587,
		"EncryptionProtocol"->"StartTLS"
	];

SetAttributes[permutate, HoldFirst];
permutate[l_, i_, j_]:=
	Module[{temp},
		temp = l[[j]];
		l[[j]] = l[[i]];
		l[[i]] = temp;
	];
	
(*http://mathematica.stackexchange.com/questions/21117/how-can-i-create-an-advanced-grid-interface-on-mathematica*)
Options[FrozenPaneGrid] = {"RowLabelSort" -> False};
FrozenPaneGrid[title_,columnLabels_,rowLabels_,data_,OptionsPattern[]]:=
	DynamicModule[
		{
			scroller=1,scrollx=0,scrolly=0,options,columns=Length[columnLabels],headings=columnLabels,i=1,
			order=Range[Length[rowLabels]],initialOrder=Ordering[rowLabels]
		},
		
		options=
			{
				Background->{None,{{White,GrayLevel[0.93]}}},
				BaseStyle->Directive[FontFamily->"Helvetica",11],
				Frame->False,
				FrameStyle->Directive[Thin,GrayLevel[0.75]]
			};
			
		Dynamic[
			Column[
				{
					Grid[
						{
							{
								Pane[
									Grid[
										{
											{
												(*title sort*)
												EventHandler[
													MouseAppearance[
														title
														,
														Framed[Style["Left Click Sort\nRight Click Reverse Sort",9],Background->White]
													]
													,
													{
														{"MouseClicked",1}:>
															(
																If[TrueQ[OptionValue["RowLabelSort"]],
																	order=initialOrder
																	,
																	order=Range[Length[rowLabels]]
																]
															)
														,
														{"MouseClicked",2}:>
															(
																If[TrueQ[OptionValue["RowLabelSort"]],
																	order=Reverse@initialOrder
																	,
																	order=Reverse@Range[Length[rowLabels]]
																]
															)
													}
												]
											}
										}
										,
										Alignment->{Left,Center},
										Dividers->All,
										ItemSize->{8,1.75},
										Spacings->{{2,{0.5},2},0.5},
										options
									]
									(*,
									{270,All}*)
									,
									Alignment->{Right,Top},
									ImageMargins->0
								]
								,
								Pane[
									Grid[
										{
											(*column sort*)
											Table[
												With[{j=j},
													EventHandler[
														MouseAppearance[
															headings[[j]]
															,
															Framed[Style["Left Click Sort\nRight Click Reverse Sort",9],Background->White]
														]
														,
														{
															{"MouseClicked",1}:>
																(
																	i=j;
																	order=Ordering[data[[All,i]]]
																)
															,
															{"MouseClicked",2}:>
																(
																	i=j;
																	order=Reverse@Ordering[data[[All,i]]]
																)
														}
													]
												]
												,
												{j,columns}
											]
										}
										,
										Alignment->{Right,Center},
										Dividers->{{-1->White},{1->True,-1->True}},
										ItemSize->{8,1.75},
										Spacings->{{2,{0.5},2},0.5},
										options
									]
									(*,
									{655,All}*)
									,
									Alignment->{Left,Top},
									ImageMargins->0,
									ScrollPosition->Dynamic[{scrollx,scroller}]
								]
							}
							,
							{
								(*row names*)
								Pane[
									Grid[
										rowLabels[[order]] // Map[List]
										,
										Alignment->{Left,Center},
										Dividers->{{1->White,-1->LightGray},None},
										ItemSize->{8,1.75},
										Spacings->{{2,{0.5},2},0.5},
										options
									]
									(*,
									{270,505}*)
									,
									Alignment->{Right,Top},
									AppearanceElements->None,
									ImageMargins->0,
									ScrollPosition->Dynamic[{scroller,scrolly}]
								]
								,
								(*grid data*)
								Pane[
									Grid[
										data[[order]]
										,
										Alignment->{Right,Center},
										Dividers->{{-1->White},None},
										ItemSize->{8,1.75},
										Spacings->{{2,{0.5},2},0.5},
										options
									]
									(*,
									{670,520}*)
									,
									Alignment->{Left,Top},
									AppearanceElements->None,
									ImageMargins->0,
									ScrollPosition->Dynamic[{scrollx,scrolly}],
									Scrollbars->Automatic
								]
							}
						}
						,
						Alignment->{{Right,Left},Top},
						Spacings->{0,0}
					]
					,
					Button["Copy",
						JoinTables[
							Prepend[rowLabels[[order]],title]
							,
							Prepend[data[[order]],headings]
						]// ExportString[#,"TSV"]& // CopyToClipboard
					]
				}
			]
			,
			TrackedSymbols:>{order}
		]
	];
	
GetPackageName[]:=Context[GetPackageName]//StringSplit[#,"`"]&//First;

GetPackageInfo[]:=
	Module[{packageName,containingDirectory},
		packageName=GetPackageName[];
		containingDirectory=
			DirectoryName@FindFile[packageName~~"`"~~packageName~~"`"]//
			StringSplit[#,"\\"]&//Most//FileNameJoin;
		
		{packageName,containingDirectory}
	];
	
InitJavaKernels[opts:OptionsPattern[InitKernels]]:=
	Module[{kernelInitFunction,packageName,containingDirectory},
		
		{packageName,containingDirectory}=GetPackageInfo[];
		
		kernelInitFunction =
			With[
				{
					classPath=FileNameJoin[{containingDirectory,packageName,"Java"}],
					containingDirectory = containingDirectory
				},
				(
					If[!MemberQ[JLink`$ExtraClassPath,classPath],
						AppendTo[JLink`$ExtraClassPath,classPath];
						AppendTo[JLink`$ExtraClassPath,FileNameJoin[{classPath,"Couchbase-Java-Client"}]];
					];
					
					If[!MemberQ[$Path,containingDirectory],
						AppendTo[$Path,containingDirectory];
					];
				)&
			];
		
		InitKernels["KernelInitFunction"->kernelInitFunction,opts];
	];
	
SetAttributes[ExtractSymbolName, HoldAll];
ExtractSymbolName[expr_] :=
	Module[{T,SR = StringReplace[#, a__ ~~ "$" ~~ DigitCharacter .. :> a] &}, 
	    Defer[expr]  /. s_Symbol :> T@MakeExpression@SR@SymbolName@Unevaluated@s  /. T@_@x___ :> x
	];

deferedExprToString = Function[deferedExpr, ToString[Unevaluated@deferedExpr,InputForm], HoldFirst];

SetAttributes[ExpressionToString,HoldFirst]
ExpressionToString[expr_]:= ExtractSymbolName@expr // Apply[deferedExprToString];

CurrentTime[]:=DateString[{"Year","/","Month","/","Day","-","Hour",":","Minute",":","Second",":","Millisecond"}];

System`CloseLogFile[file_]:= 
	If[KeyExistsQ[System`$LogFiles,file],
		Close[System`$LogFiles[file]];
		System`$LogFiles[file] = .;
		,
		Print["Log file can't be close, as it's not open already."]
	];

InitSingleton[System`$LogFiles,Association[]];

Options[System`LogIt] = 
	{
		"LogTemplate"->"`Tag` `Delimiter` `Type` `Delimiter` `Level` `Delimiter` `Time` `Delimiter` `Message`",
		"Tag"->"##",
		"Delimiter"->"|",
		"Type"->"Log",
		"Level"->1,
		"Context"->None,
		"File"->Automatic,
		"PrintMessage"->False
	};
System`LogIt[message_,opts:OptionsPattern[]]:=
	Module[{stringTemplate,messageComponents,options,output,outputFile},
		
		If[MatchQ[{OptionValue@"Type",OptionValue@"Level"},System`$LogItPattern],
			
			stringTemplate = OptionValue@"LogTemplate";
			
			options = {"Tag","Delimiter","Type","Level"} ;
			messageComponents = AssociationThread[options->(OptionValue[System`LogIt,{opts},#]& /@ options)];
			
			messageComponents["Time"]=CurrentTime[];
			
			messageComponents["Message"] = message;
			
			If[OptionValue@"Context" =!= None,		
				messageComponents["Context"] = 
					OptionValue[System`LogIt,{opts},"Context",ExpressionToString]~~" = "~~ToString[OptionValue@"Context",InputForm];
					
				stringTemplate = stringTemplate~~" `Delimiter` `Context`"
			];
			
			output = StringTemplate[stringTemplate][messageComponents];
			
			outputFile = 
				If[OptionValue@"File"===Automatic,
					System`$LogItFile
					,
					OptionValue@"File"
				];
				
			If[!KeyExistsQ[System`$LogFiles,outputFile],
				System`$LogFiles[outputFile] = OpenAppend[outputFile, PageWidth -> Infinity];
			];
			
			WriteString[System`$LogFiles[outputFile],output,"\n"];
			(*Close[logFile];*)
			
			If[OptionValue@"PrintMessage",
				Print@output;
			];
			
			output
		]
	];
	
	
(*http://mathematica.stackexchange.com/q/47659/66*)
ClearAll[withCodeAfter];
SetAttributes[withCodeAfter,HoldRest];
withCodeAfter[before_,after_]:=(after;before);

ClearAll[Ref,$Observers,MakeRef,Observe,RemoveObserver];
$Observers[_]={};

SetAttributes[Ref,HoldAll];
MakeRef[expr_]:=Module[{sym=expr},Ref[sym]];

Ref/:RefValue[Ref[sym_]]:=sym;
Ref/:Set[ref:Ref[sym_],new_]:=
	withCodeAfter[
		sym=new
		,
		Scan[
			Function[obs,obs[ref,new]]
			,
			$Observers[ref]
		]
	];
Ref/:Set[lhs_Symbol,rhs_Ref]:=
	withCodeAfter[
		OwnValues[lhs]=HoldPattern[lhs]:>rhs;
		lhs
		,
		lhs/:Set[lhs,val_]:=(rhs=val)
	];
Ref/:Observe[ref_Ref,observer_]:=AppendTo[$Observers[ref],observer];
Ref/:RemoveObserver[ref_Ref,observer_]:=$Observers[ref]=DeleteCases[$Observers[ref],observer];
	
End[]
EndPackage[]


