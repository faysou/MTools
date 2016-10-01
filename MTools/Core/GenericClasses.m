(* Mathematica Package *)

BeginPackage["MTools`Core`GenericClasses`",
	{
		"MTools`Core`MPlusPlus`",
		"MTools`Utils`Utils`",
		"MTools`Utils`MSync`",
		"MTools`Utils`Couch`"
	}
]
(* Exported symbols added here with SymbolName::usage *)  

GenericClass::usage = " ";
SelectedClass::usage = " ";
GenericGroup::usage = " ";
CommentModuleOpener::usage = "CommentModuleOpener[currentObject, title, displayJournalEntryType:False]";
PropertiesCheckboxBar::usage = "PropertiesCheckboxBar is a class that implement a CheckboxBar that selects the visible properties of GenericGroup table";

(*functions with HoldX attribute should be exported so that the exact symbol is used instead of another symbol*)
iterate::usage = " ";
superIterate::usage = " ";
selfIterate::usage = " ";
iterateAssociation::usage = " ";
treeIterate::usage = " ";
upIterate::usage = " ";
createScheduledTask::usage = " ";
runScheduledTask::usage = " ";
deferredTask::usage = " ";
asyncEvaluate::usage = " ";
$emptyElement::usage = " ";
$Component::usage = " ";
$GenericObjects::usage = " ";
$SavedObjects::usage = " ";

$o;

ValueObject::usage = "ValueObject  "
ValuesSelector::usage = "ValuesSelector  ";

Begin["`Private`"] (* Begin Private Context *) 
(* ::Subsection:: *)
(* 	GenericClass *)
(* ::Subsubsection:: *)
(* 		GenericClass/Aux functions and values *)
InitSingleton[$treeId, 0];

InitSingleton[$GenericObjects, Association[]];

$emptyElement = "-";
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* 		GenericClass/SelectedClass *)
SelectedClass = NewClass["Fields"  -> {"Selected" -> True, "Editable" -> False}];
SelectedClass.select[value_]:= o."Selected" = value;
SelectedClass.edit[value_]:= o."Editable" = value;
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* 		GenericClass/Declaration *)
GenericClass=
	NewClass[
		"Fields"  -> 
			{
				"BackupFields", "ObjectType" -> "GenericClass", "Id" -> "", "CreatedDate", "SavedDate", "TickNotify" -> False, 
				{"Description" -> "", "InputField", "Specs" -> {String}}, {"Type" -> "", "InputField", "Specs" -> String}, "SelectedObject", 
				{"Color" -> "LightBlue", "InputField", "Specs" -> {String}}, "Parent", "ObjectSavedFields" -> {}, 
			   	"Journal" -> Association[], "ScheduledTasks" -> Association[], "Refs" -> Association[], "DisplayedProperties" -> {}, "LocalSave" -> False, 
			   	"ActionAssociation" -> Association[], "VisibleInTree" -> True
			}
	];
GenericClass.init[options_]:=
	(
		If[o["Journal"] === {}, 
			o."Journal" = Association[];	
		];
		
		o.setIdIfNotDef[options];
		
		$GenericObjects[o["Id"]]=Identity@o;
		
		o."SelectedObject" = New[SelectedClass][];
	);
GenericClass.toString[]:= o["Id"];
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* 		GenericClass/Id *)
GenericClass.getIdDate[]:= DateString[{"YearShort", "Month", "Day", ":", "Hour", "Minute", "Second", ".", "Millisecond"}];
GenericClass.getTreeId[]:= ToString@Mod[$treeId++, 10]; (*not necessary,  as creating an object takes more than 1ms (for now)*)
GenericClass.getId[]:= o["ObjectType"]~~"_"~~(o.getIdDate[]);(*~~"_"~~(o.getTreeId[])*)
GenericClass.setId[]:=
	(	
		o."Id" = o.getId[];
		o."CreatedDate" = CouchDate[];
	);
GenericClass.setIdIfNotDef[options_]:=
	If[o.getOption[options, "Id"]==="", 
		o.setId[];
	];
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* 		GenericClass/Tree functions *)
GenericClass.replaceWithSelf[expr_]:= expr /. $o  ->  o;
GenericClass.executeFunction[{function_, args___}]:=o.ToExpression[function][args];
GenericClass.executeFunctions[functions_]:=
	If[functions === {}, 
		Association[]
		, 
		(*each function is of the form {label, function, args} or {{label, resultFormatFunction}, function, args}*)
		AssociationThread[
			functions[[All, 1]] // Map[If[ListQ@#, First@#, #]&]
			, 
			o.thread[executeFunction[functions[[All, 2;;]]]] // 
			MapThread[If[ListQ@#1, (#1[[2]])[#2], #2]&, {functions[[All, 1]], #}]&
		]
	];
GenericClass.executeIf[condition_]:= 
	If[condition@o, 
		o
		, 
		$Failed
	]; (*to be used as o.iterate[executeIf[#["Status"]!="Filled"&].g[]]*)
GenericClass.setField[field_, value_]:= o.field = value;
GenericClass.hasParent[]:= o["Parent"] =!= None;
GenericClass.getRoot[condition_:(True &)]:=
	If[o.hasParent[] && condition@o["Parent"], 
		o["Parent"].getRoot[condition]
		, 
		o
	];
GenericClass.delete[]:=
	(
		o["SelectedObject"].delete[];
		$GenericObjects[o["Id"]] = .;
		o.super.delete[];
	);
GenericClass.remove[]:= 
	If[o.hasParent[], 
		o["Parent"].removeComponent[o];
	];
GenericClass.removeFromAssociation[id_:"Symbol", assoc_:"Symbols"]:= 
	If[o.hasParent[], 
		o["Parent"].removeComponentFromAssociation[o[id], "IdField" -> id, "AssocField" -> assoc];
	];
SetAttributes[{iterate, iterateAssociation, upIterate}, HoldFirst];
Options[iterate] = {"Condition" -> (True&), "CarryTreeResult" -> False, "Field" -> "Components", "ExternalIterate" -> False};
Final@GenericClass.iterate[fun_, opts:OptionsPattern[iterate]]:= 
	Block[{iterationResult, condition, carryTreeResult, componentsList}, 
		
		condition = OptionValue[iterate, {opts}, "Condition"];
		carryTreeResult = OptionValue[iterate, {opts}, "CarryTreeResult"];
		componentsList = o[OptionValue[iterate, {opts}, "Field"]];
		
		iterationResult =
			Function[component, 
				If[condition@component, 					
					If[!OptionValue[iterate, {opts}, "ExternalIterate"], 
						(*get["SelectedObject"].get["Selected"] should work as fun*)
						Hold[component.fun]
						, 
						Hold[fun[component]]
					] /. $Component -> component // ReleaseHold //
					If[carryTreeResult && !SubClass[GenericGroup][component], 
						List
						, 
						Identity
					]
					(*When we carry results along a tree,  a leaf's result must be enclosed in a List,  
					in order to join it with an iteration from another branch*) 
					, 
					##&[] (*same as Unevaluated@Sequence[],  produces a "no element" in a list*)
				]
			] /@ componentsList;
			
		If[carryTreeResult, 
			Join @@ iterationResult
			, 
			iterationResult
		]
	];
Options[upIterate] = {"Condition" -> (True&), "ExternalIterate" -> False};
GenericClass.upIterate[fun_, opts:OptionsPattern[upIterate]]:=
	Block[{iterationResult, condition, carryTreeResult, componentsList}, 
		
		condition = OptionValue[upIterate, {opts}, "Condition"];
		
		If[!OptionValue[upIterate, {opts}, "ExternalIterate"], 
			o.fun
			, 
			fun[o]
		];
		
		If[o.hasParent[] && condition@o["Parent"], 
			o["Parent"].upIterate[fun, opts];
		];
	];
Options[iterateAssociation] = Append[Options[iterate], "AssociationKey" -> "Symbol"];
GenericClass.iterateAssociation[fun_, opts:OptionsPattern[iterateAssociation]]:= 
	Block[{keys, result, associationKey}, 
		
		associationKey = OptionValue[iterateAssociation, {opts}, "AssociationKey"];
		keys = o.iterate[get[associationKey]];
		
		result = o.iterate[fun, FilterRules[{opts}, Options[iterate]]];
		
		AssociationThread[keys, result]
	];
SetAttributes[treeIterate, HoldFirst];
Options[treeIterate] = Join[Options[iterate], {"IterateOn" -> All(*"Leafs"|"Nodes"|All*), "DeleteMissing" -> False, "TraversalOrder" -> "Prefix"(*or "Postfix"*)}];
GenericClass.treeIterate[fun_, opts:OptionsPattern[treeIterate]]:=
	Block[{result, condition}, 
		
		condition = OptionValue[treeIterate, {opts}, "Condition"];
	
		result =
			Switch[OptionValue[treeIterate, {opts}, "IterateOn"], 
				"Leafs"|All, 
					If[condition@o, 
						If[!OptionValue[treeIterate, {opts}, "ExternalIterate"], 
							{o.fun}
							, 
							{fun[o]}
						]
						, 
						{}
					]
				, 
				"Nodes", 
					{}
			];
			
		If[OptionValue[treeIterate, {opts}, "DeleteMissing"], 
			result = DeleteCases[result, Missing[___]|HoldComplete[_](*if function is not defined*)];
		];
		
		result
	];
GenericClass.getDepth[depth_:0, condition_:(#.hasParent[]&)]:=
	If[condition@o, 
		o["Parent"].getDepth[1+depth, condition]
		, 
		depth
	];
GenericClass.findObject[field_, value_, condition_:(True &)]:=
	If[o[field]===value, 
		o
		, 
		Missing[field]
	];
GenericClass.getRecursiveField[field_, operation_]:= o[field];
GenericClass.getRecursiveFunction[fun_, operation_]:= o.fun;
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* 		GenericClass/Display Interface*)
GenericClass.getInterface[type_:"Object"]:= GetInterface[o, type];
GenericClass.setInterface[interface_, type_:"Object"]:= SetInterface[o, interface, type];
GenericClass.modifyInterface[interfaceElement_, type_:"Object"]:= ModifyInterface[o, interfaceElement, type];
GenericClass.removeInterface[interfaceField_, type_:"Object"]:= RemoveInterface[o, interfaceField, type];
GenericClass.clearInterface[type_:"Object"]:= ClearInterface[o, type];
GenericClass.getInterfaceOrdering[type_:"Object"]:= GetInterfaceOrdering[o, type];
GenericClass.setInterfaceOrdering[interfaceOrdering_, type_:"Object"]:= SetInterfaceOrdering[o, interfaceOrdering, type];
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* 		GenericClass/Display *)
GenericClass.display[opts:OptionsPattern[EditSymbol]]:=
	Module[{returnOk}, 
		
		returnOk = EditSymbol[o, opts, "WindowTitle" -> o["Type"]];
		
		(*in case cancel has been pressed,  the result won't be refreshed if displayed,  so we do it*)
		If[!returnOk, 
			o.notify[];
		];
		
		returnOk
	];
GenericClass.displayPane[opts:OptionsPattern[EditSymbolPane]]:=EditSymbolPane[o, opts, "ScrollbarsOption" -> {False, False}, "Transpose" -> True]
GenericClass.registerDisplayedProperties[properties_]:=
	Module[{existingProperties, newProperties, oldProperties, propertiesType=None}, 
		
		propertiesType = properties[[1, 1]];
		existingProperties = o.getDisplayedProperties[propertiesType];
		
		Which[
			(*{{propertiesType}} used for deleting a propertiesType only*)
			Length@existingProperties <= Length@properties && properties =!= {{propertiesType}}, 
				(*old comment: we don't want to reregister a property if it's just different because of isDynamic*)
				newProperties = Complement[properties, existingProperties(*, "SameTest" -> (#1[[;;-2]] === #2[[;;-2]]&)*)];
				o.registerDisplayedProperty[##]& @@@ newProperties;
				
				o.deleteCases["DisplayedProperties", {propertiesType, __}];
				o.appendJoin["DisplayedProperties", properties];
			, 
			Length@existingProperties > Length@properties || properties === {{propertiesType}}, 
				oldProperties = Complement[existingProperties, properties];
				(
					o.unregisterDisplayedProperty[##];
					o.deleteCases["DisplayedProperties", {##}];
				)& @@@ oldProperties;
		];
		
		o["DisplayedProperties"]
	];
(*we use this in order to not execute a sub implementation*)
GenericClass.unregisterDisplayedProperties[propertiesType_]:= o.this.registerDisplayedProperties[{{propertiesType}}]; 
GenericClass.registerDisplayedProperty[__]:=Null;
GenericClass.unregisterDisplayedProperty[__]:=Null;
GenericClass.getDisplayedProperties[propertiesType_]:= o.cases["DisplayedProperties", {propertiesType, __}];
GenericClass.displayProperties[propertiesType_]:= o.displayProperty[##]& @@@ (o.getDisplayedProperties[propertiesType]);
GenericClass.displayProperty[propertiesType_, property_, label_, "Checkbox", callback_, isDynamic_:True]:=
	Checkbox[
		Dynamic[
			o["SelectedObject", property]
			, 
			callback[o, #]&
			, 
			GetTrackedSymbols[{o["SelectedObject"]}]
		]
	];
GenericClass.displayProperty[propertiesType_, property_, label_, propertyType_, formatType_, isDynamic_:False]:=
	If[isDynamic, 
		TrackObject[{o.getDisplayedTrackedObject[propertyType]}, 
			o.getFormatedDisplayedValue[property, propertyType, formatType]
		]
		, 
		o.getFormatedDisplayedValue[property, propertyType, formatType]
	];
GenericClass.getDisplayedTrackedObject[propertyType_]:= o;
GenericClass.getRowTrackedObject[]:= o;
GenericClass.getFormatedDisplayedValue[property_, propertyType_, formatType_]:= o.formatValue[o.displayValue[property, propertyType], propertyType, formatType];
GenericClass.displayValue[property_, {"RecursiveField", operation_}]:= o.getRecursiveField[property, operation];
GenericClass.displayValue[property_, {"Function", func_}]:= o.func;
GenericClass.displayValue[property_, propertyType_]:= o[property];
GenericClass.formatValue[value_, "ConditionalHighlight", {formatType_, {condition_, highlightColor_}}]:= 
	o.formatValue[value, "Property", formatType] // 
		If[o.condition, 
			Framed[#, "Background" -> Lighter@Orange, FrameMargins -> 1, FrameStyle -> None, ContentPadding -> False]
			, 
			#
		]&;
(*Default formatting for any propertyType*)
GenericClass.formatValue[value_, propertyType_, formatType_]:= $emptyElement /; !(NumberQ[value]||StringQ[value]);
GenericClass.formatValue[value_, propertyType_, "Date"]:= FormatCouchDate[value, True] /; StringQ@value;
GenericClass.formatValue[value_, propertyType_, "Number"]:= NumberForm[Round@value, {7, 0}, NumberSeparator -> ", ", DigitBlock  ->  3, NumberPoint  ->  ""] /; NumberQ@value;
GenericClass.formatValue[value_, propertyType_, "SmallNumber"]:= NumberForm[value, {7, 2}, NumberSeparator -> ", ", DigitBlock  ->  3] /; NumberQ@value;
GenericClass.formatValue[value_, propertyType_, "Percent"]:= NumberForm[100. value, {7, 2}, NumberSeparator -> ", ", DigitBlock  ->  3] /; NumberQ@value;
GenericClass.formatValue[value_, propertyType_, "RoundPercent"]:= Round[100. value] /; NumberQ@value;
GenericClass.formatValue[value_, propertyType_, "SmallPercent"]:= Round[100. value, 0.01] /; NumberQ@value;
GenericClass.formatValue[value_, propertyType_, "Default"]:= value;
GenericClass.formatValue[value_, propertyType_, formatType_]:= $emptyElement;
GenericClass.displayRow[list_, color_:None]:= Grid[{list}, ItemSize -> 7, Background -> color, Spacings -> {0, 0.5}];
GenericClass.treeExpand[propertiesType_, fgnTreeDepth_:"All", mainPortfolioName_:None]:=
	TrackObject[{o.getRowTrackedObject[]}, 
		o.displayRow[
			o.displayProperties[propertiesType]
			, 
			ToExpression@o["Color"]
		]
	];
GenericClass.notify[]:= 
	(
		(*changing one property of an object causes it to refresh in a table,  as the
		data association is tracked in Dynamic with TrackObject*)
		o."TickNotify" = !TrueQ@o["TickNotify"];
	);
GenericClass.getInputField[field_, {settings___}]:= 
	CustomInputField[
		TrackObject[{o}, 
			o[field]
			, 
			(o.field = #)&
		]
		, 
		settings
	];
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* 		GenericClass/Select/Edit *)
GenericClass.isSelected[]:= o["SelectedObject", "Selected"];
GenericClass.select[value_, notifyParent_:True]:=
	(
		o["SelectedObject"].select[value];
		
		If[notifyParent && o.hasParent[], 
			o["Parent"].childSelectNotify[];
		];
	);
GenericClass.isEditable[]:= o["SelectedObject", "Editable"];
GenericClass.edit[value_, notifyParent_:True]:=
	(
		o["SelectedObject"].edit[value];
		
		If[notifyParent && o.hasParent[], 
			o["Parent"].childEditNotify[];
		];
	);
GenericClass.singleEdit[condition_:(True &)]:=
	(
		If[o.hasParent[], 
			o.getRoot[condition].edit[False]; 
		];
		
		o.edit[True, False];
	);
GenericClass.getSelectedComponents[keepSpreads_:True]:=If[o.isSelected[], {o}, {}];
GenericClass.getEditableComponents[keepSpreads_:True]:=If[o.isEditable[], {o}, {}];
GenericClass.getComponents[keepSpreads_:True]:={o};
GenericClass.unGroup[]:= 
	Module[{parentObject, grandPa}, 
		
		parentObject = o["Parent"];
		
		If[o.hasParent[], 
			grandPa = parentObject["Parent"];
			
			If[grandPa =!= None, 
				grandPa.appendComponent[o];
				o.remove[];
				
				If[!parentObject.hasComponent[], 
					parentObject.remove[];
				];
			];
		];
	];
Options[move] = {"DeleteEmptySpread" -> True, "AppendObject" -> True};
GenericClass.move[targetSpread_, opts:OptionsPattern[move]]:= 
	Module[{parentObject}, 
		
		parentObject = o["Parent"];
		
		If[parentObject =!= targetSpread && targetSpread =!= o, 
			o.remove[];
			
			If[OptionValue[move, {opts}, "AppendObject"], 
				targetSpread.appendComponent[o];
				, 
				targetSpread.prependComponent[o];
			];
			
			If[!parentObject.hasComponent[] && OptionValue[move, {opts}, "DeleteEmptySpread"], 
				parentObject.remove[];
			];
		];
	];
GenericClass.copy[]:= 
	Module[{copiedComponent}, 
		copiedComponent = o.super.copy[];
		copiedComponent.setId[];
		
		copiedComponent."SelectedObject" = o["SelectedObject"].copy[];
		
		copiedComponent
	];
GenericClass.pasteCopy[targetSpread_, append_]:= 
	Module[{copiedComponent=o.copy[]}, 
		If[append, 
			targetSpread.appendComponent[copiedComponent]
			, 
			targetSpread.prependComponent[copiedComponent]
		]
	];
GenericClass.duplicate[append_:True]:= 
	Module[{copiedComponent}, 
		
		If[o.hasParent[], 
			copiedComponent=o.copy[];
			
			If[append, 
				o["Parent"].appendComponent[copiedComponent]
				, 
				o["Parent"].prependComponent[copiedComponent]
			]
		]
	];
GenericClass.moveUp[]:= 
	Module[{parentComponents, objectPosition}, 
		
		If[o.hasParent[], 
			parentComponents = o["Parent", "Components"];
			objectPosition = Position[parentComponents, o][[1, 1]];
			
			If[objectPosition > 1, 
				permutate[parentComponents, objectPosition, objectPosition-1];
				o["Parent"]."Components" = parentComponents;
				o["Parent"].setComponentIds[];
			];
		];
	];
GenericClass.moveDown[]:= 
	Module[{parentComponents, objectPosition}, 
		
		If[o.hasParent[], 
			parentComponents = o["Parent", "Components"];
			objectPosition = Position[parentComponents, o][[1, 1]];
			
			If[objectPosition < Length@parentComponents, 
				permutate[parentComponents, objectPosition, objectPosition+1];
				o["Parent"]."Components" = parentComponents;
				o["Parent"].setComponentIds[];
			];
		];
	];
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* 		GenericClass/DB/Save *)
GenericClass.createObjectFromAssociation[assoc_]:= 
	Module[{objectType}, 
		objectType=ToExpression[assoc["ObjectType"]];
		New[objectType][Normal@assoc]
	];
GenericClass.getData[]:= o.data[];
GenericClass.save[fileName_]:= 
	(
		(*save in wdx format*)
		$SavedObjects = o.getData[];
		Export[fileName, Compress@$SavedObjects]
	);
GenericClass.load[fileName_]:= 
	(
		$SavedObjects = Import[fileName] // Uncompress;
		o.createObjectFromAssociation[$SavedObjects]
	);
GenericClass.setLocalSave[value_]:= o."LocalSave" = value; 
GenericClass.getIdFields[]:= {"ObjectType"};
GenericClass.getSavedFields[fieldsField_:"ObjectSavedFields"]:= 
	If[o["LocalSave"] && o.fieldExistsQ["Local"~~fieldsField], 
		o.fieldTake[o["Local"~~fieldsField]]
		, 
		o.fieldTake[o[fieldsField]]
	] // 
	(*we delete values which are the same as default values,  except the ones from getIdFields[]*)
	Complement[#, 
		Association[
			DeleteCases[
				Options[o.type[]]
				, 
				(Alternatives @@ (o.getIdFields[]))  ->  _
			] 
		]
	]&;
GenericClass.getBackupFieldName[fieldsField_:"ObjectSavedFields"]:= fieldsField~~"Backup";
GenericClass.getBackup[fieldsField_:"ObjectSavedFields"]:= o.get[o.getBackupFieldName[fieldsField]];
GenericClass.setBackup[fieldsField_:"ObjectSavedFields"]:= o.set[o.getBackupFieldName[fieldsField], o.getSavedFields[fieldsField]];
GenericClass.isTreeModified[fieldsField_:"ObjectSavedFields"]:= o.getSavedFields[fieldsField] =!= o.getBackup[fieldsField]; (*for online save*)
GenericClass.getSavedTree[fieldsField_:"ObjectSavedFields"]:= o.getSavedFields[fieldsField]; (*for local save*)
GenericClass.getSavedDbTree[fieldsField_:"ObjectSavedFields"]:= (*for online save*)
	Module[{associationSaved, backupFields}, 

		associationSaved = o.getSavedFields[fieldsField];
		backupFields = o.getBackup[fieldsField];
		
		If[associationSaved =!= backupFields, 			
			o.storeInsert[associationSaved]
			, 
			o.storeUpdate[]
		]
	];
GenericClass.storeInsert[associationSaved_]:= Association["Action" -> "Insert", "Data" -> associationSaved];
GenericClass.storeUpdate[]:= Association["Action" -> "UpdateSavedDate", "Data" -> o["Id"]];
couchQuery::queryErr = "`1` : Query returned with errors: `2`.";
Options[couchQuery] = Join[Options[CouchQuery], {"Callback" -> Identity, "AsynchronousCall" -> False}];
GenericClass.couchQuery[query_, opts:OptionsPattern[couchQuery]]:= 
	With[{callback=OptionValue[couchQuery, {opts}, "Callback"], couchQueryOptions = FilterRules[{opts}, Options[CouchQuery]]}, 
		o.asyncEvaluate[
			CouchQuery[(*ViewIt@*)query, couchQueryOptions] (*// PrintDateString*)
			, 
			"Callback" -> 
				((
					If[#["HasError"],  (*insert not Ok*)
						Message[couchQuery::queryErr, o["Id"], query];
						inputQuery = query;
						o.logIt["couchQuery failed", "Type" -> "Debug", "Context":>inputQuery];
					];
					
					callback@#
				)&)
			, 
			"AsynchronousCall" -> OptionValue[couchQuery, {opts}, "AsynchronousCall"], 
			"ParallelKernel" -> "DBKernel"
		]
	];
couchInsert::insertErr = "`1` : Insert returned with errors.";
Options[couchInsert] = Join[Options[CouchInsert], {"AsynchronousCall" -> True}];
GenericClass.couchInsert[docs_List, opts:OptionsPattern[couchInsert]]:= 
	With[{couchInsertOptions = FilterRules[{opts}, Options[CouchInsert]]}, 
		o.asyncEvaluate[
			CouchInsert[docs, couchInsertOptions] 
			, 
			"Callback" -> 
				(
					If[AnyTrue[#, #["HasError"]&],  (*insert not Ok*)
						Message[couchInsert::insertErr, o["Id"]];
						insertedDocs = docs;
						o.logIt["couchInsert failed", "Type" -> "Debug", "Context":>insertedDocs, "PrintMessage" -> True];
					]&
				)
			, 
			"AsynchronousCall" -> OptionValue[couchInsert, {opts}, "AsynchronousCall"], 
			"ParallelKernel" -> "DBKernel"
		]
	];
GenericClass.databinInsert[databinId_, assoc_]:= 
	o.asyncEvaluate[
		DatabinAdd[databinId, assoc];
	];
GenericClass.sendMail[subject_, message_, opts:OptionsPattern[SendEmail]]:= SendEmail[subject, message, opts];
GenericClass.logIt[comment_, opts___]:= LogIt[StringTemplate["`1` : `2`"][o["Id"], comment], opts];
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* 		GenericClass/Journal *)
GenericClass.getJournalIds[displayJournalEntryType_]:= 
	Module[{idDateRules, entryDates, types, keys, nonSavedCommentPositions}, 
		
		If[o["Journal"]===Association[], 
			idDateRules = {None -> "No Entry"};
			, 			
			keys = o.getKeys["Journal"];
			entryDates = FormatCouchDate /@ (o.getJournalEntryField[#, "CreatedDate"]& /@ keys);
			
			(*we add the type of an entry,  for example Comment or Execution*)
			If[displayJournalEntryType, 
				types = o.thread[getJournalEntryType[keys]];
				entryDates = Thread[entryDates~~" / "~~types];
			];
			
			(*We display if a comment has been saved or not*)
			nonSavedCommentPositions = Position[o["Journal"]//Values, x_ /; (x["ObjectType"]=="Comment" && x["SavedDate"]=="") ];
			entryDates = MapAt[#~~" (Not Saved)"&, entryDates, nonSavedCommentPositions];
			
			idDateRules = Thread[keys -> entryDates];
		];
		
		idDateRules = idDateRules // Reverse;
		idDateRules
	];
GenericClass.appendDocToJournal[doc_]:= 
	Module[{existingIds, newId}, 
		
		existingIds = o.getKeys["Journal"];
		newId = FormatCouchDate@CouchDate[];
		o.setKey["Journal", newId, doc];
		
		newId
	];
GenericClass.appendComment[]:= 
	Module[{creationDate=CouchDate[], newDocument}, 
		
		newDocument = 
			Association[
				"ObjectId" -> o["Id"], 
				"CreatedDate" -> creationDate, 
				"SavedDate" -> "", 
				"ObjectType" -> "Comment", 
				"Comment" -> ""
			];
		
		o.appendDocToJournal[newDocument]
	];
GenericClass.saveComment[id_, content_]:=
	Module[{existingDocument}, 
		existingDocument = o["Journal", id];
		existingDocument["Comment"]=content;
		existingDocument["SavedDate"]=CouchDate[];
		
		o.setKey["Journal", id, existingDocument]
	];
GenericClass.appendComment[content_]:= 
	Module[{id}, 
		id = o.appendComment[];
		o.saveComment[id, content];
		
		id
	];
GenericClass.deleteDocFromJournal[id_]:= 
	Module[{existingIds, newId}, 
		
		If[id =!= None, 
			existingIds = o.getKeys["Journal"];
			o.deleteKey["Journal", id];
			newId = GetSelectedKeyAfterDelete[existingIds, id];
		];

		newId
	];
GenericClass.getJournalEntryField[id_, field_]:= o["Journal", id, field];
GenericClass.getJournalEntryType[id_]:= o.getJournalEntryField[id, "ObjectType"];
(*Note: don't overload definitions with a different number of arguments accross an inheritance tree,  or use sub in the super class*)
GenericClass.getJournalEntry[id_]:= o.getJournalEntryContent[id, o.getJournalEntryType[id]];
GenericClass.getJournalEntryContent[id_, "Comment"]:= o["Journal", id, "Comment"];
GenericClass.getJournalIdField[id_, field_, "CouchDate"]:= o.getJournalIdField[id, field, "Default"]//FormatCouchDate;
GenericClass.getJournalIdField[id_, field_, type_:"Default"]:= o.getJournalEntryField[id, field];
GenericClass.showJournal[displayJournalEntryType_:False]:=
	DynamicModule[
		{
			journalIds, selectedCommentId, updateComment, 
			fieldContent, commentFieldEnabled
		}, 
		
		If[o =!= None, 
			updateComment[{id_}]:=
				(						
					selectedCommentId = {id};
					
					If[id =!= None, 
						fieldContent=o.getJournalEntry[id];
						commentFieldEnabled = o.getJournalEntryType[id]=="Comment";
						, 
						fieldContent = "";
						commentFieldEnabled = False;	
						selectedCommentId = {};			
					];
				);
				
			journalIds = o.getJournalIds[displayJournalEntryType];
			updateComment[{journalIds[[1, 1]]}];
	
			Grid[
				{
					{
						"Description"
						, 
						o.getInputField["Description", {True, {400, 19}, False, False}]
					}
					, 
					{
						"Journal"
						, 
						TrackObject[{o}, 
							ListPicker[
								Dynamic[
									selectedCommentId
									, 
									updateComment[#]&
									, 
									TrackedSymbols :> {selectedCommentId}
								]
								, 
								o.getJournalIds[displayJournalEntryType]
								, 
								Multiselection -> False
							]
						]
					}
					, 
					{
						Button[
							"Delete Entry"
							, 
							If[commentFieldEnabled && ChoiceDialog["Do you really want to delete this entry?"], 
								updateComment[{o.deleteDocFromJournal[First@selectedCommentId]}];
							];
							, 
							Method -> "Queued"
						]
						, 
						Button[
							"Add Entry"
							, 
							updateComment[{o.appendComment[]}];
						]
					}
					, 
					{
						"Input"
						, 
						CustomInputField[
							Dynamic[
								fieldContent
								, 
								(
									If[# =!= fieldContent, 
										fieldContent = #;
									]
								)&
								, 
								TrackedSymbols :> {fieldContent}
							]
							, 
							TrackSymbol[commentFieldEnabled]
							, 
							{400, 100}, True, True
						]
					}
					, 
					{
						Null
						, 
						Button[
							"Save Entry"
							, 
							If[selectedCommentId =!= {} && o.getJournalEntryType[First@selectedCommentId]=="Comment" &&
							   ChoiceDialog["Do you really want to save this entry?"], 
								o.saveComment[First@selectedCommentId, fieldContent];
							];
							, 
							Method -> "Queued"
						]
					}
				}	
				, 
				Alignment  ->  Left									
			]
			, 
			"No object loaded"
		]
	];
(*commentModule is updated when currentObject changes*)
CommentModuleOpener[currentObject_, title_, displayJournalEntryType_:False]:=
	OpenerView[
		{
			title
			, 
			TrackObject[{currentObject}, 
				If[currentObject.isEmpty[], 
					"No object loaded"	
					, 
					Dynamic[
						currentObject["Value"].showJournal[displayJournalEntryType]
					]
				]
			]
		}
	];
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* 		GenericClass/Ref *)
GenericClass.makeRef[key_]:= o.setKey["Refs", key, MakeRef[None]];
GenericClass.setRef[key_, value_]:=
	Block[{ref}, 
		ref=o["Refs", key];
		ref=value
	];
GenericClass.getRef[key_]:=RefValue@o["Refs", key];
GenericClass.addObserver[key_, observer_]:= Observe[o["Refs", key], observer[#2]&];
GenericClass.removeObserver[key_, observer_]:=RemoveObserver[o["Refs", key], observer[#2]&];
GenericClass.getObservers[key_]:=$Observers[o["Refs", key]];
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* 		GenericClass/ValueObject *)
GenericClass.createValue[fields_List]:= o.thread[createValue[fields]];
GenericClass.createValue[field_ -> value_]:= o.createValue[field, value];
GenericClass.createValue[field_, value_:None]:= o.field = New[ValueObject]["Value" -> value];
GenericClass.setValue[values_List]:= o.thread[setValue[values[[All, 1]], values[[All, 2]]]];
GenericClass.setValue[field_, value_]:= o.set[field, "Value", value];
GenericClass.resetValue[fields_List]:= o.thread[resetValue[fields]];
GenericClass.resetValue[field_]:= o[field].reset[];
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* 		GenericClass/Scheduled tasks *)
SetAttributes[{createScheduledTask, runScheduledTask}, HoldRest];
GenericClass.createScheduledTask[taskKey_, {settings___}, code_]:=
	o.setKey[
		"ScheduledTasks", taskKey
		, 
		CreateScheduledTask[
			code
			, 
			(*time in seconds*)
			settings
		]	
	];
GenericClass.startScheduledTask[taskKey_]:=StartScheduledTask[o["ScheduledTasks", taskKey]];
GenericClass.runScheduledTask[taskKey_, {settings___}, code_]:=
	(
		o.createScheduledTask[taskKey, {settings}, code];
		o.startScheduledTask[taskKey];
	);
GenericClass.stopScheduledTask[taskKey_]:=StopScheduledTask[o["ScheduledTasks", taskKey]];
GenericClass.evaluateScheduledTask[taskKey_]:=EvaluateScheduledTask[o["ScheduledTasks", taskKey]];
GenericClass.nextScheduledTaskTime[taskKey_]:=NextScheduledTaskTime[o["ScheduledTasks", taskKey]]; (*for cron tasks only*)
GenericClass.resetScheduledTask[taskKey_, {settings___}]:=ResetScheduledTask[o["ScheduledTasks", taskKey], settings];
GenericClass.removeScheduledTask[taskKey_]:=
	If[o.keyExistsQ["ScheduledTasks", taskKey], 
		RemoveScheduledTask[o["ScheduledTasks", taskKey]];
		o.deleteKey["ScheduledTasks", taskKey];
	];
GenericClass.resetScheduledTask[taskKey_, {settings___}]:=ResetScheduledTask[o["ScheduledTasks", taskKey], settings];
GenericClass.taskExists[taskKey_]:= o.keyExistsQ["ScheduledTasks", taskKey];
SetAttributes[deferredTask, HoldRest];
GenericClass.deferredTask[{taskName_, taskIdsObject_, taskIds_, taskDelay_}, body_]:=
	(	
		o.createScheduledTask[
			taskName, {{taskDelay}}
			, 
			If[Length@taskIdsObject[taskIds] == 1, 
				
				(*we remove the id before starting to save*)
				taskIdsObject.deleteCases[taskIds, First@$ScheduledTask];
				RemoveScheduledTask[$ScheduledTask];
				
				body
				, 
				(*If another save is pending we don't save and delete the id*)
				taskIdsObject.deleteCases[taskIds, First@$ScheduledTask];
				RemoveScheduledTask[$ScheduledTask];
			];
		];
			
		taskIdsObject.appendTo[taskIds, First@o["ScheduledTasks", taskName]];
		
		o.startScheduledTask[taskName];
	);
GenericClass.runActionTask[actionKey_]:= 
	o.runScheduledTask[actionKey, o["ActionAssociation", actionKey][[1]],  (*settings*)
		If[(o["ActionAssociation", actionKey][[2]])[],  (*control*)
			(o["ActionAssociation", actionKey][[3]])[]; (*action*)
		];
	];
GenericClass.runActionTasks[]:= o.thread[runActionTask[o.getKeys["AssociationTask"]]];
GenericClass.stopActionTasks[]:= o.thread[removeScheduledTask[o.getKeys["AssociationTask"]]]; 
SetAttributes[asyncEvaluate, HoldFirst];
GenericClass.asyncEvaluate[parallelAction_, opts:OptionsPattern[AsyncEvaluate]]:= AsyncEvaluate[parallelAction, opts]; 
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* 		GenericClass/PropertiesCheckboxBar *)
PropertiesCheckboxBar = 
	NewClass[
		"Parents" -> {GenericClass}, 
		"Fields" -> 
			{
				"Properties", 
				"UpdateFunction", 
				"GetCurrentParameters", 
				"SetCurrentParameters", 
				"Title"
			}
	];
PropertiesCheckboxBar.init[_]:= (*Model*)
	(
		(*we create separate objects to track*)
		o.createValue[{"SelectedVariables", "AllSelected"}];
		
		(*Labels*)
		o."PropertiesLabels" = o["Properties"][[All, 3]];
		
		(*Association to easily get the specifications of a property by its label*)
		o."PropertiesByLabel" = AssociationThread[o["PropertiesLabels"] -> o["Properties"]];
		
		(*Display of FullLabel (ShortcutLabel) when they are different*)
		o."DisplayedPropertiesLabels" = Thread[
		     	o["PropertiesLabels"]  ->  
		     	MapThread[
		     		If[#1 != #2, 
		     			#2 ~~ " (" ~~ #1 ~~ ")"
		     			, 
		     			#2
		     		]&
		     		, 
		     		{o["PropertiesLabels"], o["Properties"][[All, 2]]}
		     	]
		     ];
		
		(*We initialize the state variables*)
		o.updateSelectedProperties[];
	);
PropertiesCheckboxBar.updateSelectedProperties[variables_:Null]:= (*Controller*)
	Module[{usedVariables}, 
		
		(*if the variables are not passed as argument,  we read them from propertiesField*)
		If[(usedVariables = variables) === Null, 
			If[o["GetCurrentParameters"] =!= None, 
				usedVariables = o["GetCurrentParameters"][]//GetNonEmptyColumn[#, 3]&;
				, 
				usedVariables = All;
			];
		];
			
		(*setting the selected variables in "SelectedVariables"*)	
		Which[
			usedVariables === None, 
				o.setValue["SelectedVariables", {}];
			, 
			usedVariables === All, 
				o.setValue["SelectedVariables", o["PropertiesLabels"]];
			, 
			True, 		
				o.setValue["SelectedVariables", usedVariables];	
		];
		
		(*changing the appearance of the All/None checkbox in "AllSelected"*)
		o.setValue["AllSelected", 
			Switch[Length@o["SelectedVariables", "Value"], 
				0, 
					False, 
				Length@o["PropertiesLabels"], 
					True, 
				_, 
					3
			]
		];
	
		(*we set the value of the selected labels*)
		o["SetCurrentParameters"][o["PropertiesByLabel", #]& /@ o["SelectedVariables", "Value"]];
		
		(*we call the update function that will update the display in other parts of the application*)
		o["UpdateFunction"][];
	];
PropertiesCheckboxBar.show[]:= (*View*)
	OpenerView[
		{
			o["Title"]
			, 
			Grid[
				{
					{"Selected"}
					, 
					{
						Row[
							{
								Checkbox[
									TrackObject[o[{"AllSelected"}], 
										o["AllSelected", "Value"]
										, 
										o.updateSelectedProperties[If[#, All, None]]&
									]
								]
								, 
								" All/None"
							}
						]
					}
					, 
					{
						CheckboxBar[
							(*TrackedSymbols doesn't work with CheckboxBar so can't use TrackObject*)
							Dynamic[
								o["SelectedVariables", "Value"]
								, 
								o.updateSelectedProperties[#]&
							]
							, 
							o["DisplayedPropertiesLabels"]
							, 
							Appearance -> "Vertical"
						]
					}
				}
				, 
				Alignment  ->  {{Left}}
			]	
		}	
		, 
		False
	];
(* ::Subsubsection:: *)
(* ::Subsection:: *)
(* ::Subsection:: *)
(* GenericGroup *)
(* ::Subsubsection:: *)
(* 		GenericGroup/Declaration *)
GenericGroup=
	NewClass[
		"Parents" -> {GenericClass}, 
		"Fields" -> 
			{
				"Components" -> {}, "DefaultOpenState", "ComponentIds" -> {}, 
				(*"OverloadedProperties"*)
				"ObjectType" -> "GenericGroup"
			}
	];
GenericGroup.init[options_]:= 
	(
		o.setIdIfNotDef[options];
		
		If[o.hasComponent[], 
			o.setComponentIds[];
		];
	);
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* 		GenericGroup/Id *)
GenericGroup.getId[]:=o["ObjectType"]~~"_"~~o["Type"]~~"_"~~(o.getIdDate[]);
GenericGroup.setComponentIds[]:= o."ComponentIds" = o.iterate[get["Id"]];
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* 		GenericGroup/Tree functions *)
(*$CurrentSpread.executeTable["Symbol", {{"Price", "price", "Price"}, {"Delta", "price", "Delta"}}]*)
Options[executeTable]={"Title" -> "", "Condition" -> (True&), "OutputData" -> False};
GenericGroup.executeTable[idField_, functions_, opts:OptionsPattern[executeTable]]:=
	Module[{functionsPerComponent, functionsLabels, ids, condition, outputData}, 
		
		condition = OptionValue[executeTable, {opts}, "Condition"];
		outputData = OptionValue[executeTable, {opts}, "OutputData"];
		
		functionsPerComponent = o.iterate[executeFunctions[functions], "Condition" -> condition];
		
		If[outputData, 
			functionsPerComponent
			, 
			If[functionsPerComponent === {}, 	
				"No table to display"
				, 
				ids = o.iterate[get[idField], "Condition" -> condition];
				functionsLabels = Keys@First@functionsPerComponent;
				FrozenPaneGrid[OptionValue[executeTable, {opts}, "Title"], functionsLabels, ids, functionsPerComponent//Values]
			]
		]
	];
GenericGroup.setField[field_, value_]:= o.superIterate[setField[field, value]];
GenericGroup.setParentToChildren[]:= o.iterate[set["Parent", o]];
GenericGroup.appendToComponents[newComponent_, setCompIds_:True]:= 
	(
		o.appendTo["Components", newComponent];
		
		(*it can be useful to not overwrite ids,  for example when loading 
		orders,  their components and their subOrders,  if we set component ids 
		when adding orders the components are overwritten*)
		If[setCompIds, 
			o.setComponentIds[];
		];
	);
GenericGroup.appendComponent[newComponent_, setComponentIds_:True]:=
	(
		o.appendToComponents[newComponent, setComponentIds];
		newComponent."Parent" = o;
		
		newComponent
	);
Options[appendComponentToAssociation] = {"AssocField" -> "Symbols", "IdField" -> "Symbol", "SetComponentIds" -> True};
GenericGroup.appendComponentToAssociation[newComponent_, opts:OptionsPattern[appendComponentToAssociation]]:=
	Block[{idField, fieldValue}, 
		
		idField = OptionValue[appendComponentToAssociation, {opts}, "IdField"];
		fieldValue = newComponent[idField];
		
		o.deleteCases["Components", x_ /; x[idField] === fieldValue];
		
		(*we use this in order to not execute a sub implementation*)
		o.this.appendComponent[newComponent, OptionValue[appendComponentToAssociation, {opts}, "SetComponentIds"]];
		
		o.setKey[
			OptionValue[appendComponentToAssociation, {opts}, "AssocField"], 
			fieldValue, 
			newComponent
		];
		
		newComponent
	];
GenericGroup.prependToComponents[newComponent_, setCompIds_:True]:= 
	(
		o.prependTo["Components", newComponent];
		
		If[setCompIds, 
			o.setComponentIds[];
		];
	);
GenericGroup.prependComponent[newComponent_, setComponentIds_:True]:=
	(
		o.prependToComponents[newComponent, setComponentIds];
		newComponent."Parent" = o;
		
		newComponent
	);
Options[prependComponentToAssociation] = {"AssocField" -> "Symbols", "IdField" -> "Symbol", "SetComponentIds" -> True};
GenericGroup.prependComponentToAssociation[newComponent_, opts:OptionsPattern[prependComponentToAssociation]]:=
	Block[{idField, fieldValue}, 
		
		idField = OptionValue[prependComponentToAssociation, {opts}, "IdField"];
		fieldValue = newComponent[idField];
		
		o.deleteCases["Components", x_ /; x[idField] === fieldValue];
		
		(*we use this in order to not execute a sub implementation*)
		o.this.prependComponent[newComponent, OptionValue[prependComponentToAssociation, {opts}, "SetComponentIds"]];
		
		o.setKey[
			OptionValue[prependComponentToAssociation, {opts}, "AssocField"], 
			fieldValue, 
			newComponent
		];
		
		newComponent
	];
GenericGroup.removeComponent[elem_]:= 
	(
		o.deleteCases["Components", elem];
		o.setComponentIds[];
	);
Options[removeComponentFromAssociation] = {"AssocField" -> "Symbols", "IdField" -> "Symbol"};
GenericGroup.removeComponentFromAssociation[id_, opts:OptionsPattern[removeComponentFromAssociation]]:=
	Block[{objectToRemove}, 
		objectToRemove = o.findObject[OptionValue[removeComponentFromAssociation, {opts}, "IdField"], id];
		o.removeComponent[objectToRemove];
		o.deleteKey[OptionValue[removeComponentFromAssociation, {opts}, "AssocField"], id];
	];
GenericGroup.clearComponents[]:= 
	(
		o."Components" = {};
		o.setComponentIds[];
	);
GenericGroup.delete[]:= o.superIterate[delete[], "TraversalOrder" -> "Postfix"];
GenericGroup.length[]:= Length@o["Components"];
GenericGroup.leafsLength[]:= o.treeIterate[get["Id"], "IterateOn" -> "Leafs"]//Length;
GenericGroup.hasComponent[]:= o.length[] != 0;
GenericGroup.getComponent[index_]:= o.getPart["Components", index];
GenericGroup.getFirstComponent[]:= o.getComponent[1];
GenericGroup.hasRootOnly[]:= o.length[] == 1 && !o.getFirstComponent[].hasParent[];
GenericGroup.componentsMatchQ[pattern_]:=AllTrue[o["Components"], MatchQ[#, pattern]&];
SetAttributes[superIterate, HoldFirst];
GenericGroup.anyTrue[f_]:=AnyTrue[o["Components"], #.f &];
GenericGroup.componentsThread[fun_[otherArgs___, list_]]:= MapThread[#1.fun[otherArgs, #2]&, {o["Components"], list}];
superIterate::unknownTraversal = "`1` is an unknown TraversalOrder option.";
SetAttributes[{superIterate, selfIterate}, HoldFirst];
Options[superIterate] = Join[Options[iterate], {"SuperClass" -> Automatic, "TraversalOrder" -> "Prefix"(*or "Postfix"*)}];
Final@GenericGroup.superIterate[fun_, opts:OptionsPattern[superIterate]]:= 
	Block[{superClass, superResult, iterationResult, traversalOrder}, 

		superClass = OptionValue[superIterate, {opts}, "SuperClass"];
		traversalOrder = OptionValue[superIterate, {opts}, "TraversalOrder"];
		
		Switch[traversalOrder, 
			"Prefix", 
				superResult =
					Switch[superClass, 
						Automatic, 
							o.super.fun, 
						None, 
							o.fun, 
						_, 
							o.super[superClass].fun
					];
					
				iterationResult = o.iterate[fun, FilterRules[{opts}, Options[iterate]]];
				
				Prepend[iterationResult, superResult]
			, 
			"Postfix", 
				iterationResult = o.iterate[fun, FilterRules[{opts}, Options[iterate]]];
				
				superResult =
					Switch[superClass, 
						Automatic, 
							o.super.fun, 
						None, 
							o.fun, 
						_, 
							o.super[superClass].fun
					];
					
				Append[iterationResult, superResult]
			, 
			_, 
				Message[superIterate::unknownTraversal, traversalOrder];
				Abort[];
		]
	];
GenericGroup.selfIterate[fun_, opts:OptionsPattern[superIterate]]:= o.superIterate[fun, "SuperClass" -> None, opts];
GenericGroup.treeIterate[fun_, opts:OptionsPattern[treeIterate]]:= 
	Block[{nodeValue, treeValues, result, iterationResult, condition, traversalOrder}, 
		
		condition = OptionValue[treeIterate, {opts}, "Condition"];
		traversalOrder = OptionValue[treeIterate, {opts}, "TraversalOrder"];
		
		result =
			Switch[OptionValue[treeIterate, {opts}, "IterateOn"], 
				"Leafs", 
					iterationResult = o.iterate[treeIterate[fun, opts]];
					Join @@ iterationResult
				, 
				"Nodes"|All, 
					If[traversalOrder=="Postfix", 
						iterationResult = o.iterate[treeIterate[fun, opts]];
						treeValues = Join @@ iterationResult;
					];
					
					If[condition@o, 
						If[!OptionValue[treeIterate, {opts}, "ExternalIterate"], 
							nodeValue = {o.fun};
							, 
							nodeValue = {fun[o]};
						];
						, 
						nodeValue = {};
					];
					
					If[traversalOrder=="Prefix", 
						iterationResult = o.iterate[treeIterate[fun, opts]];
						treeValues = Join @@ iterationResult;
					];
					
					Join[nodeValue, treeValues]
			];
			
		If[OptionValue[treeIterate, {opts}, "DeleteMissing"], 
			result = DeleteCases[result, Missing[___]|HoldComplete[_](*if function is not defined*)];
		];
		
		result
	];
SetAttributes[selectComponents, HoldFirst];
GenericGroup.selectComponents[fun_, opts:OptionsPattern[iterate]]:=
	Module[{conditions, componentField}, 
		
		conditions = o.iterate[fun, opts] // Map@TrueQ;
		componentField = OptionValue[iterate, {opts}, "Field"];
		
		Pick[o[componentField], conditions]
	];
(*TODO allow to return several objects*)
GenericGroup.findObject[field_, value_, condition_:(True &)]:=
	Module[{candidates}, 
		If[o[field]===value, 
			o
			, 
			candidates = DeleteCases[o.iterate[findObject[field, value, condition], "Condition" -> condition], Missing[field]];
			
			If[Length@candidates == 1, 
				First@candidates
				, 
				Missing[field]
			]
		]
	];
GenericGroup.objectPosition[object_]:=
	Module[{position}, 
		position = Position[o["Components"], object];
		
		If[position =!= {}, 
			position[[1, 1]]
			, 
			0
		]
	];
GenericGroup.sortBy[{property_, propertyType_}, increasing_:True]:=
	Module[{propertiesOrdered}, 
		
		o.iterate[sortBy[{property, propertyType}, increasing], "Condition" -> (SubClass[GenericGroup][#]&)];
		
		propertiesOrdered = o.iterate[displayValue[property, propertyType]] // Ordering // If[increasing, Identity, Reverse];
		o."Components" = o["Components"][[propertiesOrdered]];
		o.setComponentIds[];
	];
GenericGroup.getRecursiveField[field_, operation_]:= o.iterate[getRecursiveField[field, operation]] // operation;
GenericGroup.getRecursiveFunction[fun_, operation_]:= o.iterate[getRecursiveFunction[fun, operation]] // operation;
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* 		GenericGroup/Display *)
GenericGroup.registerDisplayedProperties[properties_]:= o.superIterate[registerDisplayedProperties[properties]];
GenericGroup.unregisterDisplayedProperties[propertiesType_]:= o.superIterate[unregisterDisplayedProperties[propertiesType]];
GenericGroup.notify[]:= o.superIterate[notify[]];
GenericGroup.treeExpand[propertiesType_, fgnTreeDepth_:"All", mainPortfolioName_:None]:=
	If[o.hasComponent[] && (fgnTreeDepth === "All" || o["Portfolio"] === mainPortfolioName || fgnTreeDepth>1), 
		openerView[
			o.super.treeExpand[propertiesType]
			, 
			If[fgnTreeDepth === "All" || o["Portfolio"] === mainPortfolioName, 
				o.iterate[treeExpand[propertiesType, fgnTreeDepth, mainPortfolioName], "Condition" -> (#["VisibleInTree"]&)]
				, 
				o.iterate[treeExpand[propertiesType, fgnTreeDepth-1, mainPortfolioName], "Condition" -> (#["VisibleInTree"]&)]
			]
			, 
			o["DefaultOpenState"]
		]
		, 
		o.super.treeExpand[propertiesType]
	];
treeDisplay::argErr = "The tree displayed must be a list.";
Options[treeDisplay] = {"ForeignTreeDepth" -> "All", "MainPortfolioName" -> None, "SortFunction" -> None};
GenericGroup.treeDisplay[roots_List, propertiesType_, opts:OptionsPattern[treeDisplay]]:=
	DynamicModule[{header, allDisplayedProperties, i=1, sortFunction}, 
		If[roots === {}, 
			"Empty "~~StringReplace[propertiesType, "Fields" -> ""]~~" table"
			, 
			(*dataType, property, label, propertyType, formatType, isDynamic*)
			allDisplayedProperties = First[roots].getDisplayedProperties[propertiesType];
			header = allDisplayedProperties[[All, 3]];
			
			If[(sortFunction = OptionValue[treeDisplay, {opts}, "SortFunction"]) =!= None, 
				
				header =
					Table[
						With[{j=j}, 
							EventHandler[
								MouseAppearance[
									header[[j]]
									, 
									Framed[Style["Left Click Sort\nRight Click Reverse Sort", 9], Background -> White]
								]
								, 
								{
									{"MouseClicked", 1}:>
										(
											i=j;
											(*property and propertyType are passed as arguments to sortBy and then displayValue*)
											sortFunction[allDisplayedProperties[[i, {2, 4}]], True];
										)
									, 
									{
									"MouseClicked", 2}:>
										(
											i=j;
											sortFunction[allDisplayedProperties[[i, {2, 4}]], False];
										)
								}
							]
						]
						, 
						{j, Length@header}
					];
			];
			
			Map[
				#.treeExpand[
					propertiesType, 
					OptionValue[treeDisplay, {opts}, "ForeignTreeDepth"], 
					OptionValue[treeDisplay, {opts}, "MainPortfolioName"]
				]&
				, 
				roots
			] // OpenerTree[#, o.displayRow[header], First[roots]["Id"]]&
		]
	];
GenericGroup.treeDisplay[x___]:= Null /; (Message[treeDisplay::argErr]; False);
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* 		GenericGroup/Select/Edit *)
GenericGroup.select[value_, notifyParent_:True]:=
	(
		o.super.select[value, notifyParent];
		o.iterate[select[value, False]];
	);
GenericGroup.edit[value_, notifyParent_:True]:=
	(
		o.super.edit[value, notifyParent]; 
		o.iterate[edit[value, False]]
	);
GenericGroup.childSelectNotify[]:=
	Module[{selectedFields}, 
		
		selectedFields = o.iterate[isSelected[]];
		
		Which[
			AllTrue[selectedFields, (#===True)&], 
				o["SelectedObject"].select[True];
			, 
			AllTrue[selectedFields, (#===False)&], 
				o["SelectedObject"].select[False];
			, 
			True, 
				o["SelectedObject"].select[3];
		];
		
		If[o.hasParent[], 
			o["Parent"].childSelectNotify[];
		];
	];
GenericGroup.childEditNotify[]:=
	Module[{allTrue}, 
		
		allTrue = And @@ (o.iterate[isEditable[]]);
		
		If[allTrue==False, 
			o["SelectedObject"].edit[False];
		];
		
		If[o.hasParent[], 
			o["Parent"].childEditNotify[];
		];
	];
GenericGroup.copy[]:=
	Module[{copiedComponents, newSpread}, 
		
		copiedComponents = o.iterate[copy[]];
		
		newSpread = o.super.copy[];
		newSpread."Components" = copiedComponents;
		
		o.setComponentIds[];
		newSpread.setParentToChildren[];
		
		newSpread
	];
GenericGroup.unGroup[]:= 
	If[o.hasParent[], 
		o["Parent"].thread[appendComponent[o["Components"]]];
		o.remove[];
		o.clearComponents[];
	];
GenericGroup.getSelectedComponents[keepSpreads_:True]:=
	If[o.isSelected[] && keepSpreads, 
		{o}
		, 
		Join @@ (o.iterate[getSelectedComponents[keepSpreads]])
	];
GenericGroup.getEditableComponents[keepSpreads_:True]:=
	If[o.isEditable[] && keepSpreads, 
		{o}
		, 
		Join @@ (o.iterate[getEditableComponents[keepSpreads]])
	];
GenericGroup.getComponents[keepSpreads_:True]:=
	If[keepSpreads, 
		{o}
		, 
		Join @@ (o.iterate[getComponents[keepSpreads]])
	];
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* 		GenericGroup/DB/Save *)
GenericGroup.createTree[allObjects_, setComponentIds_:True]:= 
	(
		Function[object, 
			If[SubClass[GenericGroup][object.type[]], 
				If[KeyExistsQ[allObjects, #], 
					object.appendComponent[allObjects[#], setComponentIds]
				]& /@ object["ComponentIds"];
			]
		]/@allObjects;
		
		SelectFirst[allObjects, (!#.hasParent[])&]
	);
GenericGroup.createTreeFromAssociations[associations_, setComponentIds_:True]:=
	Module[{objectId, allObjects}, 
		
		allObjects=Association[];
		
		Function[assoc, 
			objectId=assoc["Id"];
			allObjects[objectId]=o.createObjectFromAssociation[assoc];
		]/@associations;
		
		o.createTree[allObjects, setComponentIds]
	];
GenericGroup.getData[]:= o.iterate[getData[], "CarryTreeResult" -> True] // Prepend[KeyDrop[o.data[], "Components"]]; 
GenericGroup.load[fileName_]:= 
	(
		$SavedObjects = Import[fileName] // Uncompress;
		o.createTreeFromAssociations[$SavedObjects]
	);
GenericGroup.setLocalSave[value_]:= o.superIterate[setLocalSave[value]];
GenericGroup.setBackup[fieldsField_:"ObjectSavedFields"]:= o.superIterate[setBackup[fieldsField]];
GenericGroup.isTreeModified[fieldsField_:"ObjectSavedFields"]:= 
	o.super.isTreeModified[fieldsField] || Apply[Or, o.iterate[isTreeModified[fieldsField]]];
GenericGroup.getSavedDbTree[fieldsField_:"ObjectSavedFields"]:= o.superIterate[getSavedDbTree[fieldsField], "CarryTreeResult" -> True];
GenericGroup.getSavedTree[fieldsField_:"ObjectSavedFields"]:= o.superIterate[getSavedTree[fieldsField], "CarryTreeResult" -> True];
GenericGroup.flatten[fieldsField_:"ObjectSavedFields"]:= 
	Module[{savedDate=Date[], associations}, 
		associations = o.getSavedTree[fieldsField];
		ReplaceKey[#, "SavedDate" -> CouchDate[savedDate]]& /@ associations
	];
Options[saveTreeToDb] = 
	Join[
		Options@CouchConnect, 
		{
			"SavedDate" -> Automatic, "FieldsField" -> "ObjectSavedFields", "RecordSavedDate" -> True, 
			"CallbackOnSuccess" -> (None&), "ParallelKernel" -> "DBKernel"
		}
	];
GenericGroup.saveTreeToDb[collection_, opts:OptionsPattern[saveTreeToDb]]:= 
	Module[{fieldsField, associationsGathered, existingSavedDate, updateInfo, savedDate, 
		associations, recordSavedDate, connectionSettings, cluster, bucket, callbackOnSuccess, parallelKernel}, 
		
		{fieldsField, recordSavedDate, savedDate, cluster, bucket, callbackOnSuccess, parallelKernel} = 
			OptionValue[saveTreeToDb, {opts}, #]& /@ {"FieldsField", "RecordSavedDate", "SavedDate", "Cluster", "Bucket", "CallbackOnSuccess", "ParallelKernel"};
		
		(*in order to make sure that we compare the correct fields against the backup,  we don't want to compare
		local saved fields with saved fields*)
		o.setLocalSave[False];
		
		(*We want to create a new choice in the saved portfolios just if one component has been modified*)
		If[recordSavedDate && o.isTreeModified[fieldsField], 
			o."CreatedDate" = None;
		];
		
		(*If existingSavedDate is None,  there will be no update,  so it won't cause a bug*)
		existingSavedDate = o["SavedDate"];
		
		If[savedDate === Automatic, 
			savedDate = CouchDate[];
		];
		
		connectionSettings = Thread[{"Cluster", "Bucket"} -> {cluster, bucket}];
		
		(*existingSavedDate will not be used if recordSavedDate is False *)
		updateInfo = AssociationThread[{"ExistingSavedDate", "SavedDate"} -> {existingSavedDate, savedDate}];
		
		associations = o.getSavedDbTree[fieldsField] // Flatten;
		
		associationsGathered = 
			SelectEquivalents[
				associations, 
				"TagElement" -> (#["Action"]&), 
				"TransformElement" -> (#["Data"]&), 
				"TransformResults" -> List
			];
			
		(*there's a maximum of two commands per save for each tree*)		
		WithValues[{updateInfo, associationsGathered, fieldsField, recordSavedDate, savedDate, connectionSettings, callbackOnSuccess}, 
			o.asyncEvaluate[
				And @@ (insertOrUpdate[connectionSettings, updateInfo, #]& /@ associationsGathered)
				, 
				"Callback" -> 
					Function[insertOk, 
						If[insertOk, 
							If[recordSavedDate, 
								o.setField["SavedDate", savedDate];
								o.setBackup[fieldsField];
							];
							
							callbackOnSuccess[];
							, 
							Message[couchInsert::insertErr, o["Id"]];
							associationsGatheredLog = associationsGathered;
							o.logIt["SaveTreeToDb failed", "Type" -> "Debug", "Context":>associationsGatheredLog];
						]
					]
				, 
				"ParallelKernel" -> parallelKernel
			];
		];
		
		savedDate
	];
(*insertOrUpdate is called on parallel kernels*)
insertOrUpdate[connectionSettings_, updateInfo_, {"Insert", docs_}]:=
	Module[{insertOk, docsWithSavedDate, insertResult}, 
		
		docsWithSavedDate =
			ReplaceKey[
				#, 
				"CreatedDate" -> updateInfo["SavedDate"], 
				"SavedDate" -> updateInfo["SavedDate"]
			]& /@ docs;
			
		insertResult = CouchInsert[docsWithSavedDate, connectionSettings, "Id" -> "ObjectType"];
		insertOk = AllTrue[insertResult, !#["HasError"]&];
		
		insertOk
	];
insertOrUpdate[connectionSettings_, updateInfo_, {"UpdateSavedDate", ids_}]:=
	Module[{insertOk, result, statement}, 
			
		statement = 
			GetQuery[
				"UPDATE `Bucket` 
				SET SavedDate = `SavedDate`
				WHERE Id IN `Ids` AND SavedDate = `ExistingSavedDate`"  
				, 
				{
					"Bucket"  ->  ("Bucket" /. connectionSettings), 
					"SavedDate" ->  Quote@updateInfo["SavedDate"], 
					"ExistingSavedDate" ->  Quote@updateInfo["ExistingSavedDate"], 
					"Ids" -> AssociationToJson@ids
				}
			];
		
		result = CouchQuery[statement, connectionSettings];
		insertOk = !result["HasError"];
		
		insertOk
	];
(* ::Subsubsection:: *)
InitializeClass[GenericGroup];
(* ::Subsection:: *)
(* ::Subsection:: *)
(* Data structures *)
(* ::Subsubsection:: *)
(* 		Data structures/ValueObject *)
(*ValueObject can be seen as a pointer to a single value,  useful for Dynamic,  we can check if the 
value is empty or display it,  useful with TrackObject*)
ValueObject = NewClass["Fields" -> {"Value"}];
ValueObject.reset[]:= o."Value" = None;
ValueObject.isEmpty[]:= o["Value"] === None;
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* 		Data structures/ValuesSelector *)
ValuesSelector = NewClass["Fields"  -> {"Values" -> Association[], "PossibleValues" -> {} }];
ValuesSelector.getListPickerValues[]:=
	Module[{keys, values}, 
		
		keys = o.getKeys["Values"];
		
		If[keys === {}, 
			{None -> "No values"}	
			, 
			values = o.getValues["Values"];
			Thread[keys -> MapThread[#1~~" : "~~ToString@#2&, {keys, values}]]
		]
	];
ValuesSelector.show[callback_]:=
	DynamicModule[
		{currentType, currentValue, selectedValue, listPickerTick=False, keyAfterDelete, updateListPicker}
		, 
		updateListPicker = TickNotify@listPickerTick &;
		
		If[ !o.isEmpty["Values"], 
			currentType = First[o.getKeys["Values"]];
			currentValue = o["Values"][currentType];			
			, 
			currentType = First@o["PossibleValues"];
			currentValue = 0.;
		];
		selectedValue = {currentType};
		
		Column[
			{
				PopupMenu[
					Dynamic[
						currentType
						, 
						(
							currentType=#;
							currentValue=0.;
						)&
					]
					, 
					o["PossibleValues"]
					, 
					ImageSize -> Full
				]
				, 
				InputField[
					Dynamic[currentValue]
					, 
					Number
					, 
					ImageSize -> Full
				]
				, 
				Button[
					"Add/Edit"
					, 
					If[NumberQ@currentValue, 
						o.setKey["Values", currentType, currentValue];
						selectedValue = {currentType};
						callback[o["Values"]];
						updateListPicker[];
					]
					, 
					ImageSize -> Full
				]
				, 
				TrackTick[{listPickerTick}, 
					ListPicker[
						Dynamic[
							selectedValue
							, 
							(
								If[# =!= {None}, 
									selectedValue = #;
									currentType = First@selectedValue;
									currentValue = o["Values"][currentType];
								]
							)&
						]
						, 
						o.getListPickerValues[]
						, 
						Multiselection -> False
						, 
						ImageSize -> Full
					]
				]
				, 
				Button[
					"Delete"
					, 
					If[ !o.isEmpty["Values"], 
						keyAfterDelete = GetSelectedKeyAfterDelete[o.getKeys["Values"], First@selectedValue];
						o.deleteKey["Values", First@selectedValue];
					];
					
					If[ !o.isEmpty["Values"], 
						currentType = keyAfterDelete;
						currentValue = o["Values"][currentType];
						selectedValue = {currentType};
						, 
						currentValue = 0.;
					];
					
					callback[o["Values"]];
					
					updateListPicker[];
					, 
					ImageSize -> Full
				]
			}	
			, 
			Alignment  ->  Left
			, 
			ItemSize -> 8								
		]		
	];	
(* ::Subsubsection:: *)
(* ::Subsection:: *)
End[] (* End Private Context *)

EndPackage[]