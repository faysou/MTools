(* ::Package:: *)

BeginPackage["MTools`Core`MPlusPlus`",{"JLink`"}]
(* ::Subsubsection:: *)
(* Usage *)
NewClass::usage = 
"MyClass = NewClass[opts] creates a new class. You can specify the fields of the class using the option \"Fields\" 
and the parent classesusing the option \"Fields\".";
New::usage = "New[class][fieldOptions] creates an instance of class with optional property values.";

{sub,super,SubClass}; (*see below for the usage definition*)
this::usage = "Class[object].this.f[args] forces Class to execute its implementation of f.";

Supers::usage = "Supers[Class] contains the list of super classes by ascending order of priority.";
ResetClasses::usage = "ResetClasses[] allows to reset all classes already defined. This is useful for adding a function to a class and instance already defined can access it.";
ClassFields::usage = "ClassFields[class] contains all default field names of class.";
InitializeClass::usage = "InitializeClass[class] manually initializes class. This is useful for static functions.";
Initialized::usage = "Initialized[class] == True when class has already been initialized.";
FormatClass::usage = "FormatClass[class] defines the default appearance of an object as InterpretSymbol[object].";

o::usage = "o is the default symbol to refer to an object in a class function. You can change this by changing the value of $ObjectSymbol.";
$ObjectSymbol::usage = "$ObjectSymbol contains the symbol used to represent the current object in class functions, by default $ObjectSymbol == objectHold[o].";
objectHold::usage = "objectHold contains the symbol used by $ObjectSymbol, by default $ObjectSymbol == objectHold[o].";

FunctionQ::usage = "FunctionQ[class,functionName] tests if the function functionName is implemented in class.";
ObjectSet::usage = "ObjectSet[object,key,value] is a way to set a key/value to an object or a symbol.";
CopySymbol::usage = "CopySymbol[object] return a copy of an object.";
ClearObject::usage = "ClearObject[object] clears the object.";

BaseClass::usage = "BaseClass is the class inherited by any class by default that contains basic functions like setters and getters for common cases.";
(*functions with HoldX attribute should be exported so that the exact symbol is used instead of another symbol*)
{setKeyDelayed,cacheFunction,staticCacheFunction,getItem};

GetFunctions::usage = "GetFunctions[class] returns an association of all the functions of class associated to each of its super classes.";
GetAllFunctions::usage = "GetAllFunctions[class] returns all functions of class.";
GetArguments::usage = "GetArguments[class] returns an association of all the functions and its arguments of class associated to each of its super classes.";
GetAllArguments::usage = "GetAllArguments[class] returns all functions and arguments of class.";

StoreUpValue::usage = 
"StoreUpValue[symbol,parameterSymbol,value,type] stores value in the UpValue parameterSymbol[symbol]. 
type can either be \"Class\" or \"Object\".";
AccessUpValue::usage = 
"AccessUpValue[symbol,parameterSymbol,type] accesses the value stored in the UpValue parameterSymbol[symbol]. 
type can either be \"Class\" or \"Object\".";
Interface::usage = "Interface[class] can contain information on how to display certain fields of class.";
GetInterface;
SetInterface::usage = 
"SetInterface[class|object,interface,type] stores an interface for class or object in the UpValue Interface[class].
type can either be \"Class\" or \"Object\".";
GetInterface::usage = 
"GetInterface[class|object,type] accesses the interface stored in class or object in the UpValue Interface[class].
type can either be \"Class\" or \"Object\".";
ClearInterface::usage = 
"ClearInterface[class|object,type] resets the interface stored in class or object in the UpValue Interface[class].
type can either be \"Class\" or \"Object\".";
ModifyInterface::usage = 
"ModifyInterface[class|object,newInterface,type] adds a new interface specification for a given field, and deletes the previous interface for this field.
type can either be \"Class\" or \"Object\".";
RemoveInterface::usage = "RemoveInterface[class|object,field,type] removes the interface of field.
type can either be \"Class\" or \"Object\".";
InterfaceOrdering::usage = "InterfaceOrdering[class|object] can contain information on which fields should be displayed and in which order.";
GetInterfaceOrdering::usage = "GetInterfaceOrdering[class|object,type] accesses the interface ordering stored in class or object in the UpValue InterfaceOrdering[class].
type can either be \"Class\" or \"Object\".";
SetInterfaceOrdering::usage = "SetInterfaceOrdering[class|object,interfaceOrdering,type] sets the interface ordering stored in class or object in the UpValue InterfaceOrdering[class].
type can either be \"Class\" or \"Object\".";

EditSymbolPane::usage = "EditSymbolPane[object,options] displays a dynamic interface as a pane to edit fields of object.";
EditSymbol::usage = "EditSymbol[object,options] displays a dynamic interface as a popup to edit fields of object.";
EditFunction::usage = "EditFunction[function[params],options] allows to edit the optional arguments of function in a user interface before executing functions.";
EditNewObject::usage = "EditNewObject[class][params] allows to edit the optional arguments of class in a user interface before creating an object.";
GetInput::usage = "GetInput[prompt,valueNames,defaults,options] creates a prompt in order to input the parameter valueNames which default values.
The Interface and InterfaceOrdering of the valueNames can be given as options.";
InterpretSymbol::usage = "InterpretSymbol[object,options] displays object as interpretable user interface, ie. the user interface can be passed as arguments to other functions.";
InterpretSymbolName::usage = "InterpretSymbolName[object] stores the name of an object when using InterpretSymbol.";
UninterpretSymbol::usage = "UninterpretSymbol[object] converts back an interpretable object to its initial variable symbol.";
DynamicObject::usage = "DynamicObject allows to refer to an object when definining the appearance of fields.";
GetTrackedSymbols::usage = "GetTrackedSymbols[objectList] returns a rule delayed TrackedSymbols :> objectSymbols, with objectSymbols being the symbols
underlying objectList.";
SetField::usage = "SetField is expanded as SetField[object,key,value] when an automatic interface generation happens.";

SetHelp::usage = 
"SetHelp[class] stores a usage message about class.
SetHelp[class.f] store a usage message about the class function f.";
Help::usage = 
"Help[class]=message stores a usage message about class.
Help[class.f]=message store a usage message about the class function f.
Help[class] retrieves usage messages about all the functions of class.
Help[class.f] retrieves a usage message about the class function f.";
ClearHelp::usage = 
"ClearHelp[class] deletes all the usage messages of class f.
ClearHelp[class.f] deletes the usage message about the class function f.";

NKeys::usage = "NKeys[symbol] get the multidimensional keys of symbol. Ex: symbol[2,3]=2; NKeys[symbol]={{2,3}}";
keys::usage = "keys[symbol] gets the keys of a symbol, a key of a symbol a is used like a[key]=1.*)";
values::usage = "values[dict] returns the list of values of dict (gathered from DownValues).";
Items::usage = "Items[dict] returns the list of {index, value} pairs of dict (gathered from DownValues).";
PrintSymbol::usage = "PrintSymbol[symbol,fieldsExcluded] displays the symbol names and values in a table form.";
UpKeys::usage = "UpKeys[symbol] returns the symbols acting on symbol as UpValues.";
KeyQ::usage = "KeyQ[object,key] returns True if key is a member of object.";
UpKeyQ::usage = "UpKeyQ[symbol,key] == True if key[symbol] is an UpValue of symbol.";
UpdateRules::usage=
"UpdateRules[symbol, newRules, updateSymbolRulesOnly] updates the text downvalues in symbol with rules of newRules.
Ex: If mp[Quote a Quote] == 1 then after UpdateRules[mp,{a->2,b->1}] we have mp[Quote a Quote] == 2";
(* ::Subsubsection:: *)
Begin["`Private`"]
(* ::Subsubsection:: *)
(* Class *)
Options[NewClass]={"Fields"->{},"Parents"->{},"InterfaceOrdering"->{}}
NewClass /: Set[classSymbol_Symbol,NewClass[opts:OptionsPattern[]]] :=
	Module[{classProperties,parentClasses,interfaceOrdering},
		
		{classProperties,parentClasses,interfaceOrdering} = 
			OptionValue[NewClass,{opts},#]& /@ {"Fields","Parents","InterfaceOrdering"};
		
		setupNewClass[classSymbol,classProperties];
		
		inherit[classSymbol,parentClasses];
			
		InterfaceOrdering[classSymbol] ^= 
			If[interfaceOrdering =!= {},
				interfaceOrdering
				,
				{}
			];
	
		Initialized[classSymbol] = False;
		
		classSymbol
	];

SetAttributes[isOptionDuplicate,Orderless];
isOptionDuplicate[x_->_,x_->_]:=True;
isOptionDuplicate[{x_->_,__},{x_->_,__}]:=True;
isOptionDuplicate[{x_->_,__},x_->_]:=True;
isOptionDuplicate[__]:=False;

setupNewClass[classSymbol_,classOptions_]:=
    Module[{classOptionsOnly,interface,usedClassOptions,nonClassFields},
    	
    	(*we find nonClassFields, ie options that are not stored when an object is created,
    	they are option names surrounded by a list*)
    	nonClassFields =
    		Cases[
    			classOptions
    			,
    			{Verbatim[Rule][{x_},_],__} | {{x_},__} | Verbatim[Rule][{x_},_] | {x_} :> x
    			,
    			{1}
    		] // Map@MTools`Utils`Utils`GetSymbolName //
    		DeleteDuplicates; 
    		
		(*we remove the nonClassFields specification from classOptions*)
    	usedClassOptions =
			Replace[
				classOptions
				,
				{ 
					{Verbatim[Rule][{x_},y_],z__} :> {x->y,z},
					{{x_},y__} :> {x,y},
					Verbatim[Rule][{x_},y_] :> {x->y},
					{x_} :> x
				}
				,
				{1}
			];
    	
    	(*we assign a None default argument when no default option value is given*)
    	usedClassOptions = 
    		Replace[
    			usedClassOptions,
    			{optionName:Except[_Rule|_List] :> (optionName->None), {optionName:Except[_Rule],rest__} :> {optionName->None,rest}},
    			{1}
    		]//
    		DeleteDuplicates[#,isOptionDuplicate]&;
       
       	(*setting the class interface for displaying some fields in a custom way with EditSymbolPane and related functions*)
        interface=Cases[usedClassOptions,{paramName_->_,interfaceSpec__}:>{MTools`Utils`Utils`GetSymbolName@paramName,interfaceSpec}];
        SetInterface[classSymbol,interface];
        
        (*assigning Options and ClassFields*)
    	classOptionsOnly=Replace[usedClassOptions,x_List:>x[[1]],{1}];
    	
    	If[classOptionsOnly =!= {},
    		classOptionsOnly[[All,1]]=MTools`Utils`Utils`GetSymbolName /@ classOptionsOnly[[All,1]];
    		
	       	(*ClassFields are the arguments stored in an object at creation time, use
	    	BaseClass.getOption for the non stored arguments*)
    		ClassFields[classSymbol] ^= Complement[classOptionsOnly[[All,1]],nonClassFields];
    		,
    		ClassFields[classSymbol] ^= {};
    	];
    	
        Options[classSymbol] = classOptionsOnly;
    ];

(*similar to https://www.python.org/download/releases/2.3/mro
https://rhettinger.wordpress.com/2011/05/26/super-considered-super*)
getAllSupers[classList_]:=( {Supers[#],#}& /@ classList // Flatten ) /. _Supers :> Sequence[] // DeleteDuplicates;
getAllSupersIterated[classList_]:=FixedPoint[getAllSupers,classList]; 

inherit[newClass_,classList_List,newOptions_:{}] := 
	(		
    	Supers[newClass] ^= getAllSupersIterated[Prepend[classList,BaseClass]];
    	
	 	Options[newClass]=MTools`Utils`Utils`JoinOptions[newOptions,Options /@ Reverse@Append[classList,newClass]];
	 	
	 	ClassFields[newClass] ^= 
	 		(ClassFields /@ Append[classList,newClass] // Flatten) /. 
	 		_ClassFields :> Sequence[] // 
	 		DeleteDuplicates;
	 		
		inheritInterface[newClass,classList];
	);
    
(*
	6 ways to define a function, all syntaxes except the first one can accept a condition on the lhs,
	all syntaxes can accept a condition on the rhs
	BaseClass.clear[] := ClearObject[o]; (*best one*)
	BaseClass /: o_BaseClass.clear[] /; condition := ClearObject[o];
	o_BaseClass.clear[] ^:= ClearObject[o]; (*stores the definition in BaseClass and clear*)
	BaseClass /: BaseClass[o_,___].clear[] := o[2];
	BaseClass /: (o:BaseClass[object_,___]).clear[] := Print@o;
	BaseClass[o_,___].clear[] ^:= o[2]; (*stores the definition in BaseClass and clear*)
*)
SetAttributes[objectHold,HoldAllComplete];
$ObjectSymbol=objectHold@o;

Unprotect[Dot];
(*converting class.f[args]:=body to class /: o_class.f[args] := body*)
Dot /: SetDelayed[Dot[classSymbol_Symbol,f_Symbol[args___]],body_] := defineObjectFunction[classSymbol,f,{args},body];
Protect[Dot];

(*function to automatically convert BaseClass.clear[] := ClearObject[o] to BaseClass /: o_BaseClass.clear[] := ClearObject[o]*)
SetAttributes[defineObjectFunction,HoldAllComplete];
defineObjectFunction[class_,f_,{args___},body_]:=
	Block[{tagSetDelayed},
		
		SetAttributes[tagSetDelayed,HoldAllComplete];

		With[{objectSymbol = $ObjectSymbol},
			(*we need to rename TagSetDelayed in order to insert $ObjectSymbol*)
			tagSetDelayed[ 
				class
				,
				Dot[
					Pattern[objectSymbol,Blank[class]]
					,
					f[args]
				]
				,
				body
			] 
		] // 
		Replace[#,objectHold@e_ :> e,Infinity,Heads->True]& //
		ReplaceAll[#,tagSetDelayed->TagSetDelayed]&
	];
    
(*Last symbol in symbols has the top priority*)
Options[New] = {"SharedObject"->False};
New[class_Symbol,OptionsPattern[]][opts___] :=
    Module[{options,Global`object,newObject},
    	
    	If[!TrueQ@Initialized@class,
    		InitializeClass[class];
    	];
    	
		If[OptionValue@"SharedObject",
			SetSharedFunction@Global`object;
		];
		
		Global`object = Association[];
		
		newObject = class[Global`object];
		
		options = FilterRules[Flatten@{opts},Options@class];
		
		newObject.initData[options];	
		
    	If[FunctionQ[#,"init"],
    		newObject.super[#].init[options];
    	]& /@ Append[Supers[class],class];
		
		(*class[object], class contains the methods, object the data*)
        newObject
    ];
    
(*
InitJavaKernels["NKernels"->1,"ReservedKernels"->{"a"}]
KernelEvaluate[Get@"MPlusPlus`"]
x=New[BaseClass,"SharedObject"->True][]
With[{x=x},
	KernelSubmit["a",x.set["b",12]]
]
DrainKernelResults["a"] (*for the commnunication with the main kernel to happen, similar to QueueRun[]*)
x["b"]
a scheduled task on the main kernel can monitor new writes in the shared object
*)
	
(*copy constructor, to use for ParallelEvaluate[New[$Settings][]]] ?*)
New[{class_Symbol,data_Association},newOpts:OptionsPattern[]][opts___] :=
	Block[{options},	
		options = Join[{opts},Normal@data];
		New[class,newOpts][options]
	];
	
(*copy constructor*)
New[class_Symbol[data_],newOpts:OptionsPattern[]][opts___] := New[{class,data},newOpts][opts];

(*import from saved data*)
New[data_Association,newOpts:OptionsPattern[]][opts___] := New[{ToExpression@data["ObjectType"],data},newOpts][opts];
	
New[][]:= New[BaseClass][];

patternSignature[rule_]:=First@rule //. {Verbatim[Pattern][_,x_]:>x,Verbatim[Optional][x_,_]:>x};

(*so that a class inherits functions like Keys[o_BaseClass]^:= o.getFields[] from its parent super classes*)
inheritNonObjectFunctions[class_]:=
	Block[{allNonObjectFunctions},
		
		allNonObjectFunctions = 
			nonObjectRules[#] //. #->class & /@ Prepend[Reverse@Supers@class,class] // Flatten // 
			DeleteDuplicates[#,(patternSignature@#1 === patternSignature@#2)&]&;
			
		UpValues@class = 
			Join[
				allNonObjectFunctions
				,
				DeleteCases[
					UpValues@class
					,
					Verbatim /@ nonObjectRules[class] // Apply@Alternatives
				]
			];
			
		SubValues@class = 
			SubValues@# //. #->class & /@ Prepend[Reverse@Supers@class,class] // Flatten // 
			DeleteDuplicates[#,(patternSignature@#1 === patternSignature@#2)&]&;
	];
	
appendToUpValues[symbol_,newRule_]:=
	Module[{currentRules},
		currentRules = UpValues@symbol;
		currentRules = DeleteCases[currentRules,Verbatim@newRule];
		UpValues[symbol]=Flatten@{currentRules,newRule};
	];
prependToUpValues[symbol_,newRule_]:=
	Module[{currentRules},
		currentRules = UpValues@symbol;
		currentRules = DeleteCases[currentRules,Verbatim@newRule];
		UpValues[symbol]=Flatten@{newRule,currentRules};
	];
	
addSpecialRules[class_]:=
	(		
		prependToUpValues[class,
			HoldPattern[class[params___].this.f_[args___]] :> 
				executeThisFunction[class,f,class[params].f[args]]
		];
		
		(*We want a high priority for this rule, a function by default calls sub*)
		prependToUpValues[class,
			(*Dot has flat attribute so any sequence can be matched for example x.super[c].g[3] will match to (x.super[c]).g[3] 
			and x.super[c] goes to sub (we don't want this)*)
			HoldPattern[class[params___].(f:Except[super,_])[args___] /; !TrueQ[$blockSub[class,f]] ] :> 
				class[params].sub.f[args]
		];
		
		(*this allows to overload a subclass just for certain arguments, if arguments don't match the pattern matcher
		for the defintion in this class, a super class will be called*)
		appendToUpValues[class,
			HoldPattern[class[params___].(f:Except[super,_])[args___] (* /; TrueQ[$blockSub[class,f]]*)] :>
				class[params].super.f[args]
		];
		
		(*http://mathematica.stackexchange.com/a/73017/66*)
		(*so that when an object is returned as o for example, its main representation is returned*)
		appendToUpValues[class,
			HoldPattern[(h:Except[Dot,_])[a___,class[object_,___,specialRuleMainClass_],b___]] :> 
				h[a,specialRuleMainClass[object],b]
		];
	);

If[!ValueQ@$StaticAssociation,
	$StaticAssociation = Association[];
];

(*
InitializeClass is called the first time a new instance of a class is created
A class needs to be initialized manually to call static functions on it without instantiating an object
New functions can be added to the class, and will be recognised after calling ResetClasses[]
nonObject functions can't be easily reset after initialization
*)
InitializeClass[class_]:=
	(
		SetAttributes[class,HoldFirst];
		
		(*we execute and cache this expensive function at definition time*)
		stringObjectFunctions[class];
		
		If[!ValueQ@Supers[class],
			Supers[class] ^= {BaseClass};
		];
		
		inheritNonObjectFunctions[class];
		
		addSpecialRules[class];
		
		If[!TrueQ@Initialized@#,
			InitializeClass@#
		]& /@ Supers[class];
		
		If[!KeyExistsQ[$StaticAssociation,class],
			$StaticAssociation[class] = Association[];
		];
		
		Initialized@class ^= True;
	);

resetSubSuper[]:=
	(
		ClearAll@sub;
		sub::usage = 
		"Class[object,classStack___,CallingClass].sub.f[args] or CallingClass[object].sub.f[args] executes function f 
		using the implementation of CallingClass or the nearest super class of CallingClass.";
		sub::noFct = "Class `1` doesn't have a sub class with member function `2`.";
		sub /: class_Symbol[params___].sub.function_Symbol[args___] :=
		    Block[{subs,mainClass, classPosition},
		    	
		    	If[Length@{params} <= 1,
		    		subs = Flatten@{Supers[class],class};
		    		,
			    	mainClass = Last@{params};
		    		subs = Flatten@{Supers[mainClass],mainClass};
		    		
		    		(*useful in the .super[superClass] where superClass doesn't belong to inheritance tree*)
					classPosition = cachedPosition[subs,class];
		    		
		    		If[classPosition === {},
		    			(*simplified branching mechanism, we make mainClass inherit from class,
		    			priority to sub in mainClass, but inheritance tree of class can also be used*)
		    			subs = Flatten@{Supers[class],class,subs} // DeleteDuplicates;
		    		];
		    	];
		    	
		    	redefineFunction[sub,subs,{class,{params},function,{args}}]
			];
		(*tricky and rare sub example, in order to avoid infinite recursion,
		when overloaded and parent functions call each other, as, without sub in x,
		sub call cannot happen in the execution of x.f as $blockSub[x,f] is already 
		set to True when y[obj,x].super.f[x] happens followed by x[obj,x].this.f[x]
		
		x=NewClass[]
		y=NewClass["Parents"->{x}]
		x.f[x_]:=o.sub.f[2]
		y.f[x_/;EvenQ@x]:=x^2
		
		zz=New[y][]
		zz.f[3]*)

		ClearAll@super;
		super::usage = 
		"Class[object].super.f[args] executes function f using the implementation of the nearest super class of Class.
		Class[object].super[OtherClass].f[args] executes function f using the implementation of OtherClass or the nearest super class of OtherClass.";
		super::noFct = "Class `1` doesn't have a super class with member function `2`.";
		super::noClsFct = "Class `1` doesn't have as member function `2`.";
		super /: class_Symbol[params___].super.function_Symbol[args___] :=
		    Block[{supers,classPosition,mainClass},
		    	
				If[Length@{params} <= 1,
		    		supers = Supers[class];
		    		,
		    		mainClass = Last@{params};
		    		supers = Supers[mainClass];
		    		
		    		If[class =!= mainClass,
		    			
		    			classPosition = cachedPosition[supers,class];
		    			
		    			If[classPosition =!= {},
		    				(*supers from the point of view of class in the inheritance tree of Last@classStack*)
		    				(*BaseClass never calls super so classPosition[[1,1]] - 1 is OK*)
		    				supers = supers[[ ;; classPosition[[1,1]] - 1 ]];
		    				,
		    				(*useful in the .super[superClass] where superClass doesn't belong to the inheritance tree*)
		    				supers = Supers[class]; 
		    			];
		    		];
		    	];
		    	
		    	redefineFunction[super,supers,{class,{params},function,{args}}]
			];
			
		super /: class_Symbol[params___].super[superClass_].function_Symbol[args___] :=
		    Block[{supers,classPosition,mainClass,executedClass,resultFunction},
		    	
				If[Length@{params} <= 1,
		    		mainClass = class;
		    		,
		    		mainClass = Last@{params};
				];
				
				(*this allows to use mainClass if called from higher in the inheritance tree*)
	    		supers = Flatten@{Supers[mainClass],mainClass};
	    		
				classPosition = cachedPosition[supers,superClass];
	    			
    			If[classPosition =!= {},
    				(*supers from the point of view of superClass in the inheritance tree of Last@classStack,
    				including superClass*)
    				supers = supers[[ ;;classPosition[[1,1]] ]];
    				,
    				(*useful in the .super[superClass] where superClass doesn't belong to the inheritance tree*)
    				supers = Flatten@{Supers[superClass],superClass}; 
    			];
    			
    			redefineIndexedFunction[super,superClass,supers,{class,{params},function,{args}}]
			];
			
		ClearAll@SubClass;
		SubClass::usage = "SubClass[superClass][class] checks if class is a sub class of superClass.";
		g:SubClass[base_][sub_[___]|sub_]:= g = MemberQ[Append[Supers[sub],sub],base];
		(*f[x_?(SubClass[BaseClass])]:=x*)
	);
resetSubSuper[];

ResetClasses[class_:All]:=
	(
		(*many things are cached we need to reset them*)
		MTools`Utils`Utils`DeleteCachedValues[
			{
				stringObjectFunctions,getFunctionSymbol,nonObjectRules,
				cachedPosition,findFunction,objectFunctions,FunctionQ
			}
		];
		
		resetSubSuper[];
		
		(*In order to put again the special rules at the top and bottom of UpValues@class*)
		If[class === All,
			InitializeClass /@ Keys@$StaticAssociation;
			,
			InitializeClass@class;
		];
	);

(*
Example
x=NewClass[]
y=NewClass["Parents"->{x}]
z=NewClass["Parents"->{y}]
x.f[]:=2
y.f[]:=3
z.f[]:=4
zz=New[z][]
zz.f[]
zz.super.f[]
zz.super[x].f[]
*)
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* Class aux functions*)	
SetAttributes[executeThisFunction,HoldAll];
executeThisFunction[executedClass_,funct_,code_]:=
	Block[{$blockSub},
		$blockSub[executedClass,funct]=True; 
		code
	];

SetAttributes[redefineFunction,HoldAll];
redefineFunction[auxSymbol_,classList_,{class_,{params___},function_,{args___}}]:=
	Block[{executedClass,resultFunction,objecttack},
		
    	{executedClass,resultFunction}=findFunction[classList,function];
    		
		If[executedClass===None,
			Message[auxSymbol::noFct,class,SymbolName@function];
			HoldComplete[class[params].auxSymbol.function[args]]
			,
			Switch[Length@{params},
				0,
					(*we add a dummy object for a static function that doesn't need an object*)
					With[{executedClass = executedClass,funct = resultFunction}, 
				  		class /: class[].auxSymbol.function[arguments___] := 
				  			executeThisFunction[executedClass,funct,executedClass[_,class].funct[arguments]];
				  	];
				,
				1,
					With[{executedClass = executedClass,funct = resultFunction}, 
				  		class /: class[o_].auxSymbol.function[arguments___] := 
				  			executeThisFunction[executedClass,funct,executedClass[o,class].funct[arguments]];
				  	];
			  	,
				_,
					With[{executedClass = executedClass,funct = resultFunction,mainClass = Last@{params}}, 
			  			class /: class[o_,classStack___,mainClass].auxSymbol.function[arguments___] :=
			  				executeThisFunction[executedClass,funct,executedClass[o,class,classStack,mainClass].funct[arguments]];
			  		];	
			];   		
	  			
	  		class[params].auxSymbol.function[args]
		]
	];
	
SetAttributes[redefineIndexedFunction,HoldAll];
redefineIndexedFunction[auxSymbol_,indexClass_,classList_,{class_,{params___},function_,{args___}}]:=
	Block[{executedClass,resultFunction,objecttack},
		
 		{executedClass,resultFunction} = findFunction[classList,function];
		    		
		If[executedClass===None,
			Message[auxSymbol::noFct,class,SymbolName@function];
			HoldComplete[class[params].auxSymbol[indexClass].function[args]]
			,
			Switch[Length@{params},
				0,
					(*we add a dummy object for a static function that doesn't need an object*)
					With[{executedClass = executedClass,funct = resultFunction}, 
						class /: class[].auxSymbol[indexClass].function[arguments___] := 
							executeThisFunction[executedClass,funct,executedClass[_,class].funct[arguments]];
					];
				,
				1,
					With[{executedClass = executedClass,funct = resultFunction}, 
						class /: class[o_].auxSymbol[indexClass].function[arguments___] := 
							executeThisFunction[executedClass,funct,executedClass[o,class].funct[arguments]];
					];
				,
				_,
					With[{executedClass = executedClass,funct = resultFunction,mainClass = Last@{params}}, 
						class /: class[o_,classStack___,mainClass].auxSymbol[indexClass].function[arguments___] := 
							executeThisFunction[executedClass,funct,executedClass[o,class,classStack,mainClass].funct[arguments]];
					];	
			];  		
			  			
			class[params].auxSymbol[indexClass].function[args]
		]
	];
	
ClearAll@findFunction;
g:findFunction[classList_,function_]:= g =
	Block[{functionName, executedClass, resultFunction},
		
		functionName=SymbolName@function;
		
		executedClass=None;
		Scan[
			(	    		
	    		resultFunction=getFunctionSymbol[#,functionName];
	    			
	    		If[resultFunction=!=None,
	    			executedClass = #;
	    			Return[];
	    		];
			)&
			,
			Reverse@classList
		];
		
		{executedClass,resultFunction}
	];
	
ClearAll@cachedPosition;
g:cachedPosition[list_,element_]:= g = Position[list,element];

(*we find the symbol corresponding to a functionName, this is the symbol from the UpValues defintion of class*)
ClearAll@getFunctionSymbol;
g:getFunctionSymbol[class_,function_Symbol]:= g = getFunctionSymbol[class,SymbolName@function];
g:getFunctionSymbol[class_,functionName_String]:= g = 
	With[{functionPosition=cachedPosition[stringObjectFunctions[class],functionName]},
		
	    If[functionPosition =!= {},
	    	objectFunctions[class][[ functionPosition[[1,1]] ]]
	    	,
	    	None
	    ]
	];
	
ClearAll@objectFunctions;
g:objectFunctions[class_Symbol]:= g =
	Cases[
		UpValues[class][[All,1]]
		,
		(
			(*Except in order to not match the getItem rule*)
			Verbatim[HoldPattern][Verbatim[Dot][_,(function:Except[Pattern,_Symbol])[___]]] |
			Verbatim[HoldPattern][Verbatim[Condition][Verbatim[Dot][_,(function:Except[Pattern,_Symbol])[___]],_]]
		) :> function
	]//DeleteDuplicates;
	
ClearAll@nonObjectRules;	
g:nonObjectRules[class_Symbol]:= g =
	Cases[
		UpValues[class]
		,
		Except[
			(*o_class.f[], Except to match getItem rule*)
			Verbatim[RuleDelayed][Verbatim[HoldPattern][Verbatim[Dot][_,Except[Pattern][___]]],_] | 
			(*BaseClass[object,___] /; condition*)
			Verbatim[RuleDelayed][Verbatim[HoldPattern][Verbatim[Condition][Verbatim[Dot][_,_[___]],_]],_] | 
			(*we exclude sepcial key words while still including the getItem rule, we also exclude all constants*)
			(x_/; !FreeQ[x,sub|this|super|specialRuleMainClass] && FreeQ[x,(*getItem*)Except[sub|super,_Symbol|_String]] || 
			FreeQ[x,Blank|BlankSequence|BlankNullSequence|OptionsPattern])
		]
	];

ClearAll@stringObjectFunctions;
g:stringObjectFunctions[class_]:= g = SymbolName /@ objectFunctions[class];

GetFunctions[class_Symbol|class_Symbol[_]]:= 
	Module[{classes},
		classes = Append[Supers[class],class];
		objectFunctions /@ classes // Map[SymbolName,#,{2}]& // Map[DeleteDuplicates] // Map[Sort] // AssociationThread[classes->#]&
	];
	
GetAllFunctions[class_]:=GetFunctions[class]//Values//Flatten//DeleteDuplicates;
	
objectFunctionsString[class_]:=
	Cases[
		UpValues[class][[All,1]]
		,
		(
			Verbatim[HoldPattern][Verbatim[Dot][_,(function:Except[Pattern,_Symbol])[args___]]] |
			Verbatim[HoldPattern][Verbatim[Condition][Verbatim[Dot][_,(function:Except[Pattern,_Symbol])[args___]],_]]
		):>
			(
				ToString[
					MTools`Utils`Utils`GetSymbolName[function][
						{args} //. 
							{
								Verbatim[Condition][x_,_] :> x
								,
								Verbatim[Pattern][x_,_] :> MTools`Utils`Utils`GetSymbolName@x 
								,
								Verbatim[Optional][x_,default_] :> x~~":"~~MTools`Utils`Utils`GetSymbolName@default
							} // 
						Apply[Sequence]
					]	
				] // Interpretation[Tooltip[#,Help[class.function]],#]&
			)
	];
	
GetArguments[class_Symbol|class_Symbol[_]]:= 
	Module[{classes,classTooltips},
		classes = Append[Supers[class] /. _Supers -> {},class];
		classTooltips = (Interpretation[Tooltip[#,Help[#,False]],#]& /@ classes);
		objectFunctionsString /@ classes // Map[Sort] // AssociationThread[classTooltips->#]&
	];
	
GetAllArguments[class_]:=GetArguments[class]//Values//Flatten//DeleteDuplicates//Sort;
	
ClearAll@FunctionQ;
FunctionQ[class_[___],functionName_]:= FunctionQ[class,functionName];
g:FunctionQ[class_,functionName_]:= g = MemberQ[stringObjectFunctions[class],functionName];

ObjectSet[_Symbol[symbol_]|symbol_,key_,value_]:= symbol[key] = value;

SetAttributes[ClearObject, Listable];
ClearObject[_[symbol_,___]|symbol_]:=ClearAll[symbol];

CopySymbol[head_[oldSymbol_]]:=Module[{Global`object=CopySymbol[oldSymbol]},head[Global`object]];
CopySymbol[oldSymbol_Association]:=oldSymbol;

(*Allows to store an UpValue in a class or an object*)
SetAttributes[StoreUpValue,HoldFirst];
StoreUpValue[symbol_,parameterSymbol_,value_,"Class"] /; MatchQ[symbol,_[_Symbol]]:= StoreUpValue[Evaluate@Head@symbol,parameterSymbol,value,"Object"];
StoreUpValue[symbol_,parameterSymbol_,value_,"Class"]:= StoreUpValue[symbol,parameterSymbol,value,"Object"];
StoreUpValue[symbol_,parameterSymbol_,value_,"Object"] /; MatchQ[symbol,_[_Symbol]]:= symbol /. _[x_]:>StoreUpValue[x,parameterSymbol,value,"Object"];
StoreUpValue[symbol_,parameterSymbol_,value_,"Object"]:= (parameterSymbol[symbol]^=value);
StoreUpValue[symbol_,parameterSymbol_,value_,type_:"Object"]:=StoreUpValue[symbol,parameterSymbol,value,type];

(*Allows to access an UpValue in a class or an object*)
SetAttributes[AccessUpValue,HoldFirst];
AccessUpValue[symbol_,parameterSymbol_,"Class"] /; MatchQ[symbol,_[_Symbol]]:= AccessUpValue[Evaluate@Head@symbol,parameterSymbol,"Object"];
AccessUpValue[symbol_,parameterSymbol_,"Class"]:= AccessUpValue[symbol,parameterSymbol,"Object"];
AccessUpValue[symbol_,parameterSymbol_,"Object"] /; MatchQ[symbol,_[_Symbol]]:= symbol /. _[x_]:>AccessUpValue[x,parameterSymbol,"Object"];
AccessUpValue[symbol_,parameterSymbol_,"Object"]:= parameterSymbol[symbol] /. _parameterSymbol -> {};
AccessUpValue[symbol_,parameterSymbol_,type_:"Object"]:=AccessUpValue[symbol,parameterSymbol,type];
	
SetAttributes[{SetHelp,Help,ClearHelp},HoldFirst];

If[!ValueQ@$Help,
	$Help = Association[];
];

Help /: Set[Help[x_],message_]:= SetHelp[x,message]; 

SetHelp[Dot[class_,f_],message_]:=$Help[{SymbolName@class,SymbolName@f}]=message;
SetHelp[class_,message_]:=$Help[{SymbolName@class,"1_definition"}]=message;

Help[x_String]:=Help @@ Hold[Evaluate@ToExpression@x];

Help[Dot[class_,f_]]:=
	Module[{className=SymbolName@class,functionName=SymbolName@f},
		If[KeyExistsQ[$Help,{className,functionName}],
			className~~"."~~functionName~~" : "~~$Help[{className,functionName}]
			,
			className~~"."~~functionName~~" : No help found"
		]
	];
	
getHelpKeys[class_]:=Cases[Keys[$Help],{SymbolName@class,_}]//SortBy[Last];

Help[class_,displayFunctionsHelpAlso_:True]:=
	Module[{classKeys},
		classKeys=getHelpKeys[class];
		
		If[classKeys=!={},
			If[displayFunctionsHelpAlso,
				#[[1]]~~If[#[[2]]=="1_definition","","."~~#[[2]]]~~" : "~~$Help@#& /@ classKeys //
				TableForm
				,
				If[MemberQ[classKeys,{SymbolName@class,"1_definition"}],
					SymbolName@class~~" : "~~$Help@{SymbolName@class,"1_definition"}
					,
					SymbolName@class~~" : No help found"
				]
			]
			,
			SymbolName@class~~" : No help found"
		]
	];
	
ClearHelp[class_]:=(KeyDropFrom[$Help,getHelpKeys[class]];);
ClearHelp[Dot[class_,f_]]:=(KeyDropFrom[$Help,{{SymbolName@class,SymbolName@f}}];);

(*SetHelp[class,"Hello"];
SetHelp[class.f,"Hello2"];
Help[class]
Help[class.f]
ClearHelp[class.f]
ClearHelp[class]

Help[class] = "m3"
Help[class.g] = "mm"*)
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* BaseClass *)
(*Accessor stored in SubValues[BaseClass]*)
(*functionality could be delegated to BaseClass.get but this would slow down the access
as an object function call is slower than a SubValue*)
BaseClass[object_,___][keys_List] := Lookup[object,keys];
BaseClass[object_,___][key_] := object[key];
BaseClass[object_,___][keys__] := Fold[#1[#2]&,object,{keys}];

(*implementations should be in class functions in order to benefit from inheritance*)
(*BaseClass.(key:Except[sub|super,_Symbol|_String]):= o.getItem[key];*)
BaseClass /: BaseClass[object_Symbol,___].(key:Except[sub|super,_Symbol|_String]):= object[MTools`Utils`Utils`GetSymbolName@key];
SameQ[o_BaseClass,x__]^:= AllTrue[{x},o.sameQ[#]&];
UnsameQ[o_BaseClass,x_]^:= !SameQ[o,x]; 
Keys[o_BaseClass]^:= o.getFields[];
Values[o_BaseClass]^:= o.getFieldValues[];

(*Field setters getters*)
(*Function for fields. A field is an object key*)
BaseClass /: BaseClass[object_Symbol,___].set[keyValues_Association] := AssociateTo[object,keyValues];
BaseClass /: BaseClass[object_Symbol,___].set[keyValues_List] := AssociateTo[object,keyValues];
BaseClass /: BaseClass[object_Symbol,___].set[keys_List,values_List] /; Length@keys == Length@values := AssociateTo[object,Thread[keys->values]];
BaseClass.set[keys_List,value_] := o.set[keys,ConstantArray[value,Length@keys]];
BaseClass.set[keys__,lastKey_,value_] := o[keys].set[lastKey,value];
BaseClass /: BaseClass[object_Symbol,___].set[key_,value_] := object[key]=value; 
SetField = #.set[#2,#3]&;

(*clear shouln't be overloaded whereas delete can be*)
BaseClass.clear[]:= ClearObject[o];
BaseClass.delete[]:= o.clear[];
BaseClass.copy[]:= CopySymbol[o];
BaseClass.type[]:= Head@o;
BaseClass /: BaseClass[object_Symbol,___].switchType[class_]:= class[object];
BaseClass.isType[testType_]:= o.type[] === testType;
BaseClass /: BaseClass[object_Symbol,___].data[]:= object;
BaseClass.flattenObject[]:= {o.type[],o.data[]};
BaseClass /: BaseClass[object_Symbol,___].objectQ[]:= ValueQ[object];
BaseClass.sameQ[x_]:= TrueQ[x.objectQ[]] && o.isType[x.type[]] && (o.data[] === x.data[]);

(*TODO allow to not initialize all fields*)
BaseClass.initData[options_]:= 
	Block[{classOptionNames,class=o.type[]},
		
		classOptionNames=ClassFields[class];
		
		If[!MatchQ[classOptionNames,_ClassFields|{}],
			o.set[classOptionNames,OptionValue[class,options,#]& /@ classOptionNames]
		]
    ];

(*setItem and getItem can be oveloaded, and take a string as key*)
Unprotect[Dot];
Dot /: Set[Dot[object_,key_],value_] := object.setItemAux[key,value];
Dot /: AddTo[Dot[object_,key_],value_] := object.addToItemAux[key,value];
Dot /: TimesBy[Dot[object_,key_],value_] := object.multiplyByItemAux[key,value];
Protect[Dot];

BaseClass /: BaseClass[object_Symbol,___].getItem[key_]:= object[MTools`Utils`Utils`GetSymbolName@key];

BaseClass.setItemAux[HoldPattern[Dot[keys__]],value_]:=
	Fold[#1[MTools`Utils`Utils`GetSymbolName@#2]&,o,Most@{keys}].set[MTools`Utils`Utils`GetSymbolName@Evaluate@Last@{keys},value];
BaseClass /: BaseClass[object_Symbol,___].setItemAux[key_,value_] := object[MTools`Utils`Utils`GetSymbolName@key] = value;
(*BaseClass.setItemAux[HoldPattern[Dot[keys__]],value_]:=
	Fold[#1.getItem[#2]&,o,Most@{keys}].setItem[MTools`Utils`Utils`GetSymbolName@Evaluate@Last@{keys},value];
BaseClass.setItemAux[key_,value_] := o.setItem[MTools`Utils`Utils`GetSymbolName@key,value];
BaseClass.setItem[key_,value_] := o.set[key,value];*)

BaseClass.addToItemAux[HoldPattern[Dot[keys__]],value_]:=
	Fold[#1[MTools`Utils`Utils`GetSymbolName@#2]&,o,Most@{keys}].addToField[MTools`Utils`Utils`GetSymbolName@Evaluate@Last@{keys},value];
BaseClass.addToItemAux[key_,value_] := o.addToField[MTools`Utils`Utils`GetSymbolName@key,value];

BaseClass.multiplyByItemAux[HoldPattern[Dot[keys__]],value_]:=
	Fold[#1[MTools`Utils`Utils`GetSymbolName@#2]&,o,Most@{keys}].multiplyByField[MTools`Utils`Utils`GetSymbolName@Evaluate@Last@{keys},value];
BaseClass.multiplyByItemAux[key_,value_] := o.multiplyByField[MTools`Utils`Utils`GetSymbolName@key,value];
(*x=New[GenericClass][]
x.a=New[GenericClass][]
x.a.b=New[GenericClass][]
x."a".b.c=6
x.a."b".c
x.a."b".c += 3
x.a."b".c -= 4
x.a."b".c *= 4
x.a."b".c /= 4
*)

BaseClass.fieldExistsQ[field__]:= o[field] // FreeQ[Missing];
BaseClass.get[field__]:= o[field] // If[FreeQ[#,Missing], #, Missing[field]]&; 
BaseClass.get[]:= Identity@o;
BaseClass /: BaseClass[object_Symbol,___].deleteField[field_]:= Unset[object[field]]; 
BaseClass /: BaseClass[object_Symbol,___].getFields[]:= Keys[object];
BaseClass /: BaseClass[object_Symbol,___].getFieldValues[]:= Values[object];
BaseClass /: BaseClass[object_Symbol,___].fieldTake[fields_]:= KeyTake[object,fields];
BaseClass.addField[field_,value_]:= (o.set[field,value]; o); (*builder pattern*)

BaseClass.getStatic[field_]:= o.getStatic[o.type[],field];
BaseClass.getStatic[class_,field_]:= $StaticAssociation[class,field];
BaseClass.setStatic[field_,value_]:= o.setStatic[o.type[],field,value];
BaseClass.setStatic[class_,field_,value_]:= $StaticAssociation[class,field] = value;
BaseClass.deleteStatic[field_]:= o.deleteStatic[o.type[],field];
BaseClass.deleteStatic[class_,field_]:= Unset[$StaticAssociation[class,field]];
BaseClass.deleteAllStatic[]:= o.deleteAllStatic[o.type[]];
BaseClass.deleteAllStatic[class_]:= ($StaticAssociation[class] = Association[];);

BaseClass.getSuperClass[nth_:1]:= Supers[o.type[]] // Reverse // #[[Min[nth,Length@#]]]&;

(*Functions for elements contained in fields*)
BaseClass.getOption[options_,option_]:= 
	Block[{currentType},
		currentType=o.type[];
		OptionValue[currentType,FilterRules[options,Options[currentType]],option]
	];
BaseClass.clearList[asscociationField_]:= o.set[asscociationField,{}];
BaseClass.clearAssociation[asscociationField_]:= o.set[asscociationField,Association[]];
BaseClass /: BaseClass[object_Symbol,___].getKey[keys__,value_]:= object[keys];
BaseClass /: BaseClass[object_Symbol,___].setKey[keys__,value_]:= object[keys] = value;
SetAttributes[setKeyDelayed,HoldRest];
BaseClass /: BaseClass[object_Symbol,___].setKeyDelayed[keys__,value_]:= object[keys] := value;
BaseClass /: BaseClass[object_Symbol,___].deleteKey[keys__]:= Unset[object[keys]];
BaseClass.renameKey[keys__,key_,newKey_]:=
	(
		o.setKey[keys,newKey,o[keys,key]];
		o.deleteKey[keys,key];
	);
BaseClass.getKeys[asscociationField__]:= Keys[o[asscociationField]];
BaseClass /: BaseClass[object_Symbol,___].setKeys[asscociationField__,keyValues_]:= AssociateTo[object[asscociationField],keyValues];
BaseClass /: BaseClass[object_Symbol,___].setKeys[asscociationField__,keys_,values_]:= AssociateTo[object[asscociationField],Thread[keys->values]];
BaseClass /: BaseClass[object_Symbol,___].getValues[asscociationField__]:= Values[object[asscociationField]];
BaseClass /: BaseClass[object_Symbol,___].lookup[asscociationField__,keys_]:= Lookup[object[asscociationField],keys];
BaseClass /: BaseClass[object_Symbol,___].keyTake[asscociationField__,keys_]:= KeyTake[object[asscociationField],keys];
BaseClass.keySort[asscociationField__]:= o.setKey[asscociationField,KeySort[o[asscociationField]]];
BaseClass.keyExistsQ[asscociationField__,key_]:= KeyExistsQ[o[asscociationField],key];
BaseClass.appendTo[listField__,value_]:= o.setKey[listField,Append[o[listField],value]];
BaseClass.prependTo[listField__,value_]:= o.setKey[listField,Prepend[o[listField],value]];
BaseClass.insertTo[listField__,value_,position_]:= o.setKey[listField,Insert[o[listField],value,position]];
BaseClass.prependJoin[listField__,value_]:= o.setKey[listField,Join[value,o[listField]]];
BaseClass.appendJoin[listField__,value_]:= o.setKey[listField,Join[o[listField],value]];
BaseClass /: BaseClass[object_Symbol,___].setPart[part__,value_]:= object[[part]] = value;
BaseClass /: BaseClass[object_Symbol,___].getPart[part__]:= object[[part]];
BaseClass.isEmpty[listOrAssociationField__]:= Normal@o[listOrAssociationField] === {};

BaseClass.deleteCases[listField__,pattern__]:= o.setKey[listField,DeleteCases[o[listField],pattern]];
BaseClass.position[listField__,pattern__]:= Position[o[listField],pattern];
BaseClass.deleteDuplicates[listField__,test_:SameQ]:= o.setKey[listField,DeleteDuplicates[o[listField],test]];
BaseClass.cases[listField__,pattern_]:= Cases[o[listField],pattern];
BaseClass.selectField[listField__,test_]:= Select[o[listField],test];

BaseClass.thread[fun_[lists___]]:= MapThread[o.fun[##]&,{lists}];
BaseClass.through[funs_]:= o.#& /@ funs;
BaseClass.apply[f_[{list___}]]:= o.f[list];
BaseClass.excuteFunction[f_]:= f[o];

BaseClass.addToField[field__,value_]:= o.setKey[field,o[field]+value];
BaseClass.multiplyByField[field__,value_]:= o.setKey[field,o[field]*value];
BaseClass.increment[field__]:= o.addToField[field,1];
BaseClass.decrement[field__]:= o.addToField[field,-1];

BaseClass.maxIndex[list_]:= Ordering@list // Last;
BaseClass.minIndex[list_]:= Ordering@list // First;

CreateHeldValue[initValue_:None]:= Module[{Global`heldValue=initValue},Hold[Global`heldValue]];
GetHeldValue[Hold[heldSymbol_]]:= heldSymbol;
GetHeldValue[Hold[heldSymbol_],key_]:= heldSymbol[key];
SetHeldValue[Hold[heldSymbol_],value_]:= heldSymbol = value;
SetHeldValue[Hold[heldSymbol_],key_,value_]:= heldSymbol[key] = value;
DeleteHeldValue[Hold[heldSymbol_]]:= Unset[heldSymbol];
DeleteHeldValue[Hold[heldSymbol_],key_]:= Unset[heldSymbol[key]];

BaseClass.createHeldValue[field_,initValue_:None]:= o.set[field,CreateHeldValue[initValue]];
BaseClass.setHeldValue[field_,value_]:= SetHeldValue[o[field],value];
BaseClass.setHeldValue[field_,key_,value_]:= SetHeldValue[o[field],key,value];
BaseClass.getHeldValue[field_]:= GetHeldValue[o[field]];
BaseClass.getHeldValue[field_,key_]:= GetHeldValue[o[field],key];
BaseClass.deleteHeldValue[field_]:= DeleteHeldValue[o[field]];
BaseClass.deleteHeldValue[field_,key_]:= DeleteHeldValue[o[field],key];

(*useful with TrackHeldTick*)
BaseClass.newTick[fields_List]:= o.thread[newTick[fields]];
BaseClass.newTick[field_]:= o.createHeldValue[field,False];
BaseClass.tickNotify[field_]:= o[field] /. Hold[var_] :> If[ValueQ@var,var = !var];

SetAttributes[{cacheFunction,staticCacheFunction},HoldFirst];
(*class is the calling class*)
BaseClass /: BaseClass[object_Symbol,class_,___].cacheFunction[f_[args___],result_]:= (class /: class[object,___].f[args] := result; result);
BaseClass /: BaseClass[object_Symbol,class_,___].staticCacheFunction[f_[args___],result_]:= (class /: class[___].f[args] := result; result);
BaseClass /: BaseClass[object_Symbol,___].clearCacheFunction[class_,function_,cacheType_:"Object"]:= 
	Module[{oPattern,mainRepresentationRule},
		
		If[MatchQ[cacheType,"Object"|"AllObjects"],
			(*we remove the main representation rule as it makes pattern matching impossible*)
			mainRepresentationRule = Last@UpValues[class];
			UpValues[class] = Most@UpValues[class];
		];
		
		oPattern = 
			Switch[cacheType,
				"Static",
					class[Verbatim[___]]
				,
				"Object",
					class[object,___]
				,
				"AllObjects",
					class[_,Verbatim[___]]
			];

		UpValues[class] =
			DeleteCases[
				UpValues[class]
				,
				Verbatim[RuleDelayed][Verbatim[HoldPattern][Verbatim[Dot][oPattern,f_function/;FreeQ[Hold@f,Pattern]]],_]
			];
			
		If[MatchQ[cacheType,"Object"|"AllObjects"],
			UpValues[class] = Append[UpValues[class],mainRepresentationRule];
		];
	];
(*
xx=NewClass[]
yy=NewClass["Parents"->{xx}]
xxx=New[xx][]
xxx.f[]
yyy=New[yy][]
yyy.f[]
yyy.clearCacheFunction[xx,f,"Static"]
zz.fib[n_] := o.cacheFunction[fib[n],Print[n]; If[MatchQ[n, 0 | 1], 1, o.fib[n - 1] + o.fib[n - 2]]];
*)

Supers[BaseClass] ^= {};
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* Edit interface *)
(*
{{key,specs}...}
Interface example
Interface[MyClass] = {{"Field1","PopupMenu","Specs"->{{2}}},{"Field2","InputField","Callback"->Print},...}

InterfaceOrdering[IbVanilla] ^= {"key1","key2"...}
*)

SetAttributes[{Interface,InterfaceOrdering,InterpretSymbolName},HoldFirst];

SetInterface[classContainer_,interface_,type_:"Class"] := StoreUpValue[classContainer,Interface,interface,type];
GetInterface[classContainer_,type_:"Class"] := AccessUpValue[classContainer,Interface,type];
ClearInterface[classContainer_,type_:"Class"] := StoreUpValue[classContainer,Interface,{},type];
ModifyInterface[classContainer_,newClassInterface_List,type_:"Class"] := 
	StoreUpValue[
		classContainer,
		Interface,
		Prepend[AccessUpValue[classContainer,Interface,type],newClassInterface]//
		DeleteDuplicates[#,(First@#1===First@#2)&]&,
		type
	];
RemoveInterface[classContainer_,field_,type_:"Class"] := 
	StoreUpValue[
		classContainer,
		Interface,
		DeleteCases[AccessUpValue[classContainer,Interface,type],{field,__}],
		type
	];
SetInterfaceOrdering[classContainer_,interfaceOrdering_,type_:"Class"] := StoreUpValue[classContainer,InterfaceOrdering,interfaceOrdering,type];
GetInterfaceOrdering[classContainer_,type_:"Class"] := AccessUpValue[classContainer,InterfaceOrdering,type];

FormatClass[class_Symbol]:=Format[class[object_Symbol],StandardForm]:=InterpretSymbol[class[object]];
	
inheritInterface[newClass_,classList_]:=
	Module[{styling},
	    styling=
	    	DeleteDuplicates[
	    		Join@@( AccessUpValue[#,Interface]& /@ Prepend[classList,newClass] ),
	    		(First[#1] === First[#2]&)
	    	];
	    
	    If[styling =!= {}, 
	    	SetInterface[newClass,styling];
		];
		
    	(*Inheriting an appearance if already defined*)
    	If[FormatValues[newClass]==={},
	    	Scan[
	    	    Function[{inheritedType},	    	    	
	    	   		If[FormatValues[inheritedType]=!={},
	    	    		With[{Type=newClass},
	    	    			FormatValues[Type]= FormatValues[inheritedType] /. {inheritedType->Type};
	    	    		];
	    	    		
	    	    		Return[];
	    	    	];
	    	    ]
	    	    ,
	    	 	Reverse@classList
	    	];
    	];
	];
	
SetAttributes[MyHold,HoldAllComplete];

(*TODO allow different layouts and an option displey key in getinterface element*)
Options[EditSymbolPane] = 
	{
		"DynamicPane"->False,
		"InterfaceOrdering"->Automatic,
		"InterfaceOrderingSymbol"->InterfaceOrdering,
		"FieldsExcluded"->None,
		"SortKeys"->True,
		"PaneSize"->{Automatic (*H*),Automatic (*V*)},
		"ScrollbarsOption"->{False,True},
		"ForceSize"->False,
		"Transpose"->False,
		"HeaderField"->None
	};
EditSymbolPane[object_,opts:OptionsPattern[]] :=
	Module[{interfaceOrdering, dict, elements,interfaceOrderingDepth, displayedGrid},
		
		If[OptionValue@"DynamicPane",
			(*
				Allow to vary dynamically which fields are displayed or how they are displayed
				Exemple:
				$Settings.newTick["dynamicPaneTick"]
				EditSymbolPane[$Settings,"DynamicPane"->True]
				
				$Settings.setInterfaceOrdering[{"Id","Symbol"}]
				$Settings.tickNotify["dynamicPaneTick"]
			*)
			MTools`Utils`Utils`TrackHeldTick[{object["dynamicPaneTick"]},
				EditSymbolPane[object,"DynamicPane"->False,opts]
			]
	    	,
		    {dict,interfaceOrdering}=GetKeys[object,FilterRules[{opts},Options[GetKeys]]];
		    
		    interfaceOrderingDepth = Depth[interfaceOrdering]-1;
		    
		    elements = 
		        Map[
		        	If[KeyQ[object,#],
						GetInterfaceElement[object,dict@#,"Transpose"->OptionValue@"Transpose"]
						,
						GetInterfaceElement[object,None]
		        	]&
			        , 
			        interfaceOrdering
			        ,
			        {interfaceOrderingDepth}
		        ];
			
			If[KeyQ[object,OptionValue@"HeaderField"],
				elements =
					Join[
						Prepend[
							Table[{"",""},{If[interfaceOrderingDepth == 1,0,Length@First@elements-1]}]
							,
							{Style[object["Name"],Bold],SpanFromLeft}
						] // If[interfaceOrderingDepth == 1,Identity,List]
						,
						elements
					];	
			];
	
		    If[interfaceOrderingDepth == 2,
		    	elements = Join @@@ elements;
		    ];
		    
		    If[OptionValue@"Transpose",
		    	elements = Transpose@elements;
		    ];
	        
			displayedGrid =
		        Grid[
				    elements
			        ,
			        Alignment -> Left
		        ]  /.  x_Symbol /; MTools`Utils`Utils`GetSymbolName@x == "SetField" :> SetField[##] // 
		        Replace[#,MyHold@e_ :> e,Infinity,Heads->True]&;
	
		    Pane[
				displayedGrid
		        ,
		        OptionValue@"PaneSize" /. {h_,v_?NumericQ} /; ((interfaceOrderingDepth == 1) && 
		        Length[interfaceOrdering]<=5 && !OptionValue@"ForceSize") :> {h,Automatic}
				,
		        Scrollbars -> OptionValue["ScrollbarsOption"] /.  {h_,True} /; ((interfaceOrderingDepth == 1) && 
		        Length[interfaceOrdering]<=5 && !OptionValue@"ForceSize") :> {h,False}
		    ]  
		] 
	];
	
Options[GetKeys]={"InterfaceOrdering"->Automatic,"InterfaceOrderingSymbol"->InterfaceOrdering,"FieldsExcluded"->None,"SortKeys"->True};
GetKeys[object_,opts:OptionsPattern[]]:=
	Module[{paramNames,interface,interfaceRules,paramName,paramRules,dict=Association[], 
			interfaceOrdering,fieldsExcluded,sortKeys,interfaceOrderingSymbol},
		
		{fieldsExcluded,sortKeys,interfaceOrderingSymbol,interfaceOrdering} = 
			OptionValue[GetKeys,{opts},#]& /@ {"FieldsExcluded","SortKeys","InterfaceOrderingSymbol","InterfaceOrdering"};
		
	    paramNames = keys[object,sortKeys];
	    
	    If[fieldsExcluded =!= None,
	        paramNames = Complement[paramNames,fieldsExcluded];
	    ];
	    
	    (*An interface for a same element in an object overrides what's defined at class level*)
	    interface=
	    	Join[AccessUpValue[object,Interface],AccessUpValue[object,Interface,"Class"]]//
	    	DeleteDuplicates[#,(First@#1===First@#2)&]&;
	    
	    If[interface =!= {},    	
	    	interfaceRules=
	    		Map[
	    			(
	    				paramName = #[[1]]; 
	    				paramName-> #
    				)&
    				,
    				interface
				];

	    	paramRules = paramNames /. interfaceRules;
	    	,
	    	paramRules = paramNames
	    ];
	    
	    (*We take as a priority InterfaceOrdering at object level if available else at class level*)
	    If[interfaceOrdering===Automatic,
		    interfaceOrdering=AccessUpValue[object,interfaceOrderingSymbol];
		    
		    If[interfaceOrdering === {},
		    	interfaceOrdering = AccessUpValue[object,interfaceOrderingSymbol,"Class"];
		    ];
		    
		    If[interfaceOrdering==={},
		    	interfaceOrdering = paramNames;
		    ];
	    ];
	    
	    dict = AssociationThread[paramNames->paramRules];
	    (dict[#]=#)& /@ {None,SpanFromLeft,SpanFromAbove,SpanFromBoth};
	    
	    {dict,interfaceOrdering}
	];

SetAttributes[EditSymbol,HoldFirst];
Options[EditSymbol]=Join[{"Prompt"->None,"WindowTitle"->"","UseBackupOnCancel"->True},Options[EditSymbolPane]];
EditSymbol[object_,opts:OptionsPattern[]] :=
	Module[{dialogReturn,objectBackup,useBackup},

	    If[(useBackup = OptionValue@"UseBackupOnCancel"),
	    	objectBackup = object.data[];
	    ];
	    
	    dialogReturn =
		    DialogInput[
		        Column[
			        {
			        	If[OptionValue@"Prompt" =!= None,OptionValue@"Prompt",##&[]]
			        	,
						EditSymbolPane[object,FilterRules[{opts},Options@EditSymbolPane],"PaneSize"->{Automatic,(*300*) 150}]
				        ,
				        Row[{DefaultButton[DialogReturn[True]],CancelButton[DialogReturn[False]]},Alignment->Center]
			        }
		        ]
		        ,
		        "WindowTitle"->OptionValue@"WindowTitle"
		    ];
		    
		If[!dialogReturn && useBackup,
			object.set[objectBackup];
		];
		
		dialogReturn
	];
    
Options[InterpretSymbol] = Join[{"InterpretSymbolValue"->None},Options@EditSymbolPane];
SetOptions[InterpretSymbol,{"PaneSize"->{Automatic,150},"ForceSize"->True}];
SetAttributes[InterpretSymbol,HoldFirst];
InterpretSymbol[symbol_ /; Head@symbol === Association,opts:OptionsPattern[]] := InterpretSymbol[BaseClass[symbol],opts];
InterpretSymbol[symbol_,opts:OptionsPattern[]] :=
	Module[{interpretation,symb,symbolValue,interpretedSymbol},
		
		StoreUpValue[symbol,InterpretSymbolName,ToString@HoldForm@symbol];
		
		symbolValue = OptionValue@"InterpretSymbolValue";
		interpretedSymbol =
			Switch[symbolValue,
				None,symb,
				_,symb[symbolValue]
			];
		
		SetAttributes[interpretation,HoldAllComplete];
		With[{s=interpretedSymbol},
		    interpretation[
				Panel@EditSymbolPane[symbol,FilterRules[{opts},Options[EditSymbolPane]],"PaneSize"->{Automatic,150}]
			    ,
				s
		    ]
		]  /. {interpretation->Interpretation,symb->symbol}
	];
	
SetAttributes[STEP, {Flat, OneIdentity, HoldFirst}];
STEP[expr_] :=
	Module[{P},
		P = (P = Return[# (*/. HoldForm[x_] :> Defer[step[x]]*), TraceScan] &) &;
		TraceScan[P, expr, TraceDepth -> 1] 
	];

SetAttributes[UninterpretSymbol,HoldFirst];
UninterpretSymbol[symbol_]:=
	With[{symbolName=StringReplace[AccessUpValue[symbol,InterpretSymbolName],"BaseClass["~~m__~~"]":>m]},
		If[StringFreeQ[symbolName,"["],
			STEP@Symbol@symbolName/.(HoldForm[x_]:>Defer[x])
			,
			ToExpression@symbolName
		]
	];
(*Test with association
x=Association["a"->2]
x//InterpretSymbol
Modify x
x//PrintSymbol
x//UninterpretSymbol*)

SetAttributes[EditFunction, HoldFirst];
Options[EditFunction] = {"Head"->None,"UseNew"->False};
EditFunction[function_] := EditFunction[function[]];
EditFunction[function_[params___],OptionsPattern[]] :=
	Module[{symbolEdited,names,values,options},	   
		 
	    symbolEdited=MTools`Utils`Utils`GetAllOptions[function,params];
	    
	    With[{head=OptionValue@"Head"},
	    	EditSymbol[head[symbolEdited]];
	    ];
	    
	    {names,values}=Items[symbolEdited]//Transpose;
	    
	    options = MapThread[#1 -> #2 &, {MTools`Utils`Utils`GetSymbolName/@names, values}];
	    
	    If[!OptionValue@"UseNew",
		    function[params,options]
		    ,
		    New[function][params,options]
	    ]
	];

EditNewObject[function_][params___]:=EditFunction[function[params],"Head"->function,"UseNew"->True];

Options[GetInput]={"Interface"->None,"InterfaceOrdering"->None}
GetInput[prompt_,values_,defaults_,OptionsPattern[]]:=
	Module[{object=New[][],editReturn,interface,interfaceOrdering},
		
		object.set[values,defaults];
		
		If[(interface = OptionValue@"Interface") =!= None,
			SetInterface[object,interface,"Object"];
		];
		
		If[(interfaceOrdering = OptionValue@"InterfaceOrdering") =!= None,
			SetInterfaceOrdering[object,interfaceOrdering,"Object"];
		];
		
		editReturn=EditSymbol[object,"Prompt"->prompt];
		
		If[editReturn===False,
			object.delete[];
			False
			,
			(*the object returned should be deleted later*)
			object
		]
	];
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* GetInterfaceElement *)
(*eventFunction[object,key,value] passed in "Callback" option*)
(*eventFunction takes as arguments  the object, the key and a new dynamic value to be assigned*)
SetAttributes[dynamicEvent,HoldFirst];
dynamicEvent[object_,key_,None]:=ObjectSet[object,key,#]&;
dynamicEvent[object_,key_,eventFunctions_List]:=dynamicEvent[object,key,#]& /@ eventFunctions; (*for the case {fstart, f, fend}*)
dynamicEvent[object_,key_,eventFunction_]:=eventFunction[object,key,#]&;

(*doesn't work with FileNameSetter, find a solution*)
getHeldSymbol[_Symbol[symbol_,___]|symbol_]:= Hold[symbol];

getTrackedRule[heldSymbols_]:=
	With[{heldSymbolsJoined=Join @@ heldSymbols},
		TrackedSymbols:>heldSymbolsJoined /. Hold->List
	];
trackedSymbols[object_,x_:None]:=getTrackedRule[{getHeldSymbol[object]}];
(*useful to make a dynamic object react to changes in other objects*)
trackedSymbols[object_,x_List]:=getHeldSymbol /@ Prepend[x,object] // getTrackedRule;

GetTrackedSymbols[x_List]:=getHeldSymbol /@ x // getTrackedRule;

getLabel[key_,options_]:= 
	Switch[OptionValue[GetInterfaceElement,options,"Label"],
		Automatic,
			key,
		None,
			##&[],
		Null,
			"",
		_,
			OptionValue[GetInterfaceElement,options,"Label"]
	];

SetAttributes[DynamicObjectInterface,HoldFirst];
DynamicObjectInterface[object_,key_,options_,interfaceSpec_]:=
    Module[{elementSpecs,interface, eventFunction, trackedSymbolsExpression, inputFieldSpecs,label,dynamicFormat},
    	
    	label = getLabel[key,options];
    	dynamicFormat = OptionValue[GetInterfaceElement,options,"StringFormat"];
    	eventFunction = dynamicEvent[object,key,OptionValue[GetInterfaceElement,options,"Callback"]];
    	elementSpecs = Sequence @@ OptionValue[GetInterfaceElement,options,"Specs"];
    	trackedSymbolsExpression = trackedSymbols[object, OptionValue[GetInterfaceElement,options,"TrackedSymbolsExpression"]];
    	inputFieldSpecs = Sequence @@ OptionValue[GetInterfaceElement,options,"InputFieldSpecs"];
    	
    	interface = interfaceSpec /. 
    					{
    						Label -> label,
    						DynamicEvent -> eventFunction,
    						ElementSpecs -> elementSpecs,
    						TrackSymbols -> trackedSymbolsExpression,
    						InputFieldSpecs -> inputFieldSpecs,
    						DynamicFormat -> dynamicFormat
    					} /. 
    					DynamicObject -> MyHold[object];
    					
    	interface
    ];
    
SetAttributes[DynamicElement,HoldFirst]; 
DynamicElement[object_,key_,options_,element_]:=
	DynamicObjectInterface[object,key,options,
	    {
	       	MyHold@Label
	        ,
			MyHold@element[
				Dynamic[DynamicObject[key],DynamicEvent,TrackSymbols]
				,
				ElementSpecs
			]
	    }
	];

SetAttributes[GetInterfaceElement,HoldFirst];
Options[GetInterfaceElement]=
	{
		"Transpose"->False,
		"Callback"->None,
		"Label"->Automatic,
		"Specs"->{},
		"InputFieldSpecs"->{},
		"TrackedSymbolsExpression"->None (*use TrackedSymbols :> {symbols as option value}*),
		"StringFormat"->Identity
	};

GetInterfaceElement[object_,None,OptionsPattern[]]:={"",""};	
(GetInterfaceElement[object_,#,OptionsPattern[]]:={#,#})& /@ {SpanFromLeft,SpanFromAbove,SpanFromBoth};

GetInterfaceElement[object_,key_String,OptionsPattern[]]:=
	DynamicObjectInterface[object,key,{},
	    {
	        MyHold@Label
	        ,
	        Switch[object[key],
	        	True | False,
					MyHold@Checkbox[Dynamic[DynamicObject[key],DynamicEvent,TrackSymbols]],
	            _Dynamic,
	            	MyHold@DynamicObject[key],
	            _Symbol[_Symbol],
	            	MyHold@Button[
			            "Edit"
			            ,
			            EditSymbol[DynamicObject[key]]
			            ,
			            Method->"Queued"
		        	],
	            _,
					MyHold@InputField[Dynamic[DynamicObject[key],DynamicEvent,TrackSymbols],ImageSize->Tiny,FieldSize->Infinity]
	        ]
	    }
	]; 
    
$dynamicElements = 
	{
		Checkbox,
		Setter,
		Toggler,
		PopupMenu,
		RadioButtonBar,
		TogglerBar,
		SetterBar,
		CheckboxBar,
		ListPicker,
		ColorSlider,
		ColorSetter
	};
Function[element,
	With[{elementName = ToString@element},
		GetInterfaceElement[object_,{key_String,elementName,opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
		    DynamicElement[object,key,{opts},element]
	]
] /@ $dynamicElements; 

GetInterfaceElement[object_,{key_String,"String",opts___Rule},OptionsPattern[]]:=
	DynamicObjectInterface[object,key,{opts},
	    {
	       	MyHold@Label
	        ,
	        MyHold@Dynamic@DynamicFormat@DynamicObject[key]
	    }
	]; 

GetInterfaceElement[object_,{key_String,"Calendar",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
	DynamicObjectInterface[object,key,{opts},
	    {
	        MyHold@Label
	        ,
	        (*TODO allow just one Calendar Displayer*)
			MyHold@InputField[
				Dynamic[DynamicObject[key],DynamicEvent,TrackSymbols]
				,
				String,InputFieldSpecs
				,
				ImageSize->Small,FieldSize->Infinity
			]
	       	,
	        MyHold@MTools`Utils`Utils`GetCalendar[DynamicEvent]
	    }
	];
    
GetInterfaceElement[object_,{key_String,"InputField",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
	DynamicObjectInterface[object,key,{opts},
	    {
	        MyHold@Label
	        ,
			MyHold@InputField[
				Dynamic[DynamicObject[key],DynamicEvent,TrackSymbols]
				,
				ElementSpecs,ImageSize->Tiny,FieldSize->Infinity
			]
	    }
	];
	
GetInterfaceElement[object_,{key_String,"EditSymbol",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
	DynamicObjectInterface[object,key,{opts},
	    {
	        MyHold@Label
	        ,
			MyHold@Button[
	            "Edit"
	            ,
	            EditSymbol[DynamicObject[key]];
	            DynamicEvent[0];
	            ,
	            ElementSpecs
	            ,
	            Method->"Queued"
        	]
	    }
	];
	
GetInterfaceElement[object_,{key_String,"FileNameSetter",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
	DynamicObjectInterface[object,key,{opts},
	    {
	        MyHold@Label
	        ,
			MyHold@InputField[
				Dynamic[DynamicObject[key],DynamicEvent,TrackSymbols]
				,
				String,InputFieldSpecs,ImageSize->Small,FieldSize->Infinity
			]
	        ,
			MyHold@FileNameSetter[
				Dynamic[DynamicObject[key],DynamicEvent]
				,
				ElementSpecs
			]
	    }
	];
    
GetInterfaceElement[object_,{key_String,"Slider",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
	DynamicObjectInterface[object,key,{opts},
	    {
	        MyHold@Label
	        ,
	        If[OptionValue@"Transpose",Column,Row][
		    	{
					MyHold@Slider[
						Dynamic[DynamicObject[key],DynamicEvent,TrackSymbols]
						,
						ElementSpecs,ImageSize->Small
					]
			        ,
					MyHold@InputField[
						Dynamic[DynamicObject[key],DynamicEvent]
						,
						InputFieldSpecs,ImageSize -> 50
					]
		        }
	        ]
	    }
	];
	
GetInterfaceElement[object_,{key_String,"ProgressBar",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
	DynamicObjectInterface[object,key,{opts},
	    {
	        MyHold@Label
	        ,
	        If[OptionValue@"Transpose",Column,Row][
		    	{
					MyHold@ProgressIndicator[
						Dynamic[DynamicObject[key]]
						,
						{0,Dynamic[DynamicObject[key~~"End"]]}
						,
						TrackSymbols,ElementSpecs,ImageSize->Small
					]
					,
					Spacer[2]
			        ,
					MyHold@Dynamic[
						(
							If[DynamicObject[key~~"End"]==0.,
								0.
								,
								DynamicObject[key]/DynamicObject[key~~"End"]*100.
							]//NumberForm[#, {3, 1}]& // ToString
						) ~~ "% (" ~~ ToString@NumberForm[DynamicObject[key~~"End"],DigitBlock -> 3] ~~ ")"
						,
						TrackSymbols
					]
		        }
	        ]
	    }
	];
    
GetInterfaceElement[object_,{key_String,"Animator",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
	DynamicObjectInterface[object,key,{opts},
	    {
	        MyHold@Label
	        ,
			MyHold@Animator[
				Dynamic[DynamicObject[key],DynamicEvent,TrackSymbols]
				,
				ElementSpecs,ImageSize->Small
			]
	        ,
			MyHold@InputField[
				Dynamic[DynamicObject[key],DynamicEvent]
				,
				InputFieldSpecs,ImageSize -> 50
			]
	    }
	];

GetInterfaceElement[object_,{key_String,"Manipulator",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
	DynamicObjectInterface[object,key,{opts},
	    {
	        MyHold@Label
	        ,
			MyHold@Manipulator[
				Dynamic[DynamicObject[key],DynamicEvent,TrackSymbols]
				,
				ElementSpecs,ImageSize->Small
			]
	        ,
			MyHold@InputField[
				Dynamic[DynamicObject[key],DynamicEvent]
				,
				InputFieldSpecs,ImageSize -> 50
			]
	    }
	];
    
GetInterfaceElement[object_,{key_String,"VerticalSlider",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
	DynamicObjectInterface[object,key,{opts},
	    {
	        MyHold@Label
	        ,
			MyHold@VerticalSlider[
				Dynamic[DynamicObject[key],DynamicEvent,TrackSymbols]
				,
				ElementSpecs,ImageSize->Small
			]
	        ,
			MyHold@InputField[
				Dynamic[DynamicObject[key],DynamicEvent]
				,
				InputFieldSpecs,ImageSize -> 50
			]
	    }
	];
    
GetInterfaceElement[object_,{key_String,"Slider2D",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
	DynamicObjectInterface[object,key,{opts},
	    {
	        MyHold@Label
	        ,
			MyHold@Slider2D[
				Dynamic[DynamicObject[key],DynamicEvent,TrackSymbols]
				,
				ElementSpecs
			]
	        ,
			MyHold@Dynamic[DynamicObject[key]]
	    }
	];
   
GetInterfaceElement[object_,{key_String,"Dynamic",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
	DynamicObjectInterface[object,key,{opts},
	    {
			MyHold@Label
			,
			MyHold@Dynamic[DynamicObject[key],DynamicEvent,TrackSymbols,ElementSpecs]
	    }
	];
    
GetInterfaceElement[object_,{key_String,"Button",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
	DynamicObjectInterface[object,key,{opts},
		Module[{insertedFunction},
			
			With[{function = DynamicEvent},
				(*insertedFunction = f[object,key]*)
				insertedFunction = Hold[function[0]]; 
			];
			
		    {
		    	""
		    	,
		        Function[functionBody,
			        MyHold@Button[
			            Label
			            ,
			            functionBody
			            ,
			            ElementSpecs
			            ,
			            Method->"Queued"
			        ]
			        ,
			        HoldFirst
		        ] @@ insertedFunction
		    }
		]
	];
    
GetInterfaceElement[object_,{key_String,"ActionMenu",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
	DynamicObjectInterface[object,key,{opts},
	    {
	        ""
	        ,
	        MyHold@ActionMenu[
	        	Label
	        	,
	        	DynamicObject[key]
	        	,
	        	ElementSpecs
	        ]
	    }
	];
    
GetInterfaceElement[object_,{key_String,"ButtonBar",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
	DynamicObjectInterface[object,key,{opts},
	    {
	        ""
	        ,
	        MyHold@ButtonBar[
	        	Label
	        	,
	        	DynamicObject[key]
	        	,
	        	ElementSpecs
	        ]
	    }
	];

(*allows to specify an interface as object[key]=pane or object[key]={pane,options}*)
keyViews[object_,key_String]:=
	Replace[
	    Items[object[key],False]
	    ,
	    {
	        {newKey_,{(newParam:(_Symbol[_Symbol]))|newParam_Symbol,opts___}}:>(newKey->EditSymbolPane[newParam,opts])
	        ,
	        {newKey_,element_}:>(newKey->element)
		}
		,
		1
	];

GetInterfaceElement[object_,{key_String,"TabView",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
	DynamicObjectInterface[object,key,{opts},
		{
		    MyHold@TabView[
				keyViews[DynamicObject,key]
				,
				ElementSpecs
		    ]
		}
	];
	
GetInterfaceElement[object_,{key_String,"TabView2",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
	DynamicObjectInterface[object,key,{opts},
		{
		    MyHold@TabView[
				(#[[1]]->EditSymbolPane[DynamicObject,"InterfaceOrdering"->#[[2]]])& /@ DynamicObject[key]
				,
				ElementSpecs
				,
				ImageSize->Automatic
		    ]
		}
	]; (*"tab"->elements to display*)
	
GetInterfaceElement[object_,{key_String,"OpenerView",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
	With[{gridElements=GetInterfaceElement[object,#]& /@ object[key]}, (*possibility to add some styling in #*)
		DynamicObjectInterface[object,key,{opts},
			{
			    MyHold@OpenerView[
					{
						MyHold@Label
						,
						MyHold@Grid[
							gridElements
							,
							Alignment -> Left
						]
					} 
					,
					ElementSpecs
			    ]
			}
		]
	];
   
BuildParamTree[object_,key_,eventFunction_,tree_,selectBarType_,numberOfOpenedLevels_] :=
	OpenerView[
	    {
	        tree[[1]]
	        ,
	        If[Depth[tree[[2]]]>2,
	            Column[
	                BuildParamTree[object,key,eventFunction,#,selectBarType,numberOfOpenedLevels-1]& 
	                /@ 
	                tree[[2]]
	            ]
	            ,
	            (*EventHandler*)
	            selectBarType[
	                Dynamic[
	                    object[key]
	                    ,
	                    (ObjectSet[object,key,#];eventFunction[object,key][0])&
	                ]
	                ,
	                tree[[2]]
	                ,
	                Appearance->"Vertical"
	            ]
	        ]
	    }
	    ,
	    (numberOfOpenedLevels>0)
	];

GetInterfaceElement[object_,{key_String,"Tree",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
	DynamicObjectInterface[object,key,{opts},
	    {MyHold@BuildParamTree[DynamicObject,key,DynamicEvent,ElementSpecs]}
	];

GetInterfaceElement[object_,{key_String,"Pane",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:={EditSymbolPane[object[key],opts]};

GetInterfaceElement[object_,{key_String,"Row",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:= EditSymbolPane[#,opts]& /@ object[key];  
	
GetInterfaceElement[object_,{key_String,"SingleElement"},OptionsPattern[]]:={object[key]};
    
GetInterfaceElement[object_,{key_String,"ValuesSelector",opts:OptionsPattern[GetInterfaceElement]},OptionsPattern[]]:=
	Module[{valuesObject,possibleValues},
		
		ClearAll[valuesObject];
		
		DynamicObjectInterface[object,key,{opts},
			
			possibleValues = First@{ElementSpecs};
			valuesObject = New[MTools`Core`GenericClasses`ValuesSelector]["PossibleValues"->possibleValues,"Values"->object[key]];
			
			With[{valuesObject=valuesObject},
			    {
			        Label
			        ,
					valuesObject.show[(ObjectSet[object,key,#])&]
			    }
			]
		]
	];
	
GetInterfaceElement::unknownInterface="The interface `1` is not recognised.";
GetInterfaceElement[object_,interface_,OptionsPattern[]] /;(Message[GetInterfaceElement::unknownInterface,interface];False) := Null;
(* ::Subsubsection:: *)
(* ::Subsubsection:: *)
(* Key functions *)
SetAttributes[RemoveHead, {HoldAll}];
RemoveHead[h_[args___]] := {args};
NKeys[_[symbol_Symbol,___],sortKeys_:True]:=NKeys[symbol,sortKeys];
NKeys[symbol_,sortKeys_:True] := RemoveHead @@@ DownValues[symbol,Sort->sortKeys][[All,1]];

keys[container_Association,sortKeys_:True]:=Keys@container // If[sortKeys,Sort@#,#]&;
keys[symbol_Symbol[container_,___],sortKeys_:True] := keys[container,sortKeys];
keys[symbol_Symbol,sortKeys_:True] := Replace[NKeys[symbol,sortKeys], {x_} :> x, {1}];

SetAttributes[FormatUpValue, {HoldAll}];
FormatUpValue[h_[args___]] := {h, MTools`Utils`Utils`GetSymbolName /@ {args}};
FormatUpValue[_] := {"",""};
UpKeys[head_[_]]:=UpKeys[head];
UpKeys[symbol_] := FormatUpValue @@@ UpValues[symbol][[All, 1]];

KeyQ[symbol_Association,key_]  :=  KeyExistsQ[symbol,key];
KeyQ[_[symbol_,___],key_] :=  KeyQ[symbol,key];
KeyQ[symbol_,key_] :=  ValueQ[symbol[key]];

UpKeyQ[symbol_,key_] := MemberQ[MTools`Utils`Utils`GetSymbolName /@ UpKeys[symbol][[All,1]],key];

AccessKey[symbol_Association,key_] := symbol@key;
AccessKey[_[symbol_,___],key_] := AccessKey[symbol,key];
AccessKey[symbol_,key_List] := symbol @@ key;
AccessKey[symbol_,key_] := symbol@key;
values[symbol_,sortKeys_:True] := AccessKey[symbol,#]& /@ keys[symbol,sortKeys];

Items[symbol_,sortKeys_:True]:={keys[symbol,sortKeys],values[symbol,sortKeys]}//Transpose;

PrintSymbol[symbols_List,fieldsExcluded___] := 
	DynamicModule[{symbolList=Flatten[{symbols}]},
		Manipulate[
		    PrintSymbol[symbolList[[SymbolIndex]],fieldsExcluded]
	        ,
	        {SymbolIndex,1,Length[symbolList],1}
	    ]
	];
PrintSymbol[symbol_,fieldsExcluded___] :=
    Module[{symbolKeys=Complement[keys[symbol],Flatten@{fieldsExcluded}]},
        TableForm@Transpose[{symbolKeys, symbol /@ symbolKeys}]
    ];
	
UpdateRules[symbols_List,updateSymbolRulesOnly_Symbol:True,newRules___] := UpdateRules[#,updateSymbolRulesOnly,newRules]& /@ Flatten@{symbols};
UpdateRules[symbol_,updateSymbolRulesOnly_Symbol:True,newRules___] :=
    Module[{newRulesUpdated},
    	
        If[{newRules} =!= {},
        	
            newRulesUpdated = Flatten[{newRules}];
            newRulesUpdated[[All, 1]] = MTools`Utils`Utils`GetSymbolName /@ newRulesUpdated[[All, 1]];
            
            If[updateSymbolRulesOnly,
                newRulesUpdated = Intersection[newRulesUpdated, keys[symbol], SameTest -> (First[#1] === #2 &)];
            ];
            
            symbol.set[newRulesUpdated[[All,1]],newRulesUpdated[[All,2]]];
        ];
    ];
(* ::Subsubsection:: *)
End[]

EndPackage[]
