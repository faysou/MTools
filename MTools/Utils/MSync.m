(* Mathematica Package *)

BeginPackage["MTools`Utils`MSync`",
	{
		"MTools`Utils`Utils`"
	}
]
(* Exported symbols added here with SymbolName::usage *) 

$AllowAsynchronousEvaluation::usage = "If set to False, then AsyncEvaluate will always evaluate on the main kernel";

MonitorQueue::usage = "MonitorQueue[]";
StopMonitorQueue::usage = "StopMonitorQueue[]";
AsyncEvaluate::usage = "AsyncEvaluate[parallelAction,opts]";
$NumberOfMonitorQueueCalls::usage = "$NumberOfMonitorQueueCalls  ";
$QueueMonitorInterval::usage = "$QueueMonitorInterval  ";
ExecuteWhen::usage = "ExecuteWhen[condition,action,opts]";

$ReservedKernels;
ReserveKernels::usage = "ReserveKernels[names]";
DrainKernelResults::usage = "DrainKernelResults[kernelName]";
KernelEvaluate::usage = "KernelEvaluate[kernelName,expr]";
IsKernelReserved::usage = "IsKernelReserved[name]";
ParallelEvaluateExceptOn::usage = "ParallelEvaluateExceptOn[names,expr]";
GetNamedKernel::usage = "GetNamedKernel[name]";
KernelSubmit::usage = "KernelSubmit[kernelName,expr]";
ReceiveFromKernel::usage = "ReceiveFromKernel[name]";
ReceiveFromKernelIfReady::usage = "ReceiveFromKernel[name]";
{$NumberOfMonitoredCallsOnKernel,$CheckIntervalPerKernel,$KernelCallback,
$MonitorKernelTask,$EvaluationIdOnKernel,MonitorKernelTask,MonitorKernel,
StopMonitorKernel,KernelResult}
KernelSubmitExceptOn::usage = "KernelSubmitExceptOn[names,expr]"
IsKernelAvailable::usage = "IsKernelAvailable[name_]";
InitKernels::usage = "InitKernels[options]";

CreateNewLink::usage = "CreateNewKernel[]"
ReadFromLink::usage = "ReadFromKernel[kernel]"
SendOnLink::usage = "SendOnLink[link,expression]";
DrainLink::usage = "DrainLink[link]";
$SpotKernel
EvaluateOnLink::usage = "EvaluateOnLink[link,expression]";

NextCronExecutionDate::usage = "NextCronExecutionDate[cronSpec,date:Automatic]";
CronScheduledTaskObject::usage = "CronScheduledTaskObject[sheduledTask,cronSpecs] represents a Cron scheduled task"; 
DateToJavaDate::usage = "DateToJavaDate[date:Automatic]"

Begin["`Private`"] (* Begin Private Context *) 

SetAttributes[ExecuteWhen,HoldAll];
Options[ExecuteWhen] = {"WaitInterval"->1,"WaitLimit"->300,"WaitLimitCallback"->None};
ExecuteWhen[condition_,action_,opts:OptionsPattern[]]:= 
	executeWhen[{condition,action},opts];
	
(*Aux functions so that options are not held (else the function cannot execute when it should)*)
SetAttributes[executeWhen,HoldFirst];
executeWhen[{condition_,action_},opts:OptionsPattern[ExecuteWhen]]:=
	If[condition,
		action
		,	
		With[{startTime=AbsoluteTime[],waitLimit=OptionValue@"WaitLimit",waitLimitCallback=OptionValue@"WaitLimitCallback"},		
			RunScheduledTask[
				Which[
					condition,
						RemoveScheduledTask[$ScheduledTask];
						action
					,
					AbsoluteTime[]-startTime>waitLimit,
						RemoveScheduledTask[$ScheduledTask];
						waitLimitCallback[]
				]
				,
				OptionValue@"WaitInterval"
			]
		]
	];
	
(*http://mathematica.stackexchange.com/a/5274/66
http://mathematica.stackexchange.com/a/13528/66
http://reference.wolfram.com/legacy/applications/parallel/Tutorial/Concurrency.html
http://blog.wolfram.com/2009/03/18/the-evolution-of-parallel-computing-with-mathematica/
*)
$NumberOfMonitorQueueCalls = 0;
$QueueMonitorInterval = 1;
MonitorQueue[checkInterval_:1]:=
	(
		Needs["Parallel`Developer`"];
		
		(*we can reset the $QueueMonitorInterval if the current number of MonitoredCalls is 0*)
		If[$NumberOfMonitorQueueCalls == 0 || checkInterval<$QueueMonitorInterval,
			
			$QueueMonitorInterval = checkInterval;
			
			If[ValueQ[$qRunTask],
				ResetScheduledTask[$qRunTask,$QueueMonitorInterval];
				,
				$qRunTask = 
					CreateScheduledTask[
						Parallel`Developer`QueueRun[]
						,
						$QueueMonitorInterval
					];
			];
		];
		
		Parallel`Developer`QueueRun[];
		StartScheduledTask[$qRunTask];		
		
		$NumberOfMonitorQueueCalls++
	);
	
StopMonitorQueue[]:=
	(
		If[$NumberOfMonitorQueueCalls>0,
			$NumberOfMonitorQueueCalls--;
			
			If[$NumberOfMonitorQueueCalls == 0,
				StopScheduledTask[$qRunTask];
			];
		];
		
		$NumberOfMonitorQueueCalls
	);
	
InitSingleton[$AllowAsynchronousEvaluation,True];
	
SetAttributes[AsyncEvaluate,HoldFirst];
Options[AsyncEvaluate] = 
	Join[
		{
			"AsynchronousCall"->True,
			"Callback"->Identity,
			"MonitorQueueInterval"->1,
			"Priority"->Null,
			"ParallelKernel"->None,
			"BlockMainKernel"->False
		}
		,
		Options[ExecuteWhen]
	];
	
AsyncEvaluate[parallelAction_,opts:OptionsPattern[]]:=
	Module[{evaluatedFunction},
		evaluatedFunction =
			If[OptionValue@"ParallelKernel"===None,
				asyncEvaluate, 
				asynchronousEvaluateOnKernel 
			];
			
		evaluatedFunction[parallelAction,opts]
	];
	
SetAttributes[asyncEvaluate,HoldFirst];
asyncEvaluate[parallelAction_,opts:OptionsPattern[AsyncEvaluate]]:=
	Module[{pid, resultCallback,executeWhenOptions,monitorQueueInterval}, 
		
		resultCallback = OptionValue[AsyncEvaluate,{opts},"Callback"];
		
		If[OptionValue[AsyncEvaluate,{opts},"AsynchronousCall"] && TrueQ@$AllowAsynchronousEvaluation && Kernels[]=!={},
			
			pid = ParallelSubmit[parallelAction,"Scheduling"->OptionValue@"Priority"];
			
			(*MonitorQueue is useful for shared functions and variables called from a parallel kernel*)
			If[(monitorQueueInterval = OptionValue[AsyncEvaluate,{opts},"MonitorQueueInterval"]) > 0.,
				MonitorQueue[monitorQueueInterval];
			];
			
			executeWhenOptions = FilterRules[{opts},Options[ExecuteWhen]]; 
				
			WithValues[{pid,resultCallback,executeWhenOptions},
				ExecuteWhen[
					MatchQ[Parallel`Developer`ProcessState[pid],Parallel`Developer`finished[_]]
					,
					StopMonitorQueue[];
					resultCallback[First@Parallel`Developer`ProcessResult[pid]]
					,
					executeWhenOptions
				];
			];
			
			(*we return the pid so that other functions can also monitor this evaluation*)
			pid
			,
			resultCallback[parallelAction]
		]
	];
	
(*tutorial/CallingSubsidiaryWolframSystemProcesses*)
CreateNewLink[]:=
	Module[{link},
		link=LinkLaunch@"mathkernel -mathlink";
		LinkRead[link]; (*Return the first input line*)
		
		link
	];
	
SetAttributes[SendOnLink,HoldRest];
SendOnLink[link_,expression_]:=LinkWrite[link,Unevaluated[expression]];

ReadFromLink[link_]:=
	If[LinkReadyQ@link,
		LinkRead[link]//First
		,
		Missing["LinkNotReady"]
	];
	
DrainLink[link_LinkObject] := Reap[While[LinkReadyQ[link], Sow[LinkRead[link]]]];

SetAttributes[EvaluateOnLink,HoldRest];
EvaluateOnLink[link_,expression_]:=
	(
		SendOnLink[link,expression];
		DrainLink[link]
	);

InitSingleton[$ReservedKernels,Association[]];
InitSingleton[$NumberOfMonitoredCallsOnKernel,Association[]];
InitSingleton[$CheckIntervalPerKernel,Association[]];
InitSingleton[$KernelCallback,Association[]];
InitSingleton[$MonitorKernelTask,Association[]];
InitSingleton[$EvaluationIdOnKernel,0];
	
ReserveKernels::err = "Not enough kernels available.";
ReserveKernels[kernelNames_]:=
	Module[{reservedKernels,nReservedKernels=Length@kernelNames},
		
		If[Length@Kernels[] >= nReservedKernels,
			
			(*after this we can still use ParallelEvaluate, but reserved kernels won't be impacted
			by calls to ParallelSubmit, they will always be available through KernelEvaluate or KernelSubmit*)
			reservedKernels = Parallel`Protected`$sortedkernels[[;;nReservedKernels]];
			Parallel`Protected`$sortedkernels = Parallel`Protected`$sortedkernels[[nReservedKernels+1;;]];
			
(*			reservedKernels = Parallel`Protected`$kernels[[;;nReservedKernels]];
			Parallel`Protected`$kernels = Parallel`Protected`$kernels[[nReservedKernels+1;;]];
			Parallel`Kernels`Private`sortkernels[];*)
			
			MapThread[($ReservedKernels[#1]=Parallel`Developer`KernelID@#2)&,{kernelNames,reservedKernels}];
			
			(
				$NumberOfMonitoredCallsOnKernel[#] = 0;
				$CheckIntervalPerKernel[#] = 1;
			)& /@ kernelNames;
			,
			Message[ReserveKernels::err];
			$Failed
		]
	];
	
IsKernelReserved[kernelName_]:=KeyExistsQ[$ReservedKernels,kernelName];

GetNamedKernel[kernelName_]:=
	If[IsKernelReserved@kernelName,
		Parallel`Developer`KernelFromID@$ReservedKernels@kernelName
		,
		Missing["RervedKernel"]
	];
	
SetAttributes[KernelSubmit,HoldAll];
KernelSubmit::notAvailable = "Kernel `1` is not available. Evaluating on main kernel.";
KernelSubmit[kernels_List,expr_]:=KernelSubmit[#,expr]&/@kernels;
KernelSubmit[kernelName_String,expr_]:=
	If[IsKernelReserved@kernelName,
		Parallel`Developer`Send[
			GetNamedKernel@kernelName,
			expr
		];
		,
		Message[KernelSubmit::notAvailable,kernelName];
		expr
	];
KernelSubmit[kernel_,expr_]:= 
	If[MemberQ[Kernels[],kernel],
		Parallel`Developer`Send[kernel,expr];
		,
		Message[KernelSubmit::notAvailable,kernel];
		expr
	];

KernelSubmit[expr_]:=KernelSubmitExceptOn[{},expr];

SetAttributes[KernelSubmitExceptOn,HoldRest];	
KernelSubmitExceptOn[kernelNames_,expr_]:=
	Module[{kernels},
		kernels = Complement[Kernels[],GetNamedKernel /@ kernelNames // DeleteCases[_Missing]];
		Parallel`Developer`Send[#,expr]& /@ kernels;
	];
	
ReceiveFromKernel[kernelName_]:= 
	If[IsKernelReserved@kernelName,
		Parallel`Developer`Receive@GetNamedKernel@kernelName;
	];

ReceiveFromKernelIfReady[kernelName_]:= 
	If[IsKernelReserved@kernelName,
		Quiet@Parallel`Developer`ReceiveIfReady@GetNamedKernel@kernelName
	];

IsKernelAvailable[kernelName_]:= 
	IsKernelReserved@kernelName && ReceiveFromKernelIfReady@kernelName === $Failed;
	
DrainKernelResults[kernelName_]:= 		
	While[True,
		If[IsKernelAvailable@kernelName,
			Break[];
		];
	];
	
(*Useful for retrieving results from a kernel*)
SetAttributes[KernelEvaluate,HoldRest];
KernelEvaluate::notAvailable = "Kernel `1` is not available. Evaluating on main kernel.";
KernelEvaluate[kernelName_String,expr_]:=
	If[IsKernelReserved@kernelName,
		(*In order to receive any useless result from previous parallel evaluations,
		caused from using KernelSubmit but not doing any Receive afterwards, we can receive Null for example*)
		DrainKernelResults[kernelName];
		
		ParallelEvaluate[
			expr,
			$ReservedKernels@kernelName
		]
		,
		Message[KernelEvaluate::notAvailable,kernelName];
		expr
	];
KernelEvaluate[kernelNames_List,expr_]:=
	If[AllTrue[kernelNames,IsKernelReserved],
		While[True,
			If[IsKernelAvailable@# & /@ kernelNames // Apply[And],
				Break[];
			];
		];
		
		ParallelEvaluate[
			expr,
			$ReservedKernels /@ kernelNames
		]
		,
		Message[KernelEvaluate::notAvailable,kernelNames];
		expr
	];

SetAttributes[ParallelEvaluateExceptOn,HoldRest];	
ParallelEvaluateExceptOn[kernelNames_,expr_]:=
	Module[{kernelIds},
		
		kernelIds = 
			Complement[
				Parallel`Developer`KernelID /@ Kernels[]
				,
				$ReservedKernels /@ kernelNames // DeleteCases[_Missing]
			];
		
		While[True,
			If[
				MatchQ[
					Quiet@Parallel`Developer`ReceiveIfReady@Parallel`Developer`KernelFromID@#
					,
					Parallel`Developer`$NotReady|$Failed
				]& /@ kernelIds // Apply[And],
				
				Break[];
			];
		];
		
		ParallelEvaluate[expr,kernelIds]
	];

MonitorKernelTask[kernelName_]:=
	Module[{result},
		result = ReceiveFromKernelIfReady@kernelName;
		
		Which[
			result === $Failed,
				StopMonitorKernel[kernelName];
			,
			Head@result === KernelResult,
				StopMonitorKernel[kernelName];
				$KernelCallback[First@result][Last@result];
				$KernelCallback[First@result]=.;
		];
	];

MonitorKernel[kernelName_,checkInterval_:1]:=
	If[IsKernelReserved@kernelName,
		
		If[$NumberOfMonitoredCallsOnKernel[kernelName] == 0 || checkInterval<$CheckIntervalPerKernel[kernelName],
			
			$CheckIntervalPerKernel[kernelName] = checkInterval;
			
			If[KeyExistsQ[$MonitorKernelTask,kernelName],
				ResetScheduledTask[$MonitorKernelTask[kernelName],$CheckIntervalPerKernel[kernelName]];
				,
				$MonitorKernelTask[kernelName] = 
					CreateScheduledTask[
						MonitorKernelTask[kernelName]
						,
						$CheckIntervalPerKernel[kernelName]
					];
			];
		];
		
		(*a task can terminate before we start monitor task, therefore the order
		of the following instructions*)
		++ $NumberOfMonitoredCallsOnKernel[kernelName];
		
		MonitorKernelTask[kernelName];
		
		If[$NumberOfMonitoredCallsOnKernel[kernelName] > 0,
			StartScheduledTask@$MonitorKernelTask[kernelName];	
		];
		
		$NumberOfMonitoredCallsOnKernel[kernelName]
		,
		Missing["ReservedKernel"]
	]; 
	
StopMonitorKernel[kernelName_]:=
	If[IsKernelReserved@kernelName,
		If[$NumberOfMonitoredCallsOnKernel[kernelName]>0,
		
			$NumberOfMonitoredCallsOnKernel[kernelName]--;
			
			If[$NumberOfMonitoredCallsOnKernel[kernelName] == 0,
				StopScheduledTask[$MonitorKernelTask[kernelName]];
			];
		];
	
		$NumberOfMonitoredCallsOnKernel[kernelName]
		,
		Missing["ReservedKernel"]
	];
	
SetAttributes[asynchronousEvaluateOnKernel,HoldFirst];
asynchronousEvaluateOnKernel[parallelAction_,opts:OptionsPattern[AsyncEvaluate]]:=
	Module[{resultCallback,monitorQueueInterval,evaluationId,kernelName}, 
		
		kernelName = OptionValue[AsyncEvaluate,{opts},"ParallelKernel"];
		resultCallback = OptionValue[AsyncEvaluate,{opts},"Callback"];
		
		If[OptionValue[AsyncEvaluate,{opts},"AsynchronousCall"] && Kernels[] =!= {} && TrueQ@$AllowAsynchronousEvaluation &&
		   IsKernelReserved@kernelName,
			
			If[!OptionValue[AsyncEvaluate,{opts},"BlockMainKernel"],
				evaluationId = $EvaluationIdOnKernel++;
				
				WithValues[{kernelName,evaluationId},
					$KernelCallback[evaluationId]=resultCallback;
					KernelSubmit[kernelName,KernelResult[evaluationId,parallelAction]];
				];
				
				(*MonitorQueue is useful for shared functions and variables called from a parallel kernel*)
				If[(monitorQueueInterval = OptionValue[AsyncEvaluate,{opts},"MonitorQueueInterval"]) > 0.,
					MonitorKernel[kernelName,monitorQueueInterval];
				];
				
				evaluationId
				,
				resultCallback@KernelEvaluate[kernelName,parallelAction]
			]
			,
			resultCallback[parallelAction]
		]
	];
	
Options[InitKernels] = {"NKernels"->0,"ReservedKernels"->None,"KernelInitFunction"->None,"EnsureParallelSubmitKernel"->False};
InitKernels[opts:OptionsPattern[]]:=
	Module[{commandLine,nKernels,reservedKernels,kernelInitFunction,usedNKernels,ensureKernel},
		
		If[Kernels[] === {},
			
			{nKernels,reservedKernels,kernelInitFunction,ensureKernel} = OptionValue[InitKernels,{opts},#]& /@ Options[InitKernels][[All,1]];
			
			Needs["JLink`"];
			commandLine = JLink`InstallJava[] // First;
			
			If[kernelInitFunction =!= None,
				kernelInitFunction[];
			];
			
			If[nKernels > 0,
				
				CloseKernels[];
				
				usedNKernels = nKernels;
				
				(*We ensure that at least one kernel is available to ParallelSubmit*)
				If[ensureKernel,
					usedNKernels = Max[usedNKernels,If[reservedKernels===None,0,Length@reservedKernels+1]]
				];
				
				Quiet[
					LaunchKernels[usedNKernels]
					,
					{Get::noopen}
				];
				
				ParallelNeeds["JLink`"];
				With[{commandLine = commandLine},
					ParallelEvaluate@JLink`InstallJava[JLink`CommandLine->commandLine];
				];
					
				If[kernelInitFunction =!= None,
					With[{kernelInitFunction=kernelInitFunction},
						ParallelEvaluate[kernelInitFunction[]];
					];
				];
				
				If[reservedKernels =!= None,
					ReserveKernels[reservedKernels];
				];
			];
		];
	];

End[] (* End Private Context *)

EndPackage[]