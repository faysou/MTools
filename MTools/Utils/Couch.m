(* Mathematica Package *)

BeginPackage["MTools`Utils`Couch`",
	{
		"JLink`",
		"MTools`Utils`Utils`"
	}
]
(* Exported symbols added here with SymbolName::usage *)

$DefaultCluster={};
$DefaultBucket="";
$DefaultPassword="";

InitSingleton[$Clusters,Association[]];
InitSingleton[$Buckets,Association[]];

BucketExists::usage = "BucketExists[bucket]";
ClusterExists::usage = "ClusterExists[cluster]";
CouchConnect::usage = "CouchConnect[opts]";
CouchQuery::usage = "CouchQuery[statement,opts]";
GetCouchId::usage = "GetCouchId[prefix:None]";
CouchInsert::usage = "CouchInsert[id,document,opts]";
CouchCreatePrimaryIndex::usage = "CouchCreatePrimaryIndex[opts]";
CouchCreateIndex::usage = "CouchCreateIndex[indexName,indexKeys,opts]";

CouchDateStringFormat::usage = "CouchDateStringFormat[date]";
CouchDateString::usage = "CouchDateString[date:Automatic]";
DateToCouchDate::usage = "DateToCouchDate[date]";
CouchDate::usage = "CouchDate[date:Automatic]";
CouchDateStringToDate::usage = "CouchDateStringToDate[date]";
CouchDateToDate::usage = "CouchDateToDate[date]";
FormatCouchDate::usage = "FormatCouchDate[date]";
CouchDisconnect::usage = "CouchDisconnect[opts]";
CouchDropIndex::usage = "CouchDropIndex[indexName,opts]";

Begin["`Private`"] (* Begin Private Context *) 

BucketExists[{cluster_,bucket_}]:= KeyExistsQ[$Buckets,{cluster,bucket}] && JavaObjectQ@$Buckets[{cluster,bucket}];
ClusterExists[cluster_]:= KeyExistsQ[$Clusters,cluster] && JavaObjectQ@$Clusters[cluster];
	
CouchConnect::clusterErr = "Connection to the cluster `1` couldn't be established.";
CouchConnect::bucketErr = "Connection to the bucket `1` couldn't be established.";
Options[CouchConnect]={"Cluster":>$DefaultCluster,"Bucket":>$DefaultBucket,"Password":>$DefaultPassword,"MaxConnectionAttempts"->5,"ConnectionTimeout"->11};
CouchConnect[opts:OptionsPattern[]]:=
	Module[{bucket,cluster,password,maxConnectionAttempts,(*env,*)connectionTimeout},
		
		{cluster,bucket,connectionTimeout} = OptionValue[CouchConnect,{opts},#]& /@ {"Cluster","Bucket","ConnectionTimeout"};
		
		If[!BucketExists[{cluster,bucket}],
			
			If[!ClusterExists[cluster],
				
				If[!StringContainsQ[ToString@JLink`$ExtraClassPath,"Couchbase"],
					InitJavaKernels[];
				];
				
				maxConnectionAttempts = OptionValue@"MaxConnectionAttempts";
				
				LoadJavaClass["com.couchbase.client.java.CouchbaseCluster"];
(*				LoadJavaClass["com.couchbase.client.core.env.DefaultCoreEnvironment"];
				env = DefaultCouchbaseEnvironment`builder[]@connectTimeout[connectionTimeout*1000//Round]@build[];*)
								
				(*we load classes from other jars in order to decrease the probability of connection failure*)
(*				LoadJavaClass["rx.observers.Observers"];
				LoadJavaClass["com.couchbase.client.core.CouchbaseCore"];*)
				
				DoWhile[
					$Clusters[cluster]= Quiet@TimeConstrained[CouchbaseCluster`create[cluster],connectionTimeout];
					maxConnectionAttempts--;
					,
					!JavaObjectQ@$Clusters[cluster] || maxConnectionAttempts == 0
				];
				
				If[!JavaObjectQ@$Clusters[cluster],
					$Clusters[cluster] = .;
				];
			];
			
			If[ClusterExists[cluster],
				
				password = OptionValue@"Password";
				maxConnectionAttempts = OptionValue@"MaxConnectionAttempts";
				
				DoWhile[
					If[$DefaultPassword === None,
						$Buckets[{cluster,bucket}] = Quiet@TimeConstrained[$Clusters[cluster]@openBucket[bucket],connectionTimeout];
						,
						$Buckets[{cluster,bucket}] = Quiet@TimeConstrained[$Clusters[cluster]@openBucket[bucket,password],connectionTimeout];
					];
					maxConnectionAttempts--;
					,
					!JavaObjectQ@$Buckets[{cluster,bucket}] || maxConnectionAttempts == 0
				];
				
				If[JavaObjectQ@$Buckets[{cluster,bucket}],
					$Buckets[{cluster,bucket}]
					,
					Message[CouchConnect::bucketErr,bucket];
					$Buckets[{cluster,bucket}] = .;
					Abort[]
				]
				,
				Message[CouchConnect::clusterErr,cluster];
				Abort[]
			]
			,
			$Buckets[{cluster,bucket}]
		]
	];
	
CouchDisconnect[OptionsPattern[CouchConnect]]:=
	Module[{cluster},
		
		cluster = OptionValue@"Cluster";
		
		If[ClusterExists[cluster],
			$Clusters[cluster]@disconnect[];
			$Clusters[cluster] = .;
			DeleteKeys[$Buckets,{cluster,_}];
		]
	];
	
InitSingleton[$couchId,0];
GetCouchId[prefix_:None]:=If[prefix===None,"",prefix~~":"]~~DateString[{"YearShort","Month","Hour","Minute"}]~~ToString@$KernelID~~ToString[$couchId++];
	
(*http://docs.couchbase.com/sdk-api/couchbase-java-client-2.2.2/
http://developer.couchbase.com/documentation/server/4.0/n1ql/index.html*)
Options[CouchQuery] = Join[Options[CouchConnect],{"ParseResults"->True,"ParseErrors"->False,"ReturnJavaResult"->False,"AdhocQuery"->True}];
CouchQuery[inputQuery_,opts:OptionsPattern[]]:=
	Module[{result,resultIterator,bucket,errorIterator,mmResult=Association[],adhocQuery=OptionValue@"AdhocQuery"},
		
		bucket = CouchConnect[FilterRules[{opts},Options@CouchConnect]];
		
		LoadJavaClass["com.couchbase.client.java.query.N1qlQuery"];
		
		Check[
			(*http://blog.couchbase.com/2015/december/prepare-for-performance*)
			If[!adhocQuery,
				LoadJavaClass["com.couchbase.client.java.query.N1qlParams"];
				result = bucket@query[N1qlQuery`simple[inputQuery,N1qlParams`build[]@adhoc[False]]];
				,
				result = bucket@query[N1qlQuery`simple[inputQuery]];
			];
			
			mmResult["HasError"] = !result@parseSuccess[];
			
			If[OptionValue@"ParseErrors",	
				errorIterator = result@errors[]@iterator[];
					
				mmResult["Errors"] =
					NestWhileList[
						(errorIterator@next[]@toString[] // ImportString[#,"RawJSON"] & // First) &
						, 
						{}
						, 
						(errorIterator@hasNext[])&
					] // If[# =!= {},Rest@#,#]&;
			];
			
			If[!mmResult["HasError"] && OptionValue@"ParseResults",
				resultIterator = result@iterator[];
				
				mmResult["Result"] =
					NestWhileList[
						(resultIterator@next[]@value[]@toString[]//ImportString[#,"RawJSON"]&//First)&
						,
						{}
						,
						(resultIterator@hasNext[])&
					]//If[#=!={},Rest@#,#]&;
			];
			
			If[OptionValue@"ReturnJavaResult",			
				mmResult["JavaResult"] = result;
			];
			,
			mmResult["HasError"] = True;
		];
		
		mmResult
	];

Options[CouchInsert] = Join[Options[CouchQuery],{"Id"->Automatic,"Prefix"->None,"Upsert"->False}];	
CouchInsert[documents_List,opts:OptionsPattern[]]:= CouchInsert[#,opts]& /@ documents;
CouchInsert[document_Association,opts:OptionsPattern[]]:=
	Module[{statement,id},
		
		statement= 
			GetQuery[
				If[OptionValue@"Upsert","UPSERT","INSERT"]~~" INTO `Bucket` (KEY,VALUE) VALUES (`Id`,`Doc`)"
				,
				{
					"Bucket"->OptionValue@"Bucket",
					"Id"->
						Quote@GetCouchId[
							If[(id = OptionValue@"Id") === Automatic || !KeyExistsQ[document,id],
								OptionValue@"Prefix"
								,
								document[id]
							]
						],
					"Doc"->AssociationToJson@document
				}
			];
		
		CouchQuery[statement,FilterRules[{opts},Options@CouchQuery]]
	];
	
Options@CouchCreatePrimaryIndex = Options@CouchQuery;
CouchCreatePrimaryIndex[opts:OptionsPattern[]]:=
	Module[{statement},
		
		statement=
			GetQuery[
				"CREATE PRIMARY INDEX `b``Bucket`-primary-index`b` ON `b``Bucket``b` USING GSI"
				,
				{
					"Bucket"->OptionValue@"Bucket",
					"b"->$BackQuote
				}
			];
		
		CouchQuery[statement,FilterRules[{opts},Options@CouchQuery]]
	];
	
Options@CouchCreateIndex = Join[Options@CouchQuery,{"WhereClause"->"","DropIndex"->True}];
CouchCreateIndex[indexName_,indexKeys_,opts:OptionsPattern[]]:=
	Module[{statement,whereClause},
		
		If[OptionValue@"DropIndex",
			CouchDropIndex[indexName,FilterRules[{opts},Options@CouchDropIndex]];
		];
		
		statement=
			GetQuery[
				"CREATE INDEX `b``IndexName``b` ON `b``Bucket``b`(`Keys`)"
				,
				{
					"IndexName"->indexName,
					"Bucket"->OptionValue@"Bucket",
					"Keys"->ListToString@indexKeys,
					"b"->$BackQuote
				}
			]~~
			If[(whereClause = OptionValue@"WhereClause") != "",
				"WHERE "~~whereClause
				,
				""
			]~~
			" USING GSI";
				
		CouchQuery[statement,FilterRules[{opts},Options@CouchQuery]]
	];
	
Options@CouchDropIndex = Options@CouchQuery;
CouchDropIndex[indexName_,opts:OptionsPattern[]]:=
	Module[{statement},
		
		statement=
			GetQuery[
				"DROP INDEX `b``Bucket``b`.`b``IndexName``b` USING GSI"
				,
				{
					"IndexName"->indexName,
					"Bucket"->OptionValue@"Bucket",
					"b"->$BackQuote
				}
			];
				
		CouchQuery[statement,opts]
	];

couchStringFormat = {"Year","-","Month","-","Day","T","Hour",":","Minute",":","Second",".","Millisecond"};
CouchDateStringFormat[date_]:=DateString[date,couchStringFormat];
CouchDateString[date_:Automatic]:= date /. Automatic->DateList[] // CouchDateStringFormat;
DateToCouchDate[date_]:=date// CouchDateString;
CouchDate[date_:Automatic]:=CouchDateString[date];
CouchDateStringToDate[date_]:=DateList[{date,couchStringFormat}]//DateString;
CouchDateToDate[date_]:=date//CouchDateStringToDate;
FormatCouchDate[date_,shortVersion_:False]:=
	If[!shortVersion,
		StringReplace[date,{"T"->" @ ","."~~x__->""}]
		,
		CouchDateToDate@date // DateString[#,{"DayShort", "-", "MonthShort", "@", "HourShort", ":", "MinuteShort"}]&
	];

End[] (* End Private Context *)

EndPackage[]
