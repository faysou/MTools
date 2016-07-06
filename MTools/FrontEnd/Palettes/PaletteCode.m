(* Mathematica package *)

CreatePalette[
	Column[
		{
			Button["Needs[\"MTools`\"]",Needs["MTools`"], Appearance -> "Palette"]
			,
			Button[Defer[InterpretSymbol[\[SelectionPlaceholder]]], Inherited, BaseStyle -> "Evaluate", Evaluator -> None, Appearance -> "Palette"]
			,
			Button[Defer[UninterpretSymbol[\[SelectionPlaceholder]]], Inherited, BaseStyle -> "Evaluate", Evaluator -> None, Appearance -> "Palette"]
		}
	]
	, 
	WindowTitle -> "MTools"
]
