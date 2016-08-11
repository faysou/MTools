(* ::Package:: *)

Get["https://raw.githubusercontent.com/jkuczm/MathematicaBootstrapInstaller/v0.1.1/BootstrapInstaller.m"]


BootstrapInstall[
	"MTools",
	"https://github.com/faysou/MTools/releases/download/0.3.0/MTools.zip",
	"AdditionalFailureMessage" ->
		Sequence[
			"You can ",
			Hyperlink[
				"install the MTools package manually",
				"https://github.com/faysou/MTools#manual-installation"
			],
			"."
		]
]