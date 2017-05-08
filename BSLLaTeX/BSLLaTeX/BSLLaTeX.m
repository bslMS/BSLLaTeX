(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 27.04.2017 *)

(* :Title: BSLLaTeX *)
(* :Author: Guido Wolf Reichert <gwr@bsl-support.de> *)
(* :Context: BSLLaTeX` *)
(* :Version: 0.0.1 *)
(* :Date: 2017-04-27 *)
(* :Mathematica Version: 11.1 *)
(* :Copyright: (c) 2017 Guido W. Reichert *)

BeginPackage["BSLLaTeX`", { "MaTeX`" }] (* MaTeX may not be really needed, keeping making this independent *)
(* Exported symbols added here with SymbolName::usage *) 

LaTeXFigure::usage = "\
LaTeXFigure[file, caption, shortcap] generates the code for a figure environent."

LaTeXTable::usage = "\
LaTeXTable[content, caption, shortcap] generates the code for a table environment."

$LaTeXRules::usage = "\
$LaTeXRules give some convenient string replacement rules that allow for an immediate use of text input."

LaTeXTabulary::usage = "\
LaTeXTabulary[assoc] \n\
LaTeXTabulary[{assoc1,\[TripleDot]}] \n\
LaTeXTabulary[\[LeftAssociation]key1 \[Rule] assoc1, \[TripleDot] \[RightAssociation]] create a tabulary environment."

LaTeXTableForm::usage = "\
LaTeXTableForm[strArray] will return the array of strings in a form that can be immediately plugged into a tabular environment."

LaTeXCompile::usage = "\
LaTeXCompile[filename] runs pdflatex to compile the texfile."

Slots::usage = "\
Slots[template] will return the named slots of a template object."

Begin["`Private`"]
(* Implementation of the package *)

$CharacterEncoding = "UTF8"

$LaTeXRules = {
	FromCharacterCode[8222]		-> "\\glqq ",
	FromCharacterCode[8220]		-> "\\grqq{}",
	"%"							-> "\\%",
	"#"							-> "\\#",
	"&"							-> "\\&",
	"_"							-> "\\_",
	" x "						-> " $\\times$ ",
	"->"						-> "$\\rightarrow$",
	"=>"						-> "$\\implies$"
}

(* expand TemplateObject *)
Unprotect[ System`TemplateObject ]

System`TemplateObject/: Slots[ template_System`TemplateObject ] := Cases[ template, TemplateSlot[ name_ ] :> name, Infinity ]

Protect[ System`TemplateObject ]

Options[ LaTeXFigure ] = {
	"FigurePlacement" 	-> "htb",
	"FigureWidth" 		-> "0.9 \\textwidth",
	"Interpolate" 		-> True, (* useful for bitmaps to prevent pixels from showing *)
	"Prefix"			-> None,
	"Suffix"			-> None
}

LaTeXFigure[ file_String, caption_String, shortcap_String, opts:OptionsPattern[] ] := With[
	{
		parts = Association[
			"placement" 	-> OptionValue[ "FigurePlacement" ],
			"width" 		-> OptionValue[ "FigureWidth" ],
			"file"			-> file,
			"shortcaption" 	-> shortcap,
			"caption"		-> caption,
			"interpolate"	-> If[ OptionValue[ "Interpolate" ] === True, ", interpolate = true", "" ],
			"prefix" 		-> Switch[ OptionValue[ "Prefix" ], _String , OptionValue[ "Prefix" ], _ , "" ],
			"suffix" 		-> Switch[ OptionValue[ "Suffix" ], _String , OptionValue[ "Suffix" ], _ , "" ]
		]
	},
	TemplateApply[
    	StringJoin[
            "`prefix`\n",
            "\\begin{figure}[`placement`]\n",
            "\\centering\n",
            "\\includegraphics[width = `width``interpolate`]{`file`}\n",
            "\\caption[`shortcaption`]{`caption`}\n",
            "\\label{fig:`shortcaption`}\n",
            "\\end{figure}\n",
            "`suffix`\n\n"
        ],
        parts
    ]  	
]

LaTeXFigure[ ___ ] := Return[ Message[ LaTeXFigure::fargs ] ; $Failed ]

Options[ LaTeXTable ] = {
	"TablePlacement" 	-> "htb",
	"Prefix"			-> None,
	"Suffix"			-> None
}

LaTeXTable[ content_String, caption_String, shortcap_String, opts:OptionsPattern[] ] := With[
	{
		parts = Association[
			"placement" 	-> OptionValue[ "TablePlacement" ],
			"content"		-> content,
			"shortcaption" 	-> shortcap,
			"caption"		-> caption,
			"prefix" 		-> Switch[ OptionValue[ "Prefix" ], _String , OptionValue[ "Prefix" ], _ , "" ],
			"suffix" 		-> Switch[ OptionValue[ "Suffix" ], _String , OptionValue[ "Suffix" ], _ , "" ]
		]
	},
	TemplateApply[
    	StringJoin[
        	"`prefix`\n",
            "\\begin{table}[`placement`]\n",
            "\\centering\n",
            "\\caption[`shortcaption`]{`caption`}\n",
            "\\label{tab:`shortcaption`}\n",
            "`content`",               
            "\\end{table}\n",
            "`suffix`\n\n"
        ],
        parts
    ]  	
]

LaTeXTable[ ___ ] := Return[ Message[ LaTeXTable::fargs ] ; $Failed ]

Options[ LaTeXTableForm ] = {
	"StringStyle" -> "`1`" (* use placeholder `` to indicate any string, e.g. "\\textbf{`1`}" *) 
}

LaTeXTableForm[ array_, opts:OptionsPattern[] ] /; ArrayQ[ array, 2 , StringQ ] := With[
	{
		formattedArray = Map[
			TemplateApply[
				OptionValue[ "StringStyle" ],
				{ # } 
			]&,
			array,
			{2}
		]
	},
	StringJoin[
		StringRiffle[ formattedArray, " \\\\ \n", " & " ],
		" \\\\ \n"
	]
]

LaTeXTableForm[ assoc_Association, opts:OptionsPattern[] ] := LaTeXTableForm[ Values @ { assoc }, opts ]

LaTeXTableForm[ list_, opts:OptionsPattern[] ] /; VectorQ[ list, StringQ ] := LaTeXTableForm[ { list }, opts ]

LaTeXTableForm[ ___ ] := Return[ Message[ LaTeXTableForm::fargs ] ; $Failed ]



Options[ LaTeXTabulary ] = {
	"TableSpec" -> Automatic , (* or a sequence of LCRJ-alignment letters *)
	"TableWidth" -> "0.9\\textwidth" ,
	"MinimumLength" -> None , (* \tymin *)
	"MaximumLength" -> None , (* \tymax *)
	"HeaderStyle"	-> "\\textbf{`1`}",
	"BodyStyle"		-> "`1`"		
}

LaTeXTabulary[ assoc : { __Association }, opts:OptionsPattern[] ] /; ArrayQ @ assoc := With[
	{
		keys = LaTeXTableForm[ Keys @ assoc[[1]], "StringStyle" -> OptionValue[ "HeaderStyle" ] ],
		values = LaTeXTableForm[ Values @ assoc , "StringStyle" -> OptionValue[ "BodyStyle" ] ],
		
		(* make left alignment a default *)
		tableSpec = Switch[ OptionValue["TableSpec"], 
	   		_String, 
	   			OptionValue["TableSpec"], 
	   		_ ,
	   			ConstantArray[ "L", Length @ First @ assoc ] // StringJoin
	   	],
	   	tableWidth = OptionValue[ "TableWidth" ],
	   	tymin = Switch[ OptionValue[ "MinimumLength" ],
	   		_String, 
	   			"\\tymin = " <> OptionValue[ "MinimumLength" ] <> "\n",
	   		_ , 
	   			""
	   	],
	   	tymax = Switch[ OptionValue[ "MaximumLength" ],
	   		_String, 
	   			"\\tymax = " <> OptionValue[ "MaximumLength" ] <> "\n",
	   		_ , 
	   			""
	   	]
	},
	Module[
		{
			parts = Association[
				"tablewidth" 	-> tableWidth ,
				"tablespec" 	-> tableSpec ,
				"tymin"			-> tymin,
				"tymax"			-> tymax,
				"header"		-> keys,
				"body"			-> values
			]
		},
		TemplateApply[
    		StringJoin[
            		"`tymin`",
            		"`tymax`",
                	"\\begin{tabulary}{`tablewidth`}{`tablespec`}\n",
                	"\\toprule\n",
                	"`header`",
                	"\\midrule\n",
                	"`body`",
                	"\\bottomrule\n",
                	"\\end{tabulary}\n"
    		],
    		parts
    	]  
	]
]

LaTeXTabulary[ assoc_Association, opts:OptionsPattern[] ] := LaTeXTabulary[ { assoc }, opts ]

LaTeXTabulary[ ___ ] := Return[ Message[ LaTeXTabulary::fargs ] ; $Failed ]

LaTeXCompile[ file_String ] := Module[
	{
		resetDirectoryQ = True,
		fileName = FileNameTake @ file,
		directoryName = DirectoryName @ file 
	},
	
	(* check whether the directory needs to be changed *)
	Which[ 
		directoryName === "" && FileExistsQ @ fileName , 
			resetDirectoryQ = False, 
		FileExistsQ @ file == False,
			Return[ Message[ LaTeXCompile::fdnfnd, file ]; $Failed ],
		True,
			resetDirectoryQ = True;
			SetDirectory[ directoryName ]
	];
	
	(* run pdflatex - assuming that it has been installed properly for now *)
	Run[ "pdflatex " <> fileName ];
	
	(* reset the directory if needed *)
	If[ resetDirectoryQ,
		(* then *) 
		ResetDirectory[]
	];
	
	Print @ StringJoin[
		FileBaseName @ file,
		".pdf"
	]
]	

LaTeXCompile[ ___ ] := Return[ Message[ LaTeXCompile::fargs ] ; $Failed ]

End[]

EndPackage[]

