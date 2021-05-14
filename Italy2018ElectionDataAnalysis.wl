(* ::Package:: *)

(* :Title : Italy 2018 Election Data Analysis *)
(* :Context : Italy 2018 Election Data Analysis *)
(* :Author : ConteTer (Marco Ferrati, Michele Perlino, Tommaso Azzalin, Vittoria Conte) *)
(* :Summary: Package for analysing the 2018 Italian political election data published by the the Ministry of the Interior. *)
(* :Copyright : MIT *)
(* :Package Version : 1 *)
(* :Mathematica Versionfile : 12.2*)
(* :History : *)
(* :Keywords : italy, election, 2018, data, analysis*)
(* :Sources : https://github.com/jjocram/italy-2018-election-data-mathematica *)
(* :Discussion : *)
BeginPackage["Italy2018ElectionDataAnalysis`"]

PlottingElectionElectorsPie::usage = "PlottingElectionElectorsPie[chamber, region: Null, province: Null, district: Null, query: Null] returns a list of data to plot the electors pie chart."
PlottingElectionVotersPie::usage = "PlottingElectionVotersPie[chamber, region: Null, province: Null, district: Null, query: Null] returns a list of data to plot the voters pie chart."
PlottingElectionVotersNonVotersPie::usage = "PlottingElectionVotersNonVotersPie[chamber, region: Null, province: Null, district: Null, query: Null] returns a list of data to plot the voters and non voters pie chart."
PlottingElectionRegionCoalitionsBars::usage = "PlottingElectionRegionCoalitionsBars[chamber, coalition, region: Null, province: Null, district: Null, query: Null] returns a list of data to plot in each region the winning coalition."
PlottingCandidate::usage = "PlottingCandidate[name, surname, city: Null] returns a list of data to plot the histogram for the candidate."
GetChamber::usage = "DEV TOOL GetChamber[] return the chamber dataset"
GetRegions::usage = "GetRegions[] return the list of regions used in this package"

Begin["`Private`"]
	
	(* Initializes a dataset for a given year and for a given chamber, downloading it if it does not exist, reading it from the cache otherwise. *)
	InitDataset[year_, chamber_] :=
		Module[{file, readString, importedDS, cache, url},
			readString = "";
			cache = datasets[[year]][[chamber]][["cache"]];
			url = datasets[[year]][[chamber]][["url"]];
			If[
				FileExistsQ[cache],
				(
					readString = ReadString[cache];
					importedDS = ImportString[readString, "Dataset", HeaderLines->1];
				),
				(
					file = URLDownload[url];
					readString = StringReplace[ReadString[file], ";"->","];
					importedDS = ImportString[readString, "Dataset", HeaderLines->1];
					Export[cache, importedDS, "CSV"];
				)
			];
			
			Return[importedDS];
		]
	
	(* Collection of dataset references (for future updates). *)
	datasets = Association[
		"2018" -> Association[
			"chamberofdeputies" -> Association[
				"url" -> "https://dait.interno.gov.it/documenti/camera_2018_scrutini_italia.csv",
				"cache" -> "camera_2018_scrutini_italia.csv"
			],
			"senateoftherepublic" -> Association[
				"url" -> "https://dait.interno.gov.it/documenti/senato_2018_scrutini_italia.csv",
				"cache" -> "senato_2018_scrutini_italia.csv"
			]
		]
	];
	
	(* Original election dataset for the Chamber of Deputies (Camera dei Deputati) from the open data website of the Ministry of the Interior. *)
	chamberDataset = InitDataset["2018", "chamberofdeputies"];
	(* Original election dataset for the Senate of the Republic (Senato della Repubblica) from the open data website of the Ministry of the Interior. *)
	senateDataset = InitDataset["2018", "senateoftherepublic"];
	
	(* The following two maps are not representative of the official coalitions that were present during the 2018 elections. They are a simplified version: left parties are put on the "SINISTRA" value, right parties are put on the "DESTRA" value, center and/or miscellanea are put on the "CENTRO" value. *)
	coalitionsChamber = Association[
		"Destra" -> {"BLOCCO NAZIONALE PER LE LIBERTA'", "CASAPOUND", "FORZA ITALIA", "FRATELLI D'ITALIA CON GIORGIA MELONI", "GRANDE NORD", "IL POPOLO DELLA FAMIGLIA", "ITALIA AGLI ITALIANI", "ITALIA NEL CUORE", "LEGA", "LISTA DEL POPOLO PER LA COSTITUZIONE", "NOI CON L'ITALIA - UDC", "RINASCIMENTO MIR"},
		"Centro" -> {"MOVIMENTO 5 STELLE", "PARTITO REPUBBLICANO ITALIANO - ALA"},
		"Sinistra" -> {"+EUROPA", "10 VOLTE MEGLIO", "CIVICA POPOLARE LORENZIN", "ITALIA EUROPA INSIEME", "LIBERI E UGUALI", "PARTITO DEMOCRATICO", "PARTITO VALORE UMANO", "PER UNA SINISTRA REVOLUZIONARIA", "POTERE AL POPOLO!", "PATTO PER L'AUTONOMIA", "SIAMO"}
	];
	coalitionsSenate = Association[
		"Destra" -> {"BLOCCO NAZIONALE PER LE LIBERTA'", "CASAPOUND ITALIA", "FORZA ITALIA", "FRATELLI D'ITALIA CON GIORGIA MELONI", "GRANDE NORD", "IL POPOLO DELLA FAMIGLIA", "ITALIA AGLI ITALIANI", "ITALIA NEL CUORE", "LEGA", "LISTA DEL POPOLO PER LA COSTITUZIONE", "NOI CON L'ITALIA - UDC", "RINASCIMENTO MIR", "DESTRE UNITE - FORCONI"},
		"Centro" -> {"MOVIMENTO 5 STELLE", "PARTITO REPUBBLICANO ITALIANO - ALA", "SVP - PATT"},
		"Sinistra" -> {"+EUROPA", "10 VOLTE MEGLIO", "CIVICA POPOLARE LORENZIN", "ITALIA EUROPA INSIEME", "LIBERI E UGUALI", "PARTITO DEMOCRATICO", "PARTITO VALORE UMANO", "PER UNA SINISTRA REVOLUZIONARIA", "POTERE AL POPOLO!", "PATTO PER L'AUTONOMIA", "SIAMO", "PARTITO COMUNISTA"}
	];
	
	(* Italian regions *)
	regions = {"ABRUZZO", "BASILICATA", "CALABRIA", "CAMPANIA", "EMILIA-ROMAGNA", "FRIULI-VENEZIA GIULIA", "LAZIO", "LIGURIA", "LOMBARDIA", "MARCHE", "MOLISE", "PIEMONTE", "PUGLIA", "SARDEGNA", "SICILIA", "TOSCANA", "TRENITNO-ALTO ADIGE", "UMBRIA", "VALLE D'AOSTA", "VENETO"};
	
	(* Regions and their districts (circoscrizioni) *)
	districtsByRegion = Association[
		"ABRUZZO" -> {"ABRUZZO"},
		"BASILICATA" -> {"BASILICATA"},
		"CALABRIA" -> {"CALABRIA"},
		"CAMPANIA" -> {"CAMPANIA 1", "CAMPANIA 2"},
		"EMILIA-ROMAGNA" -> {"EMILIA-ROMAGNA"},
		"FRIULI-VENEZIA GIULIA" -> {"FRIULI-VENEZIA GIULIA"},
		"LAZIO" -> {"LAZIO 1", "LAZIO 2"},
		"LIGURIA" -> {"LIGURIA"},
		"LOMBARDIA" -> {"LOMBARDIA 1", "LOMBARDIA 2", "LOMBARDIA 3", "LOMBARDIA 4"},
		"MARCHE" -> {"MARCHE"},
		"MOLISE" -> {"MOLISE"},
		"PIEMONTE" -> {"PIEMONTE 1", "PIEMONTE 2"},
		"PUGLIA" -> {"PUGLIA"},
		"SARDEGNA" -> {"SARDEGNA"},
		"SICILIA" -> {"SICILIA 1", "SICILIA 2"},
		"TOSCANA" -> {"TOSCANA"},
		"TRENITNO-ALTO ADIGE" -> {"TRENTINO-ALTO ADIGE/S\[CapitalUDoubleDot]DTIROL"},
		"UMBRIA" -> {"UMBRIA"},
		"VALLE D'AOSTA" -> {},
		"VENETO" -> {"VENETO 1", "VENETO 2"}
	];
	
	GetRegions[] := regions
	
	GetChamber[] := chamberDataset
	
	(*Constant values*)
	DISTRICT = "CIRCOSCRIZIONE";
	PROVINCE = "PROVINCIA";
	LASTNAME = "COGNOME";
	FISRTNAME = "NOME";
	CITY = "COMUNE";
	LBLMALEELECTORS = "Elettori maschi";
	LBLFEMALEELECTORS = "Elettori femmine";
	CHAMBER = "CAMERA";
	MALEELECTORS = "ELETTORIMAS";
	FEMALEELECTORS = "ELETTORIFEM";
	LBLMALEVOTERS = "Votanti maschi";
	LBLFEMALEVOTERS = "Votanti femmine";
	MALEVOTERS = "VOTANTIMAS";
	FEMALEVOTERS = "VOTANTIFEM";
	LBLMALENONVOTERS = "Non votanti maschi";
	LBLFEMALENONVOTERS = "Non votanti femmine";
	VOTICANDUNINOM = "VOTICANDUNINOM";
	COALITION = "LISTA";
	UNINOMINALE = "UNINOMINALE";
	VOTISOLOCANDUNINOM = "VOTISOLOCANDUNINOM";
	
	(* d is used instead of district because it does not work otherwise *)
	GetRegionFromDistrict[d_] := StringJoin[If[MatchQ[Characters[d],{__, " ", _}], Take[Characters[d], Length[Characters[d]]-2], d]]
	
	(* Filters the given dataset by the given region *)
	FilterRegion[dataset_, region_] := If[region === Null, dataset, dataset[Select[#[DISTRICT] == ToUpperCase[region] &]]]
	
	(* Filters the given dataset by the given province *)
	FilterProvince[dataset_, province_] := If[province === Null, dataset, dataset[Select[#[PROVINCE] == ToUpperCase[province] &]]]
	
	(* Filters the given dataset by the given district *)
	FilterDistrict[dataset_, district_] := If[district === Null, dataset, dataset[Select[#[DISTRICT] == ToUpperCase[district] &]]]
	
	(* Filters the given dataset by the given query typed by the user *)
	FilterQuery[dataset_, query_] :=
		Module [{queries, queryCharacters, qReplaced, params, datasetToReturn},
			datasetToReturn = If[query === Null, Return[dataset], dataset]; (*Necessary to return the right values*)
			queries = ToUpperCase[StringSplit[query, ","]];
			Do[
				qReplaced = StringReplace[q, " "->""];
				queryCharacters = Characters[qReplaced];
				datasetToReturn = If[
					MatchQ[queryCharacters, {__, "<" , __}], 
					(params = StringSplit[qReplaced, "<"]; (* In 1 there is the attribute, in 2 there is the value*)
					datasetToReturn[Select[#[params[[1]]] < ToExpression[params[[2]]] &]]
					), 
					datasetToReturn];
				datasetToReturn = If[
					MatchQ[queryCharacters, {__, "=" , __}], 
					(params = StringSplit[qReplaced, "="]; (* In 1 there is the attribute, in 2 there is the value*)
					datasetToReturn[Select[#[params[[1]]] == ToExpression[params[[2]]] &]]
					), 
					datasetToReturn];
				datasetToReturn = If[
					MatchQ[queryCharacters, {__, ">" , __}], 
					(params = StringSplit[qReplaced, ">"]; (* In 1 there is the attribute, in 2 there is the value*)
					datasetToReturn[Select[#[params[[1]]] > ToExpression[params[[2]]] &]]
					), 
					datasetToReturn]
				,{q, queries}];
			Return[datasetToReturn]
		]
    
     (* Filters the given dataset by the given last name *)
	FilterLastName[dataset_, lastname_] := If[lastname === Null, dataset, dataset[Select[#[LASTNAME] == ToUpperCase[lastname] &]]]
	
	(* Filters the given dataset by the given first name *)
	FilterFirstName[dataset_, firstname_] := If[firstname === Null, dataset, dataset[Select[#[FIRSTNAME] == ToUpperCase[firstname] &]]]
	
     (* Filters the given dataset by the given city *)
	FilterCity[dataset_, city_] := If[city === Null, dataset, dataset[Select[#[CITY] == ToUpperCase[city] &]]]

	Options[PlottingElectionElectorsPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionElectorsPie[chamber_, opts : OptionsPattern[]] := PieChart[GetElectionElectorsPie[chamber, opts], ChartLegends->{LBLMALEELECTORS, LBLFEMALEELECTORS}, ChartStyle->{Blue, Red}]

	Options[GetElectionElectorsPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	GetElectionElectorsPie[chamber_, opts : OptionsPattern[]] := 
		Module[{dataset, datasetSelectBy, maleElectors, femaleElectors},		    
		    (* Dataset selection *)
			dataset = If[ToUpperCase[chamber] === CHAMBER, chamberDataset, senateDataset];
			datasetSelectBy = dataset[DeleteDuplicatesBy[CITY]]; (* General data on the elections are copied in each row for every candidate and party in a city, therefore we can remove the duplicates *)
			
			(* Applying filters *)
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			
			(* Returning the result *)
			maleElectors = Total[datasetSelectBy[All, MALEELECTORS]];
			femaleElectors = Total[datasetSelectBy[All, FEMALEELECTORS]];
			Return[{maleElectors, femaleElectors}]
		]
	
	Options[PlottingElectionVotersPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionVotersPie[chamber_, opts : OptionsPattern[]] := PieChart[GetElectionVotersPie[chamber, opts], ChartLegends->{LBLMALEVOTERS, LBLFEMALEVOTERS}, ChartStyle->{Blue, Red}]
	
	Options[GetElectionVotersPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	GetElectionVotersPie[chamber_, opts : OptionsPattern[]] :=
		Module[{dataset, datasetSelectBy, maleVoters, femaleVoters},
		    (* Dataset selection *)
			dataset = If[ToUpperCase[chamber] === CHAMBER, chamberDataset, senateDataset];
			datasetSelectBy = dataset[DeleteDuplicatesBy[CITY]];
			
			(* Applying filters *)
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			
			(* Returning the result *)
			maleVoters = Total[datasetSelectBy[All, MALEVOTERS]];
			femaleVoters = Total[datasetSelectBy[All, FEMALEVOTERS]];
			Return[{maleVoters, femaleVoters}]
		]
	
	Options[PlottingElectionVotersNonVotersPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionVotersNonVotersPie[chamber_, opts : OptionsPattern[]] := PieChart[GetElectionVotersNonVotersPie[chamber, opts], ChartLegends->{LBLMALEVOTERS, LBLFEMALEVOTERS, LBLMALENONVOTERS, LBLFEMALENONVOTERS}, ChartStyle->{Blue, Red, Hue[0.65,0.5,1], Hue[0.03,0.46,1]}]
			
	Options[GetElectionVotersNonVotersPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	GetElectionVotersNonVotersPie[chamber_, opts : OptionsPattern[]] :=
		Module[{dataset, datasetSelectBy, maleVoters, femaleVoters, maleElectors, femaleElectors}, 
		    (* Dataset selection *)
			dataset = If[ToUpperCase[chamber] === CHAMBER, chamberDataset, senateDataset];
			datasetSelectBy = dataset[DeleteDuplicatesBy[CITY]];
			
			(* Applying filters *)
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			
			(* Returning the result *)
			maleElectors = Total[datasetSelectBy[All, MALEELECTORS]];
			femaleElectors = Total[datasetSelectBy[All, FEMALEELECTORS]];
			maleVoters = Total[datasetSelectBy[All, MALEVOTERS]];
			femaleVoters = Total[datasetSelectBy[All, FEMALEVOTERS]];
			
			Return[{maleVoters, femaleVoters, (maleElectors - maleVoters), (femaleElectors - femaleVoters)}]
		]
		
	pairUp[xValues_,yValues_]:=({xValues[[#]],yValues[[#]]})&/@Range[Min[Length[xValues],Length[yValues]]];
	
	Options[PlottingElectionRegionCoalitionsBars] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionRegionCoalitionsBars[chamber_, opts : OptionsPattern[]] :=
		Module[{divisions, divisionsVotes, divisionsColorVotes},
			divisions=EntityValue[Entity["AdministrativeDivision",{EntityProperty["AdministrativeDivision","ParentRegion"]->Entity["Country","Italy"]}],"Entities"]; (*TODO: associare divisioni ottenute da Mathematica a regioni in regions*)
			divisionsVotes = Table[Transpose @ {divisions, GetElectionRegionCoalitionsBars[chamber, coalition, opts]}, {coalition, {"Sinistra", "Centro", "Destra"}}];
			divisionsColorVotes = pairUp[divisionsVotes, {{"Sinistra", "ValentineTones"}, {"Centro","SiennaTones"}, {"Destra", "AvocadoColors"}}];
			Return[Table[GeoRegionValuePlot[rvc[[1]], PlotLabel->rvc[[2, 1]], ColorFunction->rvc[[2, 2]]], {rvc, divisionsColorVotes}]]
		]
	
	Options[GetElectionRegionCoalitionsBars] = {region -> Null, province -> Null, district -> Null, query -> Null};
	GetElectionRegionCoalitionsBars[chamber_, coalition_, opts : OptionsPattern[]] :=
		Module[{dataset, parties, datasetSelectBy},
		    (* Dataset selection *)
			dataset = If[ToUpperCase[chamber] === CHAMBER, chamberDataset, senateDataset];
			parties = If[ToUpperCase[chamber] === CHAMBER, coalitionsChamber[[coalition]], coalitionsSenate[[coalition]]];
			datasetSelectBy = dataset;			
			
			(* Applying filters *)
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			
			(* Returning the result *)
			Return[Table[Total[datasetSelectBy[Select[GetRegionFromDistrict[#[DISTRICT]] == r&]][Select[MemberQ[parties, #[COALITION]] &]][All, VOTICANDUNINOM]], {r, regions}]]
		]
		
	Options[GetCandidate] = {city -> Null};
	GetCandidate[name_, surname_, opts : OptionsPattern[]] :=
	     Module[{chamberDatasetSelectBy, senateDatasetSelectBy, returnDataset, returnedLists, uninominaleName},	         
	         (* Applying filters into the dataset of the Chamber of Deputies *)
	         chamberDatasetSelectBy = chamberDataset;
	         
	         chamberDatasetSelectBy = FilterLastName[chamberDatasetSelectBy, surname];
	         chamberDatasetSelectBy = FilterFirstName[chamberDatasetSelectBy, name];
	         chamberDatasetSelectBy = FilterCity[chamberDatasetSelectBy, OptionValue[city]];
	         
	         (* Applying filters into the dataset of the Senate of the Republic *)
	         If[Length[chamberDatasetSelectBy] == 0, (
	             senateDatasetSelectBy = senateDataset;
	         
	             senateDatasetSelectBy = FilterLastName[senateDatasetSelectBy, surname];
	             senateDatasetSelectBy = FilterFirstName[senateDatasetSelectBy, name];
	             senateDatasetSelectBy = FilterCity[senateDatasetSelectBy, OptionValue[city]];
	         )]; (* Little performance extra: the Senate dataset is filtered only if no data is found in the Chamber dataset *)
	         
	         returnedLists = {}; (* Output variable *)
	         uninominaleName = ""; (* Part of the output variable *)
	         
	         (* Returning the result *)
	         If[Length[chamberDatasetSelectBy] > 0, (
	             uninominaleName = chamberDatasetSelectBy[1, "UNINOMINALE"]; (* It is sufficient to get the name of the uninominale from the first row, since a candidate can only be present in one uninominale *)
	             returnDataset = chamberDataset;
	             returnDataset = returnDataset[Select[#[UNINOMINALE] == uninominaleName&]];
	             returnDataset = FilterCity[returnDataset, OptionValue[city]];
	             returnDataset = returnDataset[DeleteDuplicatesBy[LASTNAME]];
	             returnDataset = returnDataset[ReverseSortBy[VOTISOLOCANDUNINOM]];
	             returnedLists = {uninominaleName, returnDataset[All, LASTNAME], returnDataset[All, FIRSTNAME], returnDataset[All, VOTISOLOCANDUNINOM]};
	         )];
	         If[Length[senateDatasetSelectBy] > 0, ((* Either a candidate is present in the Chamber or Senate or in none of the two *)
	             uninominaleName = senateDatasetSelectBy[1, UNINOMINALE];
	             returnDataset = senateDataset;
	             returnDataset = returnDataset[Select[#[UNINOMINALE] == uninominaleName&]];
	             returnDataset = FilterCity[returnDataset, OptionValue[city]];
	             returnDataset = returnDataset[DeleteDuplicatesBy[LASTNAME]];
	             returnDataset = returnDataset[ReverseSortBy[VOTISOLOCANDUNINOM]];
	             returnedLists = {uninominaleName, returnDataset[All, LASTNAME], returnDataset[All, FIRSTNAME], returnDataset[All, VOTISOLOCANDUNINOM]};
	         )];
	         If[Length[chamberDatasetSelectBy] == 0 && Length[senateDatasetSelectBy] == 0, (
	             uninominaleName = "NOT FOUND";
	             returnDataset = senateDatasetSelectBy; (* Taking this dataset knowing it is empty but with the right columns to return *)
	             returnedLists = {uninominaleName, returnDataset[All, LASTNAME], returnDataset[All, FIRSTNAME], returnDataset[All, VOTISOLOCANDUNINOM]};
	         )];
	         
	         Return[returnedLists];
	     ]
	     
	 
End[]
EndPackage[]






