(* ::Package:: *)

(* :Title : Italy 2018 Election Data Analysis *)
(* :Context : Italy 2018 Election Data Analysis *)
(* :Author : ConteTer (Marco Ferrati, Michele Perlino, Tommaso Azzalin, Vittoria Conte) *)
(* :Summary: Package for analysing the 2018 Italian political election data published by the the Ministry of the Interior. *)
(* :Copyright : MIT *)
(* :Package Version : 1 *)
(* :Mathematica Version : 12.2*)
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
	InitDataset[url_] :=
		Module[{file, stringReplaced},
			file = URLDownload[url];
			stringReplaced = StringReplace[ReadString[file], ";"->","];
			Return[ImportString[stringReplaced, "Dataset", HeaderLines->1]];
		]
	
	(* Original election dataset for the Chamber of Deputies (Camera dei Deputati) from the open data website of the Ministry of the Interior. *)
	chamberDataset = InitDataset["https://dait.interno.gov.it/documenti/camera_2018_scrutini_italia.csv"];
	(* Original election dataset for the Senate of the Republic (Senato della Repubblica) from the open data website of the Ministry of the Interior. *)
	senateDataset = InitDataset["https://dait.interno.gov.it/documenti/senato_2018_scrutini_italia.csv"];
	
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
	
	(* d is used instead of district because it does not work otherwise *)
	GetRegionFromDistrict[d_] := StringJoin[If[MatchQ[Characters[d],{__, " ", _}], Take[Characters[d], Length[Characters[d]]-2], d]]
	
	(* Filters the given dataset by the given region *)
	FilterRegion[dataset_, region_] := If[region === Null, dataset, dataset[Select[#CIRCOSCRIZIONE == ToUpperCase[region] &]]]
	
	(* Filters the given dataset by the given province *)
	FilterProvince[dataset_, province_] := If[province === Null, dataset, dataset[Select[#PROVINCIA == ToUpperCase[province] &]]]
	
	(* Filters the given dataset by the given district *)
	FilterDistrict[dataset_, district_] := If[district === Null, dataset, dataset[Select[#CIRCOSCRIZIONE == ToUpperCase[district] &]]]
	
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
	FilterLastName[dataset_, lastname_] := If[lastname === Null, dataset, dataset[Select[#COGNOME == ToUpperCase[lastname] &]]]
	
	(* Filters the given dataset by the given first name *)
	FilterFirstName[dataset_, firstname_] := If[firstname === Null, dataset, dataset[Select[#NOME == ToUpperCase[firstname] &]]]
	
     (* Filters the given dataset by the given city *)
	FilterCity[dataset_, city_] := If[city === Null, dataset, dataset[Select[#COMUNE == ToUpperCase[city] &]]]

	Options[PlottingElectionElectorsPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionElectorsPie[chamber_, opts : OptionsPattern[]] := 
		Module[{dataset, datasetSelectBy, maleElectors, femaleElectors},		    
		    (* Dataset selection *)
			dataset = If[ToUpperCase[chamber] === "CAMERA", chamberDataset, senateDataset];
			datasetSelectBy = dataset[DeleteDuplicatesBy["COMUNE"]]; (* General data on the elections are copied in each row for every candidate and party in a city, therefore we can remove the duplicates *)
			
			(* Applying filters *)
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			
			(* Returning the result *)
			maleElectors = Total[datasetSelectBy[All, "ELETTORIMAS"]];
			femaleElectors = Total[datasetSelectBy[All, "ELETTORIFEM"]];
			Return[{maleElectors, femaleElectors}]
		]
	
	Options[PlottingElectionVotersPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionVotersPie[chamber_, opts : OptionsPattern[]] :=
		Module[{dataset, datasetSelectBy, maleVoters, femaleVoters},
		    (* Dataset selection *)
			dataset = If[ToUpperCase[chamber] === "CAMERA", chamberDataset, senateDataset];
			datasetSelectBy = dataset[DeleteDuplicatesBy["COMUNE"]];
			
			(* Applying filters *)
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			
			(* Returning the result *)
			maleVoters = Total[datasetSelectBy[All, "VOTANTIMAS"]];
			femaleVoters = Total[datasetSelectBy[All, "VOTANTIFEM"]];
			Return[{maleVoters, femaleVoters}]
		]
		
	Options[PlottingElectionVotersNonVotersPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionVotersNonVotersPie[chamber_, opts : OptionsPattern[]] :=
		Module[{dataset, datasetSelectBy, maleVoters, femaleVoters, maleElectors, femaleElectors}, 
		    (* Dataset selection *)
			dataset = If[ToUpperCase[chamber] === "CAMERA", chamberDataset, senateDataset];
			datasetSelectBy = dataset[DeleteDuplicatesBy["COMUNE"]];
			
			(* Applying filters *)
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			
			(* Returning the result *)
			maleElectors = Total[datasetSelectBy[All, "ELETTORIMAS"]];
			femaleElectors = Total[datasetSelectBy[All, "ELETTORIFEM"]];
			maleVoters = Total[datasetSelectBy[All, "VOTANTIMAS"]];
			femaleVoters = Total[datasetSelectBy[All, "VOTANTIFEM"]];
			
			Return[{maleVoters, femaleVoters, (maleElectors - maleVoters), (femaleElectors - femaleVoters)}]
		]
		
	Options[PlottingElectionRegionCoalitionsBars] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionRegionCoalitionsBars[chamber_, coalition_, opts : OptionsPattern[]] :=
		Module[{dataset, parties, datasetSelectBy},
		    (* Dataset selection *)
			dataset = If[ToUpperCase[chamber] === "CAMERA", chamberDataset, senateDataset];
			parties = If[ToUpperCase[chamber] === "CAMERA", coalitionsChamber[[coalition]], coalitionsSenate[[coalition]]];
			datasetSelectBy = dataset;			
			
			(* Applying filters *)
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			
			(* Returning the result *)
			Return[Table[Total[datasetSelectBy[Select[GetRegionFromDistrict[#CIRCOSCRIZIONE] == r&]][Select[MemberQ[parties, #LISTA] &]][All, "VOTICANDUNINOM"]], {r, regions}]]
		]
		
	Options[PlottingCandidate] = {city -> Null};
	PlottingCandidate[name_, surname_, opts : OptionsPattern[]] :=
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
	             returnDataset = returnDataset[Select[#UNINOMINALE == uninominaleName&]];
	             returnDataset = FilterCity[returnDataset, OptionValue[city]];
	             returnDataset = returnDataset[DeleteDuplicatesBy["COGNOME"]];
	             returnDataset = returnDataset[ReverseSortBy["VOTISOLOCANDUNINOM"]];
	             returnedLists = {uninominaleName, returnDataset[All, "COGNOME"], returnDataset[All, "NOME"], returnDataset[All, "VOTISOLOCANDUNINOM"]};
	         )];
	         If[Length[senateDatasetSelectBy] > 0, ((* Either a candidate is present in the Chamber or Senate or in none of the two *)
	             uninominaleName = senateDatasetSelectBy[1, "UNINOMINALE"];
	             returnDataset = senateDataset;
	             returnDataset = returnDataset[Select[#UNINOMINALE == uninominaleName&]];
	             returnDataset = FilterCity[returnDataset, OptionValue[city]];
	             returnDataset = returnDataset[DeleteDuplicatesBy["COGNOME"]];
	             returnDataset = returnDataset[ReverseSortBy["VOTISOLOCANDUNINOM"]];
	             returnedLists = {uninominaleName, returnDataset[All, "COGNOME"], returnDataset[All, "NOME"], returnDataset[All, "VOTISOLOCANDUNINOM"]};
	         )];
	         If[Length[chamberDatasetSelectBy] == 0 && Length[senateDatasetSelectBy] == 0, (
	             uninominaleName = "NOT FOUND";
	             returnDataset = senateDatasetSelectBy; (* Taking this dataset knowing it is empty but with the right columns to return *)
	             returnedLists = {uninominaleName, returnDataset[All, "COGNOME"], returnDataset[All, "NOME"], returnDataset[All, "VOTISOLOCANDUNINOM"]};
	         )];
	         
	         Return[returnedLists];
	     ]
End[]
EndPackage[]



