(* ::Package:: *)

(* :Title: Italy 2018 Election Data Analysis *)
(* :Context: Italy 2018 Election Data Analysis *)
(* :Author: ConteTer (Marco Ferrati, Michele Perlino, Tommaso Azzalin, Vittoria Conte) *)
(* :Summary: Package for analysing the 2018 Italian political election data published by the the Ministry of the Interior. *)
(* :Copyright: MIT *)
(* :Package Version: 1 *)
(* :Mathematica Versionfile: 12.2 *)
(* :History: *)
(* :Keywords: italy, election, 2018, data, analysis *)
(* :Sources: https://github.com/jjocram/italy-2018-election-data-mathematica *)
(* :Discussion: *)


BeginPackage["Italy2018ElectionDataAnalysis`"]


LoadDataByYear::usage = "LoadDataByYear[year] loads the dataset for the year given as input to the function."
AnalyzeElectorsVotersNonvoters::usage = "AnalyzeElectorsVotersNonvoters[] generates the user interface with input fields for showing pie charts regarding electors, voters and non voters divided by sex."
AnalyzeVotesForCoalitionsInRegions::usage = "AnalyzeVotesForCoalitionsInRegions[] generates the user interface with house selector for showing heatmaps with how many votes each coalition got in each Italian region."
AnalyzeVotesForCandidate::usage = "AnalyzeVotesForCandidate[] generates the user interface with input fields for showing how many votes the searched candidate got in a city of his/her district."
PlottingRegionsItalyMap::usage = "PlottingRegionsItalyMap[exportDPI] generate a .png file of Italy divided by region"
PlottingElectionRegionCoalitionsBars3D::usage = "PlottingElectionRegionCoalitionsBars3D[house] generates the .3ds files for each 3D bar chart for each coalitions."


Begin["`Private`"]


	(* CONSTANTS *)
	
	(* Houses of the Italian Parliament. *)
	ChamberOfDeputies = "Chamber of Deputies"
	SenateOfTheRepublic = "Senate of the Republic"
	
	(* Italian regions. *)
	REGIONS = {"ABRUZZO", "BASILICATA", "CALABRIA", "CAMPANIA", "EMILIA-ROMAGNA", "FRIULI-VENEZIA GIULIA", "LAZIO", "LIGURIA", "LOMBARDIA", "MARCHE", "MOLISE", "PIEMONTE", "PUGLIA", "SARDEGNA", "SICILIA", "TOSCANA", "TRENTINO-ALTO ADIGE", "UMBRIA", "VALLE D'AOSTA", "VENETO"};
	
	(* Italian provinces. *)
	PROVINCES = {"AGRIGENTO", "ALESSANDRIA", "ANCONA", "AOSTA", "AREZZO", "ASCOLI PICENO", "ASTI", "AVELLINO", "BARI", "BARLETTA-ANDRIA-TRANI", "BELLUNO", "BENEVENTO", "BERGAMO", "BIELLA", "BOLOGNA", "BOLZANO", "BRESCIA", "BRINDISI", "CAGLIARI", "CALTANISSETTA", "CAMPOBASSO", "CASERTA", "CATANIA", "CATANZARO", "CHIETI", "COMO", "COSENZA", "CREMONA", "CROTONE", "CUNEO", "ENNA", "FERMO", "FERRARA", "FIRENZE", "FOGGIA", "FORLI'-CESENA", "FROSINONE", "GENOVA", "GORIZIA", "GROSSETO", "IMPERIA", "ISERNIA", "L'AQUILA", "LA SPEZIA", "LATINA", "LECCE", "LECCO", "LIVORNO", "LODI", "LUCCA", "MACERATA", "MANTOVA", "MASSA-CARRARA", "MATERA", "MESSINA", "MILANO", "MODENA", "MONZA E DELLA BRIANZA", "NAPOLI", "NOVARA", "NUORO", "ORISTANO", "PADOVA", "PALERMO", "PARMA", "PAVIA", "PAVIA", "PERUGIA", "PESARO E URBINO", "PESCARA", "PIACENZA", "PISA", "PISTOIA", "PORDENONE", "POTENZA", "PRATO", "RAGUSA", "REGGIO CALABRIA", "REGGIO EMILIA", "RIETI", "RIMINI", "ROMA", "ROVIGO", "SALERNO", "SASSARI", "SAVONA", "SIENA", "SIRACUSA", "SONDRIO", "SUD SARDEGNA", "TARANTO", "TERAMO", "TERNI", "TORINO", "TRAPANI", "TRENTO", "TREVISO", "TRIESTE", "UDINE", "VARESE", "VENEZIA", "VERBANO-CUSIO-OSSOLA", "VERCELLI", "VERONA", "VIBO VALENTIA", "VICENZA", "VITERBO"};
	
	(* Regions and their provinces. *)
	PROVINCESBYREGION = Association[
		"ABRUZZO" -> {"CHIETI", "L'AQUILA", "PESCARA", "TERAMO"},
		"BASILICATA" -> {"MATERA", "POTENZA"},
		"CALABRIA" -> {"CATANZARO", "COSENZA", "CROTONE", "REGGIO CALABRIA", "VIBO VALENTIA"},
		"CAMPANIA" -> {"AVELLINO", "BENEVENTO", "CASERTA", "NAPOLI", "SALERNO"},
		"EMILIA-ROMAGNA" -> {"BOLOGNA", "FERRARA", "FORLI'-CESENA", "MODENA", "PARMA", "PIACENZA", "REGGIO EMILIA", "RIMINI"},
		"FRIULI-VENEZIA GIULIA" -> {"GORIZIA", "PORDENONE", "TRIESTE", "UDINE"},
		"LAZIO" -> {"FROSINONE", "LATINA", "RIETI", "ROMA", "VITERBO"},
		"LIGURIA" -> {"GENOVA", "IMPERIA", "LA SPEZIA", "SAVONA"},
		"LOMBARDIA" -> {"BERGAMO", "BRESCIA", "COMO", "CREMONA", "LECCO", "LODI", "MANTOVA", "MILANO", "MONZA E DELLA BRIANZA", "PAVIA", "PAVIA", "SONDRIO", "VARESE"},
		"MARCHE" -> {"ANCONA", "ASCOLI PICENO", "FERMO", "MACERATA", "PESARO E URBINO"},
		"MOLISE" -> {"CAMPOBASSO", "ISERNIA"},
		"PIEMONTE" -> {"ALESSANDRIA", "ASTI", "BIELLA", "CUNEO", "NOVARA", "TORINO", "VERBANO-CUSIO-OSSOLA", "VERCELLI"},
		"PUGLIA" -> {"BARI", "BARLETTA-ANDRIA-TRANI", "BRINDISI", "FOGGIA", "LECCE", "TARANTO"},
		"SARDEGNA" -> {"CAGLIARI", "NUORO", "ORISTANO", "SASSARI", "SUD SARDEGNA"},
		"SICILIA" -> {"AGRIGENTO", "CALTANISSETTA", "CATANIA", "ENNA", "MESSINA", "PALERMO", "RAGUSA", "SIRACUSA", "TRAPANI"},
		"TOSCANA" -> {"AREZZO", "FIRENZE", "GROSSETO", "LIVORNO", "LUCCA", "MASSA-CARRARA", "PISA", "PISTOIA", "PRATO", "SIENA"},
		"TRENTINO-ALTO ADIGE" -> {"BOLZANO", "TRENTO"},
		"UMBRIA" -> {"PERUGIA", "TERNI"},
		"VALLE D'AOSTA" -> {"AOSTA"},
		"VENETO" -> {"BELLUNO", "PADOVA", "ROVIGO", "TREVISO", "VENEZIA", "VERONA", "VICENZA"}
	];
	
	(* Italian electoral districts (circoscrizioni). *)
	DISTRICTS = {"ABRUZZO", "BASILICATA", "CALABRIA", "CAMPANIA 1", "CAMPANIA 2", "EMILIA-ROMAGNA", "FRIULI-VENEZIA GIULIA", "LAZIO 1", "LAZIO 2", "LIGURIA", "LOMBARDIA 1", "LOMBARDIA 2", "LOMBARDIA 3", "LOMBARDIA 4", "MARCHE", "MOLISE", "PIEMONTE 1", "PIEMONTE 2", "PUGLIA", "SARDEGNA", "SICILIA 1", "SICILIA 2", "TOSCANA", "TRENTINO-ALTO ADIGE/S\[CapitalUDoubleDot]DTIROL", "UMBRIA", "VENETO 1", "VENETO 2"};
	
	(* Regions and their districts (circoscrizioni). *)
	DISTRICTSBYREGION = Association[
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
		"TRENTINO-ALTO ADIGE" -> {"TRENTINO-ALTO ADIGE/S\[CapitalUDoubleDot]DTIROL"},
		"UMBRIA" -> {"UMBRIA"},
		"VALLE D'AOSTA" -> {},
		"VENETO" -> {"VENETO 1", "VENETO 2"}
	];
	
	(* Collection of dataset references (for future updates). *)
	DATASETS = Association[
		"2018" -> Association[
			"Chamber of Deputies" -> Association[
				"url" -> "https://dait.interno.gov.it/documenti/camera_2018_scrutini_italia.csv",
				"cache" -> "camera_2018_scrutini_italia.csv"
			],
			"Senate of the Republic" -> Association[
				"url" -> "https://dait.interno.gov.it/documenti/senato_2018_scrutini_italia.csv",
				"cache" -> "senato_2018_scrutini_italia.csv"
			]
		]
	];
	
	(* The following two associations/maps, respectively for the Chamber of Deputies and Senate of the Republic, are not representative of the official coalitions that were present during the 2018 elections. *)
	(* They are a simplified version: left and *-left parties are put corresponding to key "Sinistra", right and *-right parties are put corresponding to key "Destra", center and/or miscellanea are put corresponding to key "Centro". *)
	COALITIONSCHAMBER = Association[
		"2018" -> Association[
			"Destra" -> {"BLOCCO NAZIONALE PER LE LIBERTA'", "CASAPOUND", "FORZA ITALIA", "FRATELLI D'ITALIA CON GIORGIA MELONI", "GRANDE NORD", "IL POPOLO DELLA FAMIGLIA", "ITALIA AGLI ITALIANI", "ITALIA NEL CUORE", "LEGA", "LISTA DEL POPOLO PER LA COSTITUZIONE", "NOI CON L'ITALIA - UDC", "RINASCIMENTO MIR"},
			"Centro" -> {"MOVIMENTO 5 STELLE", "PARTITO REPUBBLICANO ITALIANO - ALA"},
			"Sinistra" -> {"+EUROPA", "10 VOLTE MEGLIO", "CIVICA POPOLARE LORENZIN", "ITALIA EUROPA INSIEME", "LIBERI E UGUALI", "PARTITO DEMOCRATICO", "PARTITO VALORE UMANO", "PER UNA SINISTRA REVOLUZIONARIA", "POTERE AL POPOLO!", "PATTO PER L'AUTONOMIA", "SIAMO"}
		]
	];
	COALITIONSSENATE = Association[
		"2018" -> Association[
			"Destra" -> {"BLOCCO NAZIONALE PER LE LIBERTA'", "CASAPOUND ITALIA", "FORZA ITALIA", "FRATELLI D'ITALIA CON GIORGIA MELONI", "GRANDE NORD", "IL POPOLO DELLA FAMIGLIA", "ITALIA AGLI ITALIANI", "ITALIA NEL CUORE", "LEGA", "LISTA DEL POPOLO PER LA COSTITUZIONE", "NOI CON L'ITALIA - UDC", "RINASCIMENTO MIR", "DESTRE UNITE - FORCONI"},
			"Centro" -> {"MOVIMENTO 5 STELLE", "PARTITO REPUBBLICANO ITALIANO - ALA", "SVP - PATT"},
			"Sinistra" -> {"+EUROPA", "10 VOLTE MEGLIO", "CIVICA POPOLARE LORENZIN", "ITALIA EUROPA INSIEME", "LIBERI E UGUALI", "PARTITO DEMOCRATICO", "PARTITO VALORE UMANO", "PER UNA SINISTRA REVOLUZIONARIA", "POTERE AL POPOLO!", "PATTO PER L'AUTONOMIA", "SIAMO", "PARTITO COMUNISTA"}
		]
	]


	(* DATA KEYS FOR ACCESSING FIELDS OF DATASETKEYS *)
	DISTRICT = "district";
	PROVINCE = "province";
	LASTNAME = "lastname";
	FIRSTNAME = "firstname";
	CITY = "city";
	HOUSE = "house";
	MALEELECTORS = "maleElectors";
	FEMALEELECTORS = "femaleElectors";
	MALEVOTERS = "maleVoters";
	FEMALEVOTERS = "femaleVoters";
	VOTICANDUNINOM = "singleMemberDistrictCandidateVotes";
	COALITION = "list";
	UNINOMINALE = "singleMemberDistrict";
	VOTISOLOCANDUNINOM = "singleMemberDistrictCandidateOnlyVotes";

	(* MAPPING OF THE DATA KEYS TO THE DATASET COLUMNS *)
	DATASETKEYS = Association[
		"2018" -> Association[
			DISTRICT -> "CIRCOSCRIZIONE",
			PROVINCE -> "PROVINCIA",
			LASTNAME -> "COGNOME",
			FIRSTNAME -> "NOME",
			CITY -> "COMUNE",
			MALEELECTORS -> "ELETTORIMAS",
			FEMALEELECTORS -> "ELETTORIFEM",
			MALEVOTERS -> "VOTANTIMAS",
			FEMALEVOTERS -> "VOTANTIFEM",
			VOTICANDUNINOM -> "VOTICANDUNINOM",
			COALITION -> "LISTA",
			UNINOMINALE -> "UNINOMINALE",
			VOTISOLOCANDUNINOM -> "VOTISOLOCANDUNINOM"
		]
	];
	
	(* CHART LABELS *)
	ENLBLMALEELECTORS = "Male electors";
	ENLBLFEMALEELECTORS = "Female electors";
	ENLBLMALEVOTERS = "Male voters";
	ENLBLFEMALEVOTERS = "Female voters";
	ENLBLMALENONVOTERS = "Male\nnon-voters";
	ENLBLFEMALENONVOTERS = "Female\nnon-voters";


	(* DATA FIELDS *)
	
	(* Year selected by the user. *)
	selectedYear;
	
	(* Stores the original election dataset of the Chamber of Deputies (Camera dei Deputati) from the open data website of the Ministry of the Interior. *)
	chamberDataset;
	
	(* Stores the original election dataset of the Senate of the Republic (Senato della Repubblica) from the open data website of the Ministry of the Interior. *)
	senateDataset;


	(* SUPPORT FUNCTIONS *)
	
	(* Initializes a dataset for a given year and for a given house, downloading it if it does not exist, reading it from the cache otherwise. *)
	InitDataset[year_, house_] :=
		Module[{file, readString, importedDS, cache, url},
			(* Checking if the dataset exists and/or is supported. *)
			If[
				Not[KeyExistsQ[DATASETS, year]],
				(
					Print["No data is available for the year ", year, " for the ", house, "."];
					Return[Null];
				)
			];
			(* Storing the user selection in the package variable selectedYear for later use. *)
			selectedYear = year;
			
			(* Retrieving the dataset and saving it into the kernel. *)
			readString = "";
			cache = DATASETS[[year]][[house]][["cache"]];
			url = DATASETS[[year]][[house]][["url"]];
			If[
				FileExistsQ[cache],
				(
					Print["Loading the ", year, " ", house, " dataset from cache..."];
					readString = ReadString[cache];
					importedDS = ImportString[readString, "Dataset", HeaderLines->1];
					Print["...loaded!"];
				),
				(
					Print["Downloading the ", year, " ", house, " dataset from the internet..."];
					file = URLDownload[url];
					readString = StringReplace[ReadString[file], ";"->","];
					importedDS = ImportString[readString, "Dataset", HeaderLines->1];
					Export[cache, importedDS, "CSV"];
					Print["...downloaded and cached!"];
				)
			];
			
			Return[importedDS];
		]
	
	(* Returns the region which the district d is part of. *)
	GetRegionFromDistrict[d_] := StringJoin[If[MatchQ[Characters[d],{__, " ", _}], Take[Characters[d], Length[Characters[d]]-2], d]]
	
	(* Filters the given dataset returning only data of the given region. *)
	FilterRegion[dataset_, region_] := 
		If[region === Null, dataset, 
			dataset[
				Select[
					StringMatchQ[
						#[DATASETKEYS[[selectedYear]][[DISTRICT]]], 
						((ToUpperCase[region] ~~ " " ~~ ("1" | "2" | "3" | "4")) | ToUpperCase[region])
					]&
				]
			]
		]
	
	(* Filters the given dataset returning only data of the given province. *)
	FilterProvince[dataset_, province_] := If[province === Null, dataset, dataset[Select[#[DATASETKEYS[[selectedYear]][[PROVINCE]]] == ToUpperCase[province] &]]]
	
	(* Filters the given dataset returning only data of the given district. *)
	FilterDistrict[dataset_, district_] := If[district === Null, dataset, dataset[Select[#[DATASETKEYS[[selectedYear]][[DISTRICT]]] == ToUpperCase[district] &]]]
	
	(* Filters the given dataset returning only data satisfying the conditions expressed in the given query typed by the user. *)
	FilterQuery[dataset_, query_] :=
		Module [{queries, queryCharacters, qReplaced, params, datasetToReturn},
			If[
				query === Null,
				Return[dataset]
			];
			(* All filters will be applied to datasetToReturn. *)
			datasetToReturn = dataset;
			(* Splitting the query in the form "condition(, condition)*" in the parts is formed of. *)
			queries = ToUpperCase[StringSplit[query, ","]];
			Do[
				qReplaced = StringReplace[q, " "->""]; (* Removing white spaces. *)
				queryCharacters = Characters[qReplaced];
				(* The next lines analyze the single conditions, search for binary operator <, >, = and apply the expressed conditions. *)
				(* Inexistent fields will be ignored since they cannot be found. *)
				datasetToReturn = If[
					MatchQ[queryCharacters, {__, "<" , __}], 
					(
						params = StringSplit[qReplaced, "<"]; (* In 1 there is the attribute, in 2 there is the value. *)
						datasetToReturn[Select[#[params[[1]]] < ToExpression[params[[2]]] &]]
					), 
					datasetToReturn
				];
				datasetToReturn = If[
					MatchQ[queryCharacters, {__, "=" , __}], 
					(
						params = StringSplit[qReplaced, "="]; (* In 1 there is the attribute, in 2 there is the value. *)
						datasetToReturn[Select[#[params[[1]]] == ToExpression[params[[2]]] &]]
					), 
					datasetToReturn
				];
				datasetToReturn = If[
					MatchQ[queryCharacters, {__, ">" , __}], 
					(
						params = StringSplit[qReplaced, ">"]; (* In 1 there is the attribute, in 2 there is the value. *)
						datasetToReturn[Select[#[params[[1]]] > ToExpression[params[[2]]] &]]
					), 
					datasetToReturn
				]
				,
				{q, queries}
			];
			Return[datasetToReturn]
		]
    
    (* Filters the given dataset returning only data of the given last name. *)
	FilterLastName[dataset_, lastname_] := If[lastname === Null, dataset, dataset[Select[#[DATASETKEYS[[selectedYear]][[LASTNAME]]] == ToUpperCase[lastname] &]]]
	
	(* Filters the given dataset returning only data of the given first name. *)
	FilterFirstName[dataset_, firstname_] := If[firstname === Null, dataset, dataset[Select[#[DATASETKEYS[[selectedYear]][[FIRSTNAME]]] == ToUpperCase[firstname] &]]]
	
    (* Filters the given dataset returning only data of the given city. *)
	FilterCity[dataset_, city_] := If[city === Null, dataset, dataset[Select[#[DATASETKEYS[[selectedYear]][[CITY]]] == ToUpperCase[city] &]]]


	(* PUBLIC FUNCTIONS (PLOTTING FUNCTIONS) AND PRIVATE DATA EXTRACTION FUNCTIONS (SUPPORT FOR PUBLIC FUNCTIONS) *)


	AnalyzeElectorsVotersNonvoters[] :=
		DynamicModule[{form, charts, house, region, province, district, query},
			(* DATA INPUT *)
			form = Panel[Column[{
				(* Title of the form. *)
				Style["Data visualization on electors and voters", FontSize -> 28],
				(* Form components *)
				(* Selector of the house. *)
				RadioButtonBar[
					Dynamic[house],
					{ChamberOfDeputies, SenateOfTheRepublic}
				],
				(* Selector for the region. *)
				Row[{
					Style["Region  \t"],
					PopupMenu[
						Dynamic[region],
						Join[{"ALL"}, REGIONS],
						FieldSize -> Medium
					](* Free variable *)
				}],
				(* Selector for the province. *)
				Row[{
					Style["Province\t"],
					Dynamic[PopupMenu[
						Dynamic[province],
						(* Returning all provinces if no region is selected, othwerwise only provinces of the selected region are returned. *)
						If[region === "ALL", Join[{"ALL"}, PROVINCES], Join[{"ALL"}, PROVINCESBYREGION[[region]]]],
						FieldSize -> Medium
					]] (* Depends on region *)
				}],
				(* Selector for the district. *)
				Row[{
					Style["District  \t"],
					Dynamic[PopupMenu[
						Dynamic[district],
						(* Returning all districts if no region is selected, othwerwise only districts of the selected region are returned. *)
						If[region === "ALL", Join[{"ALL"}, DISTRICTS], Join[{"ALL"}, DISTRICTSBYREGION[[region]]]],
						FieldSize -> Medium
					]] (* Depends on region *)
				}],
				(* Input field for the query. *)
				Row[{
					Style["Query\t"],
					InputField[
						Dynamic[query, Initialization -> (query = "")],
						String,
						FieldSize -> Medium
					]
				}]
			}, Center]];
			
			(* DATA OUTPUT *)
			charts = Panel[Row[{
				Style["Electors"],
				Dynamic[PlottingElectionElectorsPie[house, "region" -> If[region === "ALL", Null, region], "province" -> If[province === "ALL", Null, province], "district" -> If[district === "ALL", Null, district], "query" -> If[StringMatchQ[query, ""], Null, query]]],
				Style["Voters"],
				Dynamic[PlottingElectionVotersPie[house, "region" -> If[region === "ALL", Null, region], "province" -> If[province === "ALL", Null, province], "district" -> If[district === "ALL", Null, district], "query" -> If[StringMatchQ[query, ""], Null, query]]],
				Style["Voters and non-voters"],
				Dynamic[PlottingElectionVotersNonVotersPie[house, "region" -> If[region === "ALL", Null, region], "province" -> If[province === "ALL", Null, province], "district" -> If[district === "ALL", Null, district], "query" -> If[StringMatchQ[query, ""], Null, query]]]
			}]];
			
			Column[{form, charts}]
		]


	AnalyzeVotesForCoalitionsInRegions[] :=
		Manipulate[
			PlottingElectionRegionCoalitionsBars[house],
			{{house, ChamberOfDeputies, "House"}, {ChamberOfDeputies, SenateOfTheRepublic}}
		]


	AnalyzeVotesForCandidate[] :=
		Manipulate[
			PlottingCandidate[name, surname, city], 
			{{name, "", "First name"}, "" InputField[#, String]&},
			{{surname, "", "Last name"}, "", InputField[#, String]&},
			{{city, "", "City"}, "", InputField[#, String]&}
		]


	LoadDataByYear[year_] :=
		(
			chamberDataset = InitDataset[year, ChamberOfDeputies];
			senateDataset = InitDataset[year, SenateOfTheRepublic];
		)


	Options[PlottingElectionElectorsPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionElectorsPie[house_, opts : OptionsPattern[]] :=
		PieChart[
			GetElectionElectorsPie[house, opts],
			ChartLegends->{ENLBLMALEVOTERS, ENLBLFEMALEVOTERS, ENLBLMALENONVOTERS, ENLBLFEMALENONVOTERS},
			ChartStyle->{Blue, Red}
		]
	
	Options[GetElectionElectorsPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	GetElectionElectorsPie[house_, opts : OptionsPattern[]] := 
		Module[{dataset, datasetSelectBy, maleElectors, femaleElectors},		    
		    (* Dataset selection. *)
			dataset = If[ToUpperCase[house] === ToUpperCase[ChamberOfDeputies], chamberDataset, senateDataset];
			datasetSelectBy = dataset[DeleteDuplicatesBy[DATASETKEYS[[selectedYear]][[CITY]]]]; (* General data on the elections are copied in each row for every candidate and party in a city, therefore we can remove the duplicates. *)
			
			(* Applying filters. *)
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			
			(* Returning the result. *)
			maleElectors = Total[datasetSelectBy[All, DATASETKEYS[[selectedYear]][[MALEELECTORS]]]];
			femaleElectors = Total[datasetSelectBy[All, DATASETKEYS[[selectedYear]][[FEMALEELECTORS]]]];
			Return[{maleElectors, femaleElectors}]
		]


	Options[PlottingElectionVotersPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionVotersPie[house_, opts : OptionsPattern[]] :=
		PieChart[GetElectionVotersPie[house, opts],
			ChartLegends->{ENLBLMALEVOTERS, ENLBLFEMALEVOTERS, ENLBLMALENONVOTERS, ENLBLFEMALENONVOTERS},
			ChartStyle->{Blue, Red}
		]
	
	Options[GetElectionVotersPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	GetElectionVotersPie[house_, opts : OptionsPattern[]] :=
		Module[{dataset, datasetSelectBy, maleVoters, femaleVoters},
		    (* Dataset selection. *)
			dataset = If[ToUpperCase[house] === ToUpperCase[ChamberOfDeputies], chamberDataset, senateDataset];
			datasetSelectBy = dataset[DeleteDuplicatesBy[DATASETKEYS[[selectedYear]][[CITY]]]];
			
			(* Applying filters. *)
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			
			(* Returning the result. *)
			maleVoters = Total[datasetSelectBy[All, DATASETKEYS[[selectedYear]][[MALEVOTERS]]]];
			femaleVoters = Total[datasetSelectBy[All, DATASETKEYS[[selectedYear]][[FEMALEVOTERS]]]];
			Return[{maleVoters, femaleVoters}]
		]


	Options[PlottingElectionVotersNonVotersPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionVotersNonVotersPie[house_, opts : OptionsPattern[]] :=
		PieChart[
			GetElectionVotersNonVotersPie[house, opts],		
			ChartLegends->{ENLBLMALEVOTERS, ENLBLFEMALEVOTERS, ENLBLMALENONVOTERS, ENLBLFEMALENONVOTERS},
			ChartStyle->{Blue, Red, Hue[0.65,0.5,1], Hue[0.03,0.46,1]}
		]
			
	Options[GetElectionVotersNonVotersPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	GetElectionVotersNonVotersPie[house_, opts : OptionsPattern[]] :=
		Module[{dataset, datasetSelectBy, maleVoters, femaleVoters, maleElectors, femaleElectors}, 
		    (* Dataset selection. *)
			dataset = If[ToUpperCase[house] === ToUpperCase[ChamberOfDeputies], chamberDataset, senateDataset];
			datasetSelectBy = dataset[DeleteDuplicatesBy[DATASETKEYS[[selectedYear]][[CITY]]]];
			
			(* Applying filters. *)
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			
			(* Returning the result. *)
			maleElectors = Total[datasetSelectBy[All, DATASETKEYS[[selectedYear]][[MALEELECTORS]]]];
			femaleElectors = Total[datasetSelectBy[All, DATASETKEYS[[selectedYear]][[FEMALEELECTORS]]]];
			maleVoters = Total[datasetSelectBy[All, DATASETKEYS[[selectedYear]][[MALEVOTERS]]]];
			femaleVoters = Total[datasetSelectBy[All, DATASETKEYS[[selectedYear]][[FEMALEVOTERS]]]];
			
			Return[{maleVoters, femaleVoters, (maleElectors - maleVoters), (femaleElectors - femaleVoters)}]
		]


	pairUp[xValues_, yValues_] := ({xValues[[#]], yValues[[#]]})&/@Range[Min[Length[xValues], Length[yValues]]];
	
	Options[PlottingElectionRegionCoalitionsBars] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionRegionCoalitionsBars[house_, opts : OptionsPattern[]] :=
		Module[{divisions, divisionUnordered, divisionsVotes, divisionsColorVotes},
			(* Retrieving Italian regions (those managed by Mathematica, since they need to be plotted). *)
			divisionUnordered = EntityValue[Entity["AdministrativeDivision",{EntityProperty["AdministrativeDivision","ParentRegion"]->Entity["Country","Italy"]}],"Entities"];
			(* divisionUnordered is alphabetically ordered by region name, in English: only region Apulia (Puglia in Italian) is in the wrong place in the Italian alphabetical order. *)
			divisions = Join[Take[divisionUnordered, 1], Take[divisionUnordered, {3, 13}], Take[divisionUnordered, {2, 2}], Take[divisionUnordered, {14, 20}]];
			divisionsVotes = Table[Transpose @ {divisions, GetElectionRegionCoalitionsBars[house, coalition, opts]}, {coalition, {"Sinistra", "Centro", "Destra"}}];
			divisionsColorVotes = pairUp[divisionsVotes, {{"Sinistra", ColorData[{"ValentineTones", "Reverse"}]}, {"Centro", ColorData[{"SiennaTones", "Reverse"}]}, {"Destra", ColorData[{"AvocadoColors", "Reverse"}]}}];
			Return[GraphicsRow[Table[GeoRegionValuePlot[rvc[[1]], PlotLabel -> rvc[[2, 1]], ColorFunction -> rvc[[2,2]], GeoBackground -> None], {rvc, divisionsColorVotes}], Frame -> All, ImageSize -> Full]]
		]
		
	PlottingElectionRegionCoalitionsBars3D[house_] :=
		Module[{divisionUnordered, divisions, divisionsVotes, centralCoordinates, polygons, coord3D, graphBar3DLeft, graphBar3DCenter, graphBar3DRight},
			divisionUnordered = EntityValue[Entity["AdministrativeDivision",{EntityProperty["AdministrativeDivision","ParentRegion"]->Entity["Country","Italy"]}],"Entities"];
			divisions = Join[Take[divisionUnordered, 1], Take[divisionUnordered, {3, 13}], Take[divisionUnordered, {2, 2}], Take[divisionUnordered, {14, 20}]];
			centralCoordinates = Reverse /@ EntityValue[divisions, EntityProperty["AdministrativeDivision", "Coordinates"]];
			polygons = EntityValue[divisions, EntityProperty["AdministrativeDivision", "Polygon"]];
			
			divisionsVotes = Transpose @ {divisions, GetElectionRegionCoalitionsBars[house, "Sinistra"]};		
			coord3D = Partition[Flatten[Transpose@{centralCoordinates,divisionsVotes[[All,2]]/1000000}], 3];
			graphBar3DLeft = Graphics3D[{Yellow, Cuboid[{#1, #2, 0}, {#1 + .2, #2 + .2, #3}] & @@@ coord3D}, Axes -> False, Boxed->False];
			
			divisionsVotes = Transpose @ {divisions, GetElectionRegionCoalitionsBars[house, "Centro"]};		
			coord3D = Partition[Flatten[Transpose@{centralCoordinates,divisionsVotes[[All,2]]/1000000}], 3];
			graphBar3DCenter = Graphics3D[{Yellow, Cuboid[{#1, #2, 0}, {#1 + .2, #2 + .2, #3}] & @@@ coord3D}, Axes -> False, Boxed->False];
			
			divisionsVotes = Transpose @ {divisions, GetElectionRegionCoalitionsBars[house, "Destra"]};		
			coord3D = Partition[Flatten[Transpose@{centralCoordinates,divisionsVotes[[All,2]]/1000000}], 3];
			graphBar3DRight = Graphics3D[{Yellow, Cuboid[{#1, #2, 0}, {#1 + .2, #2 + .2, #3}] & @@@ coord3D}, Axes -> False, Boxed->False];
			
			
			Print["Exporting 3D models..."];
			Export[StringJoin[ToString[house], "_l.3ds"], graphBar3DLeft];
			Export[StringJoin[ToString[house], "_c.3ds"], graphBar3DCenter];
			Export[StringJoin[ToString[house], "_r.3ds"], graphBar3DRight];
			Print["...exported all files!"];
			SystemOpen[DirectoryName[AbsoluteFileName[StringJoin[ToString[house], "_l.3ds"]]]];
			Return[Row[graphBar3DLeft, graphBar3DCenter, graphBar3DRight]];
		]
		
		
	PlottingRegionsItalyMap[exportDPI_: 500] :=
		Module[{regions, polygons, ItalyMap},
			regions = Entity["Country", "Italy"][EntityProperty["Country", "AdministrativeDivisions"]];
			polygons = EntityValue[regions, EntityProperty["AdministrativeDivision", "Polygon"]];
			ItalyMap = GeoGraphics[{GeoStyling["Satellite"], EdgeForm[{Thickness[Medium], White}],  polygons}, GeoBackground->None];			
			
			Print["Exporting the image..."];
			Export["ItalyMap.png", ItalyMap, ImageResolution -> exportDPI];
			Print["...Exported the image!"];
			SystemOpen[DirectoryName[AbsoluteFileName["ItalyMap.png"]]];
			Return[ItalyMap];
		]
	
	
	Options[GetElectionRegionCoalitionsBars] = {region -> Null, province -> Null, district -> Null, query -> Null};
	GetElectionRegionCoalitionsBars[house_, coalition_, opts : OptionsPattern[]] :=
		Module[{dataset, parties, datasetSelectBy, result, districtKey, coalitionKey, votesKey, regionVotes},
		    (* Dataset and parties selection. *)
			dataset = If[ToUpperCase[house] === ToUpperCase[ChamberOfDeputies], chamberDataset, senateDataset];
			(* Selecting the parties related to the coalition given as argument (they depend on the chosen house). *)
			parties = If[ToUpperCase[house] === ToUpperCase[ChamberOfDeputies], COALITIONSCHAMBER[[selectedYear]][[coalition]], COALITIONSSENATE[[selectedYear]][[coalition]]];
			datasetSelectBy = dataset;		
			
			(* Keys for accessing the dataset. *)	
			districtKey = DATASETKEYS[[selectedYear]][[DISTRICT]];
			coalitionKey = DATASETKEYS[[selectedYear]][[COALITION]];
			votesKey = DATASETKEYS[[selectedYear]][[VOTICANDUNINOM]];
			
			(* Applying filters. *)
			
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			datasetSelectBy = datasetSelectBy[Select[MemberQ[parties, #[coalitionKey]]&]];
			
			datasetSelectBy = datasetSelectBy[All, {districtKey, votesKey}];
			
			(* Returning the result. *)
			Return[
				Table[
					Total[
						datasetSelectBy[
							Select[GetRegionFromDistrict[#[districtKey]] == r&]
						][All, votesKey]
					],
					{r, REGIONS}
				]
			]
		]


	PlottingCandidate[name_, surname_, city_] := 
		Module[{result},
			result = GetCandidate[name, surname, city];
			If[
				name == "" || surname || "" || city == "",
				Return[Style["Fill in all fields of the form."]]
			];
			
			If[
				result[[1]] == "NOT FOUND",
				Return[Style[StringJoin[{"Candidate ", name, " ", surname, " was not found in the city named ", city, "."}]]]
			];
			
			Return[
				BarChart[
					result[[4]],
					ImageSize -> Large,
					ChartStyle -> "DarkRainbow", 
					ChartLabels -> Placed[
						Normal[result[[2]]], 
						{{0.5, 0}, {0.9, 1}}, 
						Rotate[#, (2/7) Pi] &
					]
				]
			];
		]
	
	GetCandidate[name_, surname_, city_] :=
	     Module[{chamberDatasetSelectBy, senateDatasetSelectBy, returnDataset, returnedLists, uninominaleName},	         
	         (* Applying filters into the dataset of the Chamber of Deputies. *)
	         chamberDatasetSelectBy = chamberDataset;
	         
	         chamberDatasetSelectBy = FilterLastName[chamberDatasetSelectBy, surname];
	         chamberDatasetSelectBy = FilterFirstName[chamberDatasetSelectBy, name];
	         chamberDatasetSelectBy = FilterCity[chamberDatasetSelectBy, city];
	         
	         (* Applying filters into the dataset of the Senate of the Republic. *)
			 (* Little performance extra: the Senate dataset is filtered only if no data is found in the Chamber dataset. *)
	         If[Length[chamberDatasetSelectBy] == 0, (
	             senateDatasetSelectBy = senateDataset;
	         
	             senateDatasetSelectBy = FilterLastName[senateDatasetSelectBy, surname];
	             senateDatasetSelectBy = FilterFirstName[senateDatasetSelectBy, name];
	             senateDatasetSelectBy = FilterCity[senateDatasetSelectBy, city];
	         )];
	         
	         returnedLists = {}; (* Output variable. *)
	         uninominaleName = ""; (* Part of the output variable. *)
	         
	         (* Returning the result. *)
	         If[Length[chamberDatasetSelectBy] > 0, (
	             uninominaleName = chamberDatasetSelectBy[1, DATASETKEYS[[selectedYear]][[UNINOMINALE]]]; (* It is sufficient to get the name of the uninominale from the first row, since a candidate can only be present in one uninominale. *)
	             returnDataset = chamberDataset;
	         )];
	         If[Length[senateDatasetSelectBy] > 0, ((* Either a candidate is present in the Chamber or Senate or in none of the two. *)
	             uninominaleName = senateDatasetSelectBy[1, DATASETKEYS[[selectedYear]][[UNINOMINALE]]];
	             returnDataset = senateDataset;
	         )];
	         If[Length[chamberDatasetSelectBy] == 0 && Length[senateDatasetSelectBy] == 0, (
	             uninominaleName = "NOT FOUND";
	             returnDataset = senateDatasetSelectBy; (* Taking this dataset knowing it is empty but with the right columns to return. *)
	         )];
	         
	         returnDataset = FilterCity[returnDataset, city];
	         returnDataset = returnDataset[DeleteDuplicatesBy[DATASETKEYS[[selectedYear]][[LASTNAME]]]];
	         returnDataset = returnDataset[ReverseSortBy[DATASETKEYS[[selectedYear]][[VOTISOLOCANDUNINOM]]]];
	         returnedLists = {
	             uninominaleName,
	             returnDataset[All, DATASETKEYS[[selectedYear]][[LASTNAME]]],
	             returnDataset[All, DATASETKEYS[[selectedYear]][[FIRSTNAME]]],
	             returnDataset[All, DATASETKEYS[[selectedYear]][[VOTISOLOCANDUNINOM]]]
	         };
	         
	         Return[returnedLists];
	     ]


End[]


EndPackage[]
