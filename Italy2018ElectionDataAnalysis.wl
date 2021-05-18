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


LoadDataByYear::usage = "LoadDataByYear[year] loads the dataset for the year given as input to the function."
ShowInterface1::usage = ""
PlottingElectionElectorsPie::usage = "PlottingElectionElectorsPie[house, region: Null, province: Null, district: Null, query: Null] returns a list of data to plot the electors pie chart."
PlottingElectionVotersPie::usage = "PlottingElectionVotersPie[house, region: Null, province: Null, district: Null, query: Null] returns a list of data to plot the voters pie chart."
PlottingElectionVotersNonVotersPie::usage = "PlottingElectionVotersNonVotersPie[house, region: Null, province: Null, district: Null, query: Null] returns a list of data to plot the voters and non voters pie chart."
PlottingElectionRegionCoalitionsBars::usage = "PlottingElectionRegionCoalitionsBars[house, coalition, region: Null, province: Null, district: Null, query: Null] returns a list of data to plot in each region the winning coalition."
PlottingElectionRegionCoalitionsBars3D::usage = ""
PlottingCandidate::usage = "PlottingCandidate[name, surname, city: Null] returns a list of data to plot the histogram for the candidate."
GetChamber::usage = "DEV TOOL GetChamber[] return the Chamber dataset"
GetRegions::usage = "GetRegions[] return the list of regions used in this package"
GetElectionRegionCoalitionsBars::usage = ""


ChamberOfDeputies = "Chamber of Deputies"
SenateOfTheRepublic = "Senate of the Republic"


Begin["`Private`"]


	(* CONSTANTS *)
	
	(* Italian regions *)
	REGIONS = {"ABRUZZO", "BASILICATA", "CALABRIA", "CAMPANIA", "EMILIA-ROMAGNA", "FRIULI-VENEZIA GIULIA", "LAZIO", "LIGURIA", "LOMBARDIA", "MARCHE", "MOLISE", "PIEMONTE", "PUGLIA", "SARDEGNA", "SICILIA", "TOSCANA", "TRENTINO-ALTO ADIGE", "UMBRIA", "VALLE D'AOSTA", "VENETO"};
	
	(* Italian provinces *)
	PROVINCES = Flatten[Values[PROVINCESBYREGION]];
	
	(* Regions and their provinces *)
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
	
	(* Italian electoral districts *)
	DISTRICTS = Flatten[Values[DISTRICTSBYREGION]];
	
	(* Regions and their districts (circoscrizioni) *)
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
	
	(* The following two maps are not representative of the official coalitions that were present during the elections. *)
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


	(* DATA KEYS *)
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
	
	(* CHART LABELS *)
	(* English labels *)
	ENLBLMALEELECTORS = "Male electors";
	ENLBLFEMALEELECTORS = "Female electors";
	ENLBLMALEVOTERS = "Male voters";
	ENLBLFEMALEVOTERS = "Female voters";
	ENLBLMALENONVOTERS = "Male non-voters";
	ENLBLFEMALENONVOTERS = "Female non-voters";
	(* Italian labels *)
	ITLBLMALEELECTORS = "Elettori maschi";
	ITLBLFEMALEELECTORS = "Elettori femmine";
	ITLBLMALEVOTERS = "Votanti maschi";
	ITLBLFEMALEVOTERS = "Votanti femmine";
	ITLBLMALENONVOTERS = "Non votanti maschi";
	ITLBLFEMALENONVOTERS = "Non votanti femmine";


	(* DATA FIELDS *)
	
	(* Year selected by the user *)
	selectedYear;
	
	(* Stores the original election dataset for the Chamber of Deputies (Camera dei Deputati) from the open data website of the Ministry of the Interior. *)
	chamberDataset;
	
	(* Stores the original election dataset for the Senate of the Republic (Senato della Repubblica) from the open data website of the Ministry of the Interior. *)
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
			(* Storing the user selection *)
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
	
	(* d is used instead of district because it does not work otherwise *)
	GetRegionFromDistrict[d_] := StringJoin[If[MatchQ[Characters[d],{__, " ", _}], Take[Characters[d], Length[Characters[d]]-2], d]]
	
	(* Filters the given dataset by the given region *)
	FilterRegion[dataset_, region_] := If[region === Null, dataset, dataset[Select[#[DATASETKEYS[[selectedYear]][[DISTRICT]]] == ToUpperCase[region] &]]]
	
	(* Filters the given dataset by the given province *)
	FilterProvince[dataset_, province_] := If[province === Null, dataset, dataset[Select[#[DATASETKEYS[[selectedYear]][[PROVINCE]]] == ToUpperCase[province] &]]]
	
	(* Filters the given dataset by the given district *)
	FilterDistrict[dataset_, district_] := If[district === Null, dataset, dataset[Select[#[DATASETKEYS[[selectedYear]][[DISTRICT]]] == ToUpperCase[district] &]]]
	
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
	FilterLastName[dataset_, lastname_] := If[lastname === Null, dataset, dataset[Select[#[DATASETKEYS[[selectedYear]][[LASTNAME]]] == ToUpperCase[lastname] &]]]
	
	(* Filters the given dataset by the given first name *)
	FilterFirstName[dataset_, firstname_] := If[firstname === Null, dataset, dataset[Select[#[DATASETKEYS[[selectedYear]][[FIRSTNAME]]] == ToUpperCase[firstname] &]]]
	
    (* Filters the given dataset by the given city *)
	FilterCity[dataset_, city_] := If[city === Null, dataset, dataset[Select[#[DATASETKEYS[[selectedYear]][[CITY]]] == ToUpperCase[city] &]]]


	(* PUBLIC FUNCTIONS (PLOTTING FUNCTIONS) AND PRIVATE DATA EXTRACTION FUNCTIONS (SUPPORT FOR PUBLIC FUNCTIONS) *)


	ShowInterface1[] :=
		DynamicModule[{form, house, region, province, district, query},
			form = Panel[Column[{
				(* Interface components *)
				RadioButtonBar[
					Dynamic[house],
					{ChamberOfDeputies, SenateOfTheRepublic}
				],
				Row[{
					Style["Region  \t"],
					PopupMenu[
						Dynamic[region],
						Join[{"ALL"}, REGIONS],
						FieldSize -> Medium
					](* Free variable *)
				}],
				Row[{
					Style["Province\t"],
					Dynamic[PopupMenu[
						Dynamic[province],
						If[region === "ALL", Join[{"ALL"}, PROVINCES], Join[{"ALL"}, PROVINCESBYREGION[[region]]]],
						FieldSize -> Medium
					]] (* Depends on region *)
				}],
				Row[{
					Style["District  \t"],
					Dynamic[PopupMenu[
						Dynamic[district],
						If[region === "ALL", Join[{"ALL"}, DISTRICTS], Join[{"ALL"}, DISTRICTSBYREGION[[region]]]],
						FieldSize -> Medium
					]] (* Depends on region *)
				}],
				Row[{
					Style["Query\t"],
					InputField[
						Dynamic[query],
						String,
						FieldSize -> Medium
					]
				}]
			}, Center]];
			
			form
		]


	GetRegions[] := REGIONS


	GetChamber[] := chamberDataset


	LoadDataByYear[year_] :=
		(
			chamberDataset = InitDataset[year, ChamberOfDeputies];
			senateDataset = InitDataset[year, SenateOfTheRepublic];
		)


	Options[PlottingElectionElectorsPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionElectorsPie[house_, opts : OptionsPattern[]] := PieChart[GetElectionElectorsPie[house, opts], ChartLegends->{ENLBLMALEELECTORS, ENLBLFEMALEELECTORS}, ChartStyle->{Blue, Red}]
	
	Options[GetElectionElectorsPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	GetElectionElectorsPie[house_, opts : OptionsPattern[]] := 
		Module[{dataset, datasetSelectBy, maleElectors, femaleElectors},		    
		    (* Dataset selection *)
			dataset = If[ToUpperCase[house] === ToUpperCase[ChamberOfDeputies], chamberDataset, senateDataset];
			datasetSelectBy = dataset[DeleteDuplicatesBy[DATASETKEYS[[selectedYear]][[CITY]]]]; (* General data on the elections are copied in each row for every candidate and party in a city, therefore we can remove the duplicates *)
			
			(* Applying filters *)
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			
			(* Returning the result *)
			maleElectors = Total[datasetSelectBy[All, DATASETKEYS[[selectedYear]][[MALEELECTORS]]]];
			femaleElectors = Total[datasetSelectBy[All, DATASETKEYS[[selectedYear]][[FEMALEELECTORS]]]];
			Return[{maleElectors, femaleElectors}]
		]


	Options[PlottingElectionVotersPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionVotersPie[house_, opts : OptionsPattern[]] := PieChart[GetElectionVotersPie[house, opts], ChartLegends->{ENLBLMALEVOTERS, ENLBLFEMALEVOTERS}, ChartStyle->{Blue, Red}]
	
	Options[GetElectionVotersPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	GetElectionVotersPie[house_, opts : OptionsPattern[]] :=
		Module[{dataset, datasetSelectBy, maleVoters, femaleVoters},
		    (* Dataset selection *)
			dataset = If[ToUpperCase[house] === ToUpperCase[ChamberOfDeputies], chamberDataset, senateDataset];
			datasetSelectBy = dataset[DeleteDuplicatesBy[DATASETKEYS[[selectedYear]][[CITY]]]];
			
			(* Applying filters *)
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			
			(* Returning the result *)
			maleVoters = Total[datasetSelectBy[All, DATASETKEYS[[selectedYear]][[MALEVOTERS]]]];
			femaleVoters = Total[datasetSelectBy[All, DATASETKEYS[[selectedYear]][[FEMALEVOTERS]]]];
			Return[{maleVoters, femaleVoters}]
		]


	Options[PlottingElectionVotersNonVotersPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionVotersNonVotersPie[house_, opts : OptionsPattern[]] := PieChart[GetElectionVotersNonVotersPie[house, opts], ChartLegends->{ENLBLMALEVOTERS, ENLBLFEMALEVOTERS, ENLBLMALENONVOTERS, ENLBLFEMALENONVOTERS}, ChartStyle->{Blue, Red, Hue[0.65,0.5,1], Hue[0.03,0.46,1]}]
			
	Options[GetElectionVotersNonVotersPie] = {region -> Null, province -> Null, district -> Null, query -> Null};
	GetElectionVotersNonVotersPie[house_, opts : OptionsPattern[]] :=
		Module[{dataset, datasetSelectBy, maleVoters, femaleVoters, maleElectors, femaleElectors}, 
		    (* Dataset selection *)
			dataset = If[ToUpperCase[house] === ToUpperCase[ChamberOfDeputies], chamberDataset, senateDataset];
			datasetSelectBy = dataset[DeleteDuplicatesBy[DATASETKEYS[[selectedYear]][[CITY]]]];
			
			(* Applying filters *)
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			
			(* Returning the result *)
			maleElectors = Total[datasetSelectBy[All, DATASETKEYS[[selectedYear]][[MALEELECTORS]]]];
			femaleElectors = Total[datasetSelectBy[All, DATASETKEYS[[selectedYear]][[FEMALEELECTORS]]]];
			maleVoters = Total[datasetSelectBy[All, DATASETKEYS[[selectedYear]][[MALEVOTERS]]]];
			femaleVoters = Total[datasetSelectBy[All, DATASETKEYS[[selectedYear]][[FEMALEVOTERS]]]];
			
			Return[{maleVoters, femaleVoters, (maleElectors - maleVoters), (femaleElectors - femaleVoters)}]
		]


	pairUp[xValues_, yValues_] := ({xValues[[#]], yValues[[#]]})&/@Range[Min[Length[xValues], Length[yValues]]];
	
	Options[PlottingElectionRegionCoalitionsBars] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionRegionCoalitionsBars[house_, opts : OptionsPattern[]] :=
		Module[{divisions, divisionsVotes, divisionsColorVotes},
			divisions = EntityValue[Entity["AdministrativeDivision",{EntityProperty["AdministrativeDivision","ParentRegion"]->Entity["Country","Italy"]}],"Entities"]; (*TODO: associare divisioni ottenute da Mathematica a regioni in regions*)
			divisionsVotes = Table[Transpose @ {divisions, GetElectionRegionCoalitionsBars[house, coalition, opts]}, {coalition, {"Sinistra", "Centro", "Destra"}}];
			divisionsColorVotes = pairUp[divisionsVotes, {{"Sinistra", ColorData[{"ValentineTones", "Reverse"}]}, {"Centro", ColorData[{"SiennaTones", "Reverse"}]}, {"Destra", ColorData[{"AvocadoColors", "Reverse"}]}}];
			Return[Table[GeoRegionValuePlot[rvc[[1]], PlotLabel->rvc[[2, 1]], ColorFunction->rvc[[2, 2]]], {rvc, divisionsColorVotes}]]
		]
		
	Options[PlottingElectionRegionCoalitionsBars] = {region -> Null, province -> Null, district -> Null, query -> Null};
	PlottingElectionRegionCoalitionsBars3D[house_, opts : OptionsPattern[]] :=
		Module[{regions, temp, votes, centralCoordinates, polygons, coord3D, graphBar3D},
			regions = Entity["Country", "Italy"][EntityProperty["Country", "AdministrativeDivisions"]];
			temp = GetElectionRegionCoalitionsBars[ChamberOfDeputies, "Centro"];
			(* Sia GetElectionRegionCoalitionsBars che Entity considerano le regioni ordinate in maniera alfabetica: tuttavia, la 1\[Degree]
			funzione usa i nomi italiani, la 2\[Degree] i nomi inglesi, per cui \[EGrave] necessaria un sorto solo per "Apulia" = "Puglia" *)
			votes = Join[Take[temp, 1], Take[temp, {3, 13}], Take[temp, {2, 2}], Take[temp, {14, 20}]];
			centralCoordinates = Reverse /@ EntityValue[regions, EntityProperty["AdministrativeDivision", "Coordinates"]];
			polygons = EntityValue[regions, EntityProperty["AdministrativeDivision", "Polygon"]];
			Return[
				Table[
				{coord3D = Partition[
					Flatten[
						Transpose@{centralCoordinates, GetElectionRegionCoalitionsBars[ChamberOfDeputies, coalitions]/1000000}], 3];
				graphBar3D = Graphics3D[{Yellow, Cuboid[{#1, #2, 0}, {#1 + .2, #2 + .2, #3}] & @@@ coord3D}, Axes -> False]},
				{coalitions, {"Sinistra",  "Centro", "Destra"}}]]
		]
	
	Options[GetElectionRegionCoalitionsBars] = {region -> Null, province -> Null, district -> Null, query -> Null};
	GetElectionRegionCoalitionsBars[house_, coalition_, opts : OptionsPattern[]] :=
		Module[{dataset, parties, datasetSelectBy},
		    (* Dataset selection *)
			dataset = If[ToUpperCase[house] === ToUpperCase[ChamberOfDeputies], chamberDataset, senateDataset];
			parties = If[ToUpperCase[house] === ToUpperCase[ChamberOfDeputies], COALITIONSCHAMBER[[selectedYear]][[coalition]], COALITIONSSENATE[[selectedYear]][[coalition]]];
			datasetSelectBy = dataset;			
			
			(* Applying filters *)
			datasetSelectBy = FilterRegion[datasetSelectBy, OptionValue[region]];
			datasetSelectBy = FilterProvince[datasetSelectBy, OptionValue[province]];
			datasetSelectBy = FilterDistrict[datasetSelectBy, OptionValue[district]];
			datasetSelectBy = FilterQuery[datasetSelectBy, OptionValue[query]];
			
			(* Returning the result *)
			Return[Table[Total[datasetSelectBy[Select[GetRegionFromDistrict[#[DATASETKEYS[[selectedYear]][[DISTRICT]]]] == r&]][Select[MemberQ[parties, #[DATASETKEYS[[selectedYear]][[COALITION]]]] &]][All, DATASETKEYS[[selectedYear]][[VOTICANDUNINOM]]]], {r, REGIONS}]]
		]


	Options[PlottingCandidate] = {city -> Null};
	PlottingCandidate[name_, surname_, opts : OptionsPattern[]] := 
		Module[{result},
			result = GetCandidate[name, surname, opts];
			Return[BarChart[result[[4]], ChartLegends -> result[[2]][All], ImageSize -> Large, ChartStyle -> "DarkRainbow"]];
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
	             uninominaleName = chamberDatasetSelectBy[1, DATASETKEYS[[selectedYear]][[UNINOMINALE]]]; (* It is sufficient to get the name of the uninominale from the first row, since a candidate can only be present in one uninominale *)
	             returnDataset = chamberDataset;
	         )];
	         If[Length[senateDatasetSelectBy] > 0, ((* Either a candidate is present in the Chamber or Senate or in none of the two *)
	             uninominaleName = senateDatasetSelectBy[1, DATASETKEYS[[selectedYear]][[UNINOMINALE]]];
	             returnDataset = senateDataset;
	         )];
	         If[Length[chamberDatasetSelectBy] == 0 && Length[senateDatasetSelectBy] == 0, (
	             uninominaleName = "NOT FOUND";
	             returnDataset = senateDatasetSelectBy; (* Taking this dataset knowing it is empty but with the right columns to return *)
	         )];
	         
	         returnDataset = FilterCity[returnDataset, OptionValue[city]];
	         returnDataset = returnDataset[DeleteDuplicatesBy[DATASETKEYS[[selectedYear]][[LASTNAME]]]];
	         returnDataset = returnDataset[ReverseSortBy[DATASETKEYS[[selectedYear]][[VOTISOLOCANDUNINOM]]]];
	         returnedLists = {uninominaleName, returnDataset[All, DATASETKEYS[[selectedYear]][[LASTNAME]]], returnDataset[All, DATASETKEYS[[selectedYear]][[FIRSTNAME]]], returnDataset[All, DATASETKEYS[[selectedYear]][[VOTISOLOCANDUNINOM]]]};
	         
	         Return[returnedLists];
	     ]


End[]


EndPackage[]
