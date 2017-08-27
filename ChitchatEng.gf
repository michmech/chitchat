concrete ChitchatEng of Chitchat = open SyntaxEng, ParadigmsEng, Predef in {

	lincat Sentence = Text;
	lin SayQuestion question = mkText question questMarkPunct;
	lin SayStatement statement = mkText statement fullStopPunct;
	lin SayYesStatement statement = mkText yes (mkText statement fullStopPunct);
	lin SayNoStatement statement = mkText no (mkText statement fullStopPunct);
	lin SayDenial denial = mkText denial fullStopPunct;
	lin SayNoDenial denial = mkText no (mkText denial fullStopPunct);
	oper yes : Text = lin Text {s = "yes ,"};
	oper no : Text = lin Text {s = "no ,"};

	lincat Question, Statement, Denial = Utt;
	lin Ask clause = mkUtt (mkQS presentTense (mkQCl clause));
	lin State clause = mkUtt (mkS presentTense clause);
	lin Deny clause = mkUtt (mkS presentTense negativePol clause);

	lincat Clause = Cl;

	------

	lincat Person = Pron;
	lin P1f, P1m = i_Pron;
	lin P2tf, P2tm = youSg_Pron;
	lin P2vf, P2vm = youPol_Pron;
	lin P3f = she_Pron;
	lin P3m = he_Pron;

	lincat Country = NP;
	lin Ireland = mkNP (mkPN "Ireland");
	lin CzechRepublic = mkNP (mkPN "the Czech Republic");
	lin Latvia = mkNP (mkPN "Latvia");

	lincat City = NP;
	lin Dublin = mkNP (mkPN "Dublin");
	lin Brno = mkNP (mkPN "Brno");
	lin Riga = mkNP (mkPN "Riga");

	------

	oper live_V : V = mkV "live" "lives";
	lin QReside person = mkUtt (mkQS (mkQCl where_IAdv (mkCl (mkNP person) live_V)));
	lin CResideCountry person country = mkCl (mkNP person) (mkVP (mkVP live_V) (SyntaxEng.mkAdv in_Prep country));
	lin CResideCity person city = mkCl (mkNP person) (mkVP (mkVP live_V) (SyntaxEng.mkAdv in_Prep city));

	------

	lincat Settlement = CN;
	lin BigCity = mkCN (mkA "big") (mkN "city");
	lin SmallTown = mkCN (mkA "small") (mkN "town");
	lin Village = mkCN (mkN "village");

	lin CSettlement person settlement = mkCl (mkNP person) (mkVP (mkVP live_V) (SyntaxEng.mkAdv in_Prep (mkNP a_Det settlement)));
	lin CSettlementCountry person settlement country = mkCl (mkNP person) (mkVP (mkVP (mkVP live_V) (SyntaxEng.mkAdv in_Prep (mkNP a_Det settlement))) (SyntaxEng.mkAdv in_Prep country));

	------

	lincat GBFriend = CN;
	lin Girlfriend = mkCN (mkN "girlfriend");
	lin Boyfriend = mkCN (mkN "boyfriend");

	lincat MaritalStatus = AP;
	lin Single = mkAP (mkA "single");
	lin Married = mkAP (mkA "married");
	lin Divorced = mkAP (mkA "divorced");
	lin Widowed = mkAP (mkA "widowed");

	lin CHaveGBFriend person gbfriendCN = mkCl (mkNP person) (mkVP have_V2 (mkNP a_Det gbfriendCN));
	lin CMaritalStatus person status = mkCl (mkNP person) status;
	lin QMaritalStatus person = mkUtt (mkQS (mkCl (mkNP person) (mkAP or_Conj (mkAP (mkA "married")) (mkAP (mkA "single")))));

	------

	lincat JobStatus = VP;
	lin Student = mkVP (mkNP a_Det (mkN "student"));
	lin Unemployed = mkVP (mkAP (mkA "unemployed"));
	lin Employed = mkVP (mkAP (mkA "employed"));
	lin SelfEmployed = mkVP (mkAP (mkA "self-employed"));
	lin Retired = mkVP (mkAP (mkA "retired"));

	lin CHaveJob person = mkCl (mkNP person) (mkVP have_V2 (mkNP a_Det (mkN "job")));
	lin CJobStatus person status = mkCl (mkNP person) status;

	------

	lincat LifeStruggle = CN;
	lin PhD = mkCN (mkN "PhD");
	lin Marriage = mkCN (mkN "marriage");
	lin Project = mkCN (mkN "project");

	lincat Goodness = A;
	lin Good = mkA "good" "better" "best" "well";
	lin Bad = mkA "bad";

	lin QLifeStruggle person struggle = mkUtt (mkQS (mkQCl how_IAdv (mkCl (mkNP person struggle) (progressiveVP (mkVP (mkV "go"))))));
	lin CLifeStruggle person struggle goodness = mkCl (mkNP person struggle) (progressiveVP (mkVP (mkVP (mkV "go")) (SyntaxEng.mkAdv goodness)));

	------

	

}
