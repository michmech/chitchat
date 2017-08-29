concrete ChitchatEng of Chitchat = open SyntaxEng, ParadigmsEng, Predef, Prelude, (ResEng = ResEng) in {

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

	oper person_Quant : NP -> Quant; --me > my, mother -> mother's (why doesn't the RGL have this?)
	oper person_Quant np = lin Quant {s = \\_,_ => np.s!(ResEng.NCase ResEng.Gen); sp = \\_,_,_ => np.s!(ResEng.NCase ResEng.Gen)};

	lincat Person = NP;
	lin P1f, P1m = (mkNP i_Pron);
	lin P2tf, P2tm = (mkNP youSg_Pron);
	lin P2vf, P2vm = (mkNP youPol_Pron);
	lin P3f = mkNP (she_Pron);
	lin P3m = mkNP (he_Pron);
	lin PMother person = mkNP (person_Quant person) (mkN "mother");
	lin PFather person = mkNP (person_Quant person) (mkN "father");

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
	lin QReside person = mkUtt (mkQS (mkQCl where_IAdv (mkCl (person) live_V)));
	lin CResideCountry person country = mkCl (person) (mkVP (mkVP live_V) (SyntaxEng.mkAdv in_Prep country));
	lin CResideCity person city = mkCl (person) (mkVP (mkVP live_V) (SyntaxEng.mkAdv in_Prep city));

	oper come_V : V = mkV "come" "comes";
	lin QOriginate person = mkUtt (mkQS (mkQCl where_IAdv (mkCl (person) (mkVP (mkVP come_V) (ParadigmsEng.mkAdv "from"))  )));
	lin COriginateCountry person country = mkCl (person) (mkVP (mkVP come_V) (SyntaxEng.mkAdv from_Prep country));
	lin COriginateCity person city = mkCl (person) (mkVP (mkVP come_V) (SyntaxEng.mkAdv from_Prep city));

	------

	lincat Settlement = CN;
	lin BigCity = mkCN (mkA "big") (mkN "city");
	lin SmallTown = mkCN (mkA "small") (mkN "town");
	lin Village = mkCN (mkN "village");

	lin CSettlement person settlement = mkCl (person) (mkVP (mkVP live_V) (SyntaxEng.mkAdv in_Prep (mkNP a_Det settlement)));
	lin CSettlementCountry person settlement country = mkCl (person) (mkVP (mkVP (mkVP live_V) (SyntaxEng.mkAdv in_Prep (mkNP a_Det settlement))) (SyntaxEng.mkAdv in_Prep country));

	------

	lincat GBFriend = CN;
	lin Girlfriend = mkCN (mkN "girlfriend");
	lin Boyfriend = mkCN (mkN "boyfriend");

	lincat MaritalStatus = AP;
	lin Single = mkAP (mkA "single");
	lin Married = mkAP (mkA "married");
	lin Divorced = mkAP (mkA "divorced");
	lin Widowed = mkAP (mkA "widowed");

	lin CHaveGBFriend person gbfriendCN = mkCl (person) (mkVP have_V2 (mkNP a_Det gbfriendCN));
	lin CMaritalStatus person status = mkCl (person) status;
	lin QMaritalStatus person = mkUtt (mkQS (mkCl (person) (mkAP or_Conj (mkAP (mkA "married")) (mkAP (mkA "single")))));

	------

	lincat JobStatus = VP;
	lin Student = mkVP (mkNP a_Det (mkN "student"));
	lin Unemployed = mkVP (mkAP (mkA "unemployed"));
	lin SelfEmployed = mkVP (mkAP (mkA "self-employed"));
	lin Retired = mkVP (mkAP (mkA "retired"));

	lin CHaveJob person = mkCl (person) (mkVP have_V2 (mkNP a_Det (mkN "job")));
	lin CJobStatus person status = mkCl (person) status;

	------

	lincat Name = PN;
	lin AName = mkPN "...";

	lin QName person = mkUtt (mkQCl what_IP (mkNP (person_Quant person) (mkCN (mkN "name"))));
	lin CName person name = mkCl (mkNP (person_Quant person) (mkCN (mkN "name"))) (mkNP name);

}
