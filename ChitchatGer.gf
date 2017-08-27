concrete ChitchatGer of Chitchat = open SyntaxGer, ParadigmsGer, Predef in {

	lincat Sentence = Text;
	lin SayQuestion question = mkText question questMarkPunct;
	lin SayStatement statement = mkText statement fullStopPunct;
	lin SayYesStatement statement = mkText yes (mkText statement fullStopPunct);
	lin SayNoStatement statement = mkText no (mkText statement fullStopPunct);
	lin SayDenial denial = mkText denial fullStopPunct;
	lin SayNoDenial denial = mkText no (mkText denial fullStopPunct);
	oper yes : Text = lin Text {s = "ja ,"};
	oper no : Text = lin Text {s = "nein ,"};

	lincat Question, Statement, Denial = Utt;
	lin Ask clause = mkUtt (mkQS presentTense (mkQCl clause.pos));
	lin State clause = mkUtt (mkS presentTense clause.pos);
	lin Deny clause = mkUtt clause.neg;

	lincat Clause = {pos : Cl; neg : S};

	------

	lincat Person = Pron;
	lin P1f, P1m = i_Pron;
	lin P2tf, P2tm = youSg_Pron;
	lin P2vf, P2vm = youPol_Pron;
	lin P3f = she_Pron;
	lin P3m = he_Pron;

	lincat Country = NP;
	lin Ireland = mkNP (mkPN "Irland");
	lin CzechRepublic = mkNP the_Det (mkCN (mkAP (mkA "Tschechisch")) (mkN "Republik" feminine));
	lin Latvia = mkNP (mkPN "Lettland");

	lincat City = NP;
	lin Dublin = mkNP (mkPN "Dublin");
	lin Brno = mkNP (mkPN "Brünn");
	lin Riga = mkNP (mkPN "Riga");

	------

	oper wohnen_V : V = mkV "wohnen";
	lin QReside person = mkUtt (mkQS (mkQCl where_IAdv (mkCl (mkNP person) wohnen_V)));
	lin CResideCountry person country = {
		pos = mkCl (mkNP person) (mkVP (mkVP wohnen_V) (SyntaxGer.mkAdv in_Prep country));
		neg = mkS presentTense negativePol (mkCl (mkNP person) (mkVP (mkVP wohnen_V) (SyntaxGer.mkAdv in_Prep country)))
	};
	lin CResideCity person city = {
		pos = mkCl (mkNP person) (mkVP (mkVP wohnen_V) (SyntaxGer.mkAdv in_Prep city));
		neg = mkS presentTense negativePol (mkCl (mkNP person) (mkVP (mkVP wohnen_V) (SyntaxGer.mkAdv in_Prep city)))
	};

	------

	lincat Settlement = CN;
	lin BigCity = mkCN (mkA "groß") (mkN "Stadt" feminine);
	lin SmallTown = mkCN (mkA "klein") (mkN "Stadt" feminine);
	lin Village = mkCN (mkN "Dorf" neuter);

	lin CSettlement person settlement = {
		pos = mkCl (mkNP person) (mkVP (mkVP wohnen_V) (SyntaxGer.mkAdv in_Prep (mkNP a_Det settlement)));
		neg = mkS presentTense negativePol (mkCl (mkNP person) (mkVP (mkVP wohnen_V) (SyntaxGer.mkAdv in_Prep (mkNP a_Det settlement))))
	};
	lin CSettlementCountry person settlement country = {
		pos = mkCl (mkNP person) (mkVP (mkVP (mkVP wohnen_V) (SyntaxGer.mkAdv in_Prep (mkNP a_Det settlement))) (SyntaxGer.mkAdv in_Prep country));
		neg = mkS presentTense negativePol (mkCl (mkNP person) (mkVP (mkVP (mkVP wohnen_V) (SyntaxGer.mkAdv in_Prep (mkNP a_Det settlement))) (SyntaxGer.mkAdv in_Prep country)))
	};

	------

	lincat GBFriend = CN;
	lin Girlfriend = mkCN (mkN "Geliebte" feminine);
	lin Boyfriend = mkCN (mkN "Geliebte" masculine);

	lincat MaritalStatus = AP;
	lin Single = mkAP (mkA "ledig");
	lin Married = mkAP (mkA "verheiratet");
	lin Divorced = mkAP (mkA "geschieden");
	lin Widowed = mkAP (mkA "verwitwet");

	lin CHaveGBFriend person gbfriendCN = {
		pos = mkCl (mkNP person) (mkVP have_V2 (mkNP a_Det gbfriendCN));
		neg = mkS (mkCl (mkNP person) (mkVP have_V2 (mkNP no_Quant gbfriendCN)));
	};
	lin CMaritalStatus person status = {
		pos = mkCl (mkNP person) status;
		neg = mkS presentTense negativePol (mkCl (mkNP person) status);
	};
	lin QMaritalStatus person = mkUtt (mkQS (mkCl (mkNP person) (mkAP or_Conj (mkAP (mkA "verheiratet")) (mkAP (mkA "ledig")))));

	------

	lincat JobStatus = VP;
	lin Student = mkVP (mkNP (mkN "Student"));
	lin Unemployed = mkVP (mkAP (mkA "arbeitslos"));
	lin SelfEmployed = mkVP (mkAP (mkA "selbstständig"));
	lin Retired = mkVP (ParadigmsGer.mkAdv "im Ruhestand");

	lin CHaveJob person = {
		pos = mkCl (mkNP person) (mkVP have_V2 (mkNP a_Det (mkN "Job" masculine)));
		neg = mkS (mkCl (mkNP person) (mkVP have_V2 (mkNP no_Quant (mkN "Job" masculine))))
	};
	lin CJobStatus person status = {
		pos = mkCl (mkNP person) status;
		neg = mkS presentTense negativePol (mkCl (mkNP person) status)
	};

	------

	lincat Name = PN;
	lin AName = mkPN "...";

	--lin QName person = mkUtt (mkQCl what_IP (mkNP person (mkN "Name" masculine)));
	lin QName person = mkUtt (mkQCl how_IAdv (mkCl (mkNP person) (mkVP (mkV "heißen"))));
	lin CName person name = {
		pos = mkCl (mkNP person) (mkVP (mkV2 "heißen") (mkNP AName));
		neg = mkS presentTense negativePol (mkCl (mkNP person) (mkVP (mkV2 "heißen") (mkNP AName)))
	};

}
