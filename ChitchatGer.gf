concrete ChitchatGer of Chitchat = open SyntaxGer, ParadigmsGer, Predef, Prelude in {

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

	lincat Clause = {pos : Cl; neg : S}; --we keep the negative separately because of the pesky German kein-negative

	-----------
	--Templates
	-----------

	oper wohnen_V : V = mkV "wohnen";
	oper kommen_V : V = mkV "kommen";
	oper woher_IAdv = ss "woher" ;
	oper aus_Prep = mkPrep "aus" dative;

	lin QReside person = mkUtt (mkQS (mkQCl where_IAdv (mkCl person.np wohnen_V)));
	lin CResideCountry person country = {
		pos = mkCl person.np (mkVP (mkVP wohnen_V) (SyntaxGer.mkAdv in_Prep country));
		neg = mkS presentTense negativePol (mkCl person.np (mkVP (mkVP wohnen_V) (SyntaxGer.mkAdv in_Prep country)))
	};
	lin CResideCity person city = {
		pos = mkCl person.np (mkVP (mkVP wohnen_V) (SyntaxGer.mkAdv in_Prep city));
		neg = mkS presentTense negativePol (mkCl person.np (mkVP (mkVP wohnen_V) (SyntaxGer.mkAdv in_Prep city)))
	};

	lin QOriginate person = mkUtt (mkQS (mkQCl woher_IAdv (mkCl person.np kommen_V)));
	lin COriginateCountry person country = {
		pos = mkCl person.np (mkVP (mkVP kommen_V) (SyntaxGer.mkAdv aus_Prep country));
		neg = mkS presentTense negativePol (mkCl person.np (mkVP (mkVP kommen_V) (SyntaxGer.mkAdv aus_Prep country)))
	};
	lin COriginateCity person city = {
		pos = mkCl person.np (mkVP (mkVP kommen_V) (SyntaxGer.mkAdv aus_Prep city));
		neg = mkS presentTense negativePol (mkCl person.np (mkVP (mkVP kommen_V) (SyntaxGer.mkAdv aus_Prep city)))
	};

	lin CSettlement person settlement = {
		pos = mkCl person.np (mkVP (mkVP wohnen_V) (SyntaxGer.mkAdv in_Prep (mkNP a_Det settlement)));
		neg = mkS presentTense negativePol (mkCl person.np (mkVP (mkVP wohnen_V) (SyntaxGer.mkAdv in_Prep (mkNP a_Det settlement))))
	};
	lin CSettlementCountry person settlement country = {
		pos = mkCl person.np (mkVP (mkVP (mkVP wohnen_V) (SyntaxGer.mkAdv in_Prep (mkNP a_Det settlement))) (SyntaxGer.mkAdv in_Prep country));
		neg = mkS presentTense negativePol (mkCl person.np (mkVP (mkVP (mkVP wohnen_V) (SyntaxGer.mkAdv in_Prep (mkNP a_Det settlement))) (SyntaxGer.mkAdv in_Prep country)))
	};

	lin CHaveGBFriend person gbfriendCN = {
		pos = mkCl person.np (mkVP have_V2 (mkNP a_Det gbfriendCN));
		neg = mkS (mkCl person.np (mkVP have_V2 (mkNP no_Quant gbfriendCN)));
	};
	lin CMaritalStatus person status = {
		pos = mkCl person.np status;
		neg = mkS presentTense negativePol (mkCl person.np status);
	};
	lin QMaritalStatus person = mkUtt (mkQS (mkCl person.np (mkAP or_Conj (mkAP (mkA "verheiratet")) (mkAP (mkA "ledig")))));

	lin CHaveJob person = {
		pos = mkCl person.np (mkVP have_V2 (mkNP a_Det (mkN "Job" masculine)));
		neg = mkS (mkCl person.np (mkVP have_V2 (mkNP no_Quant (mkN "Job" masculine))))
	};
	lin CJobStatus person status = {
		pos = mkCl person.np status;
		neg = mkS presentTense negativePol (mkCl person.np status)
	};

	lin QName person = mkUtt (mkQCl how_IAdv (mkCl person.np (mkVP (mkV "heißen"))));
	lin CName person name = {
		pos = mkCl person.np (mkVP (mkV2 "heißen") (mkNP AName));
		neg = mkS presentTense positivePol (mkCl person.np (mkVP (mkV2 "heißen") (mkNP not_Predet (mkNP AName))))
	};

	----------
	--Entities
	----------

	oper P : Type = {isPron : PBool; pron : Pron; np : NP};
	lincat Person = P;
	lin P1f, P1m = {isPron = PTrue; pron = i_Pron; np = mkNP i_Pron};
	lin P2tf, P2tm = {isPron = PTrue; pron = youSg_Pron; np = mkNP youSg_Pron};
	lin P2vf, P2vm = {isPron = PTrue; pron = youPol_Pron; np = mkNP youPol_Pron};
	lin P3f = {isPron = PTrue; pron = she_Pron; np = mkNP she_Pron};
	lin P3m = {isPron = PTrue; pron = he_Pron; np = mkNP he_Pron};

	oper possNP : P -> CN -> NP;
	oper possNP person cn = case person.isPron of {
		PTrue => mkNP person.pron cn; --meine Mutter
		PFalse => mkNP the_Det (mkCN cn (SyntaxGer.mkAdv von_Prep person.np)) --die Mutter von meiner Mutter
	};
	lin PMother person = {isPron = PFalse; pron = person.pron; np = possNP person (mkCN (mkN "Mutter" feminine))};
	lin PFather person = {isPron = PFalse; pron = person.pron; np = possNP person (mkCN (mkN "Vater" masculine))};

	lincat Country = NP;
	lin Ireland = mkNP (mkPN "Irland");
	lin CzechRepublic = mkNP the_Det (mkCN (mkAP (mkA "Tschechisch")) (mkN "Republik" feminine));
	lin Latvia = mkNP (mkPN "Lettland");

	lincat City = NP;
	lin Dublin = mkNP (mkPN "Dublin");
	lin Brno = mkNP (mkPN "Brünn");
	lin Riga = mkNP (mkPN "Riga");

	lincat Settlement = CN;
	lin BigCity = mkCN (mkA "groß") (mkN "Stadt" feminine);
	lin SmallTown = mkCN (mkA "klein") (mkN "Stadt" feminine);
	lin Village = mkCN (mkN "Dorf" neuter);

	lincat GBFriend = CN;
	lin Girlfriend = mkCN (mkN "Geliebte" feminine);
	lin Boyfriend = mkCN (mkN "Geliebte" masculine);

	lincat MaritalStatus = AP;
	lin Single = mkAP (mkA "ledig");
	lin Married = mkAP (mkA "verheiratet");
	lin Divorced = mkAP (mkA "geschieden");
	lin Widowed = mkAP (mkA "verwitwet");

	lincat JobStatus = VP;
	lin Student = mkVP (mkNP (mkN "Student"));
	lin Unemployed = mkVP (mkAP (mkA "arbeitslos"));
	lin SelfEmployed = mkVP (mkAP (mkA "selbstständig"));
	lin Retired = mkVP (ParadigmsGer.mkAdv "im Ruhestand");

	lincat Name = PN;
	lin AName = mkPN "...";
}
