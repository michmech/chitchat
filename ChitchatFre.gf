--# -path=.:present
concrete ChitchatFre of Chitchat = open (CR = CommonRomance), SyntaxFre, ParadigmsFre, IrregFre, ExtraFre, NounFre, Predef, Prelude, (ResFre = ResFre) in{

  lincat Sentence = Text;
  lin SayQuestion question = mkText question questMarkPunct;
  lin SayStatement statement = mkText statement fullStopPunct;
  lin SayYesStatement statement = mkText yes (mkText statement fullStopPunct);
  lin SayNoStatement statement = mkText no (mkText statement fullStopPunct);
  lin SayDenial denial = mkText denial fullStopPunct;
  lin SayNoDenial denial = mkText no (mkText denial fullStopPunct);
  oper yes : Text = lin Text {s = "oui ,"};
  oper no : Text = lin Text {s = "non ,"};

  lincat Question, Statement, Denial = Utt;
  lin Ask clause = mkUtt (mkQS presentTense (mkQCl clause));
  lin State clause = mkUtt (mkS presentTense clause);
  lin Deny clause = mkUtt (mkS presentTense negativePol clause);

  lincat Clause = Cl;

  -----------
  --Templates
  -----------

  oper habiter_V : V = mkV "habiter";
  oper appeler_V : V = reflV (mkV "appeler");

  lin QReside person = mkUtt (mkQS (mkQCl where_IAdv (mkCl (person.np) habiter_V)));

  lin CResideCountry person country = case country.g of {
	CR.Fem  => mkCl (person.np) (mkVP (mkVP habiter_V) (SyntaxFre.mkAdv (mkPrep "en") (mkNP country.pn)));
	CR.Masc => mkCl (person.np) (mkVP (mkVP habiter_V) (SyntaxFre.mkAdv to_Prep country.np))
	};

  lin CResideCity person city = mkCl (person.np) (mkVP (mkVP habiter_V) (SyntaxFre.mkAdv to_Prep city));

  lin QOriginate person = mkUtt (mkQS (mkQCl (lin IAdv (ss "d'où")) (mkCl (person.np) (mkVP venir_V))));

  lin COriginateCountry person country = case country.g of {
	CR.Fem => mkCl (person.np) (mkVP (mkVP venir_V) (SyntaxFre.mkAdv from_Prep (mkNP country.pn)));
	CR.Masc => mkCl (person.np) (mkVP (mkVP venir_V) (SyntaxFre.mkAdv from_Prep country.np))
	};

  lin COriginateCity person city = mkCl (person.np) (mkVP (mkVP venir_V) (SyntaxFre.mkAdv from_Prep city));

  lin CSettlement person settlement = mkCl (person.np) (mkVP (mkVP habiter_V) (SyntaxFre.mkAdv in_Prep (mkNP a_Det settlement)));


  lin CSettlementCountry person settlement country = case country.g of {
	CR.Fem => mkCl (person.np) (mkVP (mkVP (mkVP habiter_V) (SyntaxFre.mkAdv in_Prep (mkNP a_Det settlement))) (SyntaxFre.mkAdv (mkPrep "en") (mkNP country.pn)));
	CR.Masc => mkCl (person.np) (mkVP (mkVP (mkVP habiter_V) (SyntaxFre.mkAdv in_Prep (mkNP a_Det settlement))) (SyntaxFre.mkAdv to_Prep country.np))
	};

  lin CHaveGBFriend person gbfriendCN = mkCl (person.np) (mkVP have_V2 (mkNP a_Det gbfriendCN));

  lin CMaritalStatus person status = mkCl (person.np) status;

  lin QMaritalStatus person = mkUtt (mkQS (mkCl (person.np) (mkAP or_Conj single_AP married_AP)));

      -- TODO: For the moment "I don't have a job" is linearised as "je n'ai pas un travail"
      -- It would be better to have "je n'ai pas de travail", but that needs a more fundamental rewrite.
  lin CHaveJob person = mkCl (person.np) (mkVP have_V2 (mkNP a_Det (mkN "travail")));

  lin CJobStatus person status = case status.isAP of {
	True => mkCl person.np status.ap ;
	False => mkCl person.np status.compl
	};

  lin QName person = mkUtt (mkQCl how_IAdv (mkCl person.np appeler_V));
  lin CName person name = mkCl person.np (mkV2 appeler_V) (mkNP name);

  ----------
  --Entities
  ----------
  oper P : Type = {isPron : Bool; pron : Pron; np : NP};
  lincat Person = P;

  oper mkPerson = overload {
	 mkPerson : Pron -> Person = \p -> lin Person {isPron = True; pron = p; np = mkNP p};
	 mkPerson : N -> P -> Person = \n,p -> lin Person {isPron = False; pron = p.pron; np = case p.isPron of {
											    True => mkNP p.pron n;
											    False => mkNP the_Det (PossNP (mkCN n) p.np)
											    }
	   }
	 };
  lin P1f  = mkPerson i8fem_Pron;
  lin P1m  = mkPerson i_Pron;
  lin P2tf = mkPerson youSg8fem_Pron;
  lin P2tm = mkPerson youSg_Pron;
  lin P2vf = mkPerson youPol8fem_Pron;
  lin P2vm = mkPerson youPol_Pron;
  lin P3f  = mkPerson she_Pron;
  lin P3m  = mkPerson he_Pron;

  lin PMother person = mkPerson (mkN "mère") person;
  lin PFather person = mkPerson (mkN "père" "pères" masculine ) person;


  -- Since the gender of a country decides which prepositions to use, and
  -- whether to use the definite article or not, we create a new type for
  -- the linearisation of countries.
  -- TODO: Handle countries that are plural.
  oper C : Type = {pn : PN; np : NP; g : Gender};
  lincat Country = C;
  oper mkCountry : N -> Country = \n -> lin Country {pn = mkPN n; np = mkNP the_Det n; g = (mkPN n) . g};


  lin Ireland = mkCountry (mkN "Irlande" feminine);
  lin CzechRepublic = mkCountry (mkN "Tchéquie" feminine);
  lin Latvia = mkCountry (mkN "Lettonie" masculine);

  lincat City = NP;
  lin Dublin = mkNP (mkPN "Dublin");
  lin Brno = mkNP (mkPN "Brno");
  lin Riga = mkNP (mkPN "Riga");

  lincat Settlement = CN;
  lin BigCity = mkCN (prefA (mkA "grand")) (mkN "ville");
  lin SmallTown = mkCN (prefA (mkA "petit")) (mkN "ville");
  lin Village = mkCN (mkN "village");

  lincat GBFriend = CN;
  lin Girlfriend = mkCN (prefA (mkA "petit")) (mkN "copine");
  lin Boyfriend = mkCN (prefA (mkA "petit")) (mkN "copain");

  lincat MaritalStatus = AP;
  oper single_AP = mkAP (mkA "célibataire");
  lin Single = single_AP;
  oper married_AP = mkAP (mkA "marié");
  lin Married = married_AP;
  lin Divorced = mkAP (mkA "divorcé");
  lin Widowed = mkAP (mkA "veuf" "veuve");

  oper APorAdv : Type = {isAP : Bool; ap : AP; compl : Adv};
  oper mkAPorAdv = overload {
	 insAP : AP -> APorAdv = \ap ->  {isAP = True; ap = ap; compl = ParadigmsFre.mkAdv "nul"};
	 insAdv: Adv -> APorAdv = \compl -> {isAP = False; ap = mkAP (mkA "nul"); compl = compl};
	 };
  lincat JobStatus = APorAdv;
  lin Student = mkAPorAdv (mkAP (mkA "étudiant"));
  lin Unemployed = mkAPorAdv (SyntaxFre.mkAdv to_Prep  (mkNP the_Det (mkN "chomage" masculine)));
  lin SelfEmployed = mkAPorAdv (mkAP (mkA "entrepreneur" "entrepreneur"));
  lin Retired = mkAPorAdv (mkAP (mkA "retraité"));

  lincat Name = PN;
  lin AName = mkPN "...";

}
