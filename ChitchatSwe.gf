concrete ChitchatSwe of Chitchat = open SyntaxSwe, ParadigmsSwe, ExtraSwe, Predef, Prelude, ResSwe in {

  lincat Sentence = Text;
  lin SayQuestion question = mkText question questMarkPunct;
  lin SayStatement statement = mkText statement fullStopPunct;
  lin SayYesStatement statement = mkText yes (mkText statement fullStopPunct);
  lin SayNoStatement statement = mkText no (mkText statement fullStopPunct);
  lin SayDenial denial = mkText denial fullStopPunct;
  lin SayNoDenial denial = mkText no (mkText denial fullStopPunct);
  oper yes : Text = lin Text {s = "ja ,"};
  oper no : Text = lin Text {s = "nej ,"};

  lincat Question, Statement, Denial = Utt;
  lin Ask clause = mkUtt (mkQS presentTense (mkQCl clause));
  lin State clause = mkUtt (mkS presentTense clause);
  lin Deny clause = mkUtt (mkS presentTense negativePol clause);

  lincat Clause = Cl;

  -----------
  --Templates
  -----------

  oper bo_V : V = mkV "bor" ;
  oper komma_V : V = mkV "komma" "kom" "kommit" ;
  oper heta_V2 : V2 = mkV2 "heta" "hette" "hetat" ;
  oper varifraan_IAdv = lin IAdv (ss "varifrån") ;

  lin QReside person = mkUtt (mkQS (mkQCl where_IAdv (mkCl (person) bo_V)));
  lin CResideCountry person country = mkCl (person) (mkVP (mkVP bo_V) (SyntaxSwe.mkAdv in_Prep country));
  lin CResideCity person city = mkCl (person) (mkVP (mkVP bo_V) (SyntaxSwe.mkAdv in_Prep city));

  lin QOriginate person = mkUtt (mkQS (mkQCl varifraan_IAdv (mkCl person komma_V)));
  lin COriginateCountry person country = mkCl (person) (mkVP (mkVP komma_V) (SyntaxSwe.mkAdv from_Prep country));
  lin COriginateCity person city = mkCl (person) (mkVP (mkVP komma_V) (SyntaxSwe.mkAdv from_Prep city));

  lin CSettlement person settlement = mkCl (person) (mkVP (mkVP bo_V) (SyntaxSwe.mkAdv in_Prep (mkNP a_Det settlement)));
  lin CSettlementCountry person settlement country = mkCl (person) (mkVP (mkVP (mkVP bo_V) (SyntaxSwe.mkAdv in_Prep (mkNP a_Det settlement))) (SyntaxSwe.mkAdv in_Prep country));

  lin CHaveGBFriend person gbfriendCN = mkCl (person) (mkVP have_V2 (mkNP a_Det gbfriendCN));
  lin CMaritalStatus person status = mkCl (person) status;
  lin QMaritalStatus person = mkUtt (mkQS (mkCl (person) (mkAP or_Conj (mkAP (mkA "gift")) (mkAP (mkA "ogift")))));

  lin CHaveJob person = mkCl (person) (mkVP have_V2 (mkNP a_Det (mkN "arbete")));
  lin CJobStatus person status = mkCl (person) status;

  lin QName person = mkUtt (mkQCl what_IP (mkClSlash person (mkVPSlash heta_V2)));
  lin CName person name = mkCl (person) heta_V2 (mkNP name);

  ----------
  --Entities
  ----------

  lincat Person = NP;
  lin P1f, P1m = (mkNP i_Pron);
  lin P2tf, P2tm = (mkNP youSg_Pron);
  lin P2vf, P2vm = (mkNP youPol_Pron);
  lin P3f = mkNP (she_Pron);
  lin P3m = mkNP (he_Pron);
  lin PMother person = mkNP (GenNP person) (mkN "mamma");
  lin PFather person = mkNP (GenNP person) (mkN "pappa");

  lincat Country = NP;
  lin Ireland = mkNP (mkPN "Irland");
  lin CzechRepublic = mkNP (mkPN "Tjeckien");
  lin Latvia = mkNP (mkPN "Lettland");

  lincat City = NP;
  lin Dublin = mkNP (mkPN "Dublin");
  lin Brno = mkNP (mkPN "Brno");
  lin Riga = mkNP (mkPN "Riga");

  lincat Settlement = CN;
  lin BigCity = mkCN (mkA "stor") (mkN "stad");
  lin SmallTown = mkCN (mkA "liten") (mkN "stad");
  lin Village = mkCN (mkN "by");

  lincat GBFriend = CN;
  lin Girlfriend = mkCN (mkN "flickvän");
  lin Boyfriend = mkCN (mkN "pojkvän");

  lincat MaritalStatus = AP;
  lin Single = mkAP (mkA "ogift");
  lin Married = mkAP (mkA "gift");
  lin Divorced = mkAP (mkA "skild");
  -- lin Widowed = mkAP (mkA "änka/änkling");

  lincat JobStatus = VP;
  lin Student = mkVP (mkNP (mkN "studerande" ParadigmsSwe.utrum));
  lin Unemployed = mkVP (mkAP (mkA "arbetslös"));
  lin SelfEmployed = mkVP (mkNP (mkN "egenföretagare"));
  lin Retired = mkVP (mkNP (mkN "pensionär"));

  lincat Name = PN;
  lin AName = mkPN "...";

}
