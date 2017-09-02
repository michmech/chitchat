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
  oper woher_IAdv = lin IAdv (ss "woher") ;
  oper aus_Prep = mkPrep "aus" dative;

  oper mkCCC : P -> CC -> Cl : \person,complement = {
    pos = case complement.shape of {
      AsAP => mkCl person.np complement.ap; --ich bin verheiratet
      AsCN => mkCl person.np (mkNP (complement.cn!person.ng)); --ich bin Witwe/Witwer
      AsAdv => mkCl person.np complement.adv --ich bin im Ruhestand
    };
    neg = case complement.shape of{
      AsAP => mkS presentTense negativePol (mkCl person.np complement.ap); --ich bin nicht verheiratet
      AsCN => mkS presentTense (mkCl person.np (mkNP no_Quant (complement.cn!person.ng))); --ich bin keine Witwe/kein Witwer
      AsAdv => mkS presentTense negativePol (mkCl person.np complement.adv) --ich bin nicht im Ruhestand
    }
  };

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
  lin CMaritalStatus person status = mkCCC person status;;
  lin QMaritalStatus person = mkUtt (mkQS (mkCl person.np (mkAP or_Conj (mkAP (mkA "verheiratet")) (mkAP (mkA "ledig")))));

  lin CHaveJob person = {
    pos = mkCl person.np (mkVP have_V2 (mkNP a_Det (mkN "Job" masculine)));
    neg = mkS (mkCl person.np (mkVP have_V2 (mkNP no_Quant (mkN "Job" masculine))))
  };
  lin CJobStatus person status = mkCCC person status;

  lin QName person = mkUtt (mkQCl how_IAdv (mkCl person.np (mkVP (mkV "heißen"))));
  lin CName person name = {
    pos = mkCl person.np (mkVP (mkV2 "heißen") (mkNP AName));
    neg = mkS presentTense positivePol (mkCl person.np (mkVP (mkV2 "heißen") (mkNP not_Predet (mkNP AName))))
  };

  ----------
  --Entities
  ----------

  param NG = Masc | Fem; --natural gender, for agreement with complements like "Studen/Studentin", "Witwe/Witwer"
  oper P : Type = {isPron : PBool; pron : Pron; np : NP; ng : NG}; --people which can be either Pron+NP, or NP only (with dummy Pron)

  param Shape = AsAP | AsCN | AsAdv;
  oper CC : Type = {shape : Shape; ap: AP; cn: NG => CN; adv : Adv}; --copular complements which can be either an AP (eg. "verheiratet") or a gender-dependent CN (eg. "Student/Studentin") or an Adv ("im Ruhestand"):
  oper mkCCfromAP : AP -> CC = \ap -> {shape = AsAP; ap = ap; cn = \\_=>mkCN (mkN ""); adv = ParadigmsGer.mkAdv ""};
  oper mkCCfromCN : CN -> CC = \cn -> {shape = AsCN; ap = mkAP (mkA ""); cn = \\_=>cn; adv = ParadigmsGer.mkAdv ""};
  oper mkCCfromCN2 : CN -> CN -> CC = \cnmasc,cnfem -> {shape = AsCN; ap = mkAP (mkA ""); cn = table{Masc => cnmasc; Fem => cnfem}; adv = ParadigmsGer.mkAdv ""};
  oper mkCCfromAdv : Adv -> CC = \adv -> {shape = AsAdv; ap = mkAP (mkA ""); cn = \\_=>mkCN (mkN ""); adv = adv};

  lincat Person = P;
  lin P1m = {isPron = PTrue; pron = i_Pron; np = mkNP i_Pron; ng = Masc};
  lin P1f = {isPron = PTrue; pron = i_Pron; np = mkNP i_Pron; ng = Fem};
  lin P2tm = {isPron = PTrue; pron = youSg_Pron; np = mkNP youSg_Pron; ng = Masc};
  lin P2tf = {isPron = PTrue; pron = youSg_Pron; np = mkNP youSg_Pron; ng = Fem};
  lin P2vm = {isPron = PTrue; pron = youPol_Pron; np = mkNP youPol_Pron; ng = Masc};
  lin P2vf = {isPron = PTrue; pron = youPol_Pron; np = mkNP youPol_Pron; ng = Fem};
  lin P3m = {isPron = PTrue; pron = he_Pron; np = mkNP he_Pron; ng = Masc};
  lin P3f = {isPron = PTrue; pron = she_Pron; np = mkNP she_Pron; ng = Fem};

  oper possNP : P -> CN -> NP;
  oper possNP person cn = case person.isPron of {
    PTrue => mkNP person.pron cn; --meine Mutter
    PFalse => mkNP the_Det (mkCN cn (SyntaxGer.mkAdv von_Prep person.np)) --die Mutter von meiner Mutter
  };
  lin PMother person = {isPron = PFalse; pron = person.pron; np = possNP person (mkCN (mkN "Mutter" feminine)); ng = Fem};
  lin PFather person = {isPron = PFalse; pron = person.pron; np = possNP person (mkCN (mkN "Vater" masculine)); ng = Masc};

  lincat Country = NP;
  lin Ireland = mkNP (mkPN "Irland");
  lin CzechRepublic = mkNP (mkPN "Tschechien");
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
  lin Girlfriend = mkCN (mkN "Freundin" feminine);
  lin Boyfriend = mkCN (mkN "Freund" masculine);

  lincat MaritalStatus = CC;
  lin Single = mkCCfromAP (mkAP (mkA "ledig"));
  lin Married = mkCCfromAP (mkAP (mkA "verheiratet"));
  lin Divorced = mkCCfromAP (mkAP (mkA "geschieden"));
  lin Widowed = mkCCfromCN2 (mkCN (mkN "Witwer" masculine)) (mkCN (mkN "Witwe" feminine));

  lincat JobStatus = CC;
  lin Student = mkCCfromCN2 (mkCN (mkN "Student" masculine)) (mkCN (mkN "Studentin" feminine));
  lin Unemployed = mkCCfromAP (mkAP (mkA "arbeitslos"));
  lin SelfEmployed = mkCCfromAP (mkAP (mkA "selbtständig"));
  lin Retired = mkCCfromAdv (ParadigmsGer.mkAdv "im Ruhestand");

  lincat Name = PN;
  lin AName = mkPN "...";

}
