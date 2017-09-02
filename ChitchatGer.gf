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
    pos = mkCl person.np (mkVP (status.pos!person.ng));
    neg = mkS (mkCl person.np (mkVP (status.neg!person.ng)));
  };
  lin QMaritalStatus person = mkUtt (mkQS (mkCl person.np (mkAP or_Conj (mkAP (mkA "verheiratet")) (mkAP (mkA "ledig")))));

  lin CHaveJob person = {
    pos = mkCl person.np (mkVP have_V2 (mkNP a_Det (mkN "Job" masculine)));
    neg = mkS (mkCl person.np (mkVP have_V2 (mkNP no_Quant (mkN "Job" masculine))))
  };
  lin CJobStatus person status = {
    pos = mkCl person.np (mkVP (status.pos!person.ng));
    neg = mkS (mkCl person.np (mkVP (status.neg!person.ng)));
  };

  lin QName person = mkUtt (mkQCl how_IAdv (mkCl person.np (mkVP (mkV "heißen"))));
  lin CName person name = {
    pos = mkCl person.np (mkVP (mkV2 "heißen") (mkNP AName));
    neg = mkS presentTense positivePol (mkCl person.np (mkVP (mkV2 "heißen") (mkNP not_Predet (mkNP AName))))
  };

  ----------
  --Entities
  ----------

  param NG = Masc | Fem; --natural gender, for agreement with copular complements like "Student/Studentin", "Witwe/Witwer"
  oper P : Type = {isPron : PBool; pron : Pron; np : NP; ng : NG}; --people which can be either Pron+NP, or NP only (with dummy Pron)

  oper CCAdv : Type = {pos: NG => Adv; neg : NG => Adv}; --copular complement adverb
  oper mkCCadv = overload {
    mkCCadv : Str -> Str -> CCAdv = \pos,neg -> {pos = \\_ => ParadigmsGer.mkAdv pos; neg = \\_ => ParadigmsGer.mkAdv neg};
    mkCCadv : Str -> Str -> Str -> Str -> CCAdv = \posm,posf,negm,negf -> {
      pos = table {Masc => ParadigmsGer.mkAdv posm; Fem => ParadigmsGer.mkAdv posf};
      neg = table {Masc => ParadigmsGer.mkAdv negm; Fem => ParadigmsGer.mkAdv negf}
    };
  };

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

  lincat MaritalStatus = CCAdv;
  lin Single = mkCCadv "ledig" "nicht ledig";
  lin Married = mkCCadv "verheiratet" "nicht verheiratet";
  lin Divorced = mkCCadv "geschieden" "nicht geschieden";
  lin Widowed = mkCCadv "Witwer" "Witwe" "kein Witwer" "keine Witwe";

  lincat JobStatus = CCAdv;
  lin Student = mkCCadv "Student" "Studentin" "kein Student" "keine Studentin";
  lin Unemployed = mkCCadv "arbeitslos" "nicht arbeitslos";
  lin SelfEmployed = mkCCadv "selbtständig" "nicht selbtsständig";
  lin Retired = mkCCadv "im Ruhestand" "nicht im Ruhestand";

  lincat Name = PN;
  lin AName = mkPN "...";
}
