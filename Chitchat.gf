abstract Chitchat = {
  flags startcat = Sentence;

  cat Sentence; cat Restriction;
  fun SayQuestion : Question -> Sentence; --is something the case?
  fun SayStatement : Statement -> Sentence; --something is the case
  fun SayDenial : Denial -> Sentence; --something is not the case
  fun SayYesStatement : Statement -> Sentence; --yes, something is the case
  fun SayNoDenial : Denial -> Sentence; --no, something is not the case
  fun SayNoDenialBut : Denial -> Restriction -> Sentence; -- no, something is not the case, but
  fun SayNoStatement : Statement -> Sentence; --no, something is the case

  cat Question; --is something the case?
  cat Statement; --something is the case
  cat Denial; --something isn't the case
  fun Ask : Clause -> Question;
  fun State : Clause -> Statement;
  fun Deny : Clause -> Denial;

  cat Clause; --something is/isn't/is? the case

  -----------
  --Templates
  -----------

  fun QReside : Person -> Question; --where does [person] live?
  fun CResideCountry : Person -> Country -> Clause; --[somebody] lives in [country]
  fun CResideCity : Person -> City -> Clause; --[somebody] lives in [city]

  fun QOriginate : Person -> Question; --where does [person] come from?
  fun COriginateCountry : Person -> Country -> Clause; --[somebody] comes from [country]
  fun COriginateCity : Person -> City -> Clause; --[somebody] comes from [city]

  fun CSettlement : Person -> Settlement -> Clause; --somebody lives in [a large city/a small town/...]
  fun CSettlementCountry : Person -> Settlement -> Country -> Clause; --somebody lives in [a large city/a small town/...] in [country]

  fun CHaveGBFriend : Person -> GBFriend -> Clause; --[person] has [girlfriend/boyfriend]
  fun QMaritalStatus : Person -> Question; --is [person] single or married?
  fun CMaritalStatus : Person -> MaritalStatus -> Clause; --[person] is [single/married/...]

  fun CHaveJob : Person -> Clause; --[person] has a job
  fun CJobStatus : Person -> JobStatus -> Clause; --[person] is [employed/unemployed/...]

  fun QName : Person -> Question; --what is [person]'s name?
  fun CName : Person -> Name -> Clause; --[person]'s name is [...]

  fun DontGetFunnyIdeas : Restriction;
  fun NotApproving : Person -> Restriction;
  fun NeverthelessHappy : Restriction;

  ----------
  --Entities
  ----------

  cat Person;
  fun P1f, P1m : Person; --me feminine, me masculine
  fun P2tf, P2tm : Person; --you informally feminine, you informally masculine
  fun P2vf, P2vm : Person; --you politely feminine, you politely masculine
  fun P3f, P3m : Person; --he, she
  fun PMother, PFather : Person -> Person; --[someone]'s mother, [someone]'s father

  cat Country;
  fun Ireland, CzechRepublic, Latvia : Country;

  cat City;
  fun Dublin, Brno, Riga : City;

  cat Settlement;
  fun BigCity, SmallTown, Village : Settlement;

  cat GBFriend;
  fun Girlfriend, Boyfriend : GBFriend;

  cat MaritalStatus;
  fun Single, Married, Divorced, Widowed : MaritalStatus;

  cat JobStatus;
  fun Student, Unemployed, SelfEmployed, Retired : JobStatus;

  cat Name;
  fun AName : Name; --placeholder for a name, linearize as "..."

}
