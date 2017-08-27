abstract Chitchat = {
	flags startcat = Sentence;

	cat Sentence;
	fun SayQuestion : Question -> Sentence;				--is something the case?
	fun SayStatement : Statement -> Sentence;			--something is the case
	fun SayDenial : Denial -> Sentence;						--something is not the case
	fun SayYesStatement : Statement -> Sentence;	--yes, something is the case
	fun SayNoDenial : Denial -> Sentence;					--no, something is not the case
	fun SayNoStatement : Statement -> Sentence;		--no, something is the case

	cat Question;										--is something the case?
	cat Statement;									--something is the case
	cat Denial;											--something isn't the case
	fun Ask : Clause -> Question;
	fun State : Clause -> Statement;
	fun Deny : Clause -> Denial;

	cat Clause;											--something is/isn't/is? the case

	------

	cat Person;
	fun P1f, P1m, P2tf, P2tm, P2vf, P2vm, P3f, P3m : Person;

	cat Country;
	fun Ireland, CzechRepublic, Latvia : Country;

	cat City;
	fun Dublin, Brno, Riga : City;

	fun QReside : Person -> Question;														--where does [person] live?
	fun CResideCountry : Person -> Country -> Clause;						--[somebody] lives in [country]
	fun CResideCity : Person -> City -> Clause;									--[somebody] lives in [city]

	------

	cat Settlement;
	fun BigCity, SmallTown, Village : Settlement;

	fun CSettlement : Person -> Settlement -> Clause;						--somebody lives in [a large city/a small town/...]
	fun CSettlementCountry : Person -> Settlement -> Country -> Clause; --somebody lives in [a large city/a small town/...] in [country]

	------

	cat GBFriend;
	fun Girlfriend, Boyfriend : GBFriend;

	cat MaritalStatus;
	fun Single, Married, Divorced, Widowed : MaritalStatus;

	fun CHaveGBFriend : Person -> GBFriend -> Clause;						--[person] has [girlfriend/boyfriend]
	fun QMaritalStatus : Person -> Question;										--is [person] single or married?
	fun CMaritalStatus : Person -> MaritalStatus -> Clause;			--[person] is [single/married/...]

	------

	cat JobStatus;
	fun Student, Unemployed, Employed, SelfEmployed, Retired : JobStatus;

	fun CHaveJob : Person -> Clause;														--[person] has a job
	fun CJobStatus : Person -> JobStatus -> Clause;							--[person] is [employed/unemployed/...]

	------

	cat LifeStruggle;
	fun PhD, Marriage, Project : LifeStruggle;

	cat Goodness;
	fun Good, Bad : Goodness;

	fun QLifeStruggle : Person -> LifeStruggle -> Question;				  				--how is [person]'s [PhD/marriage/...] is going?
	fun CLifeStruggle : Person -> LifeStruggle -> Goodness -> Clause;				--[person]'s [PhD/marriage/...] is going [well/badly]

	------

	



}
