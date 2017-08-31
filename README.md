# Chitchat.gf

This is work in progress. (What isn't?)

This is a multilingual application grammar for casual coffee-break conversations, written in [Grammatical Framework](http://www.grammaticalframework.org). The grammar generates and parses sentences people can use to talk about themselves and their background, ask and answer questions such as where you live, what you do for a living, whether or not you have family and children, what your likes an dislikes are, and so on. The topics correspond broadly to the kind of stuff students of a foreign language go through at [CEFR](https://en.wikipedia.org/wiki/Common_European_Framework_of_Reference_for_Languages) level A1.

Possible applications:

- Simulated dialogue exercises for language learners.
- A conversational wizard that helps you set up a social networking profile or generate a [FOAF](https://en.wikipedia.org/wiki/FOAF_(ontology)) file.
- Natural-language verbalization of social networking data.
- A chatbot that gets people to divulge personal information about themselves (borderline unethical) or about other people (definitely unethical).
- An app that people with no shared language could use to have a chat.
- An app for shy people that randomly generates conversation starters. Yes, why be awkward alone when you can be awkward together?

This grammar originated at the [2017 Grammatical Framework Summer School](http://school.grammaticalframework.org/2017/) in beautiful Riga.

## Examples (Abstract + English + German)

    SayYesStatement (State (COriginateCountry P1m Latvia))
    yes , I come from Latvia .
    ja , ich komme aus Lettland .

    SayDenial (Deny (CJobStatus P2vm Retired))
    you aren't retired .
    Sie sind nicht im Ruhestand .

    SayStatement (State (CSettlement (PFather P3m) BigCity))
    his father lives in a big city .
    sein Vater wohnt in einer großen Stadt .

    SayDenial (Deny (CHaveGBFriend P1f Girlfriend))
    I don't have a girlfriend .
    ich habe keine Geliebte .

    SayNoStatement (State (CResideCity P1f Riga))
    no , I live in Riga .
    nein , ich wohne in Riga .

    SayQuestion (QMaritalStatus P3m)
    is he married or single ?
    ist er verheiratet oder ledig ?

    SayQuestion (QReside P2tf)
    where do you live ?
    wo wohnst du ?

    SayYesStatement (State (CMaritalStatus P2tf Married))
    yes , you are married .
    ja , du bist verheiratet .

    SayQuestion (QName P3f)
    what is her name ?
    wie heißt sie ?

    SayNoStatement (State (CHaveJob (PMother P1f)))
    no , my mother has a job .
    nein , meine Mutter hat einen Job .

## Understanding the abstract syntax

The top-level category is `Sentence`. It is basically just a string and includes sentence-final punctuation (a full stop or a question mark). It is linearized as RGL's `Text` in the concrete grammars.

Sentences are produced by various functions such as `SayQuestion`, `SayStatement` and `SayDenial` from instances of the categories `Question`, `Statement` and `Denial`. These are linearized as `Utt` in the concrete grammars. Examples:

- Question: `do you live in Riga`
- Statement: `you live in Riga`
- Denial: `you don't live in Riga`

The reason for having separate categories for questions, statements and denials is to know which punctuation to add to them when converting them into sentences by `SayQuestion`, `SayStatement` and `SayDenial`, and also to know whether it makes sense to add `yes ,` or `no ,` to their beginning in the functions `SayYesStatement`, `SayNoDenial` and `SayNoStatement`.

Questions, statements and denials are produced by the functions `Ask`, `State` and `Deny` from instances of the category `Clause`. A clause is something which encapsulates an idea, such as the idea of somebody living or not living in some city, before we decide whether we want to express the idea positively (as a statement), negatively (as a denial) or interrogatively (as a question). Clauses are linearized as RGL's `Cl` in in the concrete grammars.

Finally, clauses are produced by "template" functions such as `CResideCity` (= somebody living or not living in some city), `CMaritalStatus` (= somebody being or not being married/single/divorced) and so on. There is a small but growing number of these in the grammar and eventually there will be many, many, many! Their names always begin with the capital letter `C`.

Some template functions produce not clauses but questions. These are all WH-questions (= questions that begin with words such as *what*, *who*, *how*) such as `QReside` (= the question, where does somebody live?) and `QName` (= the question, what is somebody's name?). Functions that produce questions always have names that begin with the capital letter `Q`. The idea is that yes/no questions are produced from clauses produced by template functions, while WH-questions are produced by template functions directly.

## Over to you now

I want you to write a concrete grammar module for your language, whatever language it is. I'm pretty sure it can be done even if your languages doesn't have a resource grammar yet. Fork me and send me a pull request.

Then I want you to watch this repository and, as I add new template functions, to keep your concrete grammar updated.
