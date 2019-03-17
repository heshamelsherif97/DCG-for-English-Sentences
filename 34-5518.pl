%References:-
% 1)Gurney, John & C. Claffy, Kimberly & H. Elbaum, Jason. (1989).
%   Implementing a Definite Clause Grammar for Parsing Surface Syntax.
% 2)Pareschi, Remo, and Dale Miller.
%   "Extending definite clause grammars with scoping constructs." Technical Reports (CIS) (1990): 519.
% 3)Abramson, Harvey. "Definite clause translation grammars." (1984).‏‏

% Grammar Structure could be understood from the comments
% the grammar is left factored and not left recursive

% A sentence is an english Clause
s(s(Sent)) --> english_clause(Sent).
% ========================================================

% A sentence is either a subject and a predicate or an adverb followed by subject and predicate

english_clause(eng_cls(ADP, Subj, VP)) -->
    adverbPhrase(ADP),
    subject(Subj, Number),
    predicate(VP, Number).

english_clause(eng_cls(Subj, VP)) -->
    subject(Subj, Number),
    predicate(VP, Number).

subord_adv_clause(sc(C, I)) -->
    subordconj(C),
    english_clause(I).
% ========================================================

% A subject is a noun phrase
subject(subj(NP), Num) -->
    nounPhrase(NP, Num).
% ========================================================

% Predicates

%Having and between 2 predicates
predicate(predConj(P1, conj(Con), P2), Number)-->
    predicate_22(P1, Number),
    [Con], {conjunction(Con)},
    predicate_22(P2, Number).
predicate(Pred2, Number) -->
    predicate_22(Pred2, Number).

%Having an adverb phrase after a predicate
predicate_22(predAdv(Pred2, Adv), Number) -->
    predicate_2(Pred2, Number),
    adverbs(Adv).
predicate_22(pred(Pred2), Number) -->
    predicate_2(Pred2, Number).

% A predicate could be a verb phrase and then a noun phrase
predicate_2(vn(VP, NP), Number) -->
    verbPhrase(VP, Number),
    nounPhrase(NP, _).
predicate_2(Pred3, Number) -->
    predicate_3(Pred3, Number).

% A predicate could be a verb phrase and then 2 noun phrases
predicate_3(vnn(VP, NP, NP2), Number) -->
    verbPhrase(VP, Number),
    nounPhrase(NP, _),
    nounPhrase(NP2, _).
predicate_3(P, Number) -->
    predicate_4(P,Number).

% A predicate could be a verb phrase only
predicate_4(VPhr, Number)-->
    verbPhrase(VPhr, Number).
% ========================================================

% Noun Phrases
nounPhrase(NP, Number)-->
    nounPhrase_1(NP, Number).

%Relative Pronoun Clause
nounPhrase(npRel(NP, P, relClause(PRED)), Number) -->
    nounPhrase_1(NP, Number),
    relativePronoun(P, Number),
    predicate(PRED, Number).

% 2 noun phrases having a conjunction
nounPhrase_1(npConj(NP, conj(Con), NP2), plural)-->
    nounPhrase_2(NP, _),
    [Con], {conjunction(Con)},
    nounPhrase_2(NP2, _).
nounPhrase_1(NP, Number) -->
    nounPhrase_2(NP, Number).

%A determiner before a noun
nounPhrase_2(npDet(Det, NP), Number) -->
    determiner(Det, Number),
    nounPhrase_22(NP, Number).
nounPhrase_2(NP, Number) -->
    nounPhrase_22(NP, Number).

%an adjective phrase before a noun
nounPhrase_22(npAdj(Adj,NPhr2), Number) -->
    adjectivePhrase(Adj),
    nounPhrase_3(NPhr2, Number).
nounPhrase_22(NPhr2, Number) -->
    nounPhrase_3(NPhr2, Number).

nounPhrase_3(N, Number) -->
    noun(N, Number).

% Prepositions phrases after the noun
nounPhrase_3(npPrep(N,Nc), Number) -->
    noun(N, Number),
    prepositions(Nc).
% ========================================================

% Verb Phrases

%A verb phrase having a conjunction with another verb phrase
verbPhrase(vpConj(VP, conj(Con), VP2), Number)-->
    verbPhrase_2(VP, Number),
    [Con], {conjunction(Con)},
    verbPhrase_2(VP2, Number).
verbPhrase(VP,Number) -->
    verbPhrase_2(VP,Number).

%An adverb before a verb phrase
verbPhrase_2(vpAdv(Adv,Vphr),Number)-->
    adverbPhrase(Adv),
    verbPhrase_3(Vphr,Number).
verbPhrase_2(V, Number)-->
    verbPhrase_3(V,Number).

%Past simple verb
verbPhrase_3(V, _) -->
    [V], {verb(_, _, V)}.

%Singular present simple verb
verbPhrase_3(V, singular) -->
    [V], {verb(_, V, _)}.

%Plural present simple verb
verbPhrase_3(V, plural) -->
    [V], {verb(V,_,_)}.
% ========================================================

% Adjectives Phrases

%Single adjective
adjectivePhrase(Adj) -->
    adjective(Adj).

%multiple adjectives
adjectivePhrase(adjs(Adj, AdjP)) -->
    adjective(Adj),
    adjectivePhrase(AdjP).

%an adjective phrase could be an adverb and then an adjective
adjectivePhrase(advadj(Adv, Adj)) -->
    adverb(Adv),
    adjective(Adj).
% ========================================================

% Prepositions Phrases
prepositions(PPP) -->
    prepositionalPhrase(PPP).

%multiple prepositional phrases
prepositions(preps(PPP,PREPS)) -->
    prepositionalPhrase(PPP), prepositions(PREPS).

%a prepostitional phrase is just a preposition and then a noun phrase
prepositionalPhrase(prepp(Prep, np(NP))) -->
    preposition(Prep), nounPhrase(NP, _).
% ========================================================

%Adverbs Phrases
adverbs(Advp) -->
    adverbPhrase(Advp).

adverbs(advs(Advp,Preps)) -->
    adverbPhrase(Advp),
    prepositions(Preps).

adverbPhrase(Adv) -->
    adverb(Adv).

adverbPhrase(advs(Adv, ADVP)) -->
    adverb(Adv),
    adverbPhrase(ADVP).

adverbPhrase(advprep(Prep)) -->
    prepositions(Prep).

adverbPhrase(advp(S)) -->
    subord_adv_clause(S).
%========================================================

%Terminal Rules
noun(noun(N), Number) -->
    [N], {noun(N, Number)}.
adjective(adj(Adj)) -->
    [Adj], {adjective(Adj)}.
adverb(adv(Adv)) -->
    [Adv], {adverb(Adv)}.
preposition(prep(Prp)) -->
    [Prp], {preposition(Prp)}.
determiner(det(Det), Number) -->
    [Det], {determiner(Det, Number)}.
relativePronoun(pron(P),Number) -->
    [P], {pronoun(P, Number)}.
subordconj(subconj(Conj)) -->
    [Conj], {subconj(Conj)}.

%Pronouns
pronoun(who,_).
pronoun(which,_).

%Sub Ordinate Conjunctions
subconj(while).

%Nouns
noun(boy, singular).
noun(man, singular).
noun(box, singular).
noun(room, singular).
noun(school, singular).
noun(woman, singular).
noun(envelope, singular).
noun(shed, singular).
noun(building, singular).
noun(tree, singular).
noun(girl, singular).
noun(boy, singular).
noun(student, singular).
noun(kid, singular).
noun(grape, singular).
noun(university, singular).
noun(professor, singular).
noun(lecturer, singular).
noun(scientist, singular).
noun(baby, singular).
noun(cell, singular).
noun(phone, singular).
noun(researcher, singular).
noun(apples, plural).
noun(drugs, plural).
noun(professors, plural).
noun(dogs, plural).
noun(cats, plural).
noun(scientists, plural).
noun(lecturers, plural).
noun(researchers, plural).
noun(students, plural).

%Adverbs
adverb(quickly).
adverb(secretly).
adverb(quietly).
adverb(slowly).
adverb(calmly).
adverb(joyfully).
adverb(quickly).
adverb(usually).
adverb(quickly).
adverb(perfectly).
adverb(quickly).
adverb(peacefully).
adverb(quickly).
adverb(never).
adverb(quickly).
adverb(often).

%Verbs
verb(read, reads, read).
verb(play, plays, played).
verb(write, writes, wrote).
verb(eat, eats, ate).
verb(swim, swims, swam).
verb(fly, flies, flew).
verb(see, sees, saw).
verb(give, gives, gave).
verb(sleep, sleeps, slept).
verb(watch, watches, watched).
verb(die, dies, died).
verb(make, makes, made).
verb(buy, buys, bought).
verb(draw, draws, drew).
verb(paint, paints, painted).
verb(work, works, worked).
verb(push, pushes, pushed).
verb(store, stores, stored).
verb(admire, admires, admired).
verb(climb, climbs, climbed).
verb(appreciate, appreciates, appreciated).
verb(sit, sits, sat).
verb(cry, crys, cried).
verb(like, likes, liked).

%Adjectives
adjective(angry).
adjective(black).
adjective(green).
adjective(red).
adjective(blue).
adjective(white).
adjective(large).
adjective(small).
adjective(young).
adjective(big).
adjective(old).
adjective(empty).
adjective(poor).
adjective(rich).
adjective(brilliant).
adjective(bright).
adjective(talented).
adjective(tall).
adjective(short).
adjective(attractive).
adjective(great).
adjective(beautiful).
adjective(dangerous).
adjective(gigantic).
adjective(huge).

%Conjunctions
conjunction(and).
conjunction(or).

%determiners
determiner(the, _).
determiner(a, singular).
determiner(an, singular).
determiner(that, singular).
determiner(this, singular).
determiner(these, plural).
determiner(those, plural).
determiner(all, plural).
determiner(some, _).
determiner(many, plural).
determiner(most, plural).
determiner(few, plural).
determiner(every, singular).
determiner(any, _).

%Prepositions
preposition(with).
preposition(between).
preposition(through).
preposition(to).
preposition(for).
preposition(by).
preposition(of).
preposition(on).
preposition(from).
preposition(during).
preposition(at).
preposition(near).
preposition(along).
preposition(beside).
preposition(before).
preposition(after).
preposition(for).
preposition(in).
preposition(behind).