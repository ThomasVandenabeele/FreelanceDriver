# Freelance Drivers App
Erlang taak voor TAGP.

Run simulatie met:

```
simulator:start().
```

Omdat onze groepstaak rond het multi-core programmeren in erlang al zeer uitgebreid was, heb ik ervoor gekozen om deze taak afzonderlijk te maken.
Niet enkel deze taak maar dus ook de taak voor het gedeelte van mr. Rutten en mr. Valkeneers tonen aan dat ik in staat ben om functionele programmeertechnieken kan toepassen in Erlang.
Stijn heeft ervoor gekozen om verder te werken in onze groepstaak, dus ook om geen kopie van stijn zijn werk te verkrijgen, ga ik deze taak anders aanpakken.

Ik heb er dus voor gekozen om deze taak anders te realiseren dan de taak met de multi-core/processen aanpak zodat de focus meer ligt op het recursief denken en de nodige lijst-bewerkingen (hoewel dit ook zeker van toepassing is in onze andere taak). 

In mijn programma is het mogelijk dat personen evenementen kunnen aanmaken in een 'agenda'. Ze kunnen ook kiezen om een bepaalde opdracht aan deze event te linken voor een freelancer (denk hierbij bv aan het brengen van het gezien naar luchthaven, vervoeren van kinderen als ouders niet beschikbaar zijn, enz.).
Een freelancer kan via het programma via bepaalde nuttige functies informatie opvragen over hun opdrachten. Zo kunnen ze zien welke opdrachten ze moeten volbrengen op een bepaalde dag/week, hun verdiensten voor een bepaalde dag/week opvragen, ...
Het is ook mogelijk dat een freelancer zijn opdracht niet aanvaardt, deze kan hij dan ook afwijzen, de event wordt automatisch gekoppeld aan een andere beschikbare freelancer die in de freelancers-tabel van het gezin staat.

In deze taak gebruik ik tevens de volgende lijst-bewerkingen: map, filter, foldl, zipwith, ... Zo toon ik dus aan dat ik elegante en efficiënte code kan schrijven in Erlang volgens de regels van de kunst van het functioneel programmeren.

Thomas Vandenabeele