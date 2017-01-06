%% In deze file worden de gebruikte records omschreven.
-record(job, {id, beschrijving, vergoeding, opdrachtgever, freelancer_id}).
-record(freelancer, {id, naam, voornaam, leeftijd, contactgegevens}).
-record(event, {id, start, duur, opdracht = null}).