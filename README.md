Trajektorien von bestätigten COVID-19-Fällen in den deutschen
Bundesländern
================

## Beschreibung

Diese [Shiny App](https://shiny.rstudio.com/) ist ein interaktiver Graph
und stellt die Trajektorien bestätigter COVID-19-Fälle in den deutschen
Bundesländern dar. Die Anzahl der neuen bestätigten Fälle in der
vergangenen Woche wird gegen die Gesamtzahl bestätigter Fälle
aufgetragen. Beide Achsen sind dabei logarithmisch skaliert, um auch die
Entwicklung am Beginn der Ausbreitung auflösen zu können.

Bei dieser Art der Auftragung offenbart sich exponentielles Wachstum als
Gerade mit positiver Steigung. Alle Bundesländer folgen dabei ab einer
Gesamtzahl bestätigter Fälle von etwa 100 einer sehr ähnlichen Gerade.
Die Wachstumsrate der exponentiellen Ausbreitung ist dann in allen
Bundesländern vergleichbar groß.

Das Greifen von Maßnahmen zum Social Distancing sollte sich zunächst
durch das Abflachen der Steigung und schließlich durch ein Abknicken der
Trajektorie nach unten zeigen. Allerdings sollte hierbei der Meldeverzug
im Hinterkopf behalten werden, der sich besonders in den letzten ein bis
zwei Tagen der Trajektorien niederschlägt.

**Tipps:** - Bundesländer lassen sich im Auswahlfeld nach Anklicken mit
`<Del>` entfernen und nach Klicken in den weißen Bereich des Feldes auch
wieder hinzufügen. - Mit dem Play Button am rechten Ende des
Schiebereglers für das Meldedatum kann die gesamte Entwicklung animiert
werden. - Wenn der Schieberegler ausgewählt ist, kann mit den
Pfeiltasten um einzelne Tage vor- und zurückgesprungen werden.

## Credits

Daten: [Bundesamt für Kartographie und Geodäsie /
Robert-Koch-Institut](https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/data)
· Idee für diese Form der Darstellung: [Aatish
Bhatia](https://aatishb.com/covidtrends) und [Minute
Physics](https://youtu.be/54XLXg4fYsc) · Source:
[github.com/mattflor](https://github.com/mattflor/covid-19-trajektorien-app)
