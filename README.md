# titanic-shiny

Diese App befasst sich mit dem Untergang des Kreuzfahrtschiffes Titanic, das 1912 auf seiner Jungfernfahrt gesunken ist. Auf der Basis eines Datensatzes können Sie herausfinden, welche Werte welcher Merkmale der Passagiere einen Einfluss auf die Überlebenschancen hatten. Hat zum Beispiel das Alter etwas mit der Überlebenschance zu tun und wenn ja, welche Altersstufen hatten höhere und welche niedrigere Überlebenschancen? Spielen Kombinationen von Merkmalen wie Alter, Kabinenklasse und Geschlecht eine Rolle? Wenn ja, welche? Insbesondere können Sie Gruppen von Passagieren finden, die besonders hohe oder niedrige Überlebenschancen hatten.

Hierzu wurde diese Sie eine interaktive Visualisierung mit Shiny in R gebaut, mit deren Hilfe man die Überlebenschancen von Passagiergruppen untersuchen kann.

Der Datensatz titanic_data.csv enthält Informationen über knapp 900 Passagiere der insgesamt geschätzt 2224 Personen, die zum Zeitpunkt des Untergangs an Bord waren.

## Die Merkmale sind 

- PassengerId: Passenger ID
- Survived: 0=no, 1=yes
- Pclass: Passenger class
- Name
- Sex
- Age
- SibSp: Number of siblings/spouses Aboard
- Parch: Number of parents/children Aboard
- Ticket: Ticket number
- Fare: Price of ticket
- Cabin
- Embarked: Port of embarkation

Es wird mehrere Visualisierungen der Daten geben, welche z.B. durch Drop-Down-Menüs, Schieberegler, Knöpfe oder direkte Drill-Downs verändert werden können, um die Daten zu explorieren oder zu analysieren. Es werden dabei auch mehrere Merkmale in interaktiven Diagrammen verknüpft, so dass man Gruppen mit besonders hohen oder niedrigen Überlebenschancen finden kann.

## Deployment
Dieses Repository deployed automatisch zum [Shiny.io Projekt](https://theothmn.shinyapps.io/titanic/).