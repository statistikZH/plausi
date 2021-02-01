
# plausi

![](https://opendata.swiss/content/uploads/2016/02/kt_zh.png)

## R-Package für die Plausibilisierung von Abstimmungsresultaten

Das plausi-package dient der R-gestützten Abstimmungsforensik. Es enthält Funktionen die es erlauben statistische Auffälligkeiten und Anomalien in Abstimmungsresultaten zu identifizieren. 

- robuste Ausreissererkennung für kleine Fallzahlen wie auch schiefe Verteilungen
- Berechnung von Differenzen zwischen allen möglichen Kombinationen von Vorlagen (z.B. zwecks systematischem Vergleich der Stimmbeteiligung verschiedener Vorlagen über alle Auszählkreise hinweg)
- Vorhersage von zu erwartenden Resultaten via unterschiedlichen Machine-Learning Algorithmen (Ja-Anteile, Stimmbeteiligung etc.)

Installation via gitlab:

__remotes::install_url("https://gitlab.com/tlorusso/plausi.git")__
