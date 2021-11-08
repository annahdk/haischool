## Übersicht über die möglichen Methoden

### Zweistichproben t-Test
Voraussetzungen:

* normalverteilte Stichproben: $X_1 \sim \mathcal{N}(\mu_1, \sigma_1^2), X_2 \sim \mathcal{N}(\mu_2, \sigma_2^2)$

* Unabhängigkeit zwischen den Stichproben

* Teststatistik: $T(x)=\sqrt{\frac{N_1N_2}{N_1 + N_2}} \frac{\bar{x_1} - \bar{x_2}}{\hat{\sigma}_{12}(x)}$

* Gleichheit der Varianzen (sonst Welch-Zwei-Stichprobentest)

Dann kann Test zu $H_0: \mu_1 = \mu_2 \quad \text{vs.} \quad H_1: \mu_1 \neq \mu_2$ (bzw. zu $\leq$ und $\geq$) stattfinden

In \texttt{R}: \texttt{t.test()} (bzw. \texttt{t.test(..., var.equal = FALSE)})


### Shapiro-Wilk-Test
Test zur Überprüfung der Normalverteilungsannahme einer Stichprobe

* Eignet sich besonders bei kleinen Stichproben ($n<50$)

* Test: $H_0:$ Es liegt Normalverteilung vor \quad vs. \quad $H_1:$ Es liegt keine Normalverteilung vor

In \texttt{R}: \texttt{shapiro.test()}


### Bartlett-Test
Test auf Gleichheit der Varianzen von $k$ Stichproben
Voraussetzung: Stichproben haben Normalverteilung

In \texttt{R}: \texttt{bartlett.test()}


### $\chi^2$-Test (Brüning, 1994, S. 220)
Tests auf Unabhängigkeit in $n\times n$ Kontingenztabellen. An n Untersuchungsobjekten werden zwei Merkmale A und B, aufgeteilt in k bzw. l disjunkte Klassen. Jedes Meßniveau ist zugelassen.

Testproblem: $H_0:$ Merkmale $A$ und $B$ sind unabhängig gegen $H_1:$ $A$ und $B$ sind nicht unabhängig

Teststatistik: $X^2=\sum_{i=1}^k \sum_{j=1}^l \frac{(n_{ij}-E(n_{ij}))^2}{E(n_{ij})}$, mit $E(n_{ij}) = \frac{n_{i\cdot}n_{\cdot j}}{n}$ die erwartete absolute Häufigkeit in Feld $n_{ij}$ bei Gültigkeit von $H_0$

$H_0$ wird abgelehnt, wenn $X^2 > \chi_{(1-\alpha, (k-1), (l-1))}$ ist.

Testet nur für zweiseitige Alternativen


### Wilcoxon Rangsummentest

* Test für (mindestens) Ordinaldaten
* Teste, ob ein aus einer Population zufällig gezogener Wert größer ist als ein zufällig gezogener Wert einer zweiten Population
* Wenn keine stetige Verteilung vorliegt müssen Bindungen beachtet werden!
* Wird immer dann verwendet, wenn Normalverteilungsannahme nicht plausibel erscheint


### Levene-Test, Brown-Forsythe-Test

Tests zum Streuungsvergleich für zwei unabhängige Stichproben


