# 1. Pakete und Daten
library(rio)
library(car)
data <- import("/Users/vivianaleksic/Desktop/bericht.csv")

# 2. Untersuchungsgruppen auswählen und beschriften
data_study <- data[data$Gruppe %in% c(1, 2, 3), ]
data_study$Gruppe <- factor(
  data_study$Gruppe,
  levels = c(3, 2, 1),
  labels = c(
    "Kontrollgruppe",
    "Standardintervention",
    "SDT-Intervention"
  )
)

# 3. Stichprobengröße und Gruppengrößen
nrow(data_study)
table(data_study$Gruppe)

# 4. Alter und Geschlecht
mean(data_study$Alter, na.rm = TRUE)
sd(data_study$Alter, na.rm = TRUE)
min(data_study$Alter, na.rm = TRUE)
max(data_study$Alter, na.rm = TRUE)
table(data_study$Geschlecht)
prop.table(
  table(data_study$Geschlecht)
) * 100

# 5. Veränderung zwischen Prä- und Post-Messung
data_study$Veraenderung <-
  data_study$Mean2 - data_study$Mean1

# 6. Deskriptive Statistik
mittelwerte <- aggregate(
  cbind(Mean1, Mean2, Veraenderung) ~ Gruppe,
  data = data_study,
  FUN = mean
)
standardabweichungen <- aggregate(
  cbind(Mean1, Mean2) ~ Gruppe,
  data = data_study,
  FUN = sd
)
mittelwerte
standardabweichungen

# 7. Long-Format für die Mixed ANOVA
data_study$ID <- seq_len(nrow(data_study))
data_long <- rbind(
  data.frame(
    ID = data_study$ID,
    Gruppe = data_study$Gruppe,
    Zeitpunkt = "Prä",
    Motivation = data_study$Mean1
  ),
  data.frame(
    ID = data_study$ID,
    Gruppe = data_study$Gruppe,
    Zeitpunkt = "Post",
    Motivation = data_study$Mean2
  )
)
data_long$ID <- factor(data_long$ID)
data_long$Gruppe <- factor(
  data_long$Gruppe,
  levels = c(
    "Kontrollgruppe",
    "Standardintervention",
    "SDT-Intervention"
  )
)
data_long$Zeitpunkt <- factor(
  data_long$Zeitpunkt,
  levels = c("Prä", "Post")
)

# 8. Normalverteilung
normalitaet <- by(
  data_long$Motivation,
  list(data_long$Gruppe, data_long$Zeitpunkt),
  shapiro.test
)
normalitaet

# 9. Ausreißer
boxplot(
  Motivation ~ interaction(Gruppe, Zeitpunkt),
  data = data_long,
  xlab = "Gruppe und Messzeitpunkt",
  ylab = "Selbstbestimmte Motivation"
)

# 10. Varianzhomogenität
leveneTest(
  Motivation ~ interaction(Gruppe, Zeitpunkt),
  data = data_long
)

# 11. Mixed ANOVA
mixed_anova <- aov(
  Motivation ~ Gruppe * Zeitpunkt +
    Error(ID / Zeitpunkt),
  data = data_long
)
summary(mixed_anova)

# 12. ANOVA der Veränderungswerte
change_anova <- aov(
  Veraenderung ~ Gruppe,
  data = data_study
)
summary(change_anova)

# 13. Tukey-Post-hoc-Vergleiche
TukeyHSD(change_anova)

# 14. Abbildungen
# Abbildung 1: Mittlere Veränderung der selbstbestimmten Motivation
veraenderung_mittel <- aggregate(
  Veraenderung ~ Gruppe,
  data = data_study,
  FUN = mean
)
balken <- barplot(
  veraenderung_mittel$Veraenderung,
  names.arg = veraenderung_mittel$Gruppe,
  main = "Veränderung der selbstbestimmten Motivation",
  xlab = "Untersuchungsgruppe",
  ylab = "Veränderung der selbstbestimmten Motivation",
  ylim = c(0, 15),
  col = c("grey", "steelblue", "darkseagreen3")
)
text(
  x = balken,
  y = veraenderung_mittel$Veraenderung,
  labels = round(veraenderung_mittel$Veraenderung, 2),
  pos = 3
)
abline(h = 0, lty = 2)

# Abbildung 2: Verteilung der individuellen Veränderungswerte
boxplot(
  Veraenderung ~ Gruppe,
  data = data_study,
  main = "Verteilung der Veränderungswerte",
  xlab = "Untersuchungsgruppe",
  ylab = "Veränderung der selbstbestimmten Motivation",
  col = c("grey", "steelblue", "darkseagreen3")
)
abline(h = 0, lty = 2)

# 15. Tabellen
# Tabelle 1: Deskriptive Kennwerte
Tabelle1 <- aggregate(
  cbind(Mean1, Mean2, Veraenderung) ~ Gruppe,
  data = data_study,
  FUN = mean
)
SDs <- aggregate(
  cbind(Mean1, Mean2) ~ Gruppe,
  data = data_study,
  FUN = sd
)
Tabelle1
SDs

# Tabelle 2: Ergebnisse der Mixed ANOVA
anova_ergebnis <- data.frame(
  Effekt = c(
    "Untersuchungsbedingung",
    "Messzeitpunkt",
    "Untersuchungsbedingung × Messzeitpunkt"
  ),
  F = c(
    0.046,
    100.00,
    30.56
  ),
  df = c(
    "2, 297",
    "1, 297",
    "2, 297"
  ),
  p = c(
    ".955",
    "< .001",
    "< .001"
  )
)
anova_ergebnis