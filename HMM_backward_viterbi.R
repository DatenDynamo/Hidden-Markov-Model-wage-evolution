options(encoding = "UTF-8")

################################################################################
# HINWEIS------------------------------------------------- ----------------
################################################################################
# Zur Visualisierung des Modelltrainings, werden bei jeder Iteration die mu und sd
# als dataframe gespeichert was je nach Rechenkapazität recht lange dauern kann.
# Aus diesem Grund sind die Zeilen 235 und 236 auskommentiert.

# AUßerdem ist das Rendern jener Visualisierung zeitaufwändig, weswegen der 
# zugehörige Code (Zeile 597-637) und die benötigten Packages auskommentiert sind.





################################################################################
# Laden der benötigten Bibliotheken für die Visualisierung ----------------
################################################################################

library("ggplot2")
library("grid")
library("latex2exp")
library("gridExtra")
#library("animation")
#library("magick")

################################################################################
# Vorbereitung der Daten --------------------------------------------------
################################################################################

# Für Reproduzierbarkeit
set.seed(1)

#Farbpalette
# Malachite  #04E762 
# Selective yellow  #F5B700
# Celestial Blue  #00A1E4
# Mexican Pink  #DC0073
# Chartreuse  #89FC00

Tarifrunden <- c(1991,1992,1994,1995,1997,1999,2000,2002,2004,2006,2007,2009,2010,2012,2013,2015,2016,2018,2020,2021,2022)
entgelt <-  read.csv("output_Tabelle-Tarifentgelte.csv", header = FALSE, sep = ",", encoding="UTF-8")[-c(1:3),]
colnames(entgelt)<- c("Jahr", "tarifliches_jahresentgelt_ME")
entgelt <- entgelt[,-c(3:4)]
entgelt$tarifliches_jahresentgelt_ME<- sub("\\ .*", "", entgelt$tarifliches_jahresentgelt_ME)
entgelt$tarifliches_jahresentgelt_ME <-as.numeric(sub('\\.', '', entgelt$tarifliches_jahresentgelt_ME))
entgelt$Jahr <- as.numeric(entgelt$Jahr)

entgelt<- entgelt[ entgelt$Jahr %in% Tarifrunden, ]

# Berechnet Differenz der Jahresentgelte ME-INdustrie zur vorherigen Zeile
entgelt$Differenz <-  c(NA,  entgelt$tarifliches_jahresentgelt_ME[2:nrow(entgelt)] - entgelt$tarifliches_jahresentgelt_ME[1:(nrow(entgelt) - 1)])

# Berechne die relative Aenderung der Jahresentgelte ME-Industrie
entgelt$relAenderung <-  c(NA, entgelt$Differenz[2:nrow(entgelt)] / entgelt$tarifliches_jahresentgelt_ME[1:nrow(entgelt) - 1])

# Berechne die log-returns der Jahresentgelte der ME-Industrie gegenüber dem Vorjahr
#entgelt$log_returns_ME  <-  c(NA, diff(log(entgelt$tarifliches_jahresentgelt_ME)))
#clipr::write_clip(entgelt) #Kopiere in Zwischenablage



################################################################################
# Visualisierung Zeitreihe Entgeltentwicklung + Zeitreihe relative --------
################################################################################

data <-  entgelt[2:nrow(entgelt), c("Jahr", "relAenderung", "tarifliches_jahresentgelt_ME")]
data$Jahr <- as.Date(ISOdate(data$Jahr, 01, 01))

plot_entgelt <-
  ggplot(data, aes(x = Jahr, y = tarifliches_jahresentgelt_ME)) +
  geom_point() +
  geom_line() +
  scale_x_date() +
  scale_y_continuous(labels = scales::label_dollar(prefix = "€")) +
  labs(title = "M+E-Industrie Ø Jahresentgelt") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(
      size = 11,
      face = "bold",
      hjust = 0.5,
      margin = margin(t = 5, b = 5)
    ),
    plot.margin = unit(c(0, 0, 0, 1), "cm")
  )

plot_delta_me <- ggplot(data, aes(x = Jahr), show.legend= FALSE) +
  geom_point(aes(y = relAenderung)) +
  geom_line(aes(y = relAenderung, colour = "Beobachtung"), show.legend=FALSE) +
  scale_x_date() +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(title = "relative Aenderung Jahresentgelt", x = "Jahr") +
  scale_colour_manual(
    "",
    breaks = c("Beobachtung", "Vorhersage"),
    values = c("#04E762", "#DC0073")
  ) +
  theme(
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(
      size = 11,
      face = "bold",
      hjust = 0.5,
      margin = margin(t = 5, b = 5)
    ),
    plot.margin = unit(c(-0.2, 0,0, 1), "cm"),
    legend.position = c(.5, .95),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(colour = guide_legend(nrow = 1))

grid.newpage()
grid.draw(rbind(
  ggplotGrob(plot_entgelt),
  ggplotGrob(plot_delta_me),
  size = "last"
))


################################################################################
# Histogramm der relativen Änderungen ohne Zustände -----------------------
################################################################################
mean_deltas <- mean(entgelt$relAenderung, na.rm=TRUE)
sd_deltas <-  sd(entgelt$relAenderung, na.rm=TRUE)
x <-  na.omit(as.data.frame(entgelt$relAenderung))
colnames(x) <- "x"
kde <- density(data$relAenderung) #bandwidth in kde$bw
bandweite <- kde$bw

n20 <- ggplot(x, aes(x)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = round(sqrt(nrow(x)),0),
    color = "#000000",
    fill = "#D9D9D9"
  ) +
  geom_line(aes(y = after_stat(density)), stat = "density", linetype="dashed", lwd= 0.5, bw= bandweite) +
  geom_vline(xintercept = 0.023, linetype="dotted", lwd= 0.5, color="red")+
  labs(
    #title = "Histogram ME Entgelt Deltas",
    subtitle = TeX(  r"($mit\ \lfloor\sqrt{n}\rfloor =4\ \Klassen,\ \b=0.01$)"),
    x = "rel. Aenderung",
    y = "Dichte"
  ) +
  #scale_x_continuous(bins=nrow(x))+
  theme_classic()

 

n <- ggplot(x, aes(x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = ceiling(min(c(sqrt(nrow(
                   x
                 )), 10 * log(nrow(
                   x
                 ), base = 10)))),
                 color = "#000000",
                 fill = "#D9D9D9") + 
  geom_line(aes(y = after_stat(density)), stat = "density", linetype="dashed", lwd= 0.5, bw= bandweite) +
  geom_vline(xintercept = 0.023, linetype="dotted", lwd= 0.5, color="red")+
  scale_x_continuous()+
  labs(
    #title = "Histogram ME Entgelt Deltas",
    subtitle = TeX(  r"($mit\ \lceil min\{\sqrt{n}, 10 \log _{10} n\}\rceil =5\ \Klassen,\ \b=0.01$)"),
    x = "rel. Aenderung",
    y = "Dichte"
  ) +
  theme_classic()

n10 <- ggplot(x, aes(x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = ceiling(1+3.3*log(nrow(x), base = 10)),
                 color = "#000000",
                 fill = "#D9D9D9") +
  geom_line(aes(y = after_stat(density)), stat = "density", linetype="dashed", lwd= 0.5, bw= bandweite) +
  geom_vline(xintercept = 0.023, linetype="dotted", lwd= 0.5, color="red")+
  labs(
    #title = "Histogram Entgelt ME Deltas",
    subtitle = TeX(  r"($mit\ \lceil 1+3.3 \log _{10} n\rceil =6\ \Klassen,\ \b=0.01$)"),
    x = "rel. Aenderung",
    y = "Dichte"
  ) +
  theme_classic()

#grid.draw(cbind(ggplotGrob(n20), ggplotGrob(n), ggplotGrob(n10), size = "last"))

grid.arrange(n20,n,n10,nrow=1, top=textGrob("relative Aenderungen Tarifentgelte M+E-Industrie: Histogramme der Verteilung (n=19)"))


################################################################################
# Forward Algorithm -------------------------------------------------------
################################################################################

#Notationen
#  x =  Emissionen, Beobachtungen oder abhaengigen Variablen X
#  a = Matrix der Übergangswahrscheinlichkeiten oder Übergangsmatrix (M x M Matrix)
#  mu = Mittelwerte der Emissionen
#  sig = Standardabweichungen der Emissionen

#Überführe die rel. Änderungen aus dem df entgelt in ein neues df zur Modellierung
df_train <- data.frame(x = na.omit(entgelt$relAenderung))

#Für die Animation
mu_gif <- data.frame(mu1 = numeric(0),mu2 = numeric(0))
sig_gif <- data.frame(sig1 = numeric(0),sig2 = numeric(0))


# Definition Forward Algorithm --------------------------------------------
forward <- function(x, a, mu, sig) {
  N <- length(x)
  M <- length(mu)
  logalpha <- matrix(0, nrow = N, ncol = M) #Initialisierung Matrix
  
  # Initialisierung
  Delta <- rep(1/M, M) #Anfangswahrscheinlichkeiten, 1/Anzahl der Zustaende
  logalpha[1, ] <- log(Delta) + dnorm(x[1], mean = mu, sd = sig, log = T)
  
  global <- environment()  # Zugriff auf das globale Environment
  
  # Rekursion
  for (t in 2:N) {
    for (j in 1:M) {
      log_sum <- logalpha[t - 1, ] + log(a[, j]) +
        dnorm(x[t], mean = mu[j], sd = sig[j], log = T)
      max_log_sum <- max(log_sum)
      logalpha[t, j] <- max_log_sum + log(sum(exp(log_sum - max_log_sum)))
      
      # Speichern von mu und sig in _gif nach jeder Iteration (deaktivieren, da längere Rechenzeit)
      # mu_gif <<- rbind(mu_gif, data.frame(mu1 = mu[1], mu2= mu[2]))
      # sig_gif <<- rbind(sig_gif, data.frame(sig1 = sig[1], sig2= sig[2]))
    }
  }

  # Terminierung
  final_log_sum <- logalpha[N, ]
  max_final_log_sum <- max(final_log_sum)
  return(max_final_log_sum + log(sum(exp(final_log_sum -
                                           max_final_log_sum))))
}


# Definition Zielfunktion für die Optimierung -----------------------------
ziel <- function(y) {
  # transformieren des Parametervektor y in die drei Parameter des Forward Algorithm
  a1 <-
    matrix(y[1:(M * (M - 1))], ncol = M - 1, byrow = TRUE)  # Matrix a ohne die rechte Spalte
  a2 <- matrix(1 - rowSums(a1), ncol = 1)  # Matrix a rechte Spalte
  a <- cbind(a1, a2)
  #print(a)
  mu <- y[(M * (M - 1) + 1):(M * (M - 1) + M)]
  sig <- y[(M * (M - 1) + M + 1):length(y)]
  # Rückgabe der negativen Log-Likelihood
  return(-forward(df_train$x, a, mu, sig))
}


# Initialwerte der Parameter und Bounds -----------------------------------
# Anzahl der möglichen Zustände
M <- 2  

# Initialwerte der Übergangsmatrix /initial distribution /Anfangsverteilung
a_init <- matrix(c(0.5, 0.5), ncol = 1)  

# Initialwerte Mittelwerte der Emissionen
mu_init <- c(0.02, 0.04)  

# Initialwerte Standardabweichungen der Emissionen
sig_init <- c(0.05, 0.05)  

# Parameter werden zu einem einzelnen Vektor y0 zusammengefÃ¼gt
y0 <- c(a_init, mu_init, sig_init)

# Initialisierung der Grenzen fÃ¼r die Optimierung
bounds <- list()

# Setzen der Grenzen für die Optimierung
for (i in 1:(M * (M - 1))) {
  bounds[[i]] <- c(0, 1)
}
for (i in 1:(2 * M)) {
  bounds[[M * (M - 1) + i]] <- c(0, Inf)
}



# Training des Models -----------------------------------------------------
Ergebnis_optim <- optim(
                    par = y0,
                    fn = ziel,
                    method = "L-BFGS-B",
                    lower = sapply(bounds, function(y)
                      y[[1]]),
                    upper = sapply(bounds, function(y)
                      y[[2]])
                  )


# Ausgabe der optimierten Parameter ---------------------------------------
a1 <- matrix(Ergebnis_optim$par[1:(M * (M - 1))], ncol = M - 1, byrow = TRUE)
a2 <- matrix(1 - rowSums(a1), ncol = 1)
a <- cbind(a1, a2)
rownames(a) <- c("Zustand 1", "Zustand 2")
colnames(a)<- c("Zustand 1", "Zustand 2")

mu <- Ergebnis_optim$par[(M * (M - 1) + 1):(M * (M - 1) + M)]
sig <- Ergebnis_optim$par[(M * (M - 1) + M + 1):length(Ergebnis_optim$par)]
Parameter_opt <- rbind(mu, sig)
colnames(Parameter_opt) <- c("Zustand 1", "Zustand 2")
rownames(Parameter_opt) <- c("Mittelwerte", "Standardabweichungen")
ML <- (-Ergebnis_optim$value)

sink("Ergebnis.txt")
cat("> Ergebnis\n")
cat("==========================================\n")
cat("Anzahl der verdeckten Zustände:\n")
cat("------------------------------------------\n")
cat(M, "\n\n")

cat("Anzahl der Beobachtungen/Emissionen: \n")
cat("------------------------------------------\n")
cat(length(x), "\n\n")

cat("Range der Beobachtungen/Emissionen:\n")
cat("------------------------------------------\n")
cat(range(df_train), "\n\n")

cat("Anfangsverteilung:\n")
cat("------------------------------------------\n")
cat(a_init, "\n\n")

cat("Parameter optimiert:\n")
cat("------------------------------------------\n")
print(Parameter_opt, digits = 7, row.names=c("Zustand 1", "Zustand 2"), col.names=c("Mittelwerte", "Standardabweichungen"))

cat("\nÜbergangsmatrix:\n")
cat("------------------------------------------\n")
print(a, digits=7, row.names=c("Zustand 1", "Zustand 2"), col.names=c("Zustand 1", "Zustand 2"))

cat("\nnegative Maximum Log-Likelihood:\n")
cat("------------------------------------------\n")
cat(ML, "\n")
sink()
  cat(readLines("Ergebnis.txt"), sep = "\n")


################################################################################
# Viterbi Algorithm und Zustandschätzung ----------------------------------
################################################################################


# Viterbi Algorithmus für die Schätzung der Zustände ----------------------
# Initialisierung der Variablen
x <- df_train$x
N <- length(x)

# Initialisierung für den Vektor der verdeckten Zustände
z <- rep(0, N)

# Initialisierung des Vektors für die größte log Wahrscheinlichkeit der Kette
best_logp <- matrix(0, nrow = N, ncol = M)

# Initialisierung des Vektors für das Backtracking
back_ptr <- matrix(0, nrow = N, ncol = M)

# Initialisierung der Wahrscheinlichkeiten
best_logp[1,] <- dnorm(x[1], mean = mu, sd = sig, log = TRUE)

# Rekursion zur Berechnung der besten log Wahrscheinlichkeiten und Backtracking
for (t in 2:N) {
  for (k in 1:M) {
    logp <- best_logp[t - 1,] + log(a[,k]) +
      dnorm(x[t], mean = mu[k], sd = sig[k], log = TRUE)
    
    best_logp[t, k] <- max(logp)
    back_ptr[t, k] <- which.max(logp)
  }
}

# Backtracking zur Ermittlung der verdeckten ZustÃ¤nde
z[N] <- which.max(best_logp[N,])
for (t in 2:N) {
  z[N - t + 1] <- back_ptr[N - t + 2, z[N - t + 2]]
}




# Ableitung der Vorhersage aus den gesch. Zuständen und Parametern --------
x_Prognose_mean <- rep(0, N)
x_Prognose_std <- rep(0, N)
for (k in 1:M) {
  x_Prognose_mean[z == k] <- mu[k]
  x_Prognose_std[z == k] <- sig[k]
}

#Zeitreihe der Prognosen/Schaetzungen für einen Vergleich mit den realen Stichprobenwerten
#set.seed(1)
x_Prognose <- rnorm(N, mean = x_Prognose_mean, sd = x_Prognose_std)


cat(readLines("Ergebnis.txt"), sep = "\n")

###############################################################################
# weitere Visualisierungen ------------------------------------------------
###############################################################################


# Visualisierung Zeitreihen mit geschäzten Zuständen und Paramet. --------
data <-  entgelt[2:nrow(entgelt), c("Jahr", "relAenderung", "tarifliches_jahresentgelt_ME")]
data$Jahr <- as.Date(ISOdate(data$Jahr, 12, 31))
data$pred <- x_Prognose
#Taschenspielertrick um die 1 durch 2 und umgekehrt zu ersetzen, 
#damit die Zustände inutitiver sind, 1=niedrige Aenderung, 2=hohe Aenderung
data$zustaende <- ifelse(z == 1, 2, ifelse(z == 2, 1, z)) 


plot_entgelt <-
  ggplot(data, aes(x = Jahr, y = tarifliches_jahresentgelt_ME)) +
  geom_point() +
  geom_line() +
  scale_x_date() +
  scale_y_continuous(labels = scales::label_dollar(prefix = "€")) +
  labs(title = "M+E-Industrie Ø Jahresentgelt") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(
      size = 11,
      face = "bold",
      hjust = 0.5,
      margin = margin(t = 5, b = 5)
    ),
    plot.margin = unit(c(0, 1, 0, 1), "cm")
  )

plot_delta_me <- ggplot(data, aes(x = Jahr)) +
  geom_point(aes(y = relAenderung)) +
  geom_point(aes(y = pred)) +
  geom_line(aes(y = relAenderung, colour = "Beobachtung")) +
  geom_line(aes(y = pred, colour = "Vorhersage")) +
  scale_x_date() +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(title = "relative Änderung Jahresentgelt", x = "Jahr") +
  scale_colour_manual(
    "",
    breaks = c("Beobachtung", "Vorhersage"),
    values = c("#029386", "red")
  ) +
  #geom_hline(yintercept = mu[1]-sig[1], linetype="dotted", lwd= 1, color="red")+
  #geom_hline(yintercept = mu[1]+sig[1], linetype="dotted", lwd= 1, color="red")+
  #geom_hline(yintercept = mu[2]-sig[2], linetype="dotted", lwd= 1, color="green")+
  #geom_hline(yintercept = mu[2]+sig[2], linetype="dotted", lwd= 1, color="green")+
  #geom_ribbon(aes(x=Jahr, y=x_Prognose_mean, ymax=x_Prognose_mean+x_Prognose_std, ymin=x_Prognose_mean-x_Prognose_std), alpha=0.05, fill="red") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(
      size = 11,
      face = "bold",
      hjust = 0.5,
      margin = margin(t = 5, b = 5)
    ),
    plot.margin = unit(c(-0.2, 1,-0.2, 1), "cm"),
    legend.position = c(.5, .2),
    legend.background = element_rect(fill = "transparent")
  ) +
  guides(colour = guide_legend(nrow = 1))
#legend.justification = c("right", "top"),
#legend.box.just = "right",
#legend.margin = margin(0.1, 0.1, 0.1, 0.1))#+
#geom_hline(yintercept= mu[1], linetype="dashed", color = "red") + #horizontale Linien mit Mittelwerten
#geom_hline(yintercept= mu[2], linetype="dashed", color = "green")

plot_zustaende <- ggplot(data, aes(x = Jahr, y = zustaende)) +
  geom_point() +
  geom_line() +
  scale_x_date() +
  scale_y_continuous(breaks = c(1, 2), labels = c("1"="niedrige\nEntgeltänd.","2"="hohe\nEntgeltänd.")) +
  labs(title = "Zustände", x = "Jahr") +
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(
      size = 11,
      face = "bold",
      hjust = 0.5,
      margin = margin(t = 5, b = 5)
    ),
    plot.margin = unit(c(0, 1, 1, 1), "cm")
  )


grid.newpage()
grid.draw(rbind(
  ggplotGrob(plot_entgelt),
  ggplotGrob(plot_delta_me),
  ggplotGrob(plot_zustaende),
  size = "last"
))


# Histogramm der relativen Änderungen mit Zuständen ------------
mean_deltas <- mean(data$relAenderung)
sd_deltas <-  sd(data$relAenderung)
x <- as.data.frame(data$relAenderung)
colnames(x) <- "x"
colors <- c("Zustand 1"="#00A1E4", "Zustand 2"="#DC0073")

n20 <- ggplot(x, aes(x)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = nrow(x),
    color = "#000000",
    fill = "#D9D9D9"
  ) +
  geom_density() +
  stat_function(fun = dnorm,
                n = 100,
                args = list(mean = mu[1] , sd = sig[1]),aes(color= "Zustand 1"), lwd= 0.6) +
  stat_function(fun = dnorm,
                n = 100,
                args = list(mean = mu[2] , sd = sig[2]),aes(color= "Zustand 2"), lwd= 0.6) +
  labs(
    subtitle = "mit n=19 Klassen",
    x = "rel. Entgeltänderung",
    y = "Dichte"
  ) +
  theme_classic()+
  theme(legend.position = "none")

bins <- ceiling(min(c(sqrt(nrow(
  x
)), 10 * log(nrow(
  x
), base = 10))))

n <- ggplot(x, aes(x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = bins,
                 color = "#000000",
                 fill = "#D9D9D9") +
  geom_density() +
  stat_function(fun = dnorm,
                n = 100,
                args = list(mean = mu[1] , sd = sig[1]),aes(color= "Zustand 1"), lwd= 0.6) +
  stat_function(fun = dnorm,
                n = 100,
                args = list(mean = mu[2] , sd = sig[2]),aes(color= "Zustand 2"), lwd= 0.6) +
  labs(
    subtitle = TeX(
      r"($mit\ \lceil min\{\sqrt{n}, 10 \log _{10} n\}\rceil = 5\ Klassen$)"
    ),
    x = "rel. Entgeltänderung",
    y = "Dichte"
  ) +
  theme_classic()+
  theme(legend.position = c(.5, 1), legend.title = element_blank(), legend.background = element_blank())+
  guides(colour = guide_legend(nrow = 1))


n10 <- ggplot(x, aes(x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 10,
                 color = "#000000",
                 fill = "#D9D9D9") +
  geom_density() +
  stat_function(fun = dnorm,
                n = 100,
                args = list(mean = mu[1] , sd = sig[1]), aes(color= "Zustand 1"), lwd= 0.6) +
  stat_function(fun = dnorm,
                n = 100,
                args = list(mean = mu[2] , sd = sig[2]), aes(color = "Zustand 2"), lwd=0.6) +
  labs(
    subtitle = "mit 10 Klassen",
    x = "rel. Entgeltänderung",
    y = "Dichte"
  ) +
  theme_classic()+
  theme(legend.position = "none")

grid.arrange(n20,n,n10,nrow=1, top=textGrob("relative Änderungen Tarifentgelte M+E-Industrie: Histogramme der Verteilung (n=19)"))

cat(readLines("Ergebnis.txt"), sep = "\n")

# Gif der Optimierung -----------------------------------------------------
colors <- c("Kerndichteschätzung" = "black", "Mittelwerte HMM" = "#00A1E4", "Verteilungen HMM" = "#00A1E4", "Verteilungen wÃ¤hrend Optimierung" = "#DC0073")
#saveGIF({
#    for (i in 1:nrow(mu_gif)) {
#      if (i %% 100 == 0|| i == 1) { 
#      plot_gif<-ggplot(x, aes(x)) +
#        geom_histogram(aes(y = after_stat(density)),
#                      bins = 10,
#                      color = "#000000",
#                       fill = "#D9D9D9") +
#        scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 10))+
#        geom_density(adjust = 3/4, linetype="longdash", aes(color= "Kerndichteschätzung"), lwd=1, alpha = 0.75) +
#        geom_vline(xintercept = mu[1], linetype="dotted", lwd= 1, aes(color="Mittelwerte HMM"), alpha = 0.5) +
#        geom_vline(xintercept = mu[2], linetype="dotted", lwd= 1,aes(color="Mittelwerte HMM"), alpha = 0.5) +
#        stat_function(fun = dnorm,
#                      n = 100,
#                      args = list(mean = mu[1] , sd = sig[1]), aes(color = "Verteilungen HMM"), lwd=0.8, alpha = 0.5) +
#        stat_function(fun = dnorm,
#                      n = 100,
#                      args = list(mean = mu[2] , sd = sig[2]), aes(color = "Verteilungen HMM"), lwd=0.8, alpha = 0.5) +
#        stat_function(fun = dnorm,
#                      n = 100,
#                      args = list(mean = mu_gif[i,1] , sd = sig_gif[i,1]), lwd =1, aes(color="Verteilungen während Optimierung")) +
#        stat_function(fun = dnorm,
#                      n = 100,
#                      args = list(mean = mu_gif[i,2] , sd = sig_gif[i,2]), lwd =1, aes(color ="Verteilungen während Optimierung")) +
#        labs(
#          title = paste("Histogram rel. Änderungen Tarifentgelte               Iteration: ",i, "von", nrow(mu_gif)),
#          subtitle = "mit 10 Klassen",
#          x = "relative Änderungen Entgelte von Tarifrundenjahr zu Tarifrundenjahr",
#          y = "Dichte",
#          color=""
#        ) +
#        scale_color_manual(values = colors)+
#        guides(color = guide_legend(override.aes = list(linetype = c(1,1,1))))+
#        theme_classic()+
#        theme(legend.position = c(0.2, 0.9))
#        print(plot_gif)
#      # Pause zwischen den Frames
#       ani.pause(0.003)
#      }
#    }
#  }, movie.name = "optimierung2.gif")

cat(readLines("Ergebnis.txt"), sep = "\n")