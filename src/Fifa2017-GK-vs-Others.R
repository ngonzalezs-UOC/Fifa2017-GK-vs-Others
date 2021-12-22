## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----llibreries, include=FALSE----------------------------------------------------------------------------------------------------------------------
library(knitr)
library(kableExtra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(gridExtra)
library(corrplot)
library(lmtest)
library(caret)
library(Hmisc)
library(nortest)
library(goftest)


## ----taula jugador, echo=FALSE, message=FALSE, results='asis', warnings=FALSE-----------------------------------------------------------------------
# Taula descriptiva de les variables seleccionades del dataset FIFA 2017

taula.jugador <- "
| Variable               | Descripció                                                                          | Tipus       |
|:-----------------------|:------------------------------------------------------------------------------------|:------------|
| Name                   | Nom del jugador                                                                     | categòric   |
| National_Position      | Posició del jugador a la selecció nacional                                          | categòric   |
| Club_Position          | Posició del jugador al seu equip                                                    | categòric   |
| Rating                 | Valoració global del jugador, entre 0 i 100                                         | numèric     |
| Height                 | Alçada del jugador, en centímetres                                                  | numèric     |
| Weight                 | Pes del jugador, en quilograms                                                      | numèric     |
| Age                    | Edat del jugador, en anys                                                           | numèric     |
| Work_Rate              | Valoració qualitativa del jugador en termes d’atac-defensa                          | categòric   |
| Ball_Control           | Habilitat del jugador per controlar la pilota, entre 0 i 100                        | numèric     |
| Vision                 | Habilitat del jugador en termes de visió de joc, entre 0 i 100                      | numèric     |
"
cat(taula.jugador)


## ----carrega del fitxer csv-------------------------------------------------------------------------------------------------------------------------
# Càrrega del fitxer csv
df <- read.csv("Fifa2017_original.csv",   # per defecte header=TRUE i sep=","  
               strip.white = TRUE,        # espais eliminats a principi i final d'string
               stringsAsFactors = TRUE,   # strings convertits a factor
               na.strings = c(""))        # strings buits convertits a valor NA  


## ----seleccio de caracteristiques-------------------------------------------------------------------------------------------------------------------
# Selecció de característiques d'interès
df <- df %>% select(Name, National_Position, Club_Position, Rating,
                    Height, Weight, Age, Work_Rate, Ball_Control, Vision)


## ----estructura del dataset-------------------------------------------------------------------------------------------------------------------------
# Estructura del data frame
str(df)


## ----conversio de Height i Weight-------------------------------------------------------------------------------------------------------------------
# Tractament de Height
df$Height <- gsub(" ", "", df$Height)    # Treure blancs
df$Height <- gsub("cm$", "", df$Height)  # Treure text "cm"
df$Height <- as.numeric(df$Height)       # Convertir a numèric

# Tractament de Weight
df$Weight <- gsub(" ", "", df$Weight)    # Treure blancs
df$Weight <- gsub("kg$", "", df$Weight)  # Treure text "kg"
df$Weight <- as.numeric(df$Weight)       # Convertir a numèric


## ----conversio de Work_Rate-------------------------------------------------------------------------------------------------------------------------
# Tractament de Work_Rate
df$Work_Rate <- relevel(df$Work_Rate, ref="High / High")  # Establir valor referència


## ----creacio de Goalkeeper--------------------------------------------------------------------------------------------------------------------------
# Tractament de Goalkeeper
df$Goalkeeper <- as.factor(ifelse(df$Club_Position == "GK", "YES", "NO"))  # Creació
df$Goalkeeper <- relevel(df$Goalkeeper, ref="YES")                         # Valor referència


## ----creacio de International-----------------------------------------------------------------------------------------------------------------------
# Tractament de International
df$International <- as.factor(ifelse(is.na(df$National_Position), "NO", "YES"))  # Creació


## ----creacio de IMC---------------------------------------------------------------------------------------------------------------------------------
# Tractament de IMC
df$IMC <- df$Weight/((df$Height/100)^2)


## ----comprovacio de registre duplicats--------------------------------------------------------------------------------------------------------------
# Comprovació de registres duplicats
sum(duplicated(df))


## ----resum del dataset------------------------------------------------------------------------------------------------------------------------------
# Resum estadístic
summary(df)


## ----examen de Work_Rate----------------------------------------------------------------------------------------------------------------------------
# Examen de Work_Rate
kable(t(table(df$Work_Rate)), booktabs = TRUE,) %>% 
  kable_styling(latex_options = c("HOLD_position","scale_down"), position = "center")


## ----examen de Club_Position i Goalkeeper, fig.pos='H'----------------------------------------------------------------------------------------------
# Examen de Club_Position i Goalkeeper
kable(filter(df, is.na(Club_Position) | is.na(Goalkeeper)), booktabs = TRUE) %>% 
  kable_styling(latex_options = c("HOLD_position","scale_down"), position = "center")


## ----eliminació de registres missing----------------------------------------------------------------------------------------------------------------
# Eliminació dels registres missing identificats
df <- filter(df, !is.na(Club_Position))


## ----boxplot, fig.height=8, fig.width=16, message=FALSE, warnings=FALSE, error=FALSE----------------------------------------------------------------
# Boxplot (comprovació d'outliers)
bp1 <- ggplot(df, aes(y=Rating)) + geom_boxplot(fill="aquamarine2") + theme_bw()
bp2 <- ggplot(df, aes(y=Height)) + geom_boxplot(fill="lightblue3") + theme_bw()
bp3 <- ggplot(df, aes(y=Weight)) + geom_boxplot(fill="lightblue4") + theme_bw()
bp4 <- ggplot(df, aes(y=Age)) + geom_boxplot(fill="bisque2") + theme_bw()
bp5 <- ggplot(df, aes(y=Ball_Control)) + geom_boxplot(fill="khaki2") + theme_bw()
bp6 <- ggplot(df, aes(y=Vision)) + geom_boxplot(fill="wheat3") + theme_bw()
grid.arrange(bp1,bp2,bp3,bp4,bp5,bp6, nrow=2, ncol=3)


## ----test normalitat, fig.pos='H', message=FALSE, warnings=FALSE, error=FALSE-----------------------------------------------------------------------
# Test de normalitat

# Funció que aplica els tests sobre una variable passada per paràmetre
Test_Normalitat <- function(Camp, Nom, Matriu) {
  ll <- lillie.test(Camp)$p.value
  ad <- ad.test(Camp)$p.value
  cm <- cvm.test(Camp, "pnorm")$p.value
  Matriu <- rbind(Matriu, as.character(c(ll, ad, cm)))
  rownames(Matriu)[nrow(Matriu)] <- Nom
  return(Matriu)
}

# Matriu amb el resultat dels tests
M <- matrix(nrow=0, ncol=3)
colnames(M) <- c("Lilliefors", "Anderson-Darling", "Cramer-von Mises")

# Bucle que aplica els tests a les variables numèriques
for (i in 1:ncol(df)) {
  if (class(df[,i]) %in% c("integer", "numeric")) {
    M <- Test_Normalitat(df[,i], colnames(df)[i], M)
  }
}

# Presentació del resultat
kable(M, booktabs = TRUE, caption = "Test de normalitat") %>% 
  kable_styling(latex_options = "HOLD_position", position = "center")


## ----grafics normalitat, fig.height=12, fig.width=16, message=FALSE, warnings=FALSE, error=FALSE----------------------------------------------------
# Histogrames i gràfics Q-Q (estudi de la normalitat)

h1 <- ggplot(data=df, aes(x = Rating)) + theme_bw() + ggtitle("Histogram") +
      geom_histogram(aes(y=..density..), fill="aquamarine2",  binwidth=1) +
      geom_vline(aes(xintercept=mean(Rating)), color="red", size=0.2) +
      stat_function(fun=dnorm, lwd=0.5, 
                   col='red', args=list(mean=mean(df$Rating), sd=sd(df$Rating))) 

q1 <- ggplot(df, aes(sample=Rating)) + theme_bw() +
      stat_qq(col='aquamarine2') + stat_qq_line(lwd=0.5, col='red') +
      ggtitle("Q-Q Plot") + xlab("Theoretical quantiles") +ylab("Rating")

h2 <- ggplot(data=df, aes(x = Age)) + theme_bw() + ggtitle("Histogram") +
      geom_histogram(aes(y=..density..), fill="bisque2",  binwidth=1) +
      geom_vline(aes(xintercept=mean(Age)), color="red", size=0.2) +
      stat_function(fun=dnorm, lwd=0.5, 
                   col='red', args=list(mean=mean(df$Age), sd=sd(df$Age))) 

q2 <- ggplot(df, aes(sample=Age)) + theme_bw() +
      stat_qq(col='bisque2') + stat_qq_line(lwd=0.5, col='red') +
      ggtitle("Q-Q Plot") + xlab("Theoretical quantiles") +ylab("Age")


h3 <- ggplot(data=df, aes(x = Height)) + theme_bw() + ggtitle("Histogram") +
      geom_histogram(aes(y=..density..), fill="lightblue3",  binwidth=2) +
      geom_vline(aes(xintercept=mean(Height)), color="red", size=0.2) +
      stat_function(fun=dnorm, lwd=0.5, 
                   col='red', args=list(mean=mean(df$Height), sd=sd(df$Height))) 

q3 <- ggplot(df, aes(sample=Height)) + theme_bw() +
      stat_qq(col='lightblue3') + stat_qq_line(lwd=0.5, col='red') +
      ggtitle("Q-Q Plot") + xlab("Theoretical quantiles") +ylab("Height")

h4 <- ggplot(data=df, aes(x = Weight)) + theme_bw() + ggtitle("Histogram") +
      geom_histogram(aes(y=..density..), fill="lightblue4",  binwidth=2) +
      geom_vline(aes(xintercept=mean(Weight)), color="red", size=0.2) +
      stat_function(fun=dnorm, lwd=0.5, 
                   col='red', args=list(mean=mean(df$Weight), sd=sd(df$Weight))) 

q4 <- ggplot(df, aes(sample=Weight)) + theme_bw() +
      stat_qq(col='lightblue4') + stat_qq_line(lwd=0.5, col='red') +
      ggtitle("Q-Q Plot") + xlab("Theoretical quantiles") +ylab("Weight")

h5 <- ggplot(data=df, aes(x = IMC)) + theme_bw() + ggtitle("Histogram") +
      geom_histogram(aes(y=..density..), fill="thistle",  binwidth=0.2) +
      geom_vline(aes(xintercept=mean(IMC)), color="red", size=0.2) +
      stat_function(fun=dnorm, lwd=0.5, 
                   col='red', args=list(mean=mean(df$IMC), sd=sd(df$IMC))) 

q5 <- ggplot(df, aes(sample=IMC)) + theme_bw() +
      stat_qq(col='thistle') + stat_qq_line(lwd=0.5, col='red') +
      ggtitle("Q-Q Plot") + xlab("Theoretical quantiles") +ylab("IMC")

h6 <- ggplot(data=df, aes(x = Ball_Control)) + theme_bw() + ggtitle("Histogram") +
      geom_histogram(aes(y=..density..), fill="khaki2",  binwidth=1) +
      geom_vline(aes(xintercept=mean(Ball_Control)), color="red", size=0.2) +
      stat_function(fun=dnorm, lwd=0.5, 
                   col='red', args=list(mean=mean(df$Ball_Control), sd=sd(df$Ball_Control))) 

q6 <- ggplot(df, aes(sample=Ball_Control)) + theme_bw() +
      stat_qq(col='khaki2') + stat_qq_line(lwd=0.5, col='red') +
      ggtitle("Q-Q Plot") + xlab("Theoretical quantiles") +ylab("Ball_Control")

h7 <- ggplot(data=df, aes(x = Vision)) + theme_bw() + ggtitle("Histogram") +
      geom_histogram(aes(y=..density..), fill="wheat3",  binwidth=1) +
      geom_vline(aes(xintercept=mean(Vision)), color="red", size=0.2) +
      stat_function(fun=dnorm, lwd=0.5, 
                   col='red', args=list(mean=mean(df$Vision), sd=sd(df$Vision))) 

q7 <- ggplot(df, aes(sample=Vision)) + theme_bw() +
      stat_qq(col='wheat3') + stat_qq_line(lwd=0.5, col='red') +
      ggtitle("Q-Q Plot") + xlab("Theoretical quantiles") +ylab("Vision")

grid.arrange(h1,q1,h2,q2,h3,q3,h4,q4,h5,q5,h6,q6,h7,q7, nrow=4, ncol=4)


## ----segmentacio porters i jugadors-----------------------------------------------------------------------------------------------------------------
# Segmentació de variables numèriques per a porters i jugadors
Porters  <- df %>% filter(Goalkeeper=="YES") %>%
            select(Rating, Height, Weight, Age, Ball_Control, Vision, IMC)

Jugadors <- df %>% filter(Goalkeeper=="NO") %>%
            select(Rating, Height, Weight, Age, Ball_Control, Vision, IMC)


## ----analisi de correlacio per a porters, fig.pos='H'-----------------------------------------------------------------------------------------------
# Anàlisi de correlació per als porters
kable(rcorr(as.matrix(Porters), type = "spearman")$P, booktabs = TRUE) %>% 
  kable_styling(latex_options = c("HOLD_position","scale_down"), position = "center")


## ----analisi de correlacio per a jugadors, fig.pos='H'----------------------------------------------------------------------------------------------
# Anàlisi de correlació per als jugadors de camp
kable(rcorr(as.matrix(Jugadors), type = "spearman")$P, booktabs = TRUE) %>% 
  kable_styling(latex_options = c("HOLD_position","scale_down"), position = "center")


## ----coeficients de correlacio, fig.height=3, fig.width=8, message=FALSE, warnings=FALSE, error=FALSE-----------------------------------------------
# Coeficients de correlació
par(mfrow=c(1,2))

corrplot(corr = cor(x = Porters, method = "spearman"), 
         method = "number", type = "lower", cl.cex = 0.5, number.cex = 0.6,
         tl.col = "black", tl.srt = 45, tl.cex = 0.6, order = "alphabet", 
         mar=c(0,0,1,0), title="Porters")

corrplot(corr = cor(x = Jugadors, method = "spearman"), 
         method = "number", type = "lower", cl.cex = 0.5, number.cex = 0.6,  
         tl.col = "black", tl.srt = 45, tl.cex = 0.6, order = "alphabet",
         mar=c(0,0,1,0), title="Jugadors de camp")


## ----segmentacio IMC porters i jugadors-------------------------------------------------------------------------------------------------------------
# Segmentació d'IMC de porters i jugadors
IMC_porters <-  df$IMC[df$Goalkeeper=="YES"]
IMC_jugadors <- df$IMC[df$Goalkeeper=="NO"]


## ----test d’homoscedasticitat porters vs jugadors---------------------------------------------------------------------------------------------------
# Test d’homoscedasticitat per a IMC, porters vs. jugadors
var.test(x=IMC_porters, y=IMC_jugadors, alternative="two.sided", conf.level=0.95)


## ----calcul test hipotesi amb t.test----------------------------------------------------------------------------------------------------------------
# Càlcul del test d'hipòtesis amb t.test (IMC, porters vs. jugadors)
t.test(x=IMC_porters, y=IMC_jugadors, 
       alternative="two.sided", var.equal=TRUE, conf.level=0.95)


## ----boxplot IMC, fig.height=3, fig.width=6, message=FALSE, warnings=FALSE, error=FALSE-------------------------------------------------------------
# Diagrames de caixa de l'IMC per a Goalkeeper

ggplot(df, aes(x=Goalkeeper, y=IMC, fill = Goalkeeper)) + 
geom_boxplot(alpha=0.3, show.legend = TRUE) + theme_bw() + 
stat_summary(fun=mean, geom="point", size=2, color="red", fill="red") +
scale_fill_discrete(name = "", labels = c("Porter", "Jugador de camp")) +
ggtitle("Boxplot de l'IMC (porter vs jugador)") + xlab("Goalkeeper") +
theme(plot.title = element_text(hjust = 0.5))


## ----regressio lineal multiple----------------------------------------------------------------------------------------------------------------------
# Models de regressió lineal múltiple per a 'Rating'
m1  <- lm(data=df, Rating ~ Work_Rate)
m2  <- lm(data=df, Rating ~ Work_Rate + Ball_Control)
m3  <- lm(data=df, Rating ~ Work_Rate + Vision)
m4  <- lm(data=df, Rating ~ Work_Rate + Goalkeeper)
m5  <- lm(data=df, Rating ~ Work_Rate + Ball_Control + Goalkeeper)
m6  <- lm(data=df, Rating ~ Work_Rate + Vision + Goalkeeper)
m7  <- lm(data=df, Rating ~ Work_Rate + Ball_Control + Vision) 
m8  <- lm(data=df, Rating ~ Work_Rate + Ball_Control + Vision + Goalkeeper) 
m9  <- lm(data=df, Rating ~ Ball_Control)
m10 <- lm(data=df, Rating ~ Ball_Control + Vision)
m11 <- lm(data=df, Rating ~ Ball_Control + Goalkeeper)
m12 <- lm(data=df, Rating ~ Ball_Control + Vision + Goalkeeper)
m13 <- lm(data=df, Rating ~ Vision)
m14 <- lm(data=df, Rating ~ Vision + Goalkeeper)
m15 <- lm(data=df, Rating ~ Goalkeeper)


## ----analisi coeficients determinacio, fig.pos='H'--------------------------------------------------------------------------------------------------
# Anàlisi dels coeficients de determinació
R <- matrix(c(
              1, round(summary(m1)$adj.r.squared,4), round(AIC(m1),3),
              2, round(summary(m2)$adj.r.squared,4), round(AIC(m2),3),
              3, round(summary(m3)$adj.r.squared,4), round(AIC(m3),3),
              4, round(summary(m4)$adj.r.squared,4), round(AIC(m4),3),
              5, round(summary(m5)$adj.r.squared,4), round(AIC(m5),3),
              6, round(summary(m6)$adj.r.squared,4), round(AIC(m6),3),
              7, round(summary(m7)$adj.r.squared,4), round(AIC(m7),3),
              8, round(summary(m8)$adj.r.squared,4), round(AIC(m8),3),
              9, round(summary(m9)$adj.r.squared,4), round(AIC(m9),3),
              10, round(summary(m10)$adj.r.squared,4), round(AIC(m10),3),
              11, round(summary(m11)$adj.r.squared,4), round(AIC(m11),3),
              12, round(summary(m12)$adj.r.squared,4), round(AIC(m12),3),
              13, round(summary(m13)$adj.r.squared,4), round(AIC(m13),3),
              14, round(summary(m14)$adj.r.squared,4), round(AIC(m14),3),
              15, round(summary(m15)$adj.r.squared,4), round(AIC(m15),3)
             ), ncol = 3, byrow = TRUE)
colnames(R) <- c("Núm. model", "R2 ajustat", "AIC")

kable(t(R), booktabs = TRUE, 
      caption="Comparació de models mitjançant R2 ajustat i AIC") %>% 
  kable_styling(latex_options = c("HOLD_position","scale_down"), position = "center")


## ----analisi del model de regressio linal-----------------------------------------------------------------------------------------------------------
# Anàlisi del millor model de regressió lineal múltiple
summary(m8)


## ----prediccio regressio lineal, fig.pos='H'--------------------------------------------------------------------------------------------------------
# Predicció amb el model de regressió lineal múltiple
P <- predict(m8, 
     data.frame(Work_Rate="High / High", Ball_Control=60, Vision=80, Goalkeeper="NO"),  
     interval = "prediction")

kable(P, booktabs = TRUE, caption = "Interval de predicció al 95 per cent") %>% 
  kable_styling(latex_options = "HOLD_position") 


## ----regressio logistica----------------------------------------------------------------------------------------------------------------------------
# Regressió logística per a 'International'
rl <- glm(formula = International ~ Rating + Age + Goalkeeper, 
          data = df, family = binomial(link = logit))
summary(rl)


## ----odds ratio, fig.pos='H'------------------------------------------------------------------------------------------------------------------------
# Càlculs dels ODD RATIO
kable(exp(coefficients(rl)), 
      booktabs = TRUE, caption = "Odd Ratio estimat", col.names=c("OR")) %>% 
  kable_styling(latex_options = "HOLD_position") 


## ----matriu de confusio-----------------------------------------------------------------------------------------------------------------------------
# Selecció de prediccions i observacions
prediccions  <- ifelse(test = rl$fitted.values > 0.5, yes = "YES", no = "NO")
observacions <- as.character(df$International)

# Creació de la matriu (reordenem files i columnes)
matriu_confusio <- table(prediccions, observacions)
matriu_confusio <- matriu_confusio[2:1, 2:1]


## ----indicadors de la matriu de confusio------------------------------------------------------------------------------------------------------------
# Indicadors de la matriu de confusió 
confusionMatrix(matriu_confusio, positive = "YES")


## ----gravacio del dataset---------------------------------------------------------------------------------------------------------------------------
# Gravació del dataset processat (net) en fitxer CSV
write.csv(df, "Fifa2017_final.csv", row.names = FALSE)

