#TD4
library(lmtest)
library(GGally)
library(tcltk)
library(tseries)
library(moments)
data.td <- read.csv("./Td4.csv")

n <- length(data.td$Y)
print(n)

#Building Linear Model
Model <- lm(Y ~ X1 + X2 +X3, data = data.td)

summary <- summary(Model)

cat("-------------- REGRESSION ---------------\n")
print(summary)

#Building Confidence Intervals
coef <- coefficients(summary)
fstat <- summary$fstatistic[1]


confidence.beta <- data.frame(Beta =c("beta0","beta1","beta2","beta3"))

estimated.beta <- coef[,c(1)]
std.error <- coef[,c(2)]


confidence.beta$BI <- estimated.beta - 1.96 * std.error
confidence.beta$BS <- estimated.beta + 1.96 * std.error
cat("--------------- INTERVALLE DE CONFIANCE ---------------\n")
print(confidence.beta)

## Analyse de la Variance
# Table d'anova
SCE <- sum((fitted(Model) - mean(data.td$Y))^2)
SCT <- sum(((data.td$Y) - mean(data.td$Y))^2)
SCR <- sum(resid(Model)^2)

cat("--------------- EQUATION DE LA VARIANCE ---------------\n")
cat("SCR =",SCR,"\n")

cat("SCE =",SCE,"\n") 

cat("SCT =",SCT,"\n")

cat("SCT - SCE -SCR = " ,SCT - SCE -SCR,"\n")

# Validité du modèle
df <- qf(0.95,df1=3,df2=248)
cat("Fc =",fstat ,", Flu =",df,". ")
if(fstat < df){
    cat("Le modèle n'est pas valide \n")
} else {
    cat("Le modèle est valide \n")
}


# Autocorrélation
## Durbin Watson
cat("--------------- TESTS D'AUTOCORRELATION ----------------")
dwtest(Model)

## Ljung-Box
lag <- 2
Box_test <- Box.test(resid(Model), lag=lag, type="Ljung")
chi_box <- qchisq(0.95,lag)
Qstat <- Box_test$statistic

print(Box_test)
cat("Chi2(" , lag , ") = " , chi_box, ", Qstat =", Qstat,"\n")

if(Qstat<chi_box) {
    cat("Il y a non autocorrélation des résidus d'ordre 1 à 2 \n")
} else {
    cat("Il y a autocorrélation des résidus d'ordre 2 \n")
}

## Normalité
# Jarque Bera
JB <- jarque.bera.test(resid(Model))
JB_stat <- JB$statistic
chi2_JB <- qchisq(0.95,2)
cat("--------------- TEST DE NORMALITÉ --------------- \n")
print(JB)
cat("Chi2(2) = " , chi2_JB, ", JB =", JB_stat,"\n")

if(JB_stat < chi2_JB){
    cat("La distribution des résidus est normale \n")
} else {
    cat("La distribution des résidus est non normale \n")
}

# Skewness & Kurtosis
skew <- skewness(resid(Model))
kurt <- kurtosis(resid(Model))
cat("\nTests de Skewness et Kurtosis :\n")
cat("Skewness =", skew, "\n") 
if(abs((skew-0)/sqrt(n/6))<qnorm(0.975,0,1)){
    cat("Il y a symétrie normale des résidus \n")
} else {
    cat("Il n'y a pas symétrie normale des résidus \n")
}
cat("Kurtosis =", kurt, "\n")
if(abs((kurt-3)/sqrt(n/24))<qnorm(0.975,0,1)){
    cat("Il y a applatissement normale des résidus \n")
} else {
    cat("Il n'y a pas applatissement normale des résidus \n")
}

## Homoscéasticité
LM <- n*summary$r.squared
print(LM)

# ARCH

# Goldfeld 
gqtest(Model,fraction=67)

# hardcoded goldfeld
gqdata_1 <- data.td[(1:93),(2:5)]
gqdata_2 <- data.td[(159:252),(2:5)]

gq_model_1 <- lm(Y~X1+X2+X3 , data=gqdata_1)
gq_model_2 <- lm(Y~X1+X2+X3 , data=gqdata_2)

gq_stat <- sum(resid(gq_model_2)^2)/sum(resid(gq_model_1)^2)

print(gq_stat)


X11()
plot(fitted(Model),resid(Model),main="Distribution des résidus")
abline(h=0, lty=2)
X11()
acf(resid(Model),lag.max=lag)
X11()
ggpairs(data.td[2:5])
X11()
hist(resid(Model),main="Distribution des résidus")
#while (!is.null(dev.list())) Sys.sleep(1)

#png("/home/joseph/Documents/td4/test.png", width=1000, height=1000)
#plot(resid(Model))
#dev.off()
#readPNG("/home/joseph/Documents/td4/test.png")
    


