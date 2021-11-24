#TD4
library(car)
library(GGally)

data <- read.csv("Td4.csv")

data_1 <- data[which(data == "2000/01/01"):which(data == "2010/06/01"),]
data_2 <- data[which(data == "2010/07/01"):which(data == "2020/12/01"),]

n <- length(data$Y)
n_1 <- length(data_1$Y)
n_2 <- length(data_2$Y)


## Regressions 
model_0 <- lm(Y~X1+X2+X3, data = data)

model_1 <- lm(Y~X1+X2+X3, data = data_1)

model_2 <- lm(Y~X1+X2+X3, data = data_2)

model_list <- list(model_0,model_1,model_2)

for (i in 1:length(model_list)){
	print(summary(model_list[[i]]))
	#print(nobs(model_list[[i]]))
}
print(qf(0.95,df1=3,df2=n_1-4))


## Tests d'autocorrélation
cat("Test de Durbin Watson\n")
for (i in 2:length(model_list)){
	print(durbinWatsonTest(model_list[[i]]))
}

## Stabilité
# ANACOVA
cat("Test d'Anacova\n")
SCR_l <- list()
for (i in 1:length(model_list)){
	SCR <- sum(resid(model_list[[i]])^2)
	SCR_l[[i]] <- SCR
}
print(SCR_l)

SCR_a <- SCR_l[[2]] + SCR_l[[3]]
F_stat <- ((SCR_l[[1]] - SCR_a)/SCR_a) * (nobs(model_list[[1]]) - 2*4)/4
cat("SCR_a = ", SCR_a, "\n")
cat("F_stat = ", F_stat, "F_lu = ", qf(0.95,df1=4,df2=(nobs(model_list[[1]]) - 2*4)), "\n\n")

# Comparaison coef regression
#print(summary(model_0)$coef[,2])
cat("Test de comparaison des coefficients de regression\n")
for (i in 1:length(model_0$coef)){
	tc <- (model_1$coef[[i]] - model_2$coef[[i]])/sqrt((summary(model_1)$coef[i,2])^2 + (summary(model_2)$coef[i,2])^2)
	cat(paste0("tc",i,"="), tc, "\n")
}
cat("\n")

# Comparaison coef correlation
cat("Test de comparaison des coefficients de correlation\n")
z_l <- list()

for (i in 3:5){
    r_1 <- cor(data_1[,2],data_1[,i])
    r_2 <- cor(data_2[,2],data_2[,i])
    
    z_1 <- 0.5 * log((1 + r_1)/(1 - r_1))
    z_2 <- 0.5 * log((1 + r_2)/(1 - r_2))
    print(r_1)
    print(r_2) 
    print(z_1)
    print(z_2) 
    tc <- (z_1 - z_2)/sqrt((1/(n_1 - 3)) + (1/(n_2 - 3))) 
    cat(paste0("tc_",names(data[2]),names(data[i]),"="),tc, "\n")
    }
 
cat("\n")

## Colinearite
cor_matrice <- cor(data[3:5])

# Test sur les valeurs propres
cat("Matrice des coefficients de correlations (Rtilde)\n")
cor_matrice
cat("Valeurs propres de la matrice des r\n")
eigen(cor_matrice,symmetric = TRUE, only.values = TRUE)

# Test Haitovsky
cat("Test de Haitovsky\nDet(Rtilde) = ",det(cor_matrice),"\n")
k <- 4
H <- (1 + ((2*k+5))/6 - n) *log( 1- det(cor_matrice))
cat("H = ", H, paste0("CHI2(",k,") = ", qchisq(0.95,df=(k*(k-1))/2)),"\n\n")


# Test VIF
cat("Test de VIF\n")
vif(model_0)

X11()
ggpairs(data[2:5])
X11()
ggpairs(data_1[2:5])
X11()
ggpairs(data_2[2:5])
while (!is.null(dev.list())) Sys.sleep(1)

