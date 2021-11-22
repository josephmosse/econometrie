#TD4
library(car)
library(GGally)

data <- read.csv("Td4.csv")

data_1 <- data[which(data == "2000/01/01"):which(data == "2006/12/01"),]
data_2 <- data[which(data == "2013/01/01"):which(data == "2020/12/01"),]

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

## Tests d'autocorrélation

for (i in 2:length(model_list)){
	print(durbinWatsonTest(model_list[[i]]))
}

## Stabilité
# ANACOVA
SCR_l <- list()
for (i in 1:length(model_list)){
	SCR <- sum(resid(model_list[[i]])^2)
	SCR_l[[i]] <- SCR
}
print(SCR_l)

SCR_a <- SCR_l[[2]] + SCR_l[[3]]
F_stat <- ((SCR_l[[1]] - SCR_a)/SCR_a) * (nobs(model_list[[1]]) - 2*4)/4
cat("SCR_a = ", SCR_a, "\n")
cat("F_stat = ", F_stat, "F_lu = ", qf(0.95,df1=4,df2=(nobs(model_list[[1]]) - 2*4)), "\n")

# Comparaison coef regression
#print(summary(model_0)$coef[,2])

for (i in 1:length(model_0$coef)){
	tc <- (model_1$coef[[i]] - model_2$coef[[i]])/sqrt((summary(model_1)$coef[i,2])^2 + (summary(model_2)$coef[i,2])^2)
	cat(paste0("tc",i,"="), tc, "\n")
}

# Comparaison coef correlation
z_l <- list()
for (i in 3:4){
	for (j in i:4){
		r_1 <- cor(data_1[,i],data_1[,j])
		print(r_1)
}
	#r_1 <- cor(i,)
	#r_2 <- cor(i)

	#z_1 <- 0.5 * ln((1 + r_1)/(1 - r_1))
	#z_1 <- 0.5 * ln((1 + r_1)/(1 - r_1))
	
	#z_l[[i]] <- z_1 - z_2 
}

X11()
ggpairs(data_1[2:5])
while (!is.null(dev.list())) Sys.sleep(1)

