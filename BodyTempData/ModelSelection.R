# We first set up the matrix for the 3 parks
set.seed(1029)

n.groups = 3
n.sample = 10

n <- n.groups * n.sample  # Number of data points
x <- rep( 1:n.groups,rep (n.sample, n.groups)) # park indicator

# Parks: LACL = Lake Clark, KATM = Katmai, GLBA = Glacier Bay
pop <- factor(x, labels = c("LACL", "KATM", "GLBA"))

# Now the fake mass data
mmass   <- rnorm(n, mean = 3.5, sd = 1.2) 

# Now the fake Fucus data

fucus   <- rnorm(n, mean = 10, sd = 2)

# Making the x matrix
Xmat <- model.matrix(~pop*mmass + fucus)

print(Xmat, dig=2)

# Get the beta values for the model.
## Order: Intercept (LACL),KATM, GLBA, MMass, Fucus, 
##        Interaction(KATM:MMass), Interaction(GLBA:MMass) 
beta.vec <- c(12, -6, -3, 1, 0, .5, .7) # vector of hopefully realistic betas
lin.pred <- Xmat[,] %*% beta.vec        # linear predictor
eps <- rnorm(n = n, mean = 0, sd = 2)  # Some noise

# Generating the data
btl <- lin.pred + eps

# some plots
hist (btl, xlab = "byssus thread length (mm/10)" )

matplot (cbind(mmass[1:10], mmass[11:20], mmass[21:30]),
         cbind(btl[1:10],btl[11:20],btl[21:30]),
         ylim = c(0,max(btl)),ylab = "Byssus thread length (mm/10)",
         xlab = "Mussel mass (g)", col = c("red","green","blue"),
         pch = c("L", "K", "G", cex = 1.2, cex.lab = 1.5 )
)

# This is a function I found online to make a pairs plot with correlation coefficients in one of the panels

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y,method="spearman")
  p<- cor.test(x,y,method="spearman")
  t<- if(p$p.value<0.05) 2 else 1
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)    
  text(0.5, 0.5, txt, font = t, col = t)
}

pairs(cbind(fucus,mmass,pop),lower.panel=panel.smooth,pch=1, upper.panel=panel.cor)

require(AICcmodavg)

mass.mod          <- lm(btl ~ mmass)
fucus.mod        <- lm(btl ~ fucus)
pop.mod           <- lm(btl ~ pop)
mass.fucus.mod    <- lm(btl ~ mmass + fucus)
mass.pop.mod      <- lm(btl ~ mmass + pop)
fucus.pop.mod     <- lm(btl ~ fucus + pop)
mmass.pop.int.mod <- lm(btl ~ fucus * pop)
full.add.mod      <- lm(btl ~ mmass + fucus + pop)
full.int.mod     <- lm(btl ~ fucus + mmass*pop)

# We determined that the 3-way interaction didn't make any sense, nor did the interaction between fucus and pop

# Make a list of all the models

models<- list(mass.mod, fucus.mod, pop.mod, mass.fucus.mod, mass.pop.mod, fucus.pop.mod,
              mmass.pop.int.mod, full.add.mod, full.int.mod)

model.names <- c("mass.mod", "fucus.mod", "pop.mod", "mass.fucus.mod", "mass.pop.mod", "fucus.pop.mod",
                 "mmass.pop.int.mod", "full.add.mod", "full.int.mod")

# Make an aic table
aictab(cand.set = models, modnames = model.names)

require(lmtest)

lrtest(full.int.mod, full.add.mod)  

lrtest(full.int.mod, mass.pop.mod)  

lrtest(full.add.mod, mass.pop.mod)  

#airquality models
summary(airquality$Ozone)
a <- na.omit(airquality)

lm.solar <- lm(Ozone~Solar.R, data = a)
lm.wind <- lm(Ozone~Wind, data = a)
lm.temp <- lm(Ozone~Temp, data = a)
lm.month <- lm(Ozone~Month, data = a)
lm.day <- lm(Ozone~Day, data = a)
lm.solar.wind <- lm(Ozone~Solar.R + Wind, data = a)
lm.solar.temp <- lm(Ozone~Solar.R + Temp, data = a)
lm.temp.wind <- lm(Ozone~Temp + Wind, data = a)
ozone.temp.int.mod <- lm(Ozone ~ Wind * Temp, data = a)
full.add.mod      <- lm(Ozone ~ Wind + Temp + Solar.R, data = a)
full.int.mod1     <- lm(Ozone ~ Temp + Solar.R*Wind, data = a)
full.int.mod2     <- lm(Ozone ~ Solar.R + Temp*Wind, data = a)


model.list <- list(lm.solar, lm.wind, lm.temp, lm.solar.wind, lm.solar.temp, lm.temp.wind, ozone.temp.int.mod, full.add.mod, full.int.mod1, lm.month, lm.day, full.int.mod2)
model.names <- c("solar", "wind", "temp", "solar.wind", "solar.temp", "temp.wind", "ozone.temp.int", "full.add", "full.int1", "month", "day", "full.int2")

aictab(cand.set = model.list, modnames = model.names)

#of these models that we fed it, the lowest AIC value is the best model to use

summary(full.int.mod)

lrtest(full.int.mod2, full.add.mod)
lrtest(full.int.mod2, lm.solar)
lrtest(full.int.mod2, lm.temp)
lrtest(full.int.mod2, lm.wind)

summary(full.int.mod2)
