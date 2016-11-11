##SIR differential equation model

if (!require("deSolve")) install.packages("deSolve")
library(deSolve)

##SIR function
sir <- function(time, state, parms) {
    dS <- -parms$beta * state[1] * state[2]
    dI <-  parms$beta * state[1] * state[2] - parms$gamma * state[2]
    dR <-  parms$gamma * state[2]
    return(list(c(dS, dI, dR)))
}


SIRModel <- function(pop.size=100, initNbInfect=1, beta=.05, gamma=.2, times=seq(1,10,0.01), plotit=TRUE, legend.x=6, legend.y=90){
solution <- ode(y=c(S=pop.size-initNbInfect, I= initNbInfect, R=0), times= times, func=sir, parms=list(beta=beta, gamma=gamma))
if(plotit){
	par(mar=c(5,5,1,1));matplot(solution[,1],solution[,-1], type="l", lty=1, col=2:4, bty="l", xlab="time", ylab="Number of individuals")
    legend(legend.x, legend.y, c("Susceptible", "Infected", "Recovered"), pch = 1, col = 2:4, bty = "n")
    }
return(solution)
}


ans <- SIRModel(pop.size=1, initNbInfect=.01, beta=1/6, gamma=1/6, times=seq(1,100,0.01), plotit=TRUE, legend.x=80, legend.y=95)

