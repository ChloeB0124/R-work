##Task 1
#Part b: Formulate a Linear Programming (LP) model
library(lpSolve)
#Setting the coefficients of the decision variables -> C
C <- c(5, 8)

#Creating the constraint matrix
A <- matrix(c(-3,2,
              3,-1,
              -1,2,
              1,0,
              0,1,
              1,1),nrow=6, byrow = TRUE)

#Right hand size for the constraints
B <- c(rep(0,5),60)

#Direction of the constraints
constraints_direction <- c("<=",">=",">=", ">=",">=",">=")

#Finding the optimal solution
optimum <- lp(direction="min",
              objective.in= C,
              const.mat= A,
              const.dir = constraints_direction,
              const.rhs= B,
              all.int=T)

#Printing the status: 0 = success, 2 = no feasible solution
print(paste("Status:",optimum$status, sep=""))

#Printing the optimum value for x_1, x_2
best_sol <- optimum$solution
names(best_sol) <- c("x_1","x_2")
print(best_sol)

#Checking the value of objective function at optimal point
print(paste("Total Cost:", optimum$objval, sep=""))

#Part c: Graphical method
#Setting up vertices for shading with polygon
x.vert=c(0,24,40)
y.vert=c(0,36,20)

plot(1,xlim=c(0,60),ylim=c(0,60),
     xlab="Number of A in the composition",
     ylab="Number of B in the composition",
     lty=2,lwd=1.5, main="Graphical Solution for linear optimization problem")

lines(c(0,60), c(60,0), lwd=2, col="black")
abline(v=40, lwd=2, col="blue")
polygon(x.vert,y.vert,col=rgb(0.5,1,0,0.5))

#Present the optimal point and feasible region
points(40,20,pch=19)
text(40,20,"optimal point (40,20)")
text(24,36,"(24,36)")
text(20,20,"Feasible region")

#Part d
A.cost <- 5
while (TRUE) {
  A.cost <- A.cost + 1
  objective.in <- c(A.cost, 8)
  const.mat <- matrix(c(-3,2,
                        3,-1,
                        -1,2,
                        1,0,
                        0,1,
                        1,1),nrow=6, byrow = TRUE)
  const.rhs <- c(rep(0,5),60)
  constraints <- c("<=",">=",">=", ">=",">=",">=")
  solution <- lp(direction="min",
                 objective.in = C,
                 const.mat = A,
                 const.dir = constraints_direction,
                 const.rhs = B,
                 all.int = T)
  if (sum(solution$solution==c(40, 20))==2) {
    next
  } else {
    break
  }
}
cat("",
    "5 - ",
    A.cost - 1,
    sep="$")


##Task 2
#Setting the coefficients of the decision variables -> C
C <- c(19, 15, 17.5)

#Creating the constraint matrix
A <- rbind(diag(3),diag(3))

#Right hand size for the constraints
B <- c(4200, 3200, 3500, 0, 0, 0)

#Direction of the constraints
contraints_direction <- c(rep("<=",3),rep(">=",3))

#Finding the optimal solution
optimum <- lp(direction="max",
              objective.in = C,
              const.mat = A,
              const.dir = contraints_direction,
              const.rhs = B,
              all.int =T)

#Printing the status: 0 = success, 2 = no feasible solution
print(paste("Status: ", optimum$status, sep=""))

#Printing the optimum value for x_1, x_2, x_3
best_sol <- optimum$solution
names(best_sol) <- c("x_1","x_2","x_3")
print(best_sol)

#Checking the value of objective function at optimal point
print(paste("Total profit: ", optimum$objval, sep=""))

##Task 3
#Part b: formulate the payoff matrix
payoff.matrix <- data.frame(
  "1" = c(1,-1,-1,-1,-1),
  "2" = c(1,1,-1,-1,-1),
  "3" = c(1,1,1,-1,-1),
  "4" = c(1,1,1,1,-1),
  "5" = c(-1,-1,-1,-1,1)
)
colnames(payoff.matrix) <- 1:5
row.names(payoff.matrix) <- 1:5
cat("Number of posssible combinations of bids by Company 1 and Company 2\n")
payoff.matrix

#Part e & f: Build and solve LP model for Company 1 
#Setting the coefficients of the decision variables -> c
C <- c(1,1,1,1,1)

#Creating the constraint matrix
A <- matrix(rbind(diag(5),as.matrix(payoff.matrix)),nrow = 10)

#Right hand size for the constraints
B <- c(rep(0,5), rep (1,5))

#Direction of the constraints
constraints_direction <- c(rep(">=",5), rep("<=",5))

#Finding the optimal solution
optimum <- lp(direction="max",
              objective.in = C,
              const.mat = A,
              const.dir = constraints_direction,
              const.rhs = B,
              all.int = T)

#Printing the status: 0 = success, 2 = no feasible solution
print(paste("Status: ", optimum$status, sep=""))

#Displaying the optimum values for x_1, x_2, x_3, x_4, x_5
best_sol <- optimum$solution
names(best_sol) <- c("x_1","x_2","x_3","x_4","x_5")
print(best_sol)

#Checking the values of the objective function at optimal point
print(paste("Maximum Objective: ", optimum$objval, sep=""))

