#Let's first generate some data in 2 dimensions, and make them a little separated. 
#Setting random seed, you make a matrix x, normally distributed with 20 observations in 2 classes on 2 variables. 
#Then make y variable, which is going to be either -1 or 1, with 10 in each class. 
#For y = 1, you move the means from 0 to 1 in each of the coordinates.
#Finally, you can plot the data and color code the points according to their response. 
#The plotting character 19 gives you nice big visible dots coded blue or red according to whether the response is 1 or -1.

set.seed(10111)
x = matrix(rnorm(40), 20, 2)
y = rep(c(-1, 1), c(10, 10))
x[y == 1,] = x[y == 1,] + 1
plot(x, col = y + 3, pch = 19)


library(e1071)


#Make a dataframe of the data, turning y into a factor variable. 
#Make a call to svm on this dataframe, using y as the response variable and other variables as the predictors. 
#The dataframe will have unpacked the matrix x into 2 columns named x1 and x2. 
#SVM that the kernel is linear, the tune-in parameter cost is 10, and scale equals false. 
#In this example, no variables are standardize
#Printing the svmfit gives its summary

dat = data.frame(x, y = as.factor(y))
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)

 
#See that the number of support vectors is 6 - they are the points that are close to the boundary 
#or on the wrong side of the boundary.

plot(svmfit, dat)


#Make your own plot. 
#To do is to create a grid of values or a lattice of values for x1 and x2 that covers the whole domain on a fairly fine lattice. 
#Next, make a function called make.grid. 
#It takes in your data matrix x, as well as an argument n which is the number of points in each direction. 
#Now make a 75 x 75 grid.

#Within this function, apply function to get the range of each of the variables in x. 
#Then for both x1 and x2, use the seq function to go from the lowest value to the upper value to make a grid of length n. 
#Now, we have x1 and x2, each with length 75 uniformly-spaced values on each of the coordinates. 
#Finally, use the function expand.grid, which takes x1 and x2 and makes the lattice.

make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}

#Apply the make.grid function on x. Let's take a look at the first few values of the lattice from 1 to 10.
xgrid = make.grid(x)
xgrid[1:10,]



#Now we see, the grid goes through the 1st coordinate first, holding the 2nd coordinate fixed.

#Having made the lattice, we are going to make a prediction at each point in the lattice. 
#With the new data xgrid, you use predict and call the response ygrid. 
#Plot and color code the points according to the classification so that the decision boundary is clear. 
#Put the original points on this plot using the points function.

#svmfit has a component called index that tells which are the support points. 
#Plotting support points using the same function again.

ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)



#See in the plot, the points in the boxes are close to the decision boundary and are instrumental in determining that boundary.

#Unfortunately, the svm function is not too friendly, in that you have to do some work to get back the linear coefficients. 
#The reason is probably that this only makes sense for linear kernels, and the function is more general. 
#So use a formula to extract the coefficients more efficiently. 
#You extract beta and beta0, which are the linear coefficients.

beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0 = svmfit$rho


#Replot the points on the grid, then put the points back in (including the support vector points). 
#Use the coefficients to draw the decision boundary using a simple equation of the form:

#From that equation, you have to figure out a slope and an intercept for the decision boundary. 
#Use the function abline with those 2 arguments. 
#The subsequent 2 abline function represent the upper margin and the lower margin of the decision boundary, respectively.

plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)


#NON LINEAR

load(file = "C:/Users/ADMIN/Downloads/ESL.mixture.rda")
names(ESL.mixture)

rm(x, y)
attach(ESL.mixture)

plot(x, col = y + 1)

dat = data.frame(y = factor(y), x)
fit = svm(factor(y) ~ ., data = dat, scale = FALSE, kernel = "radial", cost = 5)

xgrid = expand.grid(X1 = px1, X2 = px2)
ygrid = predict(fit, xgrid)


plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(x, col = y + 1, pch = 19)

#Predicting Decision Boundries
func = predict(fit, xgrid, decision.values = TRUE)
func = attributes(func)$decision

xgrid = expand.grid(X1 = px1, X2 = px2)
ygrid = predict(fit, xgrid)
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(x, col = y + 1, pch = 19)

contour(px1, px2, matrix(func, 69, 99), level = 0, add = TRUE)
contour(px1, px2, matrix(func, 69, 99), level = 0.5, add = TRUE, col = "blue", lwd = 2)

