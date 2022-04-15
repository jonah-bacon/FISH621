#==================================================================================================
#Project Name: FISH 621 Estimation of Fish Abundance - Lab 9
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: 1.21.2022
#
#Purpose: To explore analysis of CWT data, matrix algebra operations in R, 
#           and Darroch estimation procedures for stratified mark-recapture
#             experiments
#
#
#
#==================================================================================================

# Exercise 1 - CWT Data =======================================================
# In lecture we discussed how CWT data can be used to estimate the contribution
#   of hatchery fish to total catch in a fishery.

# To begin, we can think about a simple scenario where:

# 50% of fish released from the hatchery are implanted with CWT
P <- 0.5
P

# The total catch this year is 1,000 fish
N <- 1000
N

# A sample of 200 fish is collected from the catch and inspected for CWT
s <- 200
s

# Of this sample size of s=200, 8 fish are found to have CWT
x <- 8
x

# From these data we can estimate the total catch of hatchery fish
C <- x * (N / (s*P))
C

# And the percentage contribution of hatchery fish to the catch
q <- C/N
q

# We can also calculate and estimate of the uncertainty in our estimate
#   of hatchery catch

C.var <- x*(N/(s*P))^2 * (1 - x + ( (s-1)/(N-1) ) * ( (x*N/s) -P ))
C.var

# From this we can calculate approximate confidence intervals
# First, we can calculate the standard effor for our estimate of hatchery catch
C.se <- sqrt(C.var)
C.se

# Approximate 80% CI
C.80.lb <- C - 1.282*C.se
C.80.ub <- C + 1.282*C.se

C
paste("80% CI for estimate of catchery catch:", 
        round(C.80.lb,1), "-", 
        round(C.80.ub,1))

# Approximate 95% CI
C.95.lb <- C - 1.96*C.se
C.95.ub <- C + 1.96*C.se

C
paste("95% CI for estimate of catchery catch:", 
      round(C.95.lb,1), "-", 
      round(C.95.ub,1))


# Exercise 2 - Matrix Algebra in R =============================================

# To explore matrix algebra operations in R, lets begin by creating a matrix
#   with two rows and three columns

mat.1 <- matrix(data=c("a","b","c",
                       "d","e","f"), nrow=2, ncol=3, byrow=TRUE)

mat.1


# The easiest operation to perform is the matrix transpose
#   to do so we can use the t() funciton in R

?t

t(mat.1)


# For this operation R doesn't care if we are considering a matrix
#   of strings or a matrix of numeric values



mat.2 <- matrix(data=c(10,20,30,
                       40,50,60), nrow=2, ncol=3, byrow=TRUE)


mat.2

# What types of values are contained within these two matrices
typeof(mat.1)
typeof(mat.2)

t(mat.1)
t(mat.2)


# The matrix transposition works equivalently indpendent of the dimensions
#   of our matrix, here a 3x3 matrix

mat.3 <- matrix(data=c("a","b","c",
                       "d","e","f",
                       "g","h","i"), nrow=3, ncol=3, byrow=TRUE)

mat.3

t(mat.3)

# Matrix operations ========================

# Do develop our familiarity with matrix algebra, we can explore some basic 
#   operations.

# Many of our matrix operations will use a standard notation the % % surrounding
#   the operator. This is how R knows this is a matrix, rather than an element-wise
#     operation which is the default.

# Let's begin by multiplying a scalar times a matrix

# If we multiply a matrix by a scalar then the result is a matrix
#   of the same dimensions as the original matrix

# Scalar X Matrix = Matrix

mat.2

scalar <- 2

mat.2.scalar <- scalar * mat.2

mat.2.scalar

# Let's compare the dimensions of the original matrix (mat.2) and the new
#   matrix (mat.2.scalar)

dim(mat.2)
dim(mat.2.scalar)

# We can see that the dimensions of the new matrix are the same as the original
#   matrix. 



# Next, let's explore multiplying a matrix by a vector.
#   In order for this operation to work, the vector will need to be of length
#     equal to the number of columns in the matrix.

# Matrix X Vector = Vector

# The result will be a vector of length equal to the number of rows
#   in the matrix

# We will start with our 2x3 matrix
mat.2

# And we can create a vector with 3 elements
vect <- as.vector(c(1:3))
vect

# If we use our normal multiplication operator *, we will get an INCORRECT ANSWER

# This will recycle elements of the vector during the multiplication.
mat.2
mat.2 * vect

# This is 
10*1 # [1,1]
40*2 # [1,2]
20*3 # [2,1]
50*1 # [2,2]
30*2 # [3,1]
60*3 # [3,2]

# ERROR: This isn't the matrix operation we want!!!!!! 

# To correctly implement the matrix algebra operation we need to use 
#   the matrix algebra operator: %*%
mat.2
mat.2 %*% vect

# The first element should be 
(10*1) + (20*2) + (30*3)
(40*1) + (50*2) + (60*3)

# Or equivalently...

mat.2[1,1]*vect[1] + mat.2[1,2]*vect[2] + mat.2[1,3]*vect[3]
mat.2[2,1]*vect[1] + mat.2[2,2]*vect[2] + mat.2[2,3]*vect[3]

# OK, this is correct!!!

# What happens if we attempt to our 2x3 matrix (mat.2) by a shorter vector
#   of length 2?

vect.short <- vect[1:2]
vect.short

mat.2 %*% vect.short

# ERROR: "non-comformable" arguments means that the matrix algebra operation
#   expects a the vector to be of length equal to the number of columns in the 
#     matrix
dim(mat.2)
length(vect)
length(vect.short)

# However if we transpose our mat.2 matrix, it will become a 3x2 matrix

mat.2.t <- t(mat.2)

mat.2.t
dim(mat.2.t)

# This 3x2 matrix can now be multiplied by our short vector of length 2
vect.short

mat.2.t %*% vect.short

dim(mat.2.t %*% vect.short)

# There are three elements in the resulting vector, equal to the number of rows
#  in the original matrix
dim(mat.2.t)

# So the matrix operation being performed is:
(10*1) + (40*2)
(20*1) + (50*2)
(30*1) + (60*2)

mat.2.t %*% vect.short

# Or equivalently...
mat.2.t[1,1]*vect.short[1] + mat.2.t[1,2]*vect.short[2]
mat.2.t[2,1]*vect.short[1] + mat.2.t[2,2]*vect.short[2]
mat.2.t[3,1]*vect.short[1] + mat.2.t[3,2]*vect.short[2]

# For another example of multiplying a matrix by a vector, let's create a 3x3 matrix
mat.other <- matrix(data=c(1:9), nrow=3, byrow=TRUE)
mat.other


# We can multiply this 3x3 matrix by our vector of length 3 (vect)
vect

mat.other %*% vect

# The result is a vector of length 3, or equivalently a vector with three rows
#   and a single column
dim(mat.other %*% vect)


# The operations being conducted are (for each element):
(1*1) + (2*2) + (3*3)
(4*1) + (5*2) + (6*3)
(7*1) + (8*2) + (9*3)

# Or equivalently
mat.other[1,1]*vect[1] + mat.other[1,2]*vect[2] + mat.other[1,3]*vect[3]
mat.other[2,1]*vect[1] + mat.other[2,2]*vect[2] + mat.other[2,3]*vect[3]
mat.other[3,1]*vect[1] + mat.other[3,2]*vect[2] + mat.other[3,3]*vect[3]


# Matrix Determinant ===================

# A matrix determinant  reduces a square matrix to a single value

# To explore this let's create a square matrix

mat.4 <- matrix(data=c(1,2,
                       3,4), 
                nrow=2, ncol=2, byrow=TRUE)
mat.4

# We can use the det() function in R to calculate the determinant of a matrix

?det

det(mat.4)

# Note: This is equivalent to 
mat.4

1*4 - 3*2

# The matrix determinant will work on a matrix of any size, so long as it is
#   square. However, the math becomes a bit more complicated.

mat.5 <- matrix(data=c(2,-3,1,
                       2,0,-1,
                       1,4,5), 
                nrow=3, ncol=3, byrow=TRUE)
mat.5


# To calculate the determinant ourselves, let's create the appropriate sub
#  matrices

mat.5.a <- matrix(c(0,-1,
                    4,5),
                  nrow=2, ncol=2, byrow=TRUE)
mat.5.a

mat.5.b <- matrix(c(2,-1,
                    1,5),
                  nrow=2, ncol=2, byrow=TRUE)
mat.5.b  

mat.5.c <- matrix(c(2,0,
                    1,4),
                  nrow=2, ncol=2, byrow=TRUE)
mat.5.c

# We can calculate the determinant of a 3x3 matrix as 
2*det(mat.5.a) - (-3)*det(mat.5.b) + 1*det(mat.5.c)



# OR equivalently ...
mat.5

2*(0*5 - 4*-1) - (-3)*(2*5 - 1*-1) + 1*(2*4 - 1*0)

# But, as before we can always use the simple det() function to save us
#   some time.
det(mat.5)

# Challenge A: Matrix Determinant ==============================================

mat.cha <- matrix(data=c(1:9), 
                nrow=3, ncol=3, byrow=TRUE)
mat.cha

mat.cha.2 <- matrix(data=c(1,4,2,
                           2,6,1,
                           10,5,3), 
                  nrow=3, ncol=3, byrow=TRUE)
mat.cha.2

# Above is are two 3x3 matrices, please calculate the matrix determinant for both matrices in two ways:
# 1) Use simple addition and multiplication of elements of the matrix
#      to calculate the determinant: x*(a*b-c*d) - ....
# 2) Confirm you have done this correctly by using the det() function

# SOLUTION =====================================================================

mat.cha
1*(5*9 - 8*6) - 2*(4*9 - 7*6) + 3*(4*8 - 7*5)
det(mat.cha)

mat.cha.2
1*(6*3 - 5*1) - 4*(2*3 - 10*1) + 2*(2*5 - 10*6)
det(mat.cha.2)


# Matrix Inverse M^-1 ===========================

# In order to solve an equation like 4x=8 for x, we would multiply both sides of the
#   equation by the inverse of 4, as: (4^-1)*4x = (4^-1)*8
#     This would reduce to: 4/4x = 8/4, or x=2.

# We can do the same thing in matrix algebra by finding the inverse of a 
#   matrix and using it to solve for a vector.

# The inverse of a matrix multiplied by the original matrix should equal
#  an "identity matrix" which is a matrix containing 1's on the diagonal
#    and 0's for the off-diagonal elements

# If A is a 3x3 matrix and A.inv is its matrix inverse, if we multiply them
#   the result should be A %*% A.inv =

matrix(c(1,0,0,
         0,1,0,
         0,0,1), nrow=3, byrow=TRUE)

# Let's use the mat.5 matrix for this example:
mat.5

# We can calculate the inverse of this matrix with the solve() function
?base::solve
mat.5_inv <- base::solve(mat.5)
mat.5_inv

# mat.5_inv is the inverse of mat.5

# Prove that the solve() function worked:
#  Multiplying a matrix times its inverse, should result in an identity matrix
mat.5_inv %*% mat.5

# NOTE: THE OFF DIAGONALS ARE VERY CLOSE TO ZERO, THIS IS OK.

# Let's try another example

# We can create a new matrix A
A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), 
             nrow=3, byrow=TRUE)
A

# And calculate its inverse
AI <- solve(A)
AI

# Then multiply the inverse %*% the matrix
AI %*% A

# Or equivalently the matrix times its inverse
A %*% AI

# Exercise 3 - Darroch Method ==================================================

# In this exercise we will explore Darroch's method for estimating abundance
#   based on mark-recapture data.

# In this case we have three spatial strata (locations) in which we 
#   mark and release individuals at time t1, using a separate tag color for 
#     marking individuals in each stratum.
#   This allows us to know the stratum origin of each recaptured individual.
#     At time t2 then collect a sample of individuals
#       in each of these three strata and record:
#  1) The number of unmarked individuals in the sample from each strata
#  2) The number of recaptures, and their stratum of origin

# Data ================

# In this case we have three spatial strata
s <- 3

# The data we have from this mark-recapture experiment are:

# 1) The number of individuals marked by strata i (a_i)
a <- c(484, 695, 1172)
a

# Our total number of marked individuals released at across strata is:
A <- sum(a)

# The length of this vector should naturally be equal to s
length(a)==s

# The number of individuals marked and released in stratam #2 at time t1 is: 695
a[2]



# 2) The number of unmarked individuals observed in each strata (j) during the
#       recapture event at time t2 (u_j):
u <- c(847, 2664, 6441)
u

# Here again we should expect that the length of this vector is equal to the number
#   of strata
length(u)==s

# 3) The final piece of data we have in hand following this experiment
#      is the matrix m_ij of recaptures that originated in stratum i,
#         and were later recaptured in stratum j.

# This m_ij or matrix is a critical source of information as it informs both:
#   (a) our basic mark-recapture estimator, and (b) movement rates between strata
#      between t1 and t2.

m <- matrix(data=c(59, 24, 7, 
                   34, 79, 67, 
                   11, 81, 158),
              nrow=3, byrow=TRUE)

m

# The structure of this recapture matrix is m[i,j] :
#   i = rows = the stratum in which the marked individual originated (t1)
#   j = columns = the stratum in which the tagged individual was recaptured (t2)

# I like to think of this as m[from, to]

# So, the number of individuals that were marked in stratum 1, and recaptured
#   in stratum 2 is
m[1,2]

# Number that were marked in stratum 2 and were recaptured in stratum 2
m[2,2]

# Number that were marked in stratum 3 and were recaptured in stratum 1
m[3,1]

# Preliminary Calculations ==========================

# Based on the data in hand we can do some preliminary calculations 
#   for other quantities of interest that depend on the three data 
#     objects we have defined above. 

# We can calculate the total number of recaptures by stratum at time t2 
#   from our m[i,j] matrix.

# This would be the sum of our columns in our m matrix.
#   we can use our handy apply() function in R for any operation 
#     across elements of matrix or array object.

# NOTE: apply() is a powerful function, add it to your R toolkit if it is
#         not already, as well as it's cousin lapply() that operates on a list
#           object or tapply() that operates on a ragged array

?apply

# The arguments for our apply() call will be:
# 1) The object that we are summarizing
# 2) The dimensions of the array (or matrix) that are preserved
# 3) The function we will use for summarizing across dimensions
# 4 +) Additional arguments passed to the function in (3)

# m_j is the number of recaptures by recapture strata j (Column)
#  We will indicate the second dimensions =2 for the second argument of apply()
#    this indicates we will sum() across elements of each column
#    

m_j <- apply(m, 2, sum)

m_j

# We can use the apply function to calculate the total number of recaptures
#   from each each marking stratum (i)
m_i <- apply(m, 1, sum)

m_i

# Finally, we can calculate our sample size at time t2 for our recapture event
#   in each stratum, as the sum of unmarked individuals (u) and recaptures (m_j)
#     in each stratum

n <- m_j + u
n

# We will calculate several diagonal matrices over the course of our
#   calculations. Let's calculate the diagonal matrix based on (n) our 
#     sample size in each stratum

# To create a diagonal matrix from a vector, we use a combination of 
#   the matrix() constructor function and the diag() function
n

Dn <- matrix(diag(n), nrow=s)

Dn

# We can also construct an identity matrix of sXs dimensions
I <- matrix(diag( rep(1,s) ), nrow=s)
I

# Next we can create a diagonal matrix based on our number of unmarked fish
#   sampled in each stratum at the time of recapture (t2)

Du <- matrix(diag(u), nrow=s)
Du

# Finally, let's calculate a diagonal matrix with our marking numbers by stratum
Da <- matrix(diag(a), nrow=s)
Da

# Abundance Estimation =========================

# The first step is to calculate the inverse of our m matrix.
# We can use the solve function to do so...

?solve

m.inv <- solve(m)

m.inv


# To prove this worked correctly to ourselves we can multiply the m matrix
#   by its inverse, and see if we get an identity matrix
m %*% m.inv

# Close enough to an identity matrix.

# Next, we can multiply our a vector by the inverse of the m matrix (m.inv)
#   to estimate the r vector. 

r <- m.inv %*% as.vector(a)
r


# Recall r is equal to 1/p, where p is the CAPTURE PROBABILIY for our 
#   resampling event in each stratum j

p <- 1/r
p

# Note this is equivalent to
r^-1

# Next, we will need to calculate the inverse of our diagonal matrix
#   for our marking numbers by stratum
Da.inv <- solve(Da)

Da.inv

# Now we will create a diagonal matrix based on our vector r
#   the inverse of our capture probabilities
Dr <- matrix(diag(as.vector(r)), nrow=s, ncol=s)

Dr

# Now, we can calculate our estimated movement probabilities (theta)
#   as: theta = Da^-1 * M * Dr,
#    or theta = inverse of the diagonal matrix of marking numbers by strata,
#                 times our recapture matrix, times our diagonal matrix of 
#                   the inverse of detection probability

# First, let's calculate the inverse of our Da matrix

Da.inv <- solve(Da)
Da.inv

# Double check this worked
Da.inv %*% Da #GOOD!

theta <- Da.inv %*% m %*% Dr

theta

# Notice that some of the elements of theta are negative. That is OK in this 
#   context, although a little difficult to interpret. 
# Essentially, there is uncertainty in these estimates which can lead
#   to values below 0.

# You will recall that the row elements of our movement probabilities
#   must sum to 1, we can check this.
apply(theta, 1, sum)

# Now that we have estimates of our movement probability (theta), and 
#   we have estimate our sampling probability at recapture pj,
#     we can calculate psi (the trident) which is 
# The probability of moving and being captured during the resampling (t2) event is:


psi <- matrix(nrow=s, ncol=s)

psi[1,] <- theta[1,]*p  
psi[2,] <- theta[2,]*p  
psi[3,] <- theta[3,]*p  

psi  

# Inspecting this object we can interpret the elements

# The probability of starting in stratum 1 at t1, staying in stratum 1, and being
#   sampled during the recapture event (t2) in stratum 1
psi[1,1]

# The probability of starting in stratum 1 at t1, moving stratum 2, and being
#   sampled during the recapture event (t2) in stratum 2
psi[1,2]

# The probability of starting in stratum 3 at t1, moving stratum 2, and being
#   sampled during the recapture event (t2) in stratum 2
psi[3,2]

# The probability of startin in stratum 2 at t1, moving stratum 1, and being
#   sampled during the recapture event (t2) in stratum 1
psi[2,1]
  
# Now we can estimate our number of unmarked individuals by stratum at time t2
#   as the diagonal matrix of unmarked individuals observed during the 
#     t2 sampling event and our vector r which is the inverse of the estimated
#        catch probability in our sample at t2

U_hat_j <- Du %*% as.vector(r)
U_hat_j  

# By summing this vector we can calculate our estimate of tot total number of 
#   unmarked individuals at time t2
U_hat <- sum(U_hat_j)
U_hat

# Given that our total population size (N_hat) at either t1 or t2 is the unmarked
#   plus marked individuals, we can calculate our estimate of abundance
#     at time t2

D_mj <- matrix(diag(m_j), nrow=3)

N_hat_j <- U_hat_j + D_mj %*% as.vector(r)
N_hat_j

# Note this is equivalent to...
U_hat_j + m_j*as.vector(r)

# We can sum this vector to calculate the total abundance estimate at time
#   t2
N_hat <- sum(N_hat_j)
N_hat

# Checking totals for t2 against t1 =====================

# We will recall that since we assume our population (3 spatial strata) is 
#   closed, the total abundance (N) and total unmarked (U) and total tagged individuals
#     (A) should be the same in time periods t1 as they are at the time of 
#        recapture t2.

# We can check our estimates N_hat and U_hat in t2 against those in t1
#   N_hat_star, U_hat_star

# So, now lets calculate our estimates for the total and unmarked populations
#   at time t1 to check our work.

# The unmarked component of the population at time t1 in each stratum i
#   is (U_hat_star_i) is our estimate of the unmarked component of the
#     population at time t2 in each recapture stratum j multiplied by the inverse-transpose
#       of our movement probability matrix (theta)

# We can break this up into a couple of steps

# 1) The inverse of our transition probability matrix
theta.inv <- solve(theta)
theta.inv

# 2) The transpose of the inverse matrix
theta.inv.trans <- t(theta.inv)
theta.inv.trans

# Or equivalently...
t(solve(theta))

# NOTE: Whether we calculate the inverse then transpose the matrix, or we
#         transpose the matrix then calculate the inverse, the result is the same
solve(t(theta))

U_hat_star_i <- theta.inv.trans %*% U_hat_j
U_hat_star_i

# The total estimate of the unmarked component of the population at time t1
#   is the sum of estimates by stratum
U_hat_star <- sum(U_hat_star_i)
U_hat_star

# To calculate the total population at time t1 (release event), again to double-check
#   our estimates for time t2 (recapture event), we just need to add our number
#     tagged in each stratum to the estimated unmarked in each stratum at t1

# Remember a is the number marked and released at time t1 in each stratum
a

N_hat_star_i <- U_hat_star_i + a
N_hat_star_i

# Here again we need to sum this up to get our actual estimate (without negative fish)
N_hat_star <- sum(N_hat_star_i)
N_hat_star

# OK, lets compare our U and N estimates at time t1 (release event), to those from
#    at time t2 (recapture event). These should be equal, if we did everything correctly

U_hat_star # t1 (release event)
U_hat # t2 (recapture event)

N_hat_star # t1 (release event)
N_hat # t2 (recapture event)

# FANTASTIC !!!

# Exercise 4: Comparison of Darroch Estimates with Petersen Estimates ==========

# It is important to note that with this experimental design we might be 
#   tempted to ignore our strata, and by extension variability in capture probability
#     at time t2 and use our basic Petersen estimator.

# However, based on theory we should expect this estimate to be biased

# But, let's compare the simple estimate we get if we ignore stratification and movement

# Recall that our total number of marked individuals is (A) or the sum
#   of marked and released individuals in each stratum i
a

A

n1 <- A

# We can convert this into our standard Petersen notation 
#   n1 - number captured at time t1, marked and released
#   n2 - number captured at time t2, and inspected for marks
#   m2 - number of recaptures (marked individuals), in our sample at time t2

# Also recall that our total sample size in each stratum (j) at recapture is

n

# Putting this in our standard Petersen terms this is:
n2 <- sum(n)

# Finally, recall that the total number of recaptures is the sum of of our m[i,j]
#  matrix

# We can call this m2 for consistency with our Petersen parameters
m2 <- sum(m)

# Marks
n1
# Sample size at t2
n2
# Recaptures at t2
m2

# Ok, let's calculate our Petersen, Chapman and Bailey estimators

calc_pete <- function(n1, n2, m2) {
  out <- NULL
  out$N <- (n1*n2)/m2
  return(out)
}

calc_chap <- function(n1, n2, m2) {
  N <- ((n1+1)*(n2+1))/(m2+1) - 1
  var <- ((n1+1)*(n2+1)*(n1-m2)*(n2-m2)) / ((m2+1)^2 * (m2+2))
  cv <- sqrt(var)/N
  # Output list
  out <- NULL
  out$N <- N
  out$var <- var
  out$cv <- cv
  return(out)
}

calc_bailey <- function(n1, n2, m2) {
  N <- (n1*(n2+1)) / (m2+1)
  var <- (n1^2 * (n2+1) * (n2-m2)) / ((m2+1)^2 * (m2+2))
  cv <- sqrt(var)/N
  # Output list
  out <- NULL
  out$N <- N
  out$var <- var
  out$cv <- cv
  return(out)
}

# Comparison of estimators
# Darroch
N_hat
N_hat_star

# Petersen
calc_pete(n1=n1, n2=n2, m2=m2)$N

# Chapman
calc_chap(n1=n1, n2=n2, m2=m2)$N

# Bailey
calc_bailey(n1=n1, n2=n2, m2=m2)$N

# All of our basic mark-recapture estimates are in the ballpark, but are 
#   all BIASED LOW!

# This underscores the importance of considering our (spatial) stratification
#   in the context of a mark-recapture experiment such as this!



