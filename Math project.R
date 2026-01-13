## Write each x into the vector form, let y1 = X6, y2 = X7
## Total cost = 120 * (x1 + x2 + x3) + 150* (x4 + x5 + y1) + 180 * y2 

f = function(x) 120 * (x[1] + x[2] + x[3]) + 150 *(x[4] + x[5] + x[6]) + 180 * x[7]
inequality_constraint = function(x){
  g = 0
  g [1] = 6 * x[1] - 32
  g [2] = 6 * x[2] - 68
  g [3] = 6 *(x[1] + x[3]) - 56
  g [4] = 6 *(x[2] + x[4]) - 76
  g [5] = 6 *(x[3] + x[5] + x[6]) - 64
  g [6] = 6 *(x[4] + x[6] + x[7]) - 28
  g [7] = 6 *(x[5] + x[7]) - 8
  g [8] = x[1]
  g [9] = x[2]
  g [10] = x[3]
  g [11] = x[4]
  g [12] = x[5]
  g [13] = x[6]
  g [14] = x[7]
  ## for the "last 8 g[X]" refers to the last contraint: x1...y1 > 0 
  return (g)

  }
#assume the starting x-value is 12 
p0 = rep(12,7)
p0
answer = constrOptim.nl(p0, f, hin = inequality_constraint)
answer$value
##this is approximation number
answerApprox = round(answer$par)
answerApprox

## Spanish speakers 

f = function(x) 120 * (x[1] + x[2] + x[3]) + 150 *(x[4] + x[5])
inequality_constraint2 = function(x){
  g = 0
  g [1] = 6 * x[1] - 8
  g [2] = 6 * x[2] - 17
  g [3] = 6 *(x[1] + x[3]) - 14
  g [4] = 6 *(x[2] + x[4]) - 19
  g [5] = 6 *(x[3] + x[5]) - 16
  g [6] = 6 *(x[4]) - 7
  g [7] = 6 *(x[5]) - 2
  g [8] = x[1]
  g [9] = x[2]
  g [10] = x[3]
  g [11] = x[4]
  g [12] = x[5]

  
  return (g)
  
}

p0 <- rep(3, 5) # initial guess
result <- constrOptim.nl(p0, f, hin = inequality_constraint2)

result$value       # minimized cost
round(result$par)  # optimal staffing integers
