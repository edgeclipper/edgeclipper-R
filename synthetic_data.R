# 9 variables with 500 observations
N <- 500
set.seed(12345)
A <- rnorm(N, mean = 0, sd = 1)
B <- rnorm(N, mean = 10, sd = 5)
C <- rnorm(N, mean = 0, sd = 10)
D <- A^3 + rnorm(N, mean = 0, sd = 0.1)
E <- ifelse(A + 10 >= B, A + rnorm(N, mean = 0, sd = 0.1), B/10 + rnorm(N, mean = 0, sd = 0.1))
EF <- (B - C) / (B + 10) + rnorm(N, mean = 0, sd = 0.1)
G <- ifelse(A + 10 >= B, A + sin(C) + rnorm(N, mean = 0, sd = 0.1), B  / 10 + sin(C) + rnorm(N, mean = 0, sd = 0.1))
H <- log(exp(A) + exp(EF)) + rnorm(N, mean = 0, sd = 0.1)
I <- (D + H) * (EF / 2) + rnorm(N, mean = 0, sd = 0.05)

x <- t(cbind(A, B, C, D, E, EF, G, H, I)) 

# Have to reformat for desired output file, this is just an example 

write(x, file = "input.data.txt", ncolumns = 9, sep = "    ")


