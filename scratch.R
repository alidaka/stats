simulate_bo3 = function(threshold) {
    threshold = 1-threshold
    x = runif(1, 0, 1)
    if (x > threshold) {
        x = runif(1, 0, 1)
        if (x > threshold) 1
        else {
            x = runif(1, 0, 1)
            if (x > threshold) 1
            else 0
        }
    }
    else {
        x = runif(1, 0, 1)
        if (x > threshold) {
            x = runif(1, 0, 1)
            if (x > threshold) 1
            else 0
        }
        else 0
    }
}

calculate_bo3 = function(p) {
    p^2 * (3 - 2*p)
}

simulate_bo3(.5)
calculate_bo3(.6)
mean(replicate(100000, simulate_bo3(.4)))
help(mapply)

m = matrix(1:16, 4, 4)
r = 1:4
c = 1:4
cbind(r, c)

fun3 <- function(x, y) { z <- cbind(x, y); z[,1] + z[,2]}
outer(seq(1,5,length=5),seq(.1,1,length=5),fun3)

m = matrix(c(.1,.2,.3,.4), 2, 2)
mapply(function(x, i, j) x+i+j, m, row(m), col(m))

