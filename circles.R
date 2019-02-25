
library(dplyr)
library(tidyr)

nn <- 4

prime_factors <- matrix(c(
    c(NA, NA, NA, NA), 
    c( 2, NA, NA, NA), 
    c( 3, NA, NA, NA), 
    c( 2,  2, NA, NA),
    c( 5, NA, NA, NA), 
    c( 2,  3, NA, NA), 
    c( 7, NA, NA, NA), 
    c( 2,  2,  2, NA), 
    c( 3,  3, NA, NA), 
    c( 2,  5, NA, NA), 
    c(11, NA, NA, NA), 
    c( 2,  2,  3, NA), 
    c(13, NA, NA, NA), 
    c( 2,  7, NA, NA), 
    c( 3,  5, NA, NA),
    c( 2,  2,  2, 2)), 
    nrow = nn^2, ncol = nn, byrow = TRUE)

apply(prime_factors, 1, prod, na.rm = TRUE)
pal <- c("lightblue", "lightyellow", "darkblue", "pink", "orange", "darkgreen")

d1 <- as.data.frame(prime_factors) %>% 
    mutate(ind = seq_len(n())) %>% 
    gather(key = id, value = prime, -ind) %>% 
    na.omit %>% 
    mutate(id = as.numeric(gsub("V", "", id)), 
        prime = factor(prime)) %>% 
    rbind(c(ind = 1, id = 1, prime = NA), .) %>%
    group_by(ind) %>% 
    mutate(N = min(1 / length(na.omit(prime)), 1, na.rm = TRUE))
head(d1)


primes <- as.integer(levels(d1$prime))
nprimes <- length(primes)


segment <- function(x, y, r, theta = pi / 2, thetastart = 0, 
    nsteps = 100, offset = 0.1, ...) {
    rs <- seq(
        from = pi + thetastart, 
        to = pi + theta + thetastart, 
        len = nsteps)
    if (offset < 0 | offset > 1) { 
        stop("offset must be in range 0,1") 
    }
    if (theta < 0) {
        stop("theta must be in range 0,2*pi")
    }
    if (theta > 2 * pi) { 
        theta <- theta > 2 * pi 
    }
    if (theta == 2 * pi) {
        xc <- x + (1 + offset / 2) * r * cos(rs)
        yc <- y + (1 + offset / 2) * r * sin(rs)
    } else {
        mid_theta <- thetastart + theta / 2
        rs <- rs + mid_theta
        xc <- x + r * cos(rs)
        yc <- y + r * sin(rs)
        xc <- c(x, xc, x) + offset
        yc <- c(y, yc, y) + offset
    }
    lines(xc, yc, ...)
}

par(mar = c(0, 0, 0, 0))
plot(x = 1:4, y = 1:4, type = "n", xlab = "", ylab = "", axes = FALSE, 
    xlim = c(0, nn + 1), ylim = c(nn + 1, 0), asp = 1)

for (ii in seq_len(nprimes)) {
    # find prime
    is_ith_prime <- apply(
        X = prime_factors, MARGIN = 1,
        FUN = function(x, ii) { 
            any(primes[ii] %in% x) }, ii = ii)
    # these indices are not present in this layer
    # TODO instead build up
    add_pos <- cbind(
        x = rep(seq_len(nn), each = nn)[is_ith_prime], 
        y = rep(seq_len(nn), times = nn)[is_ith_prime])
    # how many segments?
    for (jj in seq_len(nrow(add_pos))) {
        n_vals_jj <- sum(!is.na(prime_factors[which(is_ith_prime)[jj], ]))
        # draw segments
        for (kk in seq_len(n_vals_jj)) {
            segment(x = add_pos[jj, 1], y = add_pos[jj, 2], 
                r = 0.45, 
                theta = 2 * pi / n_vals_jj, 
                thetastart = (kk - 1) * 2 * pi / n_vals_jj)
        }
    }
}
