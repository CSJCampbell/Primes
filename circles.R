
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
    c( 2,  2,  2,  2),
    c(17, NA, NA, NA),
    c( 2,  3,  3, NA), 
    c(19, NA, NA, NA), 
    c( 2,  2,  5, NA), 
    c( 3,  7, NA, NA), 
    c( 2, 11, NA, NA), 
    c(23, NA, NA, NA),
    c( 2,  2,  2,  3)),
    ncol = nn, byrow = TRUE)

apply(prime_factors, 1, prod, na.rm = TRUE)
pal <- c(
    "lightblue", "#e6c85a", 
    "darkblue", "#f0463c", 
    "pink", "lightgreen", 
    "purple", "green", 
    "orange")

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
    nsteps = 100, offset = 0.03, ...) {
    rs <- seq(
        from = pi + thetastart, 
        to = pi + theta + thetastart, 
        len = nsteps)
    if (offset < 0 | offset > 1) { 
        stop("offset must be in range 0,1") 
    }
    # multiple of 2 * pi, i.e. full circle
    if (round(theta %% (2 * pi), digits = 7) == 0) {
        xc <- x + (1 + offset * 0.56) * r * cos(rs)
        yc <- y + (1 + offset * 0.56) * r * sin(rs)
    } else {
        mid_theta <- pi + thetastart + theta / 2
        #rs <- rs + mid_theta
        xc <- x + r * cos(rs)
        yc <- y + r * sin(rs)
        #  /|
        # /_| y = offset * sin(mid_theta)
        #     x = offset * cos(mid_theta)
        xc <- c(x, xc, x) + offset * cos(mid_theta)
        yc <- c(y, yc, y) + offset * sin(mid_theta)
    }
    lines(xc, yc, ...)
}

# allow non-square plot
nr <- nrow(prime_factors) / nn

for (ii in seq_len(nprimes)) {
    # find prime
    is_ith_prime <- apply(
        X = prime_factors, MARGIN = 1,
        FUN = function(x, ii) { 
            any(primes[seq_len(ii)] %in% x) }, ii = ii)
    # these indices are not present in this layer
    # TODO instead build up
    add_pos <- cbind(
        x = rep(seq_len(nn), times = nr)[is_ith_prime], 
        y = rep(seq_len(nr), each = nn)[is_ith_prime])
    
    svg(paste0("output/prime_", primes[ii], ".svg"), 
        width = 9, height = 12, antialias = "none")
    par(mar = c(0, 0, 0, 0))
    plot(x = seq_len(nn), y = seq_len(nn), type = "n", xlab = "", ylab = "", axes = FALSE, 
        xlim = c(0, nn + 1), ylim = c(nr + 1, 0), asp = 1)
    
    # how many segments?
    for (jj in seq_len(nrow(add_pos))) {
        primes_jj <- prime_factors[which(is_ith_prime)[jj], ]
        n_vals_jj <- sum(!is.na(primes_jj))
        # draw segments
        for (kk in seq_len(n_vals_jj)) {
            if (primes_jj[kk] %in% primes[seq_len(ii)]) {
                segment(x = add_pos[jj, 1], y = add_pos[jj, 2], 
                    r = 0.45, 
                    theta = 2 * pi / n_vals_jj, 
                    thetastart = (kk - 1) * 2 * pi / n_vals_jj)
            }
        }
    }
    dev.off()
}
