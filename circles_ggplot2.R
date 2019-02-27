
library(ggplot2)
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

p1 <- ggplot(
    data = d1,
    mapping = aes(
        fill = prime, 
        x = "",
        y = N)) +
    geom_bar(color = "white", width = 1,
        stat = "identity") +
    coord_polar(theta = "y", direction = -1) +
    facet_wrap(~ind, ncol = nn) +
    scale_fill_manual(values = pal, na.value = "white") +
    theme_bw() +
    theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank(),
        strip.text  = element_blank(),
        strip.background = element_blank()
    ) + 
    guides(fill = FALSE)

png("primes_display1.png", height = 600, width = 600, res = 200)
print(p1)
dev.off()

library(grid)
library(gridExtra)
primes <- as.integer(levels(d1$prime))
nprimes <- length(primes)

for (ii in seq_len(nprimes)) {
    is_ith_prime <- apply(X = prime_factors, MARGIN = 1,
        FUN = function(x, ii) { any(primes[ii] %in% x) }, ii = ii)
    # these indices are not present in this layer
    # TODO instead build up
    rm_pos <- cbind(
        x = rep(seq_len(nn), each = nn)[!is_ith_prime], 
        y = rep(seq_len(nn), times = nn)[!is_ith_prime])
    # black & white
    p2 <- ggplot(
        data = d1,
        mapping = aes(
            fill = prime, 
            x = "",
            y = N)) +
        geom_bar(color = "black", fill = "white", width = 2,
            stat = "identity") +
        coord_polar(theta = "y", direction = -1) +
        facet_wrap(~ind, ncol = nn, drop = FALSE) +
        scale_fill_manual(values = pal, na.value = "white") +
        theme_bw() +
        theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.border = element_blank(),
            strip.text  = element_blank(),
            strip.background = element_blank()
        ) + 
        guides(fill = FALSE)
    g2 <- ggplotGrob(p2)
    # TODO remove unneeded elements from needed panels
    
    # remove unneeded panels
    rm_grobs <- g2$layout$name %in% paste(rep(c("panel", "strip-t", 
        "axis-t", "axis-b", "axis-l", "axis-r"), each = nrow(rm_pos)), 
        apply(
            X = rm_pos, MARGIN = 1, 
            FUN = function(x) paste0(x[1], "-", x[2])), 
        sep = "-")
    # remove
    g2$grobs[rm_grobs] <- NULL
    g2$layout <- g2$layout[!rm_grobs, , drop = FALSE]
    # draw
    grid.draw(g2)
}
