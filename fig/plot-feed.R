library(ggplot2)
library(patchwork)
library(showtext)

product_feed_protein <- readRDS("~/Downloads/sandro/liblex-feed-protein.rds")
product_feed_fibre   <- readRDS("~/Downloads/sandro/liblex-feed-fibre.rds")


font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()
showtext_opts(dpi = 300)   # must match ggsave dpi, otherwise text is mis-sized


plot_liblex <- function(obj, title, xlab, ylab, colour) {
  d <- data.frame(
    obs  = as.vector(obj$test_y),
    pred = as.vector(obj$y_hat_liblex$predictions$pred),
    sd   = as.vector(obj$y_hat_liblex$predictions$pred_sd)
  )
  d <- d[complete.cases(d), ]
  
  r2   <- cor(d$obs, d$pred)^2
  rmse <- sqrt(mean((d$pred - d$obs)^2))
  me   <- mean(d$pred - d$obs)
  
  d$panel <- sprintf(
    "%s\nR\u00b2 = %.3f | RMSE = %.3f%% | ME = %.3f%%",
    title, r2, rmse, me
  )
  
  # same x and y range, starting at 0
  lims <- c(0, 1.05 * max(c(d$obs, d$pred + d$sd)))
  
  ggplot(d, aes(obs, pred)) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", colour = "grey50", linewidth = 0.6) +
    geom_linerange(aes(ymin = pred - sd, ymax = pred + sd),
                   colour = "grey20", alpha = 0.3) +
    geom_point(colour = colour, alpha = 0.7, size = 3) +
    geom_smooth(method = "lm", formula = y ~ x,
                colour = "black", fill = "grey60", linewidth = 0.6) +
    facet_wrap(~ panel) +
    coord_cartesian(xlim = lims, ylim = lims, expand = FALSE) +
    labs(x = xlab, y = ylab) +
    theme_gray(base_size = 16, base_family = "ubuntu") +
    theme(strip.text = element_text(size = 16, family = "ubuntu"),
          aspect.ratio = 1)
}

p_protein <- plot_liblex(
  product_feed_protein,
  title  = "liblex: protein",
  xlab   = "Observed protein content (%)",
  ylab   = "Predicted protein content (%)",
  colour = "#C8102E"
)

p_fibre <- plot_liblex(
  product_feed_fibre,
  title  = "liblex: crude fibre",
  xlab   = "Observed crude fibre content (%)",
  ylab   = "Predicted crude fibre content (%)",
  colour = "#C8102E"
)

p <- p_protein + p_fibre
p

ggsave("fig/liblex-feed-val.png", p, width = 11, height = 6, dpi = 300)
