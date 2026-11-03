library(ggplot2)
library(patchwork)
library(showtext)

product_feed_protein <- readRDS("~/Downloads/sandro/liblex-processed-meat-protein.rds")
product_feed_fat   <- readRDS("~/Downloads/sandro/liblex-processed-meat-fat.rds")


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
  lo  <- min(c(d$obs, d$pred - d$sd))
  hi  <- max(c(d$obs, d$pred + d$sd))
  pad <- 0.05 * (hi - lo)
  lims <- c(max(0, lo - pad), hi + pad)
  
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
  title  = "liblex: Protein",
  xlab   = "Observed protein content (%)",
  ylab   = "Predicted protein content (%)",
  colour = "#64b445"
)

p_fat <- plot_liblex(
  product_feed_fat,
  title  = "liblex: Fat",
  xlab   = "Observed fat content (%)",
  ylab   = "Predicted fat content (%)",
  colour = "#64b445"
)

p <- p_protein + p_fat
p

ggsave("fig/liblex-processed-meat-val.png", p, width = 11, height = 6, dpi = 300)
