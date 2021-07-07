library(tidyverse)
set.seed(1)
df <- rnorm(1000, mean = .65, sd = 1.75) %>% as_tibble() %>% filter(value >= 0) %>% mutate(decile = ntile(value,10),z = round((value - mean(value)) / sd(value),2))
df2 <- rnorm(10000, mean = 2, sd = 30) %>% as_tibble() %>% filter(value >= 0) %>% mutate(decile = ntile(value,10),z = round((value - mean(value)) / sd(value),2))
df3 <- rgeom(n = 1000, p = .1) %>% as_tibble() %>% mutate(decile = ntile(value,10),z = round((value - mean(value)) / sd(value),2))
df4 <- rgeom(n = 10000, p = .2) %>% as_tibble() %>% mutate(decile = ntile(value,10),z = round((value - mean(value)) / sd(value),2))


a <- ggplot(df2) +
 aes(x = value) +
  geom_density(adjust = 1L, fill = "#bb0000", alpha = .8, color = 'black', size = .3) +
  theme_minimal(base_family = 'IBM Plex Mono', base_size = 14) +
  labs(y="",x="")

b <- ggplot(df3) +
  aes(x = value) +
  geom_density(adjust = 1L, fill = "#003399", alpha = .8, color = 'black', size = .3) +
  theme_minimal(base_family = 'IBM Plex Mono', base_size = 14) +
  labs(y="",x="")

ggpubr::ggarrange(a,b)

median(df2$value)
median(df3$value)
mean(df2$value)
mean(df3$value)

(nrow(df %>% filter(z>=2)))/nrow(df)
(nrow(df2 %>% filter(z>=2)))/nrow(df2)
(nrow(df4 %>% filter(z>=2)))/nrow(df4)


decile_histogram <- function(data, var, n_breaks = 100) {
  breaks <- seq(min(data[[var]]), max(data[[var]]), length.out = n_breaks)
  quantiles <- quantile(data[[var]], seq(0, 1, 0.1))
  quantiles2 <- sapply(quantiles, function(x) breaks[which.min(abs(x - breaks))])

  data$bar <- as.numeric(as.character(
    cut(data[[var]], breaks, na.omit((breaks + dplyr::lag(breaks)) / 2)))
  )
  data$fill <- cut(data[[var]], quantiles2, na.omit((quantiles2 + dplyr::lag(quantiles2)) / 2))

  ggplot2::ggplot(data, ggplot2::aes(bar, y = 1, fill = fill)) +
    ggplot2::geom_col(position = 'stack', col = 1, show.legend = FALSE, width = diff(breaks)[1], size = 0.3) +
    ggplot2::scale_fill_brewer(type = 'qual', palette = 3) +
    ggplot2::theme_minimal(base_family = 'IBM Plex Mono') + ggplot2::ylim(0,100) +
    ggplot2::coord_fixed(diff(breaks)[1], expand = FALSE) +
    ggplot2::labs(x = '', y = '')
}

a <- decile_histogram((rnorm(5000, mean = 0, sd = 50) %>% as_tibble() %>% filter(value >= 0)),'value',150)
b <- decile_histogram((rnorm(5000, mean = 0, sd = 3) %>% as_tibble() %>% filter(value >= 0)),'value',150)

ggpubr::ggarrange(a,b, ncol = 2,label.y = 1)



# more garbage ------------------------------------------------------------

library(PearsonDS)
library(tidyverse)

decile_histogram <- function(data, var, n_breaks = 100) {
  breaks <- seq(min(data[[var]]), max(data[[var]]), length.out = n_breaks)
  quantiles <- quantile(data[[var]], seq(0, 1, 0.1))
  quantiles2 <- sapply(quantiles, function(x) breaks[which.min(abs(x - breaks))])

  data$bar <- as.numeric(as.character(
    cut(data[[var]], breaks, na.omit((breaks + dplyr::lag(breaks)) / 2)))
  )
  data$fill <- cut(data[[var]], quantiles2, na.omit((quantiles2 + dplyr::lag(quantiles2)) / 2))

  ggplot2::ggplot(data, ggplot2::aes(bar, y = 1, fill = fill)) +
    ggplot2::geom_col(position = 'stack', col = NA, show.legend = FALSE, width = diff(breaks)[1], size = 0.2) +
    ggplot2::scale_fill_brewer(type = 'qual', palette = 3) +
    ggplot2::theme_void(base_family = 'IBM Plex Mono') + ggplot2::ylim(0,60) + ggplot2::xlim(-1,8) +
    ggplot2::coord_fixed(diff(breaks)[1], expand = FALSE) +
    ggplot2::labs(x = '', y = '')
}
set.seed(1)
a <- decile_histogram((rpearson(1000, moments = c(mean=1,variance=3,skewness=0,kurtosis=5)) %>% as_tibble() %>% filter(value >= 0)),'value',100)
b <- decile_histogram((rpearson(1000, moments = c(mean=1,variance=1,skewness=1.5,kurtosis=6)) %>% as_tibble() %>% filter(value >= 0)),'value',100)


ggpubr::ggarrange(a,b, ncol = 1,label.y = 1) +
  ggsave('data-raw/dists.png', width = 10)
