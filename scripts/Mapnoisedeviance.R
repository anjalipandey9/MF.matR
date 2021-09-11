map_noise_deviance <- function(df) {
  df %>%
  group_by(animal_num) %>%
    nest() %>%
    mutate(model = map(data, function(df) lm(delF ~ time, data = df[1:118,])),
           glanced = map(model, broom::glance)) %>%
    unnest(glanced) %>%
    select(animal_num, deviance, df.residual, adj.r.squared)
}

Hex <- read_csv(file.choose())
HexResid <- Hex %>%
  map_noise_deviance() %>%
  mutate(genotype = "N2", cue = "buffer")

HexIAA <- read_csv(file.choose())
HexIAAresid <- HexIAA %>%
  map_noise_deviance()  %>%
  mutate(genotype = "N2", cue = "IAA")

odr3buf <- read_csv(file.choose())
odr3bufresid <- odr3buf  %>%
  map_noise_deviance() %>%
  mutate(genotype = "odr3", cue = "buffer")

odr3IAA <- read_csv(file.choose())
odr3IAAresid <- odr3IAA  %>%
  map_noise_deviance()%>%
  mutate(genotype = "odr3", cue = "IAA")

odr1IAA<- read_csv(file.choose())
odr1IAAresid <- odr1IAA %>%
  map_noise_deviance() %>%
  mutate(genotype = "odr1", cue = "IAA")

rbind(HexResid,
      HexIAAresid,
      odr3bufresid,
      odr3IAAresid,
      odr1IAAresid) %>%
  ggplot(aes(x = interaction(cue,genotype), y = adj.r.squared)) +
  geom_boxplot() +
  ggbeeswarm::geom_quasirandom(aes(color = cue), alpha = 0.5, width = 0.25) +
  coord_cartesian(ylim = c(0,1)) +
  labs(y = "adjusted r-squared \n (higher means less variability")

rbind(HexResid,
      HexIAAresid,
      odr3bufresid,
      odr3IAAresid,
      odr1IAAresid) %>%
  ggplot(aes(x = interaction(cue,genotype), y = deviance)) +
  geom_boxplot() +
  ggbeeswarm::geom_quasirandom(aes(color = cue), alpha = 0.5, width = 0.25) +
  coord_cartesian(ylim = c(0,1)) +
  labs(y = "residual deviance \n (lower means less variability")
  coord_cartesian(ylim = c(0,0.5))

library(magrittr)
rbind(HexResid,HexIAAresid) %>%
  lm(deviance ~ cue, data = .) %>% broom::glance()
  glm(deviance ~ cue, family = "quasipoisson") %>%
  summary()

rbind(Hex,odr1IAA) %>%
  #filter(time < 29.5) %>%
  ggplot(aes(x = time, y = delF)) +
  geom_line(aes(colour = genotype, lty = cue, group = interaction(animal_num,genotype)))

