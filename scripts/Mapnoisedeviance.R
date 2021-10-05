library(tidyverse)
map_noise_deviance <- function(df) {
  df %>%
  group_by(animal_num) %>%
    nest() %>%
    mutate(model = map(data, function(df) lm(delF ~ time, data = df[60:116,])),
           glanced = map(model, broom::glance)) %>%
    unnest(glanced) %>%
    select(animal_num, deviance, df.residual, adj.r.squared)
}

WTbuff <- read_csv(file.choose())
WTbuffResid <- WTbuff %>%
  map_noise_deviance() %>%
  mutate(genotype = "N2", cue = "buffer")

WTIAA <- read_csv(file.choose())
WTIAAresid <- WTIAA %>%
  map_noise_deviance()  %>%
  mutate(genotype = "N2", cue = "IAA")

WTHexSat <- read_csv(file.choose())
HexSatresid <- WTHexSat %>%
  map_noise_deviance()  %>%
  mutate(genotype = "N2", cue = "Hex")

pWT <- rbind(WTbuff,WTIAA,WTHexSat) %>%
  filter(time > 15, time <29) %>%
  ggplot(aes(x = time, y = delF)) +
  geom_line(aes(color = cue, group = animal),alpha = 0.5) +
  theme_classic() +
  coord_cartesian(ylim = c(-.2,.2))

pWT + facet_grid(cue~.)

odr3buff <- read_csv(file.choose())
odr3buffresid <- odr3buff  %>%
  map_noise_deviance() %>%
  mutate(genotype = "odr3", cue = "buffer")

odr3IAA <- read_csv(file.choose())
odr3IAAresid <- odr3IAA  %>%
  map_noise_deviance()%>%
  mutate(genotype = "odr3", cue = "IAA")

WTbuff2 <- read_csv(file.choose())
WTbuff2resid <- WTbuff2  %>%
  map_noise_deviance()%>%
  mutate(genotype = "N2", cue = "buffer")

WTIAA2 <- read_csv(file.choose())
WTIAA2resid <- WTIAA2  %>%
  map_noise_deviance() %>%
  mutate(genotype = "WT", cue = "IAA")

WTbuff3 <- read_csv(file.choose())
WTbuff3resid <- WTbuff3  %>%
  map_noise_deviance() %>%
  mutate(genotype = "WT", cue = "IAA")


podr3 <- rbind(odr3buff,odr3IAA,WTbuff2,WTbuff3) %>%
  filter(time > 15, time <29) %>%
  ggplot(aes(x = time, y = delF)) +
  geom_line(aes(color = interaction(genotype,cue), group = animal),alpha = 0.5) +
  theme_classic() +
  coord_cartesian(ylim = c(-.2,.2))

podr3 + facet_grid(interaction(genotype,cue)~.)

# group by date:
rbind(WTbuff,WTbuff2,WTbuff3) %>%
  mutate(animalID = interaction(animal, animal_num)) %>%
  separate(animal, into = "date") %>%
  filter(time > 15, time <29) %>%
  ggplot(aes(x = time, y = delF)) +
  geom_line(aes(color = date, group = animalID),alpha = 0.5) +
  theme_classic() +
  coord_cartesian(ylim = c(-.2,.2)) +
  facet_grid(date~.)


rbind(WTbuffResid,
      WTbuff2resid,
      WTIAAresid,
      WTIAA2resid) %>%
  ggplot(aes(x = interaction(cue,genotype), y = adj.r.squared)) +
  geom_boxplot() +
  ggbeeswarm::geom_quasirandom(aes(color = cue), alpha = 0.5, width = 0.25) +
  coord_cartesian(ylim = c(0,1)) +
  labs(y = "adjusted r-squared \n (higher means less variability")

rbind(WTbuffResid,
      WTbuff2resid,
      WTIAAresid,
      WTIAA2resid) %>%
  ggplot(aes(x = interaction(cue,genotype), y = deviance)) +
  geom_boxplot() +
  ggbeeswarm::geom_quasirandom(aes(color = cue), alpha = 0.5, width = 0.25) +
  labs(y = "residual deviance \n (lower means less variability") +
  coord_cartesian(ylim = c(0,0.25))

library(magrittr)
rbind(HexResid,HexIAAresid) %>%
  lm(deviance ~ cue, data = .) %>% broom::glance()
  glm(deviance ~ cue, family = "quasipoisson") %>%
  summary()

rbind(Hex,odr1IAA) %>%
  #filter(time < 29.5) %>%
  ggplot(aes(x = time, y = delF)) +
  geom_line(aes(colour = genotype, lty = cue, group = interaction(animal_num,genotype)))

