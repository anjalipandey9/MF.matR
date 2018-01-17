p <- WL.alldata %>%
  dplyr::filter(worm == "X20") %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(colour = Time, fill = speed, group = worm), alpha = 1) +
  labs(title = "All tracks") +
  viridis::scale_color_viridis(option = "inferno") +theme_classic() #+coord_cartesian(xlim = c(0,32000))
# worms in wormlab position are based on original video (uncropped) pixel position.


# need to reverse head direction when it has it opposite:

# if mean direction is negative, it is likely wrong, but
# if we select only the frames that it is moving, and if these are over Threshold n,
# then we can swtich head dir. Can use this for reversals -

#


ggplotly(p)


data %<>% mutate(state = as.factor(if_else(abs(speed) < 30, "pause",
                                if_else(abs(speed) < 20
                                        & length < 25 & bin.ang.vel > 75, "pir",
                                        if_else(speed < -15, "rev","fwd")))))


probs <- data %>% dplyr::filter(x > 400, x < 950,  y > 75, y < 675) %>% # add a gather call to include other variables
  mutate(ybin = cut(y,30)) %>%
  group_by(ybin,state) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

states <- with(probs, expand.grid(state = levels(state), ybin = levels(ybin)))

all.probs <- merge(probs, states, all.y = TRUE) %>% mutate(freq = if_else(is.na(freq), 0.001, freq))

all.probs %>% ggplot(aes(x = ybin, y = boot::logit(freq))) +
  geom_smooth(aes(group = state, colour = state), method = "loess", span = .5)

all.probs %>% ggplot(aes(x = ybin, y = boot::logit(freq))) +
  geom_point(aes(colour = state))



(probs <- data %>% dplyr::filter(x > 400, x < 950,  y > 75, y < 675) %>% # add a gather call to include other variables
  mutate(ybin = cut(y,100)) %>%
  group_by(ybin,state) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% mutate(state = as.factor(state)) %>% ggplot(aes(x = ybin, y = boot::logit(freq))) +
  geom_smooth(aes(group = state, colour = state), method = "loess", span = 1))

(probs <- data %>% dplyr::filter(x > 400, x < 950, y > 75, y < 675) %>% # add a gather call to include other variables
  mutate(ybin = cut(y,100)) %>%
  group_by(ybin,state) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% ggplot(aes(x = ybin, y = boot::logit(freq))) +
  geom_point(aes(colour = state)))

