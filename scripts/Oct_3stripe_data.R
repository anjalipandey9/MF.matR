library(MF.matR)
library(tidyverse)
# behavioral probabilities for 3 stripe octanol data:

behmat <- beh_mat_tidy() # add relative path
ymat <- y_mat_tidy()
xmat <- x_mat_tidy()
dirmat <- dir_mat_tidy()
spdmat <- spd_mat_tidy()

tot.mat <- list(behmat, ymat, xmat, dirmat, spdmat) %>%
  purrr::reduce(full_join, by = colnames(behmat[1:9]))

tot.mat %>%
  dplyr::filter(condition == "OP50", exp == 4) %>%
  ggplot(aes(x = x, y = y)) + geom_point(aes(colour = state_name), alpha = 0.3) +
  facet_wrap(~cut(time, 2))

tot.mat %>%
  dplyr::filter(state_name != "no_state_call") %>%
  ggplot(aes(x = y, fill = state_name)) + geom_histogram(aes(y=0.5*..density..),
                            alpha=0.5,position='identity',binwidth=0.5) +
  facet_wrap(condition~exp)

tot.mat <- dplyr::full_join(behmat, ymat) %>%
  dplyr::full_join(., dirmat) %>%
  dplyr::filter(y != "NaN")

#carry forward directional info by wormID for pauses:
tot.mat %<>%
  dplyr::group_by(wormID) %>%
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with("dir")),
                   dplyr::funs(dplyr::case_when(
                     state_name == "Pause" ~ zoo::na.locf(.),
                     TRUE ~ .
                   )))


#bounds<-c(0, -3.2,-6.75, -9.67, -12.875, -16) # enter boundaries of dye (cue) - used for oct
bounds<-c(0, -3, -6, -9, -12)

tot.mat.enter<-norm_beh_enter_3(tot.mat)
tot.mat.exit<-norm_beh_exit_3(tot.mat)
