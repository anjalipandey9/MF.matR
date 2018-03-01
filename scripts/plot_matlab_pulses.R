#' ---
#' output: github_document
#' ---
library(R.matlab)
library(tidyr)

  data <- readMat(file.choose())
  conditions <- names(data)
  #get filenames from matlab structure using legnth of each dataset and given each dataset has 28 fields:
  filenames <- purrr::map(conditions,
                          function(x) {
                            data[[x]][seq(from = 19, to = length(data[[x]]), by = 28)] %>% unlist
                            })
  names(filenames) <- conditions

  #states (behmat) = column #5
  states <- purrr::map(conditions,
                       function(x) {
                        data[[x]][seq(from = 5, to = length(data[[x]]), by = 28)]
                       }) %>% MF.matR::melt_matData(., datatype = state) %>% mutate(state = factor(state))

  #field #3 is frame in cycle - believe this is the worm with respect to pulse timing
  FrameInCycle <- purrr::map(conditions,
                       function(x) {
                         data[[x]][seq(from = 3, to = length(data[[x]]), by = 28)]
                       }) %>% MF.matR::melt_matData(., datatype = FrameInCycle)
  #field #4 is cycle number - believe this is synced to pulse timing
  CycleNr <- purrr::map(conditions,
                        function(x) {
                          data[[x]][seq(from = 4, to = length(data[[x]]), by = 28)]
                        }) %>% MF.matR::melt_matData(., datatype = CycleNr)

  #Sanity check to make sure dfs have not changed order:
  purrr::map(seq(1,2), ~all.equal(states[,.],FrameInCycle[,.],CycleNr[,.]))
  purrr::map(seq(4,5), ~all.equal(states[,.],FrameInCycle[,.],CycleNr[,.]))

  #merge based on identical structure to the data (not sure why cbind is changing column name here):
  mat.all.data <- data.frame(cbind(states,FrameInCycle[,3],CycleNr[,3])) %>%
    rename(FrameInCycle = FrameInCycle...3.,
           CycleNr = CycleNr...3.) %>%
    tidyr::separate(assay, c("date", "side", "genotype"), sep = "_") %>%
    mutate(state = case_when(
      state == "1" ~ "Forward",
      state == "3" ~ "Reverse",
      state == "4" ~ "Pause",
      state %in% c("5","6") ~ "Pirouette",
      TRUE ~ "undetermined"
    ))

  #write raw data to csv:
  data.table::fwrite(mat.all.data, file.path(here::here(),"data","matlab_allTrackData.csv"))

  #make proportion table averaging over cycles and worms:
  state_probs <- mat.all.data %>%
    dplyr::count(genotype,date,side,cue,FrameInCycle,state) %>%
    dplyr::group_by(genotype,date,side,cue,FrameInCycle) %>%
    dplyr::mutate(prop = prop.table(n)) %>%
    mutate(logit.p = boot::logit(prop)) %>% ungroup() %>%
    group_by(genotype,cue) %>%
    mutate(centered_logit = scale(logit.p, scale = FALSE))

  data.table::fwrite(state_probs, file.path(here::here(),"data","state_probs_byassay.csv"))

  #make proportion table including cycles, averaging over worms: #need to complete proprortions to get zeros again
  state_probs_bycycle <- mat.all.data %>%
    dplyr::count(genotype,date,side,cue,CycleNr,FrameInCycle,state) %>%
    dplyr::group_by(genotype,date,side,cue,CycleNr,FrameInCycle) %>%
    dplyr::mutate(prop = prop.table(n))

  data.table::fwrite(state_probs_bycycle, file.path(here::here(),"data","state_probs_bycycle.csv"))





  dplyr::filter(state == "Forward",
                !is.na(FrameInCycle),
                genotype %in% c("N2","tax4","osm9"),
                cue %in% c("hex","hexiaa")) %>%
  ggplot(aes(x = FrameInCycle, y = as.numeric(centered_logit))) +
  geom_line(
    aes(
      group = interaction(genotype, cue),
      colour = genotype, linetype = cue
    ),
    stat = "smooth", method = "loess", span = 0.2
  )


  # make binned cycles to see if time is an influence:
  state_probs_bycycle %<>% mutate(cycle_bin = ntile(CycleNr,6))

  ### shows some time variability
  state_probs_bycycle %>%
    dplyr::filter(state == "Forward",
                  !is.na(cycle_bin),
                  FrameInCycle != 1,
                  #date != "20170502",
                  genotype %in% c("N2","tax4","osm9"),
                  cue %in% c("hex","hexiaa","iaa")) %>%
    ggplot(aes(x = FrameInCycle, y = prop)) +
    geom_line(
      aes(
        group = interaction(genotype, cue, side),
        colour = interaction(side), linetype = cue
      ),
      stat = "smooth", method = "loess", span = 0.4
    ) + facet_wrap(~genotype)


  #look at N2 time variability
  ### shows some time variability
  state_probs_bycycle %>%
    dplyr::filter(state == "Forward",
                  !is.na(cycle_bin),
                  FrameInCycle != 1,
                  #date != "20170502",
                  genotype == "N2",
                  cue == "hexiaa") %>%
    ggplot(aes(x = FrameInCycle, y = prop)) +
    geom_line(
      aes(
        group = side,
        colour = side, linetype = cue
      ),
      stat = "smooth", method = "loess", span = 0.3
    ) + facet_wrap(~cycle_bin)
