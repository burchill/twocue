#'
#' Does a few sanity checks on the experimental data
#'
#' Mostly designed as a sanity check for the pilot data so far
#'
#' @param df The data frame
#' @return Nothing
#' @export sanity_check_data
sanity_check_data <- function(df, data_type=c("norming", "exp1")) {
  data_type <- match.arg(data_type)
  require("dplyr")

  # Everyone
  if (!column_count_equal(df, WorkerId, n()))
    stop("Rows across participants not equal!")

  # Norming
  if (data_type == "norming") {
    if (!column_count_equal(df, paste(contrast, noise_cond), n_distinct(WorkerId)))
      stop("# of subjs per condition not equal!")

    # Should add more
    if (!all(df$Response %in% c("ba","pa","da","ta","")))
      stop("Some responses dont make sense!")
  }

  # Exp1
  if (data_type == "exp1") {
    # If the number of subjects per condition is not equal
    if (!column_count_equal(df, paste(subcond, Condition), n_distinct(WorkerId)))
      stop("# of subjs per condition not equal!")

    # If the URL-param-derived 'condition' is not the same as 'Condition'
    if (nrow(filter(df, Condition != condition)) != 0)
      stop("`Condition` should equal `condition`")

    # If somehow somebody got away with not pressing d or t in test
    yarg <- df %>% filter(grepl("test",PartOfExp), !(KeyPressed %in% c(84, 68)))
    if (nrow(yarg) > 0)
      stop("Somebody pressed something other than d/t in test!")

    # If some of the workers have trials that are 0 ms long
    zero_length_trials <- df %>%
      group_by(WorkerId) %>%
      arrange(RTStart, .by_group=TRUE) %>%
      mutate(trial_time = RTStart - lag(RTStart)) %>%
      filter(any(trial_time==0, na.rm=TRUE))
    if (nrow(zero_length_trials) > 0)
      stop(paste(n_distinct(zero_length_trials$WorkerId), " workers have 0 ms trial lengths!"))

    # If the number of catch trials differ by participant
    if (!column_count_equal(df, WorkerId, sum(IsCatch=="Y")))
      stop("Catch trials per subj not equal!")
  }

  print("Sanity check ok!")
}

# Checks to make sure there are no repeat participants
no_repeat_workers <- function(d) {
  if (column_count_equal(d, "WorkerId")) {
    stop("Rows across participants not equal!")
  }
}

# Checks to see if all data grouped by a column have the same counts
column_count_equal <- function(d, column_string, expr) {
  q <- enquo(column_string)
  q2 <- enquo(expr)
  d %>%
    group_by(!!q) %>%
    summarise(n := !!q2) %>%
    {n_distinct(.$n) == 1}
}

















# If somebody fails to pass, they should have no exposure blocks
# Their repeated survey questions should be the same (kinda)
# They should not have any survey questions missing
# No missing data anywhere tbh
# Check to make sure no NAs in survey
# uniqueids should have data in every thing
# Survey data should make sense in terms of trial order, etc.
# for everybody that had any exposure blocks, they should have all the same stuff
# make sure that if you have exposure, you have EVERY exposure block (per person)
# DON'T USE THE COUNT EQUAL THING!!!! HARDCODE EVERYTHING!!!!! WORK UP FROM THE BOTTOM
# check to see if url params line up with filenames and Words
# Change UniqueID to WorkerId

zprob_check <- function(df, message, cnd="error") {
  # HACK (for knitr i guess)
  caller <- ""
  try({
    caller <- eval.parent(expr(match.call()),7)
    if (deparse(caller) == "match.call()")
      caller <- eval.parent(expr(match.call()),1)
  }, silent = TRUE)
  if (nrow(df) > 0) {
    .zprobs <<- df
    m <- paste0("in ", deparse(caller), ": ",
                message, "\n", "See `.zprobs` for who messed up.")
    signal(m, class=cnd)
  }
}



# Checking flags! ---------------------------

# Need to add dprime stuff
check_flags <- function(l) {
  check_no_na_flags(l)
  check_if_survey_flag_matches(l)
  check_flag_order_logic(l)
  check_dprime_flag(l)
}

pre_exp4_get_dprime_data <- function(l) {
  dpfn <- . %>%
    mark_mistakes(slow_presses = FALSE) %>%
    group_by(WorkerId) %>%
    mutate(IsCatch = !is.na(CatchTrialType)) %>%
    summarise(fa = sum(MistakeType == "false_positive"),
              miss = sum(MistakeType == "false_negative"),
              hit = sum(IsCatch & MistakeType=="correct"),
              c_rej = sum(!IsCatch & MistakeType=="correct")) %>%
    transmute(WorkerId = WorkerId,
              dprime = dprime(hit, miss, fa, c_rej))

  real_exp_dprimes <- l$resp %>%
    filter(grepl("exposure", PartOfExp),
           !grepl("gk", PartOfExp)) %>%
    left_join(l$survey) %>%
    dpfn()

  gk_dprimes <- l$resp %>%
    filter(grepl("gk_exposure", PartOfExp)) %>%
    left_join(l$survey) %>%
    dpfn()

  bind_rows("gk" = gk_dprimes,
            "real" = real_exp_dprimes,
            .id = "Exposure") %>%
    group_by(WorkerId) %>%
    mutate(didPart2 = any(Exposure=="real"))
}

check_dprime_flag <- function(l, cutoff=2.8, warn=TRUE) {
  dprime_vals <- l$resp %>%
    filter(grepl("gk_exposure", PartOfExp)) %>%
    left_join(l$survey) %>%
    mark_mistakes(slow_presses = FALSE) %>%
    group_by(WorkerId) %>%
    mutate(IsCatch = !is.na(CatchTrialType)) %>%
    summarise(fa = sum(MistakeType == "false_positive"),
              miss = sum(MistakeType == "false_negative"),
              hit = sum(IsCatch & MistakeType=="correct"),
              c_rej = sum(!IsCatch & MistakeType=="correct")) %>%
    transmute(WorkerId = WorkerId,
              dprime = dprime(hit, miss, fa, c_rej))
  l$survey %>%
    select(WorkerId, failed_dprime) %>%
    left_join(dprime_vals) %>%
    filter(failed_dprime == (dprime >= cutoff)) %>%
    zprob_check("Users had weird d' values!",
                cnd = ifelse(warn,"warning","error"))
}


pre_exp4_get_dprime_data <- function(l) {
  dpfn <- . %>%
    mark_mistakes(slow_presses = FALSE) %>%
    group_by(WorkerId) %>%
    mutate(IsCatch = !is.na(CatchTrialType)) %>%
    summarise(fa = sum(MistakeType == "false_positive"),
              miss = sum(MistakeType == "false_negative"),
              hit = sum(IsCatch & MistakeType=="correct"),
              c_rej = sum(!IsCatch & MistakeType=="correct")) %>%
    transmute(WorkerId = WorkerId,
              dprime = dprime(hit, miss, fa, c_rej))

  real_exp_dprimes <- l$resp %>%
    filter(grepl("exposure", PartOfExp),
           !grepl("gk", PartOfExp)) %>%
    left_join(l$survey) %>%
    dpfn()

  gk_dprimes <- l$resp %>%
    filter(grepl("gk_exposure", PartOfExp)) %>%
    left_join(l$survey) %>%
    dpfn()

  bind_rows("gk" = gk_dprimes,
            "real" = real_exp_dprimes,
            .id = "Exposure") %>%
    group_by(WorkerId) %>%
    mutate(didPart2 = any(Exposure=="real"))
}



check_no_na_flags <- function(l) {
  df <- l$survey %>%
    filter(any(is.na(bad_survey) |
               is.na(failed_dprime) |
               is.na(did_pass)))
  zprob_check(df, "NAs in survey flags!")
}
check_flag_order_logic <- function(l) {
  dprime_failures <- l$survey %>%
    filter(failed_dprime) %>%
    unipluck(UniqueID)
  l$first_survey %>%
    filter(UniqueID %in% dprime_failures) %>%
    zprob_check("d' failures took survey!")

  first_survey_failures <- l$first_survey %>%
    filter(PartOfExp == "surveyblock") %>%
    group_by(UniqueID) %>%
    filter(any(BadAnswer)) %>%
    unipluck(UniqueID)
  l$first_survey %>%
    filter(UniqueID %in% first_survey_failures,
           PartOfExp == "continuingblock") %>%
    zprob_check("Ppl who failed 1st survey took 2nd!")

  in_experiment <- l$resp %>%
    filter( grepl("exposure", PartOfExp),
           !grepl("gk", PartOfExp)) %>%
    unipluck(UniqueID)
  l$survey %>%
    filter(did_pass != UniqueID %in% in_experiment) %>%
    zprob_check("Ppl who failed are in experiment or vice versa!")
}
check_if_survey_flag_matches <- function(l) {
  bad_boys <- l$first_survey %>%
    group_by(UniqueID) %>%
    filter(any(BadAnswer)) %>%
    unipluck(UniqueID)
  l$survey %>%
    filter(UniqueID %in% bad_boys,
           bad_survey == FALSE) %>%
    zprob_check("User surveys and reported survey status dont agree!")
}







# A general check
check_surveyblock_data <- function(l) {
  df <- l$first_survey %>%
    filter(PartOfExp == "surveyblock")
  if (!column_count_equal(df, UniqueID, n()))
    stop("Rows across participants in survey block not equal!")
  df <- l$first_survey %>%
    filter(PartOfExp == "continuingblock")
  if (!column_count_equal(df, UniqueID, n()))
    stop("Rows across participants in continue block not equal!")
  if (any(is.na(l$first_survey$QResp)))
    stop("NAs in survey responses!")
}

check_test_trial_order <- function(l) {
  l$resp %>%
    filter(grepl("normaltest", PartOfExp)) %>%
    group_by(UniqueID, PartOfExp) %>%
    mutate(ya = (seq_along(PartOfExp)-1) %/% 12) %>%
    group_by(UniqueID, PartOfExp, ya) %>%
    summarise(n = n_distinct(Filename)) %>%
    filter(n != 12) %>%
    zprob_check("Normaltest trial order not randomized right")

  l$resp %>%
    filter(grepl("gridtest", PartOfExp)) %>%
    group_by(UniqueID, PartOfExp) %>%
    mutate(ya = (seq_along(PartOfExp)-1) %/% 20) %>%
    group_by(UniqueID, PartOfExp, ya) %>%
    summarise(n=n_distinct(Filename)) %>%
    filter(n!=20) %>%
    zprob_check("Gridtest trial order not randomized correctly")
}

basic_exposure_checks <- function(l) {
  l$resp %>%
    group_by(PartOfExp, UniqueID) %>%
    summarise(n_good  = n(),
              n2_good = n_distinct(Filename),
              nc_good = n_distinct(paste(condition, subcond))) %>%
    summarise_at(vars(n_good:nc_good), ~n_distinct(.)==1) %>%
    filter(if_any(n_good:nc_good, .fns = ~!map_lgl(., isTRUE))) %>%
    zprob_check("Exposure trial order not randomized correctly")

  temp <- l$resp %>%
    group_by(UniqueID, PartOfExp) %>%
    summarise(t1 = min(RTStart),
              tmax = max(RTStart)) %>%
    arrange(UniqueID, t1)
  temp %>%
    filter(tmax<lag(t1)) %>%
    zprob_check("Some parts of the experiment overlap with others!")

  df <- temp %>%
    summarise(order = paste(PartOfExp, collapse=", ")) %>%
    group_by(order) %>%
    summarise(n=n(),
              peeps=paste(UniqueID,collapse=", ")) %>%
    filter(order != "gk_exposure")
  if (nrow(df) > 1) {
    .zprobs <<- df
    stop("People differ in the order they took the experiment!\n",
         "See `.zprobs` for who messed up.")
  }

  df <- l$resp %>%
    left_join(l$survey %>% distinct(UniqueID,Run)) %>%
    group_by(PartOfExp, Condition) %>%
    mutate(total_subjs = n_distinct(UniqueID)) %>%
    group_by(total_subjs,
             PartOfExp, VOT, f0, Condition) %>%
    summarise(sub_subjs = n_distinct(UniqueID),
              runs = paste(unique(Run), collapse = ", "),
              all_subjs = paste(unique(WorkerId), collapse=", "),
              ntotal=n())

  df %>%
    group_by(PartOfExp, VOT, f0, Condition) %>%
    filter(sub_subjs < max(sub_subjs)) %>%
    zprob_check("These tokens didn't have equal numbers of subjects responding!")
  df %>%
    group_by(PartOfExp) %>%
    filter(sub_subjs != total_subjs) %>%
    zprob_check("These tokens didn't have full numbers of subjects responding!")
  df %>%
    group_by(Condition, PartOfExp, VOT, f0) %>%
    filter(n_distinct(all_subjs)!=1) %>%
    zprob_check("These tokens had differing numbers of subjects!")
}

# 1) Are workers in all parts of exp4 that they need to be in?
# 2) Do people have multiple HITs?
# 3) Do they have the same values across conditions and stuff?
check_workers <- function(l) {
  all_workers <- map(l, ~pluck(.,"WorkerId")) %>%
    set_names(NULL) %>%
    flatten_chr() %>% unique()
  map_dfc(l, function(df) {all_workers %in% df$WorkerId}) %>%
    mutate(WorkerId = all_workers) %>%
    filter(!resp | !survey) %>% # they don't have to take the 1st survey
    zprob_check("Some workers are absent from some parts of Exp4!")

  l$survey %>%
    group_by(WorkerId) %>%
    filter(n_distinct(UniqueID) > 1) %>%
    zprob_check("These Workers have multiple unique IDs!")

  l$resp %>%
    group_by(UniqueID) %>%
    summarise(n=n_distinct(Condition),
              n2=n_distinct(subcond)) %>%
    filter(n>1 | n2>1)%>%
    zprob_check("These workers have multiple listed conditions")
}



#----------------------------------------------------

#'
#' Does a few sanity checks on the Exp 4 data
#'
#' Just for 'Exp 4'
#'
#' @param l The list of data frames
#' @return Nothing
#' @export sanity_check_exp4
sanity_check_exp4 <- function(l) {
  require("dplyr")


  check_workers(l)
  check_surveyblock_data(l)
  check_flags(l)
  check_test_trial_order(l)
  basic_exposure_checks(l)

  print("Sanity check ok!")
}
