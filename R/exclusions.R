#' Exclude and keep track of participants
#'
#' @description
#'
#' A tidy way of excluding and keeping track of exlcuded participants.
#'
#' @section The return function:
#'
#' The return value of `start_exclusions` is a function that has the following arguments:
#'
#' - `df`: The dataframe you want to exclude participants from
#'
#' - `reason`: The stated reason for these participants' exclusion
#'
#' - \dots: Either the input to a `filter()` function that selects
#' the participants to be excluded, or a list of WorkerIds to be excluded
#'
#' - `just_ids=TRUE`: If you're supplying a list of filtering criteria,
#' this should be `FALSE`
#'
#' - `give_df=FALSE`: If true, returns the excluded participants dataframe
#' (and doesn't do any excluding)
#'
#' @param \dots The names of any additional columns to keep in the
#' excluded subjects dataframe. Should have constant values within subjects.
#' @return A function (described below)
#' @export start_exclusions
start_exclusions <- function(..., is_exp4 = FALSE) {
  cols2keep <- enquos(...)
  cols2keep <- quos(!!!cols2keep, WorkerId)
  excluded_df <- data.frame()
  step_count <- 1

  function(df, reason, ..., just_ids=TRUE, give_df=FALSE) {
    if (give_df)
      return(excluded_df)
    qs <- enquos(...)

    if (just_ids) {
      stopifnot(length(qs)==1)
      bad_boys <- rlang::eval_tidy(qs[[1]]) %>%
        unique()
    } else {
      bad_boys <- df %>%
        filter(!!!qs) %>%
        distinct(WorkerId) %>%
        pluck("WorkerId")
    }

    to_exclude <- df %>%
      filter(WorkerId %in% bad_boys) %>%
      select(!!!cols2keep) %>%
      distinct(WorkerId, .keep_all = TRUE) %>%
      mutate(Reason = reason, Step = step_count)

    excluded_df <<- excluded_df %>%
      bind_rows(to_exclude)
    step_count <<- step_count + 1

    df %>%
      filter(!(WorkerId %in% bad_boys))
  }
}

#' A hack for making the Exp4 gatekeeper exclusions easier
#'
#' Essentially, it just collapses the list and does the exclusion, then gets the list
#' of everyone excluded and removes them from each data frame in the list.
#'
#' @export
exp4_gk_exclusion_crutch <- function(l, excl_fn, ...) {
  # Collapses the list, and runs the excluding function
  collapse_exp4(l, remove_gk = FALSE) %>%
    excl_fn(...) %>%
    invisible()
  # Get the excluded workers
  bads <- excl_fn(give_df = TRUE) %>%
    unipluck(WorkerId)
  # Removes them all
  map(l, function(x) {
    filter(x, !(WorkerId %in% bads))
  })
}


#' Get exclusion steps for each experiment
#'
#' Given an experiment and a numeric step value, returns a function
#' that takes a data frame and outputs a list of `WorkerId`s (not guarunteed to be unique)
#' for input into the function producted by `start_exclusions()`.
#'
#' @param exp A string of the experiment name
#' @param exc_step An integer of the exclusion step
#' @return A function that takes in a dataframe and outputs a list of `WorkerId`s
#' @examples
#' exclude_exp1 <- start_exclusions(Condition)
#' d.ex1_goodsubj <- d.ex1_raw %>%
#'   {exclude_exp1(., "Technical problem", exclusion_step("Exp1", 1)(.))} %>%
#'   {exclude_exp1(., "> 4 visual catch trial false negatives", exclusion_step("Exp1", 2)(.))} %>%
#'   {exclude_exp1(., "> 15 visual catch trial false positives", exclusion_step("Exp1", 3)(.))} %>%
#'   {exclude_exp1(., "Took a break > 10 min", exclusion_step("Exp1", 4)(.))} %>%
#'   {exclude_exp1(., "Multilingual", exclusion_step("Exp1", 5)(.))} %>%
#'   {exclude_exp1(., "Did not wear headphones", exclusion_step("Exp1", 6)(.))} %>%
#'   {exclude_exp1(., "Did not take experiment in quiet location", exclusion_step("Exp1", 7)(.))}
#'
#'@export
exclusion_step <- function(exp=c("Exp1", "Exp2", "Exp3", "Exp4"), exc_step = 1) {
  exp <- match.arg(exp)
  if (exp=="Exp1") {
    exclusion_functions_exp1(exc_step)
  } else if (exp=="Exp2") {
    exclusion_functions_exp2(exc_step)
  } else if (exp=="Exp3") {
    exclusion_functions_exp3(exc_step)
  } else if (exp=="Exp4") {
    exclusion_functions_exp4(exc_step)
  }
}

# CONSTANTS ------------------------------------------------
# FN_PROP = 0.5 # unused
# FP_PROP = 0.3 # unused
MAX_BREAK_MIN = 8
# ----------------------------------------------------------

exclusion_functions_exp1  <- function(exc_step) {
  exc_step %>%
    switch (
      `1` = purrr::partial(get_excluded.trials_not_n, n = 348),
      `2` = purrr::partial(get_excluded.more_than_n_mistakes, ...=, max_mistakes = 5,
                           allow_slow_presses = FALSE, CatchTrialType %in%  c("vis", NA)),
      `3` = purrr::partial(get_excluded.n_min_break, n_min = MAX_BREAK_MIN),
      `4` = get_excluded.multilinguals,
      `5` = get_excluded.no_headphones,
      `6` = get_excluded.not_in_quiet
    )
}

exclusion_functions_exp2  <- function(exc_step) {
  exc_step %>%
    switch (
      `1` = purrr::partial(get_excluded.trials_not_n, n = 140),
      `2` = purrr::partial(get_excluded.more_than_n_mistakes, ...=, max_mistakes = 2,
                           allow_slow_presses = FALSE),
      `3` = purrr::partial(get_excluded.n_min_break, n_min = MAX_BREAK_MIN),
      `4` = get_excluded.multilinguals,
      `5` = get_excluded.no_headphones,
      `6` = get_excluded.not_in_quiet
    )
}

exclusion_functions_exp3  <- function(exc_step) {
  exc_step %>%
    switch (
      `1` = purrr::partial(get_excluded.trials_not_n, n = 500),
      `2` = purrr::partial(get_excluded.more_than_n_mistakes, ...=, max_mistakes = 14,
                           allow_slow_presses = FALSE),
      `3` = purrr::partial(get_excluded.n_min_break, n_min = MAX_BREAK_MIN),
      `4` = get_excluded.multilinguals,
      `5` = get_excluded.no_headphones,
      `6` = get_excluded.not_in_quiet
    )
}

exclusion_functions_exp4  <- function(exc_step) {
  exc_step %>%
    switch (
      # Note, doesn't capture everything, should do the pre-analysis to check
      `1` = purrr::partial(get_excluded.trials_not_n, n = c(52, 552)),
      # Gatekeeper exclusions
      `2` = . %>% filter(failed_dprime==TRUE)  %>% unipluck(WorkerId),
      `3` = . %>% filter(bad_survey==TRUE &
                           is.na(choose2stop)) %>% unipluck(WorkerId),
      `4` = . %>% filter(choose2stop==TRUE)    %>% unipluck(WorkerId),
      # Non-gatekeeper exclusions
      `5` = purrr::partial(get_excluded.more_than_n_mistakes, ...=, max_mistakes = 14,
                           allow_slow_presses = FALSE),
      `6` = purrr::partial(get_excluded.n_min_break, n_min = MAX_BREAK_MIN),
      `7` = get_excluded.multilinguals,
      `8` = get_excluded.no_headphones,
      `9` = get_excluded.not_in_quiet
    )
}


get_excluded.trials_not_n <- function(df, n) {
  # Exclude faulty experiments
  group_by(df, WorkerId) %>% filter(!(n() %in% n)) %>%
    pluck("WorkerId")
}
get_excluded.not_in_quiet <- function(df) {
  # Not in quiet
  filter(df, is.na(Answer.location) | Answer.location != "yes") %>%
    pluck("WorkerId")
}
get_excluded.no_headphones <- function(df) {
  # No headphones
  filter(df, !(Answer.audio_type %in% c("over-ear","in-ear"))) %>%
    pluck("WorkerId")
}
get_excluded.multilinguals <- function(df) {
  # Exclude language background stuff (only multilinguals excluded rn)
  filter(df, as.character(Answer.language_background) =="multilingual") %>%
    pluck("WorkerId")
}
get_excluded.n_min_break <- function(df, n_min) {
  # Exclude people who took a break longer than ten minutes between trials
  filter(df, RTStart > 0) %>%
    mutate(trialstart = as.POSIXct(RTStart/1000, origin="1970-01-01")) %>%
    group_by(WorkerId) %>%
    arrange(trialstart, .by_group=TRUE) %>%
    mutate(gap = trialstart - lag(trialstart)) %>%
    summarise(max_gap = max(gap, na.rm = T)) %>%
    filter(max_gap > lubridate::dminutes(n_min)) %>%
    pluck("WorkerId")
}



# A wrapper for `get_excluded.fn_per_block_and_type()` for Exp 3
# Makes the first exposure block one block
exp3_block1_wrapper <- function(df, fn, ...) {
  df %>%
    mutate(PartOfExp = ifelse(PartOfExp == "exposure1",
                              "exposure2", PartOfExp)) %>%
    fn(...)
}

# Dots filter after marking mistakes
get_excluded.fn_per_block_and_type <- function(df,
                                               gt_prop = 0.5,
                                               allow_slow_presses = TRUE,
                                               ...) {
  qs <- enquos(...)
  df %>%
    filter(grepl("exposure", PartOfExp)) %>%
    mark_mistakes(slow_presses = allow_slow_presses) %>%

    filter(!!!qs) %>%
    group_by(WorkerId, PartOfExp, CatchTrialType) %>%
    summarise(tc = n(),
              fn = sum(MistakeType == "false_negative")) %>%
    filter(!is.na(CatchTrialType)) %>%
    filter(fn/tc > gt_prop) %>%
    pull(WorkerId)
}

# Dots filter after marking mistakes
get_excluded.fp_per_block <- function(df,
                                      gt_prop = 0.3,
                                      allow_slow_presses = TRUE,
                                      ...) {
  qs <- enquos(...)
  df %>%
    filter(grepl("exposure", PartOfExp)) %>%
    mark_mistakes() %>%
    filter(!!!qs) %>%
    filter(is.na(CatchTrialType)) %>%
    group_by(WorkerId, PartOfExp) %>%
    summarise(fp = sum(MistakeType == "false_positive", na.rm = T),
              n = n()) %>%
    filter(fp/n > gt_prop) %>%
    pull(WorkerId)
}










#' Mark catch trial mistakes
#'
#' @description
#'
#' This takes in a data frame and marks false negative and false positive catch trials, by default accepting 'slow' button presses
#'
#' @section Slow button presses:
#'
#' When `slow_presses` is `TRUE`, we do the following: for catch trials, we simply ask whether space was pressed on the current or next trial-=-if yes, it's correct; for non-catch trials, we ask whether space was not pressed or if space was pressed and the previous trial was a catch trial---if yes, it's correct; everything else is a mistake.
#' @param df a data frame
#' @param slow_presses a boolean
#' @export
mark_mistakes <- function(df, slow_presses=TRUE) {
  slow_num <- if (slow_presses) 1 else 0

  if (!all(c("CatchTrialType", "TrialInExp") %in% names(df))) {
    cleaned_df <- df %>%
      clean_up_data()
  } else {
    cleaned_df <- df
  }
  stopifnot(all(cleaned_df$KeyPressed >= 0))

  cleaned_df %>%
    filter(grepl("exposure", PartOfExp)) %>%
    mutate(Space = KeyPressed == 32)  %>%
    select(WorkerId, PartOfExp, IsCatch, CatchTrialType, Space, TrialInExp) %>%
    arrange(WorkerId, TrialInExp) %>%
    group_by(WorkerId, PartOfExp) %>%
    mutate(PrevCatch = lead(IsCatch, slow_num) == "Y",
           NextSpace = lag(Space, slow_num)) %>%
    mutate_at(vars(PrevCatch, NextSpace), ~ifelse(is.na(.), FALSE, .)) %>%
    mutate(Good = case_when(IsCatch == "Y" ~ Space | NextSpace,
                            TRUE ~ (Space & PrevCatch) | !Space)) %>%
    mutate(MistakeType = case_when(Good == TRUE ~"correct",
                                   IsCatch=="Y" ~ "false_negative",
                                   TRUE ~ "false_positive")) %>%
    ungroup() %>%
    select(WorkerId, CatchTrialType, PartOfExp, TrialInExp, MistakeType) %>%
    # Back into original order
    right_join(cleaned_df, by=c("WorkerId", "CatchTrialType", "PartOfExp", "TrialInExp")) %>%
    select(WorkerId, CatchTrialType, PartOfExp, TrialInExp, MistakeType)
}



# # Dots filter before final by-worker summing of mistakes
get_excluded.more_than_n_mistakes <- function(df, max_mistakes,
                                              allow_slow_presses = TRUE, ...) {
  qs <- enquos(...)
  df %>%
    mark_mistakes(slow_presses = allow_slow_presses) %>%
    filter(!(MistakeType == "correct")) %>%
    filter(!!!qs) %>%
    group_by(WorkerId) %>%
    summarise(mistakes = n()) %>%
    filter(mistakes > max_mistakes) %>%
    pluck("WorkerId")
}

# # Any non-catch trial immediately following a catch trial is treated as being an extension of that previous catch trial. If a subject presses the spacebar one or both of these trials, it is counted as a single true positive. Not pressing the spacebar on _either_ of these trials counts as a false negative. If two catch trials are back-to-back, then pressing the spacebar only on the second catch trial does not count as a true positive for the first catch trial, so a sequence of trials of `Catch - Catch - Noncatch` with a sequence of spacebar presses of `None - Press - Press` will be counted as one false negative and one true positive.
# get_excluded.more_than_n_mistakes <- function(df, max_mistakes,
#                                               allow_slow_presses = FALSE, ...) {
#   qs <- enquos(...)
#   lag_num <- ifelse(allow_slow_presses, 1, 0)
#
#   df %>%
#     clean_up_data() %>%
#     filter(grepl("exposure", PartOfExp)) %>%
#     mutate(Space = KeyPressed == 32) %>%
#     select(WorkerId, PartOfExp, IsCatch, CatchTrialType, Space, TrialInExp) %>%
#     group_by(WorkerId, IsCatch) %>%
#     mutate(CatchNum = ifelse(IsCatch=="Y", seq_along(IsCatch), 0)) %>%
#     ungroup() %>%
#     select(TrialInExp, CatchNum, Space, everything()) %>%
#     # This lets you count non-catch trials following catch trials as part of the
#     #   same catch trial, basically
#     group_by(WorkerId, PartOfExp) %>%
#     mutate(CatchNum = ifelse(CatchNum==0, lag(CatchNum, lag_num), CatchNum)) %>%
#     mutate(CatchNum = ifelse(is.na(CatchNum), 0, CatchNum)) %>%
#     group_by(WorkerId, CatchNum, PartOfExp) %>%
#     mutate(ba = ifelse(CatchNum==0, sum(Space),
#                        ifelse(any(Space), 0, 1))) %>%
#     summarise(ba = first(ba),
#               CatchTrialType = CatchTrialType[!is.na(CatchTrialType)][1]) %>%
#     group_by(WorkerId, CatchTrialType, PartOfExp) %>%
#     summarise(mistakes = sum(ba)) %>%
#     filter(!!!qs) %>%
#     group_by(WorkerId) %>%
#     summarise(mistakes = sum(mistakes)) %>%
#     filter(mistakes > max_mistakes) %>%
#     pluck("WorkerId")
# }
