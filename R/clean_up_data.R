#' Clean up experiment data
#'
#' Right now, applies only to Experiment 1, 2, 3, and 4.
#'
#'
#' @param df a data frame
#' @param subjprefix the prefix you want to use for shortening `WorkerId`s
#' @export
clean_up_data <- function(.data, ...) {
  UseMethod("clean_up_data")
}

#' @rdname clean_up_data
#' @export
# The normal version
clean_up_data.data.frame <- function(.data, subjprefix = "Exp-") {
  df <- .data # 2lazy2change
  # Basically, a purely RTStart-based Trial ordering system won't always work, since a few people have weird timestamps.
  # However, the order that the data comes in to begin with is basically correct, although the BLOCK order is not.
  # So, annoyingly, you need a hybrid of the two to automatically assign trials
  df <- df %>%
    group_by(WorkerId, PartOfExp) %>%
    mutate(NativeTrial = seq_along(RTStart)) %>%
    ungroup()

  # Shortens WorkerIds
  pre_trial_check <- df %>%
    distinct(WorkerId) %>%
    mutate(Subj = paste0(subjprefix, seq_along(WorkerId))) %>%
    right_join(df, by="WorkerId") %>%
    # Gives TrialinExp
    group_by(Subj) %>%
    arrange(RTStart, .by_group=TRUE) %>%
    mutate(TrialInExp = seq_along(Trial))

  checker <- pre_trial_check %>%
    group_by(WorkerId, PartOfExp) %>%
    mutate(RTbasedTrial = TrialInExp - min(TrialInExp) + 1) %>%
    filter(RTbasedTrial != NativeTrial)

  if (nrow(checker) > 0) {
    checker %>%
      group_by(Subj) %>%
      summarise(n = n(),
                PoE = paste(unique(PartOfExp), collapse = ", "),
                Trials = paste(paste0(NativeTrial, " -> ", TrialInExp),
                               collapse=", ")) %>%
      pmap_chr(function(Subj, n, PoE, Trials, ...) {
        paste0("Subj `", Subj, "` has ", n, " trial mismatches in ",
               PoE, ": ", stringr::str_trunc(Trials, 40))
      }) %>%
      paste(collapse="\n") %>%
      paste("\n(Native -> RTBased)") %>%
      warning(immediate. = TRUE)
  }

  # Assumes the median of subjs will have right first trial # for each block
  min_trial_in_block <- pre_trial_check %>%
    group_by(PartOfExp, Subj) %>%
    summarise(FirstTrialInBlock = min(TrialInExp)) %>%
    summarise(FirstTrialInBlock = median(FirstTrialInBlock))

  pre_trial_check %>%
    left_join(min_trial_in_block) %>%
    group_by(WorkerId, PartOfExp) %>%
    mutate(TrialInExp = NativeTrial - min(NativeTrial) + FirstTrialInBlock) %>%
    select(-NativeTrial, - FirstTrialInBlock) %>%
    ungroup() %>%
    # Makes Catch Trial Type
    mutate(CatchTrialType = case_when(IsCatch=="N" ~ NA_character_,
                                      Word == "press space" ~ "vis",
                                      TRUE ~ "aud")) %>%
    # Gets rid of NA KeyPresses
    mutate(KeyPressed = ifelse(is.na(KeyPressed), 0, KeyPressed)) %>%
    # Put times into human readable formats
    mutate(trialstart = as.POSIXct(RTStart/1000, origin="1970-01-01"),
           trialend   = as.POSIXct(RTEnd/1000,   origin="1970-01-01"),
           accepttime = lubridate::ymd_hms(assignmentaccepttime),
           submittime = lubridate::ymd_hms(assignmentsubmittime),
           LoadTime = as.numeric(LoadTime)) %>%
    # If statement for Norming 2
    {
      if ("subcond" %in% names(.)) {
        # It's confusing and meaningless here
        mutate(., SubCond = subcond) %>%
          select(-Trial, -subcond, -condition)
      } else {
        .
      }
    } %>%
    # Remove unneeded columns
    select(-assignmentsubmittime, -assignmentaccepttime,
           -Answer.location, -List, -hitid, -hittypeid, -creationtime,
           -assignmentid,
           -contains('Answer.condition'), -contains('Answer.list'), -contains('Answer.speaker'))
}


#' @rdname clean_up_data
#'
#' @param remove_gk boolean, to remove the gatekeeper from Exp4
#' @param .w boolean, set to `FALSE` if you don't want a warning
#' @export
# Cleans up the Exp4 results by collapsing into df first
# Turns out I probably didn't need this, but hey

clean_up_data.list <- function(l, ..., remove_gk = TRUE, .w=TRUE) {
  if (.w)
    warning("You're using clean_up_data() on the Exp4 list, btw!")
  collapse_exp4(l, ..., remove_gk = remove_gk) %>%
    clean_up_data(...)
}

#' Turn the Exp4 list results into a data frame
#'
#' The results for Exp4 in the package come as a list.
#' This turns the results into a data frame. Be wary that it joins by 'Condition' by default.
#'
#' @param l the Exp4 list
#' @param \dots unused, but important to make `remove_gk` obligatorily named
#' @param remove_gk a boolean, to remove the gatekeeper blocks in the result
#' @export
collapse_exp4 <- function(l, ..., remove_gk = TRUE) {
  relevant_survey_data <- l$first_survey %>%
    filter(PartOfExp == 'continuingblock') %>%
    filter(map_lgl(QResp, ~!isTRUE(.=='yes'))) %>%
    distinct(WorkerId, UniqueID, choose2stop=TRUE)

  l$survey %>%
    left_join(relevant_survey_data) %>%
  {
    if (remove_gk == TRUE) {
      left_join(., filter(l$resp, !grepl("gk", PartOfExp)))
    } else {
      left_join(., l$resp)
    }
  }
}

