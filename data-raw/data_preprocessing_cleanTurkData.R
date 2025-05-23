library(tidyverse)
# require(compiler)
library(stringr)
library(digest)

REMAKE_EXPS <- c(99) # c(4) # only remake experiment 4 when adding new data (0 for norming)


# Unique ID functions
make_unique_id <- function(df) {
  paste0(df$WorkerId, "+", df$hitid)
}

give_unique_id <- function(df) {
  df %>%
    mutate(UniqueID = make_unique_id(df))
}

# Vectorizes hash functions
hash_ids <- function(v, algo="md5") {
  as.character(v) %>%
    map_chr(~digest::digest(.x, algo=algo, serialize=FALSE))
}

# This converts a string like 'contrast=dt&noise_cond=snr_10'
#   into data.frame(contrast="d", noise_cond="snr_10")
url_params_to_df <- function(s) {
  diff_things <- s %>%
    stringr::str_split("&") %>%
    map(~stringr::str_split(.x, "=")) %>%
    map_dfr(function(xx) {
      map_dfc(xx, ~tibble(x=.x[[2]]) %>% `names<-`(.x[[1]]))
    })
}

# Like `url_params_to_df()` above but faster, since it
#   assumes each UniqueID only has one url parameter
separate_url_params <- function(df, urlpcol=UrlParams) {
  q <- enquo(urlpcol)
  temp_df <- df %>%
    group_by(UniqueID) %>%
    summarise(urlparams = first(!!q)) %>%
    mutate(da = stringr::str_split(urlparams, "&") %>%
             map(~stringr::str_split(.x, "=")),
           da = map(da, function(xx) {
                       map_dfc(xx, ~tibble(x=.x[[2]]) %>% `names<-`(.x[[1]]))
                     })) %>%
    select(-urlparams) %>%
    tidyr::unnest(da)

  overridden_names <- names(temp_df) %>%
    keep(~. %in% names(df) & . != "UniqueID")

  if (length(overridden_names) > 0) {
    stop(paste("Url param names:",
               paste(overridden_names, collapse=", "),
               "match names in the pre-existing data frame and will cause errors"))
  }
  df %>%
    left_join(temp_df, by="UniqueID")
}

# Converts the messy csv mturk columns into dfs
csv_junk_to_df <- function(csv_col, unique_ids, nms=NULL) {
  df <- csv_col %>%
    as.character() %>%
    strsplit(";") %>%
    map(~strsplit(.x, ",")) %>%
    modify_depth(2, ~as_tibble(t(.x))) %>%
    map(bind_rows) %>%
    map2_dfr(unique_ids,
             ~mutate(.x, temp_col_name=.y))
  if (!is.null(nms)) names(df) <- nms
  df
}


## ------------------------------------- ##
## Function for making a df to check workers
## ------------------------------------- ##

clean_preprocess_data_norming1 <- function(df, merge=TRUE) {

  d <- df %>%
    rename(WorkerId = "workerid") %>%
    mutate(WorkerId = hash_ids(WorkerId)) %>%
    give_unique_id()

  col_names <- c("PartOfExp", "Trial", "Filename", "Word",
                 "KeyPressed", "Response", "RTStart", "RTEnd",
                 "RT", "WorkerId", "Condition", "List", "UrlParams",
                 "LoadTime", "UniqueID")

  d.test <- csv_junk_to_df(d$Answer.testResp, d$UniqueID, col_names) %>%
    # Make DANG sure to get rid of workerId
    mutate(UrlParams = gsub("&workerId=[0-9A-Za-z]+","", UrlParams)) %>%
    # Slightly hacky, needs assignmentId to be after the url params we care about
    mutate(UrlParams = gsub("&assignmentId.*","", UrlParams))

  # Slow but whatever!
  url_params_df <- url_params_to_df(d.test$UrlParams)

  d.all <- d.test %>%
    select(-UrlParams) %>%
    bind_cols(url_params_df) %>%

    # Fix the WorkerIds again
    mutate(WorkerId = hash_ids(WorkerId)) %>%

    # Make variables the right types
    mutate_at(vars(Trial, KeyPressed, RTStart, RTEnd, RT),
              as.numeric) %>%

    # Get f0 and VOT from filename
    mutate(f0 = stringr::str_extract(Filename, "(?<=f0_)[0-9]+") %>%
             as.numeric(),
           VOT = stringr::str_extract(Filename, "(?<=vot_)[0-9]+") %>%
             as.numeric())

  d.survey <- clean_preprocess_survey_data_norming1(d)

  if (merge) {
    d.survey$WorkerId = NULL
    d.survey$Condition = NULL
    d.survey$List = NULL
    d.both <- dplyr::left_join(d.all, d.survey, by="UniqueID")
    return(d.both)
  }

  return(list(d.all, d.survey))
}

clean_preprocess_data_norming2 <- function(df, merge=TRUE) {

  d <- df %>%
    rename(WorkerId = "workerid") %>%
    mutate(WorkerId = hash_ids(WorkerId)) %>%
    give_unique_id() %>%
    select(-Answer.practiceResp) # We don't need practice data

  col_names <- c("PartOfExp", "Trial", "Filename", "Word",
                 "KeyPressed", "Response", "IsCatch",
                 "CatchMethod", "CorrectResponse", "RTStart", "RTEnd",
                 "RT", "WorkerId", "Condition", "List", "UrlParams",
                 "LoadTime", "UniqueID")

  d.test <- csv_junk_to_df(d$Answer.testResp, d$UniqueID, col_names) %>%
    # Make DANG sure to get rid of workerId
    mutate(UrlParams = gsub("&workerId=[0-9A-Za-z]+","", UrlParams)) %>%
    # Slightly hacky, needs assignmentId to be after the url params we care about
    mutate(UrlParams = gsub("&assignmentId.*","", UrlParams))

  # Slow but whatever!
  url_params_df <- url_params_to_df(d.test$UrlParams)

  d.all <- d.test %>%
    select(-UrlParams) %>%
    bind_cols(url_params_df) %>%

    # Fix the WorkerIds again
    mutate(WorkerId = hash_ids(WorkerId)) %>%

    # Make variables the right types
    mutate_at(vars(Trial, KeyPressed, RTStart, RTEnd, RT),
              as.numeric) %>%

    # Get f0 and VOT from filename
    mutate(f0 = stringr::str_extract(Filename, "(?<=f0_)[0-9]+") %>%
             as.numeric(),
           VOT = stringr::str_extract(Filename, "(?<=vot_)[0-9]+") %>%
             as.numeric())

  d.survey <- clean_preprocess_survey_data_norming1(d)

  if (merge) {
    d.survey$WorkerId = NULL
    d.survey$Condition = NULL
    d.survey$List = NULL
    d.both <- dplyr::left_join(d.all, d.survey, by="UniqueID")
    return(d.both)
  }

  return(list(d.all, d.survey))
}


clean_preprocess_survey_data_norming1 <- function(d) {
  d.survey <- d %>%
    select(WorkerId,
           hitid, hittypeid, creationtime, assignmentid, assignmentaccepttime, assignmentsubmittime,
           contains("Answer."), UniqueID) %>%
    select(-contains("Resp"))

  d.survey %>%
    dplyr::rename(AudioPrice = Answer.audio_price,
                  AudioType = Answer.audio_type,
                  AudioStall = Answer.audio_stall,
                  Comments = Answer.comments,
                  LgBackground = Answer.language_background,
                  LgBackgroundFree = Answer.language_background_free,
                  Condition = Answer.condition,
                  List = Answer.list)
  return(d.survey)
}


clean_preprocess_data_exp1 <- function(df, merge=TRUE,
                                       exposure_blocks=1:3,
                                       test_blocks=1:3) {

  d <- df %>%
    rename(WorkerId = "workerid") %>%
    mutate(WorkerId = hash_ids(WorkerId)) %>%
    give_unique_id() #%>%
    # select(-Answer.practiceResp) # We don't need practice data... do we?

  col_names <- c("PartOfExp", "Trial", "Filename", "Word",
                 "KeyPressed", "Response", "IsCatch",
                 "CatchMethod", "ResponseFailure", "RTStart", "RTEnd",
                 "RT", "WorkerId", "Condition", "List", "UrlParams",
                 "LoadTime", "UniqueID")

  cleaner_fn <- . %>%
    # Make DANG sure to get rid of workerId
    mutate(UrlParams = gsub("&workerId=[0-9A-Za-z]+","", UrlParams)) %>%
    # Slightly hacky, needs assignmentId to be after the url params we care about
    mutate(UrlParams = gsub("&assignmentId.*","", UrlParams))

  d.exposure <- map_dfr(
    exposure_blocks,
    function(i) {
      colname = paste0("Answer.exposure", i, "Resp")
      csv_junk_to_df(d[[colname]], d$UniqueID, col_names) %>%
        cleaner_fn %>%
        mutate(ExpBlockNum = i)
    }
  )
  d.normtest <- map_dfr(
    test_blocks,
    function(i) {
      colname = paste0("Answer.normaltest", i, "Resp")
      csv_junk_to_df(d[[colname]], d$UniqueID, col_names) %>%
        cleaner_fn %>%
        mutate(TestBlockNum = i)
    }
  )
  d.gridtest <- csv_junk_to_df(d$Answer.gridtestResp, d$UniqueID, col_names) %>%
    cleaner_fn %>%
    mutate(TestBlockNum = max(test_blocks)+1)

  d.all <- suppressWarnings(bind_rows(d.exposure, d.normtest, d.gridtest)) %>%
    separate_url_params(UrlParams) %>%
    select(-UrlParams) %>%

    # Fix the WorkerIds again
    mutate(WorkerId = hash_ids(WorkerId)) %>%

    # Make variables the right types
    mutate_at(vars(Trial, KeyPressed, RTStart, RTEnd, RT, LoadTime),
              as.numeric) %>%

    # Get f0 and VOT from filename
    mutate(f0 = stringr::str_extract(Filename, "(?<=f0_)[0-9]+") %>%
             as.numeric(),
           VOT = stringr::str_extract(Filename, "(?<=vot_)[0-9]+") %>%
             as.numeric())
  # Uses the same survey as the norming experiments
  d.survey <- clean_preprocess_survey_data_norming1(d)

  if (merge) {
    d.survey$WorkerId = NULL
    d.survey$Condition = NULL
    d.survey$List = NULL
    d.both <- dplyr::left_join(d.all, d.survey, by="UniqueID")
    return(d.both)
  }

  return(list(d.all, d.survey))
}


## ------------------------------------- ##
## Experiment 4 functions
## ------------------------------------- ##

assess_surveyblock_responses <- function(responses) {
  bad_answer_dictionary <- list(
    "audio_stall"  = c("yes", "some", "NORESP"),
    "audio_type"   = c("computer speakers", "external", "NORESP"),
    "bilingual"    = c("yes", "NORESP"),
    "location"     = c("no", "NORESP"),
    "will_continue" = c("no", "NORESP")
  )
  bad_answers <- responses %>%
    pmap_lgl(function(QID, QResp, UniqueID, ...) {
      is.na(QResp) ||
        QResp %in% bad_answer_dictionary[[QID]] ||
        QResp == ""
    })
  bad_answers
}

process_surveyblock <- function(d) {
  col_names <- c("PartOfExp", "Trial", "QID", "Nothing",
                 "QResp", "RTStart", "RTEnd",
                 "RT", "WorkerId", "Condition", "List", "UrlParams",
                 "UniqueID")

  responses <- d %>%
    filter(!is.na(Answer.continuingblockResp),
           Answer.continuingblockResp != "") %>%
    {csv_junk_to_df(.$Answer.continuingblockResp,
                    .$UniqueID, col_names)} %>%
    bind_rows(
      csv_junk_to_df(d$Answer.surveyblockResp, d$UniqueID, col_names)
    ) %>%
    select(-Nothing, -UrlParams) %>%
    mutate(WorkerId = hash_ids(WorkerId))

  responses %>%
    mutate(BadAnswer = assess_surveyblock_responses(responses))
}

process_additional_vars <- function(col, unique_ids,
                                    def_col="bad_survey") {
  proc_row <- function(l) {
    # Hack...
    if (length(l)==0)
      return(setNames(tibble(NA), def_col))

    map_dfc(l, function(x) {
      stopifnot(x[[2]] %in% c(NA, "true", "false"))
      v <- if (x[[2]]=="true") TRUE else FALSE
      setNames(tibble(v), x[[1]])
    })
  }
  data <- col %>%
    as.character() %>%
    strsplit(";") %>%
    map_dfr(~proc_row(strsplit(.x, "\\."))) %>%
    select(-too_slow) %>%  # I didn't end up implementing this check
    select(-wants_to_stop) # I messed this one up, so it doesn't work
  # Because if they did everything right, there will be no errors
  all_nas_index <- data %>%
    rowwise() %>%
    # backwards compatability hack rather than:
    #   `summarise(m=all(is.na(c_across()))) %>%`
    summarise(m=is.na(bad_survey) &
                is.na(failed_dprime) # &
                # is.na(too_slow) &
                # is.na(wants_to_stop)
              ) %>%
    pluck("m") %>% which()
  # make them 'good'
  good_col <- tibble(bad_survey=F,
                     failed_dprime=F
                     # too_slow=F,
                     # wants_to_stop=F
                     )
  data[all_nas_index,] <- good_col
  data <- data %>%
    mutate(did_pass = rowSums(data) == 0)
  stopifnot(nrow(data) == length(unique_ids))

  data %>% mutate(UniqueID=unique_ids)
}

save_bonuses <- function(..., saved_file="paid_bonuses.csv",
                         temp_file = "unpaid_bonuses.csv") {
  saved_df <- read_csv(saved_file)
  all_df <- bind_rows(...)
  unpaid <- all_df %>%
    filter(!(worker %in% saved_df$worker))
  print("___________________________")
  print(unpaid)
  print(paste(nrow(unpaid), "/", n_distinct(unpaid$worker), "workers have not been paid: ",
              paste0(unique(unpaid$worker),collapse=", "), ". Press a button to continue"))
  unpaid %>%
    group_by(worker) %>%
    filter(assignment == first(assignment)) %>%
    ungroup() %>%
    as.data.frame() %>%
    write_csv(temp_file)
}

# A total hack
make_bonus_df <- function(df,
                          exposure_blocks=1:4,
                          test_blocks=1:4) {
  d <- df %>%
    rename(WorkerId = "workerid") %>%
    mutate(WorkerId = hash_ids(WorkerId)) %>%
    give_unique_id()

  col_names <- c("PartOfExp", "Trial", "Filename", "Word",
                 "KeyPressed", "Response", "IsCatch",
                 "CatchMethod", "ResponseFailure", "RTStart", "RTEnd",
                 "RT", "WorkerId", "Condition", "List", "UrlParams",
                 "LoadTime", "UniqueID")

  cleaner_fn <- . %>%
    # Make DANG sure to get rid of workerId
    mutate(UrlParams = gsub("&workerId=[0-9A-Za-z]+","", UrlParams)) %>%
    # Slightly hacky, needs assignmentId to be after the url params we care about
    mutate(UrlParams = gsub("&assignmentId.*","", UrlParams))

  d.exposure <- map_dfr(
    exposure_blocks,
    function(i) {
      colname <- paste0("Answer.exposure", i, "Resp")
      temp_d <- d %>%
        filter(!is.na(d[[colname]]),
               d[[colname]] != "")
      csv_junk_to_df(temp_d[[colname]], temp_d$UniqueID, col_names) %>%
        cleaner_fn() %>%
        mutate(ExpBlockNum = i)
    })

  df %>%
    distinct(workerid, assignmentid) %>%
    transmute(worker=workerid, assignment=assignmentid) %>%
    filter(worker %in% d.exposure$WorkerId)
}


clean_preprocess_data_exp4 <- function(df,
                                       exposure_blocks=1:4,
                                       test_blocks=1:4) {

  d <- df %>%
    rename(WorkerId = "workerid") %>%
    mutate(WorkerId = hash_ids(WorkerId)) %>%
    give_unique_id() %>%
    select(-Answer.list)

  col_names <- c("PartOfExp", "Trial", "Filename", "Word",
                 "KeyPressed", "Response", "IsCatch",
                 "CatchMethod", "ResponseFailure", "RTStart", "RTEnd",
                 "RT", "WorkerId", "Condition", "List", "UrlParams",
                 "LoadTime", "UniqueID")

  cleaner_fn <- . %>%
    # Make DANG sure to get rid of workerId
    mutate(UrlParams = gsub("&workerId=[0-9A-Za-z]+","", UrlParams)) %>%
    # Slightly hacky, needs assignmentId to be after the url params we care about
    mutate(UrlParams = gsub("&assignmentId.*","", UrlParams))

  # Get the GK exposure; useful for getting urlparams, etc.
  d.gk_exposure <- csv_junk_to_df(d$Answer.gk_exposureResp,
                                  d$UniqueID, col_names) %>%
    cleaner_fn()

  d.exposure <- map_dfr(
    exposure_blocks,
    function(i) {
      colname <- paste0("Answer.exposure", i, "Resp")
      temp_d <- d %>%
        filter(!is.na(d[[colname]]),
               d[[colname]] != "")
      csv_junk_to_df(temp_d[[colname]], temp_d$UniqueID, col_names) %>%
        cleaner_fn() %>%
        mutate(ExpBlockNum = i)
    })

  d.normtest <- map_dfr(
    test_blocks,
    function(i) {
      colname <- paste0("Answer.normaltest", i, "Resp")
      temp_d <- d %>%
        filter(!is.na(d[[colname]]),
               d[[colname]] != "")
      csv_junk_to_df(temp_d[[colname]], temp_d$UniqueID, col_names) %>%
        cleaner_fn() %>%
        mutate(TestBlockNum = i)
    })

  d.gridtest <- d %>%
    filter(!is.na(Answer.gridtestResp),
           Answer.gridtestResp != "") %>%
           {csv_junk_to_df(.$Answer.gridtestResp, .$UniqueID, col_names)} %>%
    cleaner_fn() %>%
    mutate(TestBlockNum = max(test_blocks)+1)

  d.all <- suppressWarnings(
    bind_rows(
      d.gk_exposure,
      d.exposure, d.normtest, d.gridtest
    )) %>%
    # Making sure that everone has only one urlparam
    {
      extraUrlParamsPerWorker <- group_by(., UniqueID) %>%
        summarise(n=n_distinct(UrlParams)) %>%
        filter(n>1 | is.na(n)) %>%
        nrow()
      stopifnot(extraUrlParamsPerWorker==0)
      .
    } %>%
    separate_url_params(UrlParams) %>%
    select(-UrlParams) %>%

    # Fix the WorkerIds again
    mutate(WorkerId = hash_ids(WorkerId)) %>%

    # Make variables the right types
    mutate_at(vars(Trial, KeyPressed, RTStart, RTEnd, RT, LoadTime),
              as.numeric) %>%

    # Get f0 and VOT from filename
    mutate(f0 = stringr::str_extract(Filename, "(?<=f0_)[0-9]+") %>%
             as.numeric(),
           VOT = stringr::str_extract(Filename, "(?<=vot_)[0-9]+") %>%
             as.numeric())
  # Uses the same survey as the norming experiments
  d.survey <- clean_preprocess_survey_data_exp4(d)
  # This is different than the final survey
  d.first_survey <- process_surveyblock(d)

  return(list(
    resp=d.all,
    survey=d.survey,
    first_survey=d.first_survey
  ))
}

clean_preprocess_survey_data_exp4 <- function(d) {
  # Get the additional info
  man_vars <- d$Answer.errors %>%
    process_additional_vars(d$UniqueID)

  d.survey <- d %>%
    select(WorkerId,
           hitid, hittypeid, creationtime, assignmentid,
           assignmentaccepttime, assignmentsubmittime,
           contains("Answer."), UniqueID, Run) %>%
    select(-contains("Resp")) %>%
    dplyr::rename(AudioPrice = Answer.audio_price,
                  AudioType = Answer.audio_type,
                  AudioStall = Answer.audio_stall,
                  Comments = Answer.comments,
                  LgBackground = Answer.language_background,
                  LgBackgroundFree = Answer.language_background_free,
                  Condition = Answer.condition)

  left_join(man_vars, d.survey, by="UniqueID")
}



## ------------------------------------- ##
## Function for making a df to check workers
## ------------------------------------- ##

make_worker_id_compendium <- function(df_list) {

  df_list_nested <- purrr::map(
    df_list,
    function(x) {
      nested_df <- x %>%
        dplyr::group_by(WorkerId, UniqueID, hitid,
                        assignmentaccepttime, assignmentsubmittime,
                        Condition, List, Run) %>%
        tidyr::nest()
      return(nested_df)
    })
  df.all <- purrr::reduce(df_list_nested, rbind) %>%
    distinct(UniqueID, .keep_all=TRUE)

  if (FALSE %in% (make_unique_id(df.all) == df.all$UniqueID)) {
    stop("UniqueIDs and WorkerIds + hitids do not match up! (in 'make_worker_id_compendium')")
  }

  d.repeatWorker <- df.all %>%
    dplyr::group_by(WorkerId) %>%
    dplyr::mutate(n=n()) %>%
    subset(n > 1) %>%
    select(WorkerId,hitid,Condition,List,assignmentaccepttime,assignmentsubmittime) %>%
    as.data.frame()
  if (nrow(d.repeatWorker) > 0) {
    warning_message=paste0("There are ",length(unique(d.repeatWorker$WorkerId))," worker(s) who took this experiment more than once!")
    warning(warning_message)
    a <-capture.output(print(d.repeatWorker))
    c <- paste(a, "\n", sep="")
    cat("These workers took the experiment more than once:\n", c, "\n")
  }
  return(df.all)
}



## --------------------------------- ##
## read in First norming experiment
## --------------------------------- ##
if (0 %in% REMAKE_EXPS) {

  norming_v1 <- read.csv("Norming1/norming1_3-18-20.csv", sep = "\t")
  norming_v1.both <- clean_preprocess_data_norming1(norming_v1) %>%
    mutate(Run = "NormingV1")

  d.norming1 <- norming_v1.both
  usethis::use_data(d.norming1, overwrite=TRUE)
} else {
  print("Loading Norming 1 data from file (not redoing)")
  load("../data/d.norming1.rda")
}

## --------------------------------- ##
## read in Second norming experiment
## --------------------------------- ##
if (0 %in% REMAKE_EXPS) {

  norming_v2 <- read.csv("Norming2/norming2_3-30-20.csv", sep = "\t")
  norming_v2.both <- clean_preprocess_data_norming2(norming_v2) %>%
    mutate(Run = "NormingV2")

  d.norming2 <- norming_v2.both
  usethis::use_data(d.norming2, overwrite=TRUE)
} else {
  print("Loading Norming 2 data from file (not redoing)")
  load("../data/d.norming2.rda")
}

## --------------------------------- ##
## read in Exp1 During Data
##  (bad aud catch, a few issues)
## --------------------------------- ##
if (1 %in% REMAKE_EXPS) {

  # Didn't warn people about responding during listening task
  exp1_pretest_v1.both <- read.csv("Exp1During/pretest_5-27-20.csv", sep = "\t") %>%
    clean_preprocess_data_exp1() %>%
    mutate(Run = "Exp1BadPilotV1")
  # Messed up the reliable f0 condition here...
  exp1_pretest_v2.both <- read.csv("Exp1During/pretest2_5-27-20.csv", sep = "\t") %>%
    clean_preprocess_data_exp1()
  ex1_pilot <- exp1_pretest_v2.both %>%
    filter(Condition == "VOT") %>%
    mutate(Run = "Exp1GoodPilot")
  d.bad_ex1_pilot <- exp1_pretest_v2.both %>%
    filter(Condition == "f0") %>%
    mutate(Run = "Exp1BadPilotV2") %>%
    {
      suppressWarnings(bind_rows(., exp1_pretest_v1.both))
    }
  # The rest of the experiment
  exp1_rest.both <- read.csv("Exp1During/exp1_5-28-20.csv", sep = "\t") %>%
    clean_preprocess_data_exp1() %>%
    mutate(Run = "Exp1")
  exp1_fillup <- read.csv("Exp1During/exp1_fillup_6-19-20.csv", sep = "\t") %>%
    clean_preprocess_data_exp1() %>%
    mutate(Run = "Exp1TopUp")

  d.ex1_raw <- suppressWarnings(bind_rows(exp1_rest.both, ex1_pilot, exp1_fillup))

  usethis::use_data(d.ex1_raw, overwrite=TRUE)
  usethis::use_data(d.bad_ex1_pilot, overwrite=TRUE)
} else {
  print("Loading Exp 1 data from file (not redoing)")
  load("../data/d.ex1_raw.rda")
  load("../data/d.bad_ex1_pilot.rda")
}


## --------------------------------- ##
## read in Exp2 Data
## --------------------------------- ##
if (2 %in% REMAKE_EXPS) {

  exp2_pretest_v1 <- read.csv("Exp2/pretest_6-03-20.csv", sep = "\t") %>%
    clean_preprocess_data_exp1(exposure_blocks=1,test_blocks=1:2) %>% # same cleaner
    mutate(Run = "Exp2PilotV1") %>%
    mutate(Answer.rsrb.age = as.factor(as.character(Answer.rsrb.age)))
  exp2_v1 <- read.csv("Exp2/exp2_6-04-20.csv", sep = "\t") %>%
    clean_preprocess_data_exp1(exposure_blocks=1, test_blocks=1:2) %>% # same cleaner
    mutate(Run = "Exp2V1")
  exp2_v2 <- read.csv("Exp2/exp2_topup_6-16-20.csv", sep = "\t") %>%
    clean_preprocess_data_exp1(exposure_blocks=1, test_blocks=1:2) %>% # same cleaner
    mutate(Run = "Exp2V1TopUp") %>%
    mutate(Answer.rsrb.age = as.factor(as.character(Answer.rsrb.age)))

  d.ex2_raw <- suppressWarnings(bind_rows(exp2_pretest_v1, exp2_v1, exp2_v2))
  usethis::use_data(d.ex2_raw, overwrite=TRUE)

} else {
  print("Loading Exp 2 data from file (not redoing)")
  load("../data/d.ex2_raw.rda")
}


## --------------------------------- ##
## read in Exp3 Data
## --------------------------------- ##
if (3 %in% REMAKE_EXPS) {

  exp3_pretest_v1 <- read.csv("Exp3/NoDelayedPretest_8-10-20.csv", sep = "\t") %>%
    clean_preprocess_data_exp1(exposure_blocks=1:4,test_blocks=1:4) %>% # same cleaner
    mutate(Run = "Exp3PilotV1") %>%
    mutate(Answer.audio_price = as.character(Answer.audio_price), Answer.rsrb.age = as.factor(as.character(Answer.rsrb.age)))

  exp3_v1 <- read.csv("Exp3/NoDelayed_8-11-20.csv", sep = "\t") %>%
    clean_preprocess_data_exp1(exposure_blocks=1:4, test_blocks=1:4) %>% # same cleaner
    mutate(Run = "Exp3V1") %>%
    mutate(Answer.audio_price = as.character(Answer.audio_price), Answer.rsrb.age = as.factor(as.character(Answer.rsrb.age)))

  d.ex3_raw <- suppressWarnings(bind_rows(exp3_pretest_v1, exp3_v1))
  usethis::use_data(d.ex3_raw, overwrite=TRUE)
} else {
  print("Loading Exp 3 data from file (not redoing)")
  load("../data/d.ex3_raw.rda")
}

## --------------------------------- ##
## read in Exp4 Data
## --------------------------------- ##
if (4 %in% REMAKE_EXPS) {
  print("Reading in Exp4 files")
  raw_exp4_data <- bind_rows(
    "Exp4NoDelayedPilot" = read.csv("Exp4/NoDelayed_pilot.csv", sep = "\t"),
    "Exp4NoDelayedRunV1" = read.csv("Exp4/exp4_first_post_pilot_06-11.csv", sep = "\t"),
    "Exp4NoDelayedRunV2" = read.csv("Exp4/second_run_6-20.csv", sep="\t"), # $1.5 fee + $7 bonus
    "Exp4NoDelayedRunV3" = read.csv("Exp4/third_run_6-22.csv", sep="\t"),
    "Exp4NoDelayedRunV4" = read.csv("Exp4/fourth_run_7-6.csv", sep="\t"),
    "Exp4NoDelayedRunV5" = read.csv("Exp4/fifth_run_7-8.csv",  sep="\t"),
    "Exp4NoDelayedRunV6" = read.csv("Exp4/sixth_run_7-11.csv", sep="\t"),
    "Exp4NoDelayedRunV8" = read.csv("Exp4/eigth_run_9-2.csv", sep="\t"),

    "Exp4DelayedPilot"   = read.csv("Exp4/exp4_delayed_pilot1_9-10.csv", sep="\t"),
    "Exp4DelayedRunV9"   = read.csv("Exp4/exp4_delayed_run1_9-21.csv", sep="\t"),
    "Exp4DelayedRunV11"   = read.csv("Exp4/exp4_delayed_run3_9-27.csv", sep="\t"),
    .id = "Run") %>%
    mutate(Answer.audio_price = as.character(Answer.audio_price),
           Answer.rsrb.age    = as.character(Answer.rsrb.age)) %>%
    # Because some MF found out how to type a sentence in the audio price text box....
    bind_rows(.,
              read.csv("Exp4/seventh_run_i_guess_9-2.csv", sep="\t") %>%
                mutate(Run="Exp4NoDelayedRunV7") %>%
                mutate(Answer.audio_price = as.character(Answer.audio_price),
                       Answer.rsrb.age    = as.character(Answer.rsrb.age)),
              read.csv("Exp4/exp4_delayed_run2_9-22.csv", sep="\t")%>%
                mutate(Run="Exp4DelayedRunV10") %>%
                mutate(Answer.audio_price = as.character(Answer.audio_price),
                       Answer.rsrb.age    = as.character(Answer.rsrb.age))
              )
  print("Processing Exp 4 files")
  d.ex4_raw  <- raw_exp4_data %>% clean_preprocess_data_exp4()
  print("Making bonus df")
  exp4_bonus <- raw_exp4_data %>% make_bonus_df()

  save_bonuses(exp4_bonus)
  usethis::use_data(d.ex4_raw, overwrite=TRUE)
  print("Done with Exp 4 processing")

} else {
  print("Loading Exp 4 data from file (not redoing)")
  load("../data/d.ex4_raw.rda")
}




## ------------------------------------------------------- ##
## Make a data-frame with all workers for referencing stuff
## ------------------------------------------------------- ##

# Make a list of all workers who have taken any form of this experiment for referencing
d.WorkerReferenceList <- make_worker_id_compendium(
  list(
    d.norming1,
    d.norming2,
    d.ex1_raw,
    d.ex2_raw,
    d.ex3_raw,
    d.bad_ex1_pilot,
    d.ex4_raw$survey %>% mutate(List="") # Since in Exp4, they're no longer dataframes
  ))
usethis::use_data(d.WorkerReferenceList, overwrite=TRUE)



