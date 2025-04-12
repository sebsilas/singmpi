

#' Run the app
#'
#' @param app_name
#' @param SNR_test
#' @param asynchronous_api_mode
#' @param include_extra
#'
#' @return Launchs a shiny test.
#' @export
#'
run_app <- function(app_name = "sing-mpi",
                    SNR_test = TRUE,
                    asynchronous_api_mode = TRUE,
                    include_extra = TRUE) {

  paradigm <- musicassessr::paradigm()
  trigger_start_of_stimulus_fun <- paradigm$trigger_start_of_stimulus_fun
  trigger_end_of_stimulus_fun <- paradigm$trigger_end_of_stimulus_fun

  SAA::SAA_standalone(app_name = app_name,
                      num_items = list(long_tones = get_config('no_long_tones'),
                                      arrhythmic = get_config('no_melodies'),
                                      rhythmic = get_config('no_melodies')
                                      ),
                       arrhythmic_item_bank = Berkowitz_easy_arrhythmic,
                       rhythmic_item_bank = Berkowitz_easy_rhythmic,
                       feedback = FALSE,
                       get_p_id = TRUE,
                       max_goes = 2L,
                       user_id = if(Sys.getenv("R_CONFIG_ACTIVE") == "prod") 188L else 1L,
                       num_examples = list(long_tones = get_config('no_examples'),
                                           arrhythmic = get_config('no_examples'),
                                           rhythmic = get_config('no_examples') ),
                       melody_length=c(4, 10),
                       SNR_test = SNR_test,
                       headphones_test = FALSE,
                       asynchronous_api_mode = asynchronous_api_mode,
                       get_user_info = FALSE,
                       requirements_page = FALSE,
                       test_name = "Sing MPI",
                       gold_msi = FALSE,
                       experiment_id = if(Sys.getenv("R_CONFIG_ACTIVE") == "prod") 7L else 1L,
                       demographics = FALSE,
                       final_results = FALSE,
                       get_range = FALSE,
                       get_answer_melodic = musicassessr::get_answer_add_trial_and_compute_trial_scores_s3,
                       default_range = list(bottom_range = 60, top_range = 72, clef = "auto"),
                       long_tone_paradigm = "call_and_response",
                       volume_meter_on_melody_trials = TRUE,
                       volume_meter_on_melody_trials_type = 'playful',
                       append_trial_block_before = if(include_extra) extra_mpi_beginning(trigger_start_of_stimulus_fun, trigger_end_of_stimulus_fun, asynchronous_api_mode) else musicassessr::empty_code_block(),
                       append_trial_block_after = if(include_extra) extra_mpi_end(trigger_start_of_stimulus_fun, trigger_end_of_stimulus_fun, asynchronous_api_mode)  else musicassessr::empty_code_block(),
                       long_tone_length = 3,
                       use_presigned_url = FALSE,
                       allow_SNR_failure = TRUE)

}


extra_mpi_beginning <- function(trigger_start_of_stimulus_fun, trigger_end_of_stimulus_fun, asynchronous_api_mode) {
  psychTestR::join(
    sing_hbd_page(trigger_start_of_stimulus_fun, trigger_end_of_stimulus_fun, asynchronous_api_mode),
    sing_all_meine_entchen_page(trigger_start_of_stimulus_fun, trigger_end_of_stimulus_fun, asynchronous_api_mode)
  )
}

extra_mpi_end <- function(trigger_start_of_stimulus_fun, trigger_end_of_stimulus_fun, asynchronous_api_mode) {

  psychTestR::join(

    sing_brother_john_pages(trigger_start_of_stimulus_fun, trigger_end_of_stimulus_fun, asynchronous_api_mode),

    sing_fav_song_page(asynchronous_api_mode = asynchronous_api_mode)

  )

}


sing_fav_song_page <- function(page_label = "sing_fav",
                               asynchronous_api_mode = TRUE) {

  psychTestR::reactive_page(function(state, ...) {

    db_vars <- musicassessr::create_db_vars_template(init_with_time_started = TRUE)

    db_vars$page_label <- page_label
    db_vars$phase <- "test"
    db_vars$feedback <- FALSE
    db_vars$test_id <- 1L
    session_id <- psychTestR::get_global("session_id", state) %>% musicassessr::get_promise_value()
    db_vars$session_id <- session_id
    db_vars$instrument <- "Voice"
    db_vars$module <- psychTestR::get_local(".module", state)
    db_vars$rhythmic <- TRUE
    db_vars$user_id <- psychTestR::get_global("user_id", state)
    db_vars$display_modality <- "auditory"

    # Sing your favourite song
    musicassessr::record_audio_page(page_title = "Sing your favourite song!",
                                    page_text = 'When you are ready, click "Record" and sing your favourite song.',
                                    page_label = page_label,
                                    db_vars = db_vars,
                                    volume_meter = TRUE,
                                    volume_meter_type = 'playful',
                                    get_answer = if(asynchronous_api_mode) musicassessr::get_answer_add_trial_and_compute_trial_scores_s3 else musicassessr::get_answer_pyin)


  })
}


sing_brother_john_pages <- function(trigger_start_of_stimulus_fun,
                                    trigger_end_of_stimulus_fun,
                                    asynchronous_api_mode) {

  brother_john <- brother_john %>%
    dplyr::mutate(idx = dplyr::row_number()) %>%
    purrr::pmap(function(abs_melody, durations, idx) {

      psychTestR::reactive_page(function(state, ...) {

        db_vars <- musicassessr::create_db_vars_template(init_with_time_started = TRUE)

        page_label <- paste0("sing_bj_", idx)
        db_vars$page_label <- page_label
        db_vars$phase <- "test"
        db_vars$feedback <- FALSE
        db_vars$test_id <- 1L
        session_id <- psychTestR::get_global("session_id", state) %>% musicassessr::get_promise_value()
        db_vars$session_id <- session_id
        db_vars$instrument <- "Voice"
        db_vars$module <- psychTestR::get_local(".module", state)
        db_vars$rhythmic <- TRUE
        db_vars$user_id <- psychTestR::get_global("user_id", state)
        db_vars$display_modality <- "auditory"


        musicassessr::present_stimuli(stimuli = itembankr::str_mel_to_vector(abs_melody),
                                      durations = itembankr::str_mel_to_vector(durations),
                                      page_title = "Sing the song!",
                                      page_text = 'When you are ready, click "Play" and sing back the song you hear.',
                                      stimuli_type = "midi_notes",
                                      display_modality = "auditory",
                                      page_type = "record_audio_page",
                                      volume_meter = TRUE,
                                      volume_meter_type = 'playful',
                                      trigger_start_of_stimulus_fun = trigger_start_of_stimulus_fun,
                                      trigger_end_of_stimulus_fun = trigger_end_of_stimulus_fun,
                                      page_label = page_label,
                                      db_vars = db_vars,
                                      get_answer = if(asynchronous_api_mode) musicassessr::get_answer_add_trial_and_compute_trial_scores_s3 else musicassessr::get_answer_pyin)
      })
    })
}

sing_hbd_page <- function(trigger_start_of_stimulus_fun,
                          trigger_end_of_stimulus_fun,
                          asynchronous_api_mode,
                          page_label = "sing_hbd") {


  psychTestR::reactive_page(function(state, ...) {

    db_vars <- musicassessr::create_db_vars_template(init_with_time_started = TRUE)

    db_vars$page_label <- page_label
    db_vars$phase <- "test"
    db_vars$feedback <- FALSE
    db_vars$test_id <- 1L
    session_id <- psychTestR::get_global("session_id", state) %>% musicassessr::get_promise_value()
    db_vars$session_id <- session_id
    db_vars$instrument <- "Voice"
    db_vars$module <- psychTestR::get_local(".module", state)
    db_vars$rhythmic <- TRUE
    db_vars$user_id <- psychTestR::get_global("user_id", state)
    db_vars$display_modality <- "auditory"

    musicassessr::present_stimuli(stimuli = itembankr::str_mel_to_vector(hbd$abs_melody),
                                  durations = itembankr::str_mel_to_vector(hbd$durations),
                                  page_title = "Sing Happy Birthday!",
                                  page_text = 'When you are ready, click "Play" and sing back Happy Birthday after you hear it.',
                                  stimuli_type = "midi_notes",
                                  display_modality = "auditory",
                                  page_type = "record_audio_page",
                                  volume_meter = TRUE,
                                  volume_meter_type = 'playful',
                                  trigger_start_of_stimulus_fun = trigger_start_of_stimulus_fun,
                                  trigger_end_of_stimulus_fun = trigger_end_of_stimulus_fun,
                                  page_label = page_label,
                                  db_vars = db_vars,
                                  get_answer = if(asynchronous_api_mode) musicassessr::get_answer_add_trial_and_compute_trial_scores_s3 else musicassessr::get_answer_pyin )

  })
}


sing_all_meine_entchen_page <- function(trigger_start_of_stimulus_fun,
                                        trigger_end_of_stimulus_fun,
                                        asynchronous_api_mode,
                                        page_label = "sing_ducklings") {

  psychTestR::reactive_page(function(state, ...) {

    db_vars <- musicassessr::create_db_vars_template(init_with_time_started = TRUE)

    db_vars$page_label <- page_label
    db_vars$phase <- "test"
    db_vars$feedback <- FALSE
    db_vars$test_id <- 1L
    session_id <- psychTestR::get_global("session_id", state) %>% musicassessr::get_promise_value()
    db_vars$session_id <- session_id
    db_vars$instrument <- "Voice"
    db_vars$module <- psychTestR::get_local(".module", state)
    db_vars$rhythmic <- TRUE
    db_vars$user_id <- psychTestR::get_global("user_id", state)
    db_vars$display_modality <- "auditory"


    musicassessr::present_stimuli(stimuli = itembankr::str_mel_to_vector(all_meine_entchen$abs_melody),
                                  durations = itembankr::str_mel_to_vector(all_meine_entchen$durations),
                                  page_title = "Sing The Ugly Ducklings!",
                                  page_text = 'When you are ready, click "Play" and sing back The Ugly Ducklings after you hear it.',
                                  stimuli_type = "midi_notes",
                                  display_modality = "auditory",
                                  page_type = "record_audio_page",
                                  volume_meter = TRUE,
                                  volume_meter_type = 'playful',
                                  page_label = "sing_ducklings",
                                  db_vars = db_vars,
                                  trigger_start_of_stimulus_fun = trigger_start_of_stimulus_fun,
                                  trigger_end_of_stimulus_fun = trigger_end_of_stimulus_fun,
                                  get_answer = if(asynchronous_api_mode) musicassessr::get_answer_add_trial_and_compute_trial_scores_s3 else musicassessr::get_answer_pyin)
  })
}



