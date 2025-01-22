


#' Run the app
#'
#' @param app_name
#'
#' @return Launchs a shiny test.
#' @export
#'
run_app <- function(app_name = "sing-mpi") {

  SAA::SAA_standalone(app_name = app_name,
                      num_items = list(long_tones = get_config('no_long_tones'),
                                      arrhythmic = get_config('no_melodies'),
                                      rhythmic = get_config('no_melodies')
                                      ),
                       arrhythmic_item_bank = Berkowitz_easy,
                       rhythmic_item_bank = Berkowitz_easy,
                       feedback = FALSE,
                       max_goes = 2L,
                       examples = get_config('no_examples'),
                       melody_length=c(4, 10),
                       SNR_test = TRUE,
                       headphones_test = FALSE,
                       get_user_info = FALSE,
                       requirements_page = FALSE,
                       test_name = "Sing MPI",
                       gold_msi = FALSE,
                       demographics = FALSE,
                       final_results=FALSE,
                       get_range = FALSE,
                       default_range = list(bottom_range = 60, top_range = 72, clef = "auto"),
                       long_tone_paradigm = "call_and_response",
                       volume_meter_on_melody_trials = TRUE,
                       volume_meter_on_melody_trials_type = 'playful',
                       append_trial_block_before = extra_mpi_beginning(),
                       append_trial_block_after = extra_mpi_end(),
                       long_tone_length = 3,
                       allow_SNR_failure = TRUE)

}


extra_mpi_beginning <- function() {
  psychTestR::join(
    sing_hbd_page(),
    sing_all_meine_entchen_page()
  )
}

extra_mpi_end <- function() {

  psychTestR::join(

    sing_brother_john_pages(),

    # Sing your favourite song
    musicassessr::record_audio_page(page_title = "Sing your favourite song!",
                                    page_text = 'When you are ready, click "Record" and sing your favourite song.',
                                    page_label = "sing_fav",
                                    get_answer = musicassessr::get_answer_pyin)

  )

}



sing_brother_john_pages <- function() {

  purrr::pmap(brother_john, function(abs_melody, durations) {
    musicassessr::present_stimuli(stimuli = itembankr::str_mel_to_vector(abs_melody),
                                  durations = itembankr::str_mel_to_vector(durations),
                                  page_title = "Sing the song!",
                                  page_text = 'When you are ready, click "Record" and sing back the song you hear.',
                                  stimuli_type = "midi_notes",
                                  display_modality = "auditory",
                                  page_type = "record_audio_page",
                                  page_label = "sing_bj",
                                  get_answer = musicassessr::get_answer_pyin)
  })
}

sing_hbd_page <- function() {

  musicassessr::present_stimuli(stimuli = itembankr::str_mel_to_vector(hbd$abs_melody),
                                durations = itembankr::str_mel_to_vector(hbd$durations),
                                page_title = "Sing Happy Birthday!",
                                page_text = 'When you are ready, click "Record" and sing back Happy Birthday after you hear it.',
                                stimuli_type = "midi_notes",
                                display_modality = "auditory",
                                page_type = "record_audio_page",
                                page_label = "sing_hbd",
                                get_answer = musicassessr::get_answer_pyin)
}


sing_all_meine_entchen_page <- function() {

  musicassessr::present_stimuli(stimuli = itembankr::str_mel_to_vector(all_meine_entchen$abs_melody),
                                durations = itembankr::str_mel_to_vector(all_meine_entchen$durations),
                                page_title = "Sing The Ugly Ducklings!",
                                page_text = 'When you are ready, click "Record" and sing back The Ugly Ducklings after you hear it.',
                                stimuli_type = "midi_notes",
                                display_modality = "auditory",
                                page_type = "record_audio_page",
                                page_label = "sing_ducklings",
                                get_answer = musicassessr::get_answer_pyin)
}



