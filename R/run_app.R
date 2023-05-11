


run_app <- function(app_name = "sing-mpi") {

  SAA::SAA_standalone(app_name = app_name,
                 num_items = list(long_tones = get_config('no_long_tones'),
                                  arrhythmic = get_config('no_melodies'),
                                  rhythmic = get_config('no_melodies')
                                  ),
                 feedback = FALSE,
                 SNR_test = FALSE,
                 get_range = FALSE,
                 musicassessr_aws = get_config('musicassessr_aws'),
                 skip_setup = get_config('skip_setup'),
                 examples = get_config('no_examples'),
                 melody_length=c(4, 10),
                 test_name = "Sing MPI",
                 gold_msi = FALSE,
                 demographics = FALSE,
                 default_range = list(bottom_range = 48, top_range = 60),
                 long_tone_paradigm = "call_and_response",
                 append_trial_block_after = extra_mpi()
                 )

}


extra_mpi <- function() {

  psychTestR::join(

    sing_hbd_page(),

    sing_brother_john_page(),

    # Sing your favourite song
    musicassessr::record_audio_page(page_title = "Sing your favourite song!",
                                    page_text = 'When you are ready, click "Record" and sing your favourite song.')

  )

}



sing_brother_john_page <- function() {
  musicassessr::present_stimuli(stimuli = itembankr::str_mel_to_vector(brother_john$abs_melody),
                                durations = itembankr::str_mel_to_vector(brother_john$durations),
                                page_title = "Sing the song!",
                                page_text = 'When you are ready, click "Record" and sing back the song you hear.',
                                stimuli_type = "midi_notes",
                                display_modality = "auditory",
                                page_type = "record_audio_page")
}

sing_hbd_page <- function() {
  musicassessr::present_stimuli(stimuli = itembankr::str_mel_to_vector(hbd$abs_melody),
                                durations = itembankr::str_mel_to_vector(hbd$durations),
                                page_title = "Sing Happy Birthday!",
                                page_text = 'When you are ready, click "Record" and sing back Happy Birthday after you hear it.',
                                stimuli_type = "midi_notes",
                                display_modality = "auditory",
                                page_type = "record_audio_page")
}



