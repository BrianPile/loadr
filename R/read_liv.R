load_liv = function(file) {

  # determine the test station
  if (str_detect(file, "_LIV[.]xlsx$")) {
    test_station = "MOIV1"
  } else if (str_detect(file, "_LIV[.]csv$")) {
    test_station = "MOIV3"
  } else {
    test_station = "???"
  }

  # extract dut info from file name
  work_order = str_extract(file, "WO\\d{2}-\\d{4}")
  fc_id = str_extract(file, "\\d{2}FC\\d{5}")
  ch = str_extract(file, "CH([1-4])", group = 1)
  dut_id = paste(sep = "-", fc_id, ch)
  test_id = str_extract(file, "_(\\d{2})_LIV", group = 1)
  temperature = str_extract(file, "_(\\d{2})[.]?\\dC?", group = 1)

  # read data
  if (test_station == "MOIV1") {

    df = readxl::read_excel(
      path = file,
      skip = 33
    )

    # select and rename columns
    df = df |>
      select(
        current = `I/mA`,
        power = `P/mW`,
        voltage = `V/V`,
        mpd_current = `Ch2I/mA`
      )

    # convert to SI units
    df = df |>
      mutate(
        current = current * 1e-3,
        power = power * 1e-3,
        mpd_current = mpd_current * 1e-3
      )
  } else if (test_station == "MOIV3") {

    df = data.table::fread(
      file = file,
      skip = 12
    ) |>
      as_tibble()

    # select and rename columns
    df = df |>
      select(
        current = `set_current[mA]`,
        power = `power[mW]`,
        voltage = `voltage[V]`,
      ) |>
      mutate(mpd_current = 0) # MOIV3 does not have MPD capability?

    # convert to SI units
    df = df |>
      mutate(
        current = current * 1e-3,
        power = power * 1e-3
      )
  }


  # add dut info
  df = df |>
    mutate(
      work_order = work_order,
      test_station = test_station,
      fc_id = fc_id,
      ch = ch,
      dut_id = dut_id,
      test_id = test_id,
      temperature = temperature,
      .before = current
    )

  return(df)

}
