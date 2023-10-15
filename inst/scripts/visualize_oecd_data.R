## Example to visualize data from OECD better life index (2017)
## Download csv file from https://stats.oecd.org/index.aspx?DataSetCode=BLI
oecd_raw <- read.csv("../../../BLI_02042021153353422.csv")
oecd_proc <- oecd_raw |>
    dplyr::filter(Inequality == "Total") |>
    dplyr::select(Country, Indicator, Value) |>
    dplyr::mutate(Indicator = make.names(Indicator)) |>
    tidyr::pivot_wider(names_from = Indicator, values_from = Value)

bettr(df = oecd_proc, idCol = "Country",
      initialTransforms = list(
          Labour.market.insecurity = list(flip = TRUE, transform = '[0,1]'),
          Stakeholder.engagement.for.developing.regulations = list(transform = '[0,1]'),
          Dwellings.without.basic.facilities = list(flip = TRUE, transform = '[0,1]'),
          Housing.expenditure = list(flip = TRUE, transform = '[0,1]'),
          Feeling.safe.walking.alone.at.night = list(transform = '[0,1]'),
          Rooms.per.person = list(transform = '[0,1]'),
          Household.net.adjusted.disposable.income = list(transform = '[0,1]'),
          Household.net.wealth = list(transform = '[0,1]'),
          Employment.rate = list(transform = '[0,1]'),
          Long.term.unemployment.rate = list(flip = TRUE, transform = '[0,1]'),
          Personal.earnings = list(transform = '[0,1]'),
          Quality.of.support.network = list(transform = '[0,1]'),
          Educational.attainment = list(transform = '[0,1]'),
          Student.skills = list(transform = '[0,1]'),
          Years.in.education = list(transform = '[0,1]'),
          Air.pollution = list(flip = TRUE, transform = '[0,1]'),
          Water.quality = list(transform = '[0,1]'),
          Voter.turnout = list(transform = '[0,1]'),
          Life.expectancy = list(transform = '[0,1]'),
          Self.reported.health = list(transform = '[0,1]'),
          Life.satisfaction = list(transform = '[0,1]'),
          Homicide.rate = list(flip = TRUE, transform = '[0,1]'),
          Employees.working.very.long.hours = list(flip = TRUE, transform = '[0,1]'),
          Time.devoted.to.leisure.and.personal.care = list(transform = '[0,1]')
      ))
