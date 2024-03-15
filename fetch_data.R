# Login to cbbdata 
cbbdata::cbd_login(username = Sys.getenv("CBD_USER"),
                   password = Sys.getenv("CBD_PW"))

# Home court is 1.014% per torvik: http://adamcwisports.blogspot.com/p/every-possession-counts.html
hcMultiplier <- 0.014

# Add the efficiency ratingsxq
barts <- cbbdata::cbd_torvik_ratings(year = "2024") |>
  dplyr::select(team, barthag, adj_o, adj_d) |>
  dplyr::add_row(team = "BubTeam", # define the bubble team
                 barthag = .849, # should consider updating
                 adj_o = 115.40, # and tuning this over time
                 adj_d = 99.65) |>
  dplyr::mutate(
    oHome = adj_o * (1 + hcMultiplier),
    dHome = adj_d * (1 - hcMultiplier),
    oAway = adj_o * (1 - hcMultiplier),
    dAway = adj_d * (1 + hcMultiplier)
  ) |>
  dplyr::mutate(
    H = (oHome ^ 11.5) / (oHome ^ 11.5 + dHome ^ 11.5),
    N = (adj_o ^ 11.5) / (adj_o ^ 11.5 + adj_d ^ 11.5),
    A = (oAway ^ 11.5) / (oAway ^ 11.5 + dAway ^ 11.5)
  ) |>
  tidyr::pivot_longer(cols = c(N, H, A),
                      names_to = "location",
                      values_to = "rtg")

# Now add entire played schedule for DI teams only
all_team_sched <- cbbdata::cbd_torvik_game_stats(year = "2024") |>
  dplyr::select(game_id,
                date,
                team,
                opp,
                conf,
                opp_conf,
                location,
                result,
                pts_scored,
                pts_allowed) |>
  dplyr::mutate(opp_location = dplyr::case_match(location,
                                                 "H" ~ "A",
                                                 "A" ~ "H",
                                                 "N" ~ "N")) |>
  dplyr::arrange(date)

# Add in the location based on BubTeam
barts |>
  dplyr::filter(team == "BubTeam" & location == "H") |>
  dplyr::pull(rtg) -> bubbleHome

barts |>
  dplyr::filter(team == "BubTeam" & location == "N") |>
  dplyr::pull(rtg) -> bubbleNeut

barts |>
  dplyr::filter(team == "BubTeam" & location == "A") |>
  dplyr::pull(rtg) -> bubbleAway


# The idea here is to use bubble team's neutral barthag to reverse engineer an 
# Offense = Defense Bubble Team
#bubbleNeut = (adj_o^11.5) / (adj_o^11.5 + adj_d^11.5)
#bubbleNeut * (adj_o^11.5 + adj_d^11.5) = (adj_o^11.5)
#bubbleNeut * adj_o^11.5 + bubbleNeut * adj_d^11.5 = adj_o^11.5
#bubbleNeut + bubbleNeut * (adj_d^11.5 / adj_o^11.5 ) = 1
#bubbleNeut * (adj_d^11.5 / adj_o^11.5 ) = -bubbleNeut


# Add played schedule with ratings, wab, and NET
# The cbbdata::cbd_add_net_quad function adds the opponent's NET
# and the Quadrant of the game
sched_with_rtg <- all_team_sched |>
  dplyr::left_join(barts, by = c("team" = "team",
                                 "location" = "location")) |>
  dplyr::left_join(barts, by = c("opp" = "team",
                                 "opp_location" = "location"))  |>
  dplyr::rename(team_rtg = rtg.x, opp_rtg = rtg.y)  |>
  dplyr::mutate(
    bub_rtg = dplyr::case_when(
      location == "H" ~ bubbleHome,
      location == "A" ~ bubbleAway,
      location == "N" ~ bubbleNeut
    )
  ) |>
  dplyr::mutate(
    #bub_win_prob = log(bub_rtg / (1 - opp_rtg), base = 5),
    bub_win_prob = (bub_rtg - bub_rtg * opp_rtg) / (bub_rtg + opp_rtg - 2 * bub_rtg * opp_rtg),
    # team A is bubble, team B is opponent
    wab = dplyr::case_when(
      result == "W" ~ (1 - bub_win_prob),
      result == "L" ~ (0 - bub_win_prob),
    ),
    wab_opp = 1 - bub_win_prob,
    score = paste0(pts_scored, "-", pts_allowed),
  ) |>
  dplyr::select(
    date,
    team,
    opp,
    conf,
    opp_conf,
    location,
    result,
    score,
    opp_location,
    team_rtg,
    opp_rtg,
    bub_rtg,
    bub_win_prob,
    wab,
    wab_opp
  ) |>
  cbbdata::cbd_add_net_quad()

# Now add in the future games 
playedGames <-
  cbbdata::cbd_torvik_game_stats(year = "2024")$game_id |> 
  unique()


all_team_future <-
  cbbdata::cbd_torvik_season_schedule(year = "2024") |>
  dplyr::filter(!game_id %in% playedGames & type != "nond1") |>
  dplyr::mutate(team = home, opp = away)

all_team_future_visitors <-
  cbbdata::cbd_torvik_season_schedule(year = "2024") |>
  dplyr::filter(!game_id %in% playedGames & type != "nond1") |>
  dplyr::mutate(team = away, opp = home)

future_sched_with_ratings <-
  rbind.data.frame(all_team_future, all_team_future_visitors) |>
  dplyr::mutate(location = ifelse(neutral, "N",
                                  ifelse(team == home, "H", "A"))) |>
  dplyr::mutate(opp_location = ifelse(neutral, "N",
                                      ifelse(team == home, "A", "H"))) |>
  dplyr::arrange(date) |>
  dplyr::left_join(barts, by = c("team" = "team",
                                 "location" = "location")) |>
  dplyr::left_join(barts, by = c("opp" = "team",
                                 "opp_location" = "location"))  |>
  dplyr::rename(team_rtg = rtg.x, opp_rtg = rtg.y) |>
  dplyr::mutate(
    bub_rtg = dplyr::case_when(
      location == "H" ~ bubbleHome,
      location == "A" ~ bubbleAway,
      location == "N" ~ bubbleNeut
    )
  ) |>
  dplyr::mutate(#bub_win_prob = log(bub_rtg / (1 - opp_rtg), base = 5),
    bub_win_prob = (bub_rtg - bub_rtg * opp_rtg) / (bub_rtg + opp_rtg - 2 * bub_rtg * opp_rtg)) |>
  dplyr::mutate(
    wabW = (1 - bub_win_prob),
    wabL = -bub_win_prob,
  ) |>
  dplyr::select(
    date,
    team,
    opp,
    location,
    opp_location,
    team_rtg,
    opp_rtg,
    bub_rtg,
    bub_win_prob,
    wabW,
    wabL
  ) |>
  cbbdata::cbd_add_net_quad()

# Write these as CSV files to be used in other functions
write.csv(sched_with_rtg, "data/played_sched_rtg.csv")

write.csv(future_sched_with_ratings,
          "data/future_sched_rtg.csv")

wabRank <- cbbdata::cbd_all_metrics() |>
  dplyr::select(team, wab_rank, trank_rank)

write.csv(wabRank, "data/wab_rank.csv")

# Compile a table that shows any played games
# date, team, conf, opp, opp_conf, location, result, score, wab, quad
clean_played_tbl <- sched_with_rtg |>
  dplyr::mutate(location = dplyr::case_match(location,
                                             "A" ~ "Away",
                                             "H" ~ "Home",
                                             "N" ~ "Neutral")) |>
  dplyr::select(date, team, conf, opp, opp_conf, location, result, score, wab, quad)

write.csv(clean_played_tbl, "data/clean_played_tbl.csv")
