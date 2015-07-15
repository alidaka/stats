# given [team1_name, winrate] and [team2_name, winrate], give odds that first_team_name would win
bo1 = function(team1, team2) {
    team1[2] / (team1[2] + team2[2])
}

# given [team1_name, winrate] and [team2_name, winrate], give odds that first_team_name would win
bo3 = function(team1, team2) {
    p_team1 = bo1(team1, team2)
    (p_team1^2) * (3 - 2 * p_team1)
}

# given a matrix of [team_name, winrate] vectors, generate a round-robin matrix with [n,m] the chance that team n beats team m
rr = function(match_format, winrates) {
    nteams = nrow(winrates)
    m = matrix(NA, nteams, nteams)
    matrix(mapply(function(r, c) match_format(winrates[r,], winrates[c,]), row(m), col(m)), nrow=nteams)
}

# convert a matrix of odds (e.g. output from rr) to wins/losses; row is winner, column is loser
odds_to_wins = function(odds) {
    # little bit of fudging to avoid problems with 50/50 splits; arbitrarily let the lower-indexed team win
    f = function(odds, winner, loser) {
        if (odds > .5) 1
        else if (odds < .5) 0
        # tiebreaker; side effect of zeroing the diagonal
        else if (winner < loser) 1
        else 0
    }
    
    matrix(mapply(f, odds, row(odds), col(odds)), nrow=nrow(odds))
}

# get the top N players from a round-robin win matrix
top_from_rr = function(teams, rr_wins, n_teams) {
    # dummy copy just to pre-initialize output dataframe
    winners = teams[1:n_teams,]
    team_wins = rowSums(rr_wins)
    
    for (i in 1:n_teams) {
        winner = which.max(team_wins)
        winners[i,] = teams[winner,]
        
        team_wins[winner] = 0
    }
    
    winners
}

# Phase One of Playoffs: bottom four teams in BO3 RR
# Top 2 to Phase Two of Playoffs
# too complicated to simulate all for now, just figure out which guys would make it and run the main event with 16
phase_one = function(teams) {
    m = rr(bo3, teams)
    m = odds_to_wins(m) 
    
    # I believe at this point the teams are guaranteed to be 3-0, 2-1, 1-2, and 0-2
    top_from_rr(teams, m, 2)
}

# Phase Two of Playoffs: 16-team BO1 RR
# Top 2 direct to Main Event
# Middle 8 to Phase Three of Playoffs
# Bottom 6 eliminated
phase_two = function(teams) {
    m = rr(bo1, teams)
    m = odds_to_wins(m)
    
    winners = top_from_rr(teams, m, 2)
    middle = top_from_rr(teams, m, 8)
    
    list(winners, middle[3:8,])
}

qualifying_teams = read.csv("winrates_gg.csv")
phase_one_winners = phase_one(qualifying_teams[15:18,])
phase_two_teams = rbind(qualifying_teams[1:14,], phase_one_winners)
r = phase_two(phase_two_teams)
main_event_teams = r[1]  # first two of them, at least
phase_three_teams = r[2]
