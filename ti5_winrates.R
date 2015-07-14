setwd("/home/alidaka/stats")
winrates = read.csv("winrates_gg.csv")

#given [team1_name, winrate] and [team2_name, winrate], give odds that first_team_name would win
bo3 = function(team1, team2) {
    p_team1 = team1[2] / (team1[2] + team2[2]) #odds of team1 win in BO1
    (p_team1^2) * (3 - 2 * p_team1)
}

#given a matrix of [team_name, winrate] vectors, generate a round-robin matrix where [n,m] is the chance that team n beats team m
bo3_rr = function(winrates) {
    nteams = nrow(winrates)
    m = matrix(NA, nteams, nteams)
    matrix(mapply(function(r, c) bo3(winrates[r,], winrates[c,]), row(m), col(m)), nrow=nteams)
    
    #outer(1:nteams, 1:nteams, FUN=function(r, c) bo3(winrates[r,], winrates[c,])
}

#playoffs, bottom four teams in BO3 RR, top 2 advance
#too complicated to simulate all for now, just figure out which guys would make it and run the main event with 16
phaseone = winrates[15:18,]
bo3_rr(phaseone)
