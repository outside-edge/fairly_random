'''
Parse Downloaded Cricket Data

'''

import time
import csv
import simplejson
from espncricinfo.match import Match

headers = ["team1", "team1_id", "team2", "team2_id", "win_toss", "bat_or_bowl", "outcome", "win_game", "start_date", "end_date", "country", "ground", "match_id", "type_of_match", "match_type_id", "home_team_id", "first_innings_runs", "first_innings_batting_team", "url"]

matches = simplejson.loads(open('/Users/derekwillis/code/cricket-stats/data/json/matches-first-class.json').read())
matches = sorted(list(set(matches))) # dedupe

##################################START PROCESSING DATA#########################################
with open("english_county_championship_innings.csv", "wb") as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(headers)
    for match in matches:
        if int(match) < 539738:
            continue
        time.sleep(1)
        try:
            m = Match(int(match))
            if m.match_json()['country_name'] != 'England':
                continue
            if m.match_json()['match_status'] == 'forthcoming':
                continue
            if m.result() == '':
                continue
            if m.result() == 'Match abandoned without a ball bowled':
                continue
            if len(m.innings()) == 0:
                continue
            print match
            try:
                m.team_2()['team_name']
            except KeyError:
                continue
            try:
                m.team_1()['team_name']
            except KeyError:
                continue
            if m.team_1_id == m.innings()[0]['batting_team_id']:
                first_innings_batting_team = m.team_1()['team_name']
            else:
                first_innings_batting_team = m.team_2()['team_name']
            if m.match_json()['international_class_card'] != "":
                match_type_id = m.match_json()['international_class_id']
            else:
                match_type_id = m.match_json()['general_class_id']
            writer.writerow([m.team_1()['team_name'], m.team_1_id(), m.team_2()['team_name'], m.team_2_id(), m.toss_winner(), m.toss_decision(), m.result(), m.match_json()['winner_team_id'], m.date(), m.match_json()['end_date_raw'], m.lighting(), m.match_json()['country_name'], m.ground_name(), match, m.match_class(), match_type_id, m.match_json()['home_team_id'], m.innings()[0]['runs'], first_innings_batting_team, m.match_url])
        except simplejson.scanner.JSONDecodeError:
            continue

##################################FINISHED#########################################
print "DONE." #"Wrote output to %s" %(FINAL_OUTPUT_FILE)
