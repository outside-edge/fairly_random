'''
Parse Downloaded Cricket Data

'''

import time
import csv
import simplejson
from espncricinfo.match import Match

headers = ["url", "team1", "team2", "win_toss", "bat_or_bowl", "outcome", "win_game", "date", "day_n_night", "ground", "rain", "duckworth_lewis", "match_id", "type_of_match"]

results = simplejson.loads(open('matches-odi.json').read())

##################################START PROCESSING DATA#########################################
with open("final_output.csv", "wb") as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(headers)
    for result in results:
        time.sleep(2)
        m = Match(int(result))
        if m.match_json()['match_status'] == 'forthcoming':
            continue
        if m.rain_rule() == 'D/L method':
            duckworth_lewis = 1
        else:
            duckworth_lewis = 0
        writer.writerow([m.team_1()['team_name'], m.team_2()['team_name'], m.toss_winner(), m.toss_decision(), m.result(), m.match_winner(), m.date(), m.lighting(), m.ground_name(), None, duckworth_lewis, result, m.match_class()])

##################################FINISHED#########################################
print "DONE. Wrote output to %s" %(FINAL_OUTPUT_FILE,)
