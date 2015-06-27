'''
Parse Cricket Data

Last Edited: 5.27.15

@author: Gaurav Sood

'''

import os
import re
import csv
import urllib2
from BeautifulSoup import BeautifulSoup, SoupStrainer

# Initialize four tables
winloss = csv.writer(open('winloss-odi.csv', 'wb'))
winloss.writerow(('match id', 'odiurl', 'team1', 'team2', 'toss', 'won', 'bat.or.bowl', 'date', 'day.n.night', 'ground', 'ground.country', 'groundurl')) # Win Head

for i in os.listdir("espncricinfo-odi"):
    # test: html = open('espncricinfo-t20/322541.html').read()
    html = open('espncricinfo-odi/'+ i).read()
    soup = BeautifulSoup(html)

    if soup.find(text=re.compile("Scorecard not yet available")):
        continue

    #odiurl
    odiurl = i
    print i

    # match id
    # No T20 #
    temp = soup.find(text=re.compile("ODI no. "))
    temp2 = soup.find(text=re.compile("List A "))
    temp3 = soup.find(text=re.compile("unofficial ODI "))
    temp4 = soup.find(text=re.compile("Test no. "))

    matchid = temp4

    if temp:
        matchid= temp
    elif temp2:
        matchid = temp2
    elif temp3:
        matchid = temp3

    print matchid

    # teams
    teams = soup.findAll('a', {'class' : 'teamLink'})
    print teams
    if not teams:
        team1 = "NA"
        team2 = "NA"
    else:
        team1 = teams[0].string
        team2 = teams[1].string

    # toss
    toss = "No Information"
    temp = soup.find('div', {'class':'match-information'})
    if not temp:
        toss = "No Information"
    else:
        temp = temp.findNext('span').text
        toss = temp.split(',')[0]

    # bat bowl
    batbowl = 'bat'
    if re.search('No toss', temp):
        batbowl= "No toss"
    elif re.search('Unknown', temp):
        batbowl= "Unknown"
    elif len(temp) < 2:
        batbowl="Not in the data"
    elif re.search('bat', temp.split(',')[1]) is None:
        batbowl = 'bowl'

    # won
    temp = soup.find('div', {'class' : 'innings-requirement'}).string
    won = temp.split('won')[0]

    # match date
    temp = soup.title.string.split(',')
    date = temp[1].lstrip() + ',' + temp[2].split('|')[0].rstrip()

    # ground
    groundsrc = soup.findAll('a', {'title':re.compile('view the ground profile')})[0]
    ground = groundsrc.string

    # Ground URL
    groundurl = 'http://www.espncricinfo.com' + groundsrc.get('href')

    # country in which the ground is
    countrysoup = BeautifulSoup(urllib2.urlopen(groundurl).read())
    country = countrysoup.findAll('h1')[0].find('span', attrs={'class': 'SubnavSubsection'}).string

    # day/night
    dayn=""
    temp = soup.findAll(text=re.compile('day/night'))
    if temp:
        dayn = temp[0].string

    # write to win loss table
    winloss.writerow((matchid, odiurl, team1, team2, toss, won, batbowl, date, dayn, ground, country, groundurl)) # Win/Loss Data
