'''
Download Cricket Data
Last Edited: 06.14.2020

@author: Gaurav Sood
@author: Derek Willis

'''

#!/usr/bin/env python
# -*- coding: utf-8 -*-

import requests
import math
import time
import json
from bs4 import BeautifulSoup, UnicodeDammit

for match_type in ['list', 'first', 'odi', 'test', 't20i', 't20']:
    results = []
    r = requests.get('http://search.espncricinfo.com/ci/content/match/search.html?all=1;page=0;search=' + match_type)
    soup = BeautifulSoup(r.text, "html.parser")
    last_match = int(soup.find_all('span', attrs={'class':'PaginationNmbrs'})[-1].text)
    last_page = int(math.ceil(float(last_match)/float(20)))
    for i in range(0, last_page):
        time.sleep(1)
        results_page = requests.get("http://search.espncricinfo.com/ci/content/match/search.html?search={0};all=1;page={1}".format(match_type, i))
        soupy = BeautifulSoup(results_page.text, "html.parser")
        for new_host in soupy.find_all('a', {'class' : 'srchPlyrNmTxt'}):
            try:
                new_host = UnicodeDammit(new_host['href']).unicode_markup
            except:
                continue
            print(new_host.split("/")[4].split('.')[0])
            results.append(new_host.split("/")[4].split('.')[0])

    if match_type == 'list':
        file_name = 'list-a'
    elif match_type == 'first':
        file_name = 'first-class'
    else:
        file_name = match_type

    with open("matches-{0}.json".format(file_name), "w") as f:
        json.dump(results, f)
