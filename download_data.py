'''
Download Cricket Data
Last Edited: 06.27.15
Updated to replace urllib2 with requests

@author: Gaurav Sood

'''

#!/usr/bin/env python
# -*- coding: utf-8 -*-

import requests
import csv
import sys
import time
import os
import math
import unicodedata
from urlparse import urlparse
from BeautifulSoup import BeautifulSoup, SoupStrainer

BASE_URL = 'http://www.espncricinfo.com'

if not os.path.exists('espncricinfo'):
    os.mkdir('espncricinfo')

for match_type in ['odi', 'test', 't20i', 't20', 'list%20a', 'first%20class']:
    r = requests.get('http://search.espncricinfo.com/ci/content/match/search.html?all=1;page=0;search=' + match_type)
    soup = BeautifulSoup(r.text)
    last_match = int(soup.findAll('span', attrs={'class':'PaginationNmbrs'})[-1].text)
    last_page = int(math.ceil(float(last_match)/float(20)))
    for i in range(0, last_page):
        time.sleep(2)
        results_page = requests.get("http://search.espncricinfo.com/ci/content/match/search.html?search={0};all=1;page={1}".format(match_type, i))
        soupy = BeautifulSoup(results_page.text)
        for new_host in soupy.findAll('a', {'class' : 'srchPlyrNmTxt'}):
            try:
                new_host = new_host['href']
            except:
                continue
            odiurl = BASE_URL + urlparse(new_host).geturl()
            new_host = unicodedata.normalize('NFKD', new_host).encode('ascii','ignore')
            print new_host
            #print(type(str.split(new_host)[3]))
            print str.split(new_host, "/")[4]
            html = requests.get(odiurl).text
            if html:
                with open('espncricinfo/%s' % str.split(new_host, "/")[4], "wb") as f:
                    f.write(html)
