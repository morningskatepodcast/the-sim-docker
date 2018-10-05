#!/usr/bin/env python

import csv
import requests
import time

# Month and Date info
m = time.strftime("%m") #Needs to update automatically
d = time.strftime("%-d")
y = time.strftime("%Y")

#Goes to FL and scrapes info
class LabsScraper:
    API_url = 'http://www.fantasylabs.com/api/teams/4/games/' + m + '-' + d + '-' + y
    # Get the list of teams playing
    def get_teams_info(self):
        response = requests.get(self.API_url)
        try:
            x = response.json()['TeamLines']
            return [i['Properties']['TeamName'].replace(" ","%20") for i in x]
        except: return []
    # Get lines for each team
    def get_lines(self,team):
        self.scraped_lines = []
        url = 'http://www.fantasylabs.com/api/lines/4/' + str(team) + '/' + m + '-' + d + '-' + y
        response_lines = requests.get(url)
        if response_lines.json()['NextMatchupData'][0]['Properties']['Pts'] == None:
            total = 5.5/2
        else: total = response_lines.json()['NextMatchupData'][0]['Properties']['Pts']
        games = len(self.get_teams_info())
        teamm = response_lines.json()['NextMatchupData'][0]['Properties']['TeamShort']
        line = response_lines.json()['PlayerLines']
        pp1 = [i['Properties']['FullName'] for i in line[20:25]]
        pp2 = [i['Properties']['FullName'] for i in line[25:]]
        for i in line[:20]:
            name = i['Properties']['FullName']
            sal = i['Properties']['Salary_DK']
            pos = i['Properties']['Position'][0]+i['Properties']['Position'][-1]
            A30 = i['Properties']['Assists_Month']
            A365 = i['Properties']['Assists_Year']
            iC30 = i['Properties']['CorsiFor_Month']
            iC365 = i['Properties']['CorsiFor_Year']
            G30 = i['Properties']['Goals_Month']
            G365 = i['Properties']['Goals_Year']
            Min30 = i['Properties']['Minutes_Month']
            Min365 = i['Properties']['Minutes_Year']
            PPShots30 = i['Properties']['PowerPlayShots_Month']
            PPShots365 = i['Properties']['PowerPlayShots_Year']
            if name in pp1:
                pp = 1
            elif name in pp2:
                pp = 2
            else:
                pp = 0
                PPShots30 = 0
                PPShots365 = 0
            info = [name,teamm,sal,pos,pp,A30,A365,iC30,iC365,G30,G365,Min30,Min365,PPShots30,PPShots365, total,games]
            self.scraped_lines.append(info)
    # Puts info into a csv
    def run(self):
        data = self.get_teams_info()
        for team in data:
            self.get_lines(team)
            with open('output/fl-latest.csv', 'ab') as csvfile:
                writer = csv.writer(csvfile)
                for r in filter(lambda x: x[3] not in ['P1', 'P2'], self.scraped_lines):
                    writer.writerow(r)
            del writer

if __name__ == '__main__':
    scraper = LabsScraper()
    scraper.run()
