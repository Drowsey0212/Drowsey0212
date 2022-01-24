import requests
import urllib.request
import time
from bs4 import BeautifulSoup
import csv


#GOAL: go through list of athlete id codes (AIDs)
#visit page corresponding to that aid and scrape data from it
#send output to individually named csv

############################VARIABLES############################
#how many aids we can work through before stopping
aid_limit = 10
starting_aid_value = 0
input_file = "allevents_aids.csv"

###BE CAREFUL ABOUT CHANGING THIS, WE DON'T WANT TO OVERLOAD THEIR SERVERS###
sleep_time = 2 #time we wait (in seconds) between each page request
###BE CAREFUL ABOUT CHANGING THIS, WE DON'T WANT TO OVERLOAD THEIR SERVERS###

#base url for accessing an athlete page
base_url = "https://www.athletic.net/TrackAndField/Athlete.aspx?AID="
final_modifier = "&L=0"
############################VARIABLES############################

def initiateAIDListRead():
  #read from input csv
  with open('allevents_aids.csv', newline='') as csvfile:
    reader = csv.reader(csvfile, delimiter=',')
    for row in reader:
      current_aid = str(row[0]) 
      full_url = base_url + current_aid + final_modifier
      print('trying to pull this url: '+full_url)
      athlete_soup = handleAthleteURL(full_url)
      processAthleteSoup(athlete_soup, current_aid)

#FOR EACH ATHLETE PAGE:
#Prepare to write out information to a .csv file
#Loop through every event time on the page
def handleAthleteURL(url):
  time.sleep(sleep_time) #sleep prior to page call
  response = requests.get(url)
  # Parse HTML and save to BeautifulSoup object
  soup = BeautifulSoup(response.text, "html.parser")
  return soup;

#go through each card on the page and strip out each table from that page
def processAthleteSoup(param_soup, current_aid):
  #create new csv with correct column names
  with open("./other_athletes/athlete_"+current_aid+".csv", 'w', newline='') as csvfile:
    writer = csv.writer(csvfile, delimiter=',')
    writer.writerow(["AID","year/season","affiliation","grade","event","finish","time","record","date","race"])
  #pluck out all div cards
  card_count = 1
  for div in param_soup.find_all("div", class_="card")[1:]:
    #we skipped the first one, which is a general records card
    print("CARD COUNT "+str(card_count)+" ==================================")    
    if card_count > 0:
      # pull out the year/season, affiliation, and grade
      first_header = div.find_all('h5')[0]
      first_header_obj = []
      for string in first_header.stripped_strings:
        first_header_obj.append(string)
      #grab all remaining headers and put them into list
      headers = div.find_all('h5')[1:]
      table_count = 0
      #loop through table list
      for table in div.find_all("table"):
        handleTable(current_aid, headers, table, table_count, "blah", first_header_obj)        
        table_count += 1    
    card_count += 1

#handle each table on the page (which is an event and a series of racenames+times under that event)
def handleTable(current_aid, headers, table, table_count, event, first_header_obj):
  #process each row in table (which is a race name + time)
  for row in table.find_all('tr'):
    #for each td element in the table row
    row_info = []
    for my_td in row.find_all('td'):
      td_count = 0
      for string in my_td.stripped_strings:
        row_info.append(string)
        td_count += 1
      #if there were no strings in the td, put in a blank one
      #useful for the first td where sometimes there is a finish and sometimes there isn't
      if td_count == 0:
        row_info.append("N/A");
    with open("./athlete_data/athlete_"+current_aid+".csv", 'a', newline='') as csvfile:
      writer = csv.writer(csvfile, delimiter=',')
      #check to make sure we put in a blank if we have no "pr" thing
      #quick and dirty check is to see if we don't have a low character count here
      if len(row_info[2]) >= 5:
        row_info.insert(2, "NR")
      #grab the event name from header, sometimes there are multiple stripped strings in the header
      event_name_arr = []
      for event_string in headers[table_count].stripped_strings:
        event_name_arr.append(event_string)
      #for first header obj, 0 is year/season, 1 is affiliation, and 2 is grade
      output_row = [current_aid, first_header_obj[0], first_header_obj[1], first_header_obj[2], event_name_arr[0], row_info[0],row_info[1],row_info[2],row_info[3],row_info[4]]
      writer.writerow(output_row)
  

#FOR EACH EVENT TIME:
#Write out a row to the .csv file with all the information contained in the event time

############################INITIATE FUNCTION############################
initiateAIDListRead()
############################INITIATE FUNCTION############################