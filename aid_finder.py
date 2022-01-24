import requests
import urllib.request
import time
from bs4 import BeautifulSoup
import csv



#GOAL: go through list of base urls, and loop through each page of these base urls
#on each page, we scrape out athlete id codes (AIDs) and write that to a file


############################VARIABLES############################
#how many pages we can go for each url in our url list (page 0 is the first page)
page_limit = 47
starting_page_value = 0

###BE CAREFUL ABOUT CHANGING THIS, WE DON'T WANT TO OVERLOAD THEIR SERVERS###
sleep_time = 5 #time we wait (in seconds) between each page requests
###BE CAREFUL ABOUT CHANGING THIS, WE DON'T WANT TO OVERLOAD THEIR SERVERS###

output_file_name = "aids_mileoutput2019.csv"
with open(output_file_name, 'w', newline='') as csvfile:
  writer = csv.writer(csvfile, delimiter=',')
  writer.writerow(["AID"])

#this is a master list of all the AIDs we have collected so far
#we check against this to make sure we aren't repeating AIDs
master_aid_set = {"blank_id"}

#Insert url array here with all the base urls that we are going to visit.
#Each base url will itself lead us to ~1000 pages, each containing a bunch of athlete page links
url_list = []

# 
url_list.append("https://www.athletic.net/TrackAndField/Division/Event.aspx?DivID=97967&Event=58&page=")
print(url_list)
############################VARIABLES############################


#FOR EACH BASE URL:
#Set up loop that goes through all pages for each base url
#Terminates when we hit a page that is not found
#Terminates when we hit the page limit

#FOR EACH PAGE:
#Set up loop that goes through all athlete name links
#Screen out links with athlete IDs that we've already seen before
#Terminates when we get to bottom of the page

#handle each individual page of the base url
#turns page into soup object, from which we can strip out useful info (like links)
def handlePageURL(url):
  response = requests.get(url)
  # Parse HTML and save to BeautifulSoup object
  soup = BeautifulSoup(response.text, "html.parser")
  return soup;

def extractAIDsFromSoup(param_soup):
  #pluck out all href links that go to an athlete page
  href_count = 0;
  for link in param_soup.find_all('a'):
    my_href = str(link.get('href'))
    if "AID=" in my_href:
      str_aid_num = (my_href.split("="))[1]
      #make sure this is not already in set
      if not str_aid_num in master_aid_set:
        master_aid_set.add(str_aid_num)
        printAIDtoFile(str_aid_num)
        href_count += 1
  return href_count
 
def printAIDtoFile(aid):
  with open(output_file_name, 'a', newline='') as csvfile:
    writer = csv.writer(csvfile, delimiter=',')
    writer.writerow([aid])

#for each base url
#go through each possible page and handle each page
def loopThroughURLList():
  for base_url in url_list:
    print("working on this base url: "+base_url);
    #while loop until base url runs out of valid pages
    areWeDoneWithBaseURL = False 
    page_counter = starting_page_value 
    while areWeDoneWithBaseURL == False:
      if page_counter > page_limit:
        areWeDoneWithBaseURL = True
      else:
        page_url = base_url + str(page_counter)
        time.sleep(sleep_time) #sleep prior to page call
        print("handling page "+str(page_counter))
        soup = handlePageURL(page_url)
                
        #extract AIDs from soup value and throw them into a file
        num_vals = extractAIDsFromSoup(soup)
        print("num vals is "+str(num_vals))
        #terminate search for this base url if we don't have any results
        if num_vals == 0:
          areWeDoneWithBaseURL = true
        
        #update page counter
        page_counter += 1



############################INITIATE FUNCTION############################
loopThroughURLList()
############################INITIATE FUNCTION############################
