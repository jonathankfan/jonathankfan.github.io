
#POSTLOCATION AND PHOTOLINK NO LONGER AVAILABLE FROM /MEDIA REST API; SCRAPE PROFILE PAGE INSTEAD

#TOGGLE DEBUG ON OR OFF
debug=0




extractaddress=0
#extractaddress=1

################################################################################
################################################################################
#macros
################################################################################
################################################################################

path=r"C:\Users\Downloads\instagramanalytics\python\api"

import time
date=(time.strftime("%Y_%m_%d"))
hour=(time.strftime("%H"))

temp1=time.strftime("%Y_%m_%d")
temp2=time.strftime("%H:%M:%S")

from datetime import datetime, timedelta
from datetime import date

#import datetime
#date=datetime.date.today()
#HOW FORMAT LEADING ZEROS?
#date=str(datetime.date.today().year)+"_"+str(datetime.date.today().month)+"_"+str(datetime.date.today().day)

#all_data['date'] = pd.to_datetime(all_data['date']) #convert dates

################################################################################
################################################################################
#install programs
################################################################################
################################################################################

#RUN FROM WINDOWS CMD
#cd C:\Users\Jonathan\AppData\Local\Programs\Python\Python35-32
#python -m pip install --upgrade pip #OR DOWNLOAD get-pip.py
#python -m pip install python-instagram #needed to access API
#python -m pip install xlwt #needed to write excel files
#python -m pip install xlrd #needed to read excel files
#python -m pip install pandas #needed for statistics, merging excel files
#python -m pip install numpy #needed for statistics, merging excel files
#python -m pip install beautifulsoup4 #web scraping
#python -m pip install selenium
#python -m pip install requests

#import instagram API
from instagram.bind import InstagramAPIError #import error exceptions
from instagram.client import InstagramAPI

import xlwt
import xlrd

import pandas as pd
import numpy as np

import glob #needed to search files for appending

import sys

import random #random generator for tokens

from bs4 import BeautifulSoup
import requests
import re

import datetime

import json
from urllib.request import urlopen

################################################################################
################################################################################
#access tokens
################################################################################
################################################################################

client_id=""
client_secret=""

accesstoken6="" 
accesstoken1="" 
accesstoken2="" 
accesstoken3="" 
accesstoken4="" 
accesstoken5="" 

tokens=[accesstoken1, accesstoken2, accesstoken3, accesstoken4, accesstoken5, accesstoken6]

################################################################################
################################################################################
#logging
################################################################################
################################################################################

import os
if not os.path.exists(path+r"\logs"):
    os.makedirs(path+r"\logs")

import logging

#logging.basicConfig(level=logging.INFO)
#logger=logging.getLogger(__name__)
#logger.setLevel(logging.INFO) #set output level to INFO or DEBUG
#logger.setLevel(logging.DEBUG) #set output level to INFO or DEBUG

#handler=logging.FileHandler(path+r"\logs\1.users.date["+date+"].hour["+hour+"].log") #create file handler
#handler.setLevel(logging.INFO)

#formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s') #logging format
#handler.setFormatter(formatter)

#logger.addHandler(handler) #add handlers to logger

#logger.INFO("START TIME: ", temp1, temp2)

print("\nSTART TIME: ", temp1, temp2)

################################################################################
################################################################################
#userlist
################################################################################
################################################################################

userlist=["apple", "samsung"]

errorlist=[]
privatelist=[]
nonuserlist=[]

for i in userlist:

    i=i.lower()
    username=i
    endpointlist=[]
    max=0
    skip=0
    errorcount=0

    date=(time.strftime("%Y_%m_%d"))
    hour=(time.strftime("%H"))

    ################################################################################
    ################################################################################
    #folder structure
    ################################################################################
    ################################################################################

    print("")
    #print("########################################")
    print("########################################")
    print("@"+username+":")
    #print("########################################")
    print("########################################")

    import os
    if not os.path.exists(path+r"\accounts"+"\\"+username+r"\data"+r"\original\users"):
        os.makedirs(path+r"\accounts"+"\\"+username+r"\data"+r"\original\users")
    if not os.path.exists(path+r"\accounts"+"\\"+username+r"\data"+r"\original\relationships"):
        os.makedirs(path+r"\accounts"+"\\"+username+r"\data"+r"\original\relationships")
    if not os.path.exists(path+r"\accounts"+"\\"+username+r"\data"+r"\original\media"):
        os.makedirs(path+r"\accounts"+"\\"+username+r"\data"+r"\original\media")
    if not os.path.exists(path+r"\accounts"+"\\"+username+r"\data"+r"\final"):
        os.makedirs(path+r"\accounts"+"\\"+username+r"\data"+r"\final")

    ################################################################################
    ################################################################################
    #set token
    ################################################################################
    ################################################################################

    #print("ACCESS TOKEN: ", access_token)

    #access_token="" #debugging token to start at same token

    access_token=random.choice(tokens) #randomly choose access token
    api=InstagramAPI(client_id=client_id, client_secret=client_secret)
    api=InstagramAPI(access_token=access_token, client_secret=client_secret)

    ################################################################################
    ################################################################################
    #extract
    ################################################################################
    ################################################################################

    spliterror=0
    ifirstelement=1
    iterations=0
    postnumber=0
    moreavailable="true"
    while moreavailable=="true":

        moreavailable=""

        iterations=iterations+1
        #print(iterations)

        cycle=0
        while cycle<20:
            if iterations==1: #first page of results
                try:
                    #r=requests.get("http://www.instagram.com/"+username+"/media")
                    r=requests.get("http://www.instagram.com/"+username+"/?__a=1")
             
                    data=r.text
                    string=r.text
                    string=r.text.replace("null",'"null"')
                    #print(string)
                                
                    ####################
                    #EXTRACT MEDIA BLOCK:
                    ####################

                    start=r'"media": '
                    end=r'"saved_media": '
                    match=r'(.*?)'
                    pattern=str(start)+str(match)+str(end)
                    mediablock=re.search(pattern,string).group(1)#.replace("true","1").replace("false","0")

                    ####################
                    #CONFIRM PAGE EXTRACTED:
                    ####################

                    start=r'"has_next_page": '
                    end=r','
                    match=r'([a-z]*?)'
                    pattern=str(start)+str(match)+str(end)
                    #try:
                    moreavailable=re.search(pattern,mediablock).group(1)#.replace("true","1").replace("false","0")
                    #except:
                        #print(string)
                        #print("END OF MEDIA, NO MORE AVAILABLE")
                        #moreavailable="false"
                        #break
                    #print(str(moreavailable)+" more available")

                    break
                
                except:
                    cycle=cycle+1
                    #if cycle==1:
                        #print("UNKNOWN LOOKUP ERROR FOR @"+username+"; RETRYING")
                        #time.sleep(5)
                    #if cycle==20:
                        #print("WARNING: USER INFO FOR @"+username+" NOT EXTRACTED; SKIPPING")
                        #time.sleep(5)
                        #errorcount=errorcount+1
                        
            if iterations>1:
                try:
                    #r=requests.get("http://www.instagram.com/"+username+"/media/?max_id="+maxid)
                    r=requests.get("http://www.instagram.com/"+username+"/?__a=1&max_id="+maxid)

                    #if cycle>1:
                        #print("requests extracted")

                    data=r.text
                    string=r.text
                    string=r.text.replace("null",'"null"')
                    #print(string)
                                          
                    ####################
                    #EXTRACT MEDIA BLOCK:
                    ####################

                    start=r'"media": '
                    end=r'"saved_media": '
                    match=r'(.*?)'
                    pattern=str(start)+str(match)+str(end)
                    mediablock=re.search(pattern,string).group(1)#.replace("true","1").replace("false","0")

                    if cycle>1:
                        print("media block extracted")

                    ####################
                    #CONFIRM PAGE EXTRACTED:
                    ####################

                    start=r'"has_next_page": '
                    end=r','
                    match=r'([a-z]*?)'
                    pattern=str(start)+str(match)+str(end)
                    moreavailable=re.search(pattern,mediablock).group(1)#.replace("true","1").replace("false","0")

                    if cycle>1:
                        print("moreavailable extracted")

                    break
                
                except:
                    #cycle=cycle+1
                    #if cycle==1:
                        #print("UNKNOWN PAGE ERROR FOR ITERATION "+str(iterations)+"; RETRYING")
                        #time.sleep(60)
                    #if cycle==20:
                        #print(string)
                        #print("http://www.instagram.com/"+username+"/?__a=1&max_id="+maxid)
                        #print("WARNING: PAGE INFO FOR ITERATION "+str(iterations)+" NOT EXTRACTED; SKIP ENTIRE USER?")
                    #print(cycle)
                    time.sleep(10)

        if cycle==20:
            break

        #soup=BeautifulSoup(data,"html.parser")
        #string=str(soup.find_all(string=re.compile("window._sharedData"))).replace("null",'"null"')
        #string=str(soup.find_all(string=re.compile("window._sharedData"))).replace("null",'"null"').replace('", "','","').replace('}, {','},{').replace('": "','":"').replace('"}, "','"},"').replace('": {"','":{"').replace('": "','":"').replace('": ','":').replace(', "',',"').replace('[]','[nullnodes]').replace('{}','{nullbrackets}')

        #string=str(soup.find_all(string=re.compile("followed_by"))).replace("null",'"null"')
        #print(string)
        #r=requests.get("http://www.instagram.com/apple/followers")
        #print(r)
        #print(data)
        #print(soup.find_all('script',attrs={'type':'text/javascript'}))
        #print(soup.prettify())

        ####################
        #PRIVATE/CONFIRM USER EXISTS:
        ####################

        ##############################################CREATE FILE PATHS ONLY IF USER EXISTS

        #print(mediablock)
        #print("MORE AVAILABLE: "+moreavailable)
        #print("")

        #NOTE: IF MORE AVAILABLE IS FALSE, THEN NO WAY TO KNOW WHETHER ALL POSTS WERE DOWNLOADED FROM /MEDIA URL; MAY BE MISSING SOME POSTS; HAVE TO COMPARE WITH COUNT OF POSTS FROM PROFILE
        #stringsplit=string.split(r'{"'+str(firstelement)+r'": ')
        #stringsplit=string.split(r'"code": ') #can split on code, unique post identifier, but misses anything that occurs before it
        stringsplit=mediablock.split(r'__typename') #split based on unique split identifier

        #print("LENGTH OF STRING SPLIT (SHOULD BE STRING PREFIX PLUS 20 POSTS): "+str(len(stringsplit)))

        cycle=0
        splitnumber=-1 #this should be equal to 20 posts after splitting the string text, since first string split contains text before the first substring
        errorflag=""
        for i in stringsplit:

            #time.sleep(1)
            cycle=cycle+1
            splitnumber=splitnumber+1
            errortag=0

            ####################
            #CONFIRM LINE IS A POST:
            ####################

            #############################################################ASSERT r'"can_delete_comments":'

            #skip first cycle, as this contains text before the first substring
            if cycle==1:
                #print("STRING PREFIX: "+str(i))
                continue

            #print sample of post block to build code
            #print("STRING PREFIX: "+str(i))
            
            ####################
            #POST NUMBER:
            ####################

            postnumber=postnumber+1
            #print(str(postnumber))
            #if iterations==1:
                #print("POST #"+str(postnumber)+": "+str(likes)+" LIKES; "+str(comments)+" COMMENTS; "+str(posttype)+" TYPE; "+str(photolink)+" PHOTOLINK; "+str(videolink)+" VIDEOLINK")

            #sys.stdout.write(".")
            #sys.stdout.write(str(cycle)+" ")
            if splitnumber==1:
                #sys.stdout.write("\n"+str(splitnumber)+" ")
                sys.stdout.write(str(iterations)+" EXTRACTING: "+str(splitnumber)+" ")
            if splitnumber>1 and splitnumber<20:
                sys.stdout.write(str(splitnumber)+" ")
            if splitnumber>=20:
                sys.stdout.write(str(splitnumber)+" posts extracted")
            #sys.stdout.write(str(postnumber)+" ")
            sys.stdout.flush()

            ####################
            #EXTRACTING USERID:
            ####################

            #SUBSPLIT

            start='"owner": {'
            end=r'}'
            match=r'(.*?)'
            pattern=str(start)+str(match)+str(end)
            isubstring=re.search(pattern,i).group(1)
            
            start=r'"id": "'
            end=r'"'
            match=r'([0-9]*?)'
            pattern=str(start)+str(match)+str(end)
            userid=re.search(pattern,isubstring).group(1)
            #print(str(userid)+" USERID")

            ####################
            #EXTRACTING POSTDATEUNIX:
            ####################

            start=r'"date": '
            end=r','
            match=r'([0-9]*?)'
            pattern=str(start)+str(match)+str(end)
            postdateunix=re.search(pattern,i).group(1)
            #print(str(postdateunix)+" POSTDATEUNIX")

            ####################
            #EXTRACTING POSTDATE:
            ####################

            postdate=datetime.datetime.fromtimestamp(float(postdateunix)).strftime('%Y-%m-%d')
            #print("POSTDATE "+str(postdate))

            ####################
            #EXTRACTING POSTTIME:
            ####################

            posttime=datetime.datetime.fromtimestamp(float(postdateunix)).strftime('%H:%M:%S')
            #print("POSTTIME "+str(posttime))

            ####################
            #GENERATE PERIOD DAYS SINCE EXTRACT DATE:
            ####################

            #print(datetime.datetime.now().strftime('%Y-%m-%d'))
            #print(datetime.datetime.fromtimestamp(float(postdateunix)).strftime('%Y-%m-%d'))
            #print(datetime.datetime.now()-datetime.datetime.fromtimestamp(float(postdateunix)))
            period=datetime.datetime.now()-datetime.datetime.fromtimestamp(float(postdateunix))
            #print(period.days)
            perioddays=period.days
            #print(perioddays)

            ####################
            #EXTRACTING TYPE:
            ####################

            start=r'"is_video": '
            end=r','
            match=r'([a-z]*?)'
            pattern=str(start)+str(match)+str(end)
            posttype=re.search(pattern,i).group(1)
            #print(str(posttype)+" POSTTYPE")

            ####################
            #EXTRACTING POSTID/MAXID:
            ####################

            start=r'", "id": "'
            end=r'"'
            #match=r'([0-9]*_[0-9]*?)'
            match=r'([0-9]*)'
            pattern=str(start)+str(match)+str(end)
            postid=re.search(pattern,i).group(1)
            maxid=postid
            #print(str(postid)+" POSTID")
            #print(str(maxid)+" MAXID")









            ####################
            #EXTRACTING POSTLINK:
            ####################

            start=r'"code": "'
            end=r'"'
            match=r'(.*?)'
            pattern=str(start)+str(match)+str(end)
            postlink=re.search(pattern,i).group(1)
            #print(str(postlink)+" POSTLINK")









            ####################
            #EXTRACTING PHOTOLINK:
            ####################

            start=r'"thumbnail_src": "'
            end=r'"'
            match=r'(.*?)'
            pattern=str(start)+str(match)+str(end)
            photolink=re.search(pattern,i).group(1)
            #print(str(photolink)+" PHOTOLINK")




            ####################
            #EXTRACTING VIDEOLINK:
            ####################

            videolink="null"
            #print("VIDEOLINK "+i)

            ####################
            #EXTRACTING LIKES:
            ####################

            start=r'"likes": {"count": '
            end=r'}'
            match=r'([0-9]*?)'
            pattern=str(start)+str(match)+str(end)
            likes=re.search(pattern,i).group(1)
            #print(str(likes)+" LIKES")

            ####################
            #EXTRACTING COMMENTS:
            ####################

            start=r'"comments": {"count": '
            end=r'}'
            match=r'([0-9]*?)'
            pattern=str(start)+str(match)+str(end)
            comments=re.search(pattern,i).group(1)
            #print(str(comments)+" COMMENTS")

            ####################
            #EXTRACTING CAPTION:
            ####################

            try:
                start='"caption": "'
                end=r'"'
                match=r'(.*?)'
                pattern=str(start)+str(match)+str(end)
                caption=re.search(pattern,i).group(1)
                #print("caption "+caption)
            except:
                caption="null"
                #print(i)
                #print("http://www.instagram.com/"+username+"/?__a=1&max_id="+maxid)

            ####################
            #EXTRACTING LOCATION:
            ####################

            postlocation="null"
            #print(str(postlocation)+" POSTLOCATION")



















            ####################
            #GENERATE DATA
            ####################

            errorflag="generatedata"

            #endpointlist.append([date,hour,username,userid,postnumber,postid,postdateunix,postdate,posttime,posttype,likes,comments,str(caption),postlink,photolink,videolink,postlocation])
            endpointlist.append([username,userid,postnumber,postid,postdate,posttime,posttype,int(likes),int(comments),str(caption),postlink,photolink,videolink,postlocation,perioddays])

            #if extractaddress==0:
                #endpointlist.append([date,hour,username,userid,postnumber,postid,postdateunix,postdate,posttime,posttype,likes,comments,str(caption),postlink,photolink,videolink,postlocation,postlocationid,postlocationlatlng])
            #if extractaddress==1:
                #endpointlist.append([date,hour,username,userid,postnumber,postid,postdateunix,postdate,posttime,posttype,likes,comments,str(caption),postlink,photolink,videolink,postlocation,postlocationid,postlocationlatlng,postlocationaddressapprox,postlocationcity,postlocationprovince,postlocationcountry])
            #endpointlist.append([date,hour,username,userid,userprivate,postnumber,postid,postdateunix,postdate,posttime,posttype,likes,comments,str(caption),postlink,photolink,videolink,postlocation])
            max=len(endpointlist)
            #print(endpointlist)
            #print(str(max)+" POSTS")

            errorflag="NONE"

        print("MAXID: "+maxid)
            
        if errortag>0:
            errorcount=errorcount+1

        toggle="off"
        if toggle=="on":
            if splitnumber!=20 and moreavailable=="true": #should be 21 substrings (20 posts) for each page, except for last page of results
                print("")
                print(string)
                #print(i)
                print("SPLIT LIST LENGTH: "+str(len(stringsplit)))
                print("ERROR: "+str(splitnumber)+"!=20")
                print("ERROR AT: "+str(errorflag))
                print("FIRST ELEMENT: "+firstelement)
                print(username)
                print(userid)
                print(postnumber)
                print(postid)
                print(postdate)
                print(posttime)
                print(posttype)
                print(likes)
                print(comments)
                print(str(caption))
                print(postlink)
                print(photolink)
                print(videolink)
                dsfsdfd

        if debug==1:
            print("")
            print("MORE AVAILABLE: "+moreavailable)
            print("MAXID: "+maxid)

    #if skip out of while loop, then skip to next username
    if skip==1:
        continue

    if debug==1:
        print("")
        print("MORE AVAILABLE: "+moreavailable)
        print("MAXID: "+maxid)

    if moreavailable=="true":
        print("ERROR: MORE PAGES EXIST, BUT NOT EXTRACTED")
        print("MORE AVAILABLE: "+str(moreavailable))
        dsfsdsd




    #continue
    #dsfdsds

    #print(str(max)+" POSTS")
    print("\n\n"+str(postnumber)+" POSTS")





    ########################################
    #export to excel
    ########################################

    import xlwt
    from tempfile import TemporaryFile
    book = xlwt.Workbook()
    sheet1 = book.add_sheet("EXTRACTED "+date+" AT "+hour+"00")

    #sheet1.write(0,0,"extractdate")
    #sheet1.write(0,1,"extracthour")
    sheet1.write(0,0,"username")
    sheet1.write(0,1,"userid")
    #sheet1.write(0,4,"userprivate")
    sheet1.write(0,2,"postnumber")
    sheet1.write(0,3,"postid")
    #sheet1.write(0,4,"postdateunix")
    sheet1.write(0,4,"postdate")
    sheet1.write(0,5,"posttime")
    sheet1.write(0,6,"posttype")
    sheet1.write(0,7,"likes")
    sheet1.write(0,8,"comments")
    sheet1.write(0,9,"caption")
    sheet1.write(0,10,"postlink")
    sheet1.write(0,11,"photolink")
    sheet1.write(0,12,"videolink")
    sheet1.write(0,13,"postlocation")
    #sheet1.write(0,17,"postlocationid")
    #sheet1.write(0,18,"postlocationlatlng")
    sheet1.write(0,14,"days_since_posting")
    if extractaddress==1:
        sheet1.write(0,19,"postlocationaddressapprox")
        sheet1.write(0,20,"postlocationcity")
        sheet1.write(0,21,"postlocationprovince")
        sheet1.write(0,22,"postlocationcountry")
    
    #if __debug__:
        #if not max==12: raise AssertionError #confirm each variable in list will be assigned a column header

    counter=0
    row=1
    col=0
    for j,sublist in enumerate(endpointlist):
        #print(j)
        #print(sublist)
        for i,e in enumerate(sublist):
            #print(i)
            #print(e)
            sheet1.write(row,col,e)
            counter=counter+1
            col=col+1
            if extractaddress==0:
                if counter==15:
                #if counter==19:
                    counter=0
                    row=row+1 #advance to next row after all columns written
                    col=0
            if extractaddress==1:
                if counter==23:
                    counter=0
                    row=row+1 #advance to next row after all columns written
                    col=0

    if extractaddress==0:
        book.save(path+r"\accounts"+"\\"+username+r"\data"+r"\original\media"+r"\userid["+userid+"].media.date["+date+"].hour["+hour+"].scrape.xls")
        book.save(TemporaryFile())

    if extractaddress==1:
        book.save(path+r"\accounts"+"\\"+username+r"\data"+r"\original\media"+r"\userid["+userid+"].media.date["+date+"].hour["+hour+"].scrape.address.xls")
        book.save(TemporaryFile())

    book.save(path+r"\accounts"+"\\"+username+r"\data"+r"\final"+r"\userid["+userid+"].media.scrape.xls")
    book.save(TemporaryFile())

    #SAVE TO COMBINED FOLDER
    if not os.path.exists(path+r"\results"):
        os.makedirs(path+r"\results")
    book.save(path+r"\results"+"\\"+r"\igfeed.userid["+username+"].posts["+str(postnumber)+"].date["+date+"].hour["+hour+"].xls")
    book.save(TemporaryFile())

    #append files
    #print(glob.glob(path+r"\accounts"+"\\"+username+r"\data"+r"\original\users"+r"\*user.counts*")) #list files in folder
    #combined=pd.DataFrame()
    #for file in glob.glob(path+r"\accounts"+"\\"+username+r"\data"+r"\original\media"+r"\*user.media*.scrape.xls"):
        #dataframe=pd.read_excel(file)
        #combined=combined.append(dataframe,ignore_index=True)
    #combined=combined.sort_values(by=["extractdate","extracthour"],ascending=[1,0]) #will convert sort variables to numeric
    #combined.to_excel(path+r"\accounts"+"\\"+username+r"\data"+r"\final"+r"\userid["+userid+"].user.counts.hourly.scrape.xls", sheet_name=date, na_rep='.', float_format=None, columns=None, header=True, index=True, index_label='filenumber', startrow=0, startcol=0, engine=None, merge_cells=True, encoding=None, inf_rep='inf', verbose=True)
    #combined.drop_duplicates(subset=["account number","name"]).ix[:,[0,1,7]].groupby(["status"])["name"].count()

    print("SUCCESS")
    #time.sleep(5)

    if errorcount>0:
        errorlist=errorlist+["@"+str(username)+" ("+str(errorcount)+" errors)"]









print("\nERRORS: ", errorlist)
print("PRIVATE: ", privatelist)
print("NO USER EXISTS: ", nonuserlist)
print("\nSTART TIME: ", temp1, temp2)
print("END TIME: ", time.strftime("%Y_%m_%d"), time.strftime("%H:%M:%S"))
input("\nPRESS ENTER TO EXIT")
