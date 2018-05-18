# =========================================================
# Script to send an email if there has been recent activity
# =========================================================

import datetime

from twilio.rest import Client

import pandas as pd

import smtplib
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText

def main(): 

  account = "xx"
  token = "xx"
  client = Client(account, token)

  # retrieve all messages sent and received on the previous date
  messages = client.messages.list(
                                 date_sent = datetime.date.today() # prev # datetime.date(2018, 4, 27), (4/24 for no incoming/failed)
                             )
  
  # formatting the messages
  msglist = []
  for msg in messages:
    info = [msg.direction,
            msg.status]
    msglist.append(info)

  # convert list of messages to pandas data frame
  msg_df = pd.DataFrame(msglist,
                        columns = ['Direction', 'Status'])
  
  # if there are no 'failed' or 'inbound' messages, stop here
  if (('failed' not in msg_df['Status'].unique()) & ('inbound' not in msg_df['Direction'].unique())):
    
    return True
  
  else: # send email 
    
    fromaddr = "xx"
    toaddr = "xx"
    password = "xx"
    
    # creating the email
    msg = MIMEMultipart()
    
    msg['From'] = fromaddr
    msg['To'] = toaddr
    msg['Subject'] = "Check Twilio Account - recent activity detected"
    
    body = "Recent activity has been detected on the <a href=""https://www.twilio.com/login"">APCD Twilio account.</a> Please log in and check." 
    
    msg.attach(MIMEText(body, 'html'))
    
    # opens server
    server = smtplib.SMTP("smtp-mail.outlook.com", port = 587) 
    # starts server
    server.starttls() 
    
    server.login(fromaddr, password)
    server.sendmail(fromaddr, toaddr, msg.as_string())
    server.quit()

if __name__ == '__main__':
    main()
