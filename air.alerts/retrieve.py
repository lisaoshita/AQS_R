# =========================================================
# Script to send an email if there has been recent activity
# =========================================================


import datetime # this might have to be installed with "pip install datetime"

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
                                 date_sent = datetime.date.today()
                             )
  
  
  # formatting the messages
  msglist = []

  
  for msg in messages:
    
    info = [msg.direction, msg.status]
            
    msglist.append(info)


  # convert list of messages to pandas data frame
  msg_df = pd.DataFrame(msglist,
                        columns = ['Direction', 'Status'])
  print(msg_df)
  # if there are no 'failed' or 'inbound' messages, stop here
  if (('failed' not in msg_df['Status'].unique()) & 
      ('inbound' not in msg_df['Direction'].unique())):
    
    return True
  
  else: # send email

    fromaddr = "xx"
    recipients = ['xx', 'xx']
    password = "xx"

    # creating the email
    msg = MIMEMultipart()

    msg['From'] = fromaddr
    msg['To'] = ", ".join(recipients)
    msg['Subject'] = "Recent activity detected on Twilio account"

    html = """\
    <html>
      <body>
      <p>Incoming messages or messages that have failed to send have been detected
         on the APCD Twilio account. <strong>Please log in (https://www.twilio.com/login) and
         check the message log.</strong><br><br> </p>
      <p> <strong>Instructions for accessing the message log:</strong> <br> <br>
         1) After logging in, click the circle icon (with three dots in the middle) located on 
            the far left of the page. <br>
         2) Click "Programmable SMS" under the "COMMUNICATIONS CLOUD" list. <br>
         3) Click "View all Message Logs", located next to the "Recent Messages" section. <br>
         4) Use the options at the top to filter all text activity to the previous day. Use
            the drop-down list titled "All status", to display all texts that failed to send or 
            texts received from subscribers. For failed texts, hovering the mouse over "Failed" 
            in the message entry, displays an error code describing why the text failed to send.<br></p>
      <p> <strong>Note on failed texts:</strong> <br><br>
         Texts usually fail to send because subscribers have unsubscribed from the text service 
         (indicated by error code 21610). If there is a different error code, check here, 
         https://www.twilio.com/docs/api/errors, for more information.
      </p>
    </body>
    </html>
    """

    msg.attach(MIMEText(html, 'html'))

    # opens server
    server = smtplib.SMTP("smtp-mail.outlook.com", port = 587)
    # starts server
    server.starttls()

    server.login(fromaddr, password)
    server.sendmail(fromaddr, recipients, msg.as_string())
    server.quit()



if __name__ == '__main__':
    main()
