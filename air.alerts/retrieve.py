# =========================================================
# Script to send an email if there has been recent activity
# =========================================================

import datetime
from twilio.rest import Client
import pandas as pd
import smtplib
from email.mime.text import MIMEText
from email.mime.image import MIMEImage
from email.mime.multipart import MIMEMultipart



# previous day's date
prev = datetime.date.today() - datetime.timedelta(1)

account = "xx"
token = "xx"
client = Client(account, token)

# retrieve all messages sent and received on the previous date
messages = client.messages.list(
                               date_sent = datetime.date(2018, 4, 27)
                           )

msglist = []
for msg in messages:
  info = [msg.direction,
          msg.status]
  msglist.append(info)

# convert list to pandas df
msg_df = pd.DataFrame(msglist,
                      columns = ['Direction', 'Status'])

# if there are any failed or received messages - send an email
if (('failed' in msg_df['Status'].unique()) | ('inbound' in msg_df['Direction'].unique())):
  
  # NOT WORKING
  fromaddr = "XX"
  toaddr = "XX"
  msg = MIMEMultipart()
  msg['From'] = fromaddr
  msg['To'] = toaddr
  msg['Subject'] = "TEST"
 
  body = "TESTESTESTETSETET" 
  msg.attach(MIMEText(body, 'plain'))
 
  server = smtplib.SMTP("smtp.mail.yahoo.com",587)
  server.starttls()
  server.login(fromaddr, "XX")
  text = msg.as_string()
  server.sendmail(fromaddr, toaddr, text)
  server.quit()





# REMOVED:
# msglist = []
# for msg in messages:
#   info = [msg.to, 
#           msg.from_,
#           msg.body,
#           msg.direction,
#           msg.status,
#           msg.error_code]
#   msglist.append(info)
# 
# # convert list to pandas df
# msg_df = pd.DataFrame(msglist,
#                       columns = ['To','From', 
#                                  'Body', 'Direction', 
#                                  'Status', 'Error_Code'])
# 
# # subsetting to include only messages received + messages failed to send 
# msg_df = msg_df[(msg_df['Direction'] == 'inbound') | (msg_df['Status'] == 'failed')]





