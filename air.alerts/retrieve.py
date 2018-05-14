# =======================
# retrieving message list 
# =======================

import datetime
from twilio.rest import Client

account = "xx"
token = "xx"
client = Client(account, token)

# this method only allows you to grab messages for a single date
# use a loop to iterate over each day, save messages in a list? 
# twilio dates are in UTC tz 
# messages = client.messages.list(
#                                # "date_sent" <= date(2018, 5, 11),
#                                start_date = date(2018, 4, 27),
#                                end_date = date(2018, 5, 4)
#                            )
# 
# for message in messages:
#   print(message.direction)

base = datetime.datetime.today()
print(base)
