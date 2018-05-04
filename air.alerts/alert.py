# ==========================
# Script to send text alerts
# ==========================

# send_alerts: 
#   - loads csv file of subscriber phone numbers
#   - removes all spaces from DUST column, converts to all lowercase
#   - deletes any incomplete phone numbers (those that contain less than 10 numbers) 
#   - adds a leading 1 to all numbers without it
#   - concatenates it with the rest of the binding, converts to a list 
#   - sends SMS using the list of numbers

# to get actual file with numbers, use: "H:/TECH/Lisa/R/subscriberlist.csv"
# to get test file run with: "H:/TECH/Lisa/R/test_subscribers.csv"


from twilio.rest import Client

import pandas as pd


def main():

    # load data as pandas data frame
    numsdf = pd.read_csv("/Users/lisaoshita/Desktop/phone_numbers.csv", 
                         sep = ",",
                         encoding = "ISO-8859-1",
                         usecols = ['PHONE', 'DUST'],
                         dtype = {'PHONE': 'str', 'DUST': 'str'})

    # remove white spaces + convert to lower case
    numsdf['DUST'] = numsdf['DUST'].str.replace(" ", "").str.lower()

    # keep phone numbers that subscribed and have entered complete numbers
    nums = numsdf.PHONE[(numsdf["DUST"] == "yes") & (numsdf['PHONE'].str.len() >= 10)]

    # add leading 1 to phone numbers without it
    nums[nums.str.len() < 11] = "1" + nums

    # create addresses + convert to list
    full = "{\"binding_type\":\"sms\",\"address\":\"+" + nums + "\"}"
    full = full.tolist()

    # send SMS
    account = "xx"
    token = "xx"
    client = Client(account, token)

    notification = client.notify.services("xx") \
     .notifications.create(
        to_binding=full,
        body='(EARLY AIRAWARE ALERT) Blowing dust detected on the Nipomo Mesa. Visit AIRNOW '
             '<http://bit.ly/NipomoAQI>, to monitor the hourly AQI.')

    return


if __name__ == '__main__':
    main()



