from twilio.rest import Client

import pandas as pd

import string as str


def send_alerts():

    # load data as pandas data frame
    numsdf = pd.read_csv("H:/TECH/Lisa/R/test_subscribers.csv", 
                         sep = ",",
                         encoding = "ISO-8859-1",
                         usecols = ['PHONE', 'DUST'],
                         dtype = {'PHONE': 'str', 'DUST': 'str'})

    # remove white spaces + convert to lower case
    numsdf['DUST'] = numsdf['DUST'].str.strip().str.lower()

    # keep phone numbers that subscribed and have entered complete numbers
    nums = numsdf.PHONE[(numsdf["DUST"] == "yes") & (numsdf['PHONE'].str.len() >= 10)]
    
    # add leading 1 to phone numbers without it
    nums[nums.str.len() < 11] = "1" + nums

    # create addresses + convert to list
    full = "{\"binding_type\":\"sms\",\"address\":\"+" + nums + "\"}"
    full = full.tolist()

    # send SMS
    account = "x"
    token = "x"
    client = Client(account, token)

    notification = client.notify.services("x") \
     .notifications.create(
        to_binding=full,
        body='(EARLY AIRAWARE ALERT) Blowing dust detected on the Nipomo Mesa. Visit AIRNOW '
             '<http://bit.ly/NipomoAQI>, to monitor the hourly AQI.')

    return


def main():
    send_alerts()


if __name__ == '__main__':
    main()
    
    
# to get actual file with numbers, use: "H:/TECH/Lisa/R/subscriberlist.csv"
