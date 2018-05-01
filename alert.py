from twilio.rest import Client

import pandas as pd


def send_alerts():

    # load data as pandas data frame
    numsdf = pd.read_csv("/Users/lisaoshita/Desktop/phone_numbers.csv",
                         sep = ",",
                         usecols = ['PHONE', 'DUST'],
                         dtype = {'PHONE': str, 'DUST': str})

    # remove white spaces + convert to lower case
    numsdf['DUST'] = numsdf['DUST'].str.strip()
    numsdf['DUST'] = numsdf['DUST'].str.lower()

    # keep phone numbers that subscribed and have entered complete numbers
    nums = numsdf.PHONE[(numsdf["DUST"] == "yes") & (numsdf['PHONE'].str.len() >= 10)]

    # add leading 1 to phone numbers
    nums[nums.str.len() < 11] = "1" + nums

    # create addresses + convert to list
    full = "{\"binding_type\":\"sms\",\"address\":\"+" + nums + "\"}"
    full = full.tolist()
    print(full)

    send SMS
    account = "ACa471eea1d61917cce7d77ac2c1637889"
    token = "a31cfe18cbdff3a5297d4dd9df941042"
    client = Client(account, token)

    notification = client.notify.services("MG412dd9d44556e2f580cee4b2b79aff52") \
     .notifications.create(
        to_binding=full,
        body='(EARLY AIRAWARE ALERT) Blowing dust detected on the Nipomo Mesa. Visit AIRNOW '
             '<http://bit.ly/NipomoAQI>, to monitor the hourly AQI.')

    return


def main():
    send_alerts()


if __name__ == '__main__':
    main()