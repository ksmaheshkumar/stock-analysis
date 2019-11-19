"""
Scrapes wikipedia for s&p 500 companies and downloads the eod data from alphavantage
and stores it in sqlite database
"""

import requests
import pandas as pd
import sqlite3
import time

ALPHAV_API_KEY = "NCDSNRJAUALPFFLH"
DB_NAME = "stocks_weekly.db"
TABLE_NAME = "eod"
alpha_eod_url = "https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol={}&outputsize=full&datatype=json&apikey={}"
alpha_eow_url = "https://www.alphavantage.co/query?function=TIME_SERIES_WEEKLY_ADJUSTED&symbol={}&outputsize=full&datatype=json&apikey={}"

def connect_db():
    """
    Connects to sqlite and returns conn and cur
    """
    # Connect to sqlite db
    conn = sqlite3.connect(DB_NAME)
    # Set the cursor
    cur = conn.cursor()
    return conn, cur

def disconnect_db(conn):
    """
    Disconnects from sqlite db
    """
    # Commit changes
    conn.commit()
    # Disconnect from sqlite
    conn.close()
    return


def get_snp_symbols():
    """
    Returns the current list of S&P 500 stock symbols 
    """
    wikipedia_url = ("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
    data = pd.read_html(wikipedia_url)
    table = data[0]
    tickers = table['Symbol'].tolist()
    return tickers

def create_table(cur, table_name):
    # Create table if it doesnt exist
    cur.execute(f'''CREATE TABLE IF NOT EXISTS {table_name} (date, symbol, open, high, low, close, adjclose, volume, dividend)''')
    return cur

def populate_db(stock_list, ind, conn, cur):
    """
    Get eod data from alphavantage and populates sqlite table
    """
    for sto in stock_list[ind:ind+3]:
        cur = create_table(cur, sto)
        response_dict = requests.get(alpha_eow_url.format(sto, ALPHAV_API_KEY)).json()
        if not "Weekly Adjusted Time Series" in response_dict:
            print(f"Stock {sto} not found, stopping operation")
            break
        time_series = response_dict['Weekly Adjusted Time Series']
        for key, val in time_series.items():
            val['date'] = key
            val['symbol'] = sto
        #to_db = [(i['date'], i['symbol'], i['1. open'], i['2. high'], i['3. low'], i['4. close'], i['5. adjusted close'],  
         #         i['6. volume'], i['7. dividend amount'], i['8. split coefficient']) for i in time_series.values()]
        to_db = [(i['date'], i['symbol'], i['1. open'], i['2. high'], i['3. low'], i['4. close'], i['5. adjusted close'],  
                  i['6. volume'], i['7. dividend amount']) for i in time_series.values()]
        cur.executemany(f'''INSERT INTO {sto} (date, symbol, open, high, low, close, adjclose, volume, dividend) VALUES (?,?,?,?,?,?,?,?,?)''', to_db)
        conn.commit()
        print(f"Added {sto}:{ind}")
    return


def main():
    """
    Wrapper function which calls populate_stock_db every 60 seconds (API limitations)
    """
    conn, cur = connect_db()
    stock_list = get_snp_symbols()
    ind = 222
    while True:
        if ind > 500:
            print("List index above 500, exiting while loop")
            break
        populate_db(stock_list, ind, conn, cur)
        ind += 5
        print("Waiting 80 Seconds")
        time.sleep(80)
    disconnect_db(conn)
    return

def test():
    print(len(get_snp_symbols()))

if __name__ == "__main__":
    main()
