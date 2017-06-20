import requests
from BeautifulSoup import BeautifulSoup
import pandas as pd

nums = range(0,1000000)

def scrapeRollerCoasters(i):
    url = "http://rcdb.com/%04d.htm" % i
    try: 
        response = requests.get(url)
        print url
        soup = BeautifulSoup(response.text)
        table = soup.findAll('tbody')
        h1s = soup.findAll('h1')
        feature = soup.find("div", {"id": "feature"})
        values = []
        d = {}
        for row in soup.findAll('tr'):
            ths = row.findAll('th')
            cells = row.findAll('td')
            i=0  
            d['name'] = h1s[0].find(text=True)
            while i < len(ths):
                d[ths[i].find(text=True)] = cells[i].find(text=True)
                i+=1
        for item in feature:
            print 'hellow'            
            #a = item.findAll('a')
            #x = a[0]
        print d
        values.append(d)
        df = pd.DataFrame(values)
        return df
    except: pass
df = pd.DataFrame()

for n in range(1000,1003):
    df = df.append(scrapeRollerCoasters(n))

print 'hello!'
print df
