from selenium import webdriver
from bs4 import BeautifulSoup
import pandas as pd
import csv


def retrieveGames(rows, gameID):
    for row in rows:
        if ("@" in row.text or "2020" in row.text):
            if ("2020" in row.text):
                date = row.text
                date = date.replace("Jan ", "01-")
                date = date.replace("Feb ", "02-")
                date = date.replace("Mar ", "03-")
                date = date.replace("Apr ", "04-")
                date = date.replace("May ", "05-")
                date = date.replace("Jun ", "06-")
                date = date.replace("Jul ", "07-")
                date = date.replace("Aug ", "08-")
                date = date.replace("Sep ", "09-")
                date = date.replace("Oct ", "10-")
                date = date.replace("Nov ", "11-")
                date = date.replace("Dec ", "12-")
                date = date.replace("-1, ", "-01-")
                date = date.replace("-2, ", "-02-")
                date = date.replace("-3, ", "-03-")
                date = date.replace("-4, ", "-04-")
                date = date.replace("-5, ", "-05-")
                date = date.replace("-6, ", "-06-")
                date = date.replace("-7, ", "-07-")
                date = date.replace("-8, ", "-08-")
                date = date.replace("-9, ", "-09-")
                date = date.replace(", ", "-")
            else:
                gameInfo = row.text
                gameInfo = gameInfo.replace("@ ", "")
                gameInfo = gameInfo.replace("H2H -  ", "")
                gameInfo = gameInfo.replace(" ", "_")
                gameInfo = gameInfo.replace(",~", " ")
                gameInfo = gameInfo.replace(".m.,", ".m. ")
                gameInfo = gameInfo.replace(".m,", ".m. ")
                gameInfo = gameInfo.strip().split()

                game = [gameID]

                index = 1

                for item in gameInfo:
                    game.append(item.replace("_", " ").strip())

                game.append(date.strip())

                gameID += 1

                print(game)

                writer.writerow(game)

    return gameID



# retrieve March games

driver = webdriver.Chrome()

driver.get('https://www.landofbasketball.com/results/2019_2020_mar_scores.htm');

gameID = 0

content = driver.page_source
soup = BeautifulSoup(content)

with open('nba-remaining-games-2019-2020.csv', 'w', newline='') as file:
    writer = csv.writer(file)
    writer.writerow(["id", "visitor_team", "home_team", "time", "tv", "date"])
    
    for a in soup.findAll('table',href=False, attrs={'class':'color-alt max-1'}):
        body = a.find('tbody')

        rows = body.findAll('tr')

        gameID = retrieveGames(rows, gameID)


driver.quit()


# retrieve April games

driver = webdriver.Chrome()

driver.get('https://www.landofbasketball.com/results/2019_2020_apr_scores.htm');

content = driver.page_source
soup = BeautifulSoup(content)

with open('nba-remaining-games-2019-2020.csv', 'a', newline='') as file:
    writer = csv.writer(file)
    
    for a in soup.findAll('table',href=False, attrs={'class':'color-alt max-1'}):
        body = a.find('tbody')

        rows = body.findAll('tr')

        gameID = retrieveGames(rows, gameID)


driver.quit()
