import os
import json
from bs4 import BeautifulSoup 

articleData = {"articles" : [], "taglist" : []};
ID = 0;

for filePath in os.listdir('./articles'):
    row = {"ID" : ID, "file" : filePath};
    with open('./articles/' + filePath) as file:
        soup = BeautifulSoup(file.read(),'html.parser');
        row["date"] = soup.find("meta", attrs={"name":"date"})["content"];
        row["tags"] = soup.find("meta", attrs={"name":"tags"})["content"].split(" ");

    for t in row["tags"]:
        if t not in articleData["taglist"]:
            articleData["taglist"].append(t);
        
    articleData["articles"].append(row)
    ID = ID + 1;

print (articleData)
with open('article-db.json', 'w') as f:
   json.dump(articleData, f)
