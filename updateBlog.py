import os
import json
from bs4 import BeautifulSoup 

articleData = {"posts" : [], "taglist" : []};

for filePath in os.listdir('./posts-html/'):
    if not filePath.startswith('.'):
        ID = os.path.splitext(filePath)[0]
        row = {"ID" : ID,
               "file" : filePath,
               "date": "",
               "tags": ""};
        with open('./posts-html/' + filePath) as file:
            soup = BeautifulSoup(file.read(),'html.parser');

            row["date"] = soup.find("meta", attrs={"name":"date"})["content"];
            row["tags"] = soup.find("meta", attrs={"name":"tags"})["content"].split(" ");

            for t in row["tags"]:
                if t not in articleData["taglist"]:
                    articleData["taglist"].append(t);
        
        articleData["posts"].append(row)

with open('article-db.json', 'w') as f:
    json.dump(articleData, f)
