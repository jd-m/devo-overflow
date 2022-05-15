import os
import json


articleData = {"articles" : []};
ID = 0;

for file in os.listdir('./articles'):
    articleData["articles"].append({"ID" : ID, "file" : file})
    ID = ID + 1;

print (articleData)

with open('article-db.json', 'w') as f:
    json.dump(articleData, f)
