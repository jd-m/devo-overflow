import os


articles = "";
# create the code that call the funtion that creates the cards. ITerate over directory and create a row for ach article
for file in os.listdir('./articles'):
    articles = articles + 'makeArticleCard(\'articles/' + file + '\');\n';

print (articles)

with open ('index.html', 'w') as newFile:
    with open('index-template.html') as f:
        updated = f.read().replace("//$make_article_list$", articles);
        print (updated)
        newFile.write(updated)
