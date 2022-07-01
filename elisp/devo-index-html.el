(org-export-define-derived-backend 'devo-index-html 'html
  :translate-alist '((template . devo-index-html-template)))


(defun devo-index-html-header (info)
  "Header for website"

  (concat
    "<head>"
  
   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
   "<script src=\"js/jquery-3.6.0.min.js\"></script>"
   "<script>"

   "</script>"
   "<link rel=\"stylesheet\" href=\"css/site.css\">"
 "</head>"

 "<div class=\"header col-12 test\">"
   "<a href=\"index.html\">"
   "<h1 class=\"center\">Devo_Overflow</h1>"
      "</a>"
   "<div class=\"triple-line\">"
     "<hr style=\"margin-left:40%; margin-right:40%;\">"
     "<hr style=\"margin-left:45%; margin-right:45%;\">"
     "<hr style=\"margin-left:48%; margin-right:48%;\">"
   
   "</div>"
 "</div>"
 "<div class=\"col-3 side\">"
   "<a href=\"post-series.html\"><h2>Series</h2></a>"
   "<ul id=\"series\">"
     "<li><a href=\"post-series.html#20220602142630\">Mining for Wisdom</a></li>"
     "<li><a href=\"post-series.html#20220602191239\">Devotional</a></li>"
     "<!--<li><a href=\"post-series.html#20220602191235\">Who do you say I am?</a></li>-->"
     "<li><a href=\"post-series.html#20220602191220\">Teach Like A Christian</a></li>"
   "</ul>"

 "</div>"
 "<div  class=\"col-6 test content center\">"

  ))


  (defun devo-index-html-footer (info)
  ""
  (concat
   "</div>"
   
   "<hr>"   
   "<div class=\"footer\" style=\"test\"></div>"
   ))

(defun devo-index-html-template (contents info)
    "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
    (concat
     (devo-index-html-header info)
     contents
     (devo-index-html-footer info)
     ))


(defun devo-index-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to pd custom HTML.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name."
  (org-publish-org-to 'devo-index-html filename ".html" plist pub-dir))
