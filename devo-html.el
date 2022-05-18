(org-export-define-derived-backend 'devo-html 'slimhtml
  :translate-alist '((template . devo-html-template)))


(defun devo-html-header (info)
  "Header for website"

  (concat
   "<head>\n"
   
   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
   
   "<script src=\"../jquery-3.6.0.min.js\"></script>\n"
   (format "<meta name=\"date\" content=\"%s\">\n" (format-time-string "%F"))
   (format "<meta name=\"tags\" content=\"%s\">\n"
	   (concat (let ((string (format "%s" (plist-get info :filetags))))
		 (string-match "(\\(.*\\))"
			       string)
		 
		 (match-string-no-properties 1 string))))
   
   "<link rel=\"stylesheet\" href=\"../site.css\">\n"
   "</head>\n"

  "<div class=\"header col-12 test\">"
  "<a href=\"../index.html\">"
  "<h1 class=\"center\">Devo_Overflow</h1>"
  "<div class=\"triple-line\">"
  "<hr style=\"margin-left:40%; margin-right:40%;\">"
  "<hr style=\"margin-left:45%; margin-right:45%;\">"
  "<hr style=\"margin-left:48%; margin-right:48%;\">"
  "</div>"
  "</a>"
  "</div>"
  "<div class=\"col-3\"></div>"
  "<div class=\"col-6 test article content center\">"
  (format "<h1 class=\"center\" id=\"title\">%s</h1" (car (plist-get info :title)))
  (format "<meta name=\"date\" content=\"%s\">\n" (format-time-string "%F"))
  (format "<h5 id=\"date\">%s</h5>" (format-time-string "%d %b %Y"))
  
   )
  )


(defun devo-html-footer (info)
  ""
  (concat
   "<hr>"   
   "<div id=\"blogTags\" class=\"blogTags\"></div>"
   "<script>"
   "let tagArr = $('meta[name=\"tags\"]').attr(\"content\").split(\" \");"
   "let tagString = \"\";"
   "$.each(tagArr, function(i, tag) {
		  tagString = tagString + \"<a href=\\\"../tags.html#\"+tag+\"\\\"> #\"  + tag + \"</a>\"});"
		  "$('#blogTags').append(tagString);"
		  "</script>"
		  "</div>"
		  "<div class=\"footer\" style=\"test\"></div>"
		  ))

(defun devo-html-template (contents info)
    "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
    (concat
     (devo-html-header info)
     contents
     (devo-html-footer info)
     ))


(defun devo-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to pd custom HTML.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name."
  (org-publish-org-to 'devo-html filename ".html" plist pub-dir))
