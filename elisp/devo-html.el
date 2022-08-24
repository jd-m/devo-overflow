(org-export-define-derived-backend 'devo-html 'html
  :translate-alist '(
		     (template . devo-html-template)
		     (headline . devo/ox-slimhtml-headline)
		     (example-block . ox-slimhtml-example-block)
		     (export-block . ox-slimhtml-export-block)
		     (export-snippet . ox-slimhtml-export-snippet)
		     (inner-template . ox-slimhtml-inner-template)
		     (italic . ox-slimhtml-italic)
		     (item . org-html-item)
		     (paragraph . ox-slimhtml-paragraph)
		     (plain-list . ox-slimhtml-plain-list)
		     (plain-text . ox-slimhtml-plain-text)
		     (section . ox-slimhtml-section)
		     (special-block . ox-slimhtml-special-block)
		     (src-block . ox-slimhtml-src-block)
		     (verbatim . ox-slimhtml-verbatim)
		     (timestamp . devo/ox-html-timestamp)
		     )
  
  :options-alist '(
		   (:html-extension "HTML_EXTENSION" nil org-html-extension)
		   (:html-link-org-as-html nil "html-link-org-files-as-html" org-html-link-org-files-as-html)
		   (:html-doctype "HTML_DOCTYPE" nil org-html-doctype)
		   (:html-container "HTML_CONTAINER" nil org-html-container-element t)
		   (:html-link-use-abs-url nil "html-link-use-abs-url" org-html-link-use-abs-url)
		   (:html-link-home "HTML_LINK_HOME" nil org-html-link-home)
		   (:html-preamble "HTML_PREAMBLE" nil "" newline)
		   (:html-postamble "HTML_POSTAMBLE" nil "" newline)
		   (:html-head "HTML_HEAD" nil org-html-head newline)
		   (:html-head-extra "HTML_HEAD_EXTRA" nil org-html-head-extra newline)
		   (:html-header "HTML_HEADER" nil "" newline)
		   (:html-footer "HTML_FOOTER" nil "" newline)
		   (:html-title "HTML_TITLE" nil "%t" t)
		   (:html-body-attr "HTML_BODY_ATTR" nil "" t)
		   (:devo-title-headline "DEVO_TITLE_HEADLINE" "devo-title-headline" nil t)
		   (:devo-title-headline-class "DEVO_TITLE_HEADLINE_CLASS" "devo-title-headline-class" nil space)
		   (:post-class "POST_CLASS" ":post-class" nil space)
		   (:page-css "PAGE_CSS" ":page-css" nil t)
		   (:post-image "POST_IMAGE" ":post-image" nil t)
		   (:page-type "PAGE_TYPE" ":page-type" nil t)
		   (:post-type "POST_TYPE" ":post-type" nil t)
		   (:description "DESCRIPTION" ":description" nil space)
		   (:snippet "SNIPPET" ":snippet" nil space)
		   (:devo-post-tags "DEVO_POST_TAGS" ":devo-post-tags" nil space)
		   (:devo-share-links "DEVO_SHARE_LINKS" ":devo-share-links" nil t)
		   (:date "DATE" "date" nil t)
		   )
  )


(defun devo/ox-slimhtml-headline (headline contents info)
  "Transcode HEADLINE from Org to HTML.
CONTENTS is the section as defined under the HEADLINE.
INFO is a plist holding contextual information."
  (let* ((text (org-export-data (org-element-property :title headline) info))
         (level (1+ (org-export-get-relative-level headline info)))
         (attributes (org-element-property :ATTR_HTML headline))
         (container (org-element-property :HTML_CONTAINER headline))
         (container-class (and container (org-element-property :HTML_CONTAINER_CLASS headline))))
    (when attributes
      (setq attributes
            (format " %s" (org-html--make-attribute-string
                           (org-export-read-attribute 'attr_html `(nil
                                                                   (attr_html ,(split-string attributes))))))))
    (concat
     (when (and container (not (string= "" container)))
       (format "<%s%s>" container (if container-class (format " class=\"%s\"" container-class) "")))
     (if (not (org-export-low-level-p headline info))
         (format "<h%d%s>%s</h%d>%s" level (or attributes "") text level (or contents ""))
       (concat
        (when (org-export-first-sibling-p headline info) "<ul>")
        (format "<li>%s%s</li>" text (or contents ""))
        (when (org-export-last-sibling-p headline info) "</ul>")))
     (when (and container (not (string= "" container)))
       (format "</%s>" (cl-subseq container 0 (cl-search " " container)))))))

(defun devo/ox-html-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-html-plain-text (org-timestamp-translate timestamp) info)))
    (format "<span class=\"timestamp\">%s</span>"
	    (replace-regexp-in-string "--" "&#x2013;" value))))

(defun devo-html-header (info)
  ""
  (let ((title (car (plist-get info :title)))
	(date (plist-get info :date))
	(post-image (plist-get info :post-image))
	(page-type (plist-get info :page-type))
	(post-type (plist-get info :post-type))
	(page-css (plist-get info :page-css))
	(description (plist-get info :description))
	(snippet (plist-get info :snippet))
	(devo-post-tags (plist-get info :devo-post-tags)))
    (concat
     "<head>\n"
     "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"    
     "<script src=\"/js/jquery-3.6.0.min.js\"></script>\n"
					;site-name
     "<meta property=\"og:site_name\" content=\"Precious not Worthless\">"
     
					;title
     (if title (format "<meta property=\"og:title\" content=\"%s\"/>\n" title) "")
     
					;type
     (if page-type (format "<meta property=\"og:type\" content=\"%s\"/>\n" page-type) "")

					;type
     (if post-type (format "<meta property=\"post_type\" content=\"%s\"/>\n" post-type) "")
     
					;url
     (format "<meta property=\"og:url\" content=\"%s\"/>\n"  (concat "http://jd-m.github.io/posts/" (file-name-base (buffer-file-name)) ".html" ))
     
					;image
     (if post-image (format "<meta property=\"og:image\" content=\"https://jd-m.github.io/img/%s\"/></n>" post-image) "")

     (if date (format "<meta property=\"date\" content=\"%s\"/>\n" (format "%s" (plist-get info :date))) "")

     (if devo-post-tags (format "<meta property=\"og:tags\" content=\"%s\">\n" devo-post-tags) "")

     (if description (format
		      "<meta property=\"og:description\" content=\"%s\"/>"
		      description) "")

     (if snippet (format
		  "<meta property=\"snippet\" content=\"%s\"/>"
		  snippet) "")

     (format "<link rel=\"stylesheet\" href=\"/css/%s\"/>\n" (if page-css page-css "site-minimal.css"))
     
     "<link rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css?family=Raleway\">\n"
     "<link rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css?family=Arvo\">\n"
     "<link rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css?family=Josefin Sans\">\n"
     "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css\"/>"
     "</head>\n"

     "<div class=\"col-12\">"
     
     "<div class=\"header\">"
     
     "<div class=\"site-top\">"
     "<a href=\"/index.html\">"
     "<div class=\"site-name\">Precious not Worthless</div>"
     "</a>"
     "<div class=\"site-description\">Slightly longer site descrtiption</div>"
     "</div>"
     
     "<div class=\"navbar disappear\">"
     "<a href=\"/index.html\">Posts</a>"
     "<a href=\"/about.html\">About</a>"
     "</div>"

     "<div class=\"dropdown-icon appear\" >"
     "<hr style=\"width: 30px\">"
     "<hr style=\"width: 30px\">"
     "<hr style=\"width: 30px\">"
     "</div>"

     "<script>"
     "$(\".dropdown-icon\").click(function(){
          $(\".drop-navbar\").toggleClass(\"hide\");
      })"
     "</script>"
     "</div>"

     "<div class=\"drop-navbar hide\">"
     "<ul>"

     "<a href=\"/index.html\"><li>Posts</li></a>"

     "<a href=\"/about.html\"><li>About</li></a>"
    
     "</ul>"
     "</div>"
     
     "</div>"

     )))



(defun devo-html-footer (info)
  ""
  (concat
   "<div class=\"footer\" style=\"\">\n"

   
   
   "</div>"
   ))

					;Templates

(defun devo-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let
      ((devo-title-headline (plist-get info :devo-title-headline))
       (devo-title-headline-class (plist-get info :devo-title-headline-class))
       (devo-share-links (plist-get info :devo-share-links))
       (post-class (plist-get info :post-class))
       (title (car (plist-get info :title)))
       (date (plist-get info :date))
       (post-type (plist-get info :post-type))
       (post-image (plist-get info :post-image))
       )
    
    (concat
     (devo-html-header info)

     (format "<div class=\"clear content %s\">" (if post-class (format " %s " post-class) ""))

     "<div class=\"col-6 indent-3 center\">"
     
     (if devo-title-headline (concat "<h1 " (if devo-title-headline-class (format "class=\"%s\"" devo-title-headline-class )) ">" title "</h1>\n") "")
     
     (if date
	 (format "<div class=\"article-date\">%s  &nbsp &#9679 &nbsp  %s</div>\n\n" date post-type) "")
     
     "</div>"

     (if post-image (format 
		     "<div class=\"col-12 article center\">\n
       <img src=\"/img/%s\"/>\n
       </div>" post-image) "")
     
     "<div class=\"col-6 indent-3 center\">"

     contents
     
     "</div>"
     "</div>"
     
     (devo-html-footer info)
     )))


(defun devo-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to pd custom HTML.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name."
  (org-publish-org-to 'devo-html filename ".html" plist pub-dir))



(defun devo-insert-share-links ()
  ""
  (interactive)
  (insert "#+begin_src emacs-lisp :results html :exports results
(let((file-name (file-name-base (buffer-file-name))))
(format \"
<span>Share to: </span>
<a class=\\\"share-btn\\\" href=\\\"https://www.facebook.com/sharer/sharer.php?u=jd-m.github.io/posts/%s.html\\\">Facebook</a>

<a class=\\\"share-btn\\\" href=\\\"https://twitter.com/intent/tweet?url=%s\\\">Twitter</a>

\" file-name file-name))
#+end_src"
))
					;


