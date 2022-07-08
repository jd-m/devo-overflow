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
		   (:devo-post-tags "DEVO_POST_TAGS" "devo-post-tags" nil space)
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
	(devo-post-tags (plist-get info :devo-post-tags)))
    (concat
     "<head>\n"
     "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
     
     "<script src=\"/js/jquery-3.6.0.min.js\"></script>\n"
     (if title (format "<meta name=\"post-title\" content=\"%s\">\n" title) "")
     (if date (format "<meta name=\"date\" content=\"%s\">\n" (format "%s" (plist-get info :date))) "")
     (if devo-post-tags (format "<meta name=\"tags\" content=\"%s\">\n" devo-post-tags) "")
     
     "<link rel=\"stylesheet\" href=\"/css/site.css\">\n"
     "</head>\n"

     "<div class=\"header col-12 test\">"
     "<a href=\"/index.html\">"
     "<h1 class=\"center\">Devo_Overflow</h1>"
     "<div class=\"triple-line\">"
     "<hr style=\"margin-left:40%; margin-right:40%;\">"
     "<hr style=\"margin-left:45%; margin-right:45%;\">"
     "<hr style=\"margin-left:48%; margin-right:48%;\">"
     "</div>"
     "</a>"
     "</div>"
     "<div class=\"col-3 side disappear\">"
     "<a href=\"/post-series.html\"><h2>Series</h2></a>"
     "<ul id=\"series\">"
     "<li><a href=\"/post-series.html#20220602142630\">Mining for Wisdom</a></li>"
     "<li><a href=\"/post-series.html#20220602191239\">Devotional</a></li>"
     "<li><a href=\"/post-series.html#20220602191220\">Teach Like A Christian</a></li>"
     "</ul>"
     "</div>"
     )))


(defun devo-html-footer (info)
  ""
  (concat
   "<hr>"   
   "<div class=\"footer\" style=\"test\"></div>"
   ))

;Templates

(defun devo-html-template (contents info)
    "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
    (let
	((devo-title-headline (plist-get info :devo-title-headline))
	 (title (car (plist-get info :title)))
	 (date (plist-get info :date)))
      (concat
       (devo-html-header info)     
       "<div class=\"col-6 test content center\">"
       (if devo-title-headline (concat "<h1>" title "</h1>\n") "")
       (if date
       (format "<span class=\"date\">%s</span>\n\n"  (format "%s" (plist-get info :date))) "")
       contents
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
