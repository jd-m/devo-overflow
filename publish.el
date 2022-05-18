(require 'ox-publish)

(setq org-publish-project-alist
      '(
	("posts"
	 :base-directory "~/jd-m.github.io/posts/"
	 :base-extension "org"
	 :publishing-directory "~/jd-m.github.io/articles/"
	 :recursive nil
	 :publishing-function devo-html-publish-to-html
	 :headline-levels 4
	 :with-toc nil
	 :html-head   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <script src=\"../jquery-3.6.0.min.js\"></script>
  
   <link rel=\"stylesheet\" href=\"../site.css\">"
	 )

	("static"
	 :base-directory "~/jd-m.github.io/posts/"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	 :publishing-directory "~/jd-m.github.io/public/"
	 :recursive t

	 :publishing-function org-publish-attachment

	 )

	("posts" :components ("posts" "static"))
	))
