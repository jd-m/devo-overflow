(require 'ox-publish)

(setq org-publish-project-alist
      '(
	("posts"
	 :base-directory "~/jd-m.github.io/posts/"
	 :base-extension "org"
	 :publishing-directory "~/jd-m.github.io/posts/"
	 :recursive nil
	 :publishing-function devo-html-publish-to-html
	 :headline-levels 4
	 :with-toc nil
	 :section-numbers nil
	 )
	
	("index"
	 :base-directory "~/jd-m.github.io/"
	 :base-extension "org"
	 :publishing-directory "~/jd-m.github.io/"
	 :recursive nil
	 :publishing-function devo-html-publish-to-html
	 :headline-levels 4
	 :with-toc nil
	 :section-numbers nil
	 )

	("devo-overflow" :components ("posts" "index"))
	))
