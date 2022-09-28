(require 'ox-publish)

(setq org-publish-project-alist
      '(
	("tlac-posts"
	 :base-directory "~/jd-m.github.io/tlac-site/posts/"
	 :base-extension "org"
	 :publishing-directory "~/jd-m.github.io/tlac-site/posts/"
	 :recursive nil
	 :publishing-function tlac-html-publish-to-html
	 :headline-levels 4
	 :with-toc nil
	 :section-numbers nil
	 )
	
	("tlac-index"
	 :base-directory "~/jd-m.github.io/tlac-site"
	 :base-extension "org"
	 :publishing-directory "~/jd-m.github.io/tlac-site/"
	 :recursive nil
	 :publishing-function tlac-html-publish-to-html
	 :headline-levels 4
	 :with-toc nil
	 :section-numbers nil
	 )

	("tlac-site" :components ("tlac-posts" "tlac-index"))
	
      
      ("devo-posts"
	 :base-directory "~/jd-m.github.io/devo-overflow/posts/"
	 :base-extension "org"
	 :publishing-directory "~/jd-m.github.io/devo-overflow/posts/"
	 :recursive nil
	 :publishing-function devo-html-publish-to-html
	 :headline-levels 4
	 :with-toc nil
	 :section-numbers nil
	 )
	
	("devo-index"
	 :base-directory "~/jd-m.github.io/devo-overflow"
	 :base-extension "org"
	 :publishing-directory "~/jd-m.github.io/devo-overflow/"
	 :recursive nil
	 :publishing-function devo-html-publish-to-html
	 :headline-levels 4
	 :with-toc nil
	 :section-numbers nil
	 )

	("devo-overflow" :components ("devo-posts" "devo-index")))
      )
