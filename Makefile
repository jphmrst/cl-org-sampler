
README.md: README.org org-sampler.asd package.lisp sampler.lisp
	mlisp -e "(require 'asdf)" \
		-e '(push #p"./" asdf:*central-registry*)' \
		-e '(push #p"~/Lib/Lisp/iterate/" asdf:*central-registry*)' \
		-e '(asdf:load-system :org-sampler)' \
		-e '(org-sampler:self-document)' \
		-kill
	emacs $< --eval '(org-gfm-export-to-markdown)' \
	         --eval '(org-latex-export-to-pdf)' \
		 --eval '(save-buffers-kill-terminal)'
	mv README.pdf OrgSampler.pdf
