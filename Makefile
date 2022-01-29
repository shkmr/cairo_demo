###
###
###
snippets.html : snippets.scm snippets-html.scm cairo.scm
	gosh -I. snippets-html.scm snippets.scm

clean:
	rm -f snippets.html snippet-*.png


