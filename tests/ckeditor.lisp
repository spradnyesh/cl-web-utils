(in-package :web-utils-tests)

(def-suite :ckeditor :in :web-utils)
(in-suite :ckeditor)

;; ck-js
(test ck-js
  (is (equal (macroexpand-1 '(ck-js "l"))
             '(FMTNIL (<:SCRIPT :TYPE "text/javascript" :SRC "/static/ckeditor/ckeditor.js")
               (<:SCRIPT :TYPE "text/javascript" :SRC
                "/static/ckeditor/adapters/jquery.js")
               (<:SCRIPT :TYPE "text/javascript"
                (FMTNIL "$('.ckeditor td textarea').ckeditor();"))
               (UNLESS (STRING= "en-IN" "l")
                 (<:SCRIPT :TYPE "text/javascript" "
CKEDITOR.on('instanceReady', function(e) {
    e.editor.document.getBody().setStyle('font-family', 'Lohit Devanagari');
    e.editor.on('mode', function(a) {
        if (a.data.previousMode == 'source') {
            a.editor.document.getBody().setStyle('font-family', 'Lohit Devanagari');
        } else { // a.data.previousMode == 'wysiwyg'
            a.editor.textarea.setStyle('font-family', 'Lohit Devanagari');
        }
    });
});
"))))))

;; cleanup-ckeditor-text
(test cleanup-ckeditor-text
  (is (string-equal (cleanup-ckeditor-text "<p>
	&nbsp;</p>abc<p>
	&nbsp;</p><div>
	&nbsp;</div>def<div>
	&nbsp;</div>")
                    "abcdef")))
