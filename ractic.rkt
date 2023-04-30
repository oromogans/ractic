#lang racket

(require racket/file
         racket/path
         markdown
         markdown/parse
         html-parsing
         html-template)
(require markdown/display-xexpr)

(define MDFILE (string->path "C:\\Users\\gans\\crumbs\\Obsidian\\Квантик.md"))

;; (parse-markdown MDFLE)

;;(define title (regexp-replace #rx"[_-]" (path->string (path-strip-extension (file-name-from-path md-file))) " "))

(define (get-title fname) (regexp-replace #rx".md" (path->string(file-name-from-path fname)) ""))
(define title (get-title MDFILE))

;; Let's parse markdown to 
(define html-content (parse-markdown MDFILE))
;;(define output-file (string-append "C:\\Users\\gans\\projects\\ractic\\output" "/" (path->string (file-name-from-path (path-replace-extension md-file ".html")))))
(define output-file (build-path "C:\\Users\\gans\\projects\\ractic\\output" (path->string (file-name-from-path (path-replace-extension MDFILE ".html")))))
(define template-vars `(("title" . ,title) ("content" . ,html-content)))

(define (html-file->string file-path)
  (call-with-input-file file-path
    (lambda (input-port)
      (port->string input-port))))

(define base-html (html-file->string "templates/base.html"))