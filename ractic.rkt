#lang racket

(require racket/file
         racket/path
         racket/date
         markdown
         markdown/parse
         html-parsing
         html-template)
(require markdown/display-xexpr)

(define (get-title fname) (regexp-replace #rx".md" (path->string(file-name-from-path fname)) ""))

(define (parse-markdown-file md-file)
  (call-with-input-file md-file
    (lambda (input-port)
      (let ([md-content (port->string input-port)])
        (string-append* (map xexpr->string (parse-markdown md-content)))))))

(define (html-file->string file-path)
  (call-with-input-file file-path
    (lambda (input-port)
      (port->string input-port))))

(define (expand-template-string template-string template-vars)
  (let loop ([result template-string]
             [vars template-vars])
    (if (empty? vars)
        result
        (let* ([var (car vars)]
               [placeholder (string-append "{{ " (car var) " }}")]
               [value (cdr var)]
               [updated-result (string-replace result placeholder value)])
          (loop updated-result (cdr vars))))))

(define (create-page mdfile output-dir base-page-html)
  ; Get the title from the Markdown file's name
  (define title (get-title mdfile))

  ; Parse the Markdown file into HTML content
  (define html-content (parse-markdown-file mdfile))

  ; Create the output file path
  (define output-file (build-path output-dir (path->string (file-name-from-path (path-replace-extension mdfile ".html")))))

  ; Define the template variables
  (define template-vars `(("title" . ,title) ("content" . ,html-content)))

  ; Read the base HTML file into a string
  (define base-html (html-file->string base-page-html))

  ; Replace the placeholders in the base HTML string with the values from the template-vars list
  (define final-html (expand-template-string base-html template-vars))

  ; Save the final HTML content to the output file
  (call-with-output-file output-file #:exists 'replace
    (lambda (output-port)
      (display final-html output-port))))


(define (create-pages-from-directory input-dir output-dir base-page-html)
  ; List all files in the input directory
  (define all-files (map (lambda (f) (build-path input-dir f)) (directory-list input-dir)))

  ; Filter the list to only include .md files
  (define md-files (filter (lambda (f) (equal? (path-get-extension f) #".md")) all-files))

  (define pages-dir (path->string (build-path output-dir "pages")))

  ; Check if the HTML file already exists in the output directory
  (define (html-file-exists? md-file)
    (let ([html-file (build-path pages-dir (path->string (file-name-from-path (path-replace-extension md-file ".html"))))])
      (file-exists? html-file)))

  ; Process each .md file and create an HTML page if it doesn't exist yet
  (for-each
    (lambda (md-file)
      (unless (html-file-exists? md-file)
        (create-page md-file pages-dir base-page-html)))
    md-files))


(define (create-index md-source-dir html-generated-dir output-dir base-page-html)
  ; List all files in the md-source-dir
  (define all-files (map (lambda (f) (build-path md-source-dir f)) (directory-list md-source-dir)))

  ; Filter the list to only include .md files, and store the last modification date
  (define md-files (filter (lambda (f) (equal? (path-get-extension f) #".md")) all-files))
  (define md-files+dates (map (lambda (f) (cons f (file-or-directory-modify-seconds f))) md-files))

  ; Sort the md-files list based on the last modification date
  (define sorted-md-files+dates (sort md-files+dates (lambda (a b) (> (cdr a) (cdr b)))))

  ; Calculate the relative path between the output directory and the HTML generated directory
  (define relative-path (find-relative-path output-dir html-generated-dir))

  ; Generate the HTML list of links
  (define index-content (string-append* (map (lambda (file+date)
                                               (let* ([md-file (car file+date)]
                                                      [last-modified (seconds->date (cdr file+date))]
                                                      [formatted-date (date->string last-modified "~B ~d, ~Y")]
                                                      [html-file (path->string (build-path "pages" (file-name-from-path (path-replace-extension md-file ".html"))))]
                                                      [title (get-title md-file)])
                                                 (format "<li><span>~a</span> <a href=\"~a\">~a</a></li>\n" formatted-date html-file title)))
                                             sorted-md-files+dates)))

  ; Create the index.html file using the base-page-html template
  (define index-file (build-path output-dir "index.html"))
  (define template-vars `(("title" . "Index") ("content" . ,index-content)))
  (define base-html (html-file->string base-page-html))
  (define final-html (expand-template-string base-html template-vars))

  ; Save the final HTML content to the index file
  (call-with-output-file index-file #:exists 'replace
    (lambda (output-port)
      (display final-html output-port))))

