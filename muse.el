;; Define a new buffer name for our mode
(defvar muse-mode-buffer-name "*muse*")

;; Define a hook for users to run their own code when entering this mode
(defvar muse-mode-hook nil)

(defvar muse-geckodriver-process nil
  "Process handle for the geckodriver server.")

(defvar muse-active-search-buffers 0
  "Number of active search buffers.")

(defun muse-port-occupied-p (port)
  "Check if a port is occupied."
  (condition-case nil
      (let ((connection (open-network-stream
                         "port-check" nil "localhost" port
                         :type 'plain :nowait nil)))
        (delete-process connection)
        t)  ; Return t if connection succeeds, implying the port is occupied.
    (file-error nil)))  ; Return nil if connection fails, implying the port is likely free.


(defun muse-start-geckodriver ()
  "Starts the geckodriver server on port 4444 if it isn't already running."
  (interactive)
  (unless (or (process-live-p muse-geckodriver-process)
              (muse-port-occupied-p 4444))
    (setq muse-geckodriver-process
          (start-process "geckodriver" "*geckodriver*" "geckodriver" "--port" "4444"))
    (message "Geckodriver started on port 4444")))


(defun muse-kill-geckodriver ()
  "Kills the running geckodriver process if any."
  (interactive)
  (when (process-live-p muse-geckodriver-process)
    (kill-process muse-geckodriver-process)
    (setq muse-geckodriver-process nil)
    (message "Geckodriver process killed.")))

(defun muse-decrement-search-buffer-count ()
  "Decrement the search buffer count and kill geckodriver if it's the last one."
  (setq muse-active-search-buffers (1- muse-active-search-buffers))
  (when (and (= muse-active-search-buffers 0) (process-live-p muse-geckodriver-process))
    (muse-kill-geckodriver)))

;; A variable to contain the map of keybindings for the mode
(defvar muse-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'muse-search)
    (define-key map (kbd "d") 'muse-download-video-at-point)
    (define-key map (kbd "v") 'muse-view-video-at-point)
    (define-key map (kbd "o") 'muse-open-video-at-point)
    map)
  "Keymap for Muse major mode.")

;; The main function to setup our buffer for muse mode
(defun muse-mode ()
  "Major mode for browsing and downloading videos from muse."
  (interactive)
  (kill-all-local-variables)
  (use-local-map muse-mode-map)
  (muse-start-geckodriver)

  ;;(set (make-local-variable 'font-lock-defaults) '(muse-mode-keywords))

  ;; Define how text in the buffer should be highlighted
  ;; Add specific highlighting rules in `muse-mode-keywords`
  (set (make-local-variable 'muse-mode-keywords) '())

  ;; Set the mode name displayed in the status bar
  (setq major-mode 'muse-mode)
  (setq mode-name "muse")

  ;; Run hooks defined by the user or other packages
  (run-hooks 'muse-mode-hook))

(define-derived-mode muse-mode fundamental-mode "Muse"
  "Major mode to manage Youtube music via the muse CLI."
  (use-local-map muse-mode-map))


(defun muse-search ()
  "Search for videos, parse the JSON results, and display them in a custom buffer."
  (interactive)
  (muse-start-geckodriver)
  (let* ((query (read-string "Search query: "))
         (raw-json (shell-command-to-string (format "muse search \"%s\"" query)))
         (json-vector (json-read-from-string raw-json))
         (buffer-name "*Muse Search Results*"))
    (with-current-buffer (get-buffer-create buffer-name)
      ;; Increment the count of active search buffers
      (setq muse-active-search-buffers (1+ muse-active-search-buffers))
      ;; Make sure the count is correctly decremented when this buffer is killed
      (add-hook 'kill-buffer-hook 'muse-decrement-search-buffer-count nil t)

      (let ((buffer-read-only nil))  ;; Temporarily make buffer writable
        (erase-buffer) ;; Clear any previous searches
        (muse-mode)  ;; Switch to your custom mode
        ;; Loop through each video in the result and insert its details
        (cl-loop for video across json-vector do
                 (let ((title (cdr (assoc 'title video)))
                       (id (cdr (assoc 'id video)))
                       (url (cdr (assoc 'url video))))
                   (insert (format "%s | %s | %s\n" id title url))))
        ;; (Optional) Make buffer read-only again after insertion
        (setq buffer-read-only t))
      ;; Display the buffer to the user
      (switch-to-buffer-other-window buffer-name))))


(defun muse-download-video-at-point ()
  "Download the video on the current line, using only its ID."
  (interactive)
  (message "begin download function")
  ;; Get the current line at point.
  (let* ((line (thing-at-point 'line t))
         ;; Split the line using "|" as the delimiter.
         (line-components (split-string line "|" t))
         ;; Extract the ID (the first component) and trim whitespace.
         (id (string-trim (nth 0 line-components))))

    ;; Debugging: Print line components and ID
    (message "Line components: %s" line-components)
    (message "Extracted ID: %s" id)

    ;; Prompt the user to choose a directory for downloading.
    (let ((directory (read-directory-name "Download directory: ")))
      ;; Perform the download using the obtained ID and directory.
      (async-shell-command (format "muse download %s \"%s\"" id directory))
      (message "Downloading video with id: %s" id))))

(defun muse-view-video-at-point ()
  "View the video on the current line, using only its ID."
  (interactive)
  ;; Get the current line at point.
  (let* ((line (thing-at-point 'line t))
         ;; Split the line using "|" as the delimiter.
         (line-components (split-string line "|" t))
         ;; Extract the ID (the first component) and trim whitespace.
         (id (string-trim (nth 0 line-components)))
         ;; Call the CLI and get the JSON output as a string.
         (json-string (shell-command-to-string (format "muse view %s" id)))
         ;; Parse the JSON string into an Emacs Lisp data structure.
         (json-data (json-read-from-string json-string))
         (buffer-name "*Muse Video View*"))

    ;; Debugging: Print ID and JSON string for verification.
    ;; (message "Extracted ID: %s" id)
    ;; (message "JSON string: %s" json-string)

    ;; Setup the new buffer.
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((buffer-read-only nil))  ;; Temporarily make buffer writable
        (erase-buffer) ;; Clear the buffer
        (muse-mode)  ;; Switch to your custom mode

        ;; Insert video details
        (let ((ascii-art (cdr (assoc 'image json-data)))
              (title (cdr (assoc 'title json-data)))
              (channel (cdr (assoc 'channel json-data)))
              (url (cdr (assoc 'url json-data)))
              (sidebar (append (cdr (assoc 'sidebar json-data)) nil)))
          (insert "Video Details:\n")
          (insert (format "Title: %s\n" title))
          (insert (format "Channel: %s\n" channel))
          (insert (format "URL: %s\n\n" url))

          ;; Position ASCII art
          (insert "Thumbnail:\n" ascii-art "\n\n")

          ;; List sidebar videos
          (insert "Sidebar Videos:\n")
          (dolist (item sidebar)
            (let ((sb-title (cdr (assoc 'title item)))
                  (sb-id (cdr (assoc 'id item)))
                  (sb-url (cdr (assoc 'url item))))
              (insert (format "%s | %s | %s\n" sb-id sb-title sb-url)))))

        ;; (Optional) Make buffer read-only again after insertion
        (setq buffer-read-only t))
      ;; Display the buffer to the user
      (switch-to-buffer-other-window buffer-name))))

(defun muse-open-video-at-point ()
  "Open the YouTube link at the current line in the default web browser."
  (interactive)
  ;; Get the current line at point.
  (let* ((line (thing-at-point 'line t))
         ;; Split the line using "|" as the delimiter and trim each part.
         (line-components (mapcar 'string-trim (split-string line "|" t))))
    ;; The YouTube link is expected to be the last component of the line.
    (let ((youtube-link (car (last line-components))))
      ;; Check if the YouTube link looks like a valid URL.
      (if (and youtube-link (string-prefix-p "http" youtube-link))
          (browse-url youtube-link)  ;; Open the link in the default web browser.
        (message "No valid YouTube link found on the current line.")))))

;; Add the mode to the `features` list
(provide 'muse-mode)
