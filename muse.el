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
    map)
  "Keymap for Muse major mode.")

;; The main function to setup our buffer for muse mode
(defun muse-mode ()
  "Major mode for browsing and downloading videos from muse."
  (interactive)
  (kill-all-local-variables)
  (use-local-map muse-mode-map)

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

;; TODO: use https://github.com/TheZoraiz/ascii-image-converter
(defun muse-search ()
  "Search for muses, parse the JSON results, and display them in a custom buffer."
  (interactive)
  (let* ((query (read-string "Search query: "))
         (raw-json (shell-command-to-string (format "/home/harry/code/rust/muse/target/debug/muse search \"%s\"" query)))
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
      (async-shell-command (format "/home/harry/code/rust/muse/target/debug/muse download %s \"%s\"" id directory))
      (message "Downloading video with id: %s" id))))



;; Add the mode to the `features` list
(provide 'muse-mode)
