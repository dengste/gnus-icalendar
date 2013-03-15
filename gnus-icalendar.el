;; gnus-icalendar.el - icalendar support for Gnus
;; (C) 2013 David Engster <deng@randomsample.de>

;; This is pre-alpha.

(require 'icalendar)

(defvar gnus-icalendar-identities
  (list (regexp-quote user-mail-address) gnus-ignored-from-addresses))

(defvar gnus-icalendar-date-format-string "%d.%m.%Y (%a)")
(defvar gnus-icalendar-time-format-string "%H:%M")
(defvar gnus-icalendar-org-capture-key "c")
(defvar gnus-icalendar-org-format-string
  (concat
   "%S\n  Organizer: %O\n  Location: %L\n  Attendees (required): %R\n"
   "  Attendess (optional): %A\n  %T\n\n  %D"))

(add-to-list 'mm-automatic-display '"text/calendar")
(add-to-list 'mm-inline-media-tests '("text/calendar" mm-display-calendar-event identity))

(defun mm-display-calendar-event (handle)
  (let ((content (with-current-buffer (mm-handle-buffer handle)
		   (buffer-string)))
	(method (cdr (assoc 'method (mm-handle-type handle))))
	(charset (cdr (assoc 'charset (mm-handle-type handle))))
	cal event)
    (with-temp-buffer
      (insert content)
      (when (eq (mm-handle-encoding handle) 'base64)
	(base64-decode-region (point-min) (point-max)))
      (goto-char (point-min))
      (save-excursion
	(while (re-search-forward "\^M" nil t)
	  (replace-match "")))
      (save-excursion
	(while (re-search-forward "^ " nil t)
	  (delete-char -2)))
      (when (string= charset "utf-8")
	(decode-coding-region (point-min) (point-max) 'utf-8))
      (when (re-search-forward "^BEGIN:VCALENDAR\\s-*$" nil t)
	(goto-char (match-beginning 0))
	(setq cal (icalendar--read-element nil nil))))
    (when cal
      (let ((zones (icalendar--convert-all-timezones cal))
	    (events (icalendar--all-events cal)))
	(dolist (cur events)
	  (gnus-icalendar-handle-event cur zones method))

	))))

(defun gnus-icalendar-handle-event (event zones method)
  (let ((ed (gnus-icalendar-extract-event-data event zones)))
    (when (plist-get ed :rsvp)
      (gnus-icalendar-insert-button "Accept" 'gnus-icalendar-accept ed)
      (insert "    ")
      (gnus-icalendar-insert-button "Decline" 'gnus-icalendar-decline ed)
      (insert "    "))
    (gnus-icalendar-insert-button "Export to Org" 'gnus-icalendar-export-org ed)
    (insert "\n\n" (propertize (plist-get ed :summary) 'face 'bold))
    (insert (propertize "\n\nOrganizer: " 'face 'bold)
	    (gnus-icalendar-get-name (plist-get ed :organizer)))
    (when (plist-get ed :attendees-req)
      (insert (propertize "\n\nAttendees (required): " 'face 'bold)
	      (plist-get ed :attendees-req)))
    (when (plist-get ed :attendees-opt)
      (insert (propertize "\n\nAttendees (optional): " 'face 'bold)
	      (plist-get ed :attendees-opt)))
    (insert (propertize "\n\nAnswer required: " 'face 'bold)
	    (if (plist-get ed :rsvp) "Yes" "No"))
    (insert (propertize "\n\nDate: " 'face 'bold)
	    (gnus-icalendar-format-time
	     (concat gnus-icalendar-date-format-string ", "
		     gnus-icalendar-time-format-string)
	     (plist-get ed :dtstart))
	    (if (plist-get ed :sameday)
		(concat
		 "-"
		 (gnus-icalendar-format-time gnus-icalendar-time-format-string
					     (plist-get ed :dtend)))
	      (concat " -- "
		      (format-time-string
		       (concat gnus-icalendar-date-format-string ", "
			       gnus-icalendar-time-format-string)
		       (apply 'encode-time (plist-get ed :dtend))))))
    (when (plist-get ed :location)
      (insert (propertize "\n\nLocation: " 'face 'bold) (plist-get ed :location)))
    (when (plist-get ed :description)
      (insert "\n\n" (plist-get ed :description)))))

(defun gnus-icalendar-extract-event-data (event zones)
  (let* ((organizer (list
		     (icalendar--get-event-property-attributes event 'ORGANIZER)
		     (icalendar--get-event-property event 'ORGANIZER)))
	 (attendees (gnus-icalendar-get-attendees (icalendar--get-children event 'VEVENT)))
	 (attendees-req (gnus-icalendar-filter-attendees attendees 'ROLE "REQ-PARTICIPANT"))
	 (attendees-opt (gnus-icalendar-filter-attendees attendees 'ROLE "OPT-PARTICIPANT"))
	 (myself (or (gnus-icalendar-get-own-entry attendees)
		     (car attendees)))
	 (description (icalendar--get-event-property event 'DESCRIPTION))
	 (location (icalendar--get-event-property event 'LOCATION))
	 (summary (icalendar--get-event-property event 'SUMMARY))
	 (uid (icalendar--get-event-property event 'UID))
	 (rsvp (plist-get (car myself) 'RSVP))
	 (times (gnus-icalendar-get-times event zones))
	 (sameday (string= (gnus-icalendar-format-time "%d.%m.%Y" (car times))
			   (gnus-icalendar-format-time "%d.%m.%Y" (cadr times))))
	 data)
    (append
     (when organizer
       `(:organizer ,organizer))
     (when attendees-req
       `(:attendees-req
	 ,(icalendar--convert-string-for-import
	   (mapconcat
	    'gnus-icalendar-get-name attendees-req "; "))))
     (when attendees-opt
       `(:attendees-opt
	 ,(icalendar--convert-string-for-import
	   (mapconcat
	    'gnus-icalendar-get-name attendees-opt "; "))))
     (when myself
       `(:myself ,myself))
     (when description
       `(:description ,(icalendar--convert-string-for-import description)))
     (when summary
       `(:summary ,(icalendar--convert-string-for-import summary)))
     (when location
       `(:location ,(icalendar--convert-string-for-import location)))
     (when rsvp
       `(:rsvp ,(string= rsvp "TRUE")))
     (when (car times)
       `(:dtstart ,(car times)))
     (when uid
       `(:uid ,uid))
     (when (cadr times)
       `(:dtend ,(cadr times) :sameday ,sameday)))))


(defun gnus-icalendar-insert-button (text callback data)
    (let ((start (point)))
      (gnus-add-text-properties
       start
       (progn
	 (insert "[ " text " ]")
	 (point))
       `(gnus-callback
	 ,callback
	 keymap ,gnus-mime-button-map
	 face ,gnus-article-button-face
	 gnus-data ,data))
      (widget-convert-button 'link start (point)
			     :action 'gnus-widget-press-button
			     :button-keymap gnus-widget-button-keymap)))

(defun gnus-icalendar-export-org (ed)
  (with-temp-buffer
    (insert "* " gnus-icalendar-org-format-string)
    (goto-char (point-min))
    (let ((repl '(("%S" . :summary)
		  ("%D" . :description)
		  ("%L" . :location)
		  ("%R" . :attendees-req)
		  ("%A" . :attendess-opt)
		  ("%O" . :organizer)
		  ("%T" . :time))))
      (dolist (cur repl)
	(save-excursion
	  (when (search-forward (car cur) nil t)
	    (cond
	     ((string= (car cur) "%O")
	      (if (plist-get ed :organizer)
		  (replace-match (gnus-icalendar-get-name (plist-get ed :organizer)) t t)
		(replace-match "")))
	     ((string= (car cur) "%T")
	      (replace-match
	       (gnus-icalendar-create-org-timestamp (plist-get ed :dtstart)
						    (plist-get ed :dtend)
						    (plist-get ed :sameday))
	       t t))
	     (t
	      (if (plist-get ed (cdr cur))
		  (replace-match (plist-get ed (cdr cur)) t t)
		(replace-match ""))))))))
    (goto-char (point-min))
    (org-mode)
    (when (plist-get ed :uid)
      (org-set-property "ID" (plist-get ed :uid)))
    (if gnus-icalendar-org-capture-key
	(progn
	  (kill-new (buffer-substring (+ (point-min) 2) (point-max)))
	  (org-capture nil gnus-icalendar-org-capture-key))
      (kill-new (buffer-string))
      (message "Org item copied to kill-ring."))))

(defun gnus-icalendar-accept (ed)
  (message "Not implemented."))

(defun gnus-icalendar-decline (ed)
  (message "Not implemented."))

(defun gnus-icalendar-create-org-timestamp (start end sameday)
  (concat
   (gnus-icalendar-format-time "<%Y-%m-%d %a %H:%M" start)
   (if sameday
       (concat
	"-"
	(gnus-icalendar-format-time "%H:%M" start)
	">")
     (concat ">--<"
	     (gnus-icalendar-format-time "%Y-%m-%d %a %H:%M>" end)))))


(defsubst gnus-icalendar-format-time (formatstr timelist)
  (format-time-string formatstr (apply 'encode-time timelist)))

(defun gnus-icalendar-get-attendees (event)
  (let ((ev (icalendar--get-children event 'VEVENT)))
    (delq nil
	  (mapcar (lambda (x)
		    (when (eq (car x) 'ATTENDEE)
		      (cdr x)))
		  (car (cddr event))))))

(defun gnus-icalendar-get-name (entry)
  (or
   (plist-get (car entry) 'CN)
   (progn (string-match "\\(MAILTO:\\)?\\(.+\\)" (cadr entry))
	  (match-string 2 (cadr entry)))))

(defun gnus-icalendar-filter-attendees (attendees attribute value)
  (delq nil
	(mapcar
	 (lambda (att)
	   (when (string= (plist-get (car att) attribute) value)
	     att))
	 attendees)))

(defun gnus-icalendar-get-own-entry (attendees)
  (if (= (length attendees) 1)
      (car attendees)
    (catch 'found
      (dolist (id gnus-icalendar-identities)
	(let ((att attendees))
	  (while (and att
		      (not (string-match
			    id
			    (cadr (car att))))
		      (setq att (cdr att))))
	  (when (car-safe att)
	    (throw 'found (car att))))))))

(defun gnus-icalendar-get-times (event zone-map)
  (let* ((calendar-date-style 'european)
	 (dtstart (icalendar--get-event-property event 'DTSTART))
	 (dtstart-zone (icalendar--find-time-zone
			(icalendar--get-event-property-attributes
			 event 'DTSTART)
			zone-map))
	 (dtstart-dec (icalendar--decode-isodatetime dtstart nil
						     dtstart-zone))
	 (dtend (icalendar--get-event-property event 'DTEND))
	 (dtend-zone (icalendar--find-time-zone
		      (icalendar--get-event-property-attributes
		       event 'DTEND)
		      zone-map))
	 (dtend-dec (icalendar--decode-isodatetime dtend
						   nil dtend-zone)))
    (list dtstart-dec dtend-dec)))
