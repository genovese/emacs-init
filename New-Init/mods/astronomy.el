(defun astronomy-twilight-time-to-time-string (time-list)
  "Converts a twilight time list to a time string.
See `solar-astronomical-twilight' from astronomy.el, for instance.
TIME-LIST is of the form (TIME ZONE) where TIME is a decimal time
and ZONE is a string, e.g., EST. The result is converted into an
hh:mm ZONE string. Treats the boundary case of a time within 30
seconds of midnight by saying \"midnight\"."
  (let* ((hour (floor (car time-list)))
         (min (round (* 60.0 (- (car time-list) hour))))
         (zone (cadr time-list)))
    (format "%s %s"
            (if (and (= min 60) (= hour 23))
                "midnight"
              (format "%02d:%02d" hour min))
            zone)))

(defun calendar-astronomical-twilight (&optional event)
  "Local time of astronomical twilight, morning and evening,
for date under cursor. Accurate to a few seconds."
  (interactive (list last-nonmenu-event))
  (or (and calendar-latitude calendar-longitude calendar-time-zone)
      (solar-setup))
  (let* ((date (calendar-cursor-to-date t event))
         (twilight (solar-astronomical-twilight date))
         (morning (car twilight))
         (evening (cadr twilight)))
    (message "Astronomical twilight on %s: %s (morning) and %s (evening)"
             (calendar-date-string date t t)
             (astronomy-twilight-time-to-time-string morning)
             (astronomy-twilight-time-to-time-string evening))))
