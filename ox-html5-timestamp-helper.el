;;; ox-html5-timestamp-helper.el --- HTML5 <time> tag support for Org HTML and slimhtml export backends -*- lexical-binding: t; -*-
;; Copyright (c) 2020-2021 Chris Charabaruk

;; Author: Chris Charabaruk <emacs at chris dot charabaruk dot com>
;; Created: November 2020
;; Package-Version: 0.1
;; Keywords: org org-export ox-html ox-slimhtml
;; Homepage: https://github.com/coldacid/ox-html5-timestamp-helper
;; Package-Requires: ((emacs "24") (org "9.4"))

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides HTML5 friendly <time> tags for the HTML and slimhtml
;; back-ends for Org generic exporter.

;;; Code:

;;; Dependencies

(require 'ox-html)
(require 'ox-icalendar)


;;; Internal Functions

(defun ox-html5--convert-timestamp (timestamp &optional tz)
  "Convert TIMESTAMP to HTML5 <time> datetime attribute format.

TIMESTAMP is a timestamp object.
TZ is an optional time zone identifier."
  (let* ((year-start (org-element-property :year-start timestamp))
         (year-end (org-element-property :year-end timestamp))
         (month-start (org-element-property :month-start timestamp))
         (month-end (org-element-property :month-end timestamp))
         (day-start (org-element-property :day-start timestamp))
         (day-end (org-element-property :day-end timestamp))
         (hour-start (org-element-property :hour-start timestamp))
         (hour-end (org-element-property :hour-end timestamp))
         (minute-start (org-element-property :minute-start timestamp))
         (minute-end (org-element-property :minute-end timestamp))
         (with-time-p minute-start)
         (equal-bounds-p
          (equal (list year-start month-start day-start hour-start minute-start)
                 (list year-end month-end day-end hour-end minute-end)))
         (start-time
          (encode-time 0
                       (if (not with-time-p) 0 minute-start)
                       (if (not with-time-p) 0 hour-start)
                       day-start
                       month-start
                       year-start))
         (end-time
          (if (not equal-bounds-p)
              (let* ((mi (cond ((not with-time-p) 0)
                               ((and org-agenda-default-appointment-duration equal-bounds-p)
                                (+ minute-end org-agenda-default-appointment-duration))
                               (t minute-end)))
                     (h (cond ((not with-time-p) 0)
                              ((or (not equal-bounds-p)
                                   org-agenda-default-appointment-duration)
                               hour-end)
                              (t (+ hour-end 2))))
                     (d (cond ((not with-time-p) (1+ day-end))
                              (t day-end))))
                (encode-time 0 mi h d month-end year-end))
            nil))
         (format-string (cond ((string-equal tz "UTC") "%Y-%m-%dT%H:%M:%SZ")
                              ((not with-time-p) "%Y-%m-%d")
                              ((stringp tz) (concat "%Y-%m-%dT%H:%M:%S" tz))
                              (t (replace-regexp-in-string "%Z"
                                                          org-icalendar-timezone
                                                          org-icalendar-date-time-format
                                                          t))))
         (time-zone (and (or (string-equal tz "UTC")
                             (and (null tz)
                                  with-time-p
                                  (org-icalendar-use-UTC-date-time-p)))
                         t))
         (start-formatted (format-time-string format-string start-time time-zone))
         (end-formatted (if (not (null end-time)) (format-time-string format-string end-time time-zone)
                          nil)))
    (if (not (null end-formatted)) (concat start-formatted "/" end-formatted)
      start-formatted)))


;;; Transcode Functions

;;;; ox-html

(defun ox-html5-html-timestamp (timestamp contents info)
  "Transcode TIMESTAMP from Org to HTML5 for the ox-html backend.

CONTENTS is nil.
INFO is a plist holding contextual information."
  (let ((value (org-html-plain-text (org-timestamp-translate timestamp) info)))
    (format "<span class=\"timestamp-wrapper\"><time class=\"timestamp\" datetime=\"%s\">%s</time></span>"
            (ox-html5--convert-timestamp timestamp)
            (replace-regexp-in-string "--" "&#x2013;" value))))

;;;; ox-slimhtml

(defun ox-html5-slimhtml-timestamp (timestamp contents info)
  "Transcode TIMESTAMP from Org to HTML5 for the ox-slimhtml backend.

CONTENTS is nil.
INFO is a plist holding contextual information."
  (let ((value (org-html-plain-text (org-timestamp-translate timestamp) info)))
    (format "<time datetime=\"%s\">%s</time>"
            (ox-html5--convert-timestamp timestamp)
            (replace-regexp-in-string "--" "&#x2013;" value))))

(provide 'ox-html5-timestamp-helper)

;;; ox-html5-timestamp-helper.el ends here
