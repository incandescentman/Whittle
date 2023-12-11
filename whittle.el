;;; whittle.el --- Clean up Whisper transcripts by removing filler and duplicated words

;; Author: Jay Dixit
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: convenience, languages
;; URL: [URL to your package repository, if applicable]

;;; Commentary:

;; Whittle is an Emacs package designed to clean up transcripts, especially those from Whisper, by removing unnecessary filler words and correcting unintended word duplications. It helps in making transcripts clearer and more professional.

;;; Code:

(defun remove-filler-words ()
  "Remove filler words from the text and report what was removed."
  (interactive)
  (let ((count-you-know 0)
        (count-kind-of-like 0)
        (count-kind-of 0)
        (count-like 0)
        (count-sort-of 0)
        (count-um 0)
        (count-right 0)
        (count-okay 0)
        (count-uh 0)
        (report-string ""))
    (save-excursion
      (goto-char (point-min))
;; Delete instances of ", you know,"
   (while (search-forward "kind of like" nil t)
    (setq count-kind-of-like (1+ count-kind-of-like))
    (replace-match ""))
      ;; Delete instances of ", you know,"
      (while (search-forward ", you know, " nil t)
        (setq count-you-know (1+ count-you-know))
        (replace-match ", "))
      ;; Delete instances of ", you know,"
      (while (search-forward "you know, " nil t)
        (setq count-you-know (1+ count-you-know))
        (replace-match ""))
      ;; Delete instances of "you know"
      (goto-char (point-min))
      (while (search-forward "you know" nil t)
        (setq count-you-know (1+ count-you-know))
        (replace-match ""))
      ;; Delete instances of "kind of"
      (goto-char (point-min))
      (while (search-forward "kind of" nil t)
        (setq count-kind-of (1+ count-kind-of))
        (replace-match ""))
      ;; Delete instances of ", like, "
      (goto-char (point-min))
      (while (search-forward ", like, " nil t)
        (setq count-like (1+ count-like))
        (replace-match ", "))
      ;; Delete instances of "sort of"
      (goto-char (point-min))
      (while (search-forward "sort of" nil t)
        (setq count-sort-of (1+ count-sort-of))
        (replace-match ""))
;; Replace "okay" with "OK"
      (goto-char (point-min))
      (while (search-forward "okay" nil t)
        (setq count-okay (1+ count-okay))
        (replace-match "OK"))

;; Remove instances of "um" and "umm" as whole words
 (goto-char (point-min))
 (while (re-search-forward "\\b\\(um\\|umm\\)\\b" nil t)
  (setq count-um (1+ count-um))
  (replace-match ""))

;; Remove instances of "uh" not followed by "huh" as a whole word
 (goto-char (point-min))
 (while (re-search-forward "\\buh\\b" nil t)
  (unless (looking-at "-huh")
   (setq count-uh (1+ count-uh))
   (replace-match "")))

      ;; Replace ", right?" with "."
      (goto-char (point-min))
      (while (search-forward ", right?" nil t)
        (setq count-right (1+ count-right))
        (replace-match "."))
      ;; Replace ", right?" with "."
      (goto-char (point-min))
      (while (search-forward ", right." nil t)
        (setq count-right (1+ count-right))
        (replace-match ".")))

      ;; Function to clean up doubled punctuation, returns t if a replacement was made
  (defun cleanup-doubled-punctuation ()
    (let ((replaced nil))
      (goto-char (point-min))
      (while (search-forward ",," nil t)
        (replace-match ",")
        (setq replaced t))
      (goto-char (point-min))
      (while (search-forward ".," nil t)
        (replace-match ".")
        (setq replaced t))
      (goto-char (point-min))
      (while (search-forward " , " nil t)
        (replace-match ", ")
        (setq replaced t))
      (goto-char (point-min))
      (while (search-forward "  " nil t)
        (replace-match " ")
        (setq replaced t))
      replaced))
  ;; Keep cleaning up doubled punctuation until no more replacements are made
  (while (cleanup-doubled-punctuation))
;; Remove stray commas
      (goto-char (point-min))
      (while (search-forward " , " nil t)
        (replace-match ", "))
    ;; Constructing the report string
   ;; Constructing the report string
   (when (> count-you-know 0)
    (setq report-string (concat report-string (format "Removed %d instances of \"you know\". " count-you-know))))
(when (> count-kind-of-like 0)
   (setq report-string (concat report-string (format "Removed %d instances of \"kind of like\". " count-kind-of-like))))
    (when (> count-kind-of 0)
      (setq report-string (concat report-string (format "Removed %d instances of \"kind of\". " count-kind-of))))
    (when (> count-like 0)
      (setq report-string (concat report-string (format "Removed %d instances of \"like\". " count-like))))
    (when (> count-sort-of 0)
      (setq report-string (concat report-string (format "Removed %d instances of \"sort of\". " count-sort-of))))
    (when (> count-um 0)
      (setq report-string (concat report-string (format "Removed %d instances of \"um\". " count-um))))
    (when (> count-right 0)
      (setq report-string (concat report-string (format "Replaced %d instances of \", right?\" with \".\" " count-right))))
    (when (> count-okay 0)
      (setq report-string (concat report-string (format "Replaced %d instances of \"okay\" with \"OK.\" " count-okay))))
  (when (> count-um 0)
    (setq report-string (concat report-string (format "Replaced %d instances of \"um\" with a space. " count-um))))
   (when (> count-uh 0)
    (setq report-string (concat report-string (format "Replaced %d instances of \"uh\" with a space. " count-uh))))
    ;; Reporting the changes
    (if (string= report-string "")
        (message "No changes made.")
      (message report-string)))
)



(defconst duplicate-word-exclusions
  '("that" "go" "no" "yes" "very" "really" "so" "chop" "please" "now" "stop" "sorry"
    "never" "ever" "more" "ha" "wait" "he" "oh" "help" "hi" "hello" "wow" "yum"
    "yo" "run" "bravo" "aha" "long" "always" "tap" "knock" "hurry" "bye" "love"
    "hate" "like" "way" "big" "best" "pretty" "well" "totally")
  "List of words to exclude from duplicate removal.")

(defun remove-duplicated-words ()
  "Remove duplicated words, excluding certain grammatical duplicates."
  (interactive)
  (let ((removed-words (make-hash-table :test 'equal))
        (report-string ""))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "\\b\\(\\w+\\)\\b\\s-+\\1\\b" nil t)
        (let ((matched-word (match-string 1)))
          (unless (member matched-word duplicate-word-exclusions)
            (puthash matched-word (1+ (gethash matched-word removed-words 0)) removed-words)
            (replace-match matched-word))))
    ;; Constructing the report string
    (maphash (lambda (key value)
               (setq report-string (concat report-string (format "\"%s\" %d times, " key value))))
             removed-words)
    ;; Reporting the changes
    (if (string= report-string "")
        (message "No duplicated words removed.")
      (message (concat "Removed duplicated words: " (substring report-string 0 -2) "."))))))

;;;###autoload
(defun whittle-transcript ()
  "Convenience function to run both filler and duplicate word removal."
  (interactive)
  (remove-filler-words)
  (remove-duplicated-words)
  (message "Transcript cleaned up."))

(provide 'whittle)
;;; whittle.el ends here
