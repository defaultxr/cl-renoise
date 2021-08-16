(in-package #:cl-renoise)

;;;; xrns.lisp - support for reading and writing renoise project files

;;; helpers

(defun string-upto (string limit)
  (if (= limit (length-upto string limit))
      (subseq string 0 limit)
      string))

(defun xml-value (xml &rest path)
  ;; (format t "~&~s; ~s~%" path (string-upto (prin1-to-string xml) 25))
  (unless path
    (return-from xml-value xml))
  (let ((item (car path))
        (rest (cdr path)))
    (cond
      ((stringp (car xml))
       (when (string-equal item (car xml))
         (let ((more (cddr xml)))
           (return-from xml-value (if rest
                                      (apply #'xml-value more rest)
                                      (if (not (listp (car more)))
                                          (car more)
                                          (values more (second xml))))))))
      ((listp (car xml))
       (typecase item
         (integer
          (let ((res (nth item xml)))
            (values (apply #'xml-value (cddr res) (cdr path))
                    (second res))))
         (t
          (dolist (element xml (error "Could not find ~s in ~a" item xml))
            (let ((vals (multiple-value-list (apply #'xml-value element path))))
              (when (car vals)
                (return-from xml-value (values-list vals)))))))))))

(defmacro define-project-datum-reader (name result-type &body xml-path)
  `(defun ,name (project)
     (let ((res (xml-value (project-xml project) "RenoiseSong" ,@xml-path)))
       ,(ecase result-type
          (integer `(parse-integer res))
          (float `(parse-float:parse-float res))
          (boolean `(parse-boolean res))
          ((string t) 'res)))))

;;; project

(defclass project ()
  ((zipfile :initarg :zipfile :documentation "The `zip:zipfile' object containing the project data.")
   (parsed-xml :initarg :parsed-xml :documentation "The project's song data parsed as s-exprs.")
   (doc-version :initarg :doc-version :documentation "The Renoise document version.")))

(defun open-project (pathname)
  (make-instance 'project
                 :zipfile (zip:open-zipfile pathname)))

(defun project-raw-xml (project)
  "The raw XML for the project's song data.

See also: `project-xml'"
  (babel:octets-to-string
   (zip:zipfile-entry-contents (zip:get-zipfile-entry "Song.xml" (slot-value project 'zipfile)))
   :encoding :utf-8))

(defun project-xml (project)
  "The parsed XML for the project's song data.

See also: `project-raw-xml'"
  (with-slots (parsed-xml) project
    (unless (slot-boundp project 'parsed-xml)
      (setf parsed-xml (list (xmls:parse-to-list (project-raw-xml project)))))
    parsed-xml))

(defmethod parsed-xml ((project project))
  (project-xml project))

;;; RenoiseSong

(define-project-datum-reader doc-version integer
  "RenoiseSong" "doc_version")

;;; GlobalSongData

(define-project-datum-reader bpm integer
  "GlobalSongData" "BeatsPerMin")

(define-project-datum-reader lpb integer
  "GlobalSongData" "LinesPerBeat")

(define-project-datum-reader tpl integer
  "GlobalSongData" "TicksPerLine")

(define-project-datum-reader signature-numerator integer
  "GlobalSongData" "SignatureNumerator")

(define-project-datum-reader signature-denominator integer
  "GlobalSongData" "SignatureDenominator")

(define-project-datum-reader metronome-beats-per-bar integer
  "GlobalSongData" "MetronomeBeatsPerBar")

(define-project-datum-reader metronome-lines-per-beat integer
  "GlobalSongData" "MetronomeLinesPerBeat")

(define-project-datum-reader shuffle-active-p boolean
  "GlobalSongData" "ShuffleIsActive")

(define-project-datum-reader octave integer
  "GlobalSongData" "Octave")

(define-project-datum-reader loop-coeff integer
  "GlobalSongData" "LoopCoeff")

(define-project-datum-reader song-name string
  "GlobalSongData" "SongName")

(defmethod name ((project project))
  (song-name project))

(define-project-datum-reader artist string
  "GlobalSongData" "Artist")

(define-project-datum-reader show-song-comments-after-loading boolean
  "GlobalSongData" "ShowSongCommentsAfterLoading")

(define-project-datum-reader show-used-automations-only-p boolean
  "GlobalSongData" "ShowUsedAutomationsOnly")

(define-project-datum-reader follow-automations-p boolean
  "GlobalSongData" "FollowAutomations")

(define-project-datum-reader sample-offset-compatibility-mode-p boolean
  "GlobalSongData" "SampleOffsetCompatibilityMode")

(define-project-datum-reader pitch-effects-compatibility-mode-p boolean
  "GlobalSongData" "PitchEffectsCompatibilityMode")

(define-project-datum-reader global-track-headroom float
  "GlobalSongData" "GlobalTrackHeadroom")

(define-project-datum-reader playback-engine-version integer
  "GlobalSongData" "PlaybackEngineVersion")

(define-project-datum-reader render-selection-name-counter integer
  "GlobalSongData" "RenderSelectionNameCounter")

(define-project-datum-reader record-sample-name-counter integer
  "GlobalSongData" "RecordSampleNameCounter")

(define-project-datum-reader new-sample-name-counter integer
  "GlobalSongData" "NewSampleNameCounter")

(defun shuffle-amounts (project)
  (mapcar (fn (parse-integer (second _)))
          (xml-value (project-xml project) "RenoiseSong" "GlobalSongData" "ShuffleAmounts")))

;;; RecordManager

(define-project-datum-reader linked-track-index integer
  "RecordManager" "LinkedTrackIndex")

;;; Instruments

(defclass instrument ()
  ((project :initarg :project :type project :documentation "The `project' that the instrument belongs to.")
   (parsed-xml :initarg :parsed-xml :documentation "The instrument's data parsed as s-exprs.")
   (index :initarg :index :reader instrument-index :type (integer 0) :documentation "This instrument's index in the project's instruments list.")))

(defmethod print-object ((instrument instrument) stream)
  (print-unreadable-object (instrument stream :type t)
    (format stream "~s ~s ~s ~s" :index (instrument-index instrument) :name (instrument-name instrument))))

(defun instrument-xml (instrument)
  "The parsed XML for the instrument's data.

See also: `project-xml'"
  (slot-value instrument 'parsed-xml))

(defmethod parsed-xml ((instrument instrument))
  (instrument-xml instrument))

(defun make-instrument (project parsed-xml index)
  (make-instance 'instrument :project project :parsed-xml parsed-xml :index index))

(defun instrument-elt (project index)
  (make-instrument project (cdr (nth index (xml-value (project-xml project) "Instruments"))) index))

(defun instruments (project &key include-empty)
  "Get a list of defined instruments in PROJECT."
  (loop :for xml :in (xml-value (project-xml project) "Instruments")
        :for index :from 0
        :if (or include-empty
                (not (and (string= "Init" (xml-value xml "SelectedPresetName"))
                          (string= "Bundled Content" (xml-value xml "SelectedPresetLibrary"))
                          (string= "false" (xml-value xml "SelectedPresetIsModified")))))
          :collect (make-instrument project xml index)))

(defun instrument-name (instrument)
  (xml-value (instrument-xml instrument) "Name"))

(defmethod name ((instrument instrument))
  (instrument-name instrument))

;;; SelectedInstrumentIndex

;;; Tracks

;;; SelectedTrackIndex

;;; SpectrumTrackDisplayA

;;; SpectrumTrackDisplayB

;;; PatternPool

(defclass pattern ()
  ((project :initarg :project :reader pattern-project :type project :documentation "The `project' that the pattern belongs to.")
   (parsed-xml :initarg :parsed-xml :documentation "The pattern's data parsed as s-exprs.")
   (index :initarg :index :reader pattern-index :type (integer 0) :documentation "This pattern's index in the project's patterns list.")))

(defmethod print-object ((pattern pattern) stream)
  (print-unreadable-object (pattern stream :type t)
    (format stream "~s ~s" :index (pattern-index pattern))))

(defun pattern-xml (pattern)
  "The parsed XML for the pattern's data.

See also: `project-xml'"
  (slot-value pattern 'parsed-xml))

(defmethod parsed-xml ((pattern pattern))
  (pattern-xml pattern))

(defun make-pattern (project parsed-xml index)
  (make-instance 'pattern :project project :parsed-xml parsed-xml :index index))

(defun patterns-elt (project index)
  (make-pattern project (xml-value (project-xml project) "RenoiseSong" "PatternPool" "Patterns" index) index))

(defun patterns (project)
  "Get a list of defined patterns in PROJECT."
  (loop :for xml :in (xml-value (project-xml project) "RenoiseSong" "PatternPool" "Patterns")
        :for index :from 0
        :do (print index)
        :collect (make-pattern project xml index)))

(defun pattern-length (pattern)
  (parse-integer (xml-value (pattern-xml pattern) "NumberOfLines")))

(defun pattern-tracks (pattern)
  (xml-value (pattern-xml pattern) "Tracks"))

(defclass pattern-track ()
  ((project :initarg :project :reader pattern-track-project :type project :documentation "The `project' that the track belongs to.")
   (pattern :initarg :pattern :reader pattern-track-pattern :type pattern :documentation "The `pattern' that the track belongs to.")
   (parsed-xml :initarg :parsed-xml :documentation "The track's data parsed as s-exprs.")
   (index :initarg :index :reader track-index :type (integer 0) :documentation "This track's index in the pattern's tracks list.")
   (type :initarg :type :accessor pattern-track-type)))

(defmethod print-object ((track pattern-track) stream)
  (print-unreadable-object (track stream :type t)
    (format stream "~s ~s" :index (track-index track))))

(defun pattern-track-xml (track)
  "The parsed XML for the track's data.

See also: `project-xml'"
  (slot-value track 'parsed-xml))

(defmethod parsed-xml ((track pattern-track))
  (pattern-track-xml track))

(defun make-pattern-track (pattern parsed-xml index)
  (make-instance 'pattern-track :project (pattern-project pattern) :pattern pattern :parsed-xml parsed-xml :index index))

(defun pattern-tracks-elt (pattern index)
  (multiple-value-bind (xml attr) (xml-value (pattern-xml pattern) "Tracks" index)
    (let* ((type (cadr (assoc "type" attr :test #'string-equal)))
           (pt (make-pattern-track pattern (cddr xml) index)))
      (setf (pattern-track-type pt) type)
      pt)))

(defun pattern-track-lines (track)
  (let ((array (make-array (list (pattern-length (pattern-track-pattern track))) :initial-element nil)))
    (dolist (line (xml-value (pattern-track-xml track) "Lines") array)
      (multiple-value-bind (line-xml attr) (xml-value line "Line")
        (setf (aref array (parse-integer (cadr (assoc "index" attr :test #'string-equal)))) line-xml)))))

;;; PatternSequence

(defun pattern-sequence (project)
  "Get a list of the indexes of the patterns that comprise PROJECT's song."
  (mapcar (fn (parse-integer (xml-value _ "Pattern")))
          (xml-value (project-xml project) "PatternSequence" "SequenceEntries")))

;;; LastSoloedOutMode

;;; sample data

(defun project-sample-names (project)
  "Get a list of the names of the samples in PROJECT."
  (mapcar (fn (subseq _ 11))
          (remove-if-not (fn (eql 0 (search "SampleData/" _ :test #'string=)))
                         (hash-table-keys (zip:zipfile-entries (slot-value project 'zipfile))))))

