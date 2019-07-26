;;;; renoise.lisp

(in-package #:renoise)

(defun concat (&rest strings)
  "Obligatory string concatenation utility function."
  (format nil "~{~A~}" (remove-if #'null strings)))

(defun escape-string (string)
  "Escape STRING's double quotes."
  (apply 'concat
         (map 'list (lambda (c)
                      (if (char= #\" c) "\\\"" c))
              string)))

(defparameter *host* #(127 0 0 1)
  "The host IP that Renoise is running on (provided as a four-component vector).")

(defparameter *port* 8000
  "The port that Renoise is listening for OSC messages on.")

(defparameter *receive-port* 8008
  "The port to listen on for OSC messages from Renoise.")

(defun send (&rest message)
  "Send an OSC message to Renoise at the host and port set in `*host*' and `*port*'."
  (let ((s (usocket:socket-connect *host* *port* :protocol :datagram :element-type '(unsigned-byte 8)))
        (b (apply 'osc:encode-message message)))
    ;; (format t "Sending to Renoise: ~a~%" message)
    (unwind-protect
         (usocket:socket-send s b (length b))
      (when s (usocket:socket-close s)))))

(defun evaluate (string)
  "Send Lua code STRING to Renoise for evaluation."
  (send "/renoise/evaluate" string))

(defun warning-dialog (string)
  "Make a warning dialog box in Renoise."
  (evaluate (concat "renoise.app():show_warning((\"" (escape-string string) "\"))")))

(defun bpm ()
  "Get the BPM of the current song."
  (error "Not done yet.") ;; FIX
  )

(defun (setf bpm) (value)
  (send "/renoise/song/bpm" value))

(defun start ()
  "Start playing the current song in Renoise.

See also: `stop', `cont'."
  (send "/renoise/transport/start"))

(defun stop ()
  "Stop playing.

See also: `start', `cont'."
  (send "/renoise/transport/stop"))

(defun cont ()
  "Continue playing the song (`continue' is a reserved word in Lisp).

See also: `start', `stop'."
  (send "/renoise/transport/continue"))

(defun panic ()
  "Stop all notes."
  (send "/renoise/transport/panic"))

(defun note-on (note &key (velocity 100) instrument track)
  "Start playing a note in Renoise.

See also: `note-off', `panic'."
  (send "/renoise/trigger/note_on" (or instrument -1) (or track -1) note velocity))

(defun note-off (note &key instrument track)
  "Stop a note that is currently playing in Renoise.

See also: `note-on', `panic'."
  (send "/renoise/trigger/note_off" (or instrument -1) (or track -1) note))

(defun edit (&key pattern track line column (note nil note-provided-p) instrument)
  "Edit the currently-loaded song. Use PATTERN, TRACK, LINE, and COLUMN to specify the position in the song you want to edit, and NOTE and INSTRUMENT to change that cell. When a position value is not provided, the currently-selected position is assumed.

Examples:

;; Set line 8, column 0 in the first pattern and second track to note 72:
;; (edit :pattern 0 :track 2 :line 8 :column 0 :note 72)

;; Set the cell under Renoise's cursor to be a note off:
;; (edit :note :off)

;; Remove any note values from the cell under the cursor:
;; (edit :note nil)

See also: `cell'."
  (assert (typep pattern '(or integer null)) (pattern))
  (assert (typep track '(or integer null)) (track))
  (assert (typep line '(or integer null)) (line))
  (assert (typep column '(or integer null)) (column))
  (assert (typep note '(or (integer 0 127) symbol null)) (note))
  (assert (typep instrument '(or (integer 0 127) null)) (instrument))
  (evaluate
   (concat
    "local lisp_edit = renoise.song():pattern("
    (if pattern
        (1+ pattern) ;; lua indexes from 1, but renoise counts patterns from 0
        "renoise.song().selected_pattern_index")
    "):track("
    (if track
        track
        "renoise.song().selected_track_index")
    "):line("
    (if line
        (1+ line) ;; lua indexes from 1, but renoise counts lines from 0
        "renoise.song().selected_line_index")
    "):note_column("
    (if column
        (1+ column)
        "renoise.song().selected_note_column_index")
    ");"
    (when note-provided-p
      (concat "lisp_edit.note_value = "
              (if (null note)
                  121
                  (if (and (symbolp note)
                           (string= :off note))
                      120
                      note))
              ";"))
    (when instrument
      (concat "note_col.instrument_value = " instrument ";")))))

(defun cell (&key pattern track line column)
  "Get the value of the cell at the position provided, or the cell under the cursor if no position is provided."
  ;; FIX
  (error "This function isn't written yet, sorry."))

(defclass cell ()
  ((pattern :type integer)
   (track :type integer)
   (line :type integer)
   (column :type integer)
   ;; (note :type (or integer symbol null) :initarg :note :initform nil :accessor note)
   ;; (instrument :type integer :initarg :instrument :initform 0 :accessor instrument)
   ;; (volume :type integer :initarg :volume :initform #x80 :accessor volume)
   ;; (pan :type integer :initarg :pan :initform #x80 :accessor pan)
   ;; (delay :type integer :initarg :delay :initform #x80 :accessor delay)
   ;; (fx ;; :type integer ;; FIX
   ;;  :initarg :fx ;; :initform nil ;; FIX
   ;;  :accessor fx)
   )
  (:documentation "Renoise track cell"))

(defgeneric refresh (object)
  (:documentation "Refresh OBJECT, re-caching its current values from Renoise."))

(defmethod refresh ((this cell))
  ())

(defclass line ()
  (;; (pattern)
   ;; (track)
   (cells :type list :initarg :cells :initform 'FIX :accessor cells)
   (fx :type list :initarg :fx ;; :initform nil ;; FIX
       :accessor fx)))

(defclass track ()
  (;; (pattern)
   (name)
   ))

(defun (setf cell) (value &key pattern track line column)
  (assert (typep value '(or integer)))
  (print value)
  (print pattern)
  (print track)
  (print line)
  (print column)
  (edit )
  )

(defun receive ()
  "Loop to receive OSC replies from Renoise and dispatch them to their proper handlers as defined by `add-reply-handler'."
  (let ((s (usocket:socket-connect nil nil
                                   :local-port *receive-port*
                                   :local-host *host*
                                   :protocol :datagram
                                   :element-type '(unsigned-byte 8)))
        (buffer (make-sequence '(vector (unsigned-byte 8)) 1024)))
    (unwind-protect
         (loop :do
              (usocket:socket-receive s buffer (length buffer))
              (let* ((decoded (osc:decode-bundle buffer))
                     (handler (reply-handler-for (elt decoded 0))))
                (if handler
                    (apply handler (cdr decoded))
                    (let ((handler (reply-handler-for t)))
                      (when handler
                        (apply handler (car decoded) (cdr decoded)))))))
      (when s (usocket:socket-close s)))))

(defun send-reply (target &rest message)
  "Make Renoise send a reply to Lisp."
  (evaluate
   (concat "lisp_client:send(renoise.Osc.Message(\"" target "\", {"
             (format nil "~{~a, ~}" (mapcar 'make-osc-arg-table message))
             "}))")))

(defun make-osc-arg-table (value)
  "Make a string representing an OSC message for Renoise's Lua API."
  (concat "{tag=\""
            (etypecase value
              (string "s")
              (float "f")
              (integer "i"))
            "\", value="
            (write-to-string value)
            "}"))

(defun initialize-osc-replier ()
  "Prepare Renoise to send replies back to Lisp."
  (evaluate
   (concat
    "lisp_client, socket_error = renoise.Socket.create_client(\"localhost\", " *receive-port*
    ", renoise.Socket.PROTOCOL_UDP)

if(socket_error) then
renoise.app():show_warning((\"Failed to start the OSC client for Lisp. Error: '%s'\"):format(socket_error))
end")))

(defparameter *reply-handlers* (make-hash-table :test 'equal)
  "Hash table mapping OSC messages to their handlers.")

(defun add-reply-handler (target function)
  "Add a reply handler for messages sent to TARGET. If TARGET is t, all messages without a more specific handler are sent to this function."
  (setf (gethash target *reply-handlers*) function))

(defun remove-reply-handler (target)
  (remhash target *reply-handlers*))

(defun reply-handler-for (target)
  (gethash target *reply-handlers*))
