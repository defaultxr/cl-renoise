;;;; package.lisp - cl-renoise package definition.

(defpackage #:cl-renoise
  (:use #:cl
        #:alexandria
        #:mutility)
  (:nicknames #:renoise)
  (:export
   #:*renoise-host*
   #:*renoise-port*
   #:*renoise-receive-port*

   #:send
   #:evaluate
   #:warning-dialog

   #:bpm
   #:start
   #:stop
   #:cont
   #:panic
   #:note-on
   #:note-off

   #:edit
   #:cell
   #:refresh

   #:receiver
   #:send-reply
   #:initialize-osc-replier
   #:add-reply-handler
   #:remove-reply-handler
   #:reply-handler-for))
